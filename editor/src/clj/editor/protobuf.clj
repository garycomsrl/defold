;; Copyright 2020-2023 The Defold Foundation
;; Copyright 2014-2020 King
;; Copyright 2009-2014 Ragnar Svensson, Christian Murray
;; Licensed under the Defold License version 1.0 (the "License"); you may not use
;; this file except in compliance with the License.
;; 
;; You may obtain a copy of the License, together with FAQs at
;; https://www.defold.com/license
;; 
;; Unless required by applicable law or agreed to in writing, software distributed
;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations under the License.

(ns editor.protobuf
  "This ns primarily converts between protobuf java objs and clojure maps.
It does this at runtime, which is why it relies on java reflection to
call the appropriate methods. Since this is very expensive (specifically
fetching the Method from the Class), it uses memoization wherever possible.
It should be possible to use macros instead and retain the same API.
Macros currently mean no foreseeable performance gain, however."
  (:require [camel-snake-kebab :refer [->CamelCase ->kebab-case]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [editor.util :as util]
            [editor.workspace :as workspace]
            [internal.java :as j]
            [util.coll :as coll :refer [pair]]
            [util.digest :as digest]
            [util.text-util :as text-util])
  (:import [com.dynamo.proto DdfExtensions DdfMath$Matrix4 DdfMath$Point3 DdfMath$Quat DdfMath$Vector3 DdfMath$Vector4]
           [com.google.protobuf DescriptorProtos$FieldOptions Descriptors$Descriptor Descriptors$EnumDescriptor Descriptors$EnumValueDescriptor Descriptors$FieldDescriptor Descriptors$FieldDescriptor$JavaType Descriptors$FieldDescriptor$Type Descriptors$FileDescriptor Message Message$Builder ProtocolMessageEnum TextFormat]
           [java.io ByteArrayOutputStream StringReader]
           [java.lang.reflect Method]
           [java.nio.charset StandardCharsets]
           [java.util Collection]
           [javax.vecmath Matrix4d Point3d Quat4d Vector3d Vector4d]
           [org.apache.commons.io FilenameUtils]))

(set! *warn-on-reflection* true)

(defprotocol GenericDescriptor
  (proto ^Message [this])
  (desc-name ^String [this])
  (full-name ^String [this])
  (file ^Descriptors$FileDescriptor [this])
  (containing-type ^Descriptors$Descriptor [this]))

(defprotocol PbConverter
  (msg->vecmath [^Message pb v] "Return the javax.vecmath equivalent for the Protocol Buffer message")
  (msg->clj [^Message pb v]))

(def ^:private upper-pattern (re-pattern #"\p{javaUpperCase}"))

(defn escape-string
  ^String [^String string]
  (-> string
      (.getBytes StandardCharsets/UTF_8)
      (TextFormat/escapeBytes)))

(defn- default-instance-raw [^Class cls]
  (j/invoke-no-arg-class-method cls "getDefaultInstance"))

(def ^:private default-instance (memoize default-instance-raw))

(defn- new-builder
  ^Message$Builder [^Class cls]
  (j/invoke-no-arg-class-method cls "newBuilder"))

(defn- field-name->key-raw [^String field-name]
  (keyword (if (re-find upper-pattern field-name)
             (->kebab-case field-name)
             (string/replace field-name "_" "-"))))

(def field-name->key (memoize field-name->key-raw))

(defn- field->key [^Descriptors$FieldDescriptor field-desc]
  (field-name->key (.getName field-desc)))

(defn- enum-name->keyword-name
  ^String [^String enum-name]
  (util/lower-case* (string/replace enum-name "_" "-")))

(defn- enum-name->keyword-raw [^String enum-name]
  (keyword (enum-name->keyword-name enum-name)))

(def ^:private enum-name->keyword (memoize enum-name->keyword-raw))

(defn- keyword->enum-name-raw
  ^String [keyword]
  (.intern (string/replace (util/upper-case* (name keyword)) "-" "_")))

(def ^:private keyword->enum-name (memoize keyword->enum-name-raw))

(defn pb-enum->val
  [val-or-desc]
  (let [^Descriptors$EnumValueDescriptor desc (if (instance? ProtocolMessageEnum val-or-desc)
                                                (.getValueDescriptor ^ProtocolMessageEnum val-or-desc)
                                                val-or-desc)]
    (enum-name->keyword (.getName desc))))

(declare ^:private pb->clj-fn)

(defn- pb-value->clj-fn [field-info default-included-field-rules]
  (let [pb-value->clj
        (case (:field-type-key field-info)
          :message (pb->clj-fn (:type field-info) default-included-field-rules)
          :enum pb-enum->val
          :bool boolean ; Java reflection returns unique Boolean instances. We want either Boolean/TRUE or Boolean/FALSE.
          identity)]
    (if (not= :repeated (:field-rule field-info))
      pb-value->clj
      (fn repeated-pb-value->clj [^Collection values]
        (when-not (.isEmpty values)
          (mapv pb-value->clj values))))))

(def ^:private methods-by-name
  (memoize
    (fn methods-by-name [^Class class]
      (into {}
            (map (fn [^Method m]
                   (pair (.getName m) m)))
            (j/get-declared-methods class)))))

(defn- lookup-method
  ^Method [^Class class method-name]
  (let [methods-by-name (methods-by-name class)]
    (or (get methods-by-name method-name)
        (throw (ex-info (str "Protobuf method lookup failed for " method-name " in " (.getName class))
                        {:available-names (vec (sort (keys methods-by-name)))
                         :class-name (.getName class)
                         :method-name method-name})))))

;; In order to get same behaviour as protobuf compiler, translated from:
;; https://github.com/google/protobuf/blob/2f4489a3e504e0a4aaffee69b551c6acc9e08374/src/google/protobuf/compiler/java/java_helpers.cc#L119
(defn underscores-to-camel-case-raw
  [^String s]
  (loop [i 0
         sb (StringBuilder. (.length s))
         cap-next? true]
    (if (< i (.length s))
      (let [c (.codePointAt s i)]
        (cond
          (<= (int \a) c (int \z))
          (let [c' (if cap-next? (Character/toUpperCase c) c)]
            (recur (inc i) (.appendCodePoint sb c') false))

          (<= (int \A) c (int \Z))
          (let [c' (if (and (zero? i) (not cap-next?)) (Character/toLowerCase c) c)]
            (recur (inc i) (.appendCodePoint sb c') false))

          (<= (int \0) c (int \9))
          (recur (inc i) (.appendCodePoint sb c) true)

          :else
          (recur (inc i) sb true)))
      (-> (if (and (pos? (.length s))
                   (= (int \#) (.codePointAt s (dec (.length s)))))
            (.appendCodePoint sb (int \_))
            sb)
          (.toString)
          (.intern)))))

(def underscores-to-camel-case (memoize underscores-to-camel-case-raw))

(defonce ^:private resource-desc (.getDescriptor DdfExtensions/resource))

(defn- options [^DescriptorProtos$FieldOptions field-options]
  (cond-> {}
          (.getField field-options resource-desc)
          (assoc :resource true)))

(defn- field-type [^Class class ^Descriptors$FieldDescriptor field-desc]
  (let [java-name (underscores-to-camel-case (.getName field-desc))
        field-accessor-name (str "get" java-name)]
    (.getReturnType (lookup-method class field-accessor-name))))

(def ^:private field-type->key
  {Descriptors$FieldDescriptor$Type/BOOL :bool
   Descriptors$FieldDescriptor$Type/BYTES :bytes
   Descriptors$FieldDescriptor$Type/DOUBLE :double
   Descriptors$FieldDescriptor$Type/ENUM :enum
   Descriptors$FieldDescriptor$Type/FIXED32 :fixed-32
   Descriptors$FieldDescriptor$Type/FIXED64 :fixed-64
   Descriptors$FieldDescriptor$Type/FLOAT :float
   Descriptors$FieldDescriptor$Type/GROUP :group
   Descriptors$FieldDescriptor$Type/INT32 :int-32
   Descriptors$FieldDescriptor$Type/INT64 :int-64
   Descriptors$FieldDescriptor$Type/MESSAGE :message
   Descriptors$FieldDescriptor$Type/SFIXED32 :sfixed-32
   Descriptors$FieldDescriptor$Type/SFIXED64 :sfixed-64
   Descriptors$FieldDescriptor$Type/SINT32 :sint-32
   Descriptors$FieldDescriptor$Type/SINT64 :sint-64
   Descriptors$FieldDescriptor$Type/STRING :string
   Descriptors$FieldDescriptor$Type/UINT32 :uint-32
   Descriptors$FieldDescriptor$Type/UINT64 :uint-64})

(defn- field-get-method-raw
  ^Method [^Class cls java-name repeated]
  (let [get-method-name (str "get" java-name (if repeated "List" ""))]
    (lookup-method cls get-method-name)))

(def ^:private field-get-method (memoize field-get-method-raw))

(defn- field-has-value-fn
  ^Method [^Class cls java-name repeated]
  (if repeated
    (let [count-method-name (str "get" java-name "Count")
          count-method (lookup-method cls count-method-name)]
      (fn repeated-field-has-value? [pb]
        (pos? (.invoke count-method pb j/no-args-array))))
    (let [has-method-name (str "has" java-name)
          has-method (lookup-method cls has-method-name)]
      (fn field-has-value? [pb]
        ;; Wrapped in boolean because reflection will produce unique Boolean
        ;; instances that cannot evaluate to false in if-expressions.
        (boolean (.invoke has-method pb j/no-args-array))))))

(defn- descriptor-raw
  ^Descriptors$Descriptor [^Class cls]
  (j/invoke-no-arg-class-method cls "getDescriptor"))

(def ^Descriptors$Descriptor descriptor (memoize descriptor-raw))

(defn- field-infos-raw [^Class cls]
  (let [desc (descriptor cls)]
    (into {}
          (map (fn [^Descriptors$FieldDescriptor field-desc]
                 (let [field-key (field->key field-desc)
                       field-rule (cond (.isRepeated field-desc) :repeated
                                        (.isRequired field-desc) :required
                                        (.isOptional field-desc) :optional
                                        :else (assert false))
                       default (when (not= Descriptors$FieldDescriptor$JavaType/MESSAGE (.getJavaType field-desc))
                                 (.getDefaultValue field-desc))
                       declared-default (when (.hasDefaultValue field-desc)
                                          default)]
                   (pair field-key
                         {:type (field-type cls field-desc)
                          :java-name (underscores-to-camel-case (.getName field-desc))
                          :field-type-key (field-type->key (.getType field-desc))
                          :field-rule field-rule
                          :default default
                          :declared-default declared-default
                          :options (options (.getOptions field-desc))}))))
          (.getFields desc))))

(def ^:private field-infos (memoize field-infos-raw))

(defn resource-field? [field-info]
  (and (= :string (:field-type-key field-info))
       (true? (:resource (:options field-info)))))

(defn message-field? [field-info]
  (= :message (:field-type-key field-info)))

(def field-key-set
  "Returns the set of field keywords applicable to the supplied protobuf Class."
  (memoize (comp set keys field-infos)))

(defn- declared-default [^Class cls field]
  (if-some [field-info (get (field-infos cls) field)]
    (:declared-default field-info)
    (throw (ex-info (format "Field '%s' does not exist in protobuf class '%s'."
                            field
                            (.getName cls))
                    {:pb-class cls
                     :field field}))))

(declare resource-field-paths)

(defn resource-field-paths-raw
  "Returns a list of path expressions pointing out all resource fields.

  path-expr := '[' elem+ ']'
  elem := :keyword                  ; index into structure using :keyword
  elem := '{' :keyword default '}'  ; index into structure using :keyword, with default value to use when not specified
  elem := '[' :keyword ']'          ; :keyword is a repeated field, use rest of step* (if any) to index into each repetition (a message)

  message ResourceSimple {
      optional string image = 1 [(resource) = true];
  }

  message ResourceDefaulted {
      optional string image = 1 [(resource) = true, default = '/default.png'];
  }

  message ResourceRepeated {
      repeated string images = 1 [(resource) = true];
  }

  message ResourceSimpleNested {
      optional ResourceSimple simple = 1;
  }

  message ResourceDefaultedNested {
      optional ResourceDefaulted defaulted = 1;
  }

  message ResourceRepeatedNested {
      optional ResourceRepeated repeated = 1;
  }

  message ResourceSimpleRepeatedlyNested {
      repeated ResourceSimple simples = 1;
  }

  message ResourceDefaultedRepeatedlyNested {
      repeated ResourceDefaulted defaulteds = 1;
  }

  message ResourceRepeatedRepeatedlyNested {
      repeated ResourceRepeated repeateds = 1;
  }

  (resource-field-paths-raw ResourceSimple)    => [ [:image] ]
  (resource-field-paths-raw ResourceDefaulted) => [ [{:image '/default.png'}] ]
  (resource-field-paths-raw ResourceRepeated)  => [ [[:images]] ]

  (resource-field-paths-raw ResourceSimpleNested)    => [ [:simple :image] ]
  (resource-field-paths-raw ResourceDefaultedNested) => [ [:defaulted {:image '/default.png'}] ]
  (resource-field-paths-raw ResourceRepeatedNested)  => [ [:repeated [:images]] ]

  (resource-field-paths-raw ResourceSimpleRepeatedlyNested)    => [ [[:simples] :image] ]
  (resource-field-paths-raw ResourceDefaultedRepeatedlyNested) => [ [[:defaulteds] {:image '/default.png'}] ]
  (resource-field-paths-raw ResourceRepeatedRepeatedlyNested)  => [ [[:repeateds] [:images]] ]"
  [^Class class]
  (into []
        (comp
          (map (fn [[key field-info]]
                 (cond
                   (resource-field? field-info)
                   (case (:field-rule field-info)
                     :repeated [ [[key]] ]
                     :required [ [key] ]
                     :optional (if-some [field-default (declared-default class key)]
                                 [ [{key field-default}] ]
                                 [ [key] ]))

                   (message-field? field-info)
                   (let [sub-paths (resource-field-paths (:type field-info))]
                     (when (seq sub-paths)
                       (let [prefix (if (= :repeated (:field-rule field-info))
                                      [[key]]
                                      [key])]
                         (mapv (partial into prefix) sub-paths)))))))
          cat
          (remove nil?))
        (field-infos class)))

(def resource-field-paths (memoize resource-field-paths-raw))

(declare get-field-fn)

(defn- get-field-fn-raw [path]
  (if (seq path)
    (let [elem (first path)
          sub-path-fn (get-field-fn (rest path))]
      (cond
        (keyword? elem)
        (fn [pb]
          (sub-path-fn (elem pb)))

        (map? elem)
        (fn [pb]
          (let [[key default] (first elem)]
            (sub-path-fn (get pb key default))))

        (vector? elem)
        (let [pbs-fn (first elem)]
          (fn [pb]
            (into [] (mapcat sub-path-fn) (pbs-fn pb))))))
    (fn [pb] [pb])))

(def ^:private get-field-fn (memoize get-field-fn-raw))

(defn- get-fields-fn-raw [paths]
  (let [get-field-fns (map get-field-fn paths)]
    (fn [pb]
      (into []
            (mapcat (fn [get-fn]
                      (get-fn pb)))
            get-field-fns))))


(def get-fields-fn (memoize get-fields-fn-raw))

(defn- make-pb->clj-fn [fields]
  (fn pb->clj [pb]
    (->> fields
         (reduce (fn [pb-map [field-key pb->field-value]]
                   (if-some [field-value (pb->field-value pb)]
                     (assoc! pb-map field-key field-value)
                     pb-map))
                 (transient {}))
         (persistent!)
         (msg->clj pb))))

(defn- default-message-raw [^Class class default-included-field-rules]
  (let [pb->clj (pb->clj-fn class default-included-field-rules)]
    (pb->clj (default-instance class))))

(def ^:private default-message (memoize default-message-raw))

(defn- pb->clj-fn-raw [^Class class default-included-field-rules]
  {:pre [(set? default-included-field-rules)
         (every? #{:optional :required} default-included-field-rules)]}
  (make-pb->clj-fn
    (mapv (fn [[field-key {:keys [field-rule java-name] :as field-info}]]
            (let [pb-value->clj (pb-value->clj-fn field-info default-included-field-rules)
                  repeated (= :repeated field-rule)
                  ^Method field-get-method (field-get-method class java-name repeated)
                  include-defaults (contains? default-included-field-rules field-rule)]
              (pair field-key
                    (cond
                      (and include-defaults
                           (= :optional field-rule)
                           (message-field? field-info))
                      (let [field-has-value? (field-has-value-fn class java-name repeated)
                            default-field-value (default-message (:type field-info) default-included-field-rules)]
                        (fn pb->field-clj-value [pb]
                          (if (field-has-value? pb)
                            (let [field-pb-value (.invoke field-get-method pb j/no-args-array)]
                              (pb-value->clj field-pb-value))
                            default-field-value)))

                      (or repeated
                          include-defaults)
                      (fn pb->field-clj-value-or-default [pb]
                        (let [field-pb-value (.invoke field-get-method pb j/no-args-array)]
                          (pb-value->clj field-pb-value)))

                      :else
                      (let [field-has-value? (field-has-value-fn class java-name repeated)]
                        (fn pb->field-clj-value [pb]
                          (when (field-has-value? pb)
                            (let [field-pb-value (.invoke field-get-method pb j/no-args-array)]
                              (pb-value->clj field-pb-value)))))))))
          (field-infos class))))

(def ^:private pb->clj-fn (memoize pb->clj-fn-raw))

(def ^:private pb->clj-with-defaults-fn (memoize #(pb->clj-fn % #{:optional :required})))

(def ^:private pb->clj-without-defaults-fn (memoize #(pb->clj-fn % #{})))

(defn- clear-defaults-from-builder! [^Message$Builder builder]
  (reduce (fn [is-default ^Descriptors$FieldDescriptor field-desc]
            (if (= Descriptors$FieldDescriptor$JavaType/MESSAGE (.getJavaType field-desc))
              ;; Message field.
              (cond
                (.isOptional field-desc)
                (if-not (.hasField builder field-desc)
                  is-default
                  (let [field-builder (.toBuilder ^Message (.getField builder field-desc))
                        field-is-default (clear-defaults-from-builder! field-builder)]
                    (if field-is-default
                      (do
                        (.clearField builder field-desc)
                        is-default)
                      (do
                        (.setField builder field-desc (.build field-builder))
                        false))))

                (.isRequired field-desc)
                (let [field-builder (.toBuilder ^Message (.getField builder field-desc))
                      field-is-default (clear-defaults-from-builder! field-builder)]
                  (.setField builder field-desc (.build field-builder))
                  (and is-default field-is-default))

                (.isRepeated field-desc)
                (let [item-count (.getRepeatedFieldCount builder field-desc)]
                  (doseq [index (range item-count)]
                    (let [item-builder (.toBuilder ^Message (.getRepeatedField builder field-desc index))]
                      (clear-defaults-from-builder! item-builder)
                      (.setRepeatedField builder field-desc index (.build item-builder))))
                  (and is-default (zero? item-count)))

                :else
                is-default)

              ;; Non-message field.
              (cond
                (.isOptional field-desc)
                (cond
                  (not (.hasField builder field-desc))
                  is-default

                  (and (= (.getDefaultValue field-desc) (.getField builder field-desc))
                       (not (-> field-desc (.getOptions) (.getField resource-desc))))
                  (do
                    (.clearField builder field-desc)
                    is-default)

                  :else
                  false)

                (.isRequired field-desc)
                (and is-default (= (.getDefaultValue field-desc) (.getField builder field-desc)))

                (.isRepeated field-desc)
                (and is-default (zero? (.getRepeatedFieldCount builder field-desc)))

                :else
                is-default)))
          true
          (.getFields (.getDescriptorForType builder))))

(defn- clear-defaults-from-message
  ^Message [^Message message]
  (let [builder (.toBuilder message)]
    (clear-defaults-from-builder! builder)
    (.build builder)))

(defn pb->map-with-defaults
  [^Message pb]
  (let [pb->clj-with-defaults (pb->clj-with-defaults-fn (.getClass pb))]
    (pb->clj-with-defaults pb)))

(defn pb->map-without-defaults
  [^Message pb]
  (let [pb->clj-without-defaults (pb->clj-without-defaults-fn (.getClass pb))]
    (pb->clj-without-defaults (clear-defaults-from-message pb))))

(defn- default-value-raw [^Class cls]
  (default-message cls #{:optional}))

(def default-value (memoize default-value-raw))

(defn default
  ([^Class cls field]
   (let [field-default (get (default-value cls) field ::not-found)]
     (if (not= ::not-found field-default)
       field-default
       (throw (ex-info (format "Field '%s' does not have a default in protobuf class '%s'."
                               field
                               (.getName cls))
                       {:pb-class cls
                        :field field})))))
  ([^Class cls field not-found]
   (get (default-value cls) field not-found)))

(defn- desc->proto-cls ^Class [desc]
  (let [cls-name (if-let [containing (containing-type desc)]
                   (str (.getName (desc->proto-cls containing)) "$" (desc-name desc))
                   (let [file (file desc)
                         options (.getOptions file)
                         package (if (.hasJavaPackage options)
                                   (.getJavaPackage options)
                                   (let [full (full-name desc)
                                         name (desc-name desc)]
                                     (subs full 0 (- (count full) (count name) 1))))
                         outer-cls (if (.hasJavaOuterClassname options)
                                     (.getJavaOuterClassname options)
                                     (->CamelCase (FilenameUtils/getBaseName (.getName file))))
                         inner-cls (desc-name desc)]
                     (str package "." outer-cls "$" inner-cls)))]
    (workspace/load-class! cls-name)))

(defn- primitive-builder [^Descriptors$FieldDescriptor desc]
  (let [type (.getJavaType desc)]
    (cond
      (= type Descriptors$FieldDescriptor$JavaType/INT) (fn [v]
                                                          (int (if (instance? Boolean v)
                                                                 (if v 1 0)
                                                                 v)))
      (= type Descriptors$FieldDescriptor$JavaType/LONG) long
      (= type Descriptors$FieldDescriptor$JavaType/FLOAT) float
      (= type Descriptors$FieldDescriptor$JavaType/DOUBLE) double
      (= type Descriptors$FieldDescriptor$JavaType/STRING) str
      ;; The reason we convert to Boolean object is for symmetry - the protobuf system do this when loading from protobuf files
      (= type Descriptors$FieldDescriptor$JavaType/BOOLEAN) (fn [v] (Boolean/valueOf (boolean v)))
      (= type Descriptors$FieldDescriptor$JavaType/BYTE_STRING) identity
      (= type Descriptors$FieldDescriptor$JavaType/ENUM) (let [enum-cls (desc->proto-cls (.getEnumType desc))]
                                                           (fn [v] (Enum/valueOf enum-cls (keyword->enum-name v))))
      :else nil)))

(declare ^:private pb-builder ^:private vector-to-map-conversions)

(defn- pb-builder-raw [^Class class]
  (let [desc (descriptor class)
        ^Method new-builder-method (j/get-declared-method class "newBuilder" [])
        builder-class (.getReturnType new-builder-method)
        ;; All methods relevant to us
        methods (into {}
                      (keep (fn [^Method m]
                              (let [m-name (.getName m)
                                    set-via-builder? (and (.startsWith m-name "set")
                                                          (when-some [^Class first-arg-class (first (.getParameterTypes m))]
                                                            (.endsWith (.getName first-arg-class) "$Builder")))]
                                (when (not set-via-builder?)
                                  (pair m-name m)))))
                      (j/get-declared-methods builder-class))
        field-descs (.getFields desc)
        setters (into {}
                      (map (fn [^Descriptors$FieldDescriptor fd]
                             (let [j-name (->CamelCase (.getName fd))
                                   repeated (.isRepeated fd)
                                   ^Method field-set-method (get methods (str (if repeated "addAll" "set") j-name))
                                   field-builder (if (= (.getJavaType fd) Descriptors$FieldDescriptor$JavaType/MESSAGE)
                                                   (let [^Method field-get-method (get methods (str "get" j-name))]
                                                     (pb-builder (.getReturnType field-get-method)))
                                                   (primitive-builder fd))
                                   value-fn (if repeated
                                              (partial mapv field-builder)
                                              field-builder)]
                               (pair (field->key fd)
                                     (fn [^Message$Builder b v]
                                       (let [value (value-fn v)]
                                         (.invoke field-set-method b (to-array [value]))))))))
                      field-descs)
        builder-fn (fn [m]
                     (let [b (new-builder class)]
                       (doseq [[k v] m
                               :when (some? v)
                               :let [setter! (get setters k)]
                               :when setter!]
                         (setter! b v))
                       (.build b)))]
    (if-some [vector->map (vector-to-map-conversions class)]
      (comp builder-fn vector->map)
      builder-fn)))

(def ^:private pb-builder (memoize pb-builder-raw))

(defn map->pb
  [^Class cls m]
  (when-let [builder (pb-builder cls)]
    (builder m)))

(defmacro str->pb [^Class cls str]
  (with-meta `(TextFormat/parse ~str ~cls)
             {:tag cls}))

(defn- break-embedded-newlines
  [^String pb-str]
  (.replace pb-str "\\n" "\\n\"\n  \""))

(defn pb->str [^Message pb format-newlines?]
  (cond-> (.printToString (TextFormat/printer) pb)
          format-newlines?
          (break-embedded-newlines)))

(defn pb->bytes [^Message pb]
  (let [out (ByteArrayOutputStream. (* 4 1024))]
    (.writeTo pb out)
    (.close out)
    (.toByteArray out)))

(defn val->pb-enum [^Class enum-class val]
  (Enum/valueOf enum-class (keyword->enum-name val)))

(def ^:private ^:const f0 (float 0.0))
(def ^:private ^:const f1 (float 1.0))

(extend-protocol PbConverter
  DdfMath$Point3
  (msg->vecmath [_pb v] (Point3d. (:x v f0) (:y v f0) (:z v f0)))
  (msg->clj [_pb v] [(:x v f0) (:y v f0) (:z v f0)])

  DdfMath$Vector3
  (msg->vecmath [_pb v] (Vector3d. (:x v f0) (:y v f0) (:z v f0)))
  (msg->clj [_pb v] [(:x v f0) (:y v f0) (:z v f0)])

  DdfMath$Vector4
  (msg->vecmath [_pb v] (Vector4d. (:x v f0) (:y v f0) (:z v f0) (:w v f0)))
  (msg->clj [_pb v] [(:x v f0) (:y v f0) (:z v f0) (:w v f0)])

  DdfMath$Quat
  (msg->vecmath [_pb v] (Quat4d. (:x v f0) (:y v f0) (:z v f0) (:w v f1)))
  (msg->clj [_pb v] [(:x v f0) (:y v f0) (:z v f0) (:w v f1)])

  DdfMath$Matrix4
  (msg->vecmath [_pb v]
    (Matrix4d. (:m00 v f1) (:m01 v f0) (:m02 v f0) (:m03 v f0)
               (:m10 v f0) (:m11 v f1) (:m12 v f0) (:m13 v f0)
               (:m20 v f0) (:m21 v f0) (:m22 v f1) (:m23 v f0)
               (:m30 v f0) (:m31 v f0) (:m32 v f0) (:m33 v f1)))
  (msg->clj [_pb v]
    [(:m00 v f1) (:m01 v f0) (:m02 v f0) (:m03 v f0)
     (:m10 v f0) (:m11 v f1) (:m12 v f0) (:m13 v f0)
     (:m20 v f0) (:m21 v f0) (:m22 v f1) (:m23 v f0)
     (:m30 v f0) (:m31 v f0) (:m32 v f0) (:m33 v f1)])

  Message
  (msg->vecmath [_pb v] v)
  (msg->clj [_pb v] v))

(defprotocol VecmathConverter
  (vecmath->pb [v] "Return the Protocol Buffer equivalent for the given javax.vecmath value"))

(extend-protocol VecmathConverter
  Point3d
  (vecmath->pb [v]
    (-> (DdfMath$Point3/newBuilder)
        (.setX (.getX v))
        (.setY (.getY v))
        (.setZ (.getZ v))
        (.build)))

  Vector3d
  (vecmath->pb [v]
    (-> (DdfMath$Vector3/newBuilder)
        (.setX (.getX v))
        (.setY (.getY v))
        (.setZ (.getZ v))
        (.build)))

  Vector4d
  (vecmath->pb [v]
    (-> (DdfMath$Vector4/newBuilder)
        (.setX (.getX v))
        (.setY (.getY v))
        (.setZ (.getZ v))
        (.setW (.getW v))
        (.build)))

  Quat4d
  (vecmath->pb [v]
    (-> (DdfMath$Quat/newBuilder)
        (.setX (.getX v))
        (.setY (.getY v))
        (.setZ (.getZ v))
        (.setW (.getW v))
        (.build)))

  Matrix4d
  (vecmath->pb [v]
    (-> (DdfMath$Matrix4/newBuilder)
        (.setM00 (.getElement v 0 0)) (.setM01 (.getElement v 0 1)) (.setM02 (.getElement v 0 2)) (.setM03 (.getElement v 0 3))
        (.setM10 (.getElement v 1 0)) (.setM11 (.getElement v 1 1)) (.setM12 (.getElement v 1 2)) (.setM13 (.getElement v 1 3))
        (.setM20 (.getElement v 2 0)) (.setM21 (.getElement v 2 1)) (.setM22 (.getElement v 2 2)) (.setM23 (.getElement v 2 3))
        (.setM30 (.getElement v 3 0)) (.setM31 (.getElement v 3 1)) (.setM32 (.getElement v 3 2)) (.setM33 (.getElement v 3 3))
        (.build))))

(extend-protocol GenericDescriptor
  Descriptors$Descriptor
  (proto [this] (.toProto this))
  (desc-name [this] (.getName this))
  (full-name [this] (.getFullName this))
  (file [this] (.getFile this))
  (containing-type [this] (.getContainingType this))

  Descriptors$EnumDescriptor
  (proto [this] (.toProto this))
  (desc-name [this] (.getName this))
  (full-name [this] (.getFullName this))
  (file [this] (.getFile this))
  (containing-type [this] (.getContainingType this)))

(defn map->str
  ([^Class cls m] (map->str cls m true))
  ([^Class cls m format-newlines?]
   (pb->str (map->pb cls m) format-newlines?)))

(defn map->bytes [^Class cls m]
  (pb->bytes (map->pb cls m)))

(defn read-pb-into!
  ^Message$Builder [^Message$Builder builder input]
  (with-open [reader (io/reader input)]
    (TextFormat/merge reader builder)
    builder))

(defn read-pb [^Class cls input]
  ;; TODO: Make into macro so we can tag the return type.
  (.build (read-pb-into! (new-builder cls) input)))

(defn str->map-with-defaults [^Class cls ^String str]
  (pb->map-with-defaults
    (with-open [reader (StringReader. str)]
      (read-pb cls reader))))

(defn str->map-without-defaults [^Class cls ^String str]
  (pb->map-without-defaults
    (with-open [reader (StringReader. str)]
      (read-pb cls reader))))

(defonce single-byte-array-args [j/byte-array-class])

(defn- parser-fn-raw [^Class cls]
  (let [^Method parse-method (j/get-declared-method cls "parseFrom" single-byte-array-args)]
    (fn parser-fn [^bytes bytes]
      (.invoke parse-method nil (object-array [bytes])))))

(def parser-fn (memoize parser-fn-raw))

(defmacro bytes->pb [^Class cls bytes]
  (with-meta `((parser-fn ~cls) ~bytes)
             {:tag cls}))

(defn bytes->map-with-defaults [^Class cls bytes]
  (let [parser (parser-fn cls)]
    (-> bytes
        parser
        pb->map-with-defaults)))

(defn- enum-values-raw [^Class cls]
  (let [^Method values-method (j/get-declared-method cls "values" [])
        values (.invoke values-method nil (object-array 0))]
    (mapv (fn [^ProtocolMessageEnum value]
            [(pb-enum->val value)
             {:display-name (-> (.getValueDescriptor value) (.getOptions) (.getExtension DdfExtensions/displayName))}])
          values)))

(def enum-values (memoize enum-values-raw))

(defn- fields-by-indices-raw [^Class cls]
  (let [desc (descriptor cls)]
    (into {}
          (map (fn [^Descriptors$FieldDescriptor field]
                 (pair (.getNumber field)
                       (field->key field))))
          (.getFields desc))))

(def fields-by-indices (memoize fields-by-indices-raw))

(defn pb->hash
  ^bytes [^String algorithm ^Message pb]
  (with-open [digest-output-stream (digest/make-digest-output-stream algorithm)]
    (.writeTo pb digest-output-stream)
    (.digest (.getMessageDigest digest-output-stream))))

(defn map->sha1-hex
  ^String [^Class cls m]
  (digest/bytes->hex (pb->hash "SHA-1" (map->pb cls m))))

(defn default-read-scale-value? [value]
  ;; The default value of the Vector3 type is zero, and protobuf does not
  ;; support custom default values for message-type fields. That means
  ;; everything read from protobuf will be scaled down to zero. However, we
  ;; might change the behavior of the protobuf reader in the future to have it
  ;; return [1.0 1.0 1.0] or even nil for default scale fields. In some way, nil
  ;; would make sense as a default for message-type fields as that is what
  ;; protobuf does without our wrapper. We could then decide on sensible default
  ;; values on a case-by-case basis. Related to all this, there has been some
  ;; discussion around perhaps omitting default values from the project data.
  (= [0.0 0.0 0.0] value))

(def ^:private vector-to-map-conversions
  ;; TODO(save-value): Introduce Vector4One for identity scale and white color default.
  (->> {DdfMath$Point3 [:x :y :z]
        DdfMath$Vector3 [:x :y :z]
        DdfMath$Vector4 [:x :y :z :w]
        DdfMath$Quat [:x :y :z :w]
        DdfMath$Matrix4 [:m00 :m01 :m02 :m03
                         :m10 :m11 :m12 :m13
                         :m20 :m21 :m22 :m23
                         :m30 :m31 :m32 :m33]}
       (into {}
             (map (fn [[^Class cls component-keys]]
                    (let [default-values (default-value cls)]
                      (pair cls
                            (fn vector->map [component-values]
                              (->> component-keys
                                   (reduce-kv
                                     (fn [result index key]
                                       (let [value (component-values index)]
                                         ;; TODO(save-value): Strip out components with default values once we have Vector4One.
                                         (assoc! result key value)
                                         #_(if (= (default-values index) value)
                                             result
                                             (assoc! result key value))))
                                     (transient {}))
                                   (persistent!))))))))))

(defn make-map-with-defaults [^Class cls & kvs]
  (into (default-value cls)
        (comp (partition-all 2)
              (keep (fn [[key value]]
                      (cond
                        (nil? value)
                        nil

                        (sequential? value)
                        (when-not (coll/empty? value)
                          (pair key (vec value)))

                        :else
                        (pair key value)))))
        kvs))

(defn make-map-without-defaults [^Class cls & kvs]
  (let [key->field-info (field-infos cls)
        key->default (default-value cls)]
    (into {}
          (comp (partition-all 2)
                (keep (fn [[key value]]
                        (when (some? value)
                          (let [field-info (key->field-info key)]
                            (case (:field-rule field-info)
                              :optional
                              (when (or (resource-field? field-info)
                                        (not (or (and (message-field? field-info)
                                                      (coll/empty? value))
                                                 (= (key->default key) value))))
                                (pair key value))

                              :repeated
                              (when (and (not (coll/empty? value))
                                         (not= (key->default key) value))
                                (pair key (vec value)))

                              :required
                              (when-not (and (message-field? field-info)
                                             (coll/empty? value))
                                (pair key value))

                              (throw (ex-info (str "Invalid field " key " for protobuf class " (.getName cls))
                                              {:key key
                                               :pb-class cls}))))))))
          kvs)))

(defn read-map-with-defaults [^Class cls input]
  (pb->map-with-defaults
    (read-pb cls input)))

(defn read-map-without-defaults [^Class cls input]
  (pb->map-without-defaults
    (read-pb cls input)))

(defn assign [pb-map field-kw value]
  {:pre [(map? pb-map)
         (keyword? field-kw)]}
  (if (or (nil? value)
          (and (map? value)
               (coll/empty? value)))
    (dissoc pb-map field-kw)
    (assoc pb-map field-kw value)))

(defn assign-repeated [pb-map field-kw items]
  {:pre [(map? pb-map)
         (keyword? field-kw)]}
  (if (coll/empty? items)
    (dissoc pb-map field-kw)
    (assoc pb-map field-kw (vec items))))

(defn sanitize
  ([pb-map field-kw]
   {:pre [(map? pb-map)
          (keyword? field-kw)]}
   (let [value (get pb-map field-kw ::not-found)]
     (if (nil? value)
       (dissoc pb-map field-kw)
       pb-map)))
  ([pb-map field-kw sanitize-value-fn]
   {:pre [(map? pb-map)
          (keyword? field-kw)
          (ifn? sanitize-value-fn)]}
   (let [value (get pb-map field-kw ::not-found)]
     (if (= ::not-found value)
       pb-map
       (assign pb-map field-kw (some-> value sanitize-value-fn))))))

(defn sanitize-repeated
  ([pb-map field-kw]
   {:pre [(map? pb-map)
          (keyword? field-kw)]}
   (let [items (get pb-map field-kw ::not-found)]
     (if (or (= ::not-found items)
             (not (coll/empty? items)))
       pb-map
       (dissoc pb-map field-kw))))
  ([pb-map field-kw sanitize-item-fn]
   {:pre [(map? pb-map)
          (keyword? field-kw)
          (ifn? sanitize-item-fn)]}
   (let [items (get pb-map field-kw ::not-found)]
     (if (= ::not-found items)
       pb-map
       (assign-repeated pb-map field-kw (some->> items (mapv sanitize-item-fn)))))))

(defn make-map-search-match-fn
  "Returns a function that takes a value and returns it if its protobuf-text
  representation matches the provided search-string. This function is suitable
  for use with coll/search and its ilk on a protobuf message in map format."
  [search-string]
  (let [enum-search-string (enum-name->keyword-name search-string)
        text-re-pattern (text-util/search-string->re-pattern search-string :case-insensitive)
        enum-re-pattern (text-util/search-string->re-pattern enum-search-string :case-insensitive)
        search-string-is-numeric (text-util/search-string-numeric? search-string)]
    (fn match-fn [value]
      (cond
        (string? value)
        (when (text-util/includes-re-pattern? value text-re-pattern)
          value)

        (keyword? value)
        (when (text-util/includes-re-pattern? (name value) enum-re-pattern)
          value)

        (and search-string-is-numeric (number? value))
        (when (text-util/includes-re-pattern? (str value) text-re-pattern)
          value)

        (boolean? value)
        (when (text-util/includes-re-pattern? (if value "true" "false") text-re-pattern)
          value)))))
