#ifndef OPENGL_DEVICE_DEFINES_H
#define OPENGL_DEVICE_DEFINES_H

#ifdef __linux__

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

#elif defined (__MACH__)

#include <OpenGL/gl.h>

#elif defined (_WIN32)

#include <windows.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <win32/glut.h>

#else
#error "Platform not supported."
#endif

namespace dmGraphics
{
    typedef uint32_t                  HVertexProgram;
    typedef uint32_t                  HFragmentProgram;
    typedef struct Context*           HContext;
    typedef struct Device*            HDevice;
    typedef struct Texture*           HTexture;
    typedef struct VertexBuffer*      HVertexBuffer;
    typedef struct IndexBuffer*       HIndexBuffer;
    typedef struct VertexDeclaration* HVertexDeclaration;

    static const HVertexProgram INVALID_VERTEX_PROGRAM_HANDLE = ~0u;
    static const HFragmentProgram INVALID_FRAGMENT_PROGRAM_HANDLE = ~0u;
}

// Primitive types
#define GFXDEVICE_PRIMITIVE_POINTLIST                   (GL_POINTS)
#define GFXDEVICE_PRIMITIVE_LINES                       (GL_LINES)
#define GFXDEVICE_PRIMITIVE_LINE_LOOP                   (GL_LINE_LOOP)
#define GFXDEVICE_PRIMITIVE_LINE_STRIP                  (GL_LINE_STRIP)
#define GFXDEVICE_PRIMITIVE_TRIANGLES                   (GL_TRIANGLES)
#define GFXDEVICE_PRIMITIVE_TRIANGLE_STRIP              (GL_TRIANGLE_STRIP)
#define GFXDEVICE_PRIMITIVE_TRIANGLE_FAN                (GL_TRIANGLE_FAN)
#define GFXDEVICE_PRIMITIVE_QUADS                       (GL_QUADS)
#define GFXDEVICE_PRIMITIVE_QUAD_STRIP                  (GL_QUAD_STRIP)

// Clear flags
#define GFXDEVICE_CLEAR_COLOURUFFER                     (GL_COLOR_BUFFER_BIT)
#define GFXDEVICE_CLEAR_DEPTHBUFFER                     (GL_DEPTH_BUFFER_BIT)
#define GFXDEVICE_CLEAR_STENCILBUFFER                   (GL_STENCIL_BUFFER_BIT)

#define GFXDEVICE_MATRIX_TYPE_WORLD                     (123ul)
#define GFXDEVICE_MATRIX_TYPE_VIEW                      (321ul)
#define GFXDEVICE_MATRIX_TYPE_PROJECTION                (GL_PROJECTION)

// Render states
#define GFXDEVICE_STATE_DEPTH_TEST                      (GL_DEPTH_TEST)
#define GFXDEVICE_STATE_ALPHA_TEST                      (GL_ALPHA_TEST)
#define GFXDEVICE_STATE_BLEND                           (GL_BLEND)
#define GFXDEVICE_STATE_CULL_FACE                       (GL_CULL_FACE)

// Types
#define GFXDEVICE_TYPE_BYTE                             (GL_BYTE)
#define GFXDEVICE_TYPE_UNSIGNED_BYTE                    (GL_UNSIGNED_BYTE)
#define GFXDEVICE_TYPE_SHORT                            (GL_SHORT)
#define GFXDEVICE_TYPE_UNSIGNED_SHORT                   (GL_UNSIGNED_SHORT)
#define GFXDEVICE_TYPE_INT                              (GL_INT)
#define GFXDEVICE_TYPE_UNSIGNED_INT                     (GL_UNSIGNED_INT)
#define GFXDEVICE_TYPE_FLOAT                            (GL_FLOAT)

// Texture format
#define GFXDEVICE_TEXTURE_FORMAT_ALPHA                  (GL_ALPHA)
#define GFXDEVICE_TEXTURE_FORMAT_RGBA                   (GL_RGBA)
#define GFXDEVICE_TEXTURE_FORMAT_RGB_DXT1               (GL_COMPRESSED_RGB_S3TC_DXT1_EXT)
#define GFXDEVICE_TEXTURE_FORMAT_RGBA_DXT1              (GL_COMPRESSED_RGBA_S3TC_DXT1_EXT)
#define GFXDEVICE_TEXTURE_FORMAT_RGBA_DXT1              (GL_COMPRESSED_RGBA_S3TC_DXT1_EXT)
#define GFXDEVICE_TEXTURE_FORMAT_RGBA_DXT3              (GL_COMPRESSED_RGBA_S3TC_DXT3_EXT)
#define GFXDEVICE_TEXTURE_FORMAT_RGBA_DXT5              (GL_COMPRESSED_RGBA_S3TC_DXT5_EXT)

// Blend factors
#define GFXDEVICE_BLEND_FACTOR_ZERO                     (GL_ZERO)
#define GFXDEVICE_BLEND_FACTOR_ONE                      (GL_ONE)
#define GFXDEVICE_BLEND_FACTOR_SRC_COLOR                (GL_SRC_COLOR)
#define GFXDEVICE_BLEND_FACTOR_ONE_MINUS_SRC_COLOR      (GL_ONE_MINUS_SRC_COLOR)
#define GFXDEVICE_BLEND_FACTOR_DST_COLOR                (GL_DST_COLOR)
#define GFXDEVICE_BLEND_FACTOR_ONE_MINUS_DST_COLOR      (GL_ONE_MINUS_DST_COLOR)
#define GFXDEVICE_BLEND_FACTOR_SRC_ALPHA                (GL_SRC_ALPHA)
#define GFXDEVICE_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA      (GL_ONE_MINUS_SRC_ALPHA)
#define GFXDEVICE_BLEND_FACTOR_DST_ALPHA                (GL_DST_ALPHA)
#define GFXDEVICE_BLEND_FACTOR_ONE_MINUS_DST_ALPHA      (GL_ONE_MINUS_DST_ALPHA)
#define GFXDEVICE_BLEND_FACTOR_SRC_ALPHA_SATURATE       (GL_SRC_ALPHA_SATURATE)

#if !defined (GL_ARB_imaging)
#define GFXDEVICE_BLEND_FACTOR_CONSTANT_COLOR           (0x8001)
#define GFXDEVICE_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR (0x8002)
#define GFXDEVICE_BLEND_FACTOR_CONSTANT_ALPHA           (0x8003)
#define GFXDEVICE_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA (0x8004)
#else
#define GFXDEVICE_BLEND_FACTOR_CONSTANT_COLOR           (GL_CONSTANT_COLOR)
#define GFXDEVICE_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR (GL_ONE_MINUS_CONSTANT_COLOR)
#define GFXDEVICE_BLEND_FACTOR_CONSTANT_ALPHA           (GL_CONSTANT_ALPHA)
#define GFXDEVICE_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA (GL_ONE_MINUS_CONSTANT_ALPHA)
#endif

#define GFXDEVICE_OPENED 0x00020001
#define GFXDEVICE_ACTIVE 0x00020002
#define GFXDEVICE_ICONIFIED 0x00020003
#define GFXDEVICE_ACCELERATED 0x00020004
#define GFXDEVICE_RED_BITS 0x00020005
#define GFXDEVICE_GREEN_BITS 0x00020006
#define GFXDEVICE_BLUE_BITS 0x00020007
#define GFXDEVICE_ALPHA_BITS 0x00020008
#define GFXDEVICE_DEPTH_BITS 0x00020009
#define GFXDEVICE_STENCIL_BITS 0x0002000A
#define GFXDEVICE_REFRESH_RATE 0x0002000B
#define GFXDEVICE_ACCUM_RED_BITS 0x0002000C
#define GFXDEVICE_ACCUM_GREEN_BITS 0x0002000D
#define GFXDEVICE_ACCUM_BLUE_BITS 0x0002000E
#define GFXDEVICE_ACCUM_ALPHA_BITS 0x0002000F
#define GFXDEVICE_AUX_BUFFERS 0x00020010
#define GFXDEVICE_STEREO 0x00020011
#define GFXDEVICE_WINDOW_NO_RESIZE 0x00020012
#define GFXDEVICE_FSAA_SAMPLES 0x00020013

#define GFXDEVICE_FACE_TYPE_FRONT           (GL_FRONT)
#define GFXDEVICE_FACE_TYPE_BACK            (GL_BACK)
#define GFXDEVICE_FACE_TYPE_FRONT_AND_BACK  (GL_FRONT_AND_BACK)

#endif // OPENGL_DEVICE_DEFINES_H
