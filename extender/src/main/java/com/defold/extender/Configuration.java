package com.defold.extender;

import java.util.Map;

public class Configuration {
    public Map<String, PlatformConfig> platforms;
    public Map<String, Object> context;
    public String[] exportedSymbols;
    public String[] includes;
    public String main;
}
