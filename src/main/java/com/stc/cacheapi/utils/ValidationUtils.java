package com.stc.cacheapi.utils;

import org.springframework.util.StringUtils;

public class ValidationUtils {
    public static boolean isNumeric(String str) {
        if (!StringUtils.hasText(str)) {
            return false;
        }
        try {
            Integer.parseInt(str);
        } catch (NumberFormatException nfe) {
            return false;
        }
        return true;
    }
}
