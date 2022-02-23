package com.stc.cacheapi.utils;

import com.stc.cacheapi.exceptions.IllegalParamException;
import org.springframework.util.StringUtils;

import java.util.Objects;

public class ValidationUtils {

    public static boolean isInt(String str) {
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

    public static boolean isLong(String str) {
        if (!StringUtils.hasText(str)) {
            return false;
        }
        try {
            Long.parseLong(str);
        } catch (NumberFormatException nfe) {
            return false;
        }
        return true;
    }

    public static Integer sanitizeTTL(String ttl){
        if (Objects.nonNull(ttl))
            if (!ValidationUtils.isInt(ttl) || Long.parseLong(ttl) <= 0) // eliminate text and negative numbers
                throw new IllegalParamException("4001", "TTL need to be a positive number , to delete a key use delete service");
            else
                return Integer.parseInt(ttl);
        else
            return null;

    }

    // start - Counter Service - post
    public static Long sanitizeStart(String start){
        if (!ValidationUtils.isLong(start)) // eliminate text
            throw new IllegalParamException("4007", "body must be an integer between " + Long.MAX_VALUE + " and " + Long.MIN_VALUE + " .");
        else
            return Long.parseLong(start);
    }

    // jump - Counter Service - put
    public static Long sanitizeJump(String jump){
        if (!ValidationUtils.isLong(jump)) // eliminate text
            throw new IllegalParamException("4006", "body must be an integer between " + Long.MAX_VALUE + " and " + Long.MIN_VALUE + " .");
        else
            return Long.parseLong(jump);
    }


    public static Integer sanitizeDBIndex(String dbIndex){
        if (Objects.nonNull(dbIndex))
            // eliminate text and negative numbers
            if (!ValidationUtils.isInt(dbIndex) || Integer.parseInt(dbIndex) < 0)
                throw new IllegalParamException("4003", "db_index is incorrect");
            else
                return Integer.parseInt(dbIndex);
        else
            throw new IllegalParamException("4002","db_index is missing");
    }

    public static String sanitizeKey(String key){
        if (Objects.isNull(key) || !StringUtils.hasText(key))
            throw new IllegalParamException("4004", "key is missing or incorrect");
        return key ;
    }

    public static String sanitizeValue(String value){
        if (Objects.isNull(value) || !StringUtils.hasText(value))
            throw new IllegalParamException("4005", "value can not be empty");
        return value ;
    }

}
