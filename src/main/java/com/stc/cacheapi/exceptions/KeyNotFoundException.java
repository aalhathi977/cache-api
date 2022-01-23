package com.stc.cacheapi.exceptions;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class KeyNotFoundException extends ApplicationException{
    private String code = "4040";
    private String message = "the provided key does not exist";

    public KeyNotFoundException(String code) {
        this.code = code ;
    }
}
