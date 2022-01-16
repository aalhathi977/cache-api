package com.stc.cacheapi.exceptions;

import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class ApplicationException extends RuntimeException{
    protected String message, code ;
}
