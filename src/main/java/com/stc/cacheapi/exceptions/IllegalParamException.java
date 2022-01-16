package com.stc.cacheapi.exceptions;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class IllegalParamException extends ApplicationException{
    private String code = "4000";
    private String message = "Illegal Param Exception";
}
