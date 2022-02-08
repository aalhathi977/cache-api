package com.stc.cacheapi.exceptions;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class UnknownGeneralRedisException extends ApplicationException{
    private String code = "5000"; // INTERNAL_SERVER_ERROR
    private String message = "General Redis Error";
}
