package com.stc.cacheapi.exceptions;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class KeyAlreadyExistException extends ApplicationException{
    private String code = "4090";
    private String message = "the provided key already exist";
}
