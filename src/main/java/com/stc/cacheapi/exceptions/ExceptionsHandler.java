package com.stc.cacheapi.exceptions;

import io.lettuce.core.RedisCommandExecutionException;
import io.lettuce.core.RedisException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import java.util.Map;

@RestControllerAdvice
public class ExceptionsHandler extends ResponseEntityExceptionHandler {


    @ExceptionHandler(IllegalParamException.class)
    ResponseEntity<?> illegalHandler(IllegalParamException e) {
        return ResponseEntity.badRequest()
                .body(Map.of(
                        "code", e.getCode(),
                        "message", e.getMessage()
                ));
    }

    @ExceptionHandler(KeyNotFoundException.class)
    ResponseEntity<?> keyNotFoundHandler(KeyNotFoundException e) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(Map.of(
                "code", e.getCode(),
                "message", e.getMessage()
        ));
    }

    @ExceptionHandler(KeyAlreadyExistException.class)
    ResponseEntity<?> keyAlreadyExistHandler(KeyAlreadyExistException e) {
        return ResponseEntity.status(HttpStatus.CONFLICT).body(Map.of(
                "code", e.getCode(),
                "message", e.getMessage()
        ));
    }

    // REDIS GENERAL EXCEPTION
    @ExceptionHandler(RedisException.class)
    ResponseEntity<?> keyAlreadyExistHandler(RedisException e) {
        return ResponseEntity
                .status(HttpStatus.BAD_GATEWAY)
                .header("x-error",e.getMessage())
                .body(Map.of(
                        "code", HttpStatus.BAD_GATEWAY.value() + "1",
                        "message", e.getMessage()
                ));
    }

}
