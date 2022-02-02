package com.stc.cacheapi.exceptions;

import io.lettuce.core.RedisException;
import org.springframework.beans.TypeMismatchException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MissingRequestHeaderException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutionException;

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



    @ExceptionHandler(MissingRequestHeaderException.class)
    ResponseEntity<?> missingRequestHeader(MissingRequestHeaderException e) {
        return ResponseEntity.status(HttpStatus.CONFLICT).body(Map.of(
                "code", "4006",
                "message", e.getHeaderName() + " ( Basic Authentication ) is required "
        ));
    }


    // REDIS GENERAL EXCEPTION
    @ExceptionHandler(RedisException.class)
    ResponseEntity<?> keyAlreadyExistHandler(RedisException e) {

        // handler caused exception
        if (Objects.nonNull(e.getCause()) && Objects.nonNull(e.getCause().getMessage())) {
            // user password is wrong
            if (e.getCause().getMessage().contains("WRONGPASS")) {
                return ResponseEntity
                        .status(HttpStatus.UNAUTHORIZED)
                        .body(Map.of(
                                "code", HttpStatus.UNAUTHORIZED.value() + "1",
                                "message", "Invalid redis username or password"
                        ));
            }
        }

        return ResponseEntity
                .status(HttpStatus.BAD_GATEWAY)
                .header("x-error",e.getMessage())
                .body(Map.of(
                        "code", HttpStatus.BAD_GATEWAY.value() + "1",
                        "message", e.getMessage()
                ));
    }


    // 500 - Internal Server Error
    @ExceptionHandler(ExecutionException.class)
    ResponseEntity<?> executionExceptionHandler(ExecutionException e) {

        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .header("x-error",e.getCause().getMessage())
                .body(Map.of(
                        "code", HttpStatus.INTERNAL_SERVER_ERROR.value() + "1",
                        "message", e.getCause().getMessage()
                ));
    }

    @ExceptionHandler(InterruptedException.class)
    ResponseEntity<?> interruptException(InterruptedException e) {
        return ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR)
                .header("x-error",e.getMessage())
                .body(Map.of(
                        "code", HttpStatus.INTERNAL_SERVER_ERROR.value() + "2",
                        "message", e.getMessage()
                ));
    }



    // 401 - unauthorized
    @ExceptionHandler(BasicAuthenticationParsingException.class)
    ResponseEntity<?> basicAuthenticationParsing(BasicAuthenticationParsingException e) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                "code", e.getCode(),
                "message", e.getMessage()
        ));
    }

    @Override
    protected ResponseEntity<Object> handleTypeMismatch(TypeMismatchException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        // handling BasicAuthenticationParsingException
        Throwable throwable = ex.getCause().getCause();
        if (throwable instanceof BasicAuthenticationParsingException) {
            BasicAuthenticationParsingException castedThrowable = (BasicAuthenticationParsingException) throwable;
            return ResponseEntity
                    .status(HttpStatus.UNAUTHORIZED)
                    .body(Map.of(
                            "code", castedThrowable.getCode(),
                            "message", castedThrowable.getMessage()
                    ));
        }else {
            return super.handleTypeMismatch(ex, headers, status, request);
        }
    }
}
