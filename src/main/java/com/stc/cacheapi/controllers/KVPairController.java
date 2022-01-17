package com.stc.cacheapi.controllers;

import com.stc.cacheapi.exceptions.ApplicationException;
import com.stc.cacheapi.exceptions.IllegalParamException;
import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.services.KVPairService;
import com.stc.cacheapi.utils.ValidationUtils;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.data.redis.core.RedisCallback;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.Objects;

@RestController
public class KVPairController {

    private final KVPairService kvPairService ;

    public KVPairController(KVPairService kvPairService) {
        this.kvPairService = kvPairService;
    }

    @GetMapping("/v1/kv-pairs/{db_index}/{key}")
    ResponseEntity<?> get(@PathVariable String key, String ttl) {

        // ttl validation
        Integer parsed_ttl = parseTTL(ttl) ;

        // key validation
        if (Objects.isNull(key) || !StringUtils.hasText(key))
            throw new IllegalParamException("4004", "key is missing or incorrect");

        // call the get service
        List<Object> results = kvPairService.get(key,parsed_ttl);

        // parse the result and return appropriate http
        if (Objects.isNull(results.get(0))){
            throw new KeyNotFoundException();
        }else {
            return ResponseEntity.ok(results.get(0));
        }
    }

    @PutMapping("/v1/kv-pairs/{db_index}/{key}")
    ResponseEntity<?> put(@PathVariable String key, String ttl , @RequestBody(required = false) String body ) {
        // ttl validation
        Integer parsed_ttl = parseTTL(ttl) ;

        // key validation
        if (Objects.isNull(key) || !StringUtils.hasText(key))
            throw new IllegalParamException("4004", "key is missing or incorrect");

        // body validation
        if (Objects.isNull(body) || !StringUtils.hasText(body))
            throw new IllegalParamException("4005", "value can not be empty");

        // call the get service
        List<Object> results = kvPairService.put(key, body, parsed_ttl);

        // parse the result and return appropriate http
        if (Boolean.FALSE.equals(results.get(0))){
            throw new KeyNotFoundException();
        }else {
            return ResponseEntity.status(HttpStatus.CREATED).build();
        }

    }

    @PostMapping("/v1/kv-pairs/{db_index}/{key}")
    ResponseEntity<?> post(@PathVariable String key, String ttl , @RequestBody(required = false) String body ) {
        // ttl validation
        Integer parsed_ttl = parseTTL(ttl) ;
        if (parsed_ttl == null)
            parsed_ttl = 900 ;

        // key validation
        if (Objects.isNull(key) || !StringUtils.hasText(key))
            throw new IllegalParamException("4004", "key is missing or incorrect");

        // body validation
        if (Objects.isNull(body) || !StringUtils.hasText(body))
            throw new IllegalParamException("4005", "value can not be empty");

        // call the get service
        List<Object> results = kvPairService.post(key, body, parsed_ttl);

        // parse the result and return appropriate http
        if (Boolean.FALSE.equals(results.get(0))){
            throw new KeyAlreadyExistException();
        }else {
            return ResponseEntity.status(HttpStatus.CREATED).build();
        }

    }

    @DeleteMapping("/v1/kv-pairs/{db_index}/{key}")
    ResponseEntity<?> delete(@PathVariable String key) {
        // key validation
        if (Objects.isNull(key) || !StringUtils.hasText(key))
            throw new IllegalParamException("4004", "key is missing or incorrect");

        // call the get service
        List<Object> results = kvPairService.delete(key);

        // parse the result and return appropriate http
        if (results.get(0) == null){
            throw new KeyNotFoundException();
        }else {
            return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
        }

    }


    private Integer parseTTL(String ttl){
        if (Objects.nonNull(ttl))
            if (!ValidationUtils.isNumeric(ttl) || Integer.parseInt(ttl) <= 0) // eliminate text and negative numbers
                throw new IllegalParamException("4001", "TTL need to be a positive number , to delete a key use delete service");
            else
                return Integer.parseInt(ttl);
        else
            return null;

    }

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
}
