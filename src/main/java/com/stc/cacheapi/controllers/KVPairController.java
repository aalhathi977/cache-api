package com.stc.cacheapi.controllers;

import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.services.KVPairService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;

import static com.stc.cacheapi.utils.ValidationUtils.*;

@RestController
@RequestMapping("/v1/kv-pairs/{db_index}/{key}")
public class KVPairController {

    private final KVPairService kvPairService ;

    public KVPairController(KVPairService kvPairService) {
        this.kvPairService = kvPairService;
    }

    @GetMapping
    ResponseEntity<?> get(@PathVariable String key, String ttl) {
        // validation
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        String sanitized_key = sanitizeKey(key);

        // call the get service
        List<Object> results = kvPairService.get(sanitized_key,sanitized_ttl);

        // parse the result and return appropriate http
        if (Objects.isNull(results.get(0))){
            throw new KeyNotFoundException();
        }else {
            return ResponseEntity.ok(results.get(0));
        }
    }

    @PutMapping
    ResponseEntity<?> put(@PathVariable String key, String ttl , @RequestBody(required = false) String body ) {

        // validation
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        String sanitized_key = sanitizeKey(key);
        String sanitized_value = sanitizeValue(body);

        // call the get service
        List<Object> results = kvPairService.update(sanitized_key, sanitized_value, sanitized_ttl);

        // parse the result and return appropriate http
        if (Boolean.FALSE.equals(results.get(0))){
            throw new KeyNotFoundException();
        }else {
            return ResponseEntity.status(HttpStatus.CREATED).build();
        }

    }

    @PostMapping
    ResponseEntity<?> post(@PathVariable String key, String ttl , @RequestBody(required = false) String body ) {
        // ttl validation
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        if (sanitized_ttl == null)
            sanitized_ttl = 900 ;
        String sanitized_key = sanitizeKey(key);
        String sanitized_value = sanitizeValue(body);

        // call the get service
        List<Object> results = kvPairService.create(sanitized_key, sanitized_value, sanitized_ttl);

        // parse the result and return appropriate http
        if (Boolean.FALSE.equals(results.get(0))){
            throw new KeyAlreadyExistException();
        }else {
            return ResponseEntity.status(HttpStatus.CREATED).build();
        }

    }

    @DeleteMapping
    ResponseEntity<?> delete(@PathVariable String key) {
        // validation
        String sanitized_key = sanitizeKey(key);

        // call the get service
        List<Object> results = kvPairService.delete(sanitized_key);

        // parse the result and return appropriate http
        if (results.get(0) == null){
            throw new KeyNotFoundException();
        }else {
            return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
        }

    }

}
