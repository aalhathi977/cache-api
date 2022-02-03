package com.stc.cacheapi.controllers;

import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.parsers.BasicAuthenticationParser;
import com.stc.cacheapi.services.KVPairService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
    ResponseEntity<?> get(@PathVariable("db_index") String dbIndex , @PathVariable String key, String ttl ,
                          @RequestHeader(value = "Authorization" ) BasicAuthenticationParser parser) {
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);
        String sanitized_key = sanitizeKey(key);

        Object result = kvPairService.get(sanitized_dbIndex,sanitized_key,sanitized_ttl,parser);

        if (Objects.isNull(result))
            throw new KeyNotFoundException("4043");
        else
            return ResponseEntity.ok(result);
    }

    @PutMapping
    ResponseEntity<?> put(@PathVariable("db_index") String dbIndex , @PathVariable String key, String ttl ,
                          @RequestBody(required = false) String body , @RequestHeader(value = "Authorization" ) BasicAuthenticationParser parser) {
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        String sanitized_key = sanitizeKey(key);
        String sanitized_value = sanitizeValue(body);
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);

        Boolean isUpdated = kvPairService.update(sanitized_dbIndex,sanitized_key, sanitized_value, sanitized_ttl,parser);

        if (isUpdated)
            return ResponseEntity.status(HttpStatus.CREATED).build();
        else
            throw new KeyNotFoundException("4044");
    }

    @PostMapping
    ResponseEntity<?> post(@PathVariable("db_index") String dbIndex , @PathVariable String key, String ttl ,
                           @RequestBody(required = false) String body , @RequestHeader(value = "Authorization" ) BasicAuthenticationParser parser) {
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        if (sanitized_ttl == null)
            sanitized_ttl = 900 ;
        String sanitized_key = sanitizeKey(key);
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);
        String sanitized_value = sanitizeValue(body);

        Boolean isCreated = kvPairService.create(sanitized_dbIndex,sanitized_key, sanitized_value, sanitized_ttl,parser);

        if (isCreated)
            return ResponseEntity.status(HttpStatus.CREATED).build();
        else
            throw new KeyAlreadyExistException("4092");
    }

    @DeleteMapping
    ResponseEntity<?> delete(@PathVariable("db_index") String dbIndex , @PathVariable String key ,
                             @RequestHeader(value = "Authorization" ) BasicAuthenticationParser parser) {
        String sanitized_key = sanitizeKey(key);
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);

        Boolean isDeleted = kvPairService.delete(sanitized_dbIndex,sanitized_key,parser);

        if (isDeleted)
            return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
        else
            throw new KeyNotFoundException("4045");
    }

}
