package com.stc.cacheapi.controllers;

import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
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
    ResponseEntity<?> get(@PathVariable String key, String ttl) {
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        String sanitized_key = sanitizeKey(key);

        Object result = kvPairService.get(sanitized_key,sanitized_ttl);

        if (Objects.isNull(result))
            throw new KeyNotFoundException();
        else
            return ResponseEntity.ok(result);
    }

    @PutMapping
    ResponseEntity<?> put(@PathVariable String key, String ttl , @RequestBody(required = false) String body ) {
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        String sanitized_key = sanitizeKey(key);
        String sanitized_value = sanitizeValue(body);

        Boolean isUpdated = kvPairService.update(sanitized_key, sanitized_value, sanitized_ttl);

        if (isUpdated)
            return ResponseEntity.status(HttpStatus.CREATED).build();
        else
            throw new KeyNotFoundException();
    }

    @PostMapping
    ResponseEntity<?> post(@PathVariable String key, String ttl , @RequestBody(required = false) String body ) {
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        if (sanitized_ttl == null)
            sanitized_ttl = 900 ;
        String sanitized_key = sanitizeKey(key);
        String sanitized_value = sanitizeValue(body);

        Boolean isCreated = kvPairService.create(sanitized_key, sanitized_value, sanitized_ttl);

        if (isCreated)
            return ResponseEntity.status(HttpStatus.CREATED).build();
        else
            throw new KeyAlreadyExistException();
    }

    @DeleteMapping
    ResponseEntity<?> delete(@PathVariable String key) {
        String sanitized_key = sanitizeKey(key);

        Boolean isDeleted = kvPairService.delete(sanitized_key);

        if (isDeleted)
            return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
        else
            throw new KeyNotFoundException();
    }

}
