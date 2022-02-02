package com.stc.cacheapi.controllers;

import com.stc.cacheapi.configs.RedisConnection;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.exceptions.UnknownGeneralRedisException;
import com.stc.cacheapi.parsers.BasicAuthenticationParser;
import com.stc.cacheapi.services.CounterTrackingService;
import io.lettuce.core.RedisCommandExecutionException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;
import java.util.Objects;

import static com.stc.cacheapi.utils.ValidationUtils.*;

@RestController
@RequestMapping("/v1/counters/{db_index}/{counter}")
@Slf4j
public class CounterTrackingController {
    private final CounterTrackingService counterTrackingService ;
    final RedisConnection redisConnection ;

    public CounterTrackingController(CounterTrackingService counterTrackingService, RedisConnection redisConnection) {
        this.counterTrackingService = counterTrackingService;
        this.redisConnection = redisConnection;
    }


    @GetMapping
    ResponseEntity<?> get (@PathVariable("db_index") String dbIndex , @PathVariable String counter, String ttl ,
                           @RequestHeader(value = "Authorization" ) BasicAuthenticationParser parser){
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);
        String sanitized_counter = sanitizeKey(counter);

        Object result = counterTrackingService.get(sanitized_dbIndex,sanitized_counter,sanitized_ttl,parser);

        if (Objects.isNull(result))
            throw new KeyNotFoundException("4046","the provided counter does not exist");
        else
            return ResponseEntity.ok(result);
    }

    @PutMapping
    ResponseEntity<?> put (@PathVariable("db_index") String dbIndex,@PathVariable String counter,String ttl,
                           @RequestHeader(value = "Authorization" ) BasicAuthenticationParser parser){
        // validation
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);
        String sanitized_counter = sanitizeKey(counter);

        // call the get service
        Object isUpdated = counterTrackingService.update(sanitized_dbIndex, sanitized_counter, sanitized_ttl,parser);

        // parse the result and return appropriate http
        if (Boolean.FALSE.equals(isUpdated))
            throw new UnknownGeneralRedisException("5005","Issue setting ttl or counter does not exist");
        else
            return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @PostMapping
    ResponseEntity<?> post (@PathVariable("db_index") String dbIndex,@PathVariable String counter,String ttl,
                            @RequestHeader(value = "Authorization" ) BasicAuthenticationParser parser){
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);
        if (sanitized_ttl == null)
            sanitized_ttl = 900 ;
        String sanitized_counter = sanitizeKey(counter);

        Object isCreated = counterTrackingService.create(sanitized_dbIndex,sanitized_counter,sanitized_ttl,parser);

        if (Boolean.FALSE.equals(isCreated))
            throw new UnknownGeneralRedisException("5004","Issue setting ttl or counter creation failed");
        else
            return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @DeleteMapping
    ResponseEntity<?> delete (@PathVariable("db_index") String dbIndex,@PathVariable String counter,
                              @RequestHeader(value = "Authorization" ) BasicAuthenticationParser parser){
        String sanitized_counter = sanitizeKey(counter);
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);

        Object isDeleted = counterTrackingService.delete(sanitized_dbIndex,sanitized_counter,parser);

        if (Boolean.TRUE.equals(isDeleted))
            return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
        else
            throw new KeyNotFoundException("4042","the provided counter does not exist");
    }

}
