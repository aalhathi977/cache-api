package com.stc.cacheapi.controllers;

import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.services.CounterTrackingService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Objects;

import static com.stc.cacheapi.utils.ValidationUtils.*;

@RestController
@RequestMapping("/v1/counters/{db_index}/{counter}")
public class CounterTrackingController {
    private final CounterTrackingService counterTrackingService ;

    public CounterTrackingController(CounterTrackingService counterTrackingService) {
        this.counterTrackingService = counterTrackingService;
    }


    @GetMapping
    ResponseEntity<?> get (@PathVariable("db_index") String dbIndex , @PathVariable String counter,String ttl){
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);
        String sanitized_counter = sanitizeKey(counter);

        Object result = counterTrackingService.get(sanitized_dbIndex,sanitized_counter,sanitized_ttl);

        if (Objects.isNull(result))
            throw new KeyNotFoundException("4046","the provided counter does not exist");
        else
            return ResponseEntity.ok(result);
    }

    @PutMapping
    ResponseEntity<?> put (@PathVariable("db_index") String dbIndex,@PathVariable String counter,String ttl){
        // validation
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);
        String sanitized_counter = sanitizeKey(counter);

        // call the get service
        counterTrackingService.update(sanitized_dbIndex, sanitized_counter, sanitized_ttl);

        // parse the result and return appropriate http
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @PostMapping
    ResponseEntity<?> post (@PathVariable("db_index") String dbIndex,@PathVariable String counter,String ttl){
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);
        if (sanitized_ttl == null)
            sanitized_ttl = 900 ;
        String sanitized_counter = sanitizeKey(counter);

        counterTrackingService.create(sanitized_dbIndex,sanitized_counter,sanitized_ttl);

        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @DeleteMapping
    ResponseEntity<?> delete (@PathVariable("db_index") String dbIndex,@PathVariable String counter){
        String sanitized_counter = sanitizeKey(counter);
        Integer sanitized_dbIndex = sanitizeDBIndex(dbIndex);

        Boolean isDeleted = counterTrackingService.delete(sanitized_dbIndex,sanitized_counter);

        if (isDeleted)
            return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
        else
            throw new KeyNotFoundException("4042","the provided counter does not exist");
    }

}
