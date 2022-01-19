package com.stc.cacheapi.controllers;

import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.services.CounterTrackingService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;

import static com.stc.cacheapi.utils.ValidationUtils.sanitizeKey;
import static com.stc.cacheapi.utils.ValidationUtils.sanitizeTTL;

@RestController
@RequestMapping("/v1/counters/{db_index}/{counter}")
public class CounterTrackingController {
    private final CounterTrackingService counterTrackingService ;

    public CounterTrackingController(CounterTrackingService counterTrackingService) {
        this.counterTrackingService = counterTrackingService;
    }


    @GetMapping
    ResponseEntity<?> get (@PathVariable String counter,String ttl){
        // validation
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        String sanitized_counter = sanitizeKey(counter);

        // call the get service
        Object result = counterTrackingService.get(sanitized_counter,sanitized_ttl);

        // parse the result and return appropriate http
        if (Objects.isNull(result))
            throw new KeyNotFoundException();
        else
            return ResponseEntity.ok(result);
    }

    @PutMapping
    ResponseEntity<?> put (@PathVariable String counter,String ttl){
        // validation
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        String sanitized_counter = sanitizeKey(counter);

        // call the get service
        counterTrackingService.update(sanitized_counter,sanitized_ttl);

        // parse the result and return appropriate http
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @PostMapping
    ResponseEntity<?> post (@PathVariable String counter,String ttl){
        // validation
        Integer sanitized_ttl = sanitizeTTL(ttl) ;
        if (sanitized_ttl == null)
            sanitized_ttl = 900 ;
        String sanitized_counter = sanitizeKey(counter);

        // call the get service
        counterTrackingService.create(sanitized_counter,sanitized_ttl);

        // parse the result and return appropriate http
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @DeleteMapping
    ResponseEntity<?> delete (@PathVariable String counter){
        // validation
        String sanitized_counter = sanitizeKey(counter);

        // call the get service
        Boolean isDeleted = counterTrackingService.delete(sanitized_counter);

        // parse the result and return appropriate http
        if (isDeleted)
            return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
        else
            throw new KeyNotFoundException("4042","the provided counter does not exist");
    }

}
