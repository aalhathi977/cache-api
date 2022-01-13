package com.stc.cacheapi.controllers;

import com.stc.cacheapi.exceptions.OutOfRangeDBIndexException;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.StringRedisConnection;
import org.springframework.data.redis.connection.lettuce.LettuceConnection;
import org.springframework.data.redis.core.*;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
public class KVPairController {

    final RedisTemplate<String, Serializable> redisTemplate;
    private static final String SERVICE_PREFIX = "KV_";

    public KVPairController(RedisTemplate<String, Serializable> redisTemplate) {
        this.redisTemplate = redisTemplate;
        this.redisTemplate.setKeySerializer(StringRedisSerializer.UTF_8);
        this.redisTemplate.setValueSerializer(StringRedisSerializer.UTF_8);
    }

    @GetMapping("/v1/kv-pairs/{db_index}/{key}")
    ResponseEntity<?> KVPair(@PathVariable int db_index , @PathVariable String key , Integer ttl){
//        final String prefixedKey = SERVICE_PREFIX + key ;

        // check db_index is a valid int
//        if (db_index < 0 || db_index > 15 )
//            throw new OutOfRangeDBIndexException();

        // by default connect to 0 --> Should be part of the pipeline


        // Without Pipeline
        long without_before = System.nanoTime();
        for (int i = 0 ; i <= 1000 ; i++)
            redisTemplate.getConnectionFactory().getConnection().ping();
        long without_after = System.nanoTime();
        System.out.println("without Pipeline -->" + ( without_after - without_before ) / 1000000000.0 + " Seconds");


        // with pipeline
        long with_before = System.nanoTime();
        redisTemplate.executePipelined((RedisCallback<Object>) connection -> {
            for (int i = 0 ; i <= 1000 ; i++)
                redisTemplate.getConnectionFactory().getConnection().ping();

            return null ;
        });
        long with_after = System.nanoTime();
        System.out.println("with Pipeline -->" + ( with_after - with_before ) / 1000000000.0 + " Seconds");





//        List<Object> results = stringRedisTemplate.executePipelined(new SessionCallback<Object>() {
//            @Override
//            public <K, V> Object execute(RedisOperations<K, V> operations) throws DataAccessException {
//                return null;
//            }
//        });

        // check if there is a ttl
//        List<Object> results = redisTemplate.executePipelined((RedisCallback<Object>) connection -> {
//            // AUTH --> authenticate user
////                redisTemplate.getConnectionFactory()
//
//
//            // SELECT --> if db_index is not zero , switch
//            if (db_index != 0) {
//                connection.select(db_index);
//                //redisTemplate.getConnectionFactory().getConnection().select(db_index);
//            }
//            // EXISTS --> check if exists ( I don't think I need it )
//            System.out.println(connection.keyCommands().exists(prefixedKey.getBytes(StandardCharsets.UTF_8)));
//            //System.out.println(redisTemplate.hasKey(prefixedKey));
//
//            // GET & EXPIRE --> update ttl
//            // additional validation for the ttl is needed
//            if (Objects.nonNull(ttl) && ttl > 0) {
//                System.out.println(connection.getEx(prefixedKey.getBytes(StandardCharsets.UTF_8), Expiration.seconds(ttl)));
//                //System.out.println(redisTemplate.opsForValue().getAndExpire(prefixedKey, Duration.of(ttl, ChronoUnit.SECONDS)));
//                // GET --> get the value
//            }else {
//                System.out.println(connection.get(prefixedKey.getBytes(StandardCharsets.UTF_8)));
//                //System.out.println(redisTemplate.opsForValue().get(prefixedKey));
//            }
//
//
//            return null;
//        });

//        System.out.println(Arrays.toString(results.toArray()));

        return ResponseEntity.ok("done");
    }

    @ExceptionHandler(OutOfRangeDBIndexException.class)
    ResponseEntity<?> outOfRangeHandler(OutOfRangeDBIndexException e) {
        return ResponseEntity.badRequest()
                .body(Map.of(
                        "code", "4001",
                        "message", "db_index should be an integer between 0 and 15"
                ));
    }


}
