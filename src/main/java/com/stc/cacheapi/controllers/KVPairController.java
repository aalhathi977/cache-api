package com.stc.cacheapi.controllers;

import com.stc.cacheapi.exceptions.IllegalParamException;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisCallback;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.types.RedisClientInfo;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

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
    ResponseEntity<?> KVPair(@PathVariable int db_index, @PathVariable String key, Integer ttl) {
        final String prefixedKey = SERVICE_PREFIX + key;

        if (Objects.isNull(ttl) || ttl < 0)
            throw new IllegalParamException("4001", "TTL can not be negative , to delete a key use delete service");



        List<Object> results = redisTemplate.executePipelined(new SessionCallback<>() {
            @Override
            public List<Object> execute(RedisOperations operations) throws DataAccessException {
                var valueOps = operations.opsForValue();
                // Auth

                // additional validation for the ttl is needed
                if (ttl > 0) {
                    valueOps.getAndExpire(prefixedKey, ttl, TimeUnit.SECONDS);
                } else {
                    valueOps.get(prefixedKey);
                }

                return null;
            }
        });
        System.out.println(Arrays.toString(results.toArray()));

        return ResponseEntity.ok("done");
    }

    @ExceptionHandler(IllegalParamException.class)
    ResponseEntity<?> outOfRangeHandler(IllegalParamException e) {
        return ResponseEntity.badRequest()
                .body(Map.of(
                        "code", e.getCode(),
                        "message", e.getMessage()
                ));
    }


}
