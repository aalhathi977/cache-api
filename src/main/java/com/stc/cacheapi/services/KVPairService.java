package com.stc.cacheapi.services;

import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

@Service
public class KVPairService {

    final RedisTemplate<String, Serializable> redisTemplate;
    private static final String SERVICE_PREFIX = "KV_";

    public KVPairService(RedisTemplate<String, Serializable> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    public List<Object> get(String key , Integer ttl){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisTemplate.executePipelined(new SessionCallback<>() {
            @Override
            public List<Object> execute(RedisOperations operations) throws DataAccessException {
                if (Objects.nonNull(ttl)) {
                    operations.opsForValue().getAndExpire(prefixedKey, ttl, TimeUnit.SECONDS);
                } else {
                    operations.opsForValue().get(prefixedKey);
                }
                return null;
            }
        });
    }

    public List<Object> put(String key , String body , Integer ttl){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisTemplate.executePipelined(new SessionCallback<>() {
            @Override
            public List<Object> execute(RedisOperations operations) throws DataAccessException {
                if (Objects.nonNull(ttl)) {
                    operations.opsForValue().setIfPresent(prefixedKey, body ,ttl, TimeUnit.SECONDS);
                } else {
                    operations.opsForValue().setIfPresent(prefixedKey,body);
                }
                return null;
            }
        });
    }

    public List<Object> post(String key , String body , Integer ttl){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisTemplate.executePipelined(new SessionCallback<>() {
            @Override
            public List<Object> execute(RedisOperations operations) throws DataAccessException {
                operations.opsForValue().setIfAbsent(prefixedKey, body ,ttl, TimeUnit.SECONDS);
                return null;
            }
        });
    }

    public List<Object> delete(String key){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisTemplate.executePipelined(new SessionCallback<>() {
            @Override
            public List<Object> execute(RedisOperations operations) throws DataAccessException {
                operations.opsForValue().getAndDelete(prefixedKey);
                return null;
            }
        });
    }
}
