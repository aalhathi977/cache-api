package com.stc.cacheapi.services;

import com.stc.cacheapi.exceptions.KeyNotFoundException;
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
public class CounterTrackingService {
    final RedisTemplate<String, Serializable> redisTemplate;
    private static final String SERVICE_PREFIX = "CT_";

    public CounterTrackingService(RedisTemplate<String, Serializable> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    public List<Object> get(String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;
        return redisTemplate.executePipelined(new SessionCallback<>() {
            @Override
            public List<Object> execute(RedisOperations operations) throws DataAccessException {
                if (Objects.nonNull(ttl)) {
                    operations.opsForValue().getAndExpire(prefixedCounter, ttl, TimeUnit.SECONDS);
                } else {
                    operations.opsForValue().get(prefixedCounter);
                }
                return null;
            }
        });
    }

    public List<Object> put(String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        redisTemplate.watch(prefixedCounter);
        if (Boolean.FALSE.equals(redisTemplate.hasKey(prefixedCounter))) {
            redisTemplate.unwatch();
            throw new KeyNotFoundException();
        }else {
            List<Object> results ;
            if (Objects.nonNull(ttl)) {
                results = redisTemplate.executePipelined(new SessionCallback<>() {
                    @Override
                    public List<Object> execute(RedisOperations operations) throws DataAccessException {
                        operations.watch(prefixedCounter);
                        operations.multi();
                        operations.opsForValue().increment(prefixedCounter);
                        operations.expire(prefixedCounter, ttl, TimeUnit.SECONDS);
                        operations.exec();
                        operations.unwatch();
                        return null;
                    }
                });
            } else {
                results = List.of(redisTemplate.opsForValue().increment(prefixedCounter));
            }
            redisTemplate.unwatch();
            return results ;
        }
    }

    public List<Object> post(String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;
        return redisTemplate.executePipelined(new SessionCallback<>() {
            @Override
            public List<Object> execute(RedisOperations operations) throws DataAccessException {
                operations.multi();
                operations.opsForValue().increment(prefixedCounter);
                operations.expire(prefixedCounter ,ttl, TimeUnit.SECONDS);
                operations.exec();
                return null;
            }
        });
    }

    public List<Object> delete(String counter){
        final String prefixedCounter = SERVICE_PREFIX + counter;
        return redisTemplate.executePipelined(new SessionCallback<>() {
            @Override
            public List<Object> execute(RedisOperations operations) throws DataAccessException {
                operations.opsForValue().getAndDelete(prefixedCounter);
                return null;
            }
        });
    }
}
