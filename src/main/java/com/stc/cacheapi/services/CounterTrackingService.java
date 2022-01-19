package com.stc.cacheapi.services;

import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
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

    public List<Object> update(String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;
        if (Boolean.FALSE.equals(redisTemplate.hasKey(prefixedCounter))) {
            throw new KeyNotFoundException("4041","the provided counter does not exist");
        }else {
            if (Objects.nonNull(ttl))
                return redisTemplate.executePipelined(new SessionCallback<>() {
                    @Override
                    public List<Object> execute(RedisOperations operations) throws DataAccessException {
                        operations.multi();
                        operations.opsForValue().increment(prefixedCounter);
                        operations.expire(prefixedCounter, ttl, TimeUnit.SECONDS);
                        operations.exec();
                        return null;
                    }
                });
            else
                return List.of(redisTemplate.opsForValue().increment(prefixedCounter));

        }
    }

    public List<Object> create(String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        if (Boolean.TRUE.equals(redisTemplate.hasKey(prefixedCounter))) {
            throw new KeyAlreadyExistException("4091","the provided counter already exist");
        }else {
            return redisTemplate.executePipelined(new SessionCallback<>() {
                @Override
                public List<Object> execute(RedisOperations operations) throws DataAccessException {
                    operations.multi();
                    operations.opsForValue().increment(prefixedCounter);
                    operations.expire(prefixedCounter, ttl, TimeUnit.SECONDS);
                    operations.exec();
                    return null;
                }
            });
        }
    }

    public Boolean delete(String counter){
        return redisTemplate.unlink(SERVICE_PREFIX + counter);
    }
}
