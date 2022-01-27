package com.stc.cacheapi.services;

import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.Collections;
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

    public Object get(Integer dbIndex , String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;
        RedisScript<Integer> script ;
        if (Objects.nonNull(ttl)) {
            script = RedisScript.of("redis.call('SELECT'," + dbIndex + ");return " +
                    "redis.call('GETEX', KEYS[1],'EX',ARGV[1])",Integer.class);
            return redisTemplate.execute(script, Collections.singletonList(prefixedCounter),ttl.toString());
        }else {
            script = RedisScript.of(
                    "redis.call('SELECT'," + dbIndex + ");" +
                    "return redis.call('GET', KEYS[1])"
                    ,Integer.class);
            return redisTemplate.execute(script, Collections.singletonList(prefixedCounter));
        }
    }

    public List<Object> update(Integer dbIndex ,String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        RedisScript<Boolean> existsScript = RedisScript.of("redis.call('SELECT'," + dbIndex + ");" +
                "return redis.call('EXISTS', KEYS[1])",Boolean.class);
        Boolean exists = redisTemplate.execute(existsScript,Collections.singletonList(prefixedCounter));

        if (Boolean.TRUE.equals(exists)){
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

    public Boolean create(Integer dbIndex , String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        RedisScript<Boolean> existsScript = RedisScript.of("redis.call('SELECT'," + dbIndex + ");" +
                "return redis.call('EXISTS', KEYS[1])",Boolean.class);
        Boolean exists = redisTemplate.execute(existsScript,Collections.singletonList(prefixedCounter));

        if (Boolean.TRUE.equals(exists))
            throw new KeyAlreadyExistException("4091","the provided counter already exist");
        else {
            RedisScript<Boolean> createScript = RedisScript.of("redis.call('SELECT'," + dbIndex + ");" +
                    "redis.call('INCR', KEYS[1])" +
                    "redis.call('EXPIRE' , KEYS[1] , ARGV[1])",Boolean.class);
            return redisTemplate.execute(createScript,Collections.singletonList(prefixedCounter),ttl.toString());
        }
    }

    public Boolean delete(Integer dbIndex ,String counter){
        return redisTemplate.unlink(SERVICE_PREFIX + counter);
    }
}
