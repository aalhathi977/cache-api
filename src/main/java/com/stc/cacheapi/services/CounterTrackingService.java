package com.stc.cacheapi.services;

import com.stc.cacheapi.configs.RedisConnection;
import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.parsers.BasicAuthenticationParser;
import io.lettuce.core.GetExArgs;
import io.lettuce.core.RedisCommandExecutionException;
import io.lettuce.core.RedisCommandTimeoutException;
import io.lettuce.core.RedisException;
import lombok.SneakyThrows;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.Collections;
import java.util.Objects;

@Service
public class CounterTrackingService {
    final RedisTemplate<String, Serializable> redisTemplate = new RedisTemplate<>();
    private static final String SERVICE_PREFIX = "CT_";
    final RedisConnection redisConnection ;


    public CounterTrackingService(RedisConnection redisConnection) {
        this.redisConnection = redisConnection;
    }

    @SneakyThrows
    @Retryable(maxAttempts = 2, include = {RedisCommandExecutionException.class,RedisCommandTimeoutException.class}, backoff = @Backoff(value = 0))
    public Object get(Integer dbIndex , String counter , Integer ttl, BasicAuthenticationParser parser){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        return redisConnection.executeAsyncCommands(parser,dbIndex,(async) -> {
            if (Objects.nonNull(ttl)) {
                return async.getex(prefixedCounter, GetExArgs.Builder.ex(ttl)).get();
            } else {
                return async.get(prefixedCounter).get();
            }
        });
    }

    public Boolean update(Integer dbIndex ,String counter , Integer ttl){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        RedisScript<Boolean> existsScript = RedisScript.of("redis.call('SELECT'," + dbIndex + ");" +
                "return redis.call('EXISTS', KEYS[1])",Boolean.class);
        Boolean exists = redisTemplate.execute(existsScript,Collections.singletonList(prefixedCounter));

        if (Boolean.FALSE.equals(exists)){
            throw new KeyNotFoundException("4041","the provided counter does not exist");
        }else {
            if (Objects.nonNull(ttl)) {
                RedisScript<Boolean> updateScript = RedisScript.of("redis.call('SELECT'," + dbIndex + ");" +
                        "redis.call('INCR', KEYS[1])" +
                        "redis.call('EXPIRE' , KEYS[1] , ARGV[1])", Boolean.class);
                return redisTemplate.execute(updateScript, Collections.singletonList(prefixedCounter), ttl.toString());
            }else {
                RedisScript<Boolean> updateScript = RedisScript.of("redis.call('SELECT'," + dbIndex + ");" +
                        "return redis.call('INCR', KEYS[1])" , Boolean.class);
                return redisTemplate.execute(updateScript, Collections.singletonList(prefixedCounter));
            }
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
        final String prefixedCounter = SERVICE_PREFIX + counter;

        RedisScript<Boolean> existsScript = RedisScript.of("redis.call('SELECT'," + dbIndex + ");" +
                "return redis.call('UNLINK', KEYS[1])",Boolean.class);
        return redisTemplate.execute(existsScript,Collections.singletonList(prefixedCounter));
    }

    @Recover
    public void recover(RedisException e){
        throw e;
    }
}
