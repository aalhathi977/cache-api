package com.stc.cacheapi.services;

import com.stc.cacheapi.configs.RedisConnection;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.parsers.BasicAuthenticationParser;
import io.lettuce.core.*;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

@Service
public class KVPairService {

    final RedisTemplate<String, Serializable> redisTemplate = new RedisTemplate<>() ;
    private static final String SERVICE_PREFIX = "KV_";
    final RedisConnection redisConnection ;
    private static final int FUTURE_TIMEOUT = 30 ; // SECONDS

    public KVPairService(RedisConnection redisConnection) {
        this.redisConnection = redisConnection;
    }

    @Retryable( maxAttempts = 2,
            include = {RedisCommandExecutionException.class, RedisCommandTimeoutException.class },
            backoff = @Backoff(value = 0))
    public Object get(Integer dbIndex , String key , Integer ttl, BasicAuthenticationParser parser){
        final String prefixedKey = SERVICE_PREFIX + key;

        return redisConnection.executeAsyncCommands(parser,dbIndex,(async) -> {
            if (Objects.nonNull(ttl)) {
                return async.getex(prefixedKey, GetExArgs.Builder.ex(ttl)).get(FUTURE_TIMEOUT,TimeUnit.SECONDS);
            } else {
                return async.get(prefixedKey).get(FUTURE_TIMEOUT,TimeUnit.SECONDS);
            }
        });
    }



    @Retryable( maxAttempts = 2,
            include = { RedisCommandExecutionException.class, RedisCommandTimeoutException.class},
            backoff = @Backoff(value = 0))
    public Boolean update(Integer dbIndex , String key , String value , Integer ttl , BasicAuthenticationParser parser){
        final String prefixedKey = SERVICE_PREFIX + key;




        if (Objects.nonNull(ttl))
            return redisTemplate.opsForValue().setIfPresent(prefixedKey, value ,ttl, TimeUnit.SECONDS);
        else {
            RedisScript<Boolean> script = RedisScript.of("return redis.call('SET', KEYS[1], ARGV[1], 'XX','KEEPTTL')",Boolean.class);
            return redisTemplate.execute(script, Collections.singletonList(prefixedKey), value);
        }
    }

    public Boolean create(Integer dbIndex , String key , String value , Integer ttl, BasicAuthenticationParser parser){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisTemplate.opsForValue().setIfAbsent(prefixedKey, value ,ttl, TimeUnit.SECONDS);
    }

    public Boolean delete(Integer dbIndex , String key, BasicAuthenticationParser parser){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisTemplate.unlink(prefixedKey);
    }


    @Recover
    public void recover(RedisException e){
        throw e;
    }
}
