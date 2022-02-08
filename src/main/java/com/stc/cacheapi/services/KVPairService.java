package com.stc.cacheapi.services;

import com.stc.cacheapi.configs.RedisConnection;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.parsers.BasicAuthenticationParser;
import io.lettuce.core.*;
import org.springframework.data.redis.connection.RedisStringCommands;
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

        Object isUpdated = redisConnection.executeAsyncCommands(parser,dbIndex, (async) -> {
            if (Objects.nonNull(ttl))
                return async.set(prefixedKey, value, SetArgs.Builder.xx().ex(ttl))
                        .get(FUTURE_TIMEOUT,TimeUnit.SECONDS);
            else {
                return async.set(prefixedKey, value, SetArgs.Builder.xx().keepttl())
                        .get(FUTURE_TIMEOUT,TimeUnit.SECONDS);
            }
        });

        return Objects.nonNull(isUpdated) && isUpdated.equals("OK") ;
    }

    public Object create(Integer dbIndex , String key , String value , Integer ttl, BasicAuthenticationParser parser){
        final String prefixedKey = SERVICE_PREFIX + key;

        Object isCreated = redisConnection.executeAsyncCommands(parser,dbIndex,
                (async) -> async.set(prefixedKey, value, SetArgs.Builder.nx().ex(ttl)).get(FUTURE_TIMEOUT,TimeUnit.SECONDS)
        );

        return Objects.nonNull(isCreated) && isCreated.equals("OK") ;
    }

    public Object delete(Integer dbIndex , String key, BasicAuthenticationParser parser){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisConnection.executeAsyncCommands(parser,dbIndex,(async) -> {
            RedisFuture<Long> value = async.unlink(prefixedKey);
            return ( value.get(FUTURE_TIMEOUT, TimeUnit.SECONDS) > 0 ) ;
        });
    }


    @Recover
    public void recover(RedisException e){
        throw e;
    }
}
