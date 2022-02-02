package com.stc.cacheapi.services;

import com.stc.cacheapi.configs.RedisConnection;
import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.parsers.BasicAuthenticationParser;
import io.lettuce.core.*;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

@Service
public class CounterTrackingService {
    private static final String SERVICE_PREFIX = "CT_";
    final RedisConnection redisConnection ;
    private static final int FUTURE_TIMEOUT = 30 ; // SECONDS


    public CounterTrackingService(RedisConnection redisConnection) {
        this.redisConnection = redisConnection;
    }


    @Retryable(maxAttempts = 2, include = {
            RedisCommandExecutionException.class,
            RedisCommandTimeoutException.class
    }, backoff = @Backoff(value = 0))
    public Object get(Integer dbIndex , String counter , Integer ttl, BasicAuthenticationParser parser){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        return redisConnection.executeAsyncCommands(parser,dbIndex,(async) -> {
            if (Objects.nonNull(ttl)) {
                return async.getex(prefixedCounter, GetExArgs.Builder.ex(ttl)).get(FUTURE_TIMEOUT,TimeUnit.SECONDS);
            } else {
                return async.get(prefixedCounter).get(FUTURE_TIMEOUT,TimeUnit.SECONDS);
            }
        });
    }

    @Retryable(maxAttempts = 2, include = {
            RedisCommandExecutionException.class,
            RedisCommandTimeoutException.class
    }, backoff = @Backoff(value = 0))
    public Object update(Integer dbIndex ,String counter , Integer ttl ,BasicAuthenticationParser parser){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        return redisConnection.executeAsyncCommands(parser,dbIndex,(async) -> {
            Long count = async.exists(prefixedCounter).get(FUTURE_TIMEOUT,TimeUnit.SECONDS);
            Boolean exists = count != null ? count > 0 : null;

            if (Boolean.FALSE.equals(exists))
                throw new KeyNotFoundException("4041","the provided counter does not exist");
            else {
                RedisFuture<Long> value = async.incr(prefixedCounter);
                if (Objects.nonNull(ttl)) {
                    RedisFuture<Boolean> isTtlSet = async.expire(prefixedCounter, ttl);
                    return ( value.get(FUTURE_TIMEOUT,TimeUnit.SECONDS) > 0 &&
                            isTtlSet.get(FUTURE_TIMEOUT,TimeUnit.SECONDS) ) ;
                }else {
                    return ( value.get(FUTURE_TIMEOUT,TimeUnit.SECONDS) > 0 ) ;
                }
            }
        });

    }


    @Retryable(maxAttempts = 2, include = {
            RedisCommandExecutionException.class,
            RedisCommandTimeoutException.class
    }, backoff = @Backoff(value = 0))
    public Object create(Integer dbIndex , String counter , Integer ttl,BasicAuthenticationParser parser){
        final String prefixedCounter = SERVICE_PREFIX + counter;

        return redisConnection.executeAsyncCommands(parser,dbIndex,(async) -> {
            Long count = async.exists(prefixedCounter).get(FUTURE_TIMEOUT,TimeUnit.SECONDS);
            Boolean exists = count != null ? count > 0 : null;

            if (Boolean.TRUE.equals(exists))
                throw new KeyAlreadyExistException("4091","the provided counter already exist");
            else {
                RedisFuture<Long> value = async.incr(prefixedCounter);
                RedisFuture<Boolean> isTtlSet = async.expire(prefixedCounter, ttl);
                return ( value.get(FUTURE_TIMEOUT, TimeUnit.SECONDS) > 0 &&
                        isTtlSet.get(FUTURE_TIMEOUT, TimeUnit.SECONDS) ) ;
            }
        });
    }


    @Retryable(maxAttempts = 2, include = {
            RedisCommandExecutionException.class,
            RedisCommandTimeoutException.class
    }, backoff = @Backoff(value = 0))
    public Object delete(Integer dbIndex ,String counter , BasicAuthenticationParser parser){
        final String prefixedCounter = SERVICE_PREFIX + counter;
        return redisConnection.executeAsyncCommands(parser,dbIndex,(async) -> {
            RedisFuture<Long> value = async.unlink(prefixedCounter);
            return ( value.get(FUTURE_TIMEOUT, TimeUnit.SECONDS) > 0 ) ;
        });
    }

    @Recover
    public void recover(RedisException e){
        throw e;
    }
}
