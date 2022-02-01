package com.stc.cacheapi.services;

import com.stc.cacheapi.configs.RedisConnection;
import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import com.stc.cacheapi.parsers.BasicAuthenticationParser;
import io.lettuce.core.*;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.sync.RedisCommands;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

@Service
public class CounterTrackingService {
    final RedisTemplate<String, Serializable> redisTemplate = new RedisTemplate<>();
    private static final String SERVICE_PREFIX = "CT_";
    final RedisConnection redisConnection ;

    public CounterTrackingService(RedisConnection redisConnection) {
        this.redisConnection = redisConnection;
    }

    @Retryable(maxAttempts = 2, include = {RedisCommandExecutionException.class,RedisCommandTimeoutException.class}, backoff = @Backoff(value = 0))
    public Object get(Integer dbIndex , String counter , Integer ttl, BasicAuthenticationParser parser){
        RedisURI standalone = redisConnection.getConnectionDetails(parser.getUsername(),parser.getPassword(),dbIndex);
        RedisClient redisClient = RedisClient.create(standalone);
        StatefulRedisConnection<String, String> connection = redisClient.connect();
        RedisCommands<String, String> sync = connection.sync();


        final String prefixedCounter = SERVICE_PREFIX + counter;
        if (Objects.nonNull(ttl)) {
            sync.set(prefixedCounter, "230");
            return sync.getex(prefixedCounter, GetExArgs.Builder.ex(ttl));
        } else {
            return sync.get(prefixedCounter);
        }
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
