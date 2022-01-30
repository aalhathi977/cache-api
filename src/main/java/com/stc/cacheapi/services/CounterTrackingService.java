package com.stc.cacheapi.services;

import com.stc.cacheapi.exceptions.KeyAlreadyExistException;
import com.stc.cacheapi.exceptions.KeyNotFoundException;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.RedisSentinelConnection;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnection;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.data.redis.core.types.Expiration;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

@Service
public class CounterTrackingService {
    final RedisTemplate<String, Serializable> redisTemplate = new RedisTemplate<>();
    private static final String SERVICE_PREFIX = "CT_";

//    public CounterTrackingService(RedisTemplate<String, Serializable> redisTemplate) {
//        this.redisTemplate = redisTemplate;
//    }

    public Object get(Integer dbIndex , String counter , Integer ttl){
        RedisStandaloneConfiguration configuration = new RedisStandaloneConfiguration();
        configuration.setDatabase(dbIndex);
        configuration.setPassword("12345");
        LettuceConnectionFactory factory = new LettuceConnectionFactory(configuration);
        factory.setShareNativeConnection(false);
        factory.setPipeliningFlushPolicy(LettuceConnection.PipeliningFlushPolicy.flushOnClose());
        factory.afterPropertiesSet();

        RedisTemplate<String, Serializable> template = new RedisTemplate<>();
        template.setKeySerializer(StringRedisSerializer.UTF_8);
        template.setValueSerializer(StringRedisSerializer.UTF_8);
        template.setConnectionFactory(factory);

        RedisConnection connection = Objects.requireNonNull(template.getConnectionFactory()).getConnection();


        final String prefixedCounter = SERVICE_PREFIX + counter;
        RedisScript<Integer> script ;
        if (Objects.nonNull(ttl)) {
            script = RedisScript.of("redis.call('AUTH','12345') ; redis.call('SELECT'," + dbIndex + ");return " +
                    "redis.call('GETEX', KEYS[1],'EX',ARGV[1])",Integer.class);
            return connection.getEx(prefixedCounter.getBytes(StandardCharsets.UTF_8), Expiration.seconds(ttl));
//            return redisTemplate.execute(script, Collections.singletonList(prefixedCounter),ttl.toString());
        }else {
            script = RedisScript.of(
                    "redis.call('AUTH','12345') ;" +
                    "redis.call('SELECT'," + dbIndex + ");" +
                    "return redis.call('GET', KEYS[1])"
                    ,Integer.class);
            return redisTemplate.execute(script, Collections.singletonList(prefixedCounter));
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
}
