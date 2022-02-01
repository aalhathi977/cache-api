package com.stc.cacheapi.services;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

@Service
public class KVPairService {

    final RedisTemplate<String, Serializable> redisTemplate = new RedisTemplate<>() ;
    private static final String SERVICE_PREFIX = "KV_";

//    public KVPairService(RedisTemplate<String, Serializable> redisTemplate) {
//        this.redisTemplate = redisTemplate;
//    }
//
    public Object get(String key , Integer ttl){
        final String prefixedKey = SERVICE_PREFIX + key;
        if (Objects.nonNull(ttl))
            return redisTemplate.opsForValue().getAndExpire(prefixedKey, ttl, TimeUnit.SECONDS);
        else
            return redisTemplate.opsForValue().get(prefixedKey);
    }

    public Boolean update(String key , String value , Integer ttl){
        final String prefixedKey = SERVICE_PREFIX + key;
        if (Objects.nonNull(ttl))
            return redisTemplate.opsForValue().setIfPresent(prefixedKey, value ,ttl, TimeUnit.SECONDS);
        else {
            RedisScript<Boolean> script = RedisScript.of("return redis.call('SET', KEYS[1], ARGV[1], 'XX','KEEPTTL')",Boolean.class);
            return redisTemplate.execute(script, Collections.singletonList(prefixedKey), value);
        }
    }

    public Boolean create(String key , String value , Integer ttl){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisTemplate.opsForValue().setIfAbsent(prefixedKey, value ,ttl, TimeUnit.SECONDS);
    }

    public Boolean delete(String key){
        final String prefixedKey = SERVICE_PREFIX + key;
        return redisTemplate.unlink(prefixedKey);
    }
}
