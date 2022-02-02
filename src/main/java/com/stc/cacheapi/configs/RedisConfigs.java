package com.stc.cacheapi.configs;

import com.stc.cacheapi.listeners.RedisConnectionRetryListener;
import io.lettuce.core.RedisClient;
import org.springframework.boot.autoconfigure.data.redis.RedisProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.retry.RetryListener;
import org.springframework.retry.annotation.EnableRetry;

@Configuration
@EnableRetry
public class RedisConfigs {

    @Bean
    RedisConnection redisConnect(RedisProperties redisProperties){
        return new RedisConnection(redisProperties);
    }

    @Bean
    public RetryListener retryListener1(RedisConnection redisConnection) {
        return new RedisConnectionRetryListener(redisConnection);
    }

}
