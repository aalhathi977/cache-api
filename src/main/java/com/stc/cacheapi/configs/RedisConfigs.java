package com.stc.cacheapi.configs;

import com.stc.cacheapi.exceptions.IllegalParamException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnection;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.web.context.annotation.RequestScope;
import org.springframework.web.servlet.HandlerMapping;

import javax.servlet.http.HttpServletRequest;
import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;

@Configuration
public class RedisConfigs {

    @Bean
    public RedisTemplate<String, Serializable> redisCacheTemplate(LettuceConnectionFactory redisConnectionFactory) {
        RedisTemplate<String, Serializable> template = new RedisTemplate<>();
        template.setConnectionFactory(redisConnectionFactory);
        return template;
    }


    @Bean
    @RequestScope
    public LettuceConnectionFactory redisFactory(HttpServletRequest request) {

        // ConnectionConfigurationExtractor(HttpServletRequest) --> do the validation inside the extractor

        RedisConfigurationExtractor extractor = new RedisConfigurationExtractor(request);

        RedisStandaloneConfiguration configuration = new RedisStandaloneConfiguration();
        configuration.setDatabase(extractor.getDbIndex());
        configuration.setUsername(extractor.getUsername());
        configuration.setPassword(extractor.getPassword());

        LettuceConnectionFactory factory = new LettuceConnectionFactory(configuration);
        factory.setShareNativeConnection(false);
        factory.setPipeliningFlushPolicy(LettuceConnection.PipeliningFlushPolicy.flushOnClose());
        return factory;
    }
}
