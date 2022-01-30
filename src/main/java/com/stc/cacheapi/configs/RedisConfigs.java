package com.stc.cacheapi.configs;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnection;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.context.annotation.RequestScope;

import javax.servlet.http.HttpServletRequest;
import java.io.Serializable;

@Configuration
public class RedisConfigs {
//
//    @Bean
//    public RedisTemplate<String, Serializable> redisCacheTemplate(/*LettuceConnectionFactory redisConnectionFactory*/) {
//        RedisTemplate<String, Serializable> template = new RedisTemplate<>();
////        template.setConnectionFactory(redisConnectionFactory);
////        template.setKeySerializer(StringRedisSerializer.UTF_8);
////        template.setValueSerializer(StringRedisSerializer.UTF_8);
////        return template;
//    }


//    @Bean
////    @RequestScope
//    public LettuceConnectionFactory redisFactory(/*HttpServletRequest request*/) {
//        // extract the needed info from the request
////        RedisConfigurationExtractor extractor = new RedisConfigurationExtractor(request);
//
//        // build Connection ( in recovery option , we should use RedisSentinelConfiguration )
////        RedisStandaloneConfiguration configuration = new RedisStandaloneConfiguration();
////        configuration.setDatabase(extractor.getDbIndex());
////        configuration.setUsername(extractor.getUsername());
////        configuration.setPassword(extractor.getPassword());
//        // FIXME: 1/17/2022 extract the Auth info and set it here
//
//        RedisStandaloneConfiguration configuration = new RedisStandaloneConfiguration();
////        configuration.setDatabase(extractor.getDbIndex());
//
//        // Build the Factory and inject the configuration
//        LettuceConnectionFactory factory = new LettuceConnectionFactory(configuration);
//        factory.setShareNativeConnection(true);
//        factory.setPipeliningFlushPolicy(LettuceConnection.PipeliningFlushPolicy.flushOnClose());
//        return factory;
//    }
}
