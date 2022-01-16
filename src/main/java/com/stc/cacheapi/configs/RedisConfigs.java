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
    //@RequestScope // if you don't know what you're doing , don't touch it
//    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public RedisTemplate<String, Serializable> redisCacheTemplate(LettuceConnectionFactory redisConnectionFactory) {
        RedisTemplate<String, Serializable> template = new RedisTemplate<>();
        template.setConnectionFactory(redisConnectionFactory);
        return template;
    }


    @Bean
    @RequestScope // if you don't know what you're doing , don't touch it
    public LettuceConnectionFactory redisFactory(HttpServletRequest request) {
        Map attribute = (Map) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
        int db_index = Integer.parseInt(attribute.get("db_index").toString());

        // check db_index is a valid int
        if (db_index < 0 || db_index > 15)
            throw new IllegalParamException("4002", "db_index should be an integer between 0 and 15");

        RedisStandaloneConfiguration configuration = new RedisStandaloneConfiguration();
        configuration.setDatabase(db_index);

        LettuceConnectionFactory factory = new LettuceConnectionFactory(configuration);
        factory.setShareNativeConnection(false);
        factory.setPipeliningFlushPolicy(LettuceConnection.PipeliningFlushPolicy.flushOnClose());
        return factory;
    }
}
