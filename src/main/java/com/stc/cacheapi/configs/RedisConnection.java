package com.stc.cacheapi.configs;

import com.stc.cacheapi.parsers.BasicAuthenticationParser;
import com.stc.cacheapi.utils.CheckedFunction;
import io.lettuce.core.RedisClient;
import io.lettuce.core.RedisURI;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.async.RedisAsyncCommands;
import io.lettuce.core.internal.HostAndPort;
import io.lettuce.core.sentinel.api.StatefulRedisSentinelConnection;
import io.lettuce.core.sentinel.api.sync.RedisSentinelCommands;
import lombok.SneakyThrows;
import org.springframework.boot.autoconfigure.data.redis.RedisProperties;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.util.Map;

@Component
public class RedisConnection {
    private String standaloneHost ;
    private Integer standalonePort ;
    private Duration connectTimeout ;

    // Sentinel Info
    private String sentinelHost ;
    private Integer sentinelPort ;
    private String masterName ;
    private String masterPassword ;

    // redis client bean
    RedisClient redisClient;


    public RedisConnection(RedisProperties redisProperties , RedisClient redisClient) {
        this.masterName = redisProperties.getSentinel().getMaster();
        this.masterPassword = redisProperties.getSentinel().getPassword();
        this.connectTimeout = redisProperties.getConnectTimeout();
        String firstNode = redisProperties.getSentinel().getNodes().get(0);
        this.sentinelHost = HostAndPort.parse(firstNode).getHostText();
        this.sentinelPort = HostAndPort.parse(firstNode).getPort();
        this.redisClient = redisClient ;
    }


    public RedisURI getConnectionDetails (String username , String password , Integer dbIndex ){
        if (standaloneHost != null && standalonePort != null) {
            return RedisURI.Builder.redis(standaloneHost, standalonePort)
                    .withAuthentication(username, password)
                    .withDatabase(dbIndex)
                    .withTimeout(connectTimeout)
                    .build();
        }else {
            // update the connection details
            updateConnectionDetails();
            // try to generate connection details
            return getConnectionDetails(username,password,dbIndex);
        }

    }

    public void updateConnectionDetails (){
        // hit sentinel and get the master ip and port --> set them in ip and port
        RedisURI sentinel = RedisURI.Builder
                        .sentinel(sentinelHost,sentinelPort,masterName,masterPassword)
                        .withTimeout(connectTimeout)
                        .build();
        StatefulRedisSentinelConnection<String, String> connection = redisClient.connectSentinel(sentinel);
        RedisSentinelCommands<String, String> sync = connection.sync();
        Map<String, String> master = sync.master(masterName);
        this.standaloneHost = master.get("ip");
        this.standalonePort = Integer.parseInt(master.get("port"));
        connection.close();
    }


    @SneakyThrows
    public Object executeAsyncCommands (BasicAuthenticationParser parser , Integer dbIndex , CheckedFunction<RedisAsyncCommands<String,String>, Object> callback){
        RedisURI standalone = getConnectionDetails(parser.getUsername(),parser.getPassword(),dbIndex);
        StatefulRedisConnection<String, String> connection = redisClient.connect(standalone);
        RedisAsyncCommands<String, String> async = connection.async();
        try {
            return callback.apply(async);
        }finally {
            connection.close();
        }
    }

}

