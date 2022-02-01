package com.stc.cacheapi.configs;

import io.lettuce.core.RedisClient;
import io.lettuce.core.RedisURI;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.sync.RedisCommands;
import io.lettuce.core.internal.HostAndPort;
import io.lettuce.core.sentinel.api.StatefulRedisSentinelConnection;
import io.lettuce.core.sentinel.api.sync.RedisSentinelCommands;
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

    public RedisConnection(RedisProperties redisProperties) {
        this.masterName = redisProperties.getSentinel().getMaster();
        this.masterPassword = redisProperties.getSentinel().getPassword();
        this.connectTimeout = redisProperties.getConnectTimeout();
        String firstNode = redisProperties.getSentinel().getNodes().get(0);
        this.sentinelHost = HostAndPort.parse(firstNode).getHostText();
        this.sentinelPort = HostAndPort.parse(firstNode).getPort();
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
        RedisClient redisClient = RedisClient.create(
                RedisURI.Builder
                        .sentinel(sentinelHost,sentinelPort,masterName,masterPassword)
                        .withTimeout(connectTimeout)
                        .build()
        );
        StatefulRedisSentinelConnection<String, String> connection = redisClient.connectSentinel();
        RedisSentinelCommands<String, String> sync = connection.sync();
        Map<String, String> master = sync.master(masterName);
        this.standaloneHost = master.get("ip");
        this.standalonePort = Integer.parseInt(master.get("port"));
    }


}

