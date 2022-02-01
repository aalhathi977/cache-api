package com.stc.cacheapi.listeners;

import com.stc.cacheapi.configs.RedisConnection;
import io.lettuce.core.RedisCommandExecutionException;
import io.lettuce.core.RedisCommandTimeoutException;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.RetryCallback;
import org.springframework.retry.RetryContext;
import org.springframework.retry.RetryListener;

import java.util.Objects;

@Slf4j
public class RedisConnectionRetryListener implements RetryListener {

    final RedisConnection redisConnection;

    public RedisConnectionRetryListener(RedisConnection redisConnection) {
        this.redisConnection = redisConnection;
    }


    @Override
    public <T, E extends Throwable> boolean open(RetryContext context, RetryCallback<T, E> callback) {
        return true;
    }

    @SneakyThrows
    @Override
    public <T, E extends Throwable> void close(RetryContext context, RetryCallback<T, E> callback, Throwable throwable) {
        if (Objects.nonNull(throwable)){
            // log the issue , recalling sentinel didn't fix the issue
            log.error("Connection Redis Attempts Consumed ", throwable);
        }
    }

    @Override
    public <T, E extends Throwable> void onError(RetryContext context, RetryCallback<T, E> callback, Throwable throwable) {
        // new master is elected , previous master is slave --> READONLY
        if (context.getRetryCount() == 1 && throwable instanceof RedisCommandExecutionException ) {
            // log the issue
            log.error("Redis Connection with master failed with READONLY message , will retry with sentinel ", throwable);

            // update sentinel
            redisConnection.updateConnectionDetails();
        }
        // new master is elected , previous master is still down --> timeout
        else if (context.getRetryCount() == 1 && throwable.getCause() instanceof RedisCommandTimeoutException){
            // log the issue
            log.error("Redis Connection with master failed with connect timeout , will retry with sentinel ", throwable);

            // update sentinel
            redisConnection.updateConnectionDetails();
        }
    }
}
