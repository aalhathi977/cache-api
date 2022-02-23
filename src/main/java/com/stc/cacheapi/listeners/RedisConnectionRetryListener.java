package com.stc.cacheapi.listeners;

import com.stc.cacheapi.configs.RedisConnection;
import io.lettuce.core.RedisCommandExecutionException;
import io.lettuce.core.RedisCommandTimeoutException;
import io.lettuce.core.RedisConnectionException;
import lombok.SneakyThrows;
import org.springframework.retry.RetryCallback;
import org.springframework.retry.RetryContext;
import org.springframework.retry.RetryListener;

import java.util.ArrayList;

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
    public <T, E extends Throwable> void close(RetryContext context, RetryCallback<T, E> callback, Throwable throwable) {}

    @Override
    public <T, E extends Throwable> void onError(RetryContext context, RetryCallback<T, E> callback, Throwable throwable) {
        // make sure it is first try
        if (context.getRetryCount() == 1) {

            // some time redis exception is assigned as cause in the exception
            ArrayList<Throwable> causes = new ArrayList<>();
            causes.add(throwable);
            if ( throwable.getCause() != null) {
                causes.add(throwable.getCause());
                if (throwable.getCause().getCause() != null)
                    causes.add(throwable.getCause().getCause());
            }

            for (Throwable cause: causes) {
                if (    cause instanceof RedisCommandExecutionException || // new master is elected , previous master is slave --> READONLY
                        cause instanceof RedisConnectionException || // new master is elected , previous master is still down --> refuse to connect
                        cause instanceof RedisCommandTimeoutException) // new master is elected , previous master is still down --> timeout
                {

                    // update sentinel
                    redisConnection.updateConnectionDetails();
                    break;
                }
            }
        }
    }
}
