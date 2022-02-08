package com.stc.cacheapi.utils;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

@FunctionalInterface
public interface CheckedFunction<T,R> {
    R apply(T t) throws InterruptedException, ExecutionException, TimeoutException;
}
