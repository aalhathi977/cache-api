package com.stc.cacheapi;

import com.stc.cacheapi.configs.RedisConfigurationExtractor;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;

@SpringBootApplication
public class CacheApiApplication {

	public static void main(String[] args) {
		SpringApplication.run(CacheApiApplication.class, args);
	}

}
