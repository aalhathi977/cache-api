package com.stc.cacheapi;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;

@SpringBootApplication
public class CacheApiApplication {

	public static void main(String[] args) {
		SpringApplication.run(CacheApiApplication.class, args);
	}

}
