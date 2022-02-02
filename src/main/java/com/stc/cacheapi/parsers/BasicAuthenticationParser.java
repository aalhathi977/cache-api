package com.stc.cacheapi.parsers;

import com.stc.cacheapi.exceptions.BasicAuthenticationParsingException;
import lombok.Data;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ExceptionHandler;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;

@Data
public class BasicAuthenticationParser {

    private String username;
    private String password;

    public BasicAuthenticationParser(String authorization) {
        if (!authorization.startsWith("Basic"))
            throw new BasicAuthenticationParsingException("4016","Malformed Basic Authentication");

        try {
            String base64 = authorization.substring("Basic".length()).trim();
            byte[] decoded = Base64.getDecoder().decode(base64);
            String[] credentials = new String(decoded, StandardCharsets.UTF_8).split(":");
            if (credentials.length != 2 /*|| !StringUtils.hasText(credentials[0]) || !StringUtils.hasText(credentials[1])*/)
                throw new BasicAuthenticationParsingException("4017","username and password is required in Basic Authentication");

            this.username = credentials[0];
            this.password = credentials[1];
        }catch (IllegalArgumentException e){
            throw new BasicAuthenticationParsingException("4018","Malformed Basic Authentication");
        }
    }
}
