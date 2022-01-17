package com.stc.cacheapi.configs;

import com.stc.cacheapi.exceptions.IllegalParamException;
import com.stc.cacheapi.utils.ValidationUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.web.servlet.HandlerMapping;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;
import java.util.Objects;

@Data
@AllArgsConstructor
public class RedisConfigurationExtractor {

    private String username = null;
    private String password = null ;
    private int dbIndex = 0 ;

    public RedisConfigurationExtractor(HttpServletRequest request){
        Map attributes = (Map) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
        dbIndex = extractDBIndex(attributes);
    }

    public int extractDBIndex(Map attributes){
        // get the index from the attributes Map
        Object index = attributes.get("db_index");

        // db_index is required , throw exception if null
        if (Objects.isNull(index) || !ValidationUtils.isNumeric(index.toString()))
            throw new IllegalParamException("4003", "db_index is missing or incorrect");

        // parse the index
        int dbIndex = Integer.parseInt(index.toString());

        // check that range is correct
        if (dbIndex < 0 || dbIndex > 15)
            throw new IllegalParamException("4002", "db_index should be an integer between 0 and 15");

        return dbIndex;
    }


}
