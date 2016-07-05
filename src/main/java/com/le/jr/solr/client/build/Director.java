package com.le.jr.solr.client.build;

import com.le.jr.solr.client.common.enums.OperateEnum;

import java.lang.reflect.Field;
import java.util.Map;

/**
 * 指挥者类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-27
 */
public class Director {

    public void construct(Builder bulider, Field field, Object object, OperateEnum operateEnum, Map<String, Object> map) throws IllegalAccessException {
        bulider.buildQuery(field, object, operateEnum, map);
    }

}
