package com.le.jr.solr.client.build;

import java.lang.reflect.Field;

/**
 * 指挥者类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-27
 */
public class Director {

    public void construct(Builder bulider, Field field, Object object) throws IllegalAccessException {
        bulider.buildQuery(field, object);
    }

}
