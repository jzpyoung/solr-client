package com.le.jr.solr.client.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 对象转换solr查询条件分页参数注解
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-23
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface PageField {

    PageFiledEnum name();

    enum PageFiledEnum {
        // 每页记录数
        PAGESIZE,

        // 起始值
        START;

        PageFiledEnum() {
        }
    }
}



