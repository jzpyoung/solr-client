package org.jzp.solr.client.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 对象转换solr查询条件排序注解
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-07-11
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface SortField {

    String name() default "";

    SortFiledEnum mode() default SortFiledEnum.ASC;

    enum SortFiledEnum {
        ASC, DESC
    }
}
