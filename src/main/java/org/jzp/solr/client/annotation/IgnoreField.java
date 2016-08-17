package org.jzp.solr.client.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 对象转换solr查询条件忽略注解
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-22
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface IgnoreField {

    String name() default "";
}
