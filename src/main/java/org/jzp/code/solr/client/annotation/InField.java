package org.jzp.code.solr.client.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * in参数注解
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-07-05
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface InField {

    String name() default "";
}
