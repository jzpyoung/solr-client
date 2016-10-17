package org.jzp.code.solr.client.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 模糊查询注解
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-07-14
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface DimField {
}
