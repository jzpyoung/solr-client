package com.le.jr.solr.client.build;

import com.le.jr.solr.client.common.enums.OperateEnum;
import org.apache.solr.client.solrj.SolrQuery;

import java.lang.reflect.Field;
import java.util.Map;

/**
 * 建造者父类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-27
 */
public abstract class Builder {

    /**
     * builder scope
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildScope(Field field, Object object, Map<String, Object> map) throws IllegalAccessException;

    /**
     * builder page
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildPage(Field field, Object object, OperateEnum operateEnum) throws IllegalAccessException;

    /**
     * builder sort
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildSort(Field field, Object object, OperateEnum operateEnum) throws IllegalAccessException;

    /**
     * builder in
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildIn(Field field, Object object) throws IllegalAccessException;

    /**
     * builder common
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildCommon(Field field, Object object) throws IllegalAccessException;

    /**
     * get SolrQuery
     *
     * @return SolrQuery
     */
    public abstract SolrQuery getResult();

}
