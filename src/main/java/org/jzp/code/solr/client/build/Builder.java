package org.jzp.code.solr.client.build;

import org.apache.solr.client.solrj.SolrQuery;
import org.jzp.code.solr.client.common.enums.OperateEnum;

import java.lang.reflect.Field;
import java.util.Map;

/**
 * 建造者父类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-27
 */
public interface Builder {

    /**
     * builder scope
     *
     * @param field
     * @param object
     */
    void buildScope(Field field, Object object, Map<String, Object> map);

    /**
     * builder page
     *
     * @param field
     * @param object
     */
    void buildPage(Field field, Object object, OperateEnum operateEnum);

    /**
     * builder sort
     *
     * @param field
     * @param object
     */
    void buildSort(Field field, Object object, OperateEnum operateEnum);

    /**
     * builder in
     *
     * @param field
     * @param object
     */
    void buildIn(Field field, Object object);

    /**
     * builder NotIn
     *
     * @param field
     * @param object
     */
    void buildNotIn(Field field, Object object);

    /**
     * builder common
     *
     * @param field
     * @param object
     */
    void buildCommon(Field field, Object object);

    /**
     * Get SolrQuery
     *
     * @return SolrQuery
     */
    SolrQuery getResult();
}
