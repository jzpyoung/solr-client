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

    void buildScope(Field field, Object object, Map<String, Object> map);

    void buildPage(Field field, Object object, OperateEnum operateEnum);

    void buildSort(Field field, Object object, OperateEnum operateEnum);

    void buildIn(Field field, Object object);

    void buildNotIn(Field field, Object object);

    void buildCommon(Field field, Object object);

    SolrQuery getResult();
}
