package com.le.jr.solr.client;

import com.le.jr.solr.client.common.enums.AggregateEnum;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrInputDocument;

import java.util.List;
import java.util.Map;

/**
 * SolrClient
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public interface SolrClient {

    /**
     * 增加单条索引数据
     *
     * @param document 待添加文档
     * @return 操作是否成功
     */
    boolean addSingle(SolrInputDocument document);

    /**
     * 增加单条索引数据
     *
     * @param object 待添加对象
     * @return 操作是否成功
     */
    boolean addSingle(Object object);

    /**
     * 增加多条索引数据
     *
     * @param documents 待添加文档集合
     * @return 操作是否成功
     */
    boolean addMulti(List<SolrInputDocument> documents);

    /**
     * 增加多条索引数据
     *
     * @param lists 待添加集合
     * @return 操作是否成功
     */
    boolean addMulti4VO(List<Object> lists);

    /**
     * 删除索引
     *
     * @param sq 删除条件
     * @return 操作是否成功
     */
    boolean delete(String sq);

    /**
     * 删除索引
     *
     * @param object 删除条件对象
     * @return 操作是否成功
     */
    boolean delete(Object object);

    /**
     * 查询
     *
     * @param sq 查询条件
     * @return 查询结果
     */
    QueryResponse query(SolrQuery sq);

    /**
     * 查询
     *
     * @param queryObj 查询条件对象
     * @param clazz    结果集合对象class
     * @return 查询结果
     */
    <T> List<T> query(Object queryObj, Class<T> clazz);

    /**
     * count
     *
     * @param solrQuery 查询条件
     * @return count
     */
    Long count(SolrQuery solrQuery);

    /**
     * count
     *
     * @param object 查询条件对象
     * @return count
     */
    Long count(Object object);

    /**
     * sum
     *
     * @param sq     查询条件
     * @param fields 待求和字段
     * @return sum的结果
     */
    Map<String, Long> sum(SolrQuery sq, String... fields);

    /**
     * sum
     *
     * @param object 查询条件对象
     * @param fields 待求和字段
     * @return sum的结果
     */
    Map<String, Long> sum(Object object, String... fields);

    /**
     * 聚合(min、max、avg、count、sum)目前只支持sum
     *
     * @param agg    聚合操作枚举
     * @param sq     查询条件
     * @param fields 待聚合字段
     * @return sum的结果
     */
    Map<String, Long> aggregate(AggregateEnum agg, SolrQuery sq, String... fields);

    /**
     * 聚合(min、max、avg、count、sum)目前只支持sum
     *
     * @param agg    聚合操作枚举
     * @param object 查询条件对象
     * @param fields 待聚合字段
     * @return sum的结果
     */
    Map<String, Long> aggregate(AggregateEnum agg, Object object, String... fields);

    /**
     * group by
     *
     * @param solrQuery 查询条件
     * @param field     groupby字段
     * @return groupby的结果
     */
    Map<String, Long> group(SolrQuery solrQuery, String field);

    /**
     * group by
     *
     * @param object 查询条件对象
     * @param field  groupby字段
     * @return groupby的结果
     */
    Map<String, Long> group(Object object, String field);
}
