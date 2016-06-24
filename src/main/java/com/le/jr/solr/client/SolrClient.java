package com.le.jr.solr.client;

import com.le.jr.solr.client.common.enums.AggregationEnum;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrInputDocument;

import java.util.List;

/**
 * dao操作solr时的solrclient 接口
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public interface SolrClient {

    /**
     * 增加单条索引数据
     *
     * @param document
     * @return 操作是否成功
     */
    boolean addSingle(SolrInputDocument document);

    /**
     * 增加多条索引数据
     *
     * @param documents 文档集合
     * @return 操作是否成功
     */
    boolean addMulti(List<SolrInputDocument> documents);

    /**
     * 根据查询条件sq查询出结果QueryResponse
     *
     * @param sq 查询条件
     * @return 查询结果
     */
    QueryResponse query(SolrQuery sq);

    /**
     * 根据查询条件sq删除索引,sq必须符合solr语法
     *
     * @param sq 查询条件
     * @return 操作是否成功
     */
    boolean delete(String sq);

    /**
     * 根据查询条件查询出的记录按照指定字段sum
     *
     * @param field 需要sum的字段
     * @param agg   聚合操作枚举
     * @param sq    查询条件
     * @return sum的结果
     */
    Long sum(String field, AggregationEnum agg, SolrQuery sq);

}
