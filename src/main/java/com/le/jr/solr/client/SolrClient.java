package com.le.jr.solr.client;

import java.util.List;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrInputDocument;

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
	 * @param document
	 * @return
	 */
	boolean addSingle(SolrInputDocument document);
	
	/**
	 * 增加多条索引数据
	 * @param documents 文档集合
	 * @return
	 */
	boolean addMulti(List<SolrInputDocument> documents);
	
	/**
	 * 根据查询条件sq查询出结果QueryResponse
	 * @param sq 查询条件
	 * @return
	 */
	QueryResponse query(SolrQuery sq);
	
	/**
	 * 根据查询条件sq删除索引,sq必须符合solr语法
	 * @param sq 查询条件
	 * @return
	 */
	boolean delete(String sq);

}
