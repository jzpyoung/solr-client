package com.le.jr.solr.client.demo;

import java.util.Map;

import javax.annotation.Resource;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.response.FieldStatsInfo;
import org.apache.solr.client.solrj.response.QueryResponse;

import com.le.jr.solr.client.SolrClient;

/**
 * solr查询 demo
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-04-14
 */
public class QueryDemo {
	
	@Resource
	private static SolrClient solrHttpClient;

	/**
	 * sum4demo
	 */
	public static Long sum4demo() {
		SolrQuery sq = new SolrQuery();
		//查询条件，*:*代表查询所有数据
		String query = "*:*";
		//查询哪些字段，*代表查询所有
		sq.addField("*");
		//设置查询条件
		sq.setQuery(query);
		
		/**
		 * 利用StatsComponent实现数据库的聚合统计查询，也就是min、max、avg、count、sum的功能
		 */
		//是否开启stats（true/false）
		sq.set("stats", true);
		//添加一个字段来统计，可以有多个(求和字段)
		sq.set("stats.field", "projectStatus");
		//执行查询
		QueryResponse res=solrHttpClient.query(sq);
		//获取执行结果
		Map<String, FieldStatsInfo> fieldStatsInfoMap = res.getFieldStatsInfo();
		//stats.field字段设置成什么，这里就获取什么
		FieldStatsInfo fieldStatsInfo = fieldStatsInfoMap.get("projectStatus");
		//获取sum值，并转换成long
		String sumStr = fieldStatsInfo.getSum().toString();
		Long sum= Double.valueOf(sumStr).longValue();
		return sum;
	}

	public static void main(String[] args) {
		sum4demo();
	}

}
