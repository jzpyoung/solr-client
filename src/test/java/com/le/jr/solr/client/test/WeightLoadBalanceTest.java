/*
 * Copyright (c) 2016-2020 LEJR.COM All Right Reserved
 */

package com.le.jr.solr.client.test;

import java.util.ArrayList;
import java.util.List;

import org.apache.solr.client.solrj.SolrServer;
import org.junit.Assert;
import org.junit.Test;

import com.le.jr.solr.client.datasource.WeightSolrServer;
import com.le.jr.solr.client.loadstrategic.PollLoadBalance;
import com.le.jr.solr.client.loadstrategic.WeightLoadBalance;

/**
 * WeightLoadBalance测试类。
 * 
 * @author jiazhipeng
 * @version 1.0 
 * @date 2016年3月31日
 */
public class WeightLoadBalanceTest {
	
	@Test
	public void testSelect(){
		WeightLoadBalance loadBalance = new WeightLoadBalance();
		List<WeightSolrServer> dataSources = new ArrayList<WeightSolrServer>();
		dataSources.add(new WeightSolrServer(2, new MockSolrServer()));
		dataSources.add(new WeightSolrServer(8, new MockSolrServer()));
		SolrServer ds = loadBalance.select(dataSources);
		Assert.assertNotNull(ds);
	}
	
	@Test(expected = RuntimeException.class)
	public void testSelectWithEmptyDSs(){
		WeightLoadBalance loadBalance = new WeightLoadBalance();
		List<WeightSolrServer> dataSources = new ArrayList<WeightSolrServer>();
		loadBalance.select(dataSources);
	}
	
	@Test(expected = RuntimeException.class)
	public void testSelectWithNullDSs(){
		WeightLoadBalance loadBalance = new WeightLoadBalance();
		List<WeightSolrServer> dataSources = null;
		loadBalance.select(dataSources);
	}
	
	@Test
	public void testSelectWithOneDS(){
		WeightLoadBalance loadBalance = new WeightLoadBalance();
		List<WeightSolrServer> dataSources = new ArrayList<WeightSolrServer>();
		dataSources.add(new WeightSolrServer(2, new MockSolrServer()));
		SolrServer ds = loadBalance.select(dataSources);
		Assert.assertNotNull(ds);
	}
	
}
