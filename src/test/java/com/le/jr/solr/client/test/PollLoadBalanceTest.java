/*
 * Copyright (c) 2016-2020 LEJR.COM All Right Reserved
 */

package com.le.jr.solr.client.test;

import java.util.ArrayList;
import java.util.List;

import org.apache.solr.client.solrj.SolrServer;
import org.junit.Assert;
import org.junit.Test;

import com.le.jr.solr.client.loadstrategic.PollLoadBalance;
import com.le.jr.solr.client.loadstrategic.RandomLoadBalance;

/**
 * PollLoadBalance测试类。
 * 
 * @author jiazhipeng
 * @version 1.0 
 * @date 2016年3月31日
 */
public class PollLoadBalanceTest {
	
	@Test
	public void testSelect(){
		PollLoadBalance loadBalance = new PollLoadBalance();
		List<SolrServer> dataSources = new ArrayList<SolrServer>();
		dataSources.add(new MockSolrServer());
		dataSources.add(new MockSolrServer());
		dataSources.add(new MockSolrServer());
		dataSources.add(new MockSolrServer());
		dataSources.add(new MockSolrServer());
		SolrServer ds1 = loadBalance.select(dataSources);
		SolrServer ds2 = loadBalance.select(dataSources);
		Assert.assertNotNull(ds1);
		Assert.assertNotNull(ds2);
	}
	
	@Test(expected = RuntimeException.class)
	public void testSelectWithEmptyDSs(){
		PollLoadBalance loadBalance = new PollLoadBalance();
		List<SolrServer> dataSources = new ArrayList<SolrServer>();
		loadBalance.select(dataSources);
	}
	
	@Test(expected = RuntimeException.class)
	public void testSelectWithNullDSs(){
		PollLoadBalance loadBalance = new PollLoadBalance();
		List<SolrServer> dataSources = null;
		loadBalance.select(dataSources);
	}
	
	@Test
	public void testSelectWithOneDS(){
		PollLoadBalance loadBalance = new PollLoadBalance();
		List<SolrServer> dataSources = new ArrayList<SolrServer>();
		dataSources.add(new MockSolrServer());
		SolrServer ds = loadBalance.select(dataSources);
		Assert.assertNotNull(ds);
	}
	
}
