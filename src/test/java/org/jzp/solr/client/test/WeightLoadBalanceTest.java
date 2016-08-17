/*
 * Copyright (c) 2016-2020 LEJR.COM All Right Reserved
 */

package org.jzp.solr.client.test;

import org.jzp.solr.client.loadstrategic.WeightLoadBalance;
import org.jzp.solr.client.datasource.WeightSolrServer;
import org.apache.solr.client.solrj.SolrServer;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * WeightLoadBalance测试类。
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-31
 */
public class WeightLoadBalanceTest {

    @Test
    public void testSelect() {
        WeightLoadBalance loadBalance = new WeightLoadBalance();
        List<WeightSolrServer> dataSources = new ArrayList<WeightSolrServer>();
        dataSources.add(new WeightSolrServer(2, new MockSolrServer()));
        dataSources.add(new WeightSolrServer(8, new MockSolrServer()));
        SolrServer ds = loadBalance.select(dataSources);
        Assert.assertNotNull(ds);
    }

    @Test(expected = RuntimeException.class)
    public void testSelectWithEmptyDSs() {
        WeightLoadBalance loadBalance = new WeightLoadBalance();
        List<WeightSolrServer> dataSources = new ArrayList<WeightSolrServer>();
        loadBalance.select(dataSources);
    }

    @Test(expected = RuntimeException.class)
    public void testSelectWithNullDSs() {
        WeightLoadBalance loadBalance = new WeightLoadBalance();
        List<WeightSolrServer> dataSources = null;
        loadBalance.select(dataSources);
    }

    @Test
    public void testSelectWithOneDS() {
        WeightLoadBalance loadBalance = new WeightLoadBalance();
        List<WeightSolrServer> dataSources = new ArrayList<WeightSolrServer>();
        dataSources.add(new WeightSolrServer(2, new MockSolrServer()));
        SolrServer ds = loadBalance.select(dataSources);
        Assert.assertNotNull(ds);
    }

}
