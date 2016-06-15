package com.le.jr.solr.client.datasource;

import java.util.List;

import org.apache.solr.client.solrj.SolrServer;

import com.le.jr.solr.client.loadstrategic.LoadBalance;
import com.le.jr.solr.client.loadstrategic.RandomLoadBalance;


/**
 * solr数据源类，包含一主多从
 * 
 * @author jiazhipeng
 * date 2016-3-30
 */
public class SolrServerGroup{
	
	/**
	 * spring容器销毁时执行的方法
	 */
	public void destory(){
		masterServer.shutdown();
		for(SolrServer slave:slaveServerList){
			slave.shutdown();
		}
	}
	
	//负载均衡策略类(此处采用随机策略类)
	private LoadBalance<SolrServer> loadBalance = new RandomLoadBalance();
	
	//master dataSource
	private SolrServer masterServer;
	
	//slave dataSourceList
	private List<SolrServer> slaveServerList;
	
	/**
	 * @return 获得master的HttpSolrServer
	 */
	public SolrServer getMasterServer(){
		return this.masterServer;
	}
	
	/**
	 * 根据loadBalance负载均衡策略类
	 * @return 获得slave的HttpSolrServer
	 */
	public SolrServer getSlaveServer(){
		if(loadBalance == null){
			throw new RuntimeException("负载均衡策略类未指定");
		}
		return loadBalance.select(slaveServerList);
	}

	/**
	 * getters && setters
	 */
	public LoadBalance<SolrServer> getLoadBalance() {
		return loadBalance;
	}

	public void setLoadBalance(LoadBalance<SolrServer> loadBalance) {
		this.loadBalance = loadBalance;
	}

	public List<SolrServer> getSlaveServerList() {
		return slaveServerList;
	}

	public void setSlaveServerList(List<SolrServer> slaveServerList) {
		this.slaveServerList = slaveServerList;
	}

	public void setMasterServer(SolrServer masterServer) {
		this.masterServer = masterServer;
	}
	
}
