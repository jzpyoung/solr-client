package com.le.jr.solr.client.datasource;

import java.io.IOException;
import java.util.Collection;
import java.util.List;

import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.SolrRequest.METHOD;
import org.apache.solr.client.solrj.SolrServer;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.StreamingResponseCallback;
import org.apache.solr.client.solrj.beans.DocumentObjectBinder;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.SolrPingResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.SolrParams;
import org.apache.solr.common.util.NamedList;

/**
 * 扩展SolrServer类，适用于权重策略类（代理SolrServer类）
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public class WeightSolrServer extends SolrServer{
	
	private static final long serialVersionUID = 1L;
	
	private int weight; //权重值
	
	private SolrServer solrServer; //代理solrServer对象
	
	public WeightSolrServer() {
	}
	
	public WeightSolrServer(int weight, SolrServer solrServer) {
		super();
		this.weight = weight;
		this.solrServer = solrServer;
	}

	public int getWeight() {
		return weight;
	}
	public void setWeight(int weight) {
		this.weight = weight;
	}
	
	/**
	 * solrServer的代理方法
	 */
	public UpdateResponse add(Collection<SolrInputDocument> docs, int commitWithinMs) throws SolrServerException, IOException {
		return solrServer.add(docs, commitWithinMs);
	}

	public UpdateResponse add(Collection<SolrInputDocument> docs) throws SolrServerException, IOException {
		return solrServer.add(docs);
	}

	public UpdateResponse add(SolrInputDocument doc, int commitWithinMs) throws SolrServerException, IOException {
		return solrServer.add(doc, commitWithinMs);
	}

	public UpdateResponse add(SolrInputDocument doc) throws SolrServerException, IOException {
		return solrServer.add(doc);
	}

	public UpdateResponse addBean(Object obj, int commitWithinMs) throws IOException, SolrServerException {
		return solrServer.addBean(obj, commitWithinMs);
	}

	public UpdateResponse addBean(Object obj) throws IOException, SolrServerException {
		return solrServer.addBean(obj);
	}

	public UpdateResponse addBeans(Collection<?> arg0, int arg1) throws SolrServerException, IOException {
		return solrServer.addBeans(arg0, arg1);
	}

	public UpdateResponse addBeans(Collection<?> beans) throws SolrServerException, IOException {
		return solrServer.addBeans(beans);
	}

	public UpdateResponse commit() throws SolrServerException, IOException {
		return solrServer.commit();
	}

	public UpdateResponse commit(boolean waitFlush, boolean waitSearcher, boolean softCommit) throws SolrServerException, IOException {
		return solrServer.commit(waitFlush, waitSearcher, softCommit);
	}

	public UpdateResponse commit(boolean waitFlush, boolean waitSearcher) throws SolrServerException, IOException {
		return solrServer.commit(waitFlush, waitSearcher);
	}

	public UpdateResponse deleteById(List<String> ids, int commitWithinMs) throws SolrServerException, IOException {
		return solrServer.deleteById(ids, commitWithinMs);
	}

	public UpdateResponse deleteById(List<String> ids) throws SolrServerException, IOException {
		return solrServer.deleteById(ids);
	}

	public UpdateResponse deleteById(String id, int commitWithinMs) throws SolrServerException, IOException {
		return solrServer.deleteById(id, commitWithinMs);
	}

	public UpdateResponse deleteById(String id) throws SolrServerException, IOException {
		return solrServer.deleteById(id);
	}

	public UpdateResponse deleteByQuery(String query, int commitWithinMs) throws SolrServerException, IOException {
		return solrServer.deleteByQuery(query, commitWithinMs);
	}

	public UpdateResponse deleteByQuery(String query) throws SolrServerException, IOException {
		return solrServer.deleteByQuery(query);
	}

	public boolean equals(Object obj) {
		return solrServer.equals(obj);
	}

	public DocumentObjectBinder getBinder() {
		return solrServer.getBinder();
	}

	public int hashCode() {
		return solrServer.hashCode();
	}

	public UpdateResponse optimize() throws SolrServerException, IOException {
		return solrServer.optimize();
	}

	public UpdateResponse optimize(boolean waitFlush, boolean waitSearcher, int maxSegments) throws SolrServerException, IOException {
		return solrServer.optimize(waitFlush, waitSearcher, maxSegments);
	}

	public UpdateResponse optimize(boolean waitFlush, boolean waitSearcher) throws SolrServerException, IOException {
		return solrServer.optimize(waitFlush, waitSearcher);
	}

	public SolrPingResponse ping() throws SolrServerException, IOException {
		return solrServer.ping();
	}

	public QueryResponse query(SolrParams params, METHOD method) throws SolrServerException {
		return solrServer.query(params, method);
	}

	public QueryResponse query(SolrParams params) throws SolrServerException {
		return solrServer.query(params);
	}

	public QueryResponse queryAndStreamResponse(SolrParams params, StreamingResponseCallback callback) throws SolrServerException,
			IOException {
		return solrServer.queryAndStreamResponse(params, callback);
	}

	public NamedList<Object> request(SolrRequest arg0) throws SolrServerException, IOException {
		return solrServer.request(arg0);
	}

	public UpdateResponse rollback() throws SolrServerException, IOException {
		return solrServer.rollback();
	}

	public void shutdown() {
		solrServer.shutdown();
	}

	public String toString() {
		return solrServer.toString();
	}
	
}
