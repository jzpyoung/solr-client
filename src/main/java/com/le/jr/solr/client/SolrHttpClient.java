package com.le.jr.solr.client;

import java.util.List;

import org.apache.log4j.Logger;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServer;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrInputDocument;

import com.le.jr.solr.client.datasource.SolrServerGroup;

/**
 * SolrClient的实现SolrHttpClient solr连接客户端，封装了solrserver的 add query方法
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public class SolrHttpClient implements SolrClient {

    private final static Logger logger = Logger.getLogger(SolrHttpClient.class);

    // solr dataSource组，提供getMaster getSlave方法
    private SolrServerGroup solrServerGroup;
    // autoCommit自动提交属性
    private Boolean autoCommit;

    @Override
    public boolean addSingle(SolrInputDocument document) {

        SolrServer masterServer = solrServerGroup.getMasterServer();

        try {
            // 提交索引
            UpdateResponse res = masterServer.add(document);

            //如果客户端没有设置自定提交，默认不是自动提交
            if (autoCommit == null) {
                autoCommit = Boolean.FALSE;
            }

            if (!autoCommit) {
                // 第一个参数：是否等待solr从内存把数据刷入硬盘再返回结果，如果只刷入硬盘查询不到
                // 第二个参数：是否等待solr从内存把数据刷入searcher再返回结果，刷入searcher才可以查询到
                masterServer.commit(true, false);
            }

            if (res.getQTime() > 300) {
                logger.warn("addSingleIndex 提交索引耗时：" + res.getQTime());
            }

        } catch (Exception e) {
            throw new RuntimeException("单条索引异常!", e);
        }

        return Boolean.TRUE;

    }

    @Override
    public boolean addMulti(List<SolrInputDocument> documents) {

        SolrServer masterServer = solrServerGroup.getMasterServer();

        boolean submitFlag = true;
        try {

            // 提交索引
            UpdateResponse res = masterServer.add(documents);

            //如果客户端没有设置自定提交，默认不是自动提交
            if (autoCommit == null) {
                autoCommit = Boolean.FALSE;
            }

            if (!autoCommit) {
                // 第一个参数：是否等待solr从内存把数据刷入硬盘再返回结果，如果只刷入硬盘查询不到
                // 第二个参数：是否等待solr从内存把数据刷入searcher再返回结果，刷入searcher才可以查询到
                masterServer.commit(true, false);
            }

            if (res.getQTime() > 300) {
                logger.warn("addMultipleIndex 提交索引耗时：" + res.getQTime());
            }
            submitFlag = true;

        } catch (Exception e) {
            throw new RuntimeException("批量索引异常!size:" + documents.size(), e);
        }
        return submitFlag;

    }

    @Override
    public QueryResponse query(SolrQuery sq) {

        // 根据配置文件策略选取slave dataSource
        SolrServer slaveServer = solrServerGroup.getSlaveServer();
        QueryResponse res = null;

        try {
            // 执行查询
            res = slaveServer.query(sq);
            return res;

        } catch (Exception e) {
            throw new RuntimeException("查询索引列表异常!", e);
        }

    }

    @Override
    public boolean delete(String sq) {

        boolean submitFlag = true;

        // 根据配置文件策略选取slave dataSource
        SolrServer masterServer = solrServerGroup.getMasterServer();
        UpdateResponse res = null;

        try {

            res = masterServer.deleteByQuery(sq);

            //如果客户端没有设置自定提交，默认不是自动提交
            if (autoCommit == null) {
                autoCommit = Boolean.FALSE;
            }

            if (!autoCommit) {
                // 第一个参数：是否等待solr从内存把数据刷入硬盘再返回结果，如果只刷入硬盘查询不到
                // 第二个参数：是否等待solr从内存把数据刷入searcher再返回结果，刷入searcher才可以查询到
                masterServer.commit(true, false);
            }

            if (res.getQTime() > 300) {
                logger.warn("delete 删除索引耗时：" + res.getQTime());
            }
            submitFlag = true;

        } catch (Exception e) {
            throw new RuntimeException("删除索引列表异常!", e);
        }
        return submitFlag;

    }

    /**
     * getters && setters
     */
    public SolrServerGroup getSolrServerGroup() {
        return solrServerGroup;
    }

    public void setSolrServerGroup(SolrServerGroup solrServerGroup) {
        this.solrServerGroup = solrServerGroup;
    }

    public Boolean getAutoCommit() {
        return autoCommit;
    }

    public void setAutoCommit(Boolean autoCommit) {
        this.autoCommit = autoCommit;
    }
}
