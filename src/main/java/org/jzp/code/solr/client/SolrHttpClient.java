package org.jzp.code.solr.client;

import com.google.common.collect.Maps;
import org.jzp.code.solr.client.common.enums.OperateEnum;
import org.jzp.code.solr.client.datasource.SolrServerGroup;
import org.jzp.code.solr.client.exceptions.SolrException;
import org.jzp.code.solr.client.common.code.ExceptionCode;
import org.jzp.code.solr.client.common.enums.AggregateEnum;
import org.jzp.code.solr.client.common.enums.ZeroOneEnum;
import org.jzp.code.solr.client.utils.SolrUtils;
import org.apache.log4j.Logger;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServer;
import org.apache.solr.client.solrj.response.*;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.GroupParams;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * SolrClient 实现
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public class SolrHttpClient implements SolrClient {

    private final Logger logger = Logger.getLogger(this.getClass());

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
            throw new SolrException("单条索引异常!", e);
        }
        return Boolean.TRUE;
    }

    @Override
    public boolean addSingle(Object object) {
        SolrInputDocument solrInputDocument = SolrUtils.vo2Solrdoc(object);
        return this.addSingle(solrInputDocument);
    }

    @Override
    public boolean addMulti(List<SolrInputDocument> documents) {
        SolrServer masterServer = solrServerGroup.getMasterServer();

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
        } catch (Exception e) {
            throw new SolrException("批量索引异常!size:" + documents.size(), e);
        }
        return Boolean.TRUE;
    }

    @Override
    public boolean addMulti4VO(List<? extends Object> lists) {
        List<SolrInputDocument> documents = SolrUtils.list2Solrdoclist(lists);
        return this.addMulti(documents);
    }

    @Override
    public QueryResponse query(SolrQuery sq) {
        // 根据配置文件策略选取slave dataSource
        SolrServer slaveServer = solrServerGroup.getSlaveServer();
        QueryResponse res;

        try {
            // 执行查询
            res = slaveServer.query(sq);
            return res;
        } catch (Exception e) {
            throw new SolrException("查询索引列表异常!", e);
        }
    }

    @Override
    public <T> List<T> query(Object queryObj, Class<T> clazz) {
        // 把传入对象转换成SolrQuery
        SolrQuery solrQuery = SolrUtils.vo2SolrQuery(queryObj, OperateEnum.QUERY);
        QueryResponse res = this.query(solrQuery);
        SolrDocumentList sdl;
        try {
            sdl = res.getResults();
            return SolrUtils.queryResponse2List(sdl, clazz);
        } catch (Exception e) {
            throw new SolrException("查询索引列表异常!", e);
        }
    }

    @Override
    public Long count(SolrQuery solrQuery) {
        QueryResponse res = this.query(solrQuery);
        return res.getResults().getNumFound();
    }

    @Override
    public Long count(Object object) {
        // 把传入对象转换成SolrQuery
        SolrQuery solrQuery = SolrUtils.vo2SolrQuery(object, OperateEnum.COUNT);
        return this.count(solrQuery);
    }

    @Override
    public Map<String, Long> sum(SolrQuery sq, String... fields) {
        return this.aggregate(AggregateEnum.SUM, sq, fields);
    }

    @Override
    public Map<String, Long> sum(Object object, String... fields) {
        return this.aggregate(AggregateEnum.SUM, object, fields);
    }

    @Override
    public boolean delete(String sq) {
        // 根据配置文件策略选取slave dataSource
        SolrServer masterServer = solrServerGroup.getMasterServer();
        UpdateResponse res;

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
        } catch (Exception e) {
            throw new SolrException("删除索引列表异常!", e);
        }
        return Boolean.TRUE;
    }

    @Override
    public boolean delete(Object object) {
        // 把传入对象转换成SolrQuery
        SolrQuery solrQuery = SolrUtils.vo2SolrQuery(object, OperateEnum.DELETE);
        return this.delete(solrQuery.getQuery());
    }

    @Override
    public Map<String, Long> aggregate(AggregateEnum agg, SolrQuery sq, String... fields) {
        if (fields == null || fields.length < 1) {
            throw new SolrException(ExceptionCode.SOLR_AGGREGATEFIELDISNULL_EXCEPTION.getMessage());
        }
        Map<String, Long> map = new HashMap<>();
        // 利用StatsComponent实现数据库的聚合统计查询，也就是min、max、avg、count、sum的功能
        // 是否开启stats（true/false）
        sq.set("stats", true);
        // 添加统计字段，可以有多个
        sq.set("stats.field", fields);
        // 执行查询
        QueryResponse res = this.query(sq);
        // 获取执行结果
        Map<String, FieldStatsInfo> fieldStatsInfoMap = res.getFieldStatsInfo();
        // stats.field字段设置成什么，这里就获取什么
        FieldStatsInfo fieldStatsInfo;
        Long result = null;
        String aggStr;
        try {
            for (String field : fields) {
                fieldStatsInfo = fieldStatsInfoMap.get(field);
                // 获取聚合值，并转换成long
                if (AggregateEnum.SUM.equals(agg)) {
                    aggStr = fieldStatsInfo.getSum().toString();
                    result = Double.valueOf(aggStr).longValue();
                }
                map.put(field, result);
            }
        } catch (Exception e) {
            throw new SolrException(ExceptionCode.SOLR_AGGREGATEFAILED_EXCEPTION.getMessage());
        } finally {
            return map;
        }
    }

    @Override
    public Map<String, Long> aggregate(AggregateEnum agg, Object object, String... fields) {
        SolrQuery solrQuery = SolrUtils.vo2SolrQuery(object, OperateEnum.QUERY);
        return this.aggregate(agg, solrQuery, fields);
    }

    @Override
    public Map<String, Long> group(SolrQuery solrQuery, String field) {
        Map<String, Long> result = Maps.newHashMap();

        // 根据配置文件策略选取slave dataSource
        SolrServer slaveServer = solrServerGroup.getSlaveServer();

        solrQuery.setParam(GroupParams.GROUP, true);
        solrQuery.setParam(GroupParams.GROUP_FIELD, field);
        solrQuery.setParam(GroupParams.GROUP_LIMIT, String.valueOf(ZeroOneEnum.ZERO.getValue()));

        QueryResponse response;
        try {
            response = slaveServer.query(solrQuery);

            if (response != null) {
                GroupResponse groupResponse = response.getGroupResponse();
                if (groupResponse != null) {
                    List<GroupCommand> groupList = groupResponse.getValues();
                    for (GroupCommand groupCommand : groupList) {
                        List<Group> groups = groupCommand.getValues();
                        for (Group group : groups) {
                            result.put(group.getGroupValue(), group.getResult().getNumFound());
                        }
                    }
                }
            }

            return result;
        } catch (Exception e) {
            throw new SolrException("GROUPBY异常!", e);
        }
    }

    @Override
    public Map<String, Long> group(Object object, String field) {
        // 把传入对象转换成SolrQuery
        SolrQuery solrQuery = SolrUtils.vo2SolrQuery(object, OperateEnum.QUERY);
        return this.group(solrQuery, field);
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
