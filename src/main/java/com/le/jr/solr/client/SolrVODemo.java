package com.le.jr.solr.client;

import com.le.jr.solr.client.annotation.IgnoreField;
import com.le.jr.solr.client.annotation.InField;
import com.le.jr.solr.client.annotation.PageField;
import com.le.jr.solr.client.annotation.ScopeField;

import java.util.Date;
import java.util.List;

/**
 * QueryVO Demo
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-23
 */
public class SolrVoDemo {

    @PageField(name = PageField.PageFiledEnum.PAGESIZE)
    private Integer pageSize;

    @PageField(name = PageField.PageFiledEnum.START)
    private Integer start;

    @ScopeField(name = "createTime", mode = ScopeField.ScopeFiledEnum.GT)
    private Date startTime;

    @ScopeField(name = "createTime", mode = ScopeField.ScopeFiledEnum.LT)
    private Date endTime;

    @IgnoreField
    private String letvUserId;

    @InField(name = "orderId")
    private List<Integer> orderList;

    @InField(name = "time")
    private List<Date> timeList;

    private String name;

    public int getPageSize() {
        return pageSize;
    }

    public void setPageSize(int pageSize) {
        this.pageSize = pageSize;
    }

    public int getStart() {
        return start;
    }

    public void setStart(int start) {
        this.start = start;
    }

    public String getLetvUserId() {
        return letvUserId;
    }

    public void setLetvUserId(String letvUserId) {
        this.letvUserId = letvUserId;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setPageSize(Integer pageSize) {
        this.pageSize = pageSize;
    }

    public void setStart(Integer start) {
        this.start = start;
    }

    public List<Integer> getOrderList() {
        return orderList;
    }

    public void setOrderList(List<Integer> orderList) {
        this.orderList = orderList;
    }

    public List<Date> getTimeList() {
        return timeList;
    }

    public void setTimeList(List<Date> timeList) {
        this.timeList = timeList;
    }

    @Override
    public String toString() {
        return "SolrVoDemo{" +
                "pageSize=" + pageSize +
                ", start=" + start +
                ", startTime=" + startTime +
                ", endTime=" + endTime +
                ", letvUserId='" + letvUserId + '\'' +
                ", name='" + name + '\'' +
                '}';
    }
}
