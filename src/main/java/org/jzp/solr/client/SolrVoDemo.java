package org.jzp.solr.client;

import org.jzp.solr.client.annotation.*;

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

    private Integer cardNum;

    private String id;

    private Integer age;

    @DimField
    private String name;

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

    @SortField(name = "memberTime", mode = SortField.SortFiledEnum.DESC)
    private String sort1 = "111";

    @SortField(name = "venderCode", mode = SortField.SortFiledEnum.ASC)
    private String sort2;

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

    public String getSort1() {
        return sort1;
    }

    public void setSort1(String sort1) {
        this.sort1 = sort1;
    }

    public String getSort2() {
        return sort2;
    }

    public void setSort2(String sort2) {
        this.sort2 = sort2;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Integer getPageSize() {
        return pageSize;
    }

    public Integer getStart() {
        return start;
    }

    public Integer getAge() {
        return age;
    }

    public void setAge(Integer age) {
        this.age = age;
    }

    public int getCardNum() {
        return cardNum;
    }

    public void setCardNum(int cardNum) {
        this.cardNum = cardNum;
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
