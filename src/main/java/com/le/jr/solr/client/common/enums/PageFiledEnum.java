package com.le.jr.solr.client.common.enums;

/**
 * 分页参数枚举类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-23
 */
public enum PageFiledEnum {

    // 每页记录数
    PAGESIZE("pageSize"),

    // 起始值
    START("start");

    private String value;

    PageFiledEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return this.value;
    }

}
