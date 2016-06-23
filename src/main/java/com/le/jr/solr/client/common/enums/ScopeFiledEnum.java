package com.le.jr.solr.client.common.enums;

/**
 * 分页参数枚举类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-23
 */
public enum ScopeFiledEnum {

    // 小于
    LT("lt"),

    // 大于
    GT("gt");

    private String value;

    ScopeFiledEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return this.value;
    }

}
