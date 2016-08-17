package org.jzp.solr.client.common.enums;

/**
 * 范围枚举
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-07-05
 */
public enum ScopeEnum {

    SCOPE("scope"),

    SCOPESTART("scopeStart"),

    SCOPEEND("scopeEnd");

    private String value;

    ScopeEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return this.value;
    }
}
