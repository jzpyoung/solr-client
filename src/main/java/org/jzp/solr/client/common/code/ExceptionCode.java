package org.jzp.solr.client.common.code;

/**
 * solr返回码枚举
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-26
 * <p/>
 * 异常码表 10000-19999段
 */
public enum ExceptionCode {

    SOLR_AGGREGATEFIELDISNULL_EXCEPTION(10000, "聚合字段不能为null"),

    SOLR_AGGREGATEFAILED_EXCEPTION(10001, "聚合失败异常,聚合字段必须为solr中的存储字段");

    private int value;
    private String message;

    ExceptionCode(int value, String message) {
        this.value = value;
        this.message = message;
    }

    public int getValue() {
        return value;
    }

    public String getMessage() {
        return message;
    }

}
