package org.jzp.code.solr.client.common.code;

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

    SOLR_AGGREGATEFIELDISNULL_EXCEPTION(10000, "aggregate field not null", "聚合字段不能为null"),

    SOLR_AGGREGATEFAILED_EXCEPTION(10001, "aggregate field must be stored in solr field", "聚合失败异常,聚合字段必须为solr中的存储字段");

    private int value;
    private String message;
    private String desc;

    ExceptionCode(int value, String message, String desc) {
        this.value = value;
        this.message = message;
        this.desc = desc;
    }

    public String getMessage() {
        return message;
    }
}
