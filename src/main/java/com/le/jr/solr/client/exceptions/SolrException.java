package com.le.jr.solr.client.exceptions;

/**
 * solr异常类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-23
 */
public class SolrException extends RuntimeException {

    public SolrException() {
        super();
    }

    public SolrException(String message) {
        super(message);
    }

    public SolrException(String message, Throwable cause) {
        super(message, cause);
    }

    public SolrException(Throwable cause) {
        super(cause);
    }

    protected SolrException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
