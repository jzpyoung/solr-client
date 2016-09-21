package org.jzp.code.solr.client.factory;

import org.apache.solr.client.solrj.response.FieldStatsInfo;
import org.jzp.code.solr.client.common.enums.AggregateEnum;

/**
 * 获取聚合结果工厂
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-09-21
 */
public class AggFactory {

    public static long getAggValue(FieldStatsInfo fieldStatsInfo, AggregateEnum agg) {
        String aggStr = null;
        switch (agg) {
            case SUM:
                aggStr = fieldStatsInfo.getSum().toString();
                break;
            case MAX:
                aggStr = fieldStatsInfo.getMax().toString();
                break;
            case MIN:
                aggStr = fieldStatsInfo.getMin().toString();
                break;
            case MEAN:
                aggStr = fieldStatsInfo.getMean().toString();
                break;
            case STDDEV:
                aggStr = fieldStatsInfo.getStddev().toString();
                break;
            case COUNT:
                return fieldStatsInfo.getCount();
            case MISSING:
                return fieldStatsInfo.getMissing();
        }
        return Double.valueOf(aggStr).longValue();
    }
}
