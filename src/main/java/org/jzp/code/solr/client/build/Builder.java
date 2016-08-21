package org.jzp.code.solr.client.build;

import org.jzp.code.solr.client.common.constant.SolrConstant;
import org.jzp.code.solr.client.common.enums.OperateEnum;
import org.jzp.code.solr.client.common.enums.ScopeEnum;
import org.jzp.code.solr.client.utils.Fields;
import org.apache.commons.lang.time.FastDateFormat;
import org.apache.solr.client.solrj.SolrQuery;

import java.lang.reflect.Field;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;

/**
 * 建造者父类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-27
 */
public abstract class Builder {

    private FastDateFormat dateFormat = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss'Z'");
    private static Calendar calendar;

    static {
        calendar = Calendar.getInstance();
    }

    public Object buildDate(Object obj) {
        calendar.setTime((Date) obj);
        calendar.add(Calendar.HOUR, -8);
        obj = dateFormat.format(calendar.getTime());
        return obj;
    }

    public Object buildScopeCond(ScopeEnum scopeEnum, Field field, Map<String, Object> map) {
        Object result = map.get(scopeEnum.getValue());
        if (result == null) {
            result = SolrConstant.star;
        } else if (SolrConstant.dateStr.equals(field.getGenericType().toString())) {
            result = buildDate(result);
        }
        return result;
    }

    public Object buildNegativeNumber(Object obj, Field field, Object object) {
        if (obj instanceof Integer) {
            if ((int) obj < 0) {
                obj = "\\" + Fields.get(object, field);
            }
        } else if (obj instanceof Double) {
            if ((double) obj < 0) {
                obj = "\\" + Fields.get(object, field);
            }
        } else if (obj instanceof Float) {
            if ((float) obj < 0) {
                obj = "\\" + Fields.get(object, field);
            }
        } else if (obj instanceof Byte) {
            if ((byte) obj < 0) {
                obj = "\\" + Fields.get(object, field);
            }
        }
        return obj;
    }

    /**
     * builder scope
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildScope(Field field, Object object, Map<String, Object> map) throws IllegalAccessException;

    /**
     * builder page
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildPage(Field field, Object object, OperateEnum operateEnum) throws IllegalAccessException;

    /**
     * builder sort
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildSort(Field field, Object object, OperateEnum operateEnum) throws IllegalAccessException;

    /**
     * builder in
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildIn(Field field, Object object) throws IllegalAccessException;

    /**
     * builder common
     *
     * @param field
     * @param object
     * @throws IllegalAccessException
     */
    public abstract void buildCommon(Field field, Object object) throws IllegalAccessException;

    /**
     * get SolrQuery
     *
     * @return SolrQuery
     */
    public abstract SolrQuery getResult();
}
