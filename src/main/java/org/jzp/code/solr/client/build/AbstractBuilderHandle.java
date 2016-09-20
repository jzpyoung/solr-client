package org.jzp.code.solr.client.build;


import org.apache.commons.lang.time.FastDateFormat;
import org.jzp.code.solr.client.common.constant.SolrConstant;
import org.jzp.code.solr.client.common.enums.ScopeEnum;
import org.jzp.code.solr.client.utils.Fields;

import java.lang.reflect.Field;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;

/**
 * 抽象builder过程中的处理方法
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-09-20
 */
public abstract class AbstractBuilderHandle {

    private FastDateFormat dateFormat = FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss'Z'");
    private static Calendar calendar;

    static {
        calendar = Calendar.getInstance();
    }

    public Object handleDate(Object obj) {
        calendar.setTime((Date) obj);
        calendar.add(Calendar.HOUR, -8);
        obj = dateFormat.format(calendar.getTime());
        return obj;
    }

    public Object handleScopeCond(ScopeEnum scopeEnum, Field field, Map<String, Object> map) {
        Object result = map.get(scopeEnum.getValue());
        if (result == null) {
            result = SolrConstant.star;
        } else if (SolrConstant.dateStr.equals(field.getGenericType().toString())) {
            result = handleDate(result);
        }
        return result;
    }

    public Object handleNegativeNumber(Object obj, Field field, Object object) {
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
}
