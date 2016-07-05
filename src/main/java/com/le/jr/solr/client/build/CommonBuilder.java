package com.le.jr.solr.client.build;

import com.le.jr.solr.client.annotation.PageField;
import com.le.jr.solr.client.annotation.ScopeField;
import com.le.jr.solr.client.common.constant.SolrConstant;
import com.le.jr.solr.client.common.enums.OperateEnum;
import com.le.jr.solr.client.common.enums.ScopeEnum;
import com.le.jr.solr.client.utils.Fields;
import org.apache.solr.client.solrj.SolrQuery;

import java.lang.reflect.Field;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;

/**
 * 通用建造者类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-27
 */
public class CommonBuilder extends Builder {

    private SolrQuery solrQuery = new SolrQuery();
    private StringBuilder str = new StringBuilder();
    private DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
    private int scopeTime = 0;
    private int commonTime = 0;
    private static Calendar calendar;

    static {
        calendar = Calendar.getInstance();
        calendar.add(Calendar.HOUR, -8);
    }

    @Override
    public void buildQuery(Field field, Object object, OperateEnum operateEnum, Map<String, Object> map) throws IllegalAccessException {
        if (OperateEnum.SCOPE.equals(operateEnum)) {
            this.buildScope(field, object, map);
        } else {
            this.buildPage(field, object, operateEnum);
        }

    }

    @Override
    public void buildScope(Field field, Object object, Map<String, Object> map) throws IllegalAccessException {
        if (scopeTime != 0 || commonTime != 0) {
            str.append(SolrConstant.andStr);
        }
        Object scopeStart = map.get(ScopeEnum.SCOPESTART.getValue());
        if (scopeStart == null) {
            scopeStart = SolrConstant.star;
        } else if (SolrConstant.dateStr.equals(field.getGenericType().toString())) {
            calendar.setTime((Date) scopeStart);
            scopeStart = dateFormat.format(calendar.getTime());
        }


        Object scopeEnd = map.get(ScopeEnum.SCOPEEND.getValue());
        if (scopeEnd == null) {
            scopeEnd = SolrConstant.star;
        } else if (SolrConstant.dateStr.equals(field.getGenericType().toString())) {
            calendar.setTime((Date) scopeEnd);
            scopeEnd = dateFormat.format(calendar.getTime());
        }
        str.append(field.getAnnotation(ScopeField.class).name() + SolrConstant.bracketLeft + scopeStart + SolrConstant.toStr + scopeEnd + SolrConstant.bracketRight);
        scopeTime++;
    }

    @Override
    public void buildPage(Field field, Object object, OperateEnum operateEnum) throws IllegalAccessException {
        if (field.isAnnotationPresent(PageField.class)) {

            switch (operateEnum) {
                case COUNT:
                    return;
            }

            switch (field.getAnnotation(PageField.class).name()) {
                case PAGESIZE:
                    solrQuery.setRows(Fields.get(object, field, Integer.class));
                    break;
                case START:
                    solrQuery.setStart(Fields.get(object, field, Integer.class));
                    break;
            }
            return;
        }

        this.buildCommon(field, object);
    }

    @Override
    public void buildCommon(Field field, Object object) throws IllegalAccessException {
        if (commonTime != 0 || scopeTime != 0) {
            str.append(SolrConstant.andStr);
        }
        str.append(field.getName() + SolrConstant.colon + Fields.get(object, field));
        commonTime++;
    }

    @Override
    public SolrQuery getResult() {
        solrQuery.addField(SolrConstant.star);
        if (scopeTime != 0 || commonTime != 0) {
            solrQuery.setQuery(str.toString());
        } else {
            solrQuery.setQuery(SolrConstant.queryStr);
        }
        return solrQuery;
    }
}
