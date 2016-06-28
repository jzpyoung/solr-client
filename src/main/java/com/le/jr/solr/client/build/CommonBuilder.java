package com.le.jr.solr.client.build;

import com.le.jr.solr.client.annotation.PageField;
import com.le.jr.solr.client.annotation.ScopeField;
import com.le.jr.solr.client.common.constant.SolrConstant;
import com.le.jr.solr.client.utils.Fields;
import org.apache.solr.client.solrj.SolrQuery;

import java.lang.reflect.Field;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

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
    private int scopeEndTime = 0;
    private int i = 0;
    private static Calendar c;

    static {
        c = Calendar.getInstance();
        c.add(Calendar.HOUR, -8);
    }

    @Override
    public void buildQuery(Field field, Object object) throws IllegalAccessException {
        this.buildPage(field, object);
    }

    @Override
    public void buildPage(Field field, Object object) throws IllegalAccessException {
        if (field.isAnnotationPresent(PageField.class)) {
            if (PageField.PageFiledEnum.PAGESIZE.equals(field.getAnnotation(PageField.class).name())) {
                solrQuery.setRows(Fields.get(object, field, Integer.class));
            }
            if (PageField.PageFiledEnum.START.equals(field.getAnnotation(PageField.class).name())) {
                solrQuery.setStart(Fields.get(object, field, Integer.class));
            }
            return;
        }

        buildScope(field, object);
    }

    @Override
    public void buildScope(Field field, Object object) throws IllegalAccessException {
        if (field.isAnnotationPresent(ScopeField.class)) {
            if (ScopeField.ScopeFiledEnum.GT.equals(field.getAnnotation(ScopeField.class).mode())) {
                if (scopeEndTime != 0) {
                    str.append(SolrConstant.andStr);
                }

                str.append(field.getAnnotation(ScopeField.class).name() + SolrConstant.bracketLeft);

                Object value = Fields.get(object, field);
                if (value == null) {
                    str.append(SolrConstant.star);
                } else {
                    // 如果需要范围查询的字段是date型，转成UTC
                    if (field.getGenericType().toString().equals(SolrConstant.dateStr)) {
                        c.setTime(Fields.get(object, field, Date.class));
                        str.append(dateFormat.format(c.getTime()) + SolrConstant.toStr);
                    } else {
                        str.append(Fields.get(object, field) + SolrConstant.toStr);
                    }
                }

            } else if (ScopeField.ScopeFiledEnum.LT.equals(field.getAnnotation(ScopeField.class).mode())) {
                Object value = Fields.get(object, field);
                if (value == null) {
                    str.append(SolrConstant.star);
                } else {
                    // 如果需要范围查询的字段是date型，转成UTC
                    if (field.getGenericType().toString().equals(SolrConstant.dateStr)) {
                        c.setTime(Fields.get(object, field, Date.class));
                        str.append(dateFormat.format(c.getTime()));
                    } else {
                        str.append(Fields.get(object, field));
                    }
                }

                str.append(SolrConstant.bracketRight);
                scopeEndTime++;
            }
            return;
        }

        buildCommon(field, object);
    }

    @Override
    public void buildCommon(Field field, Object object) throws IllegalAccessException {
        if (i != 0) {
            str.append(SolrConstant.andStr);
        }
        str.append(field.getName() + SolrConstant.colon + Fields.get(object, field));
        i++;
    }

    @Override
    public SolrQuery getResult() {
        solrQuery.addField(SolrConstant.star);
        if (i == 0) {
            solrQuery.setQuery(SolrConstant.queryStr);
        } else {
            solrQuery.setQuery(str.toString());
        }
        return solrQuery;
    }
}
