package com.le.jr.solr.client.build;

import com.le.jr.solr.client.annotation.PageField;
import com.le.jr.solr.client.annotation.ScopeField;
import com.le.jr.solr.client.common.constant.SolrConstant;
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
    private StringBuffer str = new StringBuffer();
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
                solrQuery.setRows((int) field.get(object));
            }
            if (PageField.PageFiledEnum.START.equals(field.getAnnotation(PageField.class).name())) {
                solrQuery.setStart((int) field.get(object));
            }
        } else {
            buildScope(field, object);
        }
    }

    @Override
    public void buildScope(Field field, Object object) throws IllegalAccessException {
        if (field.isAnnotationPresent(ScopeField.class)) {
            if (ScopeField.ScopeFiledEnum.GT.equals(field.getAnnotation(ScopeField.class).mode())) {
                if (scopeEndTime != 0) {
                    str.append(SolrConstant.andStr);
                }
                // 如果需要范围查询的字段是date型，转成UTC
                if (field.getGenericType().toString().equals(SolrConstant.dateStr)) {
                    c.setTime((Date) field.get(object));
                    str.append(field.getAnnotation(ScopeField.class).name() + SolrConstant.bracketLeft + dateFormat.format(c.getTime()) + SolrConstant.toStr);
                } else {
                    str.append(field.getAnnotation(ScopeField.class).name() + SolrConstant.bracketLeft + field.get(object) + SolrConstant.toStr);
                }
            } else if (ScopeField.ScopeFiledEnum.LT.equals(field.getAnnotation(ScopeField.class).mode())) {
                // 如果需要范围查询的字段是date型，转成UTC
                if (field.getGenericType().toString().equals(SolrConstant.dateStr)) {
                    c.setTime((Date) field.get(object));
                    str.append(dateFormat.format(c.getTime()) + SolrConstant.bracketRight);
                } else {
                    str.append(field.get(object) + SolrConstant.bracketRight);
                }
                scopeEndTime++;
            }
        } else {
            buildCommon(field, object);
        }
    }

    @Override
    public void buildCommon(Field field, Object object) throws IllegalAccessException {
        if (i != 0) {
            str.append(SolrConstant.andStr);
        }
        str.append(field.getName() + SolrConstant.colon + field.get(object));
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
