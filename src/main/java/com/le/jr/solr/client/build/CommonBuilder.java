package com.le.jr.solr.client.build;

import com.le.jr.solr.client.annotation.PageField;
import com.le.jr.solr.client.annotation.ScopeField;
import com.le.jr.solr.client.common.constant.SolrConstant;
import com.le.jr.solr.client.common.enums.OperateEnum;
import com.le.jr.solr.client.utils.Fields;
import org.apache.solr.client.solrj.SolrQuery;

import java.lang.reflect.Field;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import static com.le.jr.solr.client.annotation.ScopeField.ScopeFiledEnum;
import static com.le.jr.solr.client.annotation.ScopeField.ScopeFiledEnum.GT;
import static com.le.jr.solr.client.annotation.ScopeField.ScopeFiledEnum.LT;

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
    private static Calendar calendar;

    static {
        calendar = Calendar.getInstance();
        calendar.add(Calendar.HOUR, -8);
    }

    @Override
    public void buildQuery(Field field, Object object, OperateEnum operateEnum) throws IllegalAccessException {
        switch (operateEnum) {
            case QUERY:
                this.buildPage(field, object);
                break;
            case COUNT:
                this.buildScope(field, object);
                break;
        }

    }

    @Override
    public void buildPage(Field field, Object object) throws IllegalAccessException {
        if (field.isAnnotationPresent(PageField.class)) {

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

        this.buildScope(field, object);
    }

    @Override
    public void buildScope(Field field, Object object) throws IllegalAccessException {
        if (field.isAnnotationPresent(ScopeField.class)) {
            Object value = Fields.get(object, field);

            if (GT.equals(field.getAnnotation(ScopeField.class).mode())) {
                if (scopeEndTime != 0 || i != 0) {
                    str.append(SolrConstant.andStr);
                }

                str.append(field.getAnnotation(ScopeField.class).name() + SolrConstant.bracketLeft);
                appendScopeStr(field, object, value, GT);
            } else if (LT.equals(field.getAnnotation(ScopeField.class).mode())) {
                appendScopeStr(field, object, value, LT);
                str.append(SolrConstant.bracketRight);
                scopeEndTime++;
            }
            return;
        }

        this.buildCommon(field, object);
    }

    private void appendScopeStr(Field field, Object object, Object value, ScopeFiledEnum scopeFieldEnum) {
        if (value == null) {
            str.append(SolrConstant.star);
        } else {
            // 如果需要范围查询的字段是date型，转成UTC
            if (SolrConstant.dateStr.equals(field.getGenericType().toString())) {
                calendar.setTime(Fields.get(object, field, Date.class));
                str.append(dateFormat.format(calendar.getTime()));
            } else {
                str.append(Fields.get(object, field));
            }
        }
        switch (scopeFieldEnum) {
            case GT:
                str.append(SolrConstant.toStr);
                break;
        }
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
        if (scopeEndTime == 0 && i == 0) {
            solrQuery.setQuery(SolrConstant.queryStr);
        } else {
            solrQuery.setQuery(str.toString());
        }
        return solrQuery;
    }
}
