package com.le.jr.solr.client.build;

import com.le.jr.solr.client.annotation.*;
import com.le.jr.solr.client.common.constant.SolrConstant;
import com.le.jr.solr.client.common.enums.OperateEnum;
import com.le.jr.solr.client.common.enums.ScopeEnum;
import com.le.jr.solr.client.common.enums.ZeroOneEnum;
import com.le.jr.solr.client.utils.Fields;
import org.apache.solr.client.solrj.SolrQuery;

import java.lang.reflect.Field;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
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
    private int andTime = ZeroOneEnum.ZERO.getValue();
    private static Calendar calendar;

    static {
        calendar = Calendar.getInstance();
        calendar.add(Calendar.HOUR, -8);
    }

    @Override
    public void buildScope(Field field, Object object, Map<String, Object> map) throws IllegalAccessException {
        if (andTime != ZeroOneEnum.ZERO.getValue()) {
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
        andTime++;
    }

    @Override
    public void buildPage(Field field, Object object, OperateEnum operateEnum) throws IllegalAccessException {
        if (field.isAnnotationPresent(PageField.class)) {
            if (!OperateEnum.QUERY.equals(operateEnum)) {
                return;
            }

            switch (field.getAnnotation(PageField.class).name()) {
                case PAGESIZE:
                    solrQuery.setRows(Fields.get(object, field, Integer.class));
                    break;
                case START:
                    solrQuery.setStart(Fields.get(object, field, Integer.class));
                    break;
                default:
                    break;
            }
            return;
        }
        this.buildSort(field, object, operateEnum);
    }

    @Override
    public void buildSort(Field field, Object object, OperateEnum operateEnum) throws IllegalAccessException {
        if (field.isAnnotationPresent(SortField.class)) {
            if (!OperateEnum.QUERY.equals(operateEnum)) {
                return;
            }

            switch (field.getAnnotation(SortField.class).mode()) {
                case ASC:
                    solrQuery.addSort(field.getAnnotation(SortField.class).name(), SolrQuery.ORDER.asc);
                    break;
                case DESC:
                    solrQuery.addSort(field.getAnnotation(SortField.class).name(), SolrQuery.ORDER.desc);
                    break;
                default:
                    solrQuery.addSort(field.getAnnotation(SortField.class).name(), SolrQuery.ORDER.asc);
                    break;
            }
            return;
        }
        this.buildIn(field, object);
    }

    @Override
    public void buildIn(Field field, Object object) throws IllegalAccessException {
        if (field.isAnnotationPresent(InField.class)) {
            List<Object> inlist = (List) Fields.get(object, field);
            Object inEach;
            String inStr = "";
            if (inlist != null && inlist.size() > ZeroOneEnum.ZERO.getValue()) {
                if (andTime != ZeroOneEnum.ZERO.getValue()) {
                    str.append(SolrConstant.andStr);
                }
                for (int i = ZeroOneEnum.ZERO.getValue(); i < inlist.size(); i++) {
                    inEach = inlist.get(i);
                    if (i > ZeroOneEnum.ZERO.getValue()) {
                        inStr = inStr + SolrConstant.orStr;
                    }
                    if (inEach instanceof Date) {
                        calendar.setTime((Date) inEach);
                        inEach = dateFormat.format(calendar.getTime());
                    }
                    inStr = inStr + inEach;
                }
                str.append(field.getAnnotation(InField.class).name() + SolrConstant.colon + SolrConstant.miniBracketLeft + inStr + SolrConstant.miniBracketRight);
                andTime++;
            }
            return;
        }
        this.buildCommon(field, object);
    }

    @Override
    public void buildCommon(Field field, Object object) throws IllegalAccessException {
        if (andTime != ZeroOneEnum.ZERO.getValue()) {
            str.append(SolrConstant.andStr);
        }

        if (field.isAnnotationPresent(DimField.class)) {
            str.append(field.getName() + SolrConstant.colon + SolrConstant.star + Fields.get(object, field) + SolrConstant.star);
        } else {
            str.append(field.getName() + SolrConstant.colon + Fields.get(object, field));
        }

        andTime++;
    }

    @Override
    public SolrQuery getResult() {
        solrQuery.addField(SolrConstant.star);
        if (andTime != ZeroOneEnum.ZERO.getValue()) {
            solrQuery.setQuery(str.toString());
        } else {
            solrQuery.setQuery(SolrConstant.queryStr);
        }
        return solrQuery;
    }
}
