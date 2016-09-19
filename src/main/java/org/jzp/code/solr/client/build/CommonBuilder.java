package org.jzp.code.solr.client.build;

import org.apache.solr.client.solrj.SolrQuery;
import org.jzp.code.solr.client.annotation.*;
import org.jzp.code.solr.client.common.constant.SolrConstant;
import org.jzp.code.solr.client.common.enums.OperateEnum;
import org.jzp.code.solr.client.common.enums.ScopeEnum;
import org.jzp.code.solr.client.common.enums.ZeroOneEnum;
import org.jzp.code.solr.client.utils.Fields;

import java.lang.reflect.Field;
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
    private int andTime = ZeroOneEnum.ZERO.getValue();

    @Override
    public void buildScope(Field field, Object object, Map<String, Object> map) {
        if (andTime != ZeroOneEnum.ZERO.getValue()) {
            str.append(SolrConstant.andStr);
        }

        Object scopeStart = buildScopeCond(ScopeEnum.SCOPESTART, field, map);
        Object scopeEnd = buildScopeCond(ScopeEnum.SCOPEEND, field, map);

        str.append(field.getAnnotation(ScopeField.class).name() + SolrConstant.bracketLeft + scopeStart + SolrConstant.toStr + scopeEnd + SolrConstant.bracketRight);
        andTime++;
    }

    @Override
    public void buildPage(Field field, Object object, OperateEnum operateEnum) {
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
        }
    }

    @Override
    public void buildSort(Field field, Object object, OperateEnum operateEnum) {
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
        }
    }

    @Override
    public void buildIn(Field field, Object object) {
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
                    inStr += SolrConstant.orStr;
                }
                if (inEach instanceof Date) {
                    inEach = buildDate(inEach);
                }
                inStr += inEach;
            }
            str.append(field.getAnnotation(InField.class).name() + SolrConstant.colon + SolrConstant.miniBracketLeft + inStr + SolrConstant.miniBracketRight);
            andTime++;
        }
    }

    @Override
    public void buildNotIn(Field field, Object object) {
        List<Object> notInlist = (List) Fields.get(object, field);
        Object notInEach;
        String notInStr = SolrConstant.star + SolrConstant.notStr;
        if (notInlist != null && notInlist.size() > ZeroOneEnum.ZERO.getValue()) {
            if (andTime != ZeroOneEnum.ZERO.getValue()) {
                str.append(SolrConstant.andStr);
            }
            for (int i = ZeroOneEnum.ZERO.getValue(); i < notInlist.size(); i++) {
                if (field.isAnnotationPresent(DimField.class)) {
                    notInEach = SolrConstant.star + notInlist.get(i) + SolrConstant.star;
                } else {
                    notInEach = notInlist.get(i);
                }
                if (i > ZeroOneEnum.ZERO.getValue()) {
                    notInStr += SolrConstant.notStr;
                }
                if (notInEach instanceof Date) {
                    notInEach = buildDate(notInEach);
                }
                notInStr += notInEach;
            }
            str.append(field.getAnnotation(NotInField.class).name() + SolrConstant.colon + SolrConstant.miniBracketLeft + notInStr + SolrConstant.miniBracketRight);
            andTime++;
        }
    }

    @Override
    public void buildCommon(Field field, Object object) {
        if (andTime != ZeroOneEnum.ZERO.getValue()) {
            str.append(SolrConstant.andStr);
        }

        Object value = Fields.get(object, field);

        // 处理负数
        value = buildNegativeNumber(value, field, object);

        if (field.isAnnotationPresent(DimField.class)) {
            str.append(field.getName() + SolrConstant.colon + SolrConstant.star + value + SolrConstant.star);
        } else {
            str.append(field.getName() + SolrConstant.colon + value);
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
