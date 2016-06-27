package com.le.jr.solr.client.utils;

import com.le.jr.solr.client.annotation.IgnoreField;
import com.le.jr.solr.client.annotation.PageField;
import com.le.jr.solr.client.annotation.ScopeField;
import com.le.jr.solr.client.common.constant.SolrConstant;
import com.le.jr.solr.client.exceptions.SolrException;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * solr工具类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-04-11
 */
public class SolrUtils {

    private static DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");

    /**
     * 把vo对象转换成SolrInputDocument
     *
     * @param object 待转换对象
     * @return solr查询结果SolrInputDocument
     */
    public static SolrInputDocument vo2Solrdoc(Object object) {
        SolrInputDocument solrdoc = new SolrInputDocument();

        Field[] fields = object.getClass().getDeclaredFields();
        for (Field f : fields) {

            try {
                f.setAccessible(true);
                Object fValue = f.get(object);
                int modifier = f.getModifiers();
                // java优先级 && > ||
                if (Modifier.isStatic(modifier) && Modifier.isFinal(modifier) || fValue == null) {
                    continue;
                }
                solrdoc.addField(f.getName(), fValue);
            } catch (Exception e) {
                throw new SolrException(e);
            }
        }
        return solrdoc;
    }

    /**
     * 把list<vo>转换成List<SolrInputDocument>
     *
     * @param oList 待转换对象list
     * @return solr查询结果List<SolrInputDocument>
     */
    public static List<SolrInputDocument> list2Solrdoclist(List<? extends Object> oList) {
        if (oList != null && oList.size() > 0) {
            List<SolrInputDocument> list = new ArrayList<>();
            for (Object object : oList) {
                SolrInputDocument solrdoc = vo2Solrdoc(object);
                list.add(solrdoc);
            }
            return list;
        }
        return null;
    }

    /**
     * 把对象转换成SolrQuery
     *
     * @param object 待转换对象
     * @return SolrQuery
     */
    public static SolrQuery vo2SolrQuery(Object object) {
        int i = 0;
        int scopeTime = 0;
        SolrQuery query = new SolrQuery();
        query.addField(SolrConstant.star);

        StringBuffer str = new StringBuffer();

        Field[] fields = object.getClass().getDeclaredFields();
        for (Field f : fields) {
            f.setAccessible(true);
            int modifiers = f.getModifiers();
            try {
                Object fValue = f.get(object);

                // static、final、被ignorefield标识的属性忽略
                if (Modifier.isStatic(modifiers) && Modifier.isFinal(modifiers) || fValue == null || f.isAnnotationPresent(IgnoreField.class)) {
                    continue;
                }

                // 判断PageField注解标识的分页属性并设置进SolrQuery
                if (f.isAnnotationPresent(PageField.class)) {
                    if (PageField.PageFiledEnum.PAGESIZE.equals(f.getAnnotation(PageField.class).name())) {
                        query.setRows((int) f.get(object));
                    }
                    if (PageField.PageFiledEnum.START.equals(f.getAnnotation(PageField.class).name())) {
                        query.setStart((int) f.get(object));
                    }
                } else {
                    // 判断ScopeField注解标识的范围属性并设置进SolrQuery
                    if (f.isAnnotationPresent(ScopeField.class)) {
                        if (ScopeField.ScopeFiledEnum.GT.equals(f.getAnnotation(ScopeField.class).mode())) {
                            if (scopeTime != 0) {
                                str.append(SolrConstant.andStr);
                            }
                            // 如果需要范围查询的字段是date型，转成UTC
                            if (f.getGenericType().toString().equals(SolrConstant.dateStr)) {
                                Calendar c = Calendar.getInstance();
                                c.setTime((Date) f.get(object));
                                c.add(Calendar.HOUR, -8);
                                str.append(f.getAnnotation(ScopeField.class).name() + SolrConstant.bracketLeft + dateFormat.format(c.getTime()) + SolrConstant.toStr);
                            } else {
                                str.append(f.getAnnotation(ScopeField.class).name() + SolrConstant.bracketLeft + f.get(object) + SolrConstant.toStr);
                            }

                        } else if (ScopeField.ScopeFiledEnum.LT.equals(f.getAnnotation(ScopeField.class).mode())) {
                            // 如果需要范围查询的字段是date型，转成UTC
                            if (f.getGenericType().toString().equals(SolrConstant.dateStr)) {
                                Calendar c = Calendar.getInstance();
                                c.setTime((Date) f.get(object));
                                c.add(Calendar.HOUR, -8);
                                str.append(dateFormat.format(c.getTime()) + SolrConstant.bracketRight);
                            } else {
                                str.append(f.get(object) + SolrConstant.bracketRight);
                            }
                            scopeTime++;
                        }
                    } else {
                        // 没有被任何注解标识的普通属性设置进SolrQuery
                        if (i != 0) {
                            str.append(SolrConstant.andStr);
                        }
                        str.append(f.getName() + SolrConstant.colon + f.get(object));
                    }
                    i++;
                }
            } catch (Exception e) {
                throw new SolrException(e);
            }
        }

        if (i == 0) {
            query.setQuery(SolrConstant.queryStr);
        } else {
            query.setQuery(str.toString());
        }
        return query;
    }

    /**
     * 把QueryResponse转换成List<VO>
     *
     * @param docList 待转换查询结果
     * @param cls     转换目标类型
     * @return 转换结果List<VO>
     */
    public static <T> List<T> queryResponse2List(SolrDocumentList docList, Class<T> cls) {
        String listJson = Gsons.toJson(docList);
        return Gsons.fromJson2List(listJson, cls);
    }

}
