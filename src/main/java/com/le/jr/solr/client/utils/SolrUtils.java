package com.le.jr.solr.client.utils;

import com.le.jr.solr.client.annotation.IgnoreField;
import com.le.jr.solr.client.common.SolrConstant;
import org.apache.solr.common.SolrInputDocument;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

/**
 * solr工具类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-04-11
 */
public class SolrUtils {

    private static final Logger logger = LoggerFactory.getLogger(SolrUtils.class);

    /**
     * 把vo对象转换成SolrInputDocument
     *
     * @param object 待转换对象
     * @return solr查询结果SolrInputDocument
     */
    public static SolrInputDocument Vo4Solrdoc(Object object) {

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
                logger.info("实现vo到SolrInputDocument的转换结果{}:{}", f.getName(), fValue);
                solrdoc.addField(f.getName(), fValue);
            } catch (Exception e) {
                e.printStackTrace();
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
    public static List<SolrInputDocument> List4Solrdoclist(List<? extends Object> oList) {

        if (oList != null && oList.size() > 0) {
            List<SolrInputDocument> list = new ArrayList<SolrInputDocument>();
            for (Object object : oList) {
                SolrInputDocument solrdoc = Vo4Solrdoc(object);
                list.add(solrdoc);
            }
            return list;

        }

        return null;

    }

    /**
     * 把对象属性和value转换成SolrQuery的query语法字符串
     *
     * @param object 待转换对象
     * @return query语法字符串
     */
    public static String Vo4QueryStr(Object object) {
        int i = 0;

        StringBuffer str = new StringBuffer();

        Field[] fields = object.getClass().getDeclaredFields();
        for (Field f : fields) {
            f.setAccessible(true);
            int modifiers = f.getModifiers();
            try {
                Object fValue = f.get(object);

                if (Modifier.isStatic(modifiers) && Modifier.isFinal(modifiers) || fValue == null || f.isAnnotationPresent(IgnoreField.class)) {
                    continue;
                }

                if (i != 0 && !"endTime".equals(f.getName())) {
                    str.append(" AND ");
                }
                if ("startTime".equals(f.getName())) {
                    str.append("createTime" + ":[" + f.get(object) + " TO ");
                } else if ("endTime".equals(f.getName())) {
                    str.append(f.get(object) + "]");
                } else {
                    str.append(f.getName() + ":" + f.get(object));
                }

            } catch (Exception e) {
                logger.error(e.getMessage(), e);
            }
            i++;
        }

        if (i == 0) {
            return SolrConstant.queryStr;
        }

        return str.toString();
    }

}
