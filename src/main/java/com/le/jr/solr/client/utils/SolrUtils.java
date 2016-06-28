package com.le.jr.solr.client.utils;

import com.le.jr.solr.client.SolrVoDemo;
import com.le.jr.solr.client.annotation.IgnoreField;
import com.le.jr.solr.client.annotation.ScopeField;
import com.le.jr.solr.client.build.CommonBuilder;
import com.le.jr.solr.client.build.Director;
import com.le.jr.solr.client.exceptions.SolrException;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
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

    /**
     * 把对象转换成SolrQuery
     *
     * @param object 待转换对象
     * @return SolrQuery
     */
    public static SolrQuery vo2SolrQuery(Object object) {
        // 初始化指挥者类
        Director director = new Director();
        // 初始化建造者类
        CommonBuilder builder = new CommonBuilder();
        Field[] fields = object.getClass().getDeclaredFields();
        int modifiers;
        Object fValue;
        for (Field field : fields) {
            modifiers = field.getModifiers();
            fValue = Fields.get(object, field, Object.class);

            // static、final、被ignorefield标识的属性忽略
            if (Modifier.isStatic(modifiers) && Modifier.isFinal(modifiers) || field.isAnnotationPresent(IgnoreField.class) || (fValue == null && !field.isAnnotationPresent(ScopeField.class))) {
                continue;
            }

            try {
                // 指挥者执行builder
                director.construct(builder, field, object);
            } catch (Exception e) {
                throw new SolrException(e);
            }
        }

        return builder.getResult();
    }

    public static void main(String[] args) {
        SolrVoDemo demo = new SolrVoDemo();
        demo.setName("123");
        demo.setPageSize(5);
        demo.setStart(1);
        demo.setStartTime(new Date());
        demo.setEndTime(new Date());
        SolrQuery solrQuery = SolrUtils.vo2SolrQuery(demo);
        System.out.println("aaa");
    }

}
