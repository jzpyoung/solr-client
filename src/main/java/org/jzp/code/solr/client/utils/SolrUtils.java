package org.jzp.code.solr.client.utils;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.jzp.code.solr.client.build.CommonBuilder;
import org.jzp.code.solr.client.build.Director;
import org.jzp.code.solr.client.common.enums.OperateEnum;
import org.jzp.code.solr.client.exceptions.SolrException;

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

    /**
     * 把vo对象转换成SolrInputDocument
     *
     * @param object 待转换对象
     * @return solr查询结果SolrInputDocument
     */
    public static SolrInputDocument vo2Solrdoc(Object object) {
        SolrInputDocument solrdoc = new SolrInputDocument();
        Field[] fields = object.getClass().getDeclaredFields();
        int modifiers;
        Object fValue;
        for (Field f : fields) {
            modifiers = f.getModifiers();
            if (Modifier.isStatic(modifiers) && Modifier.isFinal(modifiers)) {
                continue;
            }

            fValue = Fields.get(object, f);

            try {
                // java优先级 && > ||
                if (fValue == null) {
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
        List<SolrInputDocument> list = null;
        if (oList != null && !oList.isEmpty()) {
            list = new ArrayList<>();
            for (Object object : oList) {
                SolrInputDocument solrdoc = vo2Solrdoc(object);
                list.add(solrdoc);
            }
        }
        return list;
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
    public static SolrQuery vo2SolrQuery(Object object, OperateEnum operateEnum) {
        // 初始化指挥者类
        Director director = new Director(new CommonBuilder(), operateEnum);
        Field[] fields = object.getClass().getDeclaredFields();
        for (Field field : fields) {
            try {
                // 指挥者执行builder
                director.startBuilder(field, object);
            } catch (Exception e) {
                throw new SolrException(e);
            }
        }
        return director.getResult();
    }
}
