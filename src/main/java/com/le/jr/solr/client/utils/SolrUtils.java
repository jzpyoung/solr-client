package com.le.jr.solr.client.utils;

import com.le.jr.solr.client.annotation.IgnoreField;
import com.le.jr.solr.client.annotation.ScopeField;
import com.le.jr.solr.client.build.CommonBuilder;
import com.le.jr.solr.client.build.Director;
import com.le.jr.solr.client.common.enums.OperateEnum;
import com.le.jr.solr.client.common.enums.ZeroOneEnum;
import com.le.jr.solr.client.exceptions.SolrException;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

import static com.le.jr.solr.client.annotation.ScopeField.ScopeFiledEnum.GT;
import static com.le.jr.solr.client.annotation.ScopeField.ScopeFiledEnum.LT;

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
        Director director = new Director();
        // 初始化建造者类
        CommonBuilder builder = new CommonBuilder();
        Field[] fields = object.getClass().getDeclaredFields();
        int modifiers;
        Object fValue;
        Object scopeStart = null;
        Object scopeEnd = null;
        Integer scope = ZeroOneEnum.ZERO.getValue();
        for (Field field : fields) {
            modifiers = field.getModifiers();
            if (Modifier.isStatic(modifiers) && Modifier.isFinal(modifiers)) {
                continue;
            }

            fValue = Fields.get(object, field);
            boolean flag = judgeScopeBothNull(field, fValue, scope, scopeStart, scopeEnd);

            // static、final、被ignorefield标识的属性忽略
            if (field.isAnnotationPresent(IgnoreField.class) || (fValue == null && !field.isAnnotationPresent(ScopeField.class)) || "".equals(fValue) || flag) {
                continue;
            }

            try {
                // 指挥者执行builder
                director.construct(builder, field, object, operateEnum);
            } catch (Exception e) {
                throw new SolrException(e);
            }
        }

        return builder.getResult();
    }

    private static boolean judgeScopeBothNull(Field field, Object fValue, Integer scope, Object scopeStart, Object scopeEnd) {
        if (field.isAnnotationPresent(ScopeField.class) && GT.equals(field.getAnnotation(ScopeField.class).mode())) {
            scopeStart = fValue;
            scope++;
            return judgeScopeBothNullFlag(scope, scopeStart, scopeEnd);
        } else if (field.isAnnotationPresent(ScopeField.class) && LT.equals(field.getAnnotation(ScopeField.class).mode())) {
            scopeEnd = fValue;
            scope++;
            return judgeScopeBothNullFlag(scope, scopeStart, scopeEnd);
        }
        return false;
    }

    private static boolean judgeScopeBothNullFlag(Integer scope, Object scopeStart, Object scopeEnd) {
        if (scope > ZeroOneEnum.ONE.getValue()) {
            scope = ZeroOneEnum.ZERO.getValue();
            if (scopeStart == null && scopeEnd == null) {
                return true;
            } else {
                return false;
            }
        }
        return true;
    }
}
