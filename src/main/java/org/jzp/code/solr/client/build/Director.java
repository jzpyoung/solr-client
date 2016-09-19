package org.jzp.code.solr.client.build;

import com.google.common.collect.Maps;
import org.apache.solr.client.solrj.SolrQuery;
import org.jzp.code.solr.client.annotation.*;
import org.jzp.code.solr.client.common.enums.OperateEnum;
import org.jzp.code.solr.client.common.enums.ScopeEnum;
import org.jzp.code.solr.client.common.enums.ZeroOneEnum;
import org.jzp.code.solr.client.utils.Fields;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Map;

/**
 * 指挥者类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-27
 */
public class Director {

    private Map<String, Object> scopeMap;
    private Builder builder;
    private OperateEnum operateEnum;

    public Director(Builder builder, OperateEnum operateEnum) {
        scopeMap = Maps.newHashMap();
        scopeMap.put(ScopeEnum.SCOPE.getValue(), ZeroOneEnum.ZERO.getValue());
        scopeMap.put(ScopeEnum.SCOPESTART.getValue(), null);
        scopeMap.put(ScopeEnum.SCOPEEND.getValue(), null);
        this.builder = builder;
        this.operateEnum = operateEnum;
    }

    public void startBuilder(Field field, Object object) {
        // static、final属性忽略
        int modifiers = field.getModifiers();
        if (Modifier.isStatic(modifiers) && Modifier.isFinal(modifiers)) {
            return;
        }

        // null、""、被ignorefield标识的属性忽略
        Object fValue = Fields.get(object, field);
        if (field.isAnnotationPresent(IgnoreField.class) || (fValue == null && !field.isAnnotationPresent(ScopeField.class)) || "".equals(fValue)) {
            return;
        }

        // scope范围无效忽略
        boolean flag = isInvalidScope(field, fValue, scopeMap);
        if (flag) {
            return;
        }

        // 指挥者执行builder
        if (field.isAnnotationPresent(ScopeField.class)) {
            builder.buildScope(field, object, scopeMap);
        } else if (field.isAnnotationPresent(PageField.class)) {
            builder.buildPage(field, object, operateEnum);
        } else if (field.isAnnotationPresent(SortField.class)) {
            builder.buildSort(field, object, operateEnum);
        } else if (field.isAnnotationPresent(InField.class)) {
            builder.buildIn(field, object);
        } else if (field.isAnnotationPresent(NotInField.class)) {
            builder.buildNotIn(field, object);
        } else if (field.isAnnotationPresent(NotInField.class)) {
            builder.buildNotIn(field, object);
        } else {
            builder.buildCommon(field, object);
        }
    }

    public SolrQuery getResult() {
        return builder.getResult();
    }

    private static boolean isInvalidScope(Field field, Object fValue, Map<String, Object> scopeMap) {
        int scope;
        if (field.isAnnotationPresent(ScopeField.class) && ScopeField.ScopeFiledEnum.GT.equals(field.getAnnotation(ScopeField.class).mode())) {
            scopeMap.put(ScopeEnum.SCOPESTART.getValue(), fValue);
            scope = (int) scopeMap.get(ScopeEnum.SCOPE.getValue());
            scopeMap.put(ScopeEnum.SCOPE.getValue(), ++scope);
            return judgeIsInvalidScope(scopeMap);
        } else if (field.isAnnotationPresent(ScopeField.class) && ScopeField.ScopeFiledEnum.LT.equals(field.getAnnotation(ScopeField.class).mode())) {
            scopeMap.put(ScopeEnum.SCOPEEND.getValue(), fValue);
            scope = (int) scopeMap.get(ScopeEnum.SCOPE.getValue());
            scopeMap.put(ScopeEnum.SCOPE.getValue(), ++scope);
            return judgeIsInvalidScope(scopeMap);
        }
        return false;
    }

    private static boolean judgeIsInvalidScope(Map<String, Object> scopeMap) {
        if ((int) scopeMap.get(ScopeEnum.SCOPE.getValue()) > ZeroOneEnum.ONE.getValue()) {
            scopeMap.put(ScopeEnum.SCOPE.getValue(), ZeroOneEnum.ZERO.getValue());
            if (scopeMap.get(ScopeEnum.SCOPESTART.getValue()) == null && scopeMap.get(ScopeEnum.SCOPEEND.getValue()) == null) {
                return true;
            } else {
                return false;
            }
        }
        return true;
    }
}
