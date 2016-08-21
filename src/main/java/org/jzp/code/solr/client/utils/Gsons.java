package org.jzp.code.solr.client.utils;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.google.gson.reflect.TypeToken;

import java.util.ArrayList;
import java.util.List;

/**
 * Gson工具类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-25
 */
public final class Gsons {

    private static Gson gson = new Gson();

    /**
     * 将对象转为JSON字符串
     *
     * @param o 对象
     * @return 对象JSON字符串, 或null
     */
    public static String toJson(Object o) {
        return o == null ? null : gson.toJson(o);
    }

    /**
     * 转换JSON字符串为对象
     *
     * @param json  JSON字符串
     * @param clazz 对应的类
     * @param <T>   泛型
     * @return 对象或null
     */
    public static <T> T fromJson(String json, Class<T> clazz) {
        if (json == null || "".equals(json)) return null;
        return gson.fromJson(json, clazz);
    }

    /**
     * 转换JSON字符串为对象
     *
     * @param json JSON字符串
     * @param tt   泛型类型, 如:
     *             TypeToken<List<Student>> ssType = new TypeToken<List<Student>>(){};
     * @param <T>  泛型
     * @return 对象或null
     */
    public static <T> T fromJson(String json, TypeToken<T> tt) {
        if (json == null || "".equals(json)) return null;
        return gson.fromJson(json, tt.getType());
    }

    /**
     * 转换JSON字符串为List<VO>
     *
     * @param json JSON字符串
     * @param cls  vo
     * @param <T>  泛型
     * @return 对象或null
     */
    public static <T> List<T> fromJson2List(String json, Class<T> cls) {
        if (json == null || "".equals(json)) return null;

        List<T> lst = new ArrayList<>();

        JsonArray array = new JsonParser().parse(json).getAsJsonArray();
        for (final JsonElement elem : array) {
            lst.add(new Gson().fromJson(elem, cls));
        }

        return lst;
    }

}
