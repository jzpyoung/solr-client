package org.jzp.code.solr.client.utils;

import sun.misc.Unsafe;

import java.lang.reflect.Field;

/**
 * Unsafe反射工具类
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-06-28
 */
public abstract class Fields {

    private static final Unsafe unsafe = getUnsafe();

    private static Unsafe getUnsafe() {
        try {
            Field f = Unsafe.class.getDeclaredField("theUnsafe");
            f.setAccessible(true);
            return (Unsafe) f.get(null);
        } catch (Exception e) {
            throw new RuntimeException("failed to get unsafe instance, cause");
        }
    }

    /**
     * put field to target object
     *
     * @param target target object
     * @param name   field name
     * @param value  field valiue
     */
    public static void put(Object target, String name, Object value) {
        try {
            Field field = target.getClass().getField(name);
            field.setAccessible(true);
            long fieldOffset = unsafe.objectFieldOffset(field);
            unsafe.putObject(target, fieldOffset, value);
        } catch (NoSuchFieldException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * put field to target object
     *
     * @param target target object
     * @param field  object field
     * @param value  field valiue
     */
    public static void put(Object target, Field field, Object value) {
        try {
            field.setAccessible(true);
            long fieldOffset = unsafe.objectFieldOffset(field);
            unsafe.putObject(target, fieldOffset, value);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * get field of target object
     *
     * @param target target object
     * @param name   field name
     * @return the field value
     */
    public static Object get(Object target, String name) {
        GetField getField = new GetField(target.getClass(), name).invoke();
        Field field = getField.getField();
        return get(target, field, Object.class);
    }

    /**
     * get field of target object
     *
     * @param target target object
     * @return the field value
     */
    public static Object get(Object target, Field field) {
        return get(target, field, Object.class);
    }

    /**
     * get field of target T
     *
     * @param target target object
     * @param name   field name
     * @param <T>    generic type
     * @return the field value
     */
    public static <T> T get(Object target, String name, Class<T> clazz) {
        try {
            return get(target, target.getClass().getDeclaredField(name), clazz);
        } catch (NoSuchFieldException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * get field of target T
     *
     * @param target target object
     * @param field  field
     * @param <T>    generic type
     * @return the field value
     */
    @SuppressWarnings("unchecked")
    public static <T> T get(Object target, Field field, Class<T> clazz) {
        try {
            long fieldOffset = unsafe.objectFieldOffset(field);
            return (T) unsafe.getObject(target, fieldOffset);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static class GetField {
        private Class target;
        private String name;
        private Field field;

        public GetField(Class target, String name) {
            this.target = target;
            this.name = name;
        }


        public Field getField() {
            return field;
        }

        public GetField invoke() {
            try {
                field = target.getDeclaredField(name);
            } catch (NoSuchFieldException e) {
                Class superclass = target.getSuperclass();
                if (superclass == null) {
                    throw new RuntimeException(e);
                }
                target = superclass;
                return invoke();
            }
            return this;
        }
    }
}