# Solr-Client API文档

+ 已实现组件:
	
	+ 单条添加: <a href="#addSingle-api">AddSingle()</a>
	+ 批量添加: <a href="#addMulti-api">AddMulti()</a>
	+ 删除: <a href="#delete-api">Delete()</a>
	+ 查询集合: <a href="#query-api">Query()</a>
	+ 查询数量: <a href="#count-api">Count()</a>
	+ 求和: <a href="#sum-api">Sum()</a>
	+ 分组: <a href="#groupBy-api">GroupBy()</a>
	+ 聚合: <a href="#aggregation-api">Aggregation()</a>

+ VO Demo:

    ```Java
    public class SolrVoDemo {
 
    // 普通参数，id为solr schema的字段名称，name的value为对应的字段的值
    private String id;
 
    // 忽略的参数，不会转化为add或者query的条件
    @IgnoreField
    private String letvUserId;
     
    // 模糊查询参数，name为solr schema的字段名称，name的value为对应的字段的值
    @DimField
    private String name;
 
    // 分页参数：一页几条
    @PageField(name = PageField.PageFiledEnum.PAGESIZE)
    private Integer pageSize;
    
    // 分页参数：从第几条开始，类似mysql的limit的第一个参数
    @PageField(name = PageField.PageFiledEnum.START)
    private Integer start;
 
    // 范围查询参数，name为solr schema的字段名称，mode为操作，GT代表大于
    @ScopeField(name = "createTime", mode = ScopeField.ScopeFiledEnum.GT)
    private Date startTime;
 
    // 范围查询参数，name为solr schema的字段名称，mode为操作，LT代表小于
    @ScopeField(name = "createTime", mode = ScopeField.ScopeFiledEnum.LT)
    private Date endTime;
 
    // IN查询，name为solr schema的字段名称，userList为in的范围list
    @InField(name = "time")
    private List<Date> timeList;
 
    // 排序，name为solr schema的字段名称，mode为操作，DESC代表降序
    @SortField(name = "orderTime", mode = SortField.SortFiledEnum.DESC)
    private String sort1 = "订单时间降序";
     
    // 排序，name为solr schema的字段名称，mode为操作，ASC代表升序
    @SortField(name = "venderCode", mode = SortField.SortFiledEnum.ASC)
    private String sort2 = "商户号升序";
 
    /**
     * getters && setters
     */
    }
    ```
	
+ **<a id="addSingle-api">单条添加AddSingle()</a>**:

    ```java
    /**
     * 增加单条索引数据
     *
     * @param document 待添加文档
     * @return 操作是否成功
     */
    boolean addSingle(SolrInputDocument document);
	
    /**
     * 增加单条索引数据
     *
     * @param object 待添加对象
     * @return 操作是否成功
     */
    boolean addSingle(Object object);
    ```

+ **<a id="addMulti-api">批量添加AddMulti()</a>**:
	
    ```java
    /**
     * 增加多条索引数据
     *
     * @param documents 待添加文档集合
     * @return 操作是否成功
     */
    boolean addMulti(List<SolrInputDocument> documents);
    
    /**
     * 增加多条索引数据
     *
     * @param lists 待添加集合
     * @return 操作是否成功
     */
    boolean addMulti4VO(List<? extends Object> lists);
    ```

+ **<a id="delete-api">删除Delete()</a>**:
	
    ```java
    /**
     * 删除索引
     *
     * @param sq 删除条件
     * @return 操作是否成功
     */
    boolean delete(String sq);
    
    /**
     * 删除索引
     *
     * @param object 删除条件对象
     * @return 操作是否成功
     */
    boolean delete(Object object);
    ```
	
+ **<a id="query-api">查询集合Query()</a>**:
	
    ```java
    /**
     * 查询
     *
     * @param sq 查询条件
     * @return 查询结果
     */
    QueryResponse query(SolrQuery sq);

    /**
     * 查询
     *
     * @param queryObj 查询条件对象
     * @param clazz    结果集合对象class
     * @return 查询结果
     */
    <T> List<T> query(Object queryObj, Class<T> clazz);
    ```
	
+ **<a id="count-api">查询数量Count()</a>**:
	
    ```java
    /**
     * count
     *
     * @param solrQuery 查询条件
     * @return count
     */
    Long count(SolrQuery solrQuery);

    /**
     * count
     *
     * @param object 查询条件对象
     * @return count
     */
    Long count(Object object);
    ```
	
+ **<a id="sum-api">求和Sum()</a>**:
	
    ```java
    /**
     * sum
     *
     * @param sq     查询条件
     * @param fields 待求和字段
     * @return sum的结果
     */
    Map<String, Long> sum(SolrQuery sq, String... fields);

    /**
     * sum
     *
     * @param object 查询条件对象
     * @param fields 待求和字段
     * @return sum的结果
     */
    Map<String, Long> sum(Object object, String... fields);
    ```
	
+ **<a id="groupBy-api">分组GroupBy()</a>**:
	
    ```java
    /**
     * group by
     *
     * @param solrQuery 查询条件
     * @param field     groupby字段
     * @return groupby的结果
     */
    Map<String, Long> group(SolrQuery solrQuery, String field);

    /**
     * group by
     *
     * @param object 查询条件对象
     * @param field  groupby字段
     * @return groupby的结果
     */
    Map<String, Long> group(Object object, String field);
    ```
	
+ **<a id="aggregation-api">聚合Aggregation()</a>**:
	
    ```java
    /**
     * 聚合(min、max、avg、count、sum)目前只支持sum
     *
     * @param agg    聚合操作枚举
     * @param sq     查询条件
     * @param fields 待聚合字段
     * @return sum的结果
     */
    Map<String, Long> aggregate(AggregateEnum agg, SolrQuery sq, String... fields);

    /**
     * 聚合(min、max、avg、count、sum)目前只支持sum
     *
     * @param agg    聚合操作枚举
     * @param object 查询条件对象
     * @param fields 待聚合字段
     * @return sum的结果
     */
    Map<String, Long> aggregate(AggregateEnum agg, Object object, String... fields);
    ```
