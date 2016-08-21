# solr-client
solr客户端
#spring接入文档
+ 添加 solr-client 依赖：

    ```xml
    <dependency>
        <groupId>com.le.jr</groupId>
        <artifactId>solr-client</artifactId>
        <version>1.0.4</version>
    </dependency>
    ```
+ spring配置文件配置：

    + 随机策略配置

    ```xml
    <!-- master，slave数据源组配置 -->
    <bean id="solrServerGroup" class="com.le.jr.solr.client.datasource.SolrServerGroup" destroy-method="destory">
       <property name="masterServer">
          <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
             <constructor-arg>
                <value>http://{ip}:{port}/solr/{collection1}</value>
             </constructor-arg>
             <property name="connectionTimeout" value="1000" />
             <property name="soTimeout" value="1500" />
          </bean>
       </property>
       
       <property name="slaveServerList">
          <list>
             <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
                <constructor-arg>
                   <value>http://{ip}:{port}/solr/{collection1}</value>
                </constructor-arg>
                <property name="connectionTimeout" value="1000" />
                <property name="soTimeout" value="1500" />
             </bean>
             <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
                <constructor-arg>
                   <value>http://{ip}:{port}/solr/{collection1}</value>
                </constructor-arg>
                <property name="connectionTimeout" value="1000" />
                <property name="soTimeout" value="1500" />
             </bean>
          </list>
       </property>
       
       <property name="loadBalance">
          <bean class="com.le.jr.solr.client.loadstrategic.RandomLoadBalance" />
       </property>
    </bean>
    
    <bean id="solrClient" class="com.le.jr.solr.client.SolrHttpClient">
       <property name="solrServerGroup" ref="solrServerGroup" />
    </bean>
    ```

    + 轮询策略配置

    ```xml
    <!-- master，slave数据源组配置 -->
    <bean id="solrServerGroup" class="com.le.jr.solr.client.datasource.SolrServerGroup" destroy-method="destory">
       <property name="masterServer">
          <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
             <constructor-arg>
                <value>http://{ip}:{port}/solr/{collection1}</value>
             </constructor-arg>
             <property name="connectionTimeout" value="1000" />
             <property name="soTimeout" value="1500" />
          </bean>
       </property>
       
       <property name="slaveServerList">
          <list>
             <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
                <constructor-arg>
                   <value>http://{ip}:{port}/solr/{collection1}</value>
                </constructor-arg>
                <property name="connectionTimeout" value="1000" />
                <property name="soTimeout" value="1500" />
             </bean>
             <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
                <constructor-arg>
                   <value>http://{ip}:{port}/solr/{collection1}</value>
                </constructor-arg>
                <property name="connectionTimeout" value="1000" />
                <property name="soTimeout" value="1500" />
             </bean>
          </list>
       </property>
       
       <property name="loadBalance">
          <bean class="com.le.jr.solr.client.loadstrategic.PollLoadBalance" />
       </property>
    </bean>
    
    <bean id="solrClient" class="com.le.jr.solr.client.SolrHttpClient">
       <property name="solrServerGroup" ref="solrServerGroup" />
    </bean>
    ```

    + 权重策略配置
    
    ```xml
    <!-- master，slave数据源组配置 -->
    <bean id="solrServerGroup" class="com.le.jr.solr.client.datasource.SolrServerGroup" destroy-method="destory">
       <property name="masterServer">
          <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
             <constructor-arg>
                <value>http://{ip}:{port}/solr/{collection1}</value>
             </constructor-arg>
             <property name="connectionTimeout" value="1000" />
             <property name="soTimeout" value="1500" />
          </bean>
       </property>
       
       <property name="slaveServerList">
          <list>
            <bean class="com.le.jr.solr.client.datasource.WeightSolrServer">
                <constructor-arg index="0">
                    <value>3</value>
                </constructor-arg>
                <constructor-arg index="1">
                    <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
                        <constructor-arg>
                            <value>http://{ip}:{port}/solr/{collection1}</value>
                        </constructor-arg>
                        <property name="connectionTimeout" value="1000" />
                        <property name="soTimeout" value="1500" />
                    </bean>
                </constructor-arg>
            </bean>
            <bean class="com.le.jr.solr.client.datasource.WeightSolrServer">
                <constructor-arg index="0">
                    <value>7</value>
                </constructor-arg>
                <constructor-arg index="1">
                    <bean class="org.apache.solr.client.solrj.impl.HttpSolrServer">
                        <constructor-arg>
                            <value>http://{ip}:{port}/solr/{collection1}</value>
                        </constructor-arg>
                        <property name="connectionTimeout" value="1000" />
                        <property name="soTimeout" value="1500" />
                    </bean>
                </constructor-arg>
            </bean>
          </list>
       </property>
       
       <property name="loadBalance">
          <bean class="com.le.jr.solr.client.loadstrategic.WeightLoadBalance" />
       </property>
    </bean>
    
    <bean id="solrClient" class="com.le.jr.solr.client.SolrHttpClient">
       <property name="solrServerGroup" ref="solrServerGroup" />
    </bean>
    ```

+ 自动提交设置

    默认情况下，客户端如果不配置，则视为手动提交索引，此为高并发写的情况下，让solr自动提交索引数据，客户端视情况选择。
    注意：如果设置autoCommit为true，则在solr里面的数据会有延迟，延迟的时间视solrconfig.xml里面的配置决定。

    ```xml
    <bean id="solrClient" class="com.le.jr.solr.client.SolrHttpClient">
        <property name="solrServerGroup" ref="solrServerGroup" />
        <property name="autoCommit" value="true" />
    </bean>
    ```
    
+ 已实现的组件:

	+ 单条添加: ``AddSingle()``
	+ 批量添加: ``AddMulti()``
	+ 删除: ``Delete()``
	+ 查询集合: ``Query()``
	+ 查询数量: ``Count()``
	+ 求和: ``Sum()``
	+ 分组: ``GroupBy()``
	+ 聚合: ``Aggregation()``
		
+ API文档[这里](API.md)。
    
+ 历史版本:

	+ 1.0.0:
		
		+ 基本功能实现。
	
	+ 1.0.1:
		
		+ 动态拼装查询、添加、删除条件。

	+ 1.0.2:

	    + 抽象工具类；
	    + 抽象builder类；
	    + 提升代码质量。

	+ 1.0.3:

	    + 增加GroupBy、Aggregation。

	+ 1.0.4:

	    + 修复Date类型查询Bug。
