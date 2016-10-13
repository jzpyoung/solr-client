# Spring接入文档
+ 依赖注入：

    ```Java
    @Resource
    private SolrClient solrClient;
    ```
    
+ spring配置文件配置：

    + 随机策略配置

        ```xml
        <!-- master，slave数据源组配置 -->
        <bean id="solrServerGroup" class="org.jzp.code.solr.client.datasource.SolrServerGroup" destroy-method="destory">
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
              <bean class="org.jzp.code.solr.client.loadstrategic.RandomLoadBalance" />
           </property>
        </bean>
        
        <bean id="solrClient" class="org.jzp.code.solr.client.SolrHttpClient">
           <property name="solrServerGroup" ref="solrServerGroup" />
        </bean>
        ```

    + 轮询策略配置

        ```xml
        <!-- master，slave数据源组配置 -->
        <bean id="solrServerGroup" class="org.jzp.code.solr.client.datasource.SolrServerGroup" destroy-method="destory">
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
              <bean class="org.jzp.code.solr.client.loadstrategic.PollLoadBalance" />
           </property>
        </bean>
        
        <bean id="solrClient" class="org.jzp.code.solr.client.SolrHttpClient">
           <property name="solrServerGroup" ref="solrServerGroup" />
        </bean>
        ```

    + 权重策略配置
    
        ```xml
        <!-- master，slave数据源组配置 -->
        <bean id="solrServerGroup" class="org.jzp.code.solr.client.datasource.SolrServerGroup" destroy-method="destory">
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
                <bean class="org.jzp.code.solr.client.datasource.WeightSolrServer">
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
                <bean class="org.jzp.code.solr.client.datasource.WeightSolrServer">
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
              <bean class="org.jzp.code.solr.client.loadstrategic.WeightLoadBalance" />
           </property>
        </bean>
        
        <bean id="solrClient" class="org.jzp.code.solr.client.SolrHttpClient">
           <property name="solrServerGroup" ref="solrServerGroup" />
        </bean>
        ```

+ 自动提交设置：

    默认情况下，客户端如果不配置，则视为手动提交索引，此为高并发写的情况下，让solr自动提交索引数据，客户端视情况选择。
    注意：如果设置autoCommit为true，则在solr里面的数据会有延迟，延迟的时间视solrconfig.xml里面的配置决定。

    ```xml
    <bean id="solrClient" class="org.jzp.code.solr.client.SolrHttpClient">
        <property name="solrServerGroup" ref="solrServerGroup" />
        <property name="autoCommit" value="true" />
    </bean>
    ```
