# solr-client
+ 添加 solr-client 依赖：

    ```xml
    <dependency>
        <groupId>com.le.jr</groupId>
        <artifactId>solr-client</artifactId>
        <version>1.0.4</version>
    </dependency>
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
 
+ SPRING接入文档[这里](SPRING.md)。
    
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
