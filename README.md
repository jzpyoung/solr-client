# solr-client
+ 添加 solr-client 依赖：

    ```xml
    <dependency>
        <groupId>org.jzp.code</groupId>
        <artifactId>solr-client</artifactId>
        <version>1.0.4</version>
    </dependency>
    ```
    
+ 已实现的组件:

    + 求和: ``Sum()``
    + 删除: ``Delete()``
    + 分组: ``GroupBy()``
    + 聚合: ``Aggregation()``
    + 查询集合: ``Query()``
    + 查询数量: ``Count()``
	+ 单条添加: ``AddSingle()``
	+ 批量添加: ``AddMulti()``

+ API文档[这里](API.md)。
 
+ SPRING接入文档[这里](SPRING.md)。

+ 其他相关文档

	+ [Solr](http://lucene.apache.org/solr/4_2_1/)。
    
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

	    + 增加GroupBy、Aggregation支持。

	+ 1.0.4:

	    + 修复Date类型查询Bug。
	    
	+ 1.0.5:
    
        + 增加in、notin、sort、scope、模糊查询支持。
