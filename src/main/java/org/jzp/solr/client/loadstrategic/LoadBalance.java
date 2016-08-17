package org.jzp.solr.client.loadstrategic;

import java.util.List;

/**
 * 负载均衡策略类接口
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public interface LoadBalance<S> {
	
	/**
	 * 从资源列表里面根据配置策略选取一个资源
	 * @param resources资源列表
	 * @return resource资源
	 */
	S select(List<S> resources);
	
}
