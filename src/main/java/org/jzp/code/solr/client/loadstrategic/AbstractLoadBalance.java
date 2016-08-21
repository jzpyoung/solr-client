package org.jzp.code.solr.client.loadstrategic;

import java.util.List;

/**
 * 负载均衡策略类抽象类(封装了slaveList.size为0，1时的特殊处理情况)
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public abstract class AbstractLoadBalance<S> implements LoadBalance<S> {

	@Override
	public S select(List<S> resources){
		//把resources为空和resources.size()==1的情况写入公共方法里面
		if(resources == null || resources.size() == 0){
			throw new RuntimeException("resources为空异常！");
		}else if(resources.size() == 1){
			return resources.get(0);
		}
		return doSelect(resources);
	}

	/**
	 * resources.size()>1时，根据配置策略选择一个资源
	 * @param resources资源列表
	 * @return resource资源
	 */
	public abstract S doSelect(List<S> resources);

}
