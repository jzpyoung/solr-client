package org.jzp.solr.client.loadstrategic;

import java.util.List;
import java.util.Random;

import org.apache.solr.client.solrj.SolrServer;

/**
 * 选取slave策略类实现:随机选取策略
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public class RandomLoadBalance extends AbstractLoadBalance<SolrServer> {

	@Override
	public SolrServer doSelect(List<SolrServer> resources) {
		Random random = new Random();
		int index = random.nextInt(resources.size());
		return resources.get(index);
	}


}
