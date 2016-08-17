package org.jzp.solr.client.loadstrategic;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.jzp.solr.client.datasource.WeightSolrServer;

/**
 * 选取slave策略类实现:权重策略
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-30
 */
public class WeightLoadBalance extends AbstractLoadBalance<WeightSolrServer> {

	@Override
	public WeightSolrServer doSelect(List<WeightSolrServer> resources) {
		List<WeightSolrServer> server4select = new ArrayList<WeightSolrServer>();

		for (WeightSolrServer server : resources) {
			int weight = server.getWeight();
			for (int i = 0; i < weight; i++) {
				server4select.add(server);
			}
		}

		int total = server4select.size();
		Random random = new Random();
		return server4select.get(random.nextInt(total));
	}

}
