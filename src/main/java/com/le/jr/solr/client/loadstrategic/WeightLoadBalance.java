package com.le.jr.solr.client.loadstrategic;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import com.le.jr.solr.client.datasource.WeightSolrServer;

/**
 * 选取slave策略类实现:权重策略
 * 
 * @author jiazhipeng 
 * date 2016-3-30
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
