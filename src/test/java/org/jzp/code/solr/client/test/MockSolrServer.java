package org.jzp.code.solr.client.test;

import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.SolrServer;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.common.util.NamedList;

import java.io.IOException;

/**
 * MockSolrServer
 *
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-03-31
 */
public class MockSolrServer extends SolrServer {

    private static final long serialVersionUID = 1L;

    @Override
    public NamedList<Object> request(SolrRequest arg0) throws SolrServerException, IOException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void shutdown() {
        // TODO Auto-generated method stub
    }
}
