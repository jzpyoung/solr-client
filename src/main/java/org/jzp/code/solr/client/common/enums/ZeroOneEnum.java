package org.jzp.code.solr.client.common.enums;

/**
 * 数字1，0枚举
 * 
 * @author jiazhipeng
 * @version 1.0
 * @date 2016-05-04
 */
public enum ZeroOneEnum {

	ZERO(0), ONE(1);

	private int value;

	ZeroOneEnum(int value) {
		this.value = value;
	}

	public int getValue() {
		return this.value;
	}
}
