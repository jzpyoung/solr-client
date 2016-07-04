package com.le.jr.solr.client;

import com.le.jr.solr.client.annotation.PageField;
import com.le.jr.solr.client.annotation.ScopeField;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

public class ReconciliationQueryInput implements Serializable {
	/******** 简单查询条件  ****************/
	//订单号
	private String orderId;
	//手机号
	private String telephone;
	//业务账单号
	private String brOrderId;
	//清算账单号
	private String lrOrderId;
	//用户姓名
	private String accountName;
	//产品名称
	private String projectName;
	//交易开始时间
    @ScopeField(name = "tradeDate", mode = ScopeField.ScopeFiledEnum.GT)
	private Date tradeDateBegin;
	//交易结束时间
    @ScopeField(name = "tradeDate", mode = ScopeField.ScopeFiledEnum.LT)
	private Date tradeDateEnd;
    //交易开始时间
    @ScopeField(name = "businessDate", mode = ScopeField.ScopeFiledEnum.GT)
    private Date businessDateBegin;
    //交易结束时间
    @ScopeField(name = "businessDate", mode = ScopeField.ScopeFiledEnum.LT)
    private Date businessDateEnd;
    //交易开始时间
    @ScopeField(name = "liquidationDate", mode = ScopeField.ScopeFiledEnum.GT)
    private Date liquidationDateBegin;
    //交易结束时间
    @ScopeField(name = "liquidationDate", mode = ScopeField.ScopeFiledEnum.LT)
    private Date liquidationDateEnd;
	//交易金额开始值
    @ScopeField(name = "drAmount", mode = ScopeField.ScopeFiledEnum.GT)
	private BigDecimal tradeAmountBegin;
	//交易金额结束值
    @ScopeField(name = "drAmount", mode = ScopeField.ScopeFiledEnum.LT)
	private BigDecimal tradeAmountEnd;
    //交易金额开始值
    @ScopeField(name = "brAmount", mode = ScopeField.ScopeFiledEnum.GT)
    private BigDecimal businessAmountBegin;
    //交易金额结束值
    @ScopeField(name = "brAmount", mode = ScopeField.ScopeFiledEnum.LT)
    private BigDecimal businessAmountEnd;
    //交易金额开始值
    @ScopeField(name = "lrAmount", mode = ScopeField.ScopeFiledEnum.GT)
    private BigDecimal liquidationAmountBegin;
    //交易金额结束值
    @ScopeField(name = "lrAmount", mode = ScopeField.ScopeFiledEnum.LT)
    private BigDecimal liquidationAmountEnd;
    //身份证
    private String idCardNo;

	/******** 高级查询条件  ****************/
	//交易最初状态
	private Integer tradeInitialState;
	//交易最终状态
	private Integer tradeFinalState;
	//业务对账状态
    private Integer brTransState;
    //清算对账状态
    private Integer lrTransState;
    
    //交易/业务金额
    //private Integer dbBal;
    //交易/清算金额
    //private Integer dlBal;
    //业务/交易金额
    //private Integer bdBal;
    //业务/清算金额
    //private Integer blBal;
    //清算/交易金额
    //private Integer ldBal;
    //清算/业务金额
    //private Integer lbBal;
    
    //交易/业务对账状态
    private Integer dbCheckState;
    //交易/清算对账状态
    private Integer dlCheckState;
    //业务/清算对账状态
    private Integer blCheckState;
    //业务/交易对账状态
    private Integer bdCheckState;
    //清算/交易对账状态
    private Integer ldCheckState;
    //清算/业务对账状态
    private Integer lbCheckState;

    /**是否已发送会计引擎*/
    private Byte isSend;

    /**
     * 退款原因
     * @see com.le.jr.trade.reconciliation.enums.RefundReasonEnum
     */
    private Byte refundReason;

    /**
     * 订单申购状态或者赎回单赎回状态
     * @see com.le.jr.trade.reconciliation.enums.PurchaseStatusEnum
     * @see com.le.jr.trade.reconciliation.enums.RedeemStatusEnum
     */
    private Byte orderStatus;

    /*******     下面为特殊条件          ***************/
    /**
     * @see com.le.jr.trade.reconciliation.enums.Flag
     */
	private Byte flag;
    private Byte tradeOperate; // 0：不用审核 1：需要审核
    /*******     特殊条件          ***************/

    /**  分页信息  **/
    @PageField(name = PageField.PageFiledEnum.PAGESIZE)
    private Integer pageSizeSelf;
    @PageField(name = PageField.PageFiledEnum.START)
    private Integer startRow;

    public String getOrderId() {
        return orderId;
    }

    public void setOrderId(String orderId) {
        this.orderId = orderId;
    }

    public String getTelephone() {
        return telephone;
    }

    public void setTelephone(String telephone) {
        this.telephone = telephone;
    }

    public String getBrOrderId() {
        return brOrderId;
    }

    public void setBrOrderId(String brOrderId) {
        this.brOrderId = brOrderId;
    }

    public String getLrOrderId() {
        return lrOrderId;
    }

    public void setLrOrderId(String lrOrderId) {
        this.lrOrderId = lrOrderId;
    }

    public String getAccountName() {
        return accountName;
    }

    public void setAccountName(String accountName) {
        this.accountName = accountName;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public Date getTradeDateBegin() {
        return tradeDateBegin;
    }

    public void setTradeDateBegin(Date tradeDateBegin) {
        this.tradeDateBegin = tradeDateBegin;
    }

    public Date getTradeDateEnd() {
        return tradeDateEnd;
    }

    public void setTradeDateEnd(Date tradeDateEnd) {
        this.tradeDateEnd = tradeDateEnd;
    }

    public Date getBusinessDateBegin() {
        return businessDateBegin;
    }

    public void setBusinessDateBegin(Date businessDateBegin) {
        this.businessDateBegin = businessDateBegin;
    }

    public Date getBusinessDateEnd() {
        return businessDateEnd;
    }

    public void setBusinessDateEnd(Date businessDateEnd) {
        this.businessDateEnd = businessDateEnd;
    }

    public Date getLiquidationDateBegin() {
        return liquidationDateBegin;
    }

    public void setLiquidationDateBegin(Date liquidationDateBegin) {
        this.liquidationDateBegin = liquidationDateBegin;
    }

    public Date getLiquidationDateEnd() {
        return liquidationDateEnd;
    }

    public void setLiquidationDateEnd(Date liquidationDateEnd) {
        this.liquidationDateEnd = liquidationDateEnd;
    }

    public BigDecimal getTradeAmountBegin() {
        return tradeAmountBegin;
    }

    public void setTradeAmountBegin(BigDecimal tradeAmountBegin) {
        this.tradeAmountBegin = tradeAmountBegin;
    }

    public BigDecimal getTradeAmountEnd() {
        return tradeAmountEnd;
    }

    public void setTradeAmountEnd(BigDecimal tradeAmountEnd) {
        this.tradeAmountEnd = tradeAmountEnd;
    }

    public BigDecimal getBusinessAmountBegin() {
        return businessAmountBegin;
    }

    public void setBusinessAmountBegin(BigDecimal businessAmountBegin) {
        this.businessAmountBegin = businessAmountBegin;
    }

    public BigDecimal getBusinessAmountEnd() {
        return businessAmountEnd;
    }

    public void setBusinessAmountEnd(BigDecimal businessAmountEnd) {
        this.businessAmountEnd = businessAmountEnd;
    }

    public BigDecimal getLiquidationAmountBegin() {
        return liquidationAmountBegin;
    }

    public void setLiquidationAmountBegin(BigDecimal liquidationAmountBegin) {
        this.liquidationAmountBegin = liquidationAmountBegin;
    }

    public BigDecimal getLiquidationAmountEnd() {
        return liquidationAmountEnd;
    }

    public void setLiquidationAmountEnd(BigDecimal liquidationAmountEnd) {
        this.liquidationAmountEnd = liquidationAmountEnd;
    }

    public String getIdCardNo() {
        return idCardNo;
    }

    public void setIdCardNo(String idCardNo) {
        this.idCardNo = idCardNo;
    }

    public Integer getTradeInitialState() {
        return tradeInitialState;
    }

    public void setTradeInitialState(Integer tradeInitialState) {
        this.tradeInitialState = tradeInitialState;
    }

    public Integer getTradeFinalState() {
        return tradeFinalState;
    }

    public void setTradeFinalState(Integer tradeFinalState) {
        this.tradeFinalState = tradeFinalState;
    }

    public Integer getBrTransState() {
        return brTransState;
    }

    public void setBrTransState(Integer brTransState) {
        this.brTransState = brTransState;
    }

    public Integer getLrTransState() {
        return lrTransState;
    }

    public void setLrTransState(Integer lrTransState) {
        this.lrTransState = lrTransState;
    }

    public Integer getDbCheckState() {
        return dbCheckState;
    }

    public void setDbCheckState(Integer dbCheckState) {
        this.dbCheckState = dbCheckState;
    }

    public Integer getDlCheckState() {
        return dlCheckState;
    }

    public void setDlCheckState(Integer dlCheckState) {
        this.dlCheckState = dlCheckState;
    }

    public Integer getBlCheckState() {
        return blCheckState;
    }

    public void setBlCheckState(Integer blCheckState) {
        this.blCheckState = blCheckState;
    }

    public Integer getBdCheckState() {
        return bdCheckState;
    }

    public void setBdCheckState(Integer bdCheckState) {
        this.bdCheckState = bdCheckState;
    }

    public Integer getLdCheckState() {
        return ldCheckState;
    }

    public void setLdCheckState(Integer ldCheckState) {
        this.ldCheckState = ldCheckState;
    }

    public Integer getLbCheckState() {
        return lbCheckState;
    }

    public void setLbCheckState(Integer lbCheckState) {
        this.lbCheckState = lbCheckState;
    }

    public Byte getFlag() {
        return flag;
    }

    public void setFlag(Byte flag) {
        this.flag = flag;
    }

    public Byte getTradeOperate() {
        return tradeOperate;
    }

    public void setTradeOperate(Byte tradeOperate) {
        this.tradeOperate = tradeOperate;
    }

    public Integer getPageSizeSelf() {
        return pageSizeSelf;
    }

    public void setPageSizeSelf(Integer pageSizeSelf) {
        this.pageSizeSelf = pageSizeSelf;
    }

    public Integer getStartRow() {
        return startRow;
    }

    public void setStartRow(Integer startRow) {
        this.startRow = startRow;
    }

    public Byte getIsSend() {
        return isSend;
    }

    public void setIsSend(Byte isSend) {
        this.isSend = isSend;
    }

    public Byte getRefundReason() {
        return refundReason;
    }

    public void setRefundReason(Byte refundReason) {
        this.refundReason = refundReason;
    }

    public Byte getOrderStatus() {
        return orderStatus;
    }

    public void setOrderStatus(Byte orderStatus) {
        this.orderStatus = orderStatus;
    }
}
