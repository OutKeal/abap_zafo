@AbapCatalog.sqlViewName: 'ZAFO_PO_HISTORYT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '采购订单历史带描述'
define view ZAFO_CDS_PO_HISTORY_WITH_TEXT 
as select from 
ZAFO_CDS_PO_HISTORY as h
left outer join lfa1 as a on h.lifnr = a.lifnr and h.lifnr <> ''
left outer join t001 as b on h.bukrs = b.bukrs
left outer join t001w as c on h.werks = c.werks
left outer join t001l as d on h.werks = d.werks and h.lgort = d.lgort and h.lgort <> ''
left outer join t024e as e on h.ekorg = e.ekorg
left outer join t024 as f on h.ekgrp = f.ekgrp
left outer join t161t as g on h.bsart = g.bsart and g.bstyp = 'F' and g.spras = $session.system_language
{ 
key h.mandt,
key h.ebeln,
key h.ebelp,
h.ekorg,
@EndUserText.label: '采购组织名称'
e.ekotx as ekorg_name, 
h.ekgrp,
@EndUserText.label: '采购织名称'
f.eknam as ekgrp_name, 
h.ernam, 
h.aedat, 
h.bukrs, 
@EndUserText.label: '公司名称'
b.butxt as bukrs_name,
h.lifnr,
@EndUserText.label: '供应商名称'
a.name1 as lifnr_name,
h.bsart,
@EndUserText.label: '订单类型名称'
g.batxt as bsart_name,
h.bedat,
h.loekz,
h.pstyp,
h.knttp,
h.werks,
@EndUserText.label: '工厂名称'
c.name1 as werks_name,
h.lgort,
@EndUserText.label: '存储地点名称'
d.lgobe as LGORT_NAME,
h.wepos,
h.repos,
h.elikz,
h.bednr,
h.IHREZ,
h.matnr,
h.txz01,
h.retpo,
 @Semantics.quantity.unitOfMeasure:'meins'
h.menge,
h.meins,
h.price,
h.brtwr,
h.netwr,
h.mwskz,
h.MWSKZ_AMOUNT,

 @Semantics.quantity.unitOfMeasure:'meins'
cast(h.quantity_gr as lqua_einme) as quantity_gr,
 @Semantics.quantity.unitOfMeasure:'meins'
cast( case when h.menge > h.quantity_gr then h.menge - h.quantity_gr else 0 end as obmng) as OBMNG,
cast( h.amount_gr as mm_a_gramount) as amount_gr,
 @Semantics.quantity.unitOfMeasure:'meins'
cast( h.quantity_ir as remng ) as quantity_ir ,
cast( h.amount_ir as mc_rewrt ) as amount_ir,
 @Semantics.quantity.unitOfMeasure:'meins'
cast(h.quantity_gr - h.quantity_ir  as mmiv_open_invoice_quantity ) as quantity,
cast(h.amount_gr - h.amount_ir as open_inv_amount_ca) as amount
}
