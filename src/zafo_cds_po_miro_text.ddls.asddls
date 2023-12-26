@AbapCatalog.sqlViewName: 'ZAFO_PO_MIROT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '待对账的采购清单,带文本'
define view ZAFO_CDS_PO_MIRO_TEXT 
as select from 
ZAFO_CDS_PO_MIRO as h
left outer join lfa1 as a on h.lifnr = a.lifnr and h.lifnr <> ''
left outer join t001 as b on h.bukrs = b.bukrs
left outer join t001w as c on h.werks = c.werks
left outer join t024e as e on h.ekorg = e.ekorg
left outer join t024 as f on h.ekgrp = f.ekgrp
left outer join t161t as g on h.bsart = g.bsart and g.bstyp = 'F' and g.spras = $session.system_language
{ 
key h.mandt, 
key h.lfgja, 
key h.lfbnr,
key h.lfpos,
h.ebeln,
h.ebelp,
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
h.waers,
h.frgrl,
h.loekz,
h.pstyp,
h.knttp,
h.werks,
@EndUserText.label: '工厂名称'
c.name1 as werks_name,

h.wepos,
h.repos,
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

 @Semantics.quantity.unitOfMeasure:'meins'
cast(h.quantity_gr as lqua_einme) as quantity_gr,
cast(h.net_gr as mm_a_gramount) as net_gr,
cast( division(h.quantity_gr * h.brtwr,h.menge,2) as amount_ca ) as amount_gr,

 @Semantics.quantity.unitOfMeasure:'meins'
cast( h.quantity_ir as remng ) as quantity_ir ,
cast( h.net_ir as mc_rewrt ) as net_ir,
cast( division(h.quantity_ir * h.brtwr,h.menge,2) as amount_ca ) as amount_ir,
 @Semantics.quantity.unitOfMeasure:'meins'
cast(h.quantity_gr - h.quantity_ir  as mmiv_open_invoice_quantity ) as quantity,
cast(h.net_gr - h.net_ir as open_inv_amount_ca) as net,
cast( division(h.quantity_gr * h.brtwr,h.menge,2)
- division(h.quantity_ir * h.brtwr,h.menge,2) as amount_ca ) as amount
}
