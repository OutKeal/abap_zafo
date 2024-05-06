@AbapCatalog.sqlViewName: 'ZAFO_PO_HSITORYD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '采购订单历史明细'
define view ZAFO_CDS_PO_HSITORY_D as select from ekbe
inner join ekpo on ekbe.ebeln = ekpo.ebeln and ekbe.ebelp = ekpo.ebelp
{
ekbe.ebeln,
ekbe.ebelp,
ekbe.vgabe as TYPE,
case ekbe.vgabe 
when '1' then '入库'
when '2' then '发票'
when '3' then '后续发票调整'
when '4' then '预付款'
when '6' then '交货单发货'
when '7' then '材料反冲'
when 'C' then '清账'
end as type_name,
ekbe.matnr,
ekbe.gjahr,
ekbe.belnr,
ekbe.buzei,
ekbe.bwart,
ekbe.budat,
case ekbe.shkzg
when 'H' then - ekbe.menge
else ekbe.menge end as MENGE,
case ekbe.shkzg
when 'H' then - ekbe.dmbtr
else ekbe.dmbtr end as DMBTR,
ekbe.xblnr,
ekbe.lfgja,
ekbe.lfbnr,
ekbe.lfpos
}
where vgabe = '1'
or vgabe = '2'
or vgabe = '3'
or vgabe = '4'
or vgabe = '6'
or vgabe = '7'
or vgabe = 'C'
