@AbapCatalog.sqlViewName: 'ZAFO_MO_STATUS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '生产订单状态'
define view ZAFO_CDS_MO_STATUS as select from aufk as k
inner join jest as j on k.objnr = j.objnr and j.inact = ''
{
key k.mandt,
key k.aufnr,
max(stat) as status
}
where stat = 'I0001'
or stat = 'I0002'
or stat = 'I0045'
or stat = 'I0076'
group by k.aufnr
