@AbapCatalog.sqlViewName: 'ZAFO_DONE_SUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '已经完成的业务汇总数'
define view ZAFO_CDS_DONE_SUM as select from zafo_head as H
inner join zafo_item as I on H.afono = I.afono
{
    key H.bustyp,
    key I.aufnr,
    key I.vornr,
    key I.rsnum,
    key I.rspos,
    sum( I.menge ) as MENGE
}
where H.status <> 'D'
and I.item_status <> 'D'
group by 
H.bustyp,
I.aufnr,
I.vornr,
I.rsnum,
I.rspos
