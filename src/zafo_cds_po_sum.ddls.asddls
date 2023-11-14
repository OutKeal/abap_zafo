@AbapCatalog.sqlViewName: 'ZAFO_PO_SUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '已经转订单的需求列表'
define view ZAFO_CDS_PO_SUM as select from zafo_head as h
inner join zafo_item as i on h.afono = i.afono
{   
    h.bustyp,
    i.rsnum,
    i.rspos,
    sum( i.menge ) as MENGE_DONE
}
where h.status <> 'D' and i.item_status <> 'D'
group by     
h.bustyp,
i.rsnum,
i.rspos
