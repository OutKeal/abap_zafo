@AbapCatalog.sqlViewName: 'ZAFO_MO_GR_SUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '累计收货明细'
define view ZAFO_CDS_MO_GR_SUM as select from matdoc
{
    key werks,
    key aufnr,
    key mat_kdauf,
    key mat_kdpos,
    key matnr,
    cast( sum( stock_qty ) as menge_d ) as menge_gr
}
where ( bwart = '413' or bwart = '414' )
and xauto = 'X'
and sobkz = 'E'
group by     werks,
    aufnr,
    mat_kdauf,
    mat_kdpos,
    matnr
