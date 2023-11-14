@AbapCatalog.sqlViewName: 'ZAFO_WH_STOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '仓库库存'
define view ZAFO_CDS_WH_STOCK as 
select from nsdm_v_mska as a
{
key a.matnr,
key a.werks,
key a.lgort,
key a.charg,
key a.sobkz,
key a.vbeln,
key a.posnr,
a.kalab as LABST
}
where kalab <> 0
union all

select from nsdm_v_mard as a
{
key a.matnr,
key a.werks,
key a.lgort,
key '' as CHARG,
key ' ' as SOBKZ,
key '' as VBELN,
key '000000' as POSNR,
a.labst
}
where labst <> 0
