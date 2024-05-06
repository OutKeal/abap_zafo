@AbapCatalog.sqlViewName: 'ZAFO_WH_STOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '仓库库存'
define view ZAFO_CDS_WH_STOCK as 
select from nsdm_v_mska as a
inner join vbap as v on a.vbeln = v.vbeln and a.posnr = v.posnr
{
key a.matnr,
key a.werks,
key a.lgort,
key a.charg,
key a.sobkz,
key a.vbeln,
key a.posnr,
key v.pmatn as satnr,
a.kalab as LABST
}
where kalab <> 0
union all

select from nsdm_v_mard as a
inner join mara as m on a.matnr = m.matnr
{
key a.matnr,
key a.werks,
key a.lgort,
key '' as CHARG,
key ' ' as SOBKZ,
key '' as VBELN,
key '000000' as POSNR,
m.satnr,
a.labst
}
where labst <> 0
