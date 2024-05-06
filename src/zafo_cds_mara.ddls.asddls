@AbapCatalog.sqlViewName: 'ZAFO_MARA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '工厂主数据'
define view ZAFO_CDS_MARA as 
select from mara
left outer join makt on mara.matnr = makt.matnr and makt.spras =  $session.system_language
--left outer join zmat_mara on mara.matnr = zmat_mara.matnr or mara.satnr = zmat_mara.matnr
{
    key mara.mandt,
    key mara.matnr,
    makt.maktx,
    mara.mtart,
    mara.meins,
    mara.satnr,
    mara.matkl,
    mara.spart as SPART_MAT
-- zmat_mara.color_text,
--  zmat_mara.main_ingr,
--    zmat_mara.width,
--    zmat_mara.weight,
--    zmat_mara.lifnr,
--    zmat_mara.price_long,
--    zmat_mara.kdmat
}


