@AbapCatalog.sqlViewName: 'ZAFO_MO_GR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '生产订单入库视图'
define view ZAFO_CDS_MO_GR as 
select from ZAFO_CDS_MO_HEAD as H
inner join afpo as P on H.aufnr = P.aufnr  
inner join vbak as K on H.kdauf = K.vbeln
inner join mara as M on P.matnr = M.matnr
-- left outer join zafo_mo_gr_sum as g 
--                on K.aufnr = g.aufnr 
--                and B.werks = g.werks
--                and B.vbeln = g.mat_kdauf
--                and B.posnr = g.mat_kdpos
--                and B.matnr = g.matnr
left outer join t001w as w on H.werks = w.werks
left outer join kna1 as n on K.kunnr = n.kunnr
left outer join makt as mt1 on M.satnr = mt1.matnr and mt1.spras = $session.system_language
left outer join makt as mt2 on P.matnr = mt2.matnr and mt2.spras = $session.system_language
{
key K.mandt,
key H.aufnr,
key P.posnr as POSNR_MO,
H.kdauf,
H.kdpos,
H.ihrez,
H.BSTKD,
K.kunnr,
n.name1 as kunnr_name,
H.werks,
w.name1 as werks_name,
M.satnr,
mt1.maktx as satnr_name,
mt2.maktx,
P.matnr as MATNR,
P.kdpos as KDPOS_MO,
P.meins,
P.psobs as SOBKZ,
P.psmng as menge_ref,
P.wemng as MENGE_DONE,
H.status as ITEM_STATUS,
status_text as TEXT
}
