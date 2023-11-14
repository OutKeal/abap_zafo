@AbapCatalog.sqlViewName: 'ZAFO_MO_GR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '生产订单入库视图'
define view ZAFO_CDS_MO_GR as 
select from afko as K
inner join afpo as P on K.aufnr = P.aufnr 
inner join vbak as A on P.kdauf = A.vbeln
inner join vbap as B on P.kdauf = B.vbeln and P.kdpos = B.posnr  
inner join mara as M on B.matnr = M.matnr
-- left outer join zafo_mo_gr_sum as g 
--                on K.aufnr = g.aufnr 
--                and B.werks = g.werks
--                and B.vbeln = g.mat_kdauf
--                and B.posnr = g.mat_kdpos
--                and B.matnr = g.matnr
left outer join t001w as w on B.werks = w.werks
left outer join kna1 as n on A.kunnr = n.kunnr
left outer join makt as mt1 on M.satnr = mt1.matnr and mt1.spras = $session.system_language
left outer join makt as mt2 on M.matnr = mt2.matnr and mt2.spras = $session.system_language
{
key K.mandt,
key B.vbeln as KDAUF,
key B.posnr as KDPOS,
A.kunnr,
n.name1 as kunnr_name,
K.aufnr,
P.posnr as POSNR_MO,
B.werks,
w.name1 as werks_name,
M.satnr,
mt1.maktx as satnr_name,
B.matnr,
mt2.maktx,
P.matnr as MATNR_MO,
P.kdpos as KDPOS_MO,
P.meins,
P.psobs as SOBKZ,
B.kwmeng as menge_ref,
P.wemng as MENGE_DONE
-- g.menge_gr as menge_done
}
where P.posnr <> '0001'
