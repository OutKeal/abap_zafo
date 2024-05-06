@AbapCatalog.sqlViewName: 'ZAFO_MO_HEAD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '生产订单抬头'
define view ZAFO_CDS_MO_HEAD as select from aufk as U
inner join afko as k on U.aufnr = k.aufnr
inner join afpo as P on U.aufnr = P.aufnr and P.posnr = '0001' 
inner join mara as M on P.matnr = M.matnr 
left outer join makt as T on M.satnr = T.matnr and T.spras = $session.system_language
left outer join ZAFO_CDS_MO_STATUS as s on U.aufnr = s.aufnr
--left outer join ztpp001 on k.aufnr = ztpp001.aufnr
left outer join vbak on P.kdauf = vbak.vbeln
{
key U.mandt,
key U.aufnr,
U.werks,
M.satnr,
vbak.ihrez,
T.maktx,
P.matnr,
P.psmng as MENGE,
P.meins,
P.dauat,
P.psobs,
P.kdauf,
P.kdpos,
M.mtart,
M.matkl,
k.gstrp,
k.gltrp,
k.rsnum,
case s.status
when 'I0001' then 'A'
when 'I0002' then 'C'
when 'I0045' then 'S'
when 'I0076' then 'D'
else '' end as status,
case s.status
when 'I0001' then '创建'
when 'I0002' then '下达'
when 'I0045' then '完工'
when 'I0076' then '删除'
else ''
end as status_text,
k.aufpl,
vbak.bstnk as BSTKD
--ztpp001.zfulop as remark1,
--ztpp001.zppid as remark2

}
