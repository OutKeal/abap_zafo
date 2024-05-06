@AbapCatalog.sqlViewName: 'ZAFO_MO_CONF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '报工视图'
define view ZAFO_CDS_MO_CONF as 
select from ZAFO_CDS_MO_HEAD as k
inner join afvv as v on k.aufpl = v.aufpl
inner join afvc as c on v.aufpl = c.aufpl and v.aplzl = c.aplzl
inner join s022 as s on k.aufnr = s.aufnr and c.vornr = s.vornr
--left outer join ztpp001b as b on k.remark2 = b.zppid and b.vornr = c.vornr and b.zoptp = '2'
left outer join crhd as H on c.arbid = H.objid and objty = 'A'
{
key k.aufnr,
key c.vornr,
c.werks,
case 
when k.satnr = ' ' then k.matnr else k.satnr 
end as Matnr,
v.meinh as MEINS,
k.satnr,
k.ihrez,
c.steus,
H.arbpl,
c.ltxa1,
c.ktsch,
v.mgvrg,
v.lmnga,
c.rueck,
c.rmzhl,
v.vgw01,
v.vgw02,
v.vgw03,
v.vgw04,
v.vgw05,
v.vgw06,
c.lifnr,
c.preis as PRICE,
c.peinh,
c.waers,
c.ekorg,
c.ekgrp,
c.matkl,
v.aufpl,
v.aplzl,
k.status
}
--where b.xloek <> 'X'
