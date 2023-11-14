@AbapCatalog.sqlViewName: 'ZAFO_MO_CONF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '报工视图'
define view ZAFO_CDS_MO_CONF as 
select from afko as K
inner join afpo as P on K.aufnr = P.aufnr and P.posnr = '0001'
inner join mara as m on P.matnr = m.matnr
inner join afvv as v on K.aufpl = v.aufpl
inner join afvc as c on v.aufpl = c.aufpl and v.aplzl = c.aplzl
left outer join crhd as H on c.arbid = H.objid and objty = 'A'
{
key K.aufnr,
key c.vornr,
c.werks,
case 
when m.satnr = ' ' then P.matnr else m.satnr 
end as Matnr,
v.meinh as MEINS,
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
c.matkl
}
