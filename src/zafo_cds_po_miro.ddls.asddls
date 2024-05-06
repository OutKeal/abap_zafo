@AbapCatalog.sqlViewName: 'ZAFO_PO_MIRO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '待对账的采购清单'
define view ZAFO_CDS_PO_MIRO 
as select from 
ekko 
inner join ekpo on ekko.ebeln = ekpo.ebeln 
inner join ekbe on ekbe.ebeln = ekko.ebeln and ekbe.ebelp = ekpo.ebelp
and ( ekbe.vgabe = '1' or ekbe.vgabe = '2' ) and ekbe.lfbnr is not initial
left outer join ekkn on ekpo.ebeln = ekkn.ebeln and ekpo.ebelp = ekkn.ebelp and ekkn.zekkn = '01' 
 
{ 
key ekko.mandt, 
key ekbe.lfgja, 
key ekbe.lfbnr,
key ekbe.lfpos,
ekbe.ebeln,
ekbe.ebelp,
ekko.ekorg,
ekko.ekgrp,
ekko.bukrs,
ekko.ernam,
ekko.aedat,
ekko.lifnr,
ekko.bsart,
ekko.bedat,
ekko.frgrl,
ekpo.loekz,
ekpo.pstyp,
ekpo.knttp,
ekpo.werks,
@EndUserText.label: '需收货'
ekpo.wepos,
@EndUserText.label: '需发票'
ekpo.repos,
ekpo.matnr,
@EndUserText.label: '物料名称'
ekpo.txz01,
ekpo.retpo,
ekpo.bednr,
ekpo.externalreferenceid as IHREZ,
ekkn.aufnr,

@EndUserText.label: '采购总数量'
case ekpo.retpo
when 'X' then - ekpo.menge
else ekpo.menge 
end as menge ,
ekpo.meins ,
@EndUserText.label: '含税单价'


cast(
case when ekpo.menge <> 0 
then division( ekpo.kzwi1,ekpo.menge,2 ) 
else 0 end 
as farr_unit_price) as price,
ekko.waers, 
@EndUserText.label: '采购总金额'
cast(
case ekpo.retpo
when 'X' then - ekpo.kzwi1 
else ekpo.kzwi1  
end 
as bbwert ) as brtwr ,
@EndUserText.label: '采购总金额'
ekpo.netwr ,
ekpo.mwskz,
//@EndUserText.label: '税额'
//
//cast(
//case ekpo.retpo
//when 'X' then - ( ekpo.kzwi1 - ekpo.netwr )
//else ekpo.kzwi1 - ekpo.netwr 
//end 
//as taxamount )as MWSKZ_AMOUNT ,


@EndUserText.label: '入库数量'
coalesce(
cast
(
sum( case
when ekbe.vgabe = '1' and ekbe.shkzg = 'H' then - ekbe.menge
when ekbe.vgabe = '1' and ekbe.shkzg = 'S' then ekbe.menge
end     ) as lqua_einme),0) as quantity_gr, 

@EndUserText.label: '入库净额'
coalesce(
cast
(
sum( case
when ekbe.vgabe = '1' and ekbe.shkzg = 'H' then - ekbe.dmbtr
when ekbe.vgabe = '1' and ekbe.shkzg = 'S' then ekbe.dmbtr
end ) as mm_a_gramount ),0) as net_gr,

@EndUserText.label: '开票数量'

coalesce(
cast(sum( case
when ekbe.vgabe = '2' and ekbe.shkzg = 'S' then ekbe.menge
when ekbe.vgabe = '2' and ekbe.shkzg = 'H' then - ekbe.menge
end  ) as remng),0) as quantity_ir,

@EndUserText.label: '开票净额'
coalesce(
cast( sum( case
when ekbe.vgabe = '2' and ekbe.shkzg = 'S' then ekbe.arewr
when ekbe.vgabe = '2' and ekbe.shkzg = 'H' then -ekbe.arewr
end )as mc_rewrt ),0) as net_ir
}
where ekpo.menge <> 0 --暂存凭证中可能为0
and ekpo.repos = 'X'
and ekpo.wepos = 'X' 
group by
ekko.mandt,
ekbe.lfgja, 
ekbe.lfbnr,
ekbe.lfpos,
ekbe.ebeln,
ekbe.ebelp,
ekko.ekorg,
ekko.ernam,
ekko.aedat,
ekko.ekgrp,
ekko.bukrs,
ekko.lifnr,
ekko.reswk,
ekko.bsart,
ekko.bedat,
ekko.waers,
ekko.frgrl,
ekpo.ebelp,
ekpo.loekz,
ekpo.pstyp,
ekpo.knttp,
ekpo.werks,

ekpo.wepos,
ekpo.repos,
ekpo.mwskz,

ekpo.matnr,
ekpo.txz01,
ekpo.retpo,
ekpo.bednr,
ekpo.externalreferenceid,
ekpo.menge,
ekpo.meins,
ekpo.kzwi1,
ekpo.netwr,
ekkn.aufnr
