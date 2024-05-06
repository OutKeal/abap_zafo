@AbapCatalog.sqlViewName: 'ZAFO_SO_DETAIL1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZAFO_CDS_SO_DETAIL'
define view ZAFO_CDS_SO_DETAIL1 as select from vbak as k
inner join vbap as p on k.vbeln = p.vbeln
inner join vbep as e on p.vbeln = e.vbeln and p.posnr = e.posnr
inner join mara as m on p.matnr = m.matnr
left outer join wrf_charvalt as c on m.color_atinn = c.atinn
                                  and m.color = c.atwrt
                                  and c.spras = $session.system_language
left outer join wrf_charvalt as s on m.size1_atinn = s.atinn
                                  and m.size1 = s.atwrt
                                  and s.spras = $session.system_language

{
       key k.mandt,
       key k.vbeln,
       key p.posnr,
       key e.etenr,
        p.matnr,
        m.satnr,
        e.edatu,
        m.color,
        c.atwtb as color_name,
        m.size1,
        s.atwtb as size1_name,
        e.wmeng as menge,
        p.meins
}

