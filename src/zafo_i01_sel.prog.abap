*&---------------------------------------------------------------------*
*& 包含               ZAFO_I01_SEL
*&---------------------------------------------------------------------*
data:p_typ like zafo_object-object,
     p_afono like zafo_head-afono,
     p_werks like t001w-werks.

ranges:s_lgort   for t001l-lgort          ,
       s_zzpino  for zsdsch-zzpino       ,
       s_zppdhd  FOR ztpp0089-zppdhd    ,
       s_kunnr   FOR ztpp0089-kunnr       ,
       s_zzwlly  FOR ztpp0091-zzwlly    ,
       s_bsart   FOR ekko-bsart         ,
       s_ekgrp   FOR ekko-ekgrp         ,
       s_ebeln   FOR ekko-ebeln         ,
       s_lifnr   FOR ekko-lifnr         ,
       s_potext  FOR zafo_head-remark1 ,
       s_zname1  FOR zscmt0010-zname1  ,
       s_matnr   FOR mara-matnr         ,
       s_maktx   FOR zmmt0010-maktx     ,
       s_mtart   FOR mara-mtart         ,
       s_cate1   FOR zmmt0010-zcate1    ,
       s_cate2   FOR zmmt0010-zcate2    ,
       s_idnlf   FOR zmmt0010-idnlf     ,
       s_qcno    FOR zafo_item-qcno      ,
       s_afono   FOR zafo_head-afono    ,
       s_status  FOR zafo_head-status  ,
       s_ernam   FOR zafo_head-ernam    ,
       s_erdat   FOR zafo_head-erdat    ,
       s_budat   FOR zafo_head-budat    .
