*&---------------------------------------------------------------------*
*& 包含               ZAFO_ZZ_DUC
*&---------------------------------------------------------------------*

***********  装箱单

FORM frm_zz_pack_post.

  DATA:lv_zzpacknm TYPE zzpacknm.
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  CHECK g_error NE 'X'.
  CHECK gs_head-bustyp = 'ASN02' OR gs_head-bustyp = 'R1019' OR gs_head-bustyp = 'R1020' .

  CHECK gt_item[] IS NOT INITIAL.
  SELECT vbeln,vkorg
    INTO TABLE @DATA(lt_ztpp0089)
    FROM ztpp0089
    FOR ALL ENTRIES IN @gt_item
    WHERE vbeln = @gt_item-vbeln_va
      AND posnr = @gt_item-posnr_va.

  REFRESH lt_item.
  LOOP AT lt_ztpp0089 INTO DATA(ls_ztpp0089) WHERE vkorg = '2000'.
    LOOP AT gt_item WHERE vbeln_va = ls_ztpp0089-vbeln.
      APPEND gt_item TO lt_item.
    ENDLOOP.
  ENDLOOP.

  CLEAR lv_zzpacknm.
  IF lt_item[] IS NOT INITIAL.
    PERFORM frm_zz_pack TABLES lt_item
                         USING '2000'
                         CHANGING lv_zzpacknm.
  ENDIF.

  CHECK g_error <> 'X'.

  REFRESH lt_item.
  LOOP AT lt_ztpp0089 INTO ls_ztpp0089 WHERE vkorg = '2001'.
    LOOP AT gt_item WHERE vbeln_va = ls_ztpp0089-vbeln.
      APPEND gt_item TO lt_item.
    ENDLOOP.
  ENDLOOP.

  IF lv_zzpacknm IS NOT INITIAL.
    gs_head-remark2 = lv_zzpacknm.
  ENDIF.

  CLEAR lv_zzpacknm.
  IF lt_item[] IS NOT INITIAL.
    PERFORM frm_zz_pack TABLES lt_item
                         USING '2001'
                         CHANGING lv_zzpacknm.
  ENDIF.

  CHECK g_error <> 'X'.

  IF lv_zzpacknm IS NOT INITIAL.
    IF gs_head-remark2 IS INITIAL.
      gs_head-remark2 = lv_zzpacknm.
    ELSE.
      gs_head-remark4 = lv_zzpacknm.
    ENDIF.
  ENDIF.


  UPDATE zafo_head SET remark2 = gs_head-remark2
                       remark4 = gs_head-remark4
      WHERE afono = gs_head-afono.

ENDFORM.


FORM frm_zz_pack TABLES pt_item STRUCTURE zafo_sitem
                                    USING pv_vkorg
                                 CHANGING pv_zzpacknm.
  DATA:ls_headdata TYPE zsdpack_hed.
  DATA:lt_itemdata TYPE TABLE OF zssd277 WITH HEADER LINE.
  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.


  CLEAR ls_headdata.
  REFRESH lt_itemdata.

  ls_headdata-zzck_typ = 'X'."出口状态
  ls_headdata-zzcttzlx = 'W2'."出货业务类型
  ls_headdata-zzsource = 'C'."来源
  ls_headdata-zcct_type = '7001'."交易类型
  ls_headdata-zzmix = 'X'."混装标识
  ls_headdata-vkorg = pv_vkorg.

  READ TABLE gt_barcode INDEX 1.
  IF sy-subrc EQ 0.
    IF gt_barcode-zshd = 'ET埃塞'.
      ls_headdata-werks = '5000'.
    ELSEIF gt_barcode-zshd = 'VN越南'.
      ls_headdata-werks = '3000'.
    ELSE.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '送货地不存在' '' '' ''.
    ENDIF.
  ENDIF.

  ls_headdata-dwerk = '1000'.
  ls_headdata-lgort = '9001'.
  ls_headdata-kunnr = '000000' && ls_headdata-werks.
  ls_headdata-vsart = '04'.
  ls_headdata-zzdz = 'X'.
  ls_headdata-zzquan_all = gs_head-amount.
  ls_headdata-erdat = gs_head-erdat.
  ls_headdata-erzet = gs_head-erzet.
  ls_headdata-ernam = gs_head-ernam.
  ls_headdata-aedat = gs_head-aedat.
  ls_headdata-aezet = gs_head-aetim.
  ls_headdata-aenam = gs_head-aenam.
  ls_headdata-audat = gs_head-aenam.
  ls_headdata-audat = gs_head-bldat.
  ls_headdata-budat = gs_head-budat.
  ls_headdata-budat = gs_head-budat.
  ls_headdata-budat = gs_head-budat.
  ls_headdata-mblnr = gs_head-mblnr.
  ls_headdata-mjahr = gs_head-mjahr.

  IF gs_head-remark3 IS INITIAL.
    gs_head-remark3 = gs_head-afono.
  ENDIF.
  ls_headdata-zzhkbh = gs_head-remark3.

  LOOP AT pt_item WHERE menge > 0.
    lt_itemdata-vbeln = pt_item-vbeln_va.
    lt_itemdata-posnr = pt_item-posnr_va.
    lt_itemdata-zzpino = pt_item-zzpino.
    lt_itemdata-idnlf = pt_item-idnlf.
    lt_itemdata-matnr = pt_item-matnr.
    lt_itemdata-maktx = pt_item-maktx.
    lt_itemdata-charg = pt_item-charg.
    lt_itemdata-zcolor1 = pt_item-zcolor.
    lt_itemdata-zsize1 = pt_item-zsize.
    lt_itemdata-zzbox_qua = 1.
    lt_itemdata-labst = pt_item-menge.
    lt_itemdata-brgew = pt_item-brgew.
    lt_itemdata-ntgew = pt_item-ntgew.
    lt_itemdata-zzbox_nm = pt_item-remark2.
    APPEND lt_itemdata.

  ENDLOOP.

  CALL FUNCTION 'ZSDG_PACKING_CREATE'
    EXPORTING
      headdata  = ls_headdata
    IMPORTING
      packingno = pv_zzpacknm
    TABLES
      itemdata  = lt_itemdata
      et_return = lt_return.

  LOOP AT lt_return.
    PERFORM frm_add_msg USING lt_return-type
                          lt_return-id
                          lt_return-number
                          lt_return-message_v1
                          lt_return-message_v2
                          lt_return-message_v3
                          lt_return-message_v4.
  ENDLOOP.

ENDFORM.


FORM frm_zz_pack_del.
  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: lv_packingno TYPE zzpacknm.

  CHECK g_error NE 'X'.
  CHECK gs_head-bustyp = 'ASN02' OR gs_head-bustyp = 'R1019' OR gs_head-bustyp = 'R1020' .

  IF gs_head-remark2 IS NOT INITIAL.
    lv_packingno = gs_head-remark2.
  ENDIF.
  CALL FUNCTION 'ZSDG_PACKING_DELETE'
    EXPORTING
      packingno = lv_packingno
    TABLES
      et_return = lt_return.

  LOOP AT lt_return.
    PERFORM frm_add_msg USING lt_return-type
                          lt_return-id
                          lt_return-number
                          lt_return-message_v1
                          lt_return-message_v2
                          lt_return-message_v3
                          lt_return-message_v4.
  ENDLOOP.

  CHECK g_error <> 'X'.
  CLEAR gs_head-remark2.

  IF gs_head-remark4 IS NOT INITIAL.
    lv_packingno = gs_head-remark4.
  ENDIF.

  CALL FUNCTION 'ZSDG_PACKING_DELETE'
    EXPORTING
      packingno = lv_packingno
    TABLES
      et_return = lt_return.

  LOOP AT lt_return.
    PERFORM frm_add_msg USING lt_return-type
                          lt_return-id
                          lt_return-number
                          lt_return-message_v1
                          lt_return-message_v2
                          lt_return-message_v3
                          lt_return-message_v4.
  ENDLOOP.

  CHECK g_error <> 'X'.
  CLEAR gs_head-remark4.

  UPDATE zafo_head SET remark2 = '' remark4 = '' WHERE afono = gs_head-afono.


ENDFORM.
