
FORM frm_cancel.

  CHECK g_error NE 'X'.

  CASE gs_bustyp-execute_type .
    WHEN 'CC'.

      PERFORM frm_cc_cancel.

    WHEN 'MB'.

      PERFORM frm_zz_pack_del." 删除装箱单

      PERFORM frm_zz_auto_sctl_post." 生产退料

      PERFORM frm_mb_cancel_ww_rec. "取消半成品委外收货转单凭证

      PERFORM frm_mb_cancel.

      PERFORM frm_mb_cancel_ww. "取消半成品委外发料转单凭证

      PERFORM frm_mb_cancel_cjsx.
    WHEN 'SD'.

      PERFORM frm_sd_cancel.

    WHEN 'M1'.

      PERFORM frm_m1_cancel.

    WHEN ''.

      PERFORM frm_add_msg USING 'E' 'ZAFO' 086 '' '' '' ''."记账业务类型为空,不可以冲销,请联系管理员

    WHEN OTHERS.

      PERFORM frm_add_msg USING 'E' 'ZAFO' 087 '' '' '' ''."无法冲销

  ENDCASE.

ENDFORM.


FORM frm_check_cancel_data.

  IF g_readonly <> 'D'.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 088 '' '' '' ''."只有显示状态才可以过账
  ENDIF.

  IF gs_head-status <> 'S' AND gs_head-status <> 'F'.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 089 '' '' '' ''."只有过账状态可以冲销
  ENDIF.

  IF gs_head-bustyp = 'R1002' OR gs_head-bustyp = 'R1003' OR gs_head-bustyp = 'R1006'
     OR gs_head-bustyp = 'R1004' OR gs_head-bustyp = 'R1005'
     OR gs_head-bustyp = 'R1013' OR gs_head-bustyp = 'R1015' OR gs_head-bustyp = 'R1019'
     OR gs_head-bustyp = 'ASN02' OR gs_head-bustyp = 'ASN03'.
    READ TABLE gt_item INDEX 1.
    SELECT SINGLE * INTO @DATA(cjs_head)
        FROM zafo_head
      WHERE bustyp = 'CJD01'
      AND status NOT IN ('A','D')
      AND ebeln = @gt_item-ebeln.
    IF sy-subrc EQ 0 .
      PERFORM frm_add_msg USING 'E' 'ZAFO' 000 '已有被处理的超交单：' cjs_head-afono '' ''."已有被处理的超交单！
    ENDIF.
  ENDIF.


  IF gs_head-mblnr IS NOT INITIAL.
    SELECT SINGLE lelnr INTO @DATA(ls_lelnr)
      FROM zvpaydoc
      WHERE mblnr = @gs_head-mblnr
      AND zstat <> 'D'.
    IF sy-subrc EQ 0.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 105 ls_lelnr '' '' ''."已经创建对账单,无法冲销
    ENDIF.
  ENDIF.
ENDFORM.


FORM frm_cc_cancel.
  DATA: et_return	TYPE TABLE OF	bapiret2 WITH HEADER LINE.
  DATA:lv_long_vgbel TYPE zlong_vgbel.
  DATA:lv_type TYPE zcct_type.
  lv_long_vgbel = gs_head-afono.

  SELECT SINGLE default_value INTO @DATA(ls_value)
    FROM zafo_post_rule
    WHERE bustyp = @gs_bustyp-bustyp
    AND to_fieldname = 'CC_CCT_TYPE'.

  lv_type = ls_value.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'ZCCT_CANCEL'
    EXPORTING
      im_long_vgbel = lv_long_vgbel
*     IM_VGBEL      =
      im_cct_type   = lv_type
      im_post_date  = gs_head-budat_re
*     IM_COMMIT     =
*     IM_IDOCNUM    =
*     IM_MESCOD     =
    TABLES
      et_return     = et_return
    EXCEPTIONS
      error         = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO'  090  gs_head-afono '' '' ''."冲销错误
    LOOP AT et_return WHERE type = 'E'.
      PERFORM frm_add_msg USING et_return-type
                                et_return-id
                                et_return-number
                                et_return-message_v1
                                et_return-message_v2
                                et_return-message_v3
                                et_return-message_v4.
    ENDLOOP.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ELSE.

    PERFORM frm_add_msg USING 'S' 'ZAFO'  091  gs_head-afono '' '' ''."冲销成功

    PERFORM frm_set_status USING 'D'.
    gs_head-del_flag = 'X'.
    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    LOOP AT gt_item ASSIGNING <gs_item>.
*      <gs_item>-mblnr = gs_head-mblnr.
*      <gs_item>-mjahr = gs_head-mjahr.
*      <gs_item>-zeile = gs_head-zeile.

      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text.
    ENDLOOP.

    PERFORM frm_set_ref_delete.

    PERFORM frm_db_modify.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
ENDFORM.


FORM frm_mb_cancel.

  DATA: ls_head TYPE bapi2017_gm_head_ret.
  DATA: lt_item TYPE TABLE OF bapi2017_gm_item_04 WITH HEADER LINE.
  DATA: lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CHECK g_error NE 'X'.

  IF gs_head-status = 'F'.
    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    gs_head-del_flag = 'X'.
    PERFORM frm_set_status USING 'D'.
    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-mblnr = gs_head-mblnr.
      <gs_item>-mjahr = gs_head-mjahr.
      <gs_item>-zeile = gs_head-zeile.

      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.

      APPEND gt_item_modify.
      CLEAR gt_item_modify.

    ENDLOOP.

    PERFORM frm_set_ref_delete.

    PERFORM frm_db_modify.

    PERFORM frm_add_msg USING 'S' 'ZAFO' 091  '' '' '' '' ."冲销成功,
  ELSE.

    SELECT zeile INTO TABLE lt_item
      FROM mseg
      WHERE mblnr = gs_head-mblnr
        AND mjahr = gs_head-mjahr
        AND xauto = ''.

    SORT lt_item DESCENDING.

    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument    = gs_head-mblnr
        matdocumentyear     = gs_head-mjahr
        goodsmvt_pstng_date = gs_head-budat_re
        goodsmvt_pr_uname   = sy-uname
*       DOCUMENTHEADER_TEXT =
      IMPORTING
        goodsmvt_headret    = ls_head
      TABLES
        return              = lt_return
        goodsmvt_matdocitem = lt_item[].
  ENDIF.

  LOOP AT lt_return.
    PERFORM frm_add_msg USING lt_return-type
                          lt_return-id
                          lt_return-number
                          lt_return-message_v1
                          lt_return-message_v2
                          lt_return-message_v3
                          lt_return-message_v4.
  ENDLOOP.

  IF g_error <> 'X'.
    gs_head-mblnr_re = ls_head-mat_doc.
    gs_head-mjahr_re = ls_head-doc_year.

    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    gs_head-del_flag = 'X'.

    PERFORM frm_set_status USING 'D'.
    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-mblnr = gs_head-mblnr.
      <gs_item>-mjahr = gs_head-mjahr.
      <gs_item>-zeile = gs_head-zeile.

      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.

      APPEND gt_item_modify.
      CLEAR gt_item_modify.

    ENDLOOP.

    PERFORM frm_set_ref_delete.

    PERFORM frm_db_modify.

    PERFORM frm_add_msg USING 'S' 'ZAFO' 092  gs_head-mblnr ls_head-mat_doc '' '' ."物料凭证&1已冲销成功,冲销凭证为&2

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.
  ENDIF.
ENDFORM.


FORM frm_mb_cancel_ww_rec.
  DATA:ls_goodsmvt_headret LIKE bapi2017_gm_head_ret.
  DATA: lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CHECK g_error NE 'X'.
  CHECK g_bustyp = 'R1011' OR g_bustyp = 'R1020'.
  CHECK gs_head-belnr IS NOT INITIAL.


  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument    = gs_head-belnr
      matdocumentyear     = gs_head-mjahr
      goodsmvt_pstng_date = gs_head-budat_re
      goodsmvt_pr_uname   = sy-uname
    IMPORTING
      goodsmvt_headret    = ls_goodsmvt_headret
    TABLES
      return              = lt_return.
  LOOP AT lt_return.
    PERFORM frm_add_msg USING lt_return-type
                          lt_return-id
                          lt_return-number
                          lt_return-message_v1
                          lt_return-message_v2
                          lt_return-message_v3
                          lt_return-message_v4.
  ENDLOOP.
  IF g_error = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.
  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    CLEAR:gs_head-belnr, gs_head-mjahr.

    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    PERFORM frm_db_modify.

  ENDIF.
ENDFORM.


FORM frm_mb_cancel_ww.
  DATA:ls_goodsmvt_headret LIKE bapi2017_gm_head_ret.
  DATA: lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CHECK g_error NE 'X'.
  CHECK g_bustyp = 'R1012' AND gs_head-belnr IS NOT INITIAL.

  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument    = gs_head-belnr
      matdocumentyear     = gs_head-mjahr
      goodsmvt_pstng_date = gs_head-budat_re
      goodsmvt_pr_uname   = sy-uname
    IMPORTING
      goodsmvt_headret    = ls_goodsmvt_headret
    TABLES
      return              = lt_return.
  LOOP AT lt_return.
    PERFORM frm_add_msg USING lt_return-type
                          lt_return-id
                          lt_return-number
                          lt_return-message_v1
                          lt_return-message_v2
                          lt_return-message_v3
                          lt_return-message_v4.
  ENDLOOP.
  IF g_error = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.
  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    CLEAR:gs_head-belnr, gs_head-mjahr.

    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    PERFORM frm_db_modify.

  ENDIF.

ENDFORM.


FORM frm_m1_cancel.


  DATA: ls_mb_head LIKE bapi2017_gm_head_01,
        ls_mb_item TYPE bapi2017_gm_item_create,
        lt_mb_item TYPE TABLE OF bapi2017_gm_item_create,
        mb_gm_code LIKE bapi2017_gm_code,
        lt_return  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA : l_mat_doc   LIKE bapi2017_gm_head_ret-mat_doc.
  DATA : l_doc_year LIKE bapi2017_gm_head_ret-doc_year.

  CLEAR ls_mb_head.
  ls_mb_head-pstng_date = gs_head-budat. "凭证中的过帐日期
  ls_mb_head-doc_date =  gs_head-bldat. " 凭证中的凭证日期
  ls_mb_head-pr_uname = sy-uname."用户名
  ls_mb_head-ref_doc_no = gs_head-afono."参考凭证编号

  ls_mb_head-header_txt = gs_head-remark1."参考凭证编号
  mb_gm_code = '06'.


  DATA:ls_line_id TYPE mb_line_id.
  CLEAR ls_line_id.
  DATA: lv_sales_ord  TYPE vbeln_va,
        lv_s_ord_item TYPE posnr_va.

  LOOP AT gt_item WHERE menge <> 0.
    IF gt_item-zppdhd = gt_item-to_zppdhd.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 081 '' '' '' ''."转入/转出大货通知单不能一致
    ENDIF.

    ADD 1 TO ls_line_id.
    ls_mb_item-line_id = ls_line_id.

    ls_mb_item-material = gt_item-matnr.
    ls_mb_item-plant = gt_item-werks.
    ls_mb_item-stge_loc = gt_item-lgort.
    ls_mb_item-batch = gt_item-charg.
    ls_mb_item-entry_qnt = gt_item-menge.
    ls_mb_item-entry_uom = gt_item-meins.

    SELECT SINGLE vbeln,posnr
      INTO ( @lv_sales_ord,@lv_s_ord_item )
      FROM ztpp0089
      WHERE zppdhd = @gt_item-to_zppdhd.
    IF sy-subrc NE 0 OR lv_sales_ord IS INITIAL.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 070 '' '' '' ''."对方大货通知单不存在
    ENDIF.
    ls_mb_item-sales_ord = lv_sales_ord.
    ls_mb_item-s_ord_item = lv_s_ord_item.
    ls_mb_item-val_sales_ord = lv_sales_ord.
    ls_mb_item-val_s_ord_item = lv_s_ord_item.
    ls_mb_item-move_type = '411'.
    ls_mb_item-spec_stock = 'E'.
    ls_mb_item-move_reas = '4004'.

    APPEND ls_mb_item TO lt_mb_item.

    ADD 1 TO ls_line_id.

    ls_mb_item-line_id = ls_line_id.
    ls_mb_item-move_type = '413'.
    ls_mb_item-move_reas = '4004'.
    ls_mb_item-spec_stock = ''.
    ls_mb_item-sales_ord = gt_item-vbeln_va.
    ls_mb_item-s_ord_item = gt_item-posnr_va.
    ls_mb_item-val_sales_ord = ls_mb_item-sales_ord.
    ls_mb_item-val_s_ord_item = ls_mb_item-s_ord_item.

    APPEND ls_mb_item TO lt_mb_item.
    CLEAR ls_mb_item.

  ENDLOOP.

  IF lt_mb_item[] IS INITIAL.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 078 '' '' '' ''."无有效数据行，无法过账
    RETURN.
  ENDIF.

  CHECK g_error IS INITIAL.


  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_mb_head
      goodsmvt_code    = mb_gm_code
    IMPORTING
      materialdocument = l_mat_doc
      matdocumentyear  = l_doc_year
    TABLES
      goodsmvt_item    = lt_mb_item
      return           = lt_return.

  IF sy-subrc = 0 AND l_mat_doc IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    gs_head-mblnr_re = l_mat_doc.
    gs_head-mjahr_re = l_doc_year.

    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    gs_head-del_flag = 'X'.

    PERFORM frm_set_status USING 'D'.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-mblnr = gs_head-mblnr.
      <gs_item>-mjahr = gs_head-mjahr.
      <gs_item>-zeile = gs_head-zeile.

      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
      APPEND gt_item_modify.
      CLEAR gt_item_modify.
    ENDLOOP.

    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    PERFORM frm_db_modify.

    PERFORM frm_add_msg USING 'S' 'ZAFO' 092  gs_head-mblnr l_mat_doc '' '' ."物料凭证&1已冲销成功,冲销凭证为&2

    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.
  ENDIF.
ENDFORM.


FORM frm_sd_cancel.

  DATA: ls_emkpf TYPE emkpf.
  DATA: lt_return TYPE TABLE OF mesg WITH HEADER LINE.
  DATA: lv_vbeln TYPE likp-vbeln.

  lv_vbeln = gs_head-afono.

  IF lv_vbeln IS NOT INITIAL.

    CLEAR: lt_return[],ls_emkpf.

    CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE'
      EXPORTING
        i_vbeln                   = lv_vbeln
        i_budat                   = gs_head-budat_re
        i_tcode                   = 'VL09'
        i_vbtyp                   = 'J'
      IMPORTING
        es_emkpf                  = ls_emkpf
      TABLES
        t_mesg                    = lt_return
      EXCEPTIONS
        error_reverse_goods_issue = 1
        error_message             = 99
        OTHERS                    = 2.

    IF ls_emkpf-mblnr IS NOT INITIAL.
      COMMIT WORK AND WAIT.
    ELSE.
      g_error = 'X'.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  LOOP AT lt_return WHERE  msgty = 'E'.
    PERFORM frm_add_msg USING lt_return-msgty 'ZAFO' 000  lt_return-text lt_return-msgv1 lt_return-msgv2 lt_return-msgv3 .
  ENDLOOP.

  IF g_error <> 'X'.
    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    gs_head-del_flag = 'X'.

    PERFORM frm_set_status USING 'D'.
    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    LOOP AT gt_item ASSIGNING <gs_item>.

      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.

      APPEND gt_item_modify.
      CLEAR gt_item_modify.

    ENDLOOP.

    PERFORM frm_set_ref_delete.

    PERFORM frm_db_modify.

    PERFORM frm_add_msg USING 'S' 'ZAFO' 137  '' '' '' '' ."交货单冲销成功！
  ENDIF.

ENDFORM.
