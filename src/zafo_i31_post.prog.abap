*&---------------------------------------------------------------------*
*& 包含               ZAFO_ALV_D01
*&---------------------------------------------------------------------*


FORM frm_post.

  CHECK g_error NE 'X'.

  CASE gs_bustyp-execute_type .
    WHEN 'MB'.

      PERFORM frm_mb_per_post.

      CHECK g_error NE 'X'.

      PERFORM frm_set_charg.

      PERFORM frm_save_data.

      PERFORM frm_get_cpt_jhsfp.

      PERFORM frm_mb_post_ww."半成品委外发料，先进行销售订单库存转到常规库存【411】

      PERFORM frm_mb_post.

      PERFORM frm_mb_post_ww_rec."半成品委外收货，后进行常规库存转销售订单库存【413】

      PERFORM frm_zz_auto_ransfer_post."  属性转移

      PERFORM frm_zz_cjd_post." 超交单

      PERFORM frm_zz_pack_post." 装箱单

    WHEN 'CC'.

      PERFORM frm_cc_per_post.

      PERFORM frm_set_charg.

      PERFORM frm_save_data.

      PERFORM frm_cc_post.

    WHEN 'PD'.

      PERFORM frm_set_charg.

      PERFORM frm_pd_post.

    WHEN 'M1'.

      PERFORM frm_save_data.

      PERFORM frm_m1_post.

    WHEN 'PO'.

      PERFORM frm_po_per_post.

      CHECK g_error NE 'X'.

      PERFORM frm_set_po_cost USING 'X'.

      IF gs_head-ebeln IS INITIAL.

        PERFORM frm_auth_po_check USING '01' gs_head-bsart gs_head-ekgrp CHANGING g_error.

        CHECK g_error IS INITIAL.

        PERFORM frm_po_create.

      ELSE.

        PERFORM frm_auth_po_check USING '01' gs_head-bsart gs_head-ekgrp CHANGING g_error.

        CHECK g_error IS INITIAL.

        PERFORM frm_po_change.

      ENDIF.

    WHEN 'PR'.

      PERFORM frm_po_per_post.

      IF gs_head-banfn IS INITIAL.
        PERFORM frm_pr_create.
      ELSE.
        PERFORM frm_pr_change.
      ENDIF.

    WHEN 'PRO'.
      PERFORM frm_po_per_post.

      CHECK g_error NE 'X'.

      PERFORM frm_set_pro_cost USING 'X'.

      IF gs_head-ebeln IS INITIAL.

        PERFORM frm_auth_po_check USING '01' gs_head-bsart gs_head-ekgrp CHANGING g_error.

        CHECK g_error IS INITIAL.

        PERFORM frm_pro_create.
      ELSE.

        PERFORM frm_auth_po_check USING '01' gs_head-bsart gs_head-ekgrp CHANGING g_error.

        CHECK g_error IS INITIAL.

        PERFORM frm_pro_change.
      ENDIF.

    WHEN 'POC'. " 采购变更单

      CHECK gs_head-status = 'B'.

      PERFORM frm_poc_post.

    WHEN 'POP'. " 采购单价变更单

      PERFORM frm_set_po_cost USING 'X'.

      CHECK gs_head-status = 'B'.

      PERFORM frm_pop_post.

    WHEN 'SO'." 销售订单
      IF gs_head-vbeln_va IS INITIAL.
        PERFORM frm_so_create.
      ELSE.
        PERFORM frm_so_change.
      ENDIF.

    WHEN 'SD'." 销售交货
      PERFORM frm_sd_post.

    WHEN OTHERS.

      PERFORM frm_add_msg USING 'E' 'ZAFO' '069' '' '' '' ''."记账业务类型为空,不可以过账,请联系管理员

  ENDCASE.

  PERFORM frm_post_after_check.
ENDFORM.


FORM frm_mb_per_post.

  LOOP AT gt_item WHERE menge <> 0.
    IF gt_item-to_zppdhd IS NOT INITIAL AND gt_item-zppdhd IS INITIAL.
      SELECT SINGLE vbeln,posnr
        INTO ( @gt_item-vbeln_va,@gt_item-posnr_va )
        FROM ztpp0089
        WHERE zppdhd = @gt_item-to_zppdhd.
      IF sy-subrc NE 0 OR gt_item-vbeln_va IS INITIAL.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 070 '' '' '' ''."对方大货通知单不存在
      ELSE.
        MODIFY gt_item.
      ENDIF.
    ENDIF.

    IF gt_item-zppdhd IS NOT INITIAL.
      SELECT SINGLE zzscgdh
        INTO ( @gt_item-aufnr )
        FROM ztpp0089
        WHERE zppdhd = @gt_item-zppdhd.
      IF sy-subrc NE 0 .
        PERFORM frm_add_msg USING 'E' 'ZAFO' 070 '' '' '' ''."对方大货通知单不存在
      ELSE.
        MODIFY gt_item.
      ENDIF.
    ENDIF.

    IF gt_item-bukrs <> gt_item-werks AND gt_item-bukrs IS NOT INITIAL  .
      PERFORM frm_add_msg USING 'E' 'ZAFO' 071 '' '' '' ''."业务公司与库存工厂不能不一致
    ENDIF.

    PERFORM frm_mb_check_menge USING gt_item.

  ENDLOOP.

  IF sy-subrc NE 0.
    IF g_bustyp = 'R1002'.
      PERFORM frm_pop_confirm USING TEXT-046."是否确认删除选中行?'.
      IF g_error = 'X'.
        RETURN.
      ELSE.
        g_error = 'X'.
        MESSAGE s103.
        UPDATE zafo_head SET status = 'F' WHERE afono = gs_head-afono.

        LOOP AT gt_item.
          UPDATE zafo_item SET item_status = 'F' afono_ref = gt_item-afono  afonr_ref = gt_item-afonr
            WHERE afono = gt_item-afono_ref AND afonr = gt_item-afonr_ref.
        ENDLOOP.
        UPDATE zafo_head SET status = 'T' WHERE afono = gt_item-afono_ref.
        PERFORM frm_set_status USING 'F'.
      ENDIF.
    ELSE.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 072 '' '' '' ''."物料凭证过账不能数量都为0
    ENDIF.
  ENDIF.

ENDFORM.


FORM frm_mb_check_menge USING us_item TYPE zafo_sitem.
  CHECK gs_head-bustyp <> 'R1012'.

  LOOP AT gt_screen WHERE object = gs_bustyp-object AND fieldalv = 'ITEM' AND split_col <> ''.

    CHECK gt_screen-split_col+7(2) <> 'BJ'." 因为小数取舍原因，报价数量不能比较

    ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE us_item TO FIELD-SYMBOL(<fs_value1>).
    CHECK sy-subrc EQ 0.

    ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE us_item TO FIELD-SYMBOL(<fs_value2>).
    CHECK sy-subrc EQ 0.

    IF <fs_value1> > <fs_value2>.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 073 '' '' '' ''."记账数量不能大于参考数量
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM frm_cc_per_post.

  LOOP AT gt_item WHERE menge <> 0.

    IF gt_item-to_zppdhd IS NOT INITIAL AND gt_item-zppdhd IS INITIAL.
      SELECT SINGLE vbeln,posnr INTO ( @gt_item-vbeln_va,@gt_item-posnr_va )
        FROM ztpp0089 WHERE zppdhd = @gt_item-to_zppdhd.
      IF sy-subrc NE 0 OR gt_item-vbeln_va IS INITIAL.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 070 '' '' '' ''."对方大货通知单不存在
      ELSE.
        MODIFY gt_item.
      ENDIF.
    ENDIF.

    IF gt_item-zppdhd IS NOT INITIAL.
      SELECT SINGLE zzscgdh INTO ( @gt_item-aufnr )
        FROM ztpp0089 WHERE zppdhd = @gt_item-zppdhd.
      IF sy-subrc NE 0 .
        PERFORM frm_add_msg USING 'E' 'ZAFO' 070 '' '' '' ''."对方大货通知单不存在
      ELSE.
        MODIFY gt_item.
      ENDIF.
    ENDIF.

    IF gt_item-bukrs = gt_item-werks.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 074 '' '' '' ''."业务公司与库存工厂不能一致
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 075 '' '' '' ''."不能数量都为0
  ENDIF.

ENDFORM.


FORM frm_po_per_post.
  DATA: l_msg TYPE char200.

  CLEAR l_msg.
  IF gs_head-bukrs <> gs_head-ekorg.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 098 '' '' '' ''."采购组织与公司不一致！
  ENDIF.

  CHECK g_error <> 'X'.

  IF gs_head-lifnr IS NOT INITIAL AND gs_head-bukrs IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(ls_0010)
      FROM zscmt0010
      WHERE partner = @gs_head-lifnr
           AND bukrs = @gs_head-bukrs.
    IF sy-subrc NE 0.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 076 '' '' '' ''."供应商未扩展到该公司，请联系信息部扩展该供应商
    ELSE.

      CASE ls_0010-waers.
        WHEN 'CNY'.
          IF gs_head-ekorg <> '1000'
            AND gs_head-ekorg <> '1006'
            AND gs_head-ekorg <> '1010'.
            l_msg = '本合同公司为国内公司，供应商货币不是人民币，请确认是否继续保存？'.
          ENDIF.
        WHEN OTHERS.
          IF gs_head-ekorg <> '2000'.
            l_msg = '本合同货币为外币，签署合同的公司不是2000公司，请确认是否继续保存？'.
          ENDIF.
      ENDCASE.

    ENDIF.
  ENDIF.

  IF l_msg IS NOT INITIAL.
    PERFORM frm_pop_confirm USING l_msg.
  ENDIF.

  CHECK g_error <> 'X'.

  IF gs_head-ekorg = '2000' AND gs_head-kostl IS NOT INITIAL.
    gs_head-kostl = '0020000010'.
  ENDIF.

  LOOP AT gt_item.
    gt_item-werks = gs_head-ekorg.
    MODIFY gt_item.
  ENDLOOP.

  LOOP AT gt_item_po.
    gt_item_po-werks = gs_head-ekorg.
    MODIFY gt_item_po.
  ENDLOOP.

ENDFORM.


FORM frm_set_charg .

  DATA:lt_mch1 TYPE TABLE OF zmch1 WITH HEADER LINE .
  CLEAR lt_mch1[].
  LOOP AT gt_item INTO gs_item WHERE menge <> ''.
    LOOP AT gt_item_batch WHERE afono = gs_item-afono AND afonr = gs_item-afonr.
      lt_mch1-zcolor    = gs_item-zcolor    .
      lt_mch1-zsize     = gs_item-zsize     .
      lt_mch1-znorms    = gs_item-znorms    .
      lt_mch1-zshelves  = gs_item-zshelves  .
      lt_mch1-zppflag  = gs_item-zppflag    .
*      去除产前样
      IF g_bustyp EQ 'R1002' OR g_bustyp EQ 'R1003' OR g_bustyp EQ 'R1006'
        OR g_bustyp EQ 'R1004' OR g_bustyp EQ 'R1005'
        OR g_bustyp EQ 'R1011' OR g_bustyp EQ 'R1013' OR g_bustyp EQ 'R1014'
        OR g_bustyp EQ 'R1015' OR g_bustyp EQ 'R1018' OR g_bustyp EQ 'R1019' OR g_bustyp EQ 'R1020'
        OR g_bustyp EQ 'R1032' OR g_bustyp EQ 'R1033'.
        CLEAR lt_mch1-zppflag.
      ENDIF.
*      去除产前样

      lt_mch1-zvat_nub  = gt_item_batch-zvat_nub  .

      CALL FUNCTION 'ZAFO_GET_CHARG'
        EXPORTING
          i_commit = 'X'
        CHANGING
          cs_zmch1 = lt_mch1.
      gt_item_batch-charg = lt_mch1-charg.
      MODIFY gt_item_batch.
    ENDLOOP.

    CHECK sy-subrc NE 0.
*      去除产前样
    IF g_bustyp EQ 'R1002' OR g_bustyp EQ 'R1003' OR g_bustyp EQ 'R1006'
        OR g_bustyp EQ 'R1004' OR g_bustyp EQ 'R1005'
        OR g_bustyp EQ 'R1011' OR g_bustyp EQ 'R1013' OR g_bustyp EQ 'R1014'
        OR g_bustyp EQ 'R1015' OR g_bustyp EQ 'R1018' OR g_bustyp EQ 'R1019' OR g_bustyp EQ 'R1020'
        OR g_bustyp EQ 'R1032' OR g_bustyp EQ 'R1033'.
      IF gs_item-charg IS INITIAL.
        IF gs_item-zcolor     IS NOT INITIAL
          OR gs_item-zsize     IS NOT INITIAL
          OR gs_item-znorms    IS NOT INITIAL
          OR gs_item-zshelves  IS NOT INITIAL
          OR gs_item-zvat_nub  IS NOT INITIAL.
          lt_mch1-zcolor    = gs_item-zcolor.
          lt_mch1-zsize     = gs_item-zsize.
          lt_mch1-znorms    = gs_item-znorms.
          lt_mch1-zshelves  = gs_item-zshelves.
          lt_mch1-zvat_nub  = gs_item-zvat_nub.
          APPEND lt_mch1.
          CLEAR lt_mch1.
        ELSE.
          gs_item-charg = '99'.
          MODIFY gt_item FROM gs_item TRANSPORTING charg.
        ENDIF.
      ENDIF.

    ELSE.

      IF gs_item-charg IS INITIAL.
        IF gs_item-zcolor       IS NOT INITIAL
          OR gs_item-zsize       IS NOT INITIAL
          OR gs_item-znorms      IS NOT INITIAL
          OR gs_item-zshelves    IS NOT INITIAL
          OR gs_item-zvat_nub    IS NOT INITIAL
          OR gs_item-zppflag     IS NOT INITIAL
          .
          lt_mch1-zcolor    = gs_item-zcolor    .
          lt_mch1-zsize     = gs_item-zsize     .
          lt_mch1-znorms    = gs_item-znorms    .
          lt_mch1-zshelves  = gs_item-zshelves  .
          lt_mch1-zvat_nub  = gs_item-zvat_nub  .
          lt_mch1-zppflag  = gs_item-zppflag  .

          APPEND lt_mch1.
          CLEAR lt_mch1.
        ELSE.
          gs_item-charg = '99'.
          MODIFY gt_item FROM gs_item TRANSPORTING charg.
        ENDIF.
      ENDIF.
*      去除产前样
    ENDIF.

    IF gs_item-umcha IS INITIAL.
      IF gs_item-zcolor_um     IS NOT INITIAL
        OR gs_item-zsize_um     IS NOT INITIAL
        OR gs_item-znorms_um    IS NOT INITIAL
        OR gs_item-zshelves_um  IS NOT INITIAL
        OR gs_item-zvat_nub_um  IS NOT INITIAL
        OR gs_item-zppflag_um   IS NOT INITIAL.
        lt_mch1-zcolor   = gs_item-zcolor_um  .
        lt_mch1-zsize    = gs_item-zsize_um   .
        lt_mch1-znorms   = gs_item-znorms_um  .
        lt_mch1-zshelves = gs_item-zshelves_um.
        lt_mch1-zvat_nub = gs_item-zvat_nub_um.
        lt_mch1-zppflag  = gs_item-zppflag_um .

        APPEND lt_mch1.
        CLEAR lt_mch1.
      ELSE.
        IF gs_item-ummat IS NOT INITIAL OR gs_item-lgort IS NOT INITIAL.
          gs_item-umcha = '99'.
          MODIFY gt_item FROM gs_item TRANSPORTING umcha.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'ZAFO_GET_CHARG'
    EXPORTING
      i_commit = 'X'
    TABLES
      ct_zmch1 = lt_mch1[].

  LOOP AT gt_item INTO gs_item WHERE menge <> ''.
    IF gs_item-charg IS INITIAL.

*      去除产前样
      IF g_bustyp EQ 'R1002'  OR g_bustyp EQ 'R1003'OR g_bustyp EQ 'R1006'
        OR g_bustyp EQ 'R1004' OR g_bustyp EQ 'R1005'
        OR g_bustyp EQ 'R1011' OR g_bustyp EQ 'R1013' OR g_bustyp EQ 'R1014'
        OR g_bustyp EQ 'R1015' OR g_bustyp EQ 'R1018' OR g_bustyp EQ 'R1019' OR g_bustyp EQ 'R1020'
        OR g_bustyp EQ 'R1032' OR g_bustyp EQ 'R1033'.

        IF   gs_item-zcolor       IS NOT INITIAL
          OR gs_item-zsize        IS NOT INITIAL
          OR gs_item-znorms       IS NOT INITIAL
          OR gs_item-zshelves     IS NOT INITIAL
          OR gs_item-zvat_nub     IS NOT INITIAL.

          READ TABLE lt_mch1 WITH KEY  zcolor    =  gs_item-zcolor
                                       zsize     =  gs_item-zsize
                                       znorms    =  gs_item-znorms
                                       zshelves  =  gs_item-zshelves
                                       zvat_nub  =  gs_item-zvat_nub .
          IF sy-subrc EQ 0.
            gs_item-charg = lt_mch1-charg.
            MODIFY gt_item FROM gs_item TRANSPORTING charg.
          ELSE.

          ENDIF.
          APPEND lt_mch1.
          CLEAR lt_mch1.
        ENDIF.
      ELSE.
        IF   gs_item-zcolor       IS NOT INITIAL
          OR gs_item-zsize        IS NOT INITIAL
          OR gs_item-znorms       IS NOT INITIAL
          OR gs_item-zshelves     IS NOT INITIAL
          OR gs_item-zvat_nub     IS NOT INITIAL
          OR gs_item-zppflag     IS NOT INITIAL.

          READ TABLE lt_mch1 WITH KEY  zcolor    =  gs_item-zcolor
                                       zsize     =  gs_item-zsize
                                       znorms    =  gs_item-znorms
                                       zshelves  =  gs_item-zshelves
                                       zvat_nub  =  gs_item-zvat_nub
                                       zppflag  =  gs_item-zppflag
                                       .
          IF sy-subrc EQ 0.
            gs_item-charg = lt_mch1-charg.
            MODIFY gt_item FROM gs_item TRANSPORTING charg.
          ELSE.

          ENDIF.
          APPEND lt_mch1.
          CLEAR lt_mch1.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gs_item-umcha IS INITIAL.
      IF   gs_item-zcolor_um       IS NOT INITIAL
        OR gs_item-zsize_um        IS NOT INITIAL
        OR gs_item-znorms_um       IS NOT INITIAL
        OR gs_item-zshelves_um     IS NOT INITIAL
        OR gs_item-zvat_nub_um     IS NOT INITIAL
        OR gs_item-zppflag_um     IS NOT INITIAL .
        READ TABLE lt_mch1 WITH KEY  zcolor    =  gs_item-zcolor_um
                                     zsize     =  gs_item-zsize_um
                                     znorms    =  gs_item-znorms_um
                                     zshelves  =  gs_item-zshelves_um
                                     zvat_nub  =  gs_item-zvat_nub_um
                                     zppflag  =  gs_item-zppflag_um .
        IF sy-subrc EQ 0.
          gs_item-umcha = lt_mch1-charg.
          MODIFY gt_item FROM gs_item TRANSPORTING umcha.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM frm_mb_post.

  DATA:lt_rule TYPE TABLE OF zafo_post_rule WITH HEADER LINE.

  DATA: ls_mb_head LIKE bapi2017_gm_head_01,
        ls_mb_item TYPE bapi2017_gm_item_create,
        lt_mb_item TYPE TABLE OF bapi2017_gm_item_create,
        mb_gm_code LIKE bapi2017_gm_code,
        lt_return  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA:ls_afonr TYPE zafonr.

  DATA : l_mat_doc   LIKE bapi2017_gm_head_ret-mat_doc.
  DATA : l_doc_year LIKE bapi2017_gm_head_ret-doc_year.

  FIELD-SYMBOLS: <from_value> TYPE any,
                 <to_value>   TYPE any.

  CHECK g_error NE 'X'.

  SELECT * FROM zafo_post_rule
    INTO TABLE lt_rule
    WHERE bustyp = gs_bustyp-bustyp.
  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 077 '' '' '' ''."缺少物料凭证记账配置,不可以过账,请联系管理员
    RETURN.
  ENDIF.

  CLEAR ls_mb_head.
  ls_mb_head-pstng_date = gs_head-budat. "凭证中的过帐日期
  ls_mb_head-doc_date =  gs_head-bldat. " 凭证中的凭证日期
  ls_mb_head-pr_uname = sy-uname."用户名
  ls_mb_head-ref_doc_no = gs_head-afono."参考凭证编号
  ls_mb_head-header_txt = gs_head-remark1."参考描述
  mb_gm_code = '06'.


*  READ TABLE GT_ITEM INTO GS_ITEM INDEX 1.

  LOOP AT lt_rule WHERE to_tabname = '' .
    ASSIGN  (lt_rule-to_fieldname) TO <to_value>.
    CHECK sy-subrc EQ 0.

    IF lt_rule-default_value IS NOT INITIAL.
      <to_value> = lt_rule-default_value.
    ENDIF.

    CASE lt_rule-from_fieldalv.
      WHEN 'HEAD'.
        ASSIGN COMPONENT lt_rule-from_fname OF STRUCTURE gs_head TO <from_value>.
        CHECK sy-subrc EQ 0.
        <to_value> = <from_value>.
    ENDCASE.
  ENDLOOP.

  LOOP AT lt_rule WHERE to_tabname = 'MB_HEAD' .
    ASSIGN COMPONENT lt_rule-to_fieldname OF STRUCTURE ls_mb_head TO <to_value>.
    CHECK sy-subrc EQ 0.

    IF lt_rule-default_value IS NOT INITIAL.
      <to_value> = lt_rule-default_value.
    ENDIF.

    CASE lt_rule-from_fieldalv.
      WHEN 'HEAD'.
        ASSIGN COMPONENT lt_rule-from_fname OF STRUCTURE gs_head TO <from_value>.
        CHECK sy-subrc EQ 0.
        <to_value> = <from_value>.
    ENDCASE.
  ENDLOOP.

  CLEAR lt_mb_item.
  CLEAR ls_afonr.

  LOOP AT gt_item INTO gs_item WHERE menge <> ''.
    CLEAR ls_mb_item.

    LOOP AT lt_rule WHERE to_tabname = 'MB_ITEM' .
      ASSIGN COMPONENT lt_rule-to_fieldname OF STRUCTURE ls_mb_item TO <to_value>.
      CHECK sy-subrc EQ 0.

      IF lt_rule-default_value IS NOT INITIAL.

        IF lt_rule-default_value = 'REASON'.
          SELECT SINGLE saknr INTO <to_value> FROM zafo_reason
            WHERE bustyp = g_bustyp
            AND reason = gs_head-reason.
        ELSE.
          <to_value> = lt_rule-default_value.
        ENDIF.
      ENDIF.

      CASE lt_rule-from_fieldalv.
        WHEN 'HEAD'.
          ASSIGN COMPONENT lt_rule-from_fname OF STRUCTURE gs_head TO <from_value>.
          CHECK sy-subrc EQ 0.
          <to_value> = <from_value>.

        WHEN 'ITEM'.
          ASSIGN COMPONENT lt_rule-from_fname OF STRUCTURE gs_item TO <from_value>.
          CHECK sy-subrc EQ 0.
          <to_value> = <from_value>.
      ENDCASE.

    ENDLOOP.

    IF gs_bustyp-busref = 'K'.
      IF gs_item-vbeln_va IS INITIAL.
        CLEAR ls_mb_item-spec_stock.
      ELSE.
        ls_mb_item-spec_stock = 'E'.
      ENDIF.
    ENDIF.

    IF sy-subrc EQ 0.
      LOOP AT gt_item_batch WHERE afono = gs_item-afono AND afonr = gs_item-afonr.
        ADD 1 TO ls_afonr.
        ls_mb_item-line_id = ls_afonr.
        ls_mb_item-batch = gt_item_batch-charg.
        ls_mb_item-entry_qnt = gt_item_batch-menge.
        APPEND ls_mb_item TO lt_mb_item.

      ENDLOOP.
      IF sy-subrc NE 0.
        ADD 1 TO ls_afonr.
        ls_mb_item-line_id = ls_afonr.
        APPEND ls_mb_item TO lt_mb_item.
        CLEAR ls_mb_item.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF lt_mb_item[] IS INITIAL.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 078 '' '' '' ''."无有效数据行，无法过账
    RETURN.
  ENDIF.

  PERFORM fix_mb_hk_gr TABLES lt_mb_item . " 香港公司下单采购到德清的订单处理，材料直接入库到德清仓库

  PERFORM fix_mb_hk_rc TABLES lt_mb_item . " 海外公司采购，香港公司下单处理，出入库操作代处理

  PERFORM fix_mb_kc_rc TABLES lt_mb_item . " 库存采购带出入库操作代处理

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

    gs_head-mblnr = l_mat_doc.
    gs_head-mjahr = l_doc_year.

    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    PERFORM frm_set_status USING 'S'.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-mblnr = gs_head-mblnr.
      <gs_item>-mjahr = gs_head-mjahr.
      <gs_item>-zeile = gs_head-zeile.

      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text.

*      PERFORM frm_set_ref_status USING <gs_item>-afono_ref 'S'.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
      APPEND gt_item_modify.
      CLEAR gt_item_modify.
    ENDLOOP.


    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    PERFORM frm_db_modify.

    PERFORM frm_add_msg USING 'S' 'ZAFO' 079 l_mat_doc ''  '' ''."物料凭证&1已过账成功

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
    "ADD BY AT-YUXS 20220329 取消411过账凭证 begin
    IF  g_bustyp = 'R1012' AND gs_head-belnr IS NOT INITIAL.
      PERFORM frm_mb_cancel_ww.
    ENDIF.
  ENDIF.

ENDFORM.


FORM fix_mb_hk_gr TABLES lt_item STRUCTURE bapi2017_gm_item_create .

  DATA:it_item TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE.
  DATA:ls_line_id TYPE mb_line_id.

  CLEAR ls_line_id.

  CHECK g_bustyp = 'R1002' OR g_bustyp = 'R1003' OR g_bustyp = 'R1006'
     OR g_bustyp = 'R1032' OR g_bustyp = 'R1033'.

  READ TABLE lt_item INDEX 1.

  CHECK lt_item-plant = '2000'.

  LOOP AT lt_item.
    ADD 1 TO ls_line_id.
    lt_item-line_id = ls_line_id.

    APPEND lt_item TO it_item.

    ADD 1 TO ls_line_id.
    lt_item-line_id = ls_line_id.
    lt_item-move_type = 'Z03'.
    lt_item-mvt_ind = ''.
    lt_item-costcenter = '0020000010' .

    IF g_bustyp <> 'R1032' AND g_bustyp <> 'R1033'.
      SELECT SINGLE zvbeln,zposnr FROM ekpo
        INTO ( @lt_item-sales_ord, @lt_item-s_ord_item )
        WHERE ebeln = @lt_item-po_number
        AND ebelp = @lt_item-po_item.
      IF sy-subrc EQ 0 AND lt_item-sales_ord IS NOT INITIAL.
        lt_item-spec_stock = 'E'.
        lt_item-val_sales_ord = lt_item-sales_ord.
        lt_item-val_s_ord_item = lt_item-s_ord_item.
      ENDIF.
    ENDIF.

    lt_item-po_number = ''.
    lt_item-po_item = ''.
    lt_item-move_reas = '6002'.
    APPEND lt_item TO it_item.

    ADD 1 TO ls_line_id.
    lt_item-line_id = ls_line_id.
    lt_item-plant = '1000'.
    lt_item-move_type = '501'.
    lt_item-move_reas = '4008'.
    lt_item-vendor = '0000002000'.
    CLEAR lt_item-costcenter .
    APPEND lt_item TO it_item.

  ENDLOOP.

  lt_item[] = it_item[].
ENDFORM.


FORM fix_mb_hk_rc TABLES lt_item STRUCTURE bapi2017_gm_item_create .

  DATA:it_item TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE.
  DATA:ls_line_id TYPE mb_line_id.
  CLEAR ls_line_id.

  CHECK g_bustyp = 'R1013'.

  READ TABLE lt_item INDEX 1.

  CHECK lt_item-plant = '2000'.

  LOOP AT lt_item.
    ADD 1 TO ls_line_id.
    lt_item-line_id = ls_line_id.

    APPEND lt_item TO it_item.

    ADD 1 TO ls_line_id.
    lt_item-line_id = ls_line_id.
    lt_item-move_type = 'Z03'.
    lt_item-mvt_ind = ''.
    lt_item-costcenter = '0020000010' .

    SELECT SINGLE zvbeln,zposnr FROM ekpo
      INTO ( @lt_item-sales_ord, @lt_item-s_ord_item )
      WHERE ebeln = @lt_item-po_number
      AND ebelp = @lt_item-po_item.

    IF sy-subrc EQ 0 AND lt_item-sales_ord IS NOT INITIAL.
      lt_item-spec_stock = 'E'.
      lt_item-val_sales_ord = lt_item-sales_ord.
      lt_item-val_s_ord_item = lt_item-s_ord_item.
    ENDIF.
    lt_item-po_number = ''.
    lt_item-po_item = ''.
    lt_item-move_reas = '6002'.
    APPEND lt_item TO it_item.

  ENDLOOP.

  lt_item[] = it_item[].
ENDFORM.



FORM fix_mb_kc_rc TABLES lt_item STRUCTURE bapi2017_gm_item_create .

  DATA:it_item TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE.
  DATA:ls_line_id TYPE mb_line_id.
  CLEAR ls_line_id.

  CHECK g_bustyp = '1008'.

  LOOP AT lt_item.
    ADD 1 TO ls_line_id.
    lt_item-line_id = ls_line_id.

    APPEND lt_item TO it_item.

    ADD 1 TO ls_line_id.
    lt_item-line_id = ls_line_id.
    lt_item-move_type = 'Z03'.
    lt_item-mvt_ind = ''.
    lt_item-costcenter = gs_head-kostl .
    lt_item-po_number = ''.
    lt_item-po_item = ''.
    lt_item-move_reas = '6002'.
    APPEND lt_item TO it_item.

  ENDLOOP.

  lt_item[] = it_item[].
ENDFORM.


FORM frm_cc_post.

  DATA:lt_rule TYPE TABLE OF zafo_post_rule WITH HEADER LINE.
  DATA:cc_channel1   TYPE  zchannel1,
       cc_channel2   TYPE  zchannel2,
       cc_channel3   TYPE  zchannel3,
       cc_cct_type   TYPE  zcct_type,
       cc_post_date  TYPE  budat,
       cc_bktxt      TYPE  char50,
       cc_long_vgbel TYPE  zlong_vgbel,
       cc_vkorg      TYPE  vkorg,
       cc_werks      TYPE  werks_d,
       cc_commit     TYPE  char1,
       cc_idocnum    TYPE  edi_docnum,
       cc_mescod     TYPE  edi_mescod,
       ec_lognr      TYPE lognr,
       ec_vgbel      TYPE vgbel.

  DATA: ls_cc_item TYPE zcct_data,
        lt_cc_item TYPE TABLE OF zcct_data.

  DATA: lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: lt_borident TYPE TABLE OF borident WITH HEADER LINE.


  FIELD-SYMBOLS: <from_value> TYPE any,
                 <to_value>   TYPE any.

  SELECT * FROM zafo_post_rule
    INTO TABLE lt_rule
    WHERE bustyp = gs_bustyp-bustyp.

  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 080 '' '' '' ''."缺少跨公司交易记账配置,不可以过账,请联系管理员
    RETURN.
  ENDIF.


  LOOP AT lt_rule WHERE to_tabname = '' .
    ASSIGN  (lt_rule-to_fieldname) TO <to_value>.
    CHECK sy-subrc EQ 0.

    IF lt_rule-default_value IS NOT INITIAL.
      <to_value> = lt_rule-default_value.
    ENDIF.

    CASE lt_rule-from_fieldalv.
      WHEN 'HEAD'.

        CASE lt_rule-from_fname .
          WHEN 'WERKS&LGORT'.
            <to_value> = gs_head-werks && gs_head-lgort.
          WHEN 'UMWRK&UMLGO'.
            <to_value> = gs_head-umwrk && gs_head-umlgo.
          WHEN OTHERS.
            ASSIGN COMPONENT lt_rule-from_fname OF STRUCTURE gs_head TO <from_value>.
            CHECK sy-subrc EQ 0.
            <to_value> = <from_value>.
        ENDCASE.
    ENDCASE.
  ENDLOOP.


  CLEAR lt_cc_item.

  LOOP AT gt_item INTO gs_item WHERE menge <> ''.
    CLEAR ls_cc_item.

    LOOP AT lt_rule WHERE to_tabname = 'CC_ITEM' .
      ASSIGN COMPONENT lt_rule-to_fieldname OF STRUCTURE ls_cc_item TO <to_value>.
      CHECK sy-subrc EQ 0.

      IF lt_rule-default_value IS NOT INITIAL.
        <to_value> = lt_rule-default_value.
      ENDIF.

      CASE lt_rule-from_fieldalv.
        WHEN 'HEAD'.
          ASSIGN COMPONENT lt_rule-from_fname OF STRUCTURE gs_head TO <from_value>.
          CHECK sy-subrc EQ 0.
          <to_value> = <from_value>.

        WHEN 'ITEM'.
          ASSIGN COMPONENT lt_rule-from_fname OF STRUCTURE gs_item TO <from_value>.
          CHECK sy-subrc EQ 0.
          <to_value> = <from_value>.
      ENDCASE.

    ENDLOOP.

    IF sy-subrc EQ 0.

      ls_cc_item-line_id = gs_item-afonr.

      IF ls_cc_item-vbeln_va IS NOT INITIAL.
        SELECT SINGLE * FROM ebew
          WHERE vbeln = @ls_cc_item-vbeln_va
          AND posnr = @ls_cc_item-posnr_va
          AND matnr = @ls_cc_item-matnr
          AND bwkey = @gs_head-werks
          INTO @DATA(ls_ebew).
        IF sy-subrc EQ 0.
          IF ls_ebew-lbkum IS NOT INITIAL.
            ls_cc_item-netwr = ls_cc_item-menge * ls_ebew-salk3 / ls_ebew-lbkum.
          ENDIF.
          IF ls_cc_item-netwr IS INITIAL.
            IF ls_ebew-vprsv = 'S'.
              ls_cc_item-netwr = ls_cc_item-menge * ls_ebew-stprs.
            ELSEIF ls_ebew-vprsv = 'V'.
              ls_cc_item-netwr = ls_cc_item-menge * ls_ebew-verpr.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        SELECT SINGLE * FROM mbew
          WHERE matnr = @ls_cc_item-matnr
          AND bwkey = @gs_head-werks
          INTO @DATA(ls_mbew).
        IF sy-subrc EQ 0 AND ls_mbew-lbkum IS NOT INITIAL.
          ls_cc_item-netwr = ls_cc_item-menge * ls_mbew-salk3 / ls_mbew-lbkum.
        ENDIF.
      ENDIF.

      APPEND ls_cc_item TO lt_cc_item.
    ENDIF.
  ENDLOOP.

  IF lt_cc_item[] IS INITIAL.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 078 '' '' '' ''."无有效数据行，无法过账
    RETURN.
  ENDIF.

  CLEAR lt_borident[].
  lt_borident-objtype = 'ZAFO'.
  lt_borident-objkey = gs_head-afono.
  APPEND lt_borident.

  CALL FUNCTION 'ZCCT_POST'
    EXPORTING
      im_channel1   = cc_channel1
      im_channel2   = cc_channel2
      im_channel3   = cc_channel3
      im_cct_type   = cc_cct_type
      im_post_date  = cc_post_date
      im_bktxt      = cc_bktxt
      im_long_vgbel = cc_long_vgbel
      im_vkorg      = cc_vkorg
      im_werks      = cc_werks
      im_commit     = cc_commit
      im_idocnum    = cc_idocnum
      im_mescod     = cc_mescod
    IMPORTING
      ex_lognr      = ec_lognr
      ex_vgbel      = ec_vgbel
    TABLES
      im_data       = lt_cc_item[]
*     im_line_data  =
      et_return     = lt_return[]
      et_borident   = lt_borident
    EXCEPTIONS
      error         = 1
      OTHERS        = 2.

  IF sy-subrc = 0 .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    READ TABLE lt_borident WITH KEY objtype = 'BUS2017'.
    IF sy-subrc EQ 0.
      gs_head-mblnr = lt_borident-objkey+0(10).
      gs_head-mjahr = lt_borident-objkey+10(4).
    ENDIF.

    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    PERFORM frm_set_status USING 'S'.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-mblnr = gs_head-mblnr.
      <gs_item>-mjahr = gs_head-mjahr.
      <gs_item>-zeile = gs_head-zeile.

      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text.

*      PERFORM frm_set_ref_status USING <gs_item>-afono_ref 'S'.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
      APPEND gt_item_modify.
      CLEAR gt_item_modify.
    ENDLOOP.


    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    PERFORM frm_db_modify.

    PERFORM frm_add_msg USING 'S' 'ZAFO' 079 gs_head-mblnr ''  '' ''."物料凭证&1已过账成功

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


FORM frm_m1_post.

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


*  READ TABLE GT_ITEM INTO GS_ITEM INDEX 1.


  DATA:ls_line_id TYPE mb_line_id.
  CLEAR ls_line_id.

  LOOP AT gt_item WHERE menge <> 0.
    IF gt_item-zppdhd = gt_item-to_zppdhd.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 081 '' '' '' ''."转入/转出大货通知单不能一致
    ENDIF.

    ADD 1 TO ls_line_id.
    ls_mb_item-line_id = ls_line_id.

    ls_mb_item-material = gt_item-matnr.
    ls_mb_item-plant = gt_item-werks.
*    ls_mb_item-move_plant = gt_item-werks.
    ls_mb_item-stge_loc = gt_item-lgort.
*    ls_mb_item-move_stloc = gt_item-lgort.
    ls_mb_item-batch = gt_item-charg.
*    ls_mb_item-move_batch = gt_item-charg.
    ls_mb_item-entry_qnt = gt_item-menge.
    ls_mb_item-entry_uom = gt_item-meins.
    ls_mb_item-sales_ord = gt_item-vbeln_va.
    ls_mb_item-s_ord_item = gt_item-posnr_va.
*    ls_mb_item-line_id = gt_item-afonr.

    ls_mb_item-val_sales_ord = ls_mb_item-sales_ord.
    ls_mb_item-val_s_ord_item = ls_mb_item-s_ord_item.
    ls_mb_item-move_type = '411'.
    ls_mb_item-spec_stock = 'E'.
    ls_mb_item-move_reas = '4004'.

    APPEND ls_mb_item TO lt_mb_item.

    ADD 1 TO ls_line_id.

    ls_mb_item-line_id = ls_line_id.
    ls_mb_item-move_type = '413'.
    ls_mb_item-move_reas = '4004'.
    ls_mb_item-spec_stock = ''.

    SELECT SINGLE vbeln,posnr
      INTO ( @ls_mb_item-sales_ord,@ls_mb_item-s_ord_item )
      FROM ztpp0089
      WHERE zppdhd = @gt_item-to_zppdhd.
    IF sy-subrc NE 0 OR ls_mb_item-sales_ord IS INITIAL.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 070 '' '' '' ''."对方大货通知单不存在
    ENDIF.

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

    gs_head-mblnr = l_mat_doc.
    gs_head-mjahr = l_doc_year.

    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    PERFORM frm_set_status USING 'S'.

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

    PERFORM frm_add_msg USING 'S' 'ZAFO' 079  l_mat_doc ''  '' ''."物料凭证&1已过账成功

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


FORM frm_pd_post.
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

  LOOP AT gt_item WHERE menge <> 0.

    ls_mb_item-material = gt_item-matnr.
    ls_mb_item-plant = gt_item-werks.
    ls_mb_item-stge_loc = gt_item-lgort.
    ls_mb_item-batch = gt_item-charg.

    ls_mb_item-entry_uom = gt_item-meins.
    ls_mb_item-sales_ord = gt_item-vbeln_va.
    ls_mb_item-s_ord_item = gt_item-posnr_va.
    ls_mb_item-val_sales_ord = ls_mb_item-sales_ord.
    ls_mb_item-val_s_ord_item = ls_mb_item-s_ord_item.

    IF ls_mb_item-sales_ord IS NOT INITIAL.
      ls_mb_item-spec_stock = 'E'.
    ENDIF.

    ls_mb_item-line_id = gt_item-afonr.

    IF gt_item-menge > 0.
      ls_mb_item-move_type = '701'.
      ls_mb_item-move_reas = '5001'.
      ls_mb_item-entry_qnt = gt_item-menge.
    ELSEIF gt_item-menge < 0.
      ls_mb_item-move_type = '702'.
      ls_mb_item-move_reas = '5001'.
      ls_mb_item-entry_qnt = - gt_item-menge.
    ELSEIF gt_item-menge = 0.
      CONTINUE.
    ENDIF.

    APPEND ls_mb_item TO lt_mb_item.

  ENDLOOP.

  LOOP AT gt_item WHERE menge2 <> 0.

    ls_mb_item-material = gt_item-matnr.
    ls_mb_item-plant = gt_item-werks.
    ls_mb_item-stge_loc = gt_item-lgort.
    ls_mb_item-batch = gt_item-charg.
    ls_mb_item-move_batch = gt_item-umcha.

    ls_mb_item-entry_uom = gt_item-meins.
    ls_mb_item-sales_ord = gt_item-vbeln_va.
    ls_mb_item-s_ord_item = gt_item-posnr_va.
    ls_mb_item-val_sales_ord = ls_mb_item-sales_ord.
    ls_mb_item-val_s_ord_item = ls_mb_item-s_ord_item.

    IF ls_mb_item-sales_ord IS NOT INITIAL.

      ls_mb_item-spec_stock = 'E'.

    ENDIF.

    ls_mb_item-line_id = gt_item-afonr + 100000.

    IF gt_item-menge > 0.
      ls_mb_item-move_type = '311'.
      ls_mb_item-move_reas = '5002'.
      ls_mb_item-entry_qnt = gt_item-menge2.
    ENDIF.

    APPEND ls_mb_item TO lt_mb_item.

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

    gs_head-mblnr = l_mat_doc.
    gs_head-mjahr = l_doc_year.

    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    PERFORM frm_set_status USING 'S'.

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

    PERFORM frm_add_msg USING 'S' 'ZAFO' 079 l_mat_doc ''  '' ''."物料凭证&1已过账成功

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


FORM frm_po_create.

  DATA: po_header            TYPE bapimepoheader,
        po_headerx           TYPE bapimepoheaderx,

        po_item              TYPE TABLE OF bapimepoitem WITH HEADER LINE,
        po_itemx             TYPE TABLE OF bapimepoitemx WITH HEADER LINE,

        po_con_h             TYPE TABLE OF bapimepocondheader  WITH HEADER LINE,
        po_con_hx            TYPE TABLE OF bapimepocondheaderx WITH HEADER LINE,

        po_con_i             TYPE TABLE OF bapimepocond WITH HEADER LINE,
        po_con_ix            TYPE TABLE OF bapimepocondx WITH HEADER LINE,

        po_schedule          TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
        po_schedulex         TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,

        po_account           TYPE TABLE OF bapimepoaccount WITH HEADER LINE,
        po_accountx          TYPE TABLE OF  bapimepoaccountx WITH HEADER LINE,

        po_cond              TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_condx             TYPE TABLE OF  bapimepocondx WITH HEADER LINE,

        po_textheader        TYPE TABLE OF bapimepotextheader WITH HEADER LINE,

        po_number            LIKE bapimepoheader-po_number,
        i_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,

        t_item               TYPE TABLE OF bapimepotext WITH HEADER LINE,

        pocomponents         TYPE STANDARD TABLE OF bapimepocomponent WITH HEADER LINE,
        pocomponentsx        TYPE STANDARD TABLE OF bapimepocomponentx WITH HEADER LINE,

        ls_bapi_te_mepoitem  TYPE bapi_te_mepoitem,
        ls_bapi_te_mepoitemx TYPE bapi_te_mepoitemx,
        gt_extensionin       TYPE TABLE OF bapiparex WITH HEADER LINE,
        gs_extensionin       TYPE bapiparex.

  DATA:i_module TYPE char01,
       i_commit TYPE char1,
       cs_zmch1 TYPE zmch1.

  IF gs_head-afono IS INITIAL.

    PERFORM frm_get_next_afono USING gs_object-nrnr CHANGING gs_head-afono.
    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-afono = gs_head-afono.
    ENDLOOP.

    gs_head-erdat = sy-datum.
    gs_head-erzet = sy-uzeit.
    gs_head-ernam = sy-uname.

  ENDIF.

  po_number = gs_head-afono.

  PERFORM process USING TEXT-045."'正在创建采购订单……'.

  CLEAR:  po_header,po_headerx,po_item,po_item[],po_itemx,po_itemx[],po_schedule,po_schedule[],
   t_item,t_item[],i_return,i_return[],t_return,t_return[].
  REFRESH:gt_extensionin.

  "***创建采购订单
  po_header-po_number    = po_number.
  po_header-doc_type    = gs_head-bsart.
  po_header-currency    =  gs_head-waers.
  po_header-comp_code   =  gs_head-bukrs.
  po_header-purch_org   =  gs_head-ekorg.
  po_header-pur_group   =  gs_head-ekgrp.

  po_header-vendor      =  gs_head-lifnr.
  po_header-our_ref     =  ''.
  po_header-doc_date = gs_head-bldat.

  po_header-item_intvl = 0.


  po_headerx-po_number   = 'X'.
  po_headerx-doc_type   = 'X'.
  po_headerx-currency = 'X'.
  " PO_HEADERX-ITEM_INTVL = 'X'.
  po_headerx-vendor     = 'X'.
  "PO_HEADERX-PMNTTRMS   = 'X'.
  po_headerx-comp_code  = 'X'.
  po_headerx-purch_org  = 'X'.
  po_headerx-pur_group  = 'X'.
  po_headerx-our_ref     =  'X'.
  po_headerx-doc_date = 'X'.
  po_headerx-pmnttrms = 'X'.
  po_headerx-collect_no = 'X'.
  po_headerx-sales_pers = 'X'.
  po_headerx-ref_1 = 'X'.

  po_headerx-item_intvl = 'X'.

  LOOP AT gt_item_po.

    po_item-po_item    = gt_item_po-ebelp.
    po_itemx-po_item   = gt_item_po-ebelp.

    po_item-vend_mat   = gt_item_po-idnlf.
    po_itemx-vend_mat  = 'X'.

    po_item-material_long  = gt_item_po-matnr.
    po_itemx-material_long = 'X'.

    po_item-plant   = gt_item_po-werks.
    po_itemx-plant  = 'X'.

*    po_item-stge_loc   = gt_item_po-lgort.
*    po_itemx-stge_loc   = 'X'.

    po_item-quantity  = gt_item_po-menge.
    po_itemx-quantity = 'X'.

    po_item-item_cat  = ''.
    po_itemx-item_cat = 'X'.

    po_item-over_dlv_tol  = gt_item_po-uebto.
    po_itemx-over_dlv_tol = 'X'.

    po_item-over_dlv_tol  = gt_item-uebto.
    po_itemx-over_dlv_tol = 'X'.

*    PO_ITEM-NET_PRICE  = GT_ITEM_PO-PRICE.
*    PO_ITEMX-NET_PRICE   = 'X'.

    po_item-preq_name = gs_head-afnam.
    po_item-preq_name = 'X'.

    po_item-orderpr_un  = gt_item_po-bstme.
    po_itemx-orderpr_un = 'X'.

    po_item-conv_den1  = gt_item_po-zmm_tran_rate.
    po_itemx-conv_den1 = 'X' .

    po_item-conv_num1  = 1.
    po_itemx-conv_num1 = 'X'.

    IF gt_item_po-price IS INITIAL.
      po_item-ir_ind  = ''.
      po_itemx-ir_ind = 'X'.
    ELSE.
      po_item-ir_ind  = 'X'.
      po_itemx-ir_ind = 'X'.

      po_cond-itm_number = gt_item_po-ebelp.
      po_cond-cond_type  = 'Z100'.
      po_cond-cond_value = gt_item_po-price / 10 .
      po_cond-cond_p_unt = gt_item_po-peinh.
      po_cond-change_id  = 'U'.
      APPEND po_cond.
      CLEAR po_cond.

      po_condx-itm_number = po_cond-itm_number.
      po_condx-cond_type  = 'X'.
      po_condx-cond_value = 'X'.
      po_condx-cond_p_unt = 'X'.
      po_condx-change_id  = 'X'.
      APPEND po_condx.
      CLEAR po_condx.

      po_cond-itm_number = gt_item_po-ebelp.
      po_cond-cond_type  = 'Z101'.
      po_cond-cond_value = gt_item_po-cost_amount / 10.
      po_cond-change_id  = 'U'.
      APPEND po_cond.
      CLEAR po_cond.

      po_condx-itm_number = po_cond-itm_number.
      po_condx-cond_value = 'X'.
      po_condx-change_id  = 'X'.
      APPEND po_condx.
      CLEAR po_condx.
    ENDIF.

    po_item-price_unit  = gt_item-peinh.
    po_itemx-price_unit = 'X'.

    po_item-tax_code  = gs_head-mwskz.
    po_itemx-tax_code = 'X'.

    po_item-stk_seg_long  = gt_item_po-sgt_scat.
    po_itemx-stk_seg_long = 'X'.


    IF gs_head-bustyp = 'PO003' ." 半成品委外采购合同
      po_account-po_item = gt_item_po-ebelp.
      po_accountx-po_item = 'X'.
      CASE gs_head-bukrs.
        WHEN '1000'.
          po_account-costcenter = '0010000130'.
        WHEN '1010'.
          po_account-costcenter = '0010100030'.
        WHEN '3000'.
          po_account-costcenter = '0030000020'.
        WHEN '5000'.
          po_account-costcenter = '0050000020'.
        WHEN OTHERS.
      ENDCASE.

      po_item-po_unit  = gt_item_po-meins.
      po_itemx-po_unit = 'X'.

      po_accountx-costcenter = 'X'.

      po_item-material  = gt_item_po-matnr.
      po_item-item_cat  = 'L'."项目类别-外协加工
      po_itemx-item_cat = 'X'.


      PERFORM frm_get_component_cpt TABLES pocomponents pocomponentsx USING gt_item_po.
      IF pocomponents[] IS INITIAL.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 121 '' '' '' ''."没有找到原材料，请确认BOM！
        RETURN.
      ENDIF.

      DATA: lv_charg TYPE charg_d.
      PERFORM frm_get_po_charg USING gt_item_po CHANGING lv_charg.
      po_item-batch        = lv_charg.
      po_itemx-batch       = 'X'.
      po_account-orderid   = gt_item_po-aufnr.
      po_accountx-orderid  = 'X'.
      po_account-quantity  = gt_item_po-menge.
      po_accountx-quantity = 'X'.

      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.

    ELSEIF gs_head-knttp = 'F' ." 工序委外采购合同
      po_account-po_item = gt_item_po-ebelp.
      po_accountx-po_item = gt_item_po-ebelp.
      CASE gs_head-bukrs.
        WHEN '1000'.
          po_account-costcenter = '0010000130'.
        WHEN '1010'.
          po_account-costcenter = '0010100030'.
        WHEN '3000'.
          po_account-costcenter = '0030000020'.
        WHEN '5000'.
          po_account-costcenter = '0050000020'.
        WHEN OTHERS.
      ENDCASE.
      CLEAR po_item-material_long.
      CLEAR po_itemx-material_long.
      po_item-short_text  = gt_item_po-maktx_zh.
      po_itemx-short_text  = 'X'.
      po_item-po_unit   = gt_item_po-meins.
      po_itemx-po_unit   = 'X'.

      po_accountx-costcenter = 'X'.
      po_item-acctasscat = 'F'.
      po_itemx-acctasscat = 'X'.
      po_item-matl_group = 'Z200'.
      po_itemx-matl_group = 'X'.
      po_account-orderid = gt_item_po-aufnr.
      po_accountx-orderid = 'X'.
      po_account-quantity = gt_item_po-menge.
      po_accountx-quantity = 'X'.

      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.

    ELSEIF  gt_item_po-vbeln_va IS NOT INITIAL.
      po_item-acctasscat = 'M'.
      po_itemx-acctasscat = 'X'.

      po_account-po_item = gt_item_po-ebelp.
      po_accountx-po_item = gt_item_po-ebelp.

      po_account-sd_doc = gt_item_po-vbeln_va.
      po_accountx-sd_doc = 'X'.

      po_account-itm_number = gt_item_po-posnr_va.
      po_accountx-itm_number = 'X'.

      po_account-quantity = gt_item_po-menge.
      po_accountx-quantity = 'X'.

      po_account-gl_account = '1406000000'.
      po_accountx-gl_account = 'X'.

      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.
    ENDIF.

    APPEND po_item.
    APPEND po_itemx.
    CLEAR  po_item.
    CLEAR  po_itemx.

    po_schedule-po_item = gt_item_po-ebelp.
    po_schedulex-po_item = gt_item_po-ebelp.

    po_schedule-sched_line = 1.
    po_schedulex-sched_line = 1.

    po_schedulex-po_itemx = 'X'.

    po_schedule-del_datcat_ext = 'D'.
    po_schedulex-del_datcat_ext = 'X'.

    po_schedule-delivery_date = gt_item_po-eeind.
    po_schedulex-delivery_date = 'X'.

    po_schedule-quantity = gt_item_po-menge.
    po_schedulex-quantity = 'X'.


    APPEND po_schedule.
    APPEND po_schedulex.
    CLEAR  po_schedule.
    CLEAR  po_schedulex.

    ls_bapi_te_mepoitem-po_item = gt_item_po-ebelp.
    ls_bapi_te_mepoitem-zzpino = gt_item_po-zzpino.
    ls_bapi_te_mepoitem-zppdhd = gt_item_po-zppdhd.
*    LS_BAPI_TE_MEPOITEM-ZMATNR = GT_ITEM_PO-ZMATNR.
    ls_bapi_te_mepoitem-zkunnr_mat = gt_item_po-zkunnr_mat.
    ls_bapi_te_mepoitem-zvbeln = gt_item_po-vbeln_va.
    ls_bapi_te_mepoitem-zposnr = gt_item_po-posnr_va.
    ls_bapi_te_mepoitem-zkunnr = gt_item_po-kunnr.
    ls_bapi_te_mepoitem-zcolor = gt_item_po-zcolor.
    ls_bapi_te_mepoitem-zcolor_text = gt_item_po-zcolor_text.
    ls_bapi_te_mepoitem-zsize = gt_item_po-zsize.
    ls_bapi_te_mepoitem-zppflag = gt_item_po-zppflag.

    gs_extensionin-structure =   'BAPI_TE_MEPOITEM'.
    MOVE ls_bapi_te_mepoitem TO gs_extensionin-valuepart1.
    APPEND gs_extensionin TO gt_extensionin.
    CLEAR gs_extensionin.

    ls_bapi_te_mepoitemx-po_item = gt_item_po-ebelp.
    ls_bapi_te_mepoitemx-zzpino = 'X'.
    ls_bapi_te_mepoitemx-zppdhd = 'X'.
*    LS_BAPI_TE_MEPOITEMX-ZMATNR = 'X'.
    ls_bapi_te_mepoitemx-zkunnr_mat = 'X'.
    ls_bapi_te_mepoitemx-zvbeln = 'X'.
    ls_bapi_te_mepoitemx-zposnr = 'X'.
    ls_bapi_te_mepoitemx-zkunnr = 'X'.
    ls_bapi_te_mepoitemx-zcolor = 'X'.
    ls_bapi_te_mepoitemx-zcolor_text = 'X'.
    ls_bapi_te_mepoitemx-zsize = 'X'.
    ls_bapi_te_mepoitemx-zppflag = 'X'.

    gs_extensionin-structure =   'BAPI_TE_MEPOITEMX'.
    MOVE ls_bapi_te_mepoitemx TO gs_extensionin-valuepart1.
    APPEND gs_extensionin TO gt_extensionin.
    CLEAR gs_extensionin.

  ENDLOOP.

  CLEAR: gs_extensionin,ls_bapi_te_mepoitem,ls_bapi_te_mepoitemx.

*  CLEAR: po_number.

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader         = po_header
      poheaderx        = po_headerx
      no_price_from_po = 'X'
*    IMPORTING
*     exppurchaseorder = po_number
    TABLES
      return           = i_return
      poitem           = po_item
      poitemx          = po_itemx
      potextheader     = po_textheader
      poschedule       = po_schedule
      poschedulex      = po_schedulex
      poaccount        = po_account
      poaccountx       = po_accountx
      pocond           = po_cond
      pocondx          = po_condx
      potextitem       = t_item
      pocomponents     = pocomponents[] "add by at-yuxs 20220328 委外采购订单组件
      pocomponentsx    = pocomponentsx[] "add by at-yuxs 20220328 委外采购订单组件
      extensionin      = gt_extensionin.

  READ TABLE i_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    gs_head-ebeln = po_number.

    PERFORM frm_set_status USING 'A'.
    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-ebeln = gs_head-ebeln.
      <gs_item>-afono = gs_head-afono.
    ENDLOOP.

    LOOP AT gt_item_po ASSIGNING <gs_item_po>.
      <gs_item_po>-ebeln = gs_head-ebeln.
      <gs_item_po>-afono = gs_head-afono.
    ENDLOOP.

    LOOP AT gt_item_po_cpt ASSIGNING <gs_item_po_cpt>.
      <gs_item_po_cpt>-ebeln = gs_head-ebeln.
      <gs_item_po_cpt>-afono = gs_head-afono.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CLEAR po_number.
    CLEAR gs_head-ebeln.
  ENDIF.

  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.

ENDFORM.


FORM frm_po_change.

  DATA: po_header            TYPE bapimepoheader,
        po_headerx           TYPE bapimepoheaderx,

        po_item              TYPE TABLE OF bapimepoitem WITH HEADER LINE,
        po_itemx             TYPE TABLE OF bapimepoitemx WITH HEADER LINE,

        po_con_h             TYPE TABLE OF bapimepocondheader  WITH HEADER LINE,
        po_con_hx            TYPE TABLE OF bapimepocondheaderx WITH HEADER LINE,

        po_con_i             TYPE TABLE OF bapimepocond WITH HEADER LINE,
        po_con_ix            TYPE TABLE OF bapimepocondx WITH HEADER LINE,

        po_schedule          TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
        po_schedulex         TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,

        po_account           TYPE TABLE OF bapimepoaccount WITH HEADER LINE,
        po_accountx          TYPE TABLE OF  bapimepoaccountx WITH HEADER LINE,

        po_condy             TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_cond              TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_condx             TYPE TABLE OF  bapimepocondx WITH HEADER LINE,

        po_textheader        TYPE TABLE OF bapimepotextheader WITH HEADER LINE,

        po_number            LIKE bapimepoheader-po_number,
        i_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,

        t_item               TYPE TABLE OF bapimepotext WITH HEADER LINE,

        pocomponents         TYPE STANDARD TABLE OF bapimepocomponent WITH HEADER LINE,
        pocomponentsx        TYPE STANDARD TABLE OF bapimepocomponentx WITH HEADER LINE,
        ls_bapi_te_mepoitem  TYPE bapi_te_mepoitem,
        ls_bapi_te_mepoitemx TYPE bapi_te_mepoitemx,
        gt_extensionin       TYPE TABLE OF bapiparex WITH HEADER LINE,
        gs_extensionin       TYPE bapiparex.

  DATA:i_module TYPE char01,
       i_commit TYPE char1,
       cs_zmch1 TYPE zmch1.

  PERFORM process USING TEXT-045."'正在修改采购订单……'.

  CLEAR:  po_header,po_headerx,po_item,po_item[],po_itemx,po_itemx[],po_schedule,po_schedule[],
   t_item,t_item[],i_return,i_return[],t_return,t_return[].
  REFRESH:gt_extensionin.

  po_number = gs_head-ebeln.

  "***创建采购订单

  po_header-po_number   =  gs_head-ebeln.
  po_header-doc_type    =  gs_head-bsart.
  po_header-currency    =  gs_head-waers.
  po_header-comp_code   =  gs_head-bukrs.
  po_header-purch_org   =  gs_head-ekorg.
  po_header-pur_group   =  gs_head-ekgrp.

  po_header-vendor      =  gs_head-lifnr.
  po_header-our_ref     =  ''.
  po_header-doc_date = gs_head-bldat.

  po_header-item_intvl = 0.

  po_headerx-po_number   = 'X'.
  po_headerx-doc_type   = 'X'.
  po_headerx-currency = 'X'.
  " PO_HEADERX-ITEM_INTVL = 'X'.
  po_headerx-vendor     = 'X'.
  "PO_HEADERX-PMNTTRMS   = 'X'.
  po_headerx-comp_code  = 'X'.
  po_headerx-purch_org  = 'X'.
  po_headerx-pur_group  = 'X'.
  po_headerx-our_ref     =  'X'.
  po_headerx-doc_date = 'X'.
  po_headerx-pmnttrms = 'X'.
  po_headerx-collect_no = 'X'.
  po_headerx-sales_pers = 'X'.
  po_headerx-ref_1 = 'X'.

  po_headerx-item_intvl = 'X'.

*  PERFORM frm_get_text.
*  IF gt_tline[] IS NOT INITIAL.
*    LOOP AT gt_tline INTO DATA(ls_tline).
*      MOVE-CORRESPONDING ls_tline TO po_textheader.
*      po_textheader-po_number = po_header-po_number.
*      po_textheader-text_form = ls_tline-tdformat.
*      po_textheader-text_line = ls_tline-tdline.
*      po_textheader-text_id = 'F01'.
*      APPEND po_textheader.
*    ENDLOOP.
*  ENDIF.

  DATA:ls_kposn TYPE kposn.

  CALL FUNCTION 'BAPI_PO_GETDETAIL1'
    EXPORTING
      purchaseorder = po_number
    TABLES
      pocond        = po_condy.


  LOOP AT gt_item_po.

    IF gt_item_po-loekz IS NOT INITIAL.
      po_item-delete_ind = 'X'.
      po_itemx-delete_ind = 'X'.
    ENDIF.
    po_item-po_item    = gt_item_po-ebelp.
    po_itemx-po_item   = gt_item_po-ebelp.

    po_item-vend_mat   = gt_item_po-idnlf.
    po_itemx-vend_mat  = 'X'.

    po_item-material_long  = gt_item_po-matnr.
    po_itemx-material_long = 'X'.

*    po_item-plant      = gt_item-werks.
    po_item-plant      = gs_head-werks.
    po_itemx-plant     = 'X'.

    po_item-stge_loc   = gt_item_po-lgort.
    po_itemx-stge_loc   = 'X'.

    po_item-quantity   = gt_item_po-menge.
    po_itemx-quantity   = 'X'.

    po_item-item_cat   = ''.
    po_itemx-item_cat   = 'X'.

    po_item-over_dlv_tol = gt_item_po-uebto.
    po_itemx-over_dlv_tol = 'X'.

*    PO_ITEM-UNDER_DLV_TOL = GT_ZMMS0041-UNTTO.
*    PO_ITEMX-OVER_DLV_TOL = 'X'.


*    po_item-net_price  = gt_item_po-price.
*    PO_ITEMX-NET_PRICE   = 'X'.

    po_item-price_unit = gt_item-peinh.
    po_itemx-price_unit   = 'X'.

    po_item-tax_code = gs_head-mwskz.
    po_itemx-tax_code   = 'X'.

    po_item-stk_seg_long = gt_item_po-sgt_scat.
    po_itemx-stk_seg_long = 'X'.

    po_item-preq_name = gs_head-afnam.
    po_item-preq_name = 'X'.


    IF gs_head-bustyp = 'PO003' ." 半成品委外采购合同
      po_account-po_item = gt_item_po-ebelp.
      po_accountx-po_item = gt_item_po-ebelp.
      CASE gs_head-bukrs.
        WHEN '1000'.
          po_account-costcenter = '0010000130'.
        WHEN '1010'.
          po_account-costcenter = '0010100030'.
        WHEN '3000'.
          po_account-costcenter = '0030000020'.
        WHEN '5000'.
          po_account-costcenter = '0050000020'.
        WHEN OTHERS.
      ENDCASE.

      po_item-po_unit   = gt_item_po-meins.
      po_itemx-po_unit   = 'X'.

      po_accountx-costcenter = 'X'.

      po_item-material   = gt_item_po-matnr.
      po_item-item_cat = 'L'."项目类别-外协加工
      po_itemx-item_cat = 'X'.


*      PERFORM frm_get_component TABLES pocomponents pocomponentsx  USING gt_item_po.
      PERFORM frm_get_component_cpt TABLES pocomponents pocomponentsx USING gt_item_po.
      IF pocomponents[] IS INITIAL.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 121 '' '' '' ''."没有找到原材料，请确认BOM！
        RETURN.
      ENDIF.
      DATA: lv_charg TYPE charg_d.
      PERFORM frm_get_po_charg USING gt_item_po CHANGING lv_charg.
      po_item-batch         = lv_charg.
      po_itemx-batch        = 'X'.

      po_account-orderid    = gt_item_po-aufnr.
      po_accountx-orderid   = 'X'.
      po_account-quantity   = gt_item_po-menge.
      po_accountx-quantity  = 'X'.
      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.

    ELSEIF gs_head-knttp = 'F' ." 工序委外采购合同
      po_account-po_item = gt_item_po-ebelp.
      po_accountx-po_item = gt_item_po-ebelp.
      CASE gs_head-bukrs.
        WHEN '1000'.
          po_account-costcenter = '0010000130'.
        WHEN '1010'.
          po_account-costcenter = '0010100030'.
        WHEN '3000'.
          po_account-costcenter = '0030000020'.
        WHEN '5000'.
          po_account-costcenter = '0050000020'.
        WHEN OTHERS.
      ENDCASE.
      CLEAR po_item-material_long.
      CLEAR po_itemx-material_long.
      po_item-short_text  = gt_item_po-maktx_zh.
      po_itemx-short_text  = 'X'.
      po_item-po_unit   = gt_item_po-meins.
      po_itemx-po_unit   = 'X'.

      po_accountx-costcenter = 'X'.
      po_item-acctasscat = 'F'.
      po_itemx-acctasscat = 'X'.
      po_item-matl_group = 'Z200'.
      po_itemx-matl_group = 'X'.
      po_account-orderid = gt_item_po-aufnr.
      po_accountx-orderid = 'X'.
      po_account-quantity = gt_item_po-menge.
      po_accountx-quantity = 'X'.

      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.

    ELSEIF  gt_item_po-vbeln_va IS NOT INITIAL.
      po_item-acctasscat = 'M'.
      po_itemx-acctasscat = 'X'.

      po_account-po_item = gt_item_po-ebelp.
      po_accountx-po_item = gt_item_po-ebelp.

      po_account-sd_doc = gt_item_po-vbeln_va.
      po_accountx-sd_doc = 'X'.

      po_account-itm_number = gt_item_po-posnr_va.
      po_accountx-itm_number = 'X'.

      po_account-quantity = gt_item_po-menge.
      po_accountx-quantity = 'X'.

      po_account-gl_account = '1406000000'.
      po_accountx-gl_account = 'X'.

      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.
    ENDIF.


    IF gt_item_po-price IS INITIAL.
      po_item-ir_ind = ''.
      po_itemx-ir_ind = 'X'.
    ELSE.
      po_item-ir_ind = 'X'.
      po_itemx-ir_ind = 'X'.

      LOOP AT po_condy WHERE itm_number = ls_kposn..

        READ TABLE gt_item_po WITH KEY ebelp = po_condy-itm_number.

        IF po_condy-cond_type = 'Z100' .

          MOVE po_condy TO po_cond.
          po_cond-cond_value = gt_item_po-price / gt_item_po-zmm_tran_rate.
          po_cond-cond_p_unt = gt_item_po-peinh.
          po_cond-change_id = 'U'.
          APPEND po_cond.

          po_condx-condition_no = po_cond-condition_no.
          po_condx-itm_number   = po_cond-itm_number.
          po_condx-cond_st_no   = po_cond-cond_st_no.
          po_condx-cond_p_unt = 'X'.
          po_condx-cond_value   = 'X'.
          po_condx-change_id = 'X'.
          APPEND po_condx.
        ENDIF.

        IF po_condy-cond_type = 'Z101'.

          MOVE po_condy TO po_cond.
          po_cond-cond_value = gt_item_po-cost_amount.
          po_cond-change_id = 'U'.
          APPEND po_cond.

          po_condx-condition_no = po_cond-condition_no.
          po_condx-itm_number   = po_cond-itm_number.
          po_condx-cond_st_no   = po_cond-cond_st_no.
          po_condx-cond_value   = 'X'.
          po_condx-change_id = 'X'.
          APPEND po_condx.
        ENDIF.
      ENDLOOP.

      IF sy-subrc NE 0.
        po_cond-itm_number = gt_item_po-ebelp.
        po_cond-cond_type = 'Z100'.
        po_cond-cond_value = gt_item_po-price / 10 .
        po_cond-cond_p_unt = gt_item_po-peinh.
        po_cond-change_id = 'U'.
        APPEND po_cond.
        CLEAR po_cond.

        po_condx-itm_number   = gt_item_po-ebelp.
        po_condx-cond_type   = 'X'.
        po_condx-cond_value   = 'X'.
        po_condx-cond_p_unt   = 'X'.
        po_condx-change_id = 'X'.
        APPEND po_condx.
        CLEAR po_condx.


        po_cond-itm_number = gt_item_po-ebelp.
        po_cond-cond_type = 'Z101'.
        po_cond-cond_value = gt_item_po-cost_amount / 10 .
        po_cond-cond_p_unt = gt_item_po-peinh.
        po_cond-change_id = 'U'.
        APPEND po_cond.
        CLEAR po_cond.

        po_condx-itm_number   = gt_item_po-ebelp.
        po_condx-cond_type   = 'X'.
        po_condx-cond_value   = 'X'.
        po_condx-cond_p_unt   = 'X'.
        po_condx-change_id = 'X'.
        APPEND po_condx.
        CLEAR po_condx.

      ENDIF.
    ENDIF.

    APPEND po_item.
    APPEND po_itemx.
    CLEAR  po_item.
    CLEAR  po_itemx.

    po_schedule-po_item = gt_item_po-ebelp.
    po_schedulex-po_item = gt_item_po-ebelp.

    po_schedule-sched_line = 1.
    po_schedulex-sched_line = 1.

    po_schedulex-po_itemx = 'X'.

    po_schedule-del_datcat_ext = 'D'.
    po_schedulex-del_datcat_ext = 'X'.

    po_schedule-delivery_date = gt_item_po-eeind.
    po_schedulex-delivery_date = 'X'.

    po_schedule-quantity = gt_item_po-menge.
    po_schedulex-quantity = 'X'.


    APPEND po_schedule.
    APPEND po_schedulex.
    CLEAR  po_schedule.
    CLEAR  po_schedulex.

    ls_bapi_te_mepoitem-po_item = gt_item_po-ebelp.
    ls_bapi_te_mepoitem-zzpino = gt_item_po-zzpino.
    ls_bapi_te_mepoitem-zppdhd = gt_item_po-zppdhd.
*    LS_BAPI_TE_MEPOITEM-ZMATNR = GT_ITEM_PO-ZMATNR.
    ls_bapi_te_mepoitem-zkunnr_mat = gt_item_po-zkunnr_mat.
    ls_bapi_te_mepoitem-zvbeln = gt_item_po-vbeln_va.
    ls_bapi_te_mepoitem-zposnr = gt_item_po-posnr_va.
    ls_bapi_te_mepoitem-zkunnr = gt_item_po-kunnr.
    ls_bapi_te_mepoitem-zcolor = gt_item_po-zcolor.
    ls_bapi_te_mepoitem-zcolor_text = gt_item_po-zcolor_text.
    ls_bapi_te_mepoitem-zsize = gt_item_po-zsize.
    ls_bapi_te_mepoitem-zppflag = gt_item_po-zppflag.

    gs_extensionin-structure =   'BAPI_TE_MEPOITEM'.
    MOVE ls_bapi_te_mepoitem TO gs_extensionin-valuepart1.
    APPEND gs_extensionin TO gt_extensionin.
    CLEAR gs_extensionin.

    ls_bapi_te_mepoitemx-po_item = gt_item_po-ebelp.
    ls_bapi_te_mepoitemx-zzpino = 'X'.
    ls_bapi_te_mepoitemx-zppdhd = 'X'.
*    LS_BAPI_TE_MEPOITEMX-ZMATNR = 'X'.
    ls_bapi_te_mepoitemx-zkunnr_mat = 'X'.
    ls_bapi_te_mepoitemx-zvbeln = 'X'.
    ls_bapi_te_mepoitemx-zposnr = 'X'.
    ls_bapi_te_mepoitemx-zkunnr = 'X'.
    ls_bapi_te_mepoitemx-zcolor = 'X'.
    ls_bapi_te_mepoitemx-zcolor_text = 'X'.
    ls_bapi_te_mepoitemx-zsize = 'X'.
    ls_bapi_te_mepoitemx-zppflag = 'X'.

    gs_extensionin-structure =   'BAPI_TE_MEPOITEMX'.
    MOVE ls_bapi_te_mepoitemx TO gs_extensionin-valuepart1.
    APPEND gs_extensionin TO gt_extensionin.
    CLEAR gs_extensionin.

  ENDLOOP.

  CLEAR: gs_extensionin,ls_bapi_te_mepoitem,ls_bapi_te_mepoitemx.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = po_number
      poheader      = po_header
      poheaderx     = po_headerx
*     NO_PRICE_FROM_PO = 'X'
* IMPORTING
*     EXPHEADER     =
*     EXPPOEXPIMPHEADER            =
    TABLES
      return        = i_return
      poitem        = po_item
      poitemx       = po_itemx
      potextheader  = po_textheader
      poschedule    = po_schedule
      poschedulex   = po_schedulex
      poaccount     = po_account
      poaccountx    = po_accountx
      pocond        = po_cond
      pocondx       = po_condx
      potextitem    = t_item
      pocomponents  = pocomponents[] "add by at-yuxs 20220328 委外采购订单组件
      pocomponentsx = pocomponentsx[] "add by at-yuxs 20220328 委外采购订单组件
      extensionin   = gt_extensionin.

  READ TABLE i_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    IF po_number IS NOT INITIAL.
      gs_head-ebeln = po_number.
    ENDIF.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-ebeln = po_number.
    ENDLOOP.

    LOOP AT gt_item_po ASSIGNING <gs_item_po>.
      <gs_item_po>-ebeln = po_number.
    ENDLOOP.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.


  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.

ENDFORM.


FORM frm_poc_post.

  DATA: po_header            TYPE bapimepoheader,
        po_headerx           TYPE bapimepoheaderx,

        po_item              TYPE TABLE OF bapimepoitem WITH HEADER LINE,
        po_itemx             TYPE TABLE OF bapimepoitemx WITH HEADER LINE,

        po_con_h             TYPE TABLE OF bapimepocondheader  WITH HEADER LINE,
        po_con_hx            TYPE TABLE OF bapimepocondheaderx WITH HEADER LINE,

        po_con_i             TYPE TABLE OF bapimepocond WITH HEADER LINE,
        po_con_ix            TYPE TABLE OF bapimepocondx WITH HEADER LINE,

        po_schedule          TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
        po_schedulex         TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,

        po_account           TYPE TABLE OF bapimepoaccount WITH HEADER LINE,
        po_accountx          TYPE TABLE OF  bapimepoaccountx WITH HEADER LINE,

        po_condy             TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_cond              TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_condx             TYPE TABLE OF  bapimepocondx WITH HEADER LINE,

        po_textheader        TYPE TABLE OF bapimepotextheader WITH HEADER LINE,

        po_number            LIKE bapimepoheader-po_number,
        i_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,

        t_item               TYPE TABLE OF bapimepotext WITH HEADER LINE,


        ls_bapi_te_mepoitem  TYPE bapi_te_mepoitem,
        ls_bapi_te_mepoitemx TYPE bapi_te_mepoitemx,
        gt_extensionin       TYPE TABLE OF bapiparex WITH HEADER LINE,
        gs_extensionin       TYPE bapiparex.


  PERFORM process USING TEXT-045."'正在修改采购订单……'.

  CLEAR:  po_header,po_headerx,po_item,po_item[],po_itemx,po_itemx[],po_schedule,po_schedule[],
   t_item,t_item[],i_return,i_return[],t_return,t_return[].
  REFRESH:gt_extensionin.


  po_number = gs_head-ebeln.
  po_header-po_number = gs_head-ebeln.
  po_headerx-po_number = gs_head-ebeln.
  po_header-item_intvl = 1.

  po_headerx-item_intvl = 'X'.

  SELECT SINGLE * FROM zafo_head
    INTO @DATA(ls_head)
    WHERE afono = @gs_head-ebeln.

  SELECT * FROM zafo_item
    INTO TABLE @DATA(lt_item)
    WHERE afono = @ls_head-afono
    AND del_flag = ''.

  SELECT * FROM zafo_item_po
    INTO TABLE @DATA(lt_item_po)
    WHERE afono = @ls_head-afono
    AND loekz = ''.

  DATA:ls_afonr TYPE zafonr.
  DATA:ls_ebelp TYPE ebelp.
  DATA:lv_amount TYPE zafo_amount.
  DATA:lsc_item_po TYPE zafo_item_po.

  CLEAR ls_afonr.
  CLEAR ls_ebelp.
  LOOP AT gt_item .
    CASE gt_item-change_flag.
      WHEN 'M'.
        READ TABLE lt_item ASSIGNING FIELD-SYMBOL(<lsc_item>)
                     WITH KEY zzpino = gt_item-zzpino
                              matnr = gt_item-matnr
                              zcolor = gt_item-zcolor
                              zsize = gt_item-zsize
                              znorms = gt_item-znorms
                              zppflag = gt_item-zppflag.
        IF sy-subrc EQ 0.
          <lsc_item>-menge = <lsc_item>-menge + gt_item-menge.
          <lsc_item>-menge_cg = <lsc_item>-menge_cg + gt_item-menge_cg.
          <lsc_item>-menge_zj = <lsc_item>-menge_zj + gt_item-menge_zj.
          <lsc_item>-eeind = gt_item-eeind.
          <lsc_item>-price = gt_item-price.
          <lsc_item>-price_long = gt_item-price_long.
          <lsc_item>-peinh = gt_item-peinh.
          <lsc_item>-amount = ( <lsc_item>-menge_cg + <lsc_item>-menge_zj ) * <lsc_item>-price_long.
          IF <lsc_item>-menge <= 0.
            <lsc_item>-del_flag = 'X'.
          ENDIF.
          LOOP AT gt_item_po WHERE afonr = gt_item-afonr.
            READ TABLE lt_item_po INTO lsc_item_po  WITH KEY zzpino  = gt_item_po-zzpino
                                                         zppdhd  = gt_item_po-zppdhd
                                                         matnr   = gt_item_po-matnr
                                                         zcolor  = gt_item_po-zcolor
                                                         zsize   = gt_item_po-zsize
                                                         znorms  = gt_item_po-znorms
                                                         zppflag = gt_item_po-zppflag.
            IF sy-subrc EQ 0.
              lsc_item_po-menge = lsc_item_po-menge + gt_item_po-menge.
              lsc_item_po-price = gt_item_po-price.
              lsc_item_po-peinh = gt_item_po-peinh.
              lsc_item_po-amount = lsc_item_po-amount + gt_item_po-amount.
              IF lsc_item_po-menge <= 0.
                lsc_item_po-menge  = lsc_item_po-ktmng.
                lsc_item_po-loekz = 'X'.
              ENDIF.
              APPEND lsc_item_po TO gt_item_po_modify.
            ELSE.
              PERFORM frm_add_msg USING 'E' 'ZAFO' 082 '' '' '' ''."变更行不完整，请检查数据
            ENDIF.
          ENDLOOP.
        ELSE.
          PERFORM frm_add_msg USING 'E' 'ZAFO' 083 '' '' '' ''."变更行不存在，请检查数据
        ENDIF.
      WHEN 'I'.
        READ TABLE lt_item ASSIGNING <lsc_item>
                                  WITH KEY zzpino  = gt_item-zzpino
                                           matnr   = gt_item-matnr
                                           zcolor  = gt_item-zcolor
                                           zsize   = gt_item-zsize
                                           znorms  = gt_item-znorms
                                           zppflag = gt_item-zppflag.
        IF sy-subrc EQ 0.
          <lsc_item>-menge    = <lsc_item>-menge + gt_item-menge.
          <lsc_item>-menge_cg = <lsc_item>-menge_cg + gt_item-menge_cg.
          <lsc_item>-menge_zj = <lsc_item>-menge_zj + gt_item-menge_zj.
          <lsc_item>-amount   = ( <lsc_item>-menge_cg + <lsc_item>-menge_zj ) * <lsc_item>-price_long.
          <lsc_item>-eeind    = gt_item-eeind.

          LOOP AT gt_item_po WHERE afonr = gt_item-afonr.
            MOVE-CORRESPONDING gt_item_po TO lsc_item_po.

            IF ls_ebelp IS INITIAL.
              SELECT MAX( ebelp ) INTO @ls_ebelp
                  FROM ekpo WHERE ebeln = @gs_head-ebeln.
            ENDIF.
            ADD 1 TO ls_ebelp.
            lsc_item_po-afono = gs_head-ebeln.
            lsc_item_po-ebeln = gs_head-ebeln.
            lsc_item_po-ebelp = ls_ebelp.
            lsc_item_po-afonr = <lsc_item>-afonr.

            APPEND lsc_item_po TO gt_item_po_modify.
            CLEAR lsc_item_po.
          ENDLOOP.

        ELSE.

          APPEND INITIAL LINE TO lt_item ASSIGNING <lsc_item>.

          MOVE-CORRESPONDING gt_item TO <lsc_item>.

          CLEAR <lsc_item>-change_flag .
          <lsc_item>-afono = gs_head-ebeln.
          IF ls_afonr IS INITIAL.
            SELECT MAX( afonr ) INTO @ls_afonr
              FROM zafo_item WHERE afono = @<lsc_item>-afono.
          ENDIF.
          ADD 1 TO ls_afonr.
          <lsc_item>-afonr = ls_afonr.

          LOOP AT gt_item_po WHERE afonr = gt_item-afonr.
            MOVE-CORRESPONDING gt_item_po TO lsc_item_po.

            IF ls_ebelp IS INITIAL.
              SELECT MAX( ebelp ) INTO @ls_ebelp
                  FROM ekpo WHERE ebeln = @gs_head-ebeln.
            ENDIF.
            ADD 1 TO ls_ebelp.
            lsc_item_po-afono = gs_head-ebeln.
            lsc_item_po-ebeln = gs_head-ebeln.
            lsc_item_po-ebelp = ls_ebelp.
            lsc_item_po-afonr = ls_afonr.

            APPEND lsc_item_po TO gt_item_po_modify.
            CLEAR lsc_item_po.

          ENDLOOP.
        ENDIF.

      WHEN 'D'.
        READ TABLE lt_item ASSIGNING <lsc_item>
                              WITH KEY zzpino  = gt_item-zzpino
                                       matnr   = gt_item-matnr
                                       zcolor  = gt_item-zcolor
                                       zsize   = gt_item-zsize
                                       znorms  = gt_item-znorms
                                       zppflag = gt_item-zppflag.
        IF sy-subrc EQ 0.
          <lsc_item>-menge = <lsc_item>-menge + gt_item-menge.
          <lsc_item>-menge_cg = <lsc_item>-menge_cg + gt_item-menge_cg.
          <lsc_item>-menge_zj = <lsc_item>-menge_zj + gt_item-menge_zj.
          <lsc_item>-amount = ( <lsc_item>-menge_cg + <lsc_item>-menge_zj ) * <lsc_item>-price_long.
          <lsc_item>-eeind = gt_item-eeind.

          IF <lsc_item>-menge <= 0.
            <lsc_item>-del_flag = 'X'.
          ENDIF.

          LOOP AT gt_item_po WHERE afonr = gt_item-afonr.
            READ TABLE lt_item_po INTO lsc_item_po WITH KEY zzpino = gt_item_po-zzpino
                                                            zppdhd = gt_item_po-zppdhd
                                                            matnr = gt_item_po-matnr
                                                            zcolor = gt_item_po-zcolor
                                                            zsize = gt_item_po-zsize
                                                            znorms = gt_item_po-znorms
                                                            zppflag = gt_item_po-zppflag.
            IF sy-subrc EQ 0.
              lsc_item_po-loekz = 'X'.
              APPEND lsc_item_po TO gt_item_po_modify.
              CLEAR lsc_item_po.
            ELSE.
              PERFORM frm_add_msg USING 'W'  'ZAFO' 084 '' '' '' ''."删除行不完整,请检查数据
            ENDIF.
          ENDLOOP.
        ELSE.
          PERFORM frm_add_msg USING 'W' 'ZAFO'  085 '' '' '' ''."删除行不存在,请检查数据
        ENDIF.
    ENDCASE.
  ENDLOOP.

  CHECK g_error IS INITIAL.

  CLEAR ls_head-menge.
  CLEAR ls_head-amount.

  LOOP AT lt_item INTO DATA(ls_item).
    ls_head-menge = ls_head-menge + ls_item-menge_cg + ls_item-menge_zj.
    ls_head-amount = ls_head-amount + ls_item-amount.
  ENDLOOP.

  ls_head-amount = ls_head-amount + ls_head-cost_amount.

  ADD 1 TO ls_head-edition.

  UPDATE zafo_head SET menge = ls_head-menge
                     amount = ls_head-amount
                     edition = ls_head-edition
                     WHERE afono = gs_head-ebeln.

  gs_head_modify_ref = ls_head.


  DATA:lt1_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:lt1_item_po TYPE TABLE OF zafo_sitem_po WITH HEADER LINE.

  lt1_item[] = gt_item[].
  lt1_item_po[] = gt_item_po[].

  REFRESH gt_item.
  REFRESH gt_item_po.

  LOOP AT lt_item ASSIGNING <lsc_item>.
    APPEND <lsc_item> TO gt_item_modify.

    CLEAR gt_item.
    MOVE-CORRESPONDING <lsc_item> TO gt_item.
    APPEND gt_item.
  ENDLOOP.

  LOOP AT gt_item_po_modify.
    MOVE-CORRESPONDING gt_item_po_modify TO gt_item_po.
    APPEND gt_item_po.
    CLEAR gt_item_po.
    po_item-po_item    = gt_item_po_modify-ebelp.
    po_itemx-po_item   = gt_item_po_modify-ebelp.

    IF gt_item_po_modify-loekz = 'X'.
      po_item-delete_ind = 'L'.
      po_itemx-delete_ind = 'X'.
    ENDIF.


    po_item-vend_mat   = gt_item_po_modify-idnlf.
    po_itemx-vend_mat  = 'X'.

    po_item-material_long      = gt_item_po_modify-matnr.
    po_itemx-material_long      = 'X'.

    po_item-plant      = gt_item_po_modify-werks.
    po_itemx-plant     = 'X'.

    po_item-quantity   = gt_item_po_modify-menge.
    po_itemx-quantity   = 'X'.

    po_item-item_cat   = ''.
    po_itemx-item_cat   = 'X'.

    po_item-over_dlv_tol = gt_item_po_modify-uebto.
    po_itemx-over_dlv_tol = 'X'.

    po_item-over_dlv_tol = gt_item_po_modify-uebto.
    po_itemx-over_dlv_tol = 'X'.

    po_item-preq_name = ls_head-afnam.
    po_item-preq_name = 'X'.

    po_item-orderpr_un = gt_item_po_modify-bstme.
    po_itemx-orderpr_un = 'X'.

    po_item-conv_den1 = gt_item_po_modify-zmm_tran_rate.
    po_itemx-conv_den1 = 'X' .

    po_item-conv_num1 = 1.
    po_itemx-conv_num1 = 'X'.

    po_item-price_unit = gt_item_po_modify-peinh.
    po_itemx-price_unit   = 'X'.

    po_item-tax_code = ls_head-mwskz.
    po_itemx-tax_code   = 'X'.

    IF gt_item_po_modify-vbeln_va IS NOT INITIAL.
      po_item-acctasscat = 'M'.
      po_itemx-acctasscat = 'X'.

      po_account-po_item = gt_item_po_modify-ebelp.
      po_accountx-po_item = gt_item_po_modify-ebelp.

      po_account-sd_doc = gt_item_po_modify-vbeln_va.
      po_accountx-sd_doc = 'X'.

      po_account-itm_number = gt_item_po_modify-posnr_va.
      po_accountx-itm_number = 'X'.

      po_account-quantity = gt_item_po_modify-menge.
      po_accountx-quantity = 'X'.

      po_account-gl_account = '1406000000'.
      po_accountx-gl_account = 'X'.

      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.
    ENDIF.


    IF gt_item_po_modify-price IS INITIAL.
      po_item-ir_ind = ''.
      po_itemx-ir_ind = 'X'.
    ELSE.
      po_item-ir_ind = 'X'.
      po_itemx-ir_ind = 'X'.

      po_cond-itm_number = gt_item_po_modify-ebelp.
      po_cond-cond_type = 'Z100'.
      po_cond-cond_value = gt_item_po_modify-price / 10 ."/ gt_item_po-zmm_tran_rate.
      po_cond-cond_p_unt = gt_item_po_modify-peinh.
      po_cond-change_id = 'U'.
      APPEND po_cond.
      CLEAR po_cond.

      po_condx-itm_number   = po_cond-itm_number.
      po_condx-cond_type   = 'X'.
      po_condx-cond_value   = 'X'.
      po_condx-cond_p_unt   = 'X'.
      po_condx-change_id = 'X'.
      APPEND po_condx.
      CLEAR po_condx.

      po_cond-itm_number = gt_item_po_modify-ebelp.
      po_cond-cond_type = 'Z101'.
      po_cond-cond_value = gt_item_po_modify-cost_amount / 10.
      po_cond-change_id = 'U'.
      APPEND po_cond.
      CLEAR po_cond.

      po_condx-itm_number   = po_cond-itm_number.
      po_condx-cond_value   = 'X'.
      po_condx-change_id = 'X'.
      APPEND po_condx.
      CLEAR po_condx.
    ENDIF.

    APPEND po_item.
    APPEND po_itemx.
    CLEAR  po_item.
    CLEAR  po_itemx.

    po_schedule-po_item = gt_item_po_modify-ebelp.
    po_schedulex-po_item = gt_item_po_modify-ebelp.

    po_schedule-sched_line = 1.
    po_schedulex-sched_line = 1.

    po_schedulex-po_itemx = 'X'.

    po_schedule-del_datcat_ext = 'D'.
    po_schedulex-del_datcat_ext = 'X'.

    po_schedule-delivery_date = gt_item_po_modify-eeind.
    po_schedulex-delivery_date = 'X'.

    po_schedule-quantity = gt_item_po_modify-menge.
    po_schedulex-quantity = 'X'.

    APPEND po_schedule.
    APPEND po_schedulex.
    CLEAR  po_schedule.
    CLEAR  po_schedulex.

  ENDLOOP.

  g_kzfae = 2.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = po_number
      poheader      = po_header
      poheaderx     = po_headerx
*     NO_PRICE_FROM_PO = 'X'
* IMPORTING
*     EXPHEADER     =
*     EXPPOEXPIMPHEADER            =
    TABLES
      return        = i_return
      poitem        = po_item
      poitemx       = po_itemx
*     potextheader  = po_textheader
      poschedule    = po_schedule
      poschedulex   = po_schedulex
      poaccount     = po_account
      poaccountx    = po_accountx
      pocond        = po_cond
      pocondx       = po_condx.
*     potextitem    = t_item
*     extensionin   = gt_extensionin.

  READ TABLE i_return WITH KEY type = 'E'.

  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  gt_item[] = lt1_item[].
  gt_item_po[] = lt1_item_po[].

  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.

*  LOOP AT gt_item ASSIGNING <gs_item>.
*    <gs_item>-ebeln = po_number.
*  ENDLOOP.
*
*  LOOP AT gt_item_po ASSIGNING <gs_item_po>.
*    <gs_item_po>-ebeln = po_number.
*  ENDLOOP.

ENDFORM.


FORM frm_pop_post.

  DATA: po_header            TYPE bapimepoheader,
        po_headerx           TYPE bapimepoheaderx,

        po_item              TYPE TABLE OF bapimepoitem WITH HEADER LINE,
        po_itemx             TYPE TABLE OF bapimepoitemx WITH HEADER LINE,

        po_con_h             TYPE TABLE OF bapimepocondheader  WITH HEADER LINE,
        po_con_hx            TYPE TABLE OF bapimepocondheaderx WITH HEADER LINE,

        po_con_i             TYPE TABLE OF bapimepocond WITH HEADER LINE,
        po_con_ix            TYPE TABLE OF bapimepocondx WITH HEADER LINE,

        po_schedule          TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
        po_schedulex         TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,

        po_account           TYPE TABLE OF bapimepoaccount WITH HEADER LINE,
        po_accountx          TYPE TABLE OF  bapimepoaccountx WITH HEADER LINE,

        po_condy             TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_cond              TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_condx             TYPE TABLE OF  bapimepocondx WITH HEADER LINE,

        po_textheader        TYPE TABLE OF bapimepotextheader WITH HEADER LINE,

        po_number            LIKE bapimepoheader-po_number,
        i_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,

        t_item               TYPE TABLE OF bapimepotext WITH HEADER LINE,


        ls_bapi_te_mepoitem  TYPE bapi_te_mepoitem,
        ls_bapi_te_mepoitemx TYPE bapi_te_mepoitemx,
        gt_extensionin       TYPE TABLE OF bapiparex WITH HEADER LINE,
        gs_extensionin       TYPE bapiparex.


  PERFORM process USING TEXT-045."'正在修改采购订单……'.

  CLEAR:  po_header,po_headerx,po_item,po_item[],po_itemx,po_itemx[],po_schedule,po_schedule[],
   t_item,t_item[],i_return,i_return[],t_return,t_return[].
  REFRESH:gt_extensionin.


  po_number = gs_head-ebeln.

  SELECT SINGLE * FROM zafo_head
    INTO @DATA(ls_head_dest)
    WHERE afono = @gs_head-ebeln.

  po_header-po_number = gs_head-ebeln.
  po_headerx-po_number = gs_head-ebeln.

  po_header-item_intvl = 0.
  po_headerx-item_intvl = 'X'.


  SELECT * FROM zafo_item
    INTO TABLE @DATA(lt_item_dest)
    WHERE afono = @ls_head_dest-afono
    AND del_flag = ''.

  SELECT * FROM zafo_item_po
    INTO TABLE @DATA(lt_item_po_dest)
    WHERE afono = @ls_head_dest-afono
    AND loekz = ''.

  DATA:ls_afonr TYPE zafonr.
  DATA:ls_ebelp TYPE ebelp.
  CLEAR ls_afonr.
  CLEAR ls_ebelp.
  LOOP AT gt_item .
    READ TABLE lt_item_dest ASSIGNING FIELD-SYMBOL(<lsc_item>)
                 WITH KEY zzpino = gt_item-zzpino
                          matnr = gt_item-matnr
                          zcolor = gt_item-zcolor
                          zsize = gt_item-zsize
                          znorms = gt_item-znorms
                          zppflag = gt_item-zppflag.
    IF sy-subrc EQ 0.
      <lsc_item>-eeind = gt_item-eeind.
      <lsc_item>-price = gt_item-price.
      <lsc_item>-price_long = gt_item-price_long.
      <lsc_item>-peinh = gt_item-peinh.
      <lsc_item>-amount = gt_item-amount.



      LOOP AT gt_item_po WHERE afonr = gt_item-afonr.
        READ TABLE lt_item_po_dest INTO DATA(lsc_item_po) WITH KEY zzpino = gt_item_po-zzpino
                                                              zppdhd = gt_item_po-zppdhd
                                                              matnr = gt_item_po-matnr
                                                              zcolor = gt_item_po-zcolor
                                                              zsize = gt_item_po-zsize
                                                              znorms = gt_item_po-znorms
                                                              zppflag = gt_item_po-zppflag.
        IF sy-subrc EQ 0.
          lsc_item_po-price = gt_item_po-price.
          lsc_item_po-cost_amount = gt_item_po-cost_amount.
          lsc_item_po-peinh = gt_item_po-peinh.
          lsc_item_po-amount = gt_item_po-amount.
          APPEND lsc_item_po TO gt_item_po_modify.
          CLEAR lsc_item_po.
        ELSE.
          PERFORM frm_add_msg USING 'E' 'ZAFO' 082 '' '' '' ''."变更行不完整，请检查数据
        ENDIF.
      ENDLOOP.
    ELSE.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 082 '' '' '' ''."变更行不存在，请检查数据
    ENDIF.

  ENDLOOP.

  CLEAR ls_head_dest-menge.
  CLEAR ls_head_dest-amount.
  CLEAR ls_head_dest-cost_amount.

  LOOP AT lt_item_dest INTO DATA(ls_item_dest).
    ls_head_dest-menge = ls_head_dest-menge + ls_item_dest-menge.
    ls_head_dest-amount = ls_head_dest-amount + ls_item_dest-amount.
  ENDLOOP.

  LOOP AT gt_item_po_modify INTO DATA(ls_item_po_modify).
    ls_head_dest-cost_amount = ls_item_po_modify-cost_amount .
  ENDLOOP.

  ADD 1 TO ls_head_dest-edition.

  UPDATE zafo_head SET menge = ls_head_dest-menge
                       amount = ls_head_dest-amount
                       cost_amount = ls_head_dest-cost_amount
                       edition = ls_head_dest-edition
                       WHERE afono = gs_head-ebeln.

  DELETE FROM zafo_item_cost WHERE afono = ls_head_dest-afono.

  MODIFY zafo_item_cost FROM TABLE gt_item_cost[].

  gs_head_modify_ref = ls_head_dest.

  DATA:lt1_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:lt1_item_po TYPE TABLE OF zafo_sitem_po WITH HEADER LINE.

  lt1_item[] = gt_item[].
  lt1_item_po[] = gt_item_po[].

  REFRESH gt_item.
  REFRESH gt_item_po.

  LOOP AT lt_item_dest ASSIGNING <lsc_item>.
    APPEND <lsc_item> TO gt_item_modify.
    CLEAR gt_item.
    MOVE-CORRESPONDING <lsc_item> TO gt_item.
    APPEND gt_item.
  ENDLOOP.

  LOOP AT gt_item_po_modify.
    MOVE-CORRESPONDING gt_item_po_modify TO gt_item_po.
    APPEND gt_item_po.
    CLEAR gt_item_po.
    po_item-po_item    = gt_item_po_modify-ebelp.
    po_itemx-po_item   = gt_item_po_modify-ebelp.

    IF gt_item_po_modify-loekz = 'X'.
      po_item-delete_ind = 'L'.
      po_itemx-delete_ind = 'X'.
    ENDIF.


    po_item-vend_mat   = gt_item_po_modify-idnlf.
    po_itemx-vend_mat  = 'X'.

    po_item-material_long      = gt_item_po_modify-matnr.
    po_itemx-material_long      = 'X'.

    po_item-plant      = gt_item_po_modify-werks.
    po_itemx-plant     = 'X'.

    po_item-quantity   = gt_item_po_modify-menge.
    po_itemx-quantity   = 'X'.

    po_item-item_cat   = ''.
    po_itemx-item_cat   = 'X'.

    po_item-over_dlv_tol = gt_item_po_modify-uebto.
    po_itemx-over_dlv_tol = 'X'.

    po_item-over_dlv_tol = gt_item_po_modify-uebto.
    po_itemx-over_dlv_tol = 'X'.

    po_item-preq_name = ls_head_dest-afnam.
    po_item-preq_name = 'X'.

    po_item-orderpr_un = gt_item_po_modify-bstme.
    po_itemx-orderpr_un = 'X'.

    po_item-conv_den1 = gt_item_po_modify-zmm_tran_rate.
    po_itemx-conv_den1 = 'X' .

    po_item-conv_num1 = 1.
    po_itemx-conv_num1 = 'X'.

    po_item-price_unit = gt_item_po_modify-peinh.
    po_itemx-price_unit   = 'X'.

    po_item-tax_code = ls_head_dest-mwskz.
    po_itemx-tax_code   = 'X'.

    IF gt_item_po_modify-vbeln_va IS NOT INITIAL.
      po_item-acctasscat = 'M'.
      po_itemx-acctasscat = 'X'.

      po_account-po_item = gt_item_po_modify-ebelp.
      po_accountx-po_item = gt_item_po_modify-ebelp.

      po_account-sd_doc = gt_item_po_modify-vbeln_va.
      po_accountx-sd_doc = 'X'.

      po_account-itm_number = gt_item_po_modify-posnr_va.
      po_accountx-itm_number = 'X'.

      po_account-quantity = gt_item_po_modify-menge.
      po_accountx-quantity = 'X'.

      po_account-gl_account = '1406000000'.
      po_accountx-gl_account = 'X'.

      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.
    ENDIF.


    IF gt_item_po_modify-price IS INITIAL.
      po_item-ir_ind = ''.
      po_itemx-ir_ind = 'X'.
    ELSE.
      po_item-ir_ind = 'X'.
      po_itemx-ir_ind = 'X'.

      po_cond-itm_number = gt_item_po_modify-ebelp.
      po_cond-cond_type = 'Z100'.
      po_cond-cond_value = gt_item_po_modify-price / 10 ."/ gt_item_po-zmm_tran_rate.
      po_cond-cond_p_unt = gt_item_po_modify-peinh.
      po_cond-change_id = 'U'.
      APPEND po_cond.
      CLEAR po_cond.

      po_condx-itm_number   = po_cond-itm_number.
      po_condx-cond_type   = 'X'.
      po_condx-cond_value   = 'X'.
      po_condx-cond_p_unt   = 'X'.
      po_condx-change_id = 'X'.
      APPEND po_condx.
      CLEAR po_condx.

      po_cond-itm_number = gt_item_po_modify-ebelp.
      po_cond-cond_type = 'Z101'.
      po_cond-cond_value = gt_item_po_modify-cost_amount / 10.
      po_cond-change_id = 'U'.
      APPEND po_cond.
      CLEAR po_cond.

      po_condx-itm_number   = po_cond-itm_number.
      po_condx-cond_value   = 'X'.
      po_condx-change_id = 'X'.
      APPEND po_condx.
      CLEAR po_condx.
    ENDIF.

    APPEND po_item.
    APPEND po_itemx.
    CLEAR  po_item.
    CLEAR  po_itemx.

    po_schedule-po_item = gt_item_po_modify-ebelp.
    po_schedulex-po_item = gt_item_po_modify-ebelp.

    po_schedule-sched_line = 1.
    po_schedulex-sched_line = 1.

    po_schedulex-po_itemx = 'X'.

    po_schedule-del_datcat_ext = 'D'.
    po_schedulex-del_datcat_ext = 'X'.

    po_schedule-delivery_date = gt_item_po_modify-eeind.
    po_schedulex-delivery_date = 'X'.

    po_schedule-quantity = gt_item_po_modify-menge.
    po_schedulex-quantity = 'X'.

    APPEND po_schedule.
    APPEND po_schedulex.
    CLEAR  po_schedule.
    CLEAR  po_schedulex.

  ENDLOOP.

  g_kzfae = 2.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = po_number
      poheader      = po_header
      poheaderx     = po_headerx
*     NO_PRICE_FROM_PO = 'X'
* IMPORTING
*     EXPHEADER     =
*     EXPPOEXPIMPHEADER            =
    TABLES
      return        = i_return
      poitem        = po_item
      poitemx       = po_itemx
*     potextheader  = po_textheader
      poschedule    = po_schedule
      poschedulex   = po_schedulex
      poaccount     = po_account
      poaccountx    = po_accountx
      pocond        = po_cond
      pocondx       = po_condx.
*     potextitem    = t_item
*     extensionin   = gt_extensionin.

  READ TABLE i_return WITH KEY type = 'E'.

  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  gt_item[]    = lt1_item[].
  gt_item_po[] = lt1_item_po[].

  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.

ENDFORM.


FORM frm_pr_create.

  DATA: header  LIKE  bapimereqheader,
        headerx LIKE  bapimereqheaderx.
  DATA: return LIKE TABLE OF   bapiret2 WITH HEADER LINE,
        item   LIKE TABLE OF bapimereqitemimp WITH HEADER LINE,
        itemx  LIKE TABLE OF  bapimereqitemx WITH HEADER LINE.
  DATA: lt_mereqitem LIKE TABLE OF bapi_te_mereqitem.
  DATA: ls_mereqitem LIKE bapi_te_mereqitem.

  DATA: lt_mereqitemx LIKE TABLE OF bapi_te_mereqitemx,
        ls_mereqitemx LIKE LINE OF lt_mereqitemx.

  DATA: praccount	  LIKE TABLE OF	bapimereqaccount WITH HEADER LINE,
        praccountx  LIKE TABLE OF bapimereqaccountx WITH HEADER LINE,
        extensionin TYPE TABLE OF  bapiparex WITH HEADER LINE,
        lw_itm      TYPE bapi_te_mereqitem,
        lw_itmx     TYPE bapi_te_mereqitemx,
        pritemtext  LIKE TABLE OF bapimereqitemtext WITH HEADER LINE.
  DATA: number LIKE eban-banfn.
  DATA: l_item LIKE eban-bnfpo.
  "  DATA: L_SCHED TYPE J_3AEBSP.
  DATA: l_mes TYPE string.
  DATA: ls_eban LIKE eban.
  DATA: l_price TYPE string.

*抬头信息
  header-pr_type = gs_head-bsart.
  headerx-pr_type = 'X'.

  header-preq_no = gs_head-banfn.
  IF header-preq_no IS NOT INITIAL.
    headerx-item_intvl = 'X'.
    headerx-preq_no = 'X'.
  ENDIF.

*行项目信息
  CLEAR l_item.

  LOOP AT gt_item .

    IF gt_item-afonr IS NOT INITIAL.
      l_item = gt_item-afonr.
    ELSE.
      l_item = l_item + 1.
    ENDIF.
    gt_item-bnfpo = l_item.
    MODIFY gt_item.

    item-preq_item = l_item."'00010'.行项目
    item-pur_group = gs_head-ekgrp.                       "'101'.采购组
    item-material = gt_item-matnr ."'EBB76001U11'.物料号
    item-matl_group = gt_item-matkl.
    item-unit = gt_item-meins.
    item-short_text = gt_item-maktx.
    item-plant = gt_item-werks.                           "'1200'.工厂
    item-rel_date = sy-datum.                           "批准日期
    item-quantity = gt_item-menge."数量
    item-deliv_date = gt_item-eeind.   "交货日期
*    item-item_cat = gs_head-knttp.   "项目类型
    item-store_loc = gt_item-lgort.   "库存地点
    item-preq_name = gt_item-remark2.   "申请者
    item-quantity = gt_item-menge.   "申请者
*    item-trackingno = gt_item-bednr.   "跟踪号
    item-vend_mat = gt_item-idnlf."订单类型

    item-preq_name = gs_head-afnam.

*      l_price =  lt_item-preis.
    CONDENSE l_price NO-GAPS.
    item-preq_price = gt_item-price.   "价格
    item-price_unit = gt_item-peinh.   "价格单位
    IF gt_item-vbeln_va IS NOT INITIAL AND gt_item-posnr_va IS NOT INITIAL AND gs_head-knttp = 'M'."
      praccount-preq_item = l_item.
      praccount-serial_no = '01'.
      praccountx-serial_no = '01'.
      praccountx-preq_item = l_item.

      praccount-gl_account = '1406000000'.
      praccountx-gl_account = 'X'.

      praccount-sd_doc = gt_item-vbeln_va.
      praccountx-sd_doc = 'X'.
      praccount-itm_number = gt_item-posnr_va.
      praccountx-itm_number = 'X'.
      APPEND praccount.
      APPEND praccountx.
      item-acctasscat = 'M'.
      itemx-acctasscat = 'X'.
    ENDIF.

    IF gs_head-knttp = 'K'.
      praccount-preq_item = l_item.
      praccount-serial_no = '01'.
      praccountx-serial_no = '01'.
      praccountx-preq_item = l_item.

      praccount-gl_account = '6600130200'.
      praccountx-gl_account = 'X'.
      praccount-costcenter = gs_head-kostl.
      praccountx-costcenter = 'X'.

      APPEND praccount.
      APPEND praccountx.
      item-acctasscat = 'K'.
      itemx-acctasscat = 'X'.

    ENDIF.

    IF gs_head-knttp = 'A'.
      praccount-preq_item = l_item.
      praccount-serial_no = '01'.
      praccountx-serial_no = '01'.
      praccountx-preq_item = l_item.

      praccount-asset_no = gt_item-anln1.
      praccountx-asset_no = 'X'.

      APPEND praccount.
      APPEND praccountx.
      item-acctasscat = 'A'.
      itemx-acctasscat = 'X'.
    ENDIF.
    item-acctasscat = gs_head-knttp.

    APPEND item.
    itemx-preq_item = l_item.

    item-preq_name = 'X'.
    itemx-pur_group = 'X'.
    itemx-rel_date = 'X'.
    itemx-preq_itemx = 'X'.
    itemx-pur_group = 'X'.
    itemx-short_text = 'X'.
    itemx-material = 'X'.
    itemx-matl_group = 'X'.
    itemx-unit = 'X'.
    itemx-plant = 'X'.
    itemx-store_loc = 'X'.
    itemx-quantity = 'X'.
    itemx-deliv_date = 'X'.
    itemx-preq_name = 'X'.
    itemx-item_cat = 'X'.
    itemx-trackingno = 'X'.
    itemx-vend_mat = 'X'. "add by fangcheng 2020.09.08
    itemx-preq_price = 'X'.   "价格
    itemx-price_unit = 'X'.   "价格单位

    itemx-acctasscat = 'X'.
    APPEND  itemx.

  ENDLOOP.

  CALL FUNCTION 'BAPI_PR_CREATE'
    EXPORTING
      prheader    = header
      prheaderx   = headerx
    IMPORTING
      number      = number
    TABLES
      return      = return
      pritem      = item
      pritemx     = itemx
      praccount   = praccount
      praccountx  = praccountx
      pritemtext  = pritemtext
      extensionin = extensionin.

  LOOP AT return WHERE type EQ 'A' OR type EQ 'E'.

    PERFORM frm_add_msg USING return-type
                              return-id
                              return-number
                              return-message_v1
                              return-message_v2
                              return-message_v3
                              return-message_v4.
  ENDLOOP.
  IF sy-subrc NE 0.
    gs_head-banfn = number.
    gs_head-afono = number.
    gs_head-erdat = sy-datum.
    gs_head-erzet = sy-uzeit.
    PERFORM frm_add_msg USING 'S' '06' 402 number '' '' ''.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    PERFORM frm_set_status USING 'A'.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-banfn = number.
      <gs_item>-afono = number.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  CLEAR: header,headerx,number,  return[],item[],itemx[],praccount[],praccountx[],
"   AFS_PRITEM[],AFS_PRSCHEDULE[],AFS_PRSCHEDULEX[],
  pritemtext,pritemtext[].

ENDFORM.

FORM frm_pr_change.
  DATA: header  LIKE  bapimereqheader,
        headerx LIKE  bapimereqheaderx.
  DATA: return LIKE TABLE OF   bapiret2 WITH HEADER LINE,
        item   LIKE TABLE OF bapimereqitemimp WITH HEADER LINE,
        itemx  LIKE TABLE OF  bapimereqitemx WITH HEADER LINE.
  DATA: lt_mereqitem LIKE TABLE OF bapi_te_mereqitem.
  DATA: ls_mereqitem LIKE bapi_te_mereqitem.

  DATA: lt_mereqitemx LIKE TABLE OF bapi_te_mereqitemx,
        ls_mereqitemx LIKE LINE OF lt_mereqitemx.


  DATA:
    praccount	  LIKE TABLE OF	bapimereqaccount WITH HEADER LINE,
    praccountx  LIKE TABLE OF bapimereqaccountx WITH HEADER LINE,
    extensionin TYPE TABLE OF  bapiparex WITH HEADER LINE,
    lw_itm      TYPE bapi_te_mereqitem,
    lw_itmx     TYPE bapi_te_mereqitemx,
    pritemtext  LIKE TABLE OF bapimereqitemtext WITH HEADER LINE.
  DATA: number LIKE eban-banfn.
  DATA: l_item LIKE eban-bnfpo.
  "  DATA: L_SCHED TYPE J_3AEBSP.
  DATA: l_mes TYPE string.
  DATA: ls_eban LIKE eban.
  DATA: l_price TYPE string.

*抬头信息
  header-pr_type = gs_head-bsart.
  headerx-pr_type = 'X'.

  header-preq_no = gs_head-banfn.
  IF header-preq_no IS NOT INITIAL.
    headerx-item_intvl = 'X'.
    headerx-preq_no = 'X'.
  ENDIF.

*行项目信息
  CLEAR l_item.

  LOOP AT gt_item .
    IF gt_item-afonr IS NOT INITIAL.
      l_item = gt_item-afonr.
    ELSE.
      l_item = l_item + 1.
    ENDIF.
    gt_item-bnfpo = l_item.
    MODIFY gt_item.

    item-preq_item = l_item."'00010'.行项目
    item-pur_group = gs_head-ekgrp.                       "'101'.采购组
    item-material = gt_item-matnr ."'EBB76001U11'.物料号
    item-matl_group = gt_item-matkl.
    item-unit = gt_item-meins.
    item-short_text = gt_item-maktx.
    item-plant = gt_item-werks.                           "'1200'.工厂
    item-rel_date = sy-datum.                           "批准日期
    item-quantity = gt_item-menge."数量
    item-deliv_date = gt_item-eeind.   "交货日期
*    item-item_cat = gs_head-knttp.   "项目类型
    item-store_loc = gt_item-lgort.   "库存地点
    item-preq_name = gt_item-remark2.   "申请者
    item-quantity = gt_item-menge.   "申请者
*    item-trackingno = gt_item-bednr.   "跟踪号
    item-vend_mat = gt_item-idnlf."订单类型
*      l_price =  lt_item-preis.
    CONDENSE l_price NO-GAPS.
    item-preq_price = gt_item-price.   "价格
    item-price_unit = gt_item-peinh.   "价格单位
    item-preq_name = gs_head-afnam.

    IF gt_item-vbeln_va IS NOT INITIAL AND gt_item-posnr_va IS NOT INITIAL AND gs_head-knttp = 'M'."
      praccount-preq_item = l_item.
      praccount-serial_no = '01'.
      praccountx-serial_no = '01'.
      praccountx-preq_item = l_item.

      praccount-gl_account = '1406000000'.
      praccountx-gl_account = 'X'.

      praccount-sd_doc = gt_item-vbeln_va.
      praccountx-sd_doc = 'X'.
      praccount-itm_number = gt_item-posnr_va.
      praccountx-itm_number = 'X'.
      APPEND praccount.
      APPEND praccountx.
      item-acctasscat = 'M'.
      itemx-acctasscat = 'X'.
    ENDIF.

    IF gs_head-knttp = 'K'.
      praccount-preq_item = l_item.
      praccount-serial_no = '01'.
      praccountx-serial_no = '01'.
      praccountx-preq_item = l_item.

      praccount-gl_account = '6600130200'.
      praccountx-gl_account = 'X'.

      praccount-costcenter = gs_head-kostl.
      praccountx-costcenter = 'X'.
      APPEND praccount.
      APPEND praccountx.
      item-acctasscat = 'M'.
      itemx-acctasscat = 'X'.
    ENDIF.
    item-acctasscat = gs_head-knttp.

    APPEND item.
    itemx-preq_item = l_item.
    item-preq_name = 'X'.
    itemx-pur_group = 'X'.
    itemx-rel_date = 'X'.
    itemx-preq_itemx = 'X'.
    itemx-pur_group = 'X'.
    itemx-short_text = 'X'.
    itemx-material = 'X'.
    itemx-matl_group = 'X'.
    itemx-unit = 'X'.
    itemx-material = 'X'.
    itemx-plant = 'X'.
    itemx-store_loc = 'X'.
    itemx-quantity = 'X'.
    itemx-deliv_date = 'X'.
    itemx-preq_name = 'X'.
    itemx-item_cat = 'X'.
    itemx-trackingno = 'X'.
    itemx-vend_mat = 'X'. "add by fangcheng 2020.09.08
    itemx-preq_price = 'X'.   "价格
    itemx-price_unit = 'X'.   "价格单位

    itemx-acctasscat = 'X'.
    APPEND  itemx.

  ENDLOOP.
  CALL FUNCTION 'BAPI_PR_CHANGE'
    EXPORTING
      number      = gs_head-banfn
      prheader    = header
      prheaderx   = headerx
    TABLES
      return      = return
      pritem      = item
      pritemx     = itemx
      praccount   = praccount
      praccountx  = praccountx
      pritemtext  = pritemtext
      extensionin = extensionin.

  LOOP AT return WHERE type EQ 'A' OR type EQ 'E'.

    PERFORM frm_add_msg USING return-type
                              return-id
                              return-number
                              return-message_v1
                              return-message_v2
                              return-message_v3
                              return-message_v4.
  ENDLOOP.
  IF sy-subrc NE 0.
*    gs_head-banfn = number.
*    gs_head-afono = number.
    PERFORM frm_add_msg USING 'S' '06' 402 number '' '' ''.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    PERFORM frm_set_status USING 'A'.

*    LOOP AT gt_item ASSIGNING <gs_item>.
*      <gs_item>-banfn = number.
*      <gs_item>-afono = number.
*    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  CLEAR: header,headerx,number,  return[],item[],itemx[],praccount[],praccountx[],
"   AFS_PRITEM[],AFS_PRSCHEDULE[],AFS_PRSCHEDULEX[],
  pritemtext,pritemtext[].

ENDFORM.


FORM frm_pro_create.
  DATA: po_header            TYPE bapimepoheader,
        po_headerx           TYPE bapimepoheaderx,

        po_item              TYPE TABLE OF bapimepoitem WITH HEADER LINE,
        po_itemx             TYPE TABLE OF bapimepoitemx WITH HEADER LINE,

        po_con_h             TYPE TABLE OF bapimepocondheader  WITH HEADER LINE,
        po_con_hx            TYPE TABLE OF bapimepocondheaderx WITH HEADER LINE,

        po_con_i             TYPE TABLE OF bapimepocond WITH HEADER LINE,
        po_con_ix            TYPE TABLE OF bapimepocondx WITH HEADER LINE,

        po_schedule          TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
        po_schedulex         TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,

        po_account           TYPE TABLE OF bapimepoaccount WITH HEADER LINE,
        po_accountx          TYPE TABLE OF  bapimepoaccountx WITH HEADER LINE,

        po_cond              TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_condx             TYPE TABLE OF  bapimepocondx WITH HEADER LINE,

        po_textheader        TYPE TABLE OF bapimepotextheader WITH HEADER LINE,

        po_number            LIKE bapimepoheader-po_number,
        i_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,

        t_item               TYPE TABLE OF bapimepotext WITH HEADER LINE,


        ls_bapi_te_mepoitem  TYPE bapi_te_mepoitem,
        ls_bapi_te_mepoitemx TYPE bapi_te_mepoitemx,
        gt_extensionin       TYPE TABLE OF bapiparex WITH HEADER LINE,
        gs_extensionin       TYPE bapiparex.


  IF gs_head-afono IS INITIAL.

    PERFORM frm_get_next_afono USING gs_object-nrnr CHANGING gs_head-afono.
    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-afono = gs_head-afono.
    ENDLOOP.
    IF sy-uname <> '10015894'.
      gs_head-erdat = sy-datum.
      gs_head-erzet = sy-uzeit.
      gs_head-ernam = sy-uname.
    ENDIF.

    PERFORM frm_set_status USING 'A'.
  ENDIF.

  po_number = gs_head-afono.

  PERFORM process USING TEXT-045."'正在创建采购订单……'.

  CLEAR:  po_header,po_headerx,po_item,po_item[],po_itemx,po_itemx[],po_schedule,po_schedule[],
   t_item,t_item[],i_return,i_return[],t_return,t_return[].
  REFRESH:gt_extensionin.

  "***创建采购订单
  po_header-po_number    = po_number.
  po_header-doc_type    = gs_head-bsart.
  po_header-currency    =  gs_head-waers.
  po_header-comp_code   =  gs_head-bukrs.
  po_header-purch_org   =  gs_head-ekorg.
  po_header-pur_group   =  gs_head-ekgrp.

  po_header-vendor      =  gs_head-lifnr.
  po_header-our_ref     =  ''.
  po_header-doc_date = gs_head-bldat.
  po_header-item_intvl = 0.


  po_headerx-po_number   = 'X'.
  po_headerx-doc_type   = 'X'.
  po_headerx-currency = 'X'.
  " PO_HEADERX-ITEM_INTVL = 'X'.
  po_headerx-vendor     = 'X'.
  "PO_HEADERX-PMNTTRMS   = 'X'.
  po_headerx-comp_code  = 'X'.
  po_headerx-purch_org  = 'X'.
  po_headerx-pur_group  = 'X'.
  po_headerx-our_ref     =  'X'.
  po_headerx-doc_date = 'X'.
  po_headerx-pmnttrms = 'X'.
  po_headerx-collect_no = 'X'.
  po_headerx-sales_pers = 'X'.
  po_headerx-ref_1 = 'X'.

  po_headerx-item_intvl = 'X'.

*  PERFORM frm_get_text.
*
*  IF gt_tline[] IS NOT INITIAL.
*    LOOP AT gt_tline INTO DATA(ls_tline).
*      MOVE-CORRESPONDING ls_tline TO po_textheader.
**      po_textheader-po_number = po_header-po_number.
*      po_textheader-text_form = ls_tline-tdformat.
*      po_textheader-text_line = ls_tline-tdline.
*      po_textheader-text_id = 'F01'.
*      APPEND po_textheader.
*    ENDLOOP.
*  ENDIF.

  LOOP AT gt_item.
    gt_item-ebelp = gt_item-afonr.
    MODIFY gt_item.
    po_item-po_item    = gt_item-ebelp.
    po_itemx-po_item   = gt_item-ebelp.

    po_item-vend_mat   = gt_item-idnlf.
    po_itemx-vend_mat  = 'X'.

    po_item-material_long      = gt_item-matnr.
    po_itemx-material_long      = 'X'.

    po_item-plant      = gt_item-werks.
    po_itemx-plant     = 'X'.

    po_item-ret_item      = gt_item-retpo.
    po_itemx-ret_item     = 'X'.

    po_item-short_text   = gt_item-maktx.
    po_itemx-short_text   = 'X'.

    po_item-po_unit   = gt_item-meins.
    po_itemx-po_unit   = 'X'.

    po_item-stge_loc   = gt_item-lgort.
    po_itemx-stge_loc   = 'X'.

    po_item-quantity   = gt_item-menge.
    po_itemx-quantity   = 'X'.

    po_item-item_cat   = ''.
    po_itemx-item_cat   = 'X'.

    po_item-over_dlv_tol = gt_item-uebto.
    po_itemx-over_dlv_tol = 'X'.

    po_item-over_dlv_tol = gt_item-uebto.
    po_itemx-over_dlv_tol = 'X'.

*    PO_ITEM-NET_PRICE  = GT_ITEM_PO-PRICE.
*    PO_ITEMX-NET_PRICE   = 'X'.


    IF gs_head-knttp <> 'K'.
      po_item-orderpr_un = gt_item-bprme.
      po_itemx-orderpr_un = 'X'.

      po_item-conv_den1 = gt_item-zmm_tran_rate.
      po_itemx-conv_den1 = 'X' .

      po_item-conv_num1 = 1.
      po_itemx-conv_num1 = 'X'.

    ENDIF.

    IF gt_item-price IS INITIAL.
      po_item-ir_ind = ''.
      po_itemx-ir_ind = 'X'.
    ELSE.
      po_item-ir_ind = 'X'.
      po_itemx-ir_ind = 'X'.

      po_cond-itm_number = gt_item-ebelp.
      po_cond-cond_type = 'Z100'.
      po_cond-cond_value = gt_item-price / 10 .
      po_cond-cond_p_unt = gt_item-peinh.
      po_cond-change_id = 'U'.
      APPEND po_cond.
      CLEAR po_cond.

      po_condx-itm_number   = po_cond-itm_number.
      po_condx-cond_type   = 'X'.
      po_condx-cond_value   = 'X'.
      po_condx-cond_p_unt   = 'X'.
      po_condx-change_id = 'X'.
      APPEND po_condx.
      CLEAR po_condx.

      IF gt_item-cost_amount > 0.
        po_cond-itm_number = gt_item-ebelp.
        po_cond-cond_type = 'Z101'.
        po_cond-cond_value = gt_item-cost_amount.
        po_cond-change_id = 'U'.
        APPEND po_cond.
        CLEAR po_cond.

        po_condx-itm_number = po_cond-itm_number.
        po_condx-cond_value = 'X'.
        po_condx-change_id  = 'X'.
        APPEND po_condx.
        CLEAR po_condx.
      ENDIF.

    ENDIF.

    po_item-price_unit = gt_item-peinh.
    po_itemx-price_unit   = 'X'.

    po_item-tax_code = gs_head-mwskz.
    po_itemx-tax_code   = 'X'.

    po_item-stk_seg_long = gt_item-sgt_scat.
    po_itemx-stk_seg_long = 'X'.

*   po_item-preq_no = gt_item-banfn.
*   po_itemx-preq_no = 'X'.
*
*   po_item-preq_item = gt_item-bnfpo.
*   IF gt_item-bnfpo IS INITIAL.
*     gt_item-bnfpo = gt_item-afonr_ref.
*   ENDIF.
*    po_itemx-preq_item = 'X'.

    po_item-preq_name = gs_head-afnam.
    po_itemx-preq_name = 'X'.

    IF gt_item-vbeln_va IS NOT INITIAL.
      po_item-acctasscat = 'M'.
      po_itemx-acctasscat = 'X'.

      po_account-po_item = gt_item-ebelp.
      po_accountx-po_item = gt_item-ebelp.

      po_account-sd_doc = gt_item-vbeln_va.
      po_accountx-sd_doc = 'X'.

      po_account-itm_number = gt_item-posnr_va.
      po_accountx-itm_number = 'X'.

      po_account-quantity = gt_item-menge.
      po_accountx-quantity = 'X'.

      po_account-gl_account = '1406000000'.
      po_accountx-gl_account = 'X'.


      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.
    ENDIF.

    IF gs_head-knttp = 'K'.
      po_account-po_item = gt_item-ebelp.
      po_account-serial_no = '01'.
      po_accountx-serial_no = '01'.
      po_accountx-po_item = gt_item-ebelp.

      po_account-costcenter = gs_head-kostl.
      po_accountx-costcenter = 'X'.

      APPEND po_account.
      APPEND po_accountx.


      po_item-matl_group = gt_item-matkl.
      po_itemx-matl_group = 'X'.
      po_item-acctasscat = 'K'.
      po_itemx-acctasscat = 'X'.
    ENDIF.

    IF gs_head-knttp = 'A'.
      po_account-po_item = gt_item-ebelp.
      po_account-serial_no = '01'.
      po_accountx-serial_no = '01'.
      po_accountx-po_item = gt_item-ebelp.

      po_account-asset_no = gt_item-anln1.
      po_accountx-asset_no = 'X'.

      APPEND po_account.
      APPEND po_accountx.

      po_item-matl_group = 'Z007'.
      po_itemx-matl_group = 'X'.
      po_item-acctasscat = 'A'.
      po_itemx-acctasscat = 'X'.
    ENDIF.

    APPEND po_item.
    APPEND po_itemx.
    CLEAR  po_item.
    CLEAR  po_itemx.

    po_schedule-po_item = gt_item-ebelp.
    po_schedulex-po_item = gt_item-ebelp.

    po_schedule-sched_line = 1.
    po_schedulex-sched_line = 1.

    po_schedulex-po_itemx = 'X'.

    po_schedule-del_datcat_ext = 'D'.
    po_schedulex-del_datcat_ext = 'X'.

    po_schedule-delivery_date = gt_item-eeind.
    po_schedulex-delivery_date = 'X'.

    po_schedule-quantity = gt_item-menge.
    po_schedulex-quantity = 'X'.


    APPEND po_schedule.
    APPEND po_schedulex.
    CLEAR  po_schedule.
    CLEAR  po_schedulex.
*
*    ls_bapi_te_mepoitem-po_item = gt_item_po-ebelp.
*    ls_bapi_te_mepoitem-zzpino = gt_item_po-zzpino.
*    ls_bapi_te_mepoitem-zppdhd = gt_item_po-zppdhd.
**    LS_BAPI_TE_MEPOITEM-ZMATNR = GT_ITEM_PO-ZMATNR.
*    ls_bapi_te_mepoitem-zkunnr_mat = gt_item_po-zkunnr_mat.
*    ls_bapi_te_mepoitem-zvbeln = gt_item_po-vbeln_va.
*    ls_bapi_te_mepoitem-zposnr = gt_item_po-posnr_va.
*    ls_bapi_te_mepoitem-zkunnr = gt_item_po-kunnr.
*    ls_bapi_te_mepoitem-zcolor = gt_item_po-zcolor.
*    ls_bapi_te_mepoitem-zcolor_text = gt_item_po-zcolor_text.
*    ls_bapi_te_mepoitem-zsize = gt_item_po-zsize.
*    ls_bapi_te_mepoitem-zppflag = gt_item_po-zppflag.
*
*    gs_extensionin-structure =   'BAPI_TE_MEPOITEM'.
*    MOVE ls_bapi_te_mepoitem TO gs_extensionin-valuepart1.
*    APPEND gs_extensionin TO gt_extensionin.
*    CLEAR gs_extensionin.
*
*    ls_bapi_te_mepoitemx-po_item = gt_item_po-ebelp.
*    ls_bapi_te_mepoitemx-zzpino = 'X'.
*    ls_bapi_te_mepoitemx-zppdhd = 'X'.
**    LS_BAPI_TE_MEPOITEMX-ZMATNR = 'X'.
*    ls_bapi_te_mepoitemx-zkunnr_mat = 'X'.
*    ls_bapi_te_mepoitemx-zvbeln = 'X'.
*    ls_bapi_te_mepoitemx-zposnr = 'X'.
*    ls_bapi_te_mepoitemx-zkunnr = 'X'.
*    ls_bapi_te_mepoitemx-zcolor = 'X'.
*    ls_bapi_te_mepoitemx-zcolor_text = 'X'.
*    ls_bapi_te_mepoitemx-zsize = 'X'.
*    ls_bapi_te_mepoitemx-zppflag = 'X'.
*
*    gs_extensionin-structure =   'BAPI_TE_MEPOITEMX'.
*    MOVE ls_bapi_te_mepoitemx TO gs_extensionin-valuepart1.
*    APPEND gs_extensionin TO gt_extensionin.
*    CLEAR gs_extensionin.

  ENDLOOP.

  CLEAR: gs_extensionin,ls_bapi_te_mepoitem,ls_bapi_te_mepoitemx.

*  CLEAR: po_number.

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader         = po_header
      poheaderx        = po_headerx
      no_price_from_po = 'X'
*    IMPORTING
*     exppurchaseorder = po_number
    TABLES
      return           = i_return
      poitem           = po_item
      poitemx          = po_itemx
      potextheader     = po_textheader
      poschedule       = po_schedule
      poschedulex      = po_schedulex
      poaccount        = po_account
      poaccountx       = po_accountx
      pocond           = po_cond
      pocondx          = po_condx
      potextitem       = t_item
      extensionin      = gt_extensionin.

  READ TABLE i_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    gs_head-ebeln = po_number.
    PERFORM frm_set_status USING 'A'.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-ebeln = gs_head-ebeln.
      <gs_item>-afono = gs_head-afono.
    ENDLOOP.

    LOOP AT gt_item_po ASSIGNING <gs_item_po>.
      <gs_item_po>-ebeln = gs_head-ebeln.
      <gs_item_po>-afono = gs_head-afono.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CLEAR po_number.
    CLEAR gs_head-ebeln.
  ENDIF.

  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.
ENDFORM.


FORM frm_pro_change.
  DATA: po_header            TYPE bapimepoheader,
        po_headerx           TYPE bapimepoheaderx,

        po_item              TYPE TABLE OF bapimepoitem WITH HEADER LINE,
        po_itemx             TYPE TABLE OF bapimepoitemx WITH HEADER LINE,

        po_con_h             TYPE TABLE OF bapimepocondheader  WITH HEADER LINE,
        po_con_hx            TYPE TABLE OF bapimepocondheaderx WITH HEADER LINE,

        po_con_i             TYPE TABLE OF bapimepocond WITH HEADER LINE,
        po_con_ix            TYPE TABLE OF bapimepocondx WITH HEADER LINE,

        po_schedule          TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
        po_schedulex         TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,

        po_account           TYPE TABLE OF bapimepoaccount WITH HEADER LINE,
        po_accountx          TYPE TABLE OF  bapimepoaccountx WITH HEADER LINE,

        po_condy             TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_cond              TYPE TABLE OF  bapimepocond WITH HEADER LINE,
        po_condx             TYPE TABLE OF  bapimepocondx WITH HEADER LINE,

        po_textheader        TYPE TABLE OF bapimepotextheader WITH HEADER LINE,

        po_number            LIKE bapimepoheader-po_number,
        i_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return             TYPE TABLE OF bapiret2 WITH HEADER LINE,

        t_item               TYPE TABLE OF bapimepotext WITH HEADER LINE,


        ls_bapi_te_mepoitem  TYPE bapi_te_mepoitem,
        ls_bapi_te_mepoitemx TYPE bapi_te_mepoitemx,
        gt_extensionin       TYPE TABLE OF bapiparex WITH HEADER LINE,
        gs_extensionin       TYPE bapiparex.

  PERFORM process USING TEXT-045."'正在修改采购订单……'.

  CLEAR:  po_header,po_headerx,po_item,po_item[],po_itemx,po_itemx[],po_schedule,po_schedule[],
   t_item,t_item[],i_return,i_return[],t_return,t_return[].
  REFRESH:gt_extensionin.

  po_number = gs_head-ebeln.

  "***创建采购订单

  po_header-po_number   =  gs_head-ebeln.
  po_header-doc_type    =  gs_head-bsart.
  po_header-currency    =  gs_head-waers.
  po_header-comp_code   =  gs_head-bukrs.
  po_header-purch_org   =  gs_head-ekorg.
  po_header-pur_group   =  gs_head-ekgrp.

  po_header-vendor      =  gs_head-lifnr.
  po_header-our_ref     =  ''.
  po_header-doc_date = gs_head-bldat.
  po_header-item_intvl = 0.

  po_headerx-po_number   = 'X'.
  po_headerx-doc_type   = 'X'.
  po_headerx-currency = 'X'.
  " PO_HEADERX-ITEM_INTVL = 'X'.
  po_headerx-vendor     = 'X'.
  "PO_HEADERX-PMNTTRMS   = 'X'.
  po_headerx-comp_code  = 'X'.
  po_headerx-purch_org  = 'X'.
  po_headerx-pur_group  = 'X'.
  po_headerx-our_ref     =  'X'.
  po_headerx-doc_date = 'X'.
  po_headerx-pmnttrms = 'X'.
  po_headerx-collect_no = 'X'.
  po_headerx-sales_pers = 'X'.
  po_headerx-ref_1 = 'X'.
  po_headerx-item_intvl = 'X'.
*  PERFORM frm_get_text.
*  IF gt_tline[] IS NOT INITIAL.
*    LOOP AT gt_tline INTO DATA(ls_tline).
*      MOVE-CORRESPONDING ls_tline TO po_textheader.
*      po_textheader-po_number = po_header-po_number.
*      po_textheader-text_form = ls_tline-tdformat.
*      po_textheader-text_line = ls_tline-tdline.
*      po_textheader-text_id = 'F01'.
*      APPEND po_textheader.
*    ENDLOOP.
*  ENDIF.

  DATA:ls_kposn TYPE kposn.

  CALL FUNCTION 'BAPI_PO_GETDETAIL1'
    EXPORTING
      purchaseorder = po_number
    TABLES
      pocond        = po_condy.

  LOOP AT gt_item.
    IF gt_item-ebelp IS INITIAL .
      gt_item-ebelp = gt_item-afonr.
      MODIFY gt_item.
    ENDIF.

    IF gt_item-del_flag = 'X'.
      po_item-delete_ind = 'L'.
      po_itemx-delete_ind = 'X'.
    ENDIF.
    po_item-po_item    = gt_item-ebelp.
    po_itemx-po_item   = gt_item-ebelp.

    po_item-vend_mat   = gt_item-idnlf.
    po_itemx-vend_mat  = 'X'.

    po_item-material   = gt_item-matnr.
    po_itemx-material  = 'X'.

    po_item-material_long  = gt_item-matnr.
    po_itemx-material_long = 'X'.

    po_item-short_text   = gt_item-maktx.
    po_itemx-short_text   = 'X'.

    po_item-plant      = gt_item-werks.
    po_itemx-plant     = 'X'.

    po_item-stge_loc   = gt_item-lgort.
    po_itemx-stge_loc  = 'X'.

    po_item-quantity   = gt_item-menge.
    po_itemx-quantity  = 'X'.

    po_item-item_cat   = ''.
    po_itemx-item_cat  = 'X'.

    po_item-over_dlv_tol  = gt_item-uebto.
    po_itemx-over_dlv_tol = 'X'.

*    PO_ITEM-UNDER_DLV_TOL = GT_ZMMS0041-UNTTO.
*    PO_ITEMX-OVER_DLV_TOL = 'X'.

    IF gs_head-knttp <> 'K'.
      po_item-orderpr_un = gt_item-bprme.
      po_itemx-orderpr_un = 'X'.

      po_item-conv_den1 = gt_item-zmm_tran_rate.
      po_itemx-conv_den1 = 'X' .

      po_item-conv_num1 = 1.
      po_itemx-conv_num1 = 'X'.

    ENDIF.

    po_item-net_price  = gt_item-price.
*    PO_ITEMX-NET_PRICE   = 'X'.

    po_item-price_unit  = gt_item-peinh.
    po_itemx-price_unit = 'X'.

    po_item-tax_code    = gs_head-mwskz.
    po_itemx-tax_code   = 'X'.

    po_item-stk_seg_long  = gt_item-sgt_scat.
    po_itemx-stk_seg_long = 'X'.

    po_item-preq_name  = gs_head-afnam.
    po_itemx-preq_name = 'X'.


    IF gt_item_po-vbeln_va IS NOT INITIAL.
      po_item-acctasscat = 'M'.
      po_itemx-acctasscat = 'X'.

      po_account-po_item = gt_item-ebelp.
      po_accountx-po_item = gt_item-ebelp.

      po_account-sd_doc = gt_item-vbeln_va.
      po_accountx-sd_doc = 'X'.

      po_account-itm_number = gt_item-posnr_va.
      po_accountx-itm_number = 'X'.

      po_account-quantity = gt_item-menge.
      po_accountx-quantity = 'X'.

      po_account-gl_account = '1406000000'.
      po_accountx-gl_account = 'X'.

      APPEND po_account.
      APPEND po_accountx.
      CLEAR  po_account.
      CLEAR po_accountx.
    ENDIF.

    IF gs_head-knttp = 'K'.
      po_account-po_item = gt_item-ebelp.
      po_account-serial_no = '01'.
      po_accountx-serial_no = '01'.
      po_accountx-po_item = gt_item-ebelp.

      po_account-costcenter = gs_head-kostl.
      po_accountx-costcenter = 'X'.

      po_item-po_unit   = gt_item-meins.
      po_itemx-po_unit   = 'X'.

      APPEND po_account.
      APPEND po_accountx.


      po_item-matl_group = gt_item-matkl.
      po_itemx-matl_group = 'X'.
      po_item-acctasscat = 'K'.
      po_itemx-acctasscat = 'X'.
    ENDIF.

    IF gt_item-price IS INITIAL.
      po_item-ir_ind = ''.
      po_itemx-ir_ind = 'X'.
    ELSE.
      po_item-ir_ind = 'X'.
      po_itemx-ir_ind = 'X'.


      LOOP AT po_condy WHERE itm_number = gt_item-ebelp.
        IF po_condy-cond_type = 'Z100'." 含税价

          MOVE po_condy TO po_cond.
          po_cond-cond_value = gt_item-price .
          po_cond-cond_p_unt = gt_item-peinh.
          po_cond-change_id = 'U'.
          APPEND po_cond.

          po_condx-condition_no = po_cond-condition_no.
          po_condx-itm_number   = po_cond-itm_number.
          po_condx-cond_st_no   = po_cond-cond_st_no.
          po_condx-cond_value   = 'X'.
          po_condx-change_id = 'X'.
          APPEND po_condx.
        ENDIF.

        IF po_condy-cond_type = 'Z101' AND gs_head-bustyp <> 'PO100'." 附加费

          MOVE po_condy TO po_cond.
          po_cond-cond_value = gt_item-cost_amount.
          po_cond-change_id = 'U'.
          APPEND po_cond.

          po_condx-condition_no = po_cond-condition_no.
          po_condx-itm_number   = po_cond-itm_number.
          po_condx-cond_st_no   = po_cond-cond_st_no.
          po_condx-cond_value   = 'X'.
          po_condx-change_id = 'X'.
          APPEND po_condx.
        ENDIF.
      ENDLOOP.
      IF sy-subrc NE 0.
        CLEAR po_cond.
        CLEAR po_condx.

        po_cond-itm_number = gt_item-ebelp.
        po_cond-cond_type = 'Z100'.
        po_cond-cond_value = gt_item-price / 10 .
        po_cond-cond_p_unt = gt_item-peinh.
        po_cond-change_id = 'U'.
        APPEND po_cond.
        CLEAR po_cond.

        po_condx-itm_number  = po_cond-itm_number.
        po_condx-cond_type   = 'X'.
        po_condx-cond_value  = 'X'.
        po_condx-cond_p_unt  = 'X'.
        po_condx-change_id = 'X'.
        APPEND po_condx.
        CLEAR po_condx.

        IF gt_item-cost_amount > 0.
          po_cond-itm_number = gt_item-ebelp.
          po_cond-cond_type = 'Z101'.
          po_cond-cond_value = gt_item-cost_amount.
          po_cond-change_id = 'U'.
          APPEND po_cond.
          CLEAR po_cond.
          po_condx-itm_number = po_cond-itm_number.
          po_condx-cond_value = 'X'.
          po_condx-change_id  = 'X'.
          APPEND po_condx.
          CLEAR po_condx.
        ENDIF.
      ENDIF.

    ENDIF.

    APPEND po_item.
    APPEND po_itemx.
    CLEAR  po_item.
    CLEAR  po_itemx.

    po_schedule-po_item = gt_item-ebelp.
    po_schedulex-po_item = gt_item-ebelp.

    po_schedule-sched_line = 1.
    po_schedulex-sched_line = 1.

    po_schedulex-po_itemx = 'X'.

    po_schedule-del_datcat_ext = 'D'.
    po_schedulex-del_datcat_ext = 'X'.

    po_schedule-delivery_date = gt_item-eeind.

    IF po_schedule-delivery_date IS INITIAL.
      po_schedule-delivery_date = sy-datum.
    ENDIF.
    po_schedulex-delivery_date = 'X'.

    po_schedule-quantity = gt_item-menge.
    po_schedulex-quantity = 'X'.


    APPEND po_schedule.
    APPEND po_schedulex.
    CLEAR  po_schedule.
    CLEAR  po_schedulex.

    ls_bapi_te_mepoitem-po_item = gt_item-ebelp.
    ls_bapi_te_mepoitem-zzpino = gt_item-zzpino.
    ls_bapi_te_mepoitem-zppdhd = gt_item-zppdhd.
*    LS_BAPI_TE_MEPOITEM-ZMATNR = GT_ITEM_PO-ZMATNR.
    ls_bapi_te_mepoitem-zkunnr_mat = gt_item-zkunnr_mat.
    ls_bapi_te_mepoitem-zvbeln = gt_item-vbeln_va.
    ls_bapi_te_mepoitem-zposnr = gt_item-posnr_va.
    ls_bapi_te_mepoitem-zkunnr = gt_item-kunnr.
    ls_bapi_te_mepoitem-zcolor = gt_item-zcolor.
    ls_bapi_te_mepoitem-zcolor_text = gt_item-zcolor_text.
    ls_bapi_te_mepoitem-zsize = gt_item-zsize.
    ls_bapi_te_mepoitem-zppflag = gt_item-zppflag.

    gs_extensionin-structure =   'BAPI_TE_MEPOITEM'.
    MOVE ls_bapi_te_mepoitem TO gs_extensionin-valuepart1.
    APPEND gs_extensionin TO gt_extensionin.
    CLEAR gs_extensionin.

    ls_bapi_te_mepoitemx-po_item = gt_item-ebelp.
    ls_bapi_te_mepoitemx-zzpino = 'X'.
    ls_bapi_te_mepoitemx-zppdhd = 'X'.
*    LS_BAPI_TE_MEPOITEMX-ZMATNR = 'X'.
    ls_bapi_te_mepoitemx-zkunnr_mat = 'X'.
    ls_bapi_te_mepoitemx-zvbeln = 'X'.
    ls_bapi_te_mepoitemx-zposnr = 'X'.
    ls_bapi_te_mepoitemx-zkunnr = 'X'.
    ls_bapi_te_mepoitemx-zcolor = 'X'.
    ls_bapi_te_mepoitemx-zcolor_text = 'X'.
    ls_bapi_te_mepoitemx-zsize = 'X'.
    ls_bapi_te_mepoitemx-zppflag = 'X'.

    gs_extensionin-structure =   'BAPI_TE_MEPOITEMX'.
    MOVE ls_bapi_te_mepoitemx TO gs_extensionin-valuepart1.
    APPEND gs_extensionin TO gt_extensionin.
    CLEAR gs_extensionin.

  ENDLOOP.

  CLEAR: gs_extensionin,ls_bapi_te_mepoitem,ls_bapi_te_mepoitemx.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = po_number
      poheader      = po_header
      poheaderx     = po_headerx
*     NO_PRICE_FROM_PO = 'X'
* IMPORTING
*     EXPHEADER     =
*     EXPPOEXPIMPHEADER            =
    TABLES
      return        = i_return
      poitem        = po_item
      poitemx       = po_itemx
      potextheader  = po_textheader
      poschedule    = po_schedule
      poschedulex   = po_schedulex
      poaccount     = po_account
      poaccountx    = po_accountx
      pocond        = po_cond
      pocondx       = po_condx
      potextitem    = t_item
      extensionin   = gt_extensionin.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  IF po_number IS NOT INITIAL.
    gs_head-ebeln = po_number.
  ENDIF.

  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.

ENDFORM.


FORM frm_so_create.

  DATA: so_header TYPE bapisdhd1.
  DATA: so_headerx TYPE bapisdhd1x.
  DATA: so_items TYPE TABLE OF bapisditm WITH HEADER LINE.
  DATA: so_itemsx TYPE TABLE OF bapisditmx WITH HEADER LINE.
  DATA: so_partners TYPE TABLE OF bapiparnr WITH HEADER LINE.
  DATA: so_schedules TYPE TABLE OF bapischdl WITH HEADER LINE.
  DATA: so_schedulesx TYPE TABLE OF bapischdlx WITH HEADER LINE.
  DATA: so_conditions TYPE TABLE OF bapicond WITH HEADER LINE.
  DATA: so_conditionsx TYPE TABLE OF bapicondx WITH HEADER LINE.

  DATA: i_return TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CLEAR:so_header,so_headerx,
   so_items,so_items[],
   so_itemsx,so_itemsx[],
   so_schedules,so_schedules[],
   so_schedulesx,so_schedulesx[],
   so_conditions,so_conditions[],
   so_conditionsx,so_conditionsx[],
   so_partners[],
   i_return,i_return[],t_return,t_return[].

  so_header-doc_type = gs_head-auart."订单类型

  so_header-sales_org = gs_head-vkorg."销售组织

  so_header-distr_chan = gs_head-vtweg."分销渠道

  so_header-division = gs_head-spart."产品组

  so_header-sales_off =  gs_head-vkbur. "销售办事处

*  so_header-req_date_h = gs_head-eeind.
  so_header-req_date_h = sy-datum.

  so_header-purch_no_c = gs_head-kunnr.

  so_header-pmnttrms = '0001'."收付条件代码

  so_headerx-updateflag = 'I'.
  so_headerx-doc_type = 'X'.
  so_headerx-sales_org = 'X'.
  so_headerx-distr_chan = 'X'.
  so_headerx-division = 'X'.
  so_header-sales_off = 'X'.
  so_headerx-ass_number = 'X'.
  so_headerx-req_date_h = 'X'.
  so_headerx-purch_no_c = 'X'.
  so_headerx-pmnttrms = 'X'.

  CLEAR so_partners.
  so_partners-partn_role = 'AG'."售达方
  so_partners-partn_numb = gs_head-kunnr.
  APPEND so_partners.
  CLEAR so_partners.
  so_partners-partn_role = 'WE'."送达方
  so_partners-partn_numb = gs_head-kunnr.
  APPEND so_partners.
  CLEAR so_partners.
  so_partners-partn_role = 'RE'."付款方
  so_partners-partn_numb = gs_head-kunnr.
  APPEND so_partners.
  CLEAR so_partners.
  so_partners-partn_role = 'RG'."开票方
  so_partners-partn_numb = gs_head-kunnr.
  APPEND so_partners.


  LOOP AT gt_item.
    CLEAR: so_items, so_itemsx,
           so_schedules, so_schedulesx,
           so_conditions, so_conditionsx.

    IF gt_item-posnr_va IS INITIAL .
      gt_item-posnr_va = gt_item-afonr * 10.
      MODIFY gt_item.
    ENDIF.

    so_items-itm_number = gt_item-posnr_va.
    so_items-item_categ = 'ZTAN'.

    IF gt_item-del_flag = 'X'.
    ENDIF.

    so_items-material = gt_item-matnr.

    so_items-plant = gt_item-werks.

    so_items-store_loc = gt_item-lgort.

    so_items-target_qty = gt_item-menge.

    so_items-batch = gt_item-charg.

    so_items-ship_point = '0001'."装运点

    so_items-target_qu  = gt_item-meins.

    so_items-sales_unit = gt_item-meins.

    so_itemsx-itm_number = gt_item-posnr_va.
    so_itemsx-updateflag  =  'I'.
    so_itemsx-item_categ =  'X'.
    so_itemsx-material  =  'X'.
    so_itemsx-plant    =  'X'.
    so_itemsx-store_loc =  'X'.
    so_itemsx-target_qty =  'X'.
    so_itemsx-batch = 'X'.
    so_itemsx-ship_point = 'X'."装运点
    so_itemsx-target_qu = 'X'.
    so_itemsx-sales_unit = 'X'.
    APPEND so_items .
    APPEND so_itemsx.

    so_schedules-itm_number = gt_item-posnr_va.
    so_schedules-sched_line = '0001'.
    so_schedules-req_qty = gt_item-menge.
    so_schedules-tp_date = sy-datum.
    so_schedules-load_date = sy-datum.
    so_schedules-gi_date = sy-datum.
    so_schedules-ms_date = sy-datum.

    so_schedulesx-itm_number = gt_item-posnr_va.
    so_schedulesx-sched_line = '0001'.
    so_schedulesx-req_qty = 'X'.
    so_schedulesx-tp_date = 'X'.
    so_schedulesx-load_date = 'X'.
    so_schedulesx-gi_date = 'X'.
    so_schedulesx-ms_date = 'X'.
    so_schedulesx-updateflag = 'I'.
    APPEND so_schedules.
    APPEND so_schedulesx.

    so_conditions-itm_number = gt_item-posnr_va.

    so_conditions-cond_st_no = ''.

    so_conditions-cond_count = ''.

    so_conditions-cond_type = 'ZPB1'. "定价条件

    so_conditions-cond_value = gt_item-amount.
    so_conditions-cond_p_unt = 1.

    so_conditions-currency = gt_item-waers. "币别

    so_conditionsx-itm_number = gt_item-posnr_va.
    so_conditionsx-updateflag = 'I'.
    so_conditionsx-cond_st_no = ''.
    so_conditionsx-cond_count = ''.
    so_conditionsx-cond_type = 'X'. "定价条件
    so_conditionsx-cond_value = 'X'. "价格
    so_conditionsx-cond_p_unt = 'X'.
    so_conditionsx-currency = 'X'. "币别
    APPEND so_conditions.
    APPEND so_conditionsx.

  ENDLOOP.


  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
    EXPORTING
*     SALESDOCUMENTIN      =
      order_header_in      = so_header
      order_header_inx     = so_headerx
*     SENDER               =
*     BINARY_RELATIONSHIPTYPE       =
*     INT_NUMBER_ASSIGNMENT         =
*     BEHAVE_WHEN_ERROR    =
*     LOGIC_SWITCH         =
*     TESTRUN              =
*     CONVERT              = ' '
    IMPORTING
      salesdocument        = gs_head-vbeln_va
    TABLES
      return               = i_return
      order_items_in       = so_items
      order_items_inx      = so_itemsx
      order_partners       = so_partners
      order_schedules_in   = so_schedules
      order_schedules_inx  = so_schedulesx
      order_conditions_in  = so_conditions
      order_conditions_inx = so_conditionsx
*     ORDER_CFGS_REF       =
*     ORDER_CFGS_INST      =
*     ORDER_CFGS_PART_OF   =
*     ORDER_CFGS_VALUE     =
*     ORDER_CFGS_BLOB      =
*     ORDER_CFGS_VK        =
*     ORDER_CFGS_REFINST   =
*     ORDER_CCARD          =
*     ORDER_TEXT           =
*     ORDER_KEYS           =
*     EXTENSIONIN          =
*     PARTNERADDRESSES     =
*     EXTENSIONEX          =
*     NFMETALLITMS         =
    .

  READ TABLE i_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    PERFORM frm_set_status USING 'A'.

    gs_head-afono = gs_head-vbeln_va.
    gs_head-erdat = sy-datum.
    gs_head-erzet = sy-uzeit.
    gs_head-ernam = sy-uname.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-vbeln_va = gs_head-vbeln_va.
      <gs_item>-afono = gs_head-vbeln_va.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CLEAR gs_head-vbeln_va.
  ENDIF.

  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.

ENDFORM.


FORM frm_so_change.

  DATA: so_header TYPE bapisdh1.
  DATA: so_headerx TYPE bapisdh1x.
  DATA: so_items TYPE TABLE OF bapisditm WITH HEADER LINE.
  DATA: so_itemsx TYPE TABLE OF bapisditmx WITH HEADER LINE.
  DATA: so_partners TYPE TABLE OF bapiparnr WITH HEADER LINE.
  DATA: so_schedules TYPE TABLE OF bapischdl WITH HEADER LINE.
  DATA: so_schedulesx TYPE TABLE OF bapischdlx WITH HEADER LINE.
  DATA: so_conditions TYPE TABLE OF bapicond WITH HEADER LINE.
  DATA: so_conditionsx TYPE TABLE OF bapicondx WITH HEADER LINE.


  DATA: i_return TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CLEAR:so_header,so_headerx,
   so_items,so_items[],
   so_itemsx,so_itemsx[],
   so_schedules,so_schedules[],
   so_schedulesx,so_schedulesx[],
   so_conditions,so_conditions[],
   so_conditionsx,so_conditionsx[],
   so_partners[],
   i_return,i_return[],t_return,t_return[].

  so_header-sales_org = gs_head-vkorg."销售组织

  so_header-distr_chan = gs_head-vtweg."分销渠道

  so_header-division = gs_head-spart."产品组

  so_header-sales_off =  gs_head-vkbur. "销售办事处

  so_header-req_date_h = sy-datum.

  so_header-purch_no_c = gs_head-kunnr.

  so_header-pmnttrms = '0001'."收付条件代码

  so_headerx-updateflag = 'U'.
  so_headerx-sales_org = 'X'.
  so_headerx-distr_chan = 'X'.
  so_headerx-division = 'X'.
  so_header-sales_off = 'X'.
  so_headerx-ass_number = 'X'.
  so_headerx-req_date_h = 'X'.
  so_headerx-purch_no_c = 'X'.
  so_headerx-pmnttrms = 'X'.

  CLEAR so_partners.
  so_partners-partn_role = 'AG'."售达方
  so_partners-partn_numb = gs_head-kunnr.
  APPEND so_partners.
  CLEAR so_partners.
  so_partners-partn_role = 'WE'."送达方
  so_partners-partn_numb = gs_head-kunnr.
  APPEND so_partners.
  CLEAR so_partners.
  so_partners-partn_role = 'RE'."付款方
  so_partners-partn_numb = gs_head-kunnr.
  APPEND so_partners.
  CLEAR so_partners.
  so_partners-partn_role = 'RG'."开票方
  so_partners-partn_numb = gs_head-kunnr.
  APPEND so_partners.


  LOOP AT gt_item.
    CLEAR: so_items,
     so_itemsx,
     so_schedules,
     so_schedulesx,
     so_conditions,
     so_conditionsx.

    IF gt_item-posnr_va IS INITIAL .
      gt_item-posnr_va = gt_item-afonr * 10.
      MODIFY gt_item.
    ENDIF.

    so_items-itm_number    = gt_item-posnr_va.
    so_itemsx-itm_number   = gt_item-posnr_va.

    IF gt_item-del_flag = 'X'.
*      so_items-delete_ind = 'L'.
*      so_itemsx-delete_ind = 'X'.
    ENDIF.

    so_items-material = gt_item-matnr.

    so_items-plant = gt_item-werks.

    so_items-store_loc = gt_item-lgort.

    so_items-target_qty = gt_item-menge.

    so_items-ship_point = '0001'."装运点

    so_items-target_qu = gt_item-meins.

    so_items-sales_unit = gt_item-meins.

    APPEND so_items .
    APPEND so_itemsx.

    so_schedules-itm_number = gt_item-posnr_va.
    so_schedules-sched_line = '0001'.
    so_schedules-req_qty = gt_item-menge.
    so_schedules-tp_date = sy-datum.
    so_schedules-load_date = sy-datum.
    so_schedules-gi_date = sy-datum.

    so_schedulesx-itm_number = gt_item-posnr_va.
    so_schedulesx-sched_line = '0001'.
    so_schedulesx-req_qty = 'X'.
    so_schedulesx-tp_date = 'X'.
    so_schedulesx-load_date = 'X'.
    so_schedulesx-gi_date = 'X'.
    so_schedulesx-updateflag = 'U'.
    APPEND so_schedules.
    APPEND so_schedulesx.

    so_conditions-itm_number = gt_item-posnr_va.

    so_conditions-cond_st_no = 13." 定价条件 根据 PRCD_ELEMENTS 表

    so_conditions-cond_count = 1.

    so_conditions-cond_type = 'ZPB1'. "定价条件

    so_conditions-cond_value = gt_item-amount.

    so_conditions-currency = gt_item-waers. "币别

    so_conditionsx-itm_number = gt_item-posnr_va.

    so_conditionsx-cond_st_no = 13.

    so_conditionsx-cond_count = 1.

    so_conditionsx-updateflag = 'U'.

    so_conditionsx-cond_type = 'ZPB1'. "定价条件

    so_conditionsx-cond_value = 'X'. "价格

    so_conditionsx-currency = 'X'. "币别
    APPEND so_conditions.
    APPEND so_conditionsx.

  ENDLOOP.


  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = gs_head-vbeln_va
      order_header_in  = so_header
      order_header_inx = so_headerx
*     SIMULATION       =
*     BEHAVE_WHEN_ERROR           = ' '
*     INT_NUMBER_ASSIGNMENT       = ' '
*     LOGIC_SWITCH     =
*     NO_STATUS_BUF_INIT          = ' '
    TABLES
      return           = i_return
      order_item_in    = so_items
      order_item_inx   = so_itemsx
*     PARTNERS         =
*     PARTNERCHANGES   =
*     PARTNERADDRESSES =
*     ORDER_CFGS_REF   =
*     ORDER_CFGS_INST  =
*     ORDER_CFGS_PART_OF          =
*     ORDER_CFGS_VALUE =
*     ORDER_CFGS_BLOB  =
*     ORDER_CFGS_VK    =
*     ORDER_CFGS_REFINST          =
      schedule_lines   = so_schedules
      schedule_linesx  = so_schedulesx
*     ORDER_TEXT       =
*     ORDER_KEYS       =
      conditions_in    = so_conditions
      conditions_inx   = so_conditionsx
*     EXTENSIONIN      =
*     EXTENSIONEX      =
*     NFMETALLITMS     =
    .
  READ TABLE i_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    PERFORM frm_set_status USING 'A'.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-vbeln_va = gs_head-vbeln_va.
      <gs_item>-afono = gs_head-afono.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CLEAR gs_head-vbeln_va.
  ENDIF.

  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.

ENDFORM.


FORM frm_sd_post.

  DATA: ls_header_data    LIKE bapiobdlvhdrcon,
        ls_header_control LIKE bapiobdlvhdrctrlcon.

  DATA: lw_delivery       TYPE bapiobdlvhdrcon-deliv_numb.

  DATA : l_mat_doc   LIKE bapi2017_gm_head_ret-mat_doc.
  DATA : l_doc_year LIKE bapi2017_gm_head_ret-doc_year.

  DATA: i_return TYPE TABLE OF bapiret2 WITH HEADER LINE,
        t_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  ls_header_data-deliv_numb     = gs_head-afono.

  ls_header_control-deliv_numb  = gs_head-afono.

  ls_header_control-post_gi_flg = 'X'. "自动过帐货物移动

  ls_header_control-volume_flg  = 'X'. "量的确认

  ls_header_control-deliv_date_flg = 'X'. "确认交货日期

  lw_delivery                   = gs_head-afono.



  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CONFIRM_DEC'
    EXPORTING
      header_data    = ls_header_data
      header_control = ls_header_control
      delivery       = lw_delivery
*     TECHN_CONTROL  =
*     HEADER_DATA_SPL                  =
*     HEADER_CONTROL_SPL               =
    TABLES
*     HEADER_PARTNER =
*     HEADER_PARTNER_ADDR              =
*     HEADER_DEADLINES                 =
*     ITEM_DATA      =
*     ITEM_CONTROL   =
*     ITEM_CODING_BLOCK                =
*     ITEM_SERIAL_NO =
*     SUPPLIER_CONS_DATA               =
*     HANDLING_UNIT_HEADER             =
*     HANDLING_UNIT_ITEM               =
*     HANDLING_UNIT_HEADER_EPC         =
*     HANDLING_UNIT_ITEMS_EPC          =
*     HANDLING_UNIT_SERNO              =
*     EXTENSION1     =
*     EXTENSION2     =
      return         = i_return.
*     TOKENREFERENCE =
*     HANDLING_UNIT_HEADER_CROSS       =
*     ITEM_DATA_SPL  =
*     HANDLING_UNIT_IDENTIFIERS        =
*     HANDLING_UNIT_ITEM_SPL           =
*     ITEM_DATA_DOCU_BATCH             =
*     NEW_ITEM_DATA  =
*     NEW_ITEM_DATA_SPL                =
*     NEW_ITEM_ORG   =
*     TEXT_HEADER    =
*     TEXT_LINES     =
*     NEW_ITEM_DATA_SKU                =
*     CWM_ITEM_DATA  =
*     CWM_HU_ITEM    =
  .
  READ TABLE i_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    SELECT SINGLE vbeln,mjahr INTO ( @l_mat_doc , @l_doc_year )
    FROM vbfa
    WHERE vbelv = @lw_delivery
      AND vbtyp_n = 'R'
      AND bwart NE ''
      AND rfmng NE 0 "R 货物移动（过账物料凭证）
      AND NOT EXISTS ( SELECT * FROM m_mbmps "视图比MSEG多条件
                      WHERE smbln = vbfa~vbeln OR mblnr = vbfa~vbeln ). "非冲销和被冲销

    gs_head-mblnr = l_mat_doc.
    gs_head-mjahr = l_doc_year.

    gs_head-aenam =  sy-uname.
    gs_head-aedat =  sy-datum.
    gs_head-aetim =  sy-uzeit.
    PERFORM frm_set_status USING 'S'.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-mblnr = gs_head-mblnr.
      <gs_item>-mjahr = gs_head-mjahr.

      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
      APPEND gt_item_modify.
      CLEAR gt_item_modify.
    ENDLOOP.


    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    PERFORM frm_db_modify.

    PERFORM frm_add_msg USING 'S' 'ZAFO' 079 l_mat_doc ''  '' ''."物料凭证&1已过账成功

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  LOOP AT i_return.
    CHECK i_return-type <> 'W'.
    PERFORM frm_add_msg USING i_return-type
                              i_return-id
                              i_return-number
                              i_return-message_v1
                              i_return-message_v2
                              i_return-message_v3
                              i_return-message_v4.
  ENDLOOP.

ENDFORM.
