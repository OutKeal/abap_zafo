*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_SAVE
*&---------------------------------------------------------------------*

FORM frm_save_afo_db USING status.
  DATA:lt_afo_item TYPE TABLE OF zafo_item WITH HEADER LINE.

  CHECK gt_head_modify[] IS NOT INITIAL.

  LOOP AT gt_head_modify.
    IF gt_head_modify-afono IS INITIAL OR gt_head_modify-afonr IS INITIAL.
      MESSAGE '前置单据单号为空(' && gv_qcmode && ')，更新失败' TYPE 'E'.
      g_error = 'X'.
      RETURN.
    ENDIF.
  ENDLOOP.

  SELECT * INTO TABLE lt_afo_item
    FROM zafo_item
    FOR ALL ENTRIES IN gt_head_modify
    WHERE afono = gt_head_modify-afono
      AND afonr = gt_head_modify-afonr.
  DATA(line) = lines( lt_afo_item[] ).
  IF lt_afo_item[] IS INITIAL OR line > 300.
    MESSAGE '明细行行数异常，更新失败' TYPE 'E'.
    RETURN.
  ENDIF.


  LOOP AT lt_afo_item.

    lt_afo_item-qc_status = status.

    READ TABLE gt_head_modify WITH KEY afono = lt_afo_item-afono
                                        afonr = lt_afo_item-afonr.
    IF sy-subrc EQ 0.
      CASE status.
        WHEN 'A'.
          lt_afo_item-qcno = ''.
          lt_afo_item-qcnr = ''.
        WHEN 'B'.
          lt_afo_item-qcno = gt_head_modify-qcno.
          lt_afo_item-qcnr = gt_head_modify-qcnr.
        WHEN 'C'.
          lt_afo_item-qcno = gt_head_modify-qcno.
          lt_afo_item-qcnr = gt_head_modify-qcnr.
      ENDCASE.
    ENDIF.

    MODIFY lt_afo_item.
    CLEAR lt_afo_item.
  ENDLOOP.

  PERFORM frm_th_afo USING status.

  CHECK g_error <> 'X' .

  PERFORM frm_save_afo_head_db USING status.

  CHECK g_error <> 'X' .

  MODIFY zafo_item FROM TABLE lt_afo_item .
  IF sy-subrc NE 0.
    g_error = 'X'.
    ROLLBACK WORK.
    MESSAGE '更新失败' TYPE 'E'.
  ENDIF.

ENDFORM.


FORM frm_save_afo_head_db USING status.
  DATA:lt_afo_head TYPE TABLE OF zafo_head WITH HEADER LINE.
  DATA:lt_afo_item TYPE TABLE OF zafo_item WITH HEADER LINE.
  DATA:lv_qc_status TYPE zafo_qc_status.
  DATA:lv_afo_bustyp TYPE zafo_bustyp.

  CLEAR lv_qc_status.

  CHECK gv_qcmode = 'A' OR  gv_qcmode = 'B'.

  " 质检执行单据
  SELECT afono,qc_status INTO CORRESPONDING FIELDS OF TABLE @lt_afo_head
    FROM zafo_head
    FOR ALL ENTRIES IN @gt_head_modify
    WHERE afono = @gt_head_modify-afono.

  SELECT afono,afonr,qc_status INTO CORRESPONDING FIELDS OF TABLE  @lt_afo_item
    FROM zafo_item
    FOR ALL ENTRIES IN @gt_head_modify
    WHERE afono = @gt_head_modify-afono
    AND del_flag <> 'X'.

  " 用当前要更新的质检单质检状态更新afo单据明细
  LOOP AT lt_afo_item ASSIGNING FIELD-SYMBOL(<fs_afo_item>).
    READ TABLE gt_head_modify WITH  KEY afono = <fs_afo_item>-afono
                                        afonr = <fs_afo_item>-afonr.
    IF sy-subrc = 0 AND <fs_afo_item>-qc_status <> gt_head_modify-qc_status.
      <fs_afo_item>-qc_status = gt_head_modify-qc_status.
    ENDIF.
  ENDLOOP.

  CASE gv_qcmode.
    WHEN 'A'.
      lv_afo_bustyp = 'R1001'.
    WHEN 'B'.
      lv_afo_bustyp = 'R1006'.
    WHEN 'C'.
  ENDCASE.

  LOOP AT lt_afo_head.
    lv_qc_status = 'C'.

    READ TABLE lt_afo_item WITH KEY afono = lt_afo_head-afono qc_status = 'B' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0." 明细有 B状态整单状态就为B
      lv_qc_status = 'B'.
    ELSE.
      READ TABLE lt_afo_item WITH KEY afono = lt_afo_head-afono qc_status = 'C' TRANSPORTING NO FIELDS.
      IF sy-subrc = 4." 明细没有B，C状态整单状态就为A
        lv_qc_status = 'A'.
      ENDIF.
    ENDIF.

    UPDATE zafo_head SET qc_status = lv_qc_status
     WHERE afono = lt_afo_head-afono
     AND bustyp = lv_afo_bustyp.
  ENDLOOP.

ENDFORM.


FORM frm_th_afo USING status.
  DATA:cs_head TYPE zafo_head.
  DATA:ct_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:lt_rk_item TYPE TABLE OF zafo_item WITH HEADER LINE.

  CHECK status = 'C' .
  CHECK gv_qcmode = 'B'.

  READ TABLE gt_head_modify INDEX 1.

  DELETE gt_head_sure WHERE qcno <> gt_head_modify-qcno.

  LOOP AT gt_head_sure WHERE qc_status <> 'C'."保证是最后一次质检数量确认
    RETURN.
  ENDLOOP.

  SELECT * INTO TABLE lt_rk_item
    FROM zafo_item
    FOR ALL ENTRIES IN gt_head_sure
    WHERE afono = gt_head_sure-afono
      AND afonr = gt_head_sure-afonr.
  IF sy-subrc NE 0 .
    RETURN.
  ENDIF.


  LOOP AT lt_rk_item ASSIGNING FIELD-SYMBOL(<fs_rk_item>).
    READ TABLE gt_head_sure WITH KEY afono = <fs_rk_item>-afono afonr = <fs_rk_item>-afonr.
    IF sy-subrc EQ 0 .
      <fs_rk_item>-menge4_bj = gt_head_sure-menge_kc.  " 库存数量
      <fs_rk_item>-menge5_bj = gt_head_sure-menge_qc_f." 退货数量

      IF <fs_rk_item>-menge4_bj < <fs_rk_item>-menge5_bj.
        MESSAGE '库存数量不够！' TYPE 'E'.
        g_error = 'X'.
        RETURN.
      ENDIF.

      <fs_rk_item>-lfgja = <fs_rk_item>-mjahr.
      <fs_rk_item>-lfbnr = <fs_rk_item>-mblnr.
      SELECT SINGLE zeile INTO <fs_rk_item>-lfpos
        FROM mseg
        WHERE bwart = '101'
          AND mjahr = <fs_rk_item>-mjahr
          AND mblnr = <fs_rk_item>-mblnr
          AND kdauf = <fs_rk_item>-vbeln_va
          AND kdpos = <fs_rk_item>-posnr_va
          AND matnr = <fs_rk_item>-matnr
          AND charg = <fs_rk_item>-charg
          AND menge = <fs_rk_item>-menge.
    ENDIF.
  ENDLOOP.

  DATA:lv_afonr TYPE zafonr.
  LOOP AT lt_rk_item WHERE menge5_bj > 0.
    MOVE-CORRESPONDING lt_rk_item TO ct_item.

    ADD 1 TO lv_afonr.
    ct_item-afonr = lv_afonr.
    ct_item-menge2 = ct_item-menge." 收货数量为采购入库
    ct_item-menge2_bj = ct_item-menge_cg.
    ct_item-menge3 = ct_item-menge." 未清数量为采购入库
    ct_item-menge3_bj = ct_item-menge_cg.

    ct_item-menge_cg = ct_item-menge5_bj .
    IF ct_item-menge_cg = ct_item-menge3_bj." 全退的时候取入库基本单位数量，避免反算尾差
      ct_item-menge = ct_item-menge3.
    ELSE.
      ct_item-menge = ct_item-menge5_bj * ct_item-zmm_tran_rate.
    ENDIF.

    ct_item-menge4 = ct_item-menge4_bj * ct_item-zmm_tran_rate.
    APPEND ct_item TO ct_item.
  ENDLOOP.

  CHECK ct_item[] IS NOT INITIAL.

  cs_head-remark1 =  '入库单:' && gt_head_modify-afono && ',质检单:'  && gt_head_modify-qcno && '退货'.

  SELECT SINGLE zstaff,remark2,remark_date
    INTO (@cs_head-zstaff,@cs_head-remark2,@cs_head-remark_date)
    FROM zafo_head
    WHERE afono = @gt_head_modify-afono.

  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CALL FUNCTION 'ZAFO_CREATE_SAVE'
    EXPORTING
      i_bustyp  = 'R1004'
*     NO_COMMIT =
*     NO_AUTHCHECK       =
*     NO_SAVE   =
      i_post    = 'X'
    TABLES
      et_return = lt_return[]
      ct_item   = ct_item[]
    CHANGING
      cs_head   = cs_head
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.

  IF sy-subrc <> 0 OR cs_head-status <> 'S'.
    g_error = 'X'.
  ENDIF.

  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0 .
    MESSAGE lt_return-message TYPE 'E'.
    g_error = 'X'.
  ENDIF.

ENDFORM.


FORM frm_del_afo_db USING status." 因为老单子zafo_item没有  qcnr 所以加这个FROM 删除
  DATA:lt_afo_item TYPE TABLE OF zafo_item WITH HEADER LINE.

  CHECK gt_head_modify[] IS NOT INITIAL.

  LOOP AT gt_head_modify.
    IF gt_head_modify-afono IS INITIAL
      OR gt_head_modify-qcno IS INITIAL
      OR gt_head_modify-qcnr IS INITIAL.
      MESSAGE '质检单号为空，更新失败' TYPE 'E'.
      g_error = 'X'.
      RETURN.
    ENDIF.
  ENDLOOP.

  SELECT * INTO TABLE lt_afo_item
    FROM zafo_item
    FOR ALL ENTRIES IN gt_head_modify
    WHERE afono = gt_head_modify-afono
      AND qcno = gt_head_modify-qcno.
  DATA(line) = lines( lt_afo_item[] ).
  IF lt_afo_item[] IS INITIAL OR line > 300.
    g_error = 'X'.
    MESSAGE '更新失败' TYPE 'E'.
    RETURN.
  ENDIF.


  LOOP AT lt_afo_item.

    lt_afo_item-qc_status = status.

    READ TABLE gt_head_modify WITH KEY afono = lt_afo_item-afono
                                        qcno = gt_head_modify-qcno.
    IF sy-subrc EQ 0.

      CASE status.
        WHEN 'A'.
          lt_afo_item-qcno = ''.
          lt_afo_item-qcnr = ''.
        WHEN 'B'.
          lt_afo_item-qcno = gt_head_modify-qcno.
          lt_afo_item-qcnr = gt_head_modify-qcnr.
        WHEN 'C'.
          lt_afo_item-qcno = gt_head_modify-qcno.
          lt_afo_item-qcnr = gt_head_modify-qcnr.
      ENDCASE.

      IF gv_qcmode = 'A'..
        lt_afo_item-item_status =  status.
      ENDIF.

    ENDIF.


    MODIFY lt_afo_item.
    CLEAR lt_afo_item.
  ENDLOOP.

  PERFORM frm_save_afo_head_db USING status.

  MODIFY zafo_item FROM TABLE lt_afo_item .
  IF sy-subrc NE 0.
    g_error = 'X'.
    ROLLBACK WORK.
    MESSAGE '更新失败' TYPE 'E'.
  ENDIF.
ENDFORM.


FORM frm_save_db.

  IF NOT gt_head_modify[] IS INITIAL.
    MODIFY zafo_qc_head FROM TABLE gt_head_modify[].
    CLEAR gt_head_modify[].
  ENDIF.

  IF NOT gt_item_modify[] IS INITIAL.
    MODIFY zafo_qc_item FROM TABLE gt_item_modify[].
    CLEAR gt_item_modify[].
  ENDIF.

  IF NOT gt_item_t[] IS  INITIAL.
    MODIFY zafo_qc_item_t FROM TABLE gt_item_t[].
    CLEAR gt_item_t[].
  ENDIF.

  IF NOT gt_item_i[] IS  INITIAL.
    MODIFY zafo_qc_item_i FROM TABLE gt_item_i[].
    CLEAR gt_item_i[].
  ENDIF.

  COMMIT WORK.

ENDFORM.
