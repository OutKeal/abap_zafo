*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_CHECK
*&---------------------------------------------------------------------*

FORM frm_unsure_check."全部不合格校验
  DATA:lv_msg TYPE char40.

  PERFORM frm_check_rk.

  CHECK g_error IS INITIAL.

ENDFORM.


FORM frm_del_check."删除质检报告校验
  DATA:lv_msg TYPE char40.

  PERFORM frm_check_rk.

  PERFORM frm_check_th.

  PERFORM frm_check_dz.

  CHECK g_error IS INITIAL.

  IF <gs_head>-qc_status <> 'B' AND <gs_head>-qcno IS INITIAL.
    MESSAGE '不是检验中状态,无法删除质检报告.' TYPE 'S' DISPLAY LIKE 'E'.
    g_error = 'X'.
    RETURN.
  ENDIF.

  SELECT SINGLE qcnr INTO @DATA(lv_qcnr)
    FROM zafo_qc_item
    WHERE qcno = @gs_head-qcno
      AND qc_status <> 'A'.
  IF sy-subrc EQ 0.
    lv_msg = '选中行已有执行中的质检报告,是否仍要取消?'.
  ELSE.
    lv_msg = '是否确认删除选中行?'.
  ENDIF.

  PERFORM frm_pop_confirm USING lv_msg.

ENDFORM.


FORM frm_check_rk.
  DATA:lv_msg TYPE char40.

  CHECK gv_qcmode = 'A'.

  SELECT SINGLE  h~* INTO @DATA(ls_zafo_head)
    FROM zafo_head AS h
    LEFT JOIN zafo_item AS i ON h~afono EQ i~afono
    WHERE h~bustyp = 'R1002'
      AND h~status NE 'D'
      AND i~qcno EQ @gs_head-qcno.
  IF sy-subrc EQ 0 .
    lv_msg = '质检单:' && gs_head-qcno && ' 已存在入库单，不容许执行此操作！' .
    g_error = 'X'.
    MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.


FORM frm_check_th.
  DATA:lv_msg TYPE char40.

  CHECK gv_qcmode = 'B'.

  SELECT SINGLE  h~* INTO @DATA(ls_zafo_head)
    FROM zafo_head AS h
    LEFT JOIN zafo_item AS i ON h~afono EQ i~afono
    WHERE h~bustyp = 'R1004'
      AND h~status NE 'D'
      AND i~qcno EQ @gs_head-qcno.
  IF sy-subrc EQ 0 .
    lv_msg = '质检单(' && gs_head-qcno && '):已存在退货单(' && ls_zafo_head-afono && ')，不容许执行此操作！' .
    g_error = 'X'.
    MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.


FORM frm_check_dz.
  DATA:lv_msg TYPE char40.

  CHECK gv_qcmode = 'B'.

    SELECT SINGLE i~*  INTO @DATA(ls_ztmm013)
      FROM ztmm013_item AS i
      LEFT JOIN ztmm013_head AS h ON i~lelnr = h~lelnr
      WHERE h~zstat <> 'D' AND afono = @gs_head-afono.
  IF sy-subrc EQ 0 .
    lv_msg = '入库单(' && gs_head-afono && '):已经对账(' && ls_ztmm013-lelnr && ')，不容许执行此操作！' .
    g_error = 'X'.
    MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.
