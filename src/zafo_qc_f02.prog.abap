*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_F02
*&---------------------------------------------------------------------*

FORM frm_pop_sure.

  CLEAR g_ucomm.
  CLEAR g_menge_qc_f.
  CLEAR g_menge_qc.
  g_menge_qc_f = 0.
  g_menge_qc_f1 = 0.
  g_menge_qc = gs_head-menge_gr - gs_head-menge_qc - gs_head-menge_qc_f.

  CALL SCREEN 103 STARTING AT 50 5  ENDING AT 100 15.

  CHECK g_ucomm = '&OK'.

  IF g_menge_qc < 0 OR g_menge_qc_f < 0.
    MESSAGE '合格或不合格数量合不能小于0,请重新填入' TYPE 'I'.
    g_error = 'X'.
    RETURN.
  ENDIF.

  g_menge_qc = gs_head-menge_qc + g_menge_qc.
  g_menge_qc_f = gs_head-menge_qc_f + g_menge_qc_f.

  IF gs_head-menge_gr < g_menge_qc.
    MESSAGE '合格数量大于收货数量,请重新填入' TYPE 'I'.
    g_error = 'X'.
    RETURN.
  ENDIF.

  gs_head-menge_qc_f = g_menge_qc_f.
  gs_head-menge_qc  = g_menge_qc.

  CLEAR g_menge_qc.
  CLEAR g_menge_qc_f.
  CLEAR g_menge_qc_f1.

  IF gs_head-menge_gr - gs_head-menge_qc - gs_head-menge_qc_f > 0.
    gs_head-qc_status = 'B'.
  ELSE.
    gs_head-qc_status = 'C'.
  ENDIF.
  gs_head-monam = sy-uname.
  gs_head-modat = sy-datum.
  gs_head-mozet = sy-uzeit.

  PERFORM frm_set_icon USING gs_head-qc_status CHANGING gs_head-icon gs_head-text.

ENDFORM.


FORM frm_set_round USING meins CHANGING value.
  DATA: lv_round TYPE zafo_round.

  CHECK meins IS NOT INITIAL.

  SELECT SINGLE andec INTO @DATA(ls_andec) FROM t006 WHERE msehi = @meins.
  IF sy-subrc = 0.
    lv_round = ls_andec.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals = lv_round
        input    = value
        sign     = '+'
      IMPORTING
        output   = value.
  ENDIF.
ENDFORM.
