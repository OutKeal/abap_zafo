*&---------------------------------------------------------------------*
*& report zafo_qc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT zafo_qc.

INCLUDE zafo_qc_top.

INCLUDE zafo_qc_sel.

INCLUDE zafo_qc_alv.

INCLUDE zafo_qc_alv_200.

INCLUDE zafo_qc_pbo.

INCLUDE zafo_qc_pai.

INCLUDE zafo_qc_f01.

INCLUDE zafo_qc_f02.

INCLUDE zafo_qc_check.

INCLUDE zafo_qc_data.

INCLUDE zafo_qc_save.

INCLUDE zafo_qc_alv_400.

INCLUDE zafo_qc_alv_500.

INCLUDE zafo_qc_excel.

INITIALIZATION.
  PERFORM frm_sel_ini.

AT SELECTION-SCREEN OUTPUT.

  PERFORM frm_set_sel_screen.

START-OF-SELECTION.

  CASE 'X'.
    WHEN p_cre.
      IF gv_qcmode = 'A'.
        PERFORM frm_create_sh.

      ELSEIF gv_qcmode = 'B'.
        PERFORM frm_create_rk.

      ELSEIF gv_qcmode = 'C'.
        PERFORM frm_create_cg.

      ENDIF.

    WHEN p_dis.
      IF gv_qcmode = 'Q'.
        PERFORM frm_get_data_cx.
      ELSE.
        PERFORM frm_get_data.
      ENDIF.

  ENDCASE.


  IF gv_qcmode = 'Q'.
    IF gt_item[] IS INITIAL.
      MESSAGE '无有效数据' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    CALL SCREEN 0200.

  ELSE.
    IF gt_head[] IS INITIAL.
      MESSAGE '无有效数据' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    DATA: lr_zzpino TYPE RANGE OF zzpino.
    PERFORM frm_set_zzpino_hide IN PROGRAM saplzafo IF FOUND TABLES lr_zzpino.
    IF lr_zzpino[] IS NOT INITIAL.
      DELETE gt_head WHERE zzpino IN lr_zzpino.
    ENDIF.

    PERFORM frm_get_kc." 获取质检单对应库存数
    CALL SCREEN 0100.

  ENDIF.
