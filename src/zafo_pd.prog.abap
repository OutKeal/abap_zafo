*&---------------------------------------------------------------------*
*& Report ZAFO_PD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo_pd.


INCLUDE zafo_pd_top.
INCLUDE zafo_pd_sel.
INCLUDE zafo_pd_alv_0100.
INCLUDE zafo_pd_alv_0200.
INCLUDE zafo_pd_alv_0300.
INCLUDE zafo_pd_f01.

INITIALIZATION.
  GET PARAMETER ID 'ZAPP_JUMP' FIELD gv_jump.
  SET PARAMETER ID 'ZAPP_JUMP' FIELD abap_false.

  IF sy-calld = 'X' OR gv_jump = abap_true.
    p_dis = 'X'.
    CLEAR p_cre.
  ELSE.
    p_cre = 'X'.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  PERFORM frm_set_sel_screen.

START-OF-SELECTION.

  CASE 'X'.
    WHEN  p_cre .

      AUTHORITY-CHECK OBJECT 'ZAFO_PD'
             ID 'WERKS' FIELD p_werks
             ID 'ACTVT' FIELD '01'.
      IF sy-subrc NE 0.
        MESSAGE '无权限' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      PERFORM frm_get_stock.

      CALL SCREEN 100.

    WHEN  p_dis .
*      AUTHORITY-CHECK OBJECT 'ZAFO_PD'
*             ID 'WERKS' FIELD p_werks
*             ID 'ACTVT' FIELD '03'.
*      IF sy-subrc NE 0.
*        MESSAGE '无权限' TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.
      IF sy-calld = 'X' OR gv_jump = abap_true.
        SELECT SINGLE * FROM zafo_pd_head
          WHERE afopd = @p_afopd
          INTO @gt_pd_head.

        SELECT * FROM zafo_pd_item
          WHERE afopd = @p_afopd
          INTO TABLE @gt_pd_item.

        PERFORM init_head_text.
        g_change = 'D'.

        CALL SCREEN 200.

      ELSE.
        PERFORM frm_get_pd.
        CALL SCREEN 300.
      ENDIF.

  ENDCASE.
