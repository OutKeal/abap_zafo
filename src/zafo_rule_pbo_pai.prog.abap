*&---------------------------------------------------------------------*
*& 包含               ZAFO_RULE_PBO_PAI
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100' WITH gs_rule-rule_name gs_rule-bapi_name.
ENDMODULE.

MODULE create_object_0100 OUTPUT.
  PERFORM frm_init_container.
  PERFORM frm_init_treel.
  PERFORM frm_init_treer.
  PERFORM frm_init_falv.

ENDMODULE.

MODULE exit_command INPUT.
  CLEAR gt_func.
  CLEAR gt_at.
  CLEAR gs_rule.
  CLEAR gt_detail.
  g_tree_left->free( ).
  g_tree_right->free( ).
  FREE g_tree_left.
  FREE g_tree_right.
  LEAVE TO SCREEN 0.
ENDMODULE.

MODULE exit_command_0200 INPUT.
  CLEAR g_code.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA(ok_code) = sy-ucomm.
  CASE ok_code.
    WHEN 'REFRESH'.
      PERFORM frm_refresh_icon.
      PERFORM frm_set_icon.
    WHEN 'RELATE'.
      PERFORM frm_relate.
    WHEN 'DEFAULT'.
      PERFORM frm_default.
    WHEN 'LAST'.
      PERFORM frm_last.
    WHEN 'SAVE'.
      PERFORM frm_save.
    WHEN 'ROUTINE'.
      PERFORM frm_routine.
    WHEN 'DELETE'.
      PERFORM frm_delete.
    WHEN 'HTML'.
      PERFORM frm_init_html.
*        CALL SCREEN '300'.
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.

MODULE user_command_0200 INPUT.
  cl_gui_cfw=>flush( ).
  DATA:error_flag TYPE char1.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'ABAP_CHECK'.
      CLEAR error_flag.
      PERFORM frm_abap_check  CHANGING error_flag.
    WHEN 'ABAP_SAVE'.
      CLEAR error_flag.
      PERFORM frm_abap_check  CHANGING error_flag.
      CHECK error_flag IS INITIAL.
      PERFORM frm_abap_save.
    WHEN 'FORMAT'.
      PERFORM frm_abap_format.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  DATA:l_text TYPE char100.
  SET PF-STATUS '200'.
  SET TITLEBAR  '200'.
  CASE g_action.
    WHEN 'MOVE_VALUE'.
      l_text = |{ gs_func-parent }-{ gs_func-fieldname } = { gs_at-parent }-{ gs_at-fieldname }|.
    WHEN 'BEFORE_POST'.
      l_text = '( BEFORE_POST )记账前处理'.
    WHEN 'AFTER_POST'.
      l_text = '( AFTER_POST )记账前处理'.
  ENDCASE.
  SET PF-STATUS '200'.
  SET TITLEBAR  '200' WITH l_text.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS '300'.
  SET TITLEBAR '300'.

  PERFORM frm_init_html.
ENDMODULE.
