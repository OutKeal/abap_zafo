*&---------------------------------------------------------------------*
*& Report ZAFO_RULE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo_rule.

INCLUDE zafo_rule_top.

INCLUDE zafo_rule_abap_display.

INCLUDE zafo_rule_f01.

INCLUDE zafo_rule_pbo_pai.

START-OF-SELECTION.

  PERFORM frm_get_data.

  IF gt_rule[] IS INITIAL.
    STOP.
  ELSEIF lines( gt_rule ) = 1.
    gs_rule =  gt_rule[ 1 ].
    PERFORM frm_get_single_data.
    CALL SCREEN 100.
  ELSE.
    g_falv_list = zwft_falv=>create( CHANGING ct_table = gt_rule ).
    g_falv_list->display( ).
  ENDIF.
