*&---------------------------------------------------------------------*
*& 包含               ZAFO_PAI
*&---------------------------------------------------------------------*

MODULE exit_command INPUT.

  PERFORM frm_free_object.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      CLEAR sy-ucomm.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'EXIT' OR 'QUIT'.
      CLEAR sy-ucomm.
      LEAVE PROGRAM.

    WHEN OTHERS .
      CLEAR sy-ucomm.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " EXIT_COMMAND  INPUT

MODULE user_command_0100 INPUT.
  <io_class>->user_command_ref( EXPORTING c_falv = <io_class>->falv_ref
                                                                            i_ucomm = sy-ucomm ).
ENDMODULE.

MODULE user_command_0110 INPUT.
  IF sy-ucomm = '&SWITCH'.
    ref_in_switch = COND #( WHEN ref_in_switch = icon_data_area_expand
                                             THEN icon_data_area_collapse
                                              ELSE icon_data_area_expand ).
    PERFORM frm_set_dynp_value USING  'REF_IN_SWITCH' ref_in_switch.
    CLEAR sy-ucomm.
  ENDIF.
ENDMODULE.

MODULE user_command_0200 INPUT.
  IF sy-ucomm+0(1) = '%'.
    PERFORM frm_page_trun USING sy-ucomm.
  ELSE.
    <io_class>->user_command_main( sy-ucomm ).
  ENDIF.
ENDMODULE.

MODULE user_command_0300 INPUT.
  <io_class>->user_command_report( sy-ucomm ).
ENDMODULE.

MODULE tag0200_active_tab_get INPUT.

  CASE sy-ucomm.
    WHEN c_tag0200-tab1.
      g_tag0200-pressed_tab = c_tag0200-tab1.
    WHEN c_tag0200-tab2.
      g_tag0200-pressed_tab = c_tag0200-tab2.
    WHEN c_tag0200-tab3.
      g_tag0200-pressed_tab = c_tag0200-tab3.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

MODULE get_cursor INPUT.
  IF sy-dynnr = '0400'.
    GET CURSOR FIELD cursor_400.
  ELSEIF cursor_400 IS NOT INITIAL.
    cursor = cursor_400.
  ELSE.
    GET CURSOR FIELD cursor.
  ENDIF.
ENDMODULE.

MODULE mwskz_list INPUT.
  PERFORM frm_mwskz_list.
ENDMODULE.

MODULE vtweg_list INPUT.
  PERFORM frm_vtweg_list.
ENDMODULE.


MODULE lfart_list INPUT.ENDMODULE.

MODULE lgort_list INPUT.
  PERFORM frm_lgort_list.
ENDMODULE.

MODULE umlgo_list INPUT.
  PERFORM frm_umlgo_list.
ENDMODULE.

MODULE bukrs_list INPUT.
  PERFORM frm_bukrs_list.
ENDMODULE.

MODULE kostl_list INPUT.
  PERFORM frm_kostl_list.
ENDMODULE.

MODULE po_ekgrp_list INPUT.
  PERFORM frm_po_ekgrp_list.
ENDMODULE.

MODULE po_bsart_list INPUT.
  PERFORM frm_po_bsart_list.
ENDMODULE.

MODULE pr_bsart_list INPUT.
  PERFORM frm_pr_bsart_list.
ENDMODULE.

MODULE head-lifnr INPUT.
  PERFORM frm_head_lifnr.
ENDMODULE.

MODULE head-lifnr_f4 INPUT.
  PERFORM frm_head_lifnr_f4.
ENDMODULE.

MODULE head-lifnr_bukrs INPUT.
  PERFORM frm_head_lifnr_bukrs.
ENDMODULE.

MODULE head-lifnr_f4_bukrs INPUT.
  PERFORM frm_head_lifnr_f4_bukrs.
ENDMODULE.

MODULE head-kunnr INPUT.
  PERFORM frm_head_kunnr.
ENDMODULE.

MODULE head-kunnr_f4 INPUT.
  PERFORM frm_head_kunnr_f4.
ENDMODULE.

MODULE head-werks INPUT.
  PERFORM frm_head_werks..
ENDMODULE.

MODULE head-lgort INPUT.
  PERFORM frm_head_lgort.
ENDMODULE.

MODULE head-umwrk INPUT.
  PERFORM frm_head_umwrk.
ENDMODULE.

MODULE head-umlgo INPUT.
  PERFORM frm_head_umlgo.
ENDMODULE.


MODULE head-bukrs INPUT.
  PERFORM frm_head_bukrs.
ENDMODULE.

MODULE head-kostl INPUT.
  PERFORM frm_head_kostl.
ENDMODULE.


MODULE head-ekorg INPUT.ENDMODULE.
MODULE head-eeind INPUT.ENDMODULE.
MODULE head-vkorg INPUT.ENDMODULE.

MODULE head-dict01 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict02 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict03 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict04 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict05 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict06 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict07 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict08 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict09 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.

MODULE head-dict10 INPUT.
  PERFORM frm_set_dict .
ENDMODULE.
