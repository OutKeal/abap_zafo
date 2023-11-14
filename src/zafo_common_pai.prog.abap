*&---------------------------------------------------------------------*
*& 包含               ZAFO_PAI
*&---------------------------------------------------------------------*

MODULE exit_command INPUT.

  IF sy-dynnr = '0200'.
    <io_class>->free( ).
    IF g_container_200 IS BOUND.
      g_splitter_200->free( ).
      g_container_200->free( ).
      FREE g_splitter_200.
      FREE g_container_200.
      IF text_container IS BOUND.
        text_container->free( ).
        FREE text_container.
      ENDIF.
    ENDIF.
  ENDIF.

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
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT_COMMAND  INPUT

MODULE user_command_0100 INPUT.
  <io_class>->user_command_ref( EXPORTING c_falv = <io_class>->falv_ref i_ucomm = sy-ucomm ).
ENDMODULE.

MODULE user_command_0200 INPUT.
  <io_class>->user_command_main( sy-ucomm ).
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
MODULE vtweg_list INPUT.ENDMODULE.
MODULE lfart_list INPUT.ENDMODULE.
MODULE lgort_list INPUT.
  PERFORM frm_lgort_list.
ENDMODULE.

MODULE umlgo_list INPUT.
  PERFORM frm_umlgo_list.
ENDMODULE.

MODULE pr_bsart_list INPUT.
  PERFORM frm_pr_bsart_list.
ENDMODULE.

MODULE head-tmodel_list INPUT.
  PERFORM frm_tmodel_list.
ENDMODULE.

MODULE head-lifnr INPUT.
  SELECT SINGLE
    name1 INTO <io_class>->head-lifnr_name
    FROM lfa1
    WHERE lifnr = <io_class>->head-lifnr.

  SELECT SINGLE"绝大多数情况下供应商的币种唯一
    waers INTO <io_class>->head-waers
    FROM lfm1
    WHERE lifnr = <io_class>->head-lifnr.
ENDMODULE.

MODULE head-werks INPUT.
  IF <io_class>->head-werks IS INITIAL.
    CLEAR <io_class>->head-werks_name.
    RETURN.
  ELSE.
    SELECT SINGLE name1 FROM t001w
      WHERE werks = @<io_class>->head-werks
      INTO @<io_class>->head-werks_name.
    IF sy-subrc NE 0.
      MESSAGE i045 .
      CLEAR: <io_class>->head-werks,<io_class>->head-werks_name.
    ENDIF.
  ENDIF.
ENDMODULE.

MODULE head-lgort INPUT.
  IF <io_class>->head-werks IS INITIAL.
    MESSAGE i005."请输入工厂
    CLEAR: <io_class>->head-lgort,<io_class>->head-lgort_name.
    RETURN.
  ENDIF.

  IF <io_class>->head-lgort IS INITIAL.
    CLEAR <io_class>->head-lgort_name.
    RETURN.
  ENDIF.

  SELECT SINGLE lgobe
    INTO <io_class>->head-lgort_name
    FROM t001l
    WHERE werks = <io_class>->head-werks
    AND lgort = <io_class>->head-lgort.
  IF sy-subrc NE 0.
    MESSAGE i115 ."仓库不存在
    CLEAR: <io_class>->head-lgort,<io_class>->head-lgort_name.
  ENDIF.
ENDMODULE.

MODULE head-umwrk INPUT.
  IF <io_class>->head-umwrk IS INITIAL.
    CLEAR <io_class>->head-umwrk_name.
    RETURN.
  ELSE.
    SELECT SINGLE name1 FROM t001w
    WHERE werks = @<io_class>->head-umwrk
    INTO @<io_class>->head-umwrk_name.
    IF sy-subrc NE 0.
      MESSAGE i045 .
      CLEAR: <io_class>->head-umwrk,<io_class>->head-umwrk_name.
    ENDIF.
  ENDIF.
ENDMODULE.


MODULE head-bukrs INPUT.
  IF <io_class>->head-bukrs IS INITIAL.
    CLEAR <io_class>->head-bukrs_name.
  ENDIF.

  SELECT SINGLE butxt FROM t001
  WHERE bukrs = @<io_class>->head-bukrs
  INTO @<io_class>->head-bukrs_name.
  IF sy-subrc NE 0.
    CLEAR: <io_class>->head-bukrs,<io_class>->head-bukrs_name.
  ENDIF.
ENDMODULE.

MODULE head-kostl INPUT.
  SELECT SINGLE ktext INTO <io_class>->head-kostl_name
    FROM cskt
    WHERE spras = sy-langu
    AND kostl = <io_class>->head-kostl.
ENDMODULE.

MODULE head-umlgo INPUT.
  IF <io_class>->head-umwrk IS INITIAL.
    MESSAGE i005."请输入工厂
    CLEAR: <io_class>->head-umlgo,<io_class>->head-umlgo_name.
    RETURN.
  ENDIF.

  IF <io_class>->head-umlgo IS INITIAL.
    CLEAR <io_class>->head-umlgo_name.
    RETURN.
  ENDIF.

  SELECT SINGLE lgobe
  INTO <io_class>->head-umlgo_name
  FROM t001l
  WHERE werks = <io_class>->head-umwrk
  AND lgort = <io_class>->head-umlgo.
  IF sy-subrc NE 0.
    MESSAGE i115 ."仓库不存在
    CLEAR: <io_class>->head-umlgo,<io_class>->head-umlgo_name.
  ENDIF.
ENDMODULE.

MODULE head-ekorg INPUT.ENDMODULE.
MODULE head-eeind INPUT.ENDMODULE.
MODULE head-vkorg INPUT.ENDMODULE.
MODULE head-kunnr INPUT.ENDMODULE.


MODULE head-dict01 INPUT.
  PERFORM frm_set_dict USING '01'.
ENDMODULE.

MODULE head-dict02 INPUT.
  PERFORM frm_set_dict USING '02'.
ENDMODULE.

MODULE head-dict03 INPUT.
  PERFORM frm_set_dict USING '03'.
ENDMODULE.

MODULE head-dict04 INPUT.
  PERFORM frm_set_dict USING '04'.
ENDMODULE.

MODULE head-dict05 INPUT.
  PERFORM frm_set_dict USING '05'.
ENDMODULE.

MODULE head-dict06 INPUT.
  PERFORM frm_set_dict USING '06'.
ENDMODULE.

MODULE head-dict07 INPUT.
  PERFORM frm_set_dict USING '07'.
ENDMODULE.

MODULE head-dict08 INPUT.
  PERFORM frm_set_dict USING '08'.
ENDMODULE.

MODULE head-dict09 INPUT.
  PERFORM frm_set_dict USING '09'.
ENDMODULE.

MODULE head-dict10 INPUT.
  PERFORM frm_set_dict USING '10'.
ENDMODULE.
