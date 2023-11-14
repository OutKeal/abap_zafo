*&---------------------------------------------------------------------*
*& 包含               ZAFO_RULE_ABAP_DISPLAY
*&---------------------------------------------------------------------*

DATA:g_container TYPE REF TO cl_gui_custom_container.
DATA:
      g_editor TYPE REF TO cl_gui_abapedit.
TYPES: txt_line TYPE string,
       txt      TYPE STANDARD TABLE OF txt_line
                  WITH DEFAULT KEY.

DATA:g_code TYPE txt.

FORM frm_init_abap_edit.
  IF g_editor IS NOT INITIAL.
    g_editor->free( ).
    FREE g_editor.
    g_container->free( ).
    FREE g_container.
  ENDIF.
  CREATE OBJECT g_container
    EXPORTING
      container_name = 'CUSTOM_CONTAINER'.

  CREATE OBJECT g_editor
    EXPORTING
      parent = g_container.

  g_editor->set_toolbar_mode( 1 ).
  g_editor->set_statusbar_mode( 1 ).
  g_editor->set_readonly_mode( 0 ).
  g_editor->set_text( g_code ).
ENDFORM.

FORM frm_abap_check  CHANGING error_flag.
  DATA:source TYPE txt.
  DATA idx TYPE sy-tabix.
  DATA subrc TYPE sy-subrc.
  DATA replace_code TYPE string.
  DATA: mess     TYPE string,
        lin      TYPE i ##needed,
        wrd      TYPE string ##needed,
        warnings TYPE  STANDARD TABLE OF rslinlmsg.

  g_editor->get_text( IMPORTING table = g_code ).
  CASE g_action.
    WHEN 'MOVE_VALUE'.
      replace_code = '* implementation_101'.
    WHEN 'BEFORE_POST'.
      replace_code = '* implementation_before_post'.
    WHEN 'AFTER_POST'.
      replace_code = '* implementation_after_post'.
  ENDCASE.

  TRY.
      READ REPORT zafo_run=>abap_template INTO source.
      subrc = sy-subrc.
    CATCH cx_sy_read_src_line_too_long.
      subrc = 4.
  ENDTRY.

  IF subrc = 0.
    FIND replace_code IN TABLE source MATCH LINE idx.
    subrc = sy-subrc.
    DELETE source INDEX idx.
    INSERT LINES OF g_code INTO source INDEX idx.
  ENDIF.

  SYNTAX-CHECK FOR source MESSAGE mess LINE lin WORD wrd
  ID 'MSG' TABLE warnings
  PROGRAM 'zafo_RULE_ROUTINE_TEMPLATE'.
  IF sy-subrc NE 0.
    error_flag = 'X'.
    MESSAGE mess TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE '代码检查通过' TYPE 'S' .
  ENDIF.

ENDFORM.

FORM frm_abap_save.
  CLEAR g_code.
  g_editor->get_text( IMPORTING table = g_code ).
  DELETE  gt_code WHERE action = g_action AND to_tabname = gs_func-parent AND to_fieldname = gs_func-fieldname.
  DELETE  FROM zafo_rule_code WHERE rule_name = gs_rule-rule_name AND action = g_action AND to_tabname = gs_func-parent AND to_fieldname = gs_func-fieldname.



  LOOP AT g_code INTO DATA(l_code).
    APPEND VALUE #( rule_name = gs_rule-rule_name
                                    action = g_action
                                    to_tabname = gs_func-parent
                                    to_fieldname = gs_func-fieldname
                                    line = sy-tabix
                                    code = l_code
                                  ) TO gt_code.
  ENDLOOP.
  MODIFY zafo_rule_code FROM TABLE gt_code.
  COMMIT WORK AND WAIT.
ENDFORM.

FORM frm_abap_format.

  g_editor->get_text( IMPORTING table = g_code ).
  DATA:
    l_pp       TYPE REF TO cl_sedi_pretty_printer,
    l_exc      TYPE REF TO cx_sedi_pretty_printer,
    l_settings TYPE REF TO if_pretty_printer_settings.
  DATA:buffer TYPE  rswsourcet.
  CREATE OBJECT l_pp.

  TRY.
      buffer = g_code.
      CREATE OBJECT l_settings TYPE cl_pretty_printer_wb_settings.
      l_pp->format_source(
      EXPORTING
        i_include  = zafo_run=>abap_template
        i_settings = l_settings
      CHANGING
        c_source   = buffer ).
    CATCH cx_sedi_pretty_printer INTO l_exc.
      MESSAGE l_exc TYPE sy-msgty DISPLAY LIKE 'S'.
      RETURN.
  ENDTRY.

  g_code = buffer.
  g_editor->set_text(  g_code ).
ENDFORM.
