*&---------------------------------------------------------------------*
*& 包含               ZAFO_I08_TAB_650
*&---------------------------------------------------------------------*

CONTROLS: tag0650 TYPE TABSTRIP.

CONSTANTS: BEGIN OF c_tag0650,
             tab1 LIKE sy-ucomm VALUE 'TAG0650_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAG0650_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAG0650_FC3',
           END OF c_tag0650.
DATA: BEGIN OF g_tag0650,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'SAPLZAFO',
        pressed_tab LIKE sy-ucomm VALUE c_tag0650-tab1,
      END OF g_tag0650.

DATA editor TYPE REF TO cl_gui_textedit.
DATA text_container TYPE REF TO cl_gui_custom_container.


MODULE tag0650_active_tab_set OUTPUT.
  tag0650-activetab = g_tag0650-pressed_tab.
  CASE g_tag0650-pressed_tab.
    WHEN c_tag0650-tab1.
      g_tag0650-subscreen = '0660'.
    WHEN c_tag0650-tab2.
      g_tag0650-subscreen = '0670'.
    WHEN c_tag0650-tab3.
      g_tag0650-subscreen = '0680'.
  ENDCASE.
ENDMODULE.


MODULE tag0650_active_tab_get INPUT.

  CASE sy-ucomm.
    WHEN c_tag0650-tab1.
      g_tag0650-pressed_tab = c_tag0650-tab1.
    WHEN c_tag0650-tab2.
      g_tag0650-pressed_tab = c_tag0650-tab2.
    WHEN c_tag0650-tab3.
      g_tag0650-pressed_tab = c_tag0650-tab3.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.


MODULE set_editor_text INPUT.

  CHECK gs_head-tmodel IS NOT INITIAL.
  CHECK g_readonly <> 'D'.

  IF  g_f4_contract = 'X'.
    CLEAR g_f4_contract.
    RETURN.
  ENDIF.

  CLEAR gt_line[].

  SELECT line FROM zafo_contract_t
    WHERE tmodel = @gs_head-tmodel
    ORDER BY line_id
    INTO TABLE @gt_line.

  IF sy-subrc EQ 0.
    editor->set_text_as_stream(  text = gt_line ) .
  ENDIF.
ENDMODULE.


FORM free_object.
  IF editor IS BOUND.
    CALL METHOD editor->free.
    FREE editor.
  ENDIF.
  IF text_container IS BOUND.
    CALL METHOD text_container->free.
    FREE text_container.
  ENDIF.

ENDFORM.


MODULE gs_head-tmodel_f4 INPUT.
  DATA:lt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  SELECT DISTINCT tmodel
    INTO TABLE @DATA(t_model_values)
    FROM zafo_contract_t.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TMODEL'
      pvalkey         = 'TMODEL'
      dynprofield     = 'TMODEL'
      value_org       = 'S'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
    TABLES
      value_tab       = t_model_values[]
      return_tab      = lt_return_tab[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CHECK lt_return_tab[] IS NOT INITIAL.
  LOOP AT lt_return_tab.
    gs_head-tmodel = lt_return_tab-fieldval.
  ENDLOOP.

  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = 'TMODEL'.

  CALL METHOD cl_gui_cfw=>flush.

  CHECK gs_head-tmodel IS NOT INITIAL.
  CHECK g_readonly <> 'D'.

  CLEAR gt_line[].

  SELECT line FROM zafo_contract_t
    WHERE tmodel = @gs_head-tmodel
    ORDER BY line_id
    INTO TABLE @gt_line.

  IF sy-subrc EQ 0.
    editor->set_text_as_stream(  text = gt_line ) .
    g_f4_contract = 'X'.
  ENDIF.

ENDMODULE.


MODULE  set_editor OUTPUT.
  CHECK text_container IS NOT BOUND.

  CREATE OBJECT: text_container EXPORTING container_name = 'EDITOR'.

  CREATE OBJECT editor
    EXPORTING
*     max_number_chars           = 180
      parent                     = text_container
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 256
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

  editor->set_statusbar_mode( cl_gui_textedit=>false ).
  editor->set_toolbar_mode( cl_gui_textedit=>false ).

  IF gs_head-ebeln IS NOT INITIAL OR gs_head-afono IS NOT INITIAL..
    PERFORM frm_read_text.

    IF gt_line IS NOT INITIAL.
      editor->set_text_as_stream(  text = gt_line ) .
    ENDIF.
  ENDIF.
ENDMODULE.


MODULE set_editor_screen OUTPUT.
  IF g_readonly = 'D'.
    editor->set_readonly_mode( 1 ).
  ELSE.
    editor->set_readonly_mode( 0 ).
  ENDIF.
ENDMODULE.


FORM frm_read_text.
  DATA: ls_header TYPE thead.

  REFRESH: gt_tline.
  REFRESH: gt_line.

  IF gs_head-ebeln IS NOT INITIAL.
    ls_header-tdid = 'F01'.
    ls_header-tdobject =  'EKKO'.
    ls_header-tdspras = sy-langu.
    CONCATENATE gs_head-ebeln ls_header-tdid INTO ls_header-tdname.
  ELSEIF gs_head-afono IS NOT INITIAL.
    ls_header-tdid = 'F01'.
    ls_header-tdobject =  'ZAFO'.
    ls_header-tdspras = sy-langu.
    CONCATENATE gs_head-afono ls_header-tdid INTO ls_header-tdname.
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = ls_header-tdid
      language                = ls_header-tdspras
      name                    = ls_header-tdname
      object                  = ls_header-tdobject
    TABLES
      lines                   = gt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
    EXPORTING
      language    = sy-langu
    TABLES
      itf_text    = gt_tline
      text_stream = gt_line.
ENDFORM.


FORM frm_save_text.
  DATA:ls_header TYPE thead.

  CHECK editor IS BOUND.

  CALL METHOD cl_gui_cfw=>flush.

  REFRESH: gt_tline.
  REFRESH: gt_line.

  editor->get_text_as_stream( IMPORTING text = gt_line ).

  CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
    EXPORTING
      language    = sy-langu
    TABLES
      text_stream = gt_line
      itf_text    = gt_tline.

  CHECK gt_tline[] IS NOT INITIAL.

  IF gs_head-ebeln IS NOT INITIAL.
    ls_header-tdid = 'F01'.
    ls_header-tdobject =  'EKKO'.
    ls_header-tdspras = sy-langu.
    CONCATENATE gs_head-ebeln ls_header-tdid INTO ls_header-tdname.
  ELSEIF gs_head-afono IS NOT INITIAL.
    ls_header-tdid = 'F01'.
    ls_header-tdobject =  'ZAFO'.
    ls_header-tdspras = sy-langu.
    CONCATENATE gs_head-afono ls_header-tdid INTO ls_header-tdname.
  ENDIF.
  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = ls_header
      savemode_direct = 'X'
    TABLES
      lines           = gt_tline[]
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.
