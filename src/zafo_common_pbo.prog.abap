*&---------------------------------------------------------------------*
*& 包含               ZAFO_PBO
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  DATA ex_code_100 TYPE TABLE OF sy-ucomm.
  IF <io_class>->bustype-busref = 'Y'.
    APPEND '&SELECTALL' TO ex_code_100.
  ENDIF.
  SET PF-STATUS '100' EXCLUDING ex_code_100.
  SET TITLEBAR '100' WITH <io_class>->bustype-bustyp '-' <io_class>->bustype-bustyp_name1.
ENDMODULE.

MODULE create_object_0100 OUTPUT.
  IF <io_class>->falv_ref IS INITIAL.
    g_splitter_100 = NEW cl_gui_splitter_container(
    parent = NEW cl_gui_docking_container( extension = '3000' )
    rows = 1 columns = 1 ).

    <io_class>->falv_ref = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = <io_class>->get_falv_handle( repid = sy-repid alv_name = 'FALV_REF' )
      i_parent = g_splitter_100->get_container( row = 1 column = 1 )
      CHANGING ct_table = <io_class>->item_ref ) .

    <io_class>->init_alv( <io_class>->falv_ref ).

    <io_class>->falv_ref->display( ).
  ELSE.
    <io_class>->falv_ref->soft_refresh( ).
  ENDIF.

ENDMODULE.

MODULE status_0200 OUTPUT.
  DATA(lt_fcodes) = <io_class>->set_gui_status_exclude( ).
  SET PF-STATUS '200' EXCLUDING lt_fcodes.
  SET TITLEBAR '200' WITH <io_class>->bustype-bustyp '-' <io_class>->bustype-bustyp_name1 .
  MOVE <io_class>->head TO zafo_shead.
ENDMODULE.


MODULE create_object_0200 OUTPUT.
  IF <io_class>->falv_item IS INITIAL.
    g_container_200 = NEW cl_gui_custom_container( 'ITEM' ).
    g_splitter_200 = NEW cl_gui_splitter_container(
    parent = g_container_200
    rows = 1 columns = 1 ).
    <io_class>->falv_item = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = <io_class>->get_falv_handle( repid = sy-repid alv_name = 'FALV_ITEM' )
      i_parent = g_splitter_200->get_container( row = 1 column = 1 )
    CHANGING ct_table = <io_class>->item ) .

    <io_class>->init_alv( <io_class>->falv_item ).
    <io_class>->falv_item->display( ).
  ELSE.
    <io_class>->falv_item->soft_refresh( ).
  ENDIF.

ENDMODULE.

MODULE tag0200_active_tab_set OUTPUT.
  tag0200-activetab = g_tag0200-pressed_tab.
  CASE g_tag0200-pressed_tab.
    WHEN c_tag0200-tab1.
      g_tag0200-subscreen = '0210'.
    WHEN c_tag0200-tab2.
      g_tag0200-subscreen = '0220'.
    WHEN c_tag0200-tab3.
      g_tag0200-subscreen = '0230'.
  ENDCASE.
ENDMODULE.

MODULE set_cursor OUTPUT.
*  CLEAR:g_ean.
*  SET CURSOR FIELD 'G_EAN'.
ENDMODULE.                 " SET_CURSOR

MODULE status_0300 OUTPUT.
  SET PF-STATUS '300' EXCLUDING lt_fcodes.
  SET TITLEBAR '300' WITH <io_class>->bustype-bustyp '-' <io_class>->bustype-bustyp_name1 .
ENDMODULE.

MODULE create_object_0300 OUTPUT.
  IF <io_class>->falv_dis_head IS INITIAL.
    g_splitter_300 = NEW cl_gui_splitter_container(
    parent = NEW cl_gui_docking_container( extension = '3000' )
    rows = 2 columns = 1 ).

    <io_class>->falv_dis_head = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = <io_class>->get_falv_handle( repid = sy-repid alv_name = 'FALV_DIS_HEAD' )
      i_parent = g_splitter_300->get_container( row = 1 column = 1 )
    CHANGING ct_table = <io_class>->head_dis ) .
    <io_class>->init_alv( <io_class>->falv_dis_head ).
    <io_class>->falv_dis_head->display( ).

    <io_class>->falv_dis_item = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = <io_class>->get_falv_handle( repid = sy-repid alv_name = 'FALV_DIS_ITEM' )
      i_parent = g_splitter_300->get_container( row = 2 column = 1 )
    CHANGING ct_table = <io_class>->item_dis ) .
    <io_class>->init_alv( <io_class>->falv_dis_item ).
    <io_class>->falv_dis_item->display( ).

  ELSE.
    <io_class>->falv_dis_head->soft_refresh( ).
    <io_class>->falv_dis_item->soft_refresh( ).
  ENDIF.
ENDMODULE.

MODULE screen_mod_head OUTPUT.

  DATA(lt_screen) = <io_class>->screen_con.
  DATA it_screen LIKE lt_screen.
  DATA is_screen LIKE LINE OF it_screen.
  DELETE lt_screen WHERE fieldalv = 'ITEM'.
  LOOP AT lt_screen ASSIGNING FIELD-SYMBOL(<screen>) WHERE fieldalv = 'HEAD'.
    ASSIGN COMPONENT <screen>-fieldname OF STRUCTURE <gs_text> TO FIELD-SYMBOL(<value>).
    IF sy-subrc EQ 0.
      is_screen = <screen>.
      is_screen-fieldname = |<GS_TEXT>-{ <screen>-fieldname }|.
      APPEND is_screen TO it_screen.
    ENDIF.
    <screen>-fieldname = |<IO_CLASS>->HEAD-{ <screen>-fieldname }|.
  ENDLOOP.

  APPEND LINES OF it_screen TO lt_screen.

  LOOP AT SCREEN.
    READ TABLE lt_screen ASSIGNING <screen> WITH KEY fieldname = screen-name.
    IF sy-subrc EQ 0.
      IF <screen>-edit = 'X'.
        screen-input = '1'.
      ELSEIF <screen>-edit = 'C'.
        IF <io_class>->action = 'CREATE'.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
      ELSE.
        screen-input = '0'.
      ENDIF.

      IF <screen>-requi = 'X'.
        screen-required = '2'.
      ELSE.
        screen-required = '0'.
      ENDIF.

      IF <screen>-hidde = 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
    ELSE.
      IF screen-group4 = 'NOT'.
        screen-active = 0.
      ENDIF.
    ENDIF.
    IF <io_class>->readonly = abap_true .
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.

*MODULE listbox_ekgrp OUTPUT.
*  TYPE-POOLS vrm.
*  DATA: vid    TYPE vrm_id VALUE '<IO_CLASS>->HEAD-EKGRP',
*        vlist  TYPE vrm_values,
*        values LIKE LINE OF vlist.
*  CHECK gt_t024 IS INITIAL.
*  SELECT * FROM t024
*    INTO TABLE @gt_t024.
*
*  LOOP AT gt_t024 INTO DATA(gs_t024).
*    MOVE gs_t024-ekgrp TO values-key.
*    MOVE gs_t024-eknam TO values-text.
*    APPEND values TO vlist.
*  ENDLOOP.
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id              = vid
*      values          = vlist
*    EXCEPTIONS
*      id_illegal_name = 1
*      OTHERS          = 2.
*  IF sy-subrc <> 0.
*  ENDIF.
*ENDMODULE.
*
*MODULE listbox_vtweg OUTPUT.
*  TYPE-POOLS vrm.
*  DATA: vid1    TYPE vrm_id VALUE '<IO_CLASS>->HEAD-VTWEG',
*        vlist1  TYPE vrm_values,
*        values1 LIKE LINE OF vlist.
*  CHECK gt_tvtwt IS INITIAL.
*  SELECT * FROM tvtwt
*  INTO TABLE @gt_tvtwt
*    WHERE spras = @sy-langu.
*  vlist1 = CORRESPONDING #( gt_tvtwt
*                                               MAPPING key = vtweg
*                                                                 text = vtext
*                                             ).
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id              = vid1
*      values          = vlist1
*    EXCEPTIONS
*      id_illegal_name = 1
*      OTHERS          = 2.
*  IF sy-subrc <> 0.
*  ENDIF.
*ENDMODULE.

MODULE set_editor OUTPUT.
  CHECK text_container IS INITIAL.
  CREATE OBJECT text_container EXPORTING container_name = 'TEXT_CONTAINER'.
  CREATE OBJECT <io_class>->editor
    EXPORTING
*     max_number_chars           = 180
      parent                     = text_container
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 72
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
  <io_class>->editor->set_statusbar_mode( cl_gui_textedit=>false ).
  <io_class>->editor->set_toolbar_mode( cl_gui_textedit=>false ).
  IF <io_class>->head-afono IS NOT INITIAL..
    <io_class>->editer_read_text( ).
  ENDIF.
ENDMODULE.

MODULE set_editor_screen OUTPUT.
  IF <io_class>->readonly = abap_true.
    <io_class>->editor->set_readonly_mode( 1 ).
  ELSE.
    <io_class>->editor->set_readonly_mode( 0 ).
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name = 'ZOOMIN'.
      screen-input = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.

MODULE create_gos_service OUTPUT.
  DATA:obj TYPE borident.
  CHECK <io_class>->gos_manager IS INITIAL.
  CHECK <io_class>->head-afono IS NOT INITIAL .
  obj-objtype = 'ZAFO'.
  obj-objkey = <io_class>->head-afono .
  CREATE OBJECT <io_class>->gos_manager
    EXPORTING
*     IP_START_DIRECT      = 'X'
*     it_service_selection = it_gos_sels
*     ip_no_instance       = 'X'
      is_object = obj
*     ip_no_commit         = 'X'
    EXCEPTIONS
      OTHERS    = 1.
  cl_gui_cfw=>flush( ).
ENDMODULE.

MODULE set_dynamic_screen OUTPUT.
  <io_class>->set_dynamic_screen( ).
ENDMODULE.
