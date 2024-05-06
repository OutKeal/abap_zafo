*&---------------------------------------------------------------------*
*& 包含               ZAFO_PBO
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  DATA ex_code_100 TYPE TABLE OF sy-ucomm.
  IF <io_class>->bustype-busref = 'Y'.
    APPEND '&SELECTALL' TO ex_code_100.
  ENDIF.
  IF <io_class>->action = 'REF_IN'.
    APPEND '&REFRESH' TO ex_code_100.
  ENDIF.
  SET PF-STATUS '100' EXCLUDING ex_code_100.
  <io_class>->set_head_text( ) .
  SET TITLEBAR '100' WITH <io_class>->head_text.
ENDMODULE.

MODULE create_object_0100 OUTPUT.
  IF <io_class>->falv_ref IS INITIAL.
    <io_class>->falv_ref = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = <io_class>->get_falv_handle( repid = sy-repid alv_name = 'FALV_REF' )
      i_parent = NEW cl_gui_docking_container( extension = '3000' )"g_splitter_100->get_container( row = 1 column = 1 )
      CHANGING ct_table = <io_class>->item_ref ) .

    <io_class>->init_alv( <io_class>->falv_ref ).
    <io_class>->falv_ref->display( ).
  ELSE.
    <io_class>->falv_ref->soft_refresh( ).
  ENDIF.

ENDMODULE.

MODULE set_call_screen OUTPUT.

  IF ref_in_switch = icon_data_area_expand.
    ref_in_call_subsrceen = '1100'.
    ref_in_call_report = 'ZAFO'.
  ELSE.
    ref_in_call_subsrceen = '0111'.
    ref_in_call_report = 'SAPLZAFO'.
  ENDIF.

ENDMODULE.

MODULE create_object_0110 OUTPUT.
  IF g_container_112 IS INITIAL.
    g_container_112 = NEW cl_gui_custom_container( 'ALV' ).
  ELSE.
  ENDIF.
  IF <io_class>->falv_ref IS INITIAL.
    <io_class>->falv_ref = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = <io_class>->get_falv_handle( repid = sy-repid alv_name = 'FALV_REF' )
      i_parent = g_container_112
    CHANGING ct_table = <io_class>->item_ref ) .

    <io_class>->init_alv( <io_class>->falv_ref ).
    <io_class>->falv_ref->display( ).
  ELSE.
    <io_class>->falv_ref->soft_refresh( ).
  ENDIF.

ENDMODULE.

MODULE status_0200 OUTPUT.
  DATA(lt_fcodes) = <io_class>->set_gui_status_exclude( ).
  DELETE lt_fcodes WHERE table_line+0(1) <> '&'.
  SET PF-STATUS '200' EXCLUDING lt_fcodes.
  <io_class>->set_head_text( ) .
  SET TITLEBAR '200' WITH <io_class>->head_text.
  MOVE <io_class>->head TO zafo_shead.
ENDMODULE.


MODULE create_object_0200 OUTPUT.

  IF g_container_200 IS INITIAL.
    g_container_200 = NEW cl_gui_custom_container( 'ITEM' ).
  ELSE.
*    g_container_200->link( container = 'ITEM' ).
  ENDIF.

  IF <io_class>->falv_item IS INITIAL.
    <io_class>->falv_item = zwft_falv=>create( EXPORTING
      i_popup = ''
      i_repid = sy-repid
      i_handle = <io_class>->get_falv_handle( repid = sy-repid alv_name = 'FALV_ITEM' )
      i_parent = g_container_200
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
  <io_class>->set_head_text( ).
  SET TITLEBAR '300' WITH <io_class>->head_text.
ENDMODULE.

MODULE create_object_0300 OUTPUT.
  DATA g_splitter_300 TYPE REF TO cl_gui_splitter_container.
  IF <io_class>->falv_dis_head IS INITIAL.
    IF g_splitter_300 IS INITIAL.
      g_splitter_300 = NEW cl_gui_splitter_container(
      parent = NEW cl_gui_docking_container( extension = '3000' )
      rows = 2 columns = 1 ).
    ENDIF.

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
    ASSIGN COMPONENT <screen>-fieldname OF STRUCTURE <text> TO FIELD-SYMBOL(<value>).
    IF sy-subrc EQ 0.
      is_screen = <screen>.
      is_screen-fieldname = |<TEXT>-{ <screen>-fieldname }|.
      APPEND is_screen TO it_screen.
    ENDIF.
    <screen>-fieldname = |<HEAD>-{ <screen>-fieldname }|.
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
    IF screen-name+0(1) = '&'.
      screen-input = '1'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.

*MODULE listbox_ekgrp OUTPUT.
*  TYPE-POOLS vrm.
*  DATA: vid    TYPE vrm_id VALUE '<HEAD>-EKGRP',
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
*  DATA: vid1    TYPE vrm_id VALUE '<HEAD>-VTWEG',
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
  IF <head>-afono IS NOT INITIAL..
    <io_class>->editer_read_text( ).
  ENDIF.
ENDMODULE.

MODULE set_editor_screen OUTPUT.

  IF <io_class>->editor IS BOUND.
    IF <io_class>->readonly = abap_true.
      <io_class>->editor->set_readonly_mode( 1 ).
    ELSE.
      <io_class>->editor->set_readonly_mode( 0 ).
    ENDIF.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name = 'TEXT_DISPLAY_ALL' OR screen-name = 'HTML_EDITOR_DISPLAY'.
      screen-input = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.

MODULE create_gos_service OUTPUT.
  DATA:obj TYPE borident.
  CHECK <io_class>->gos_manager IS INITIAL.
  CHECK <head>-afono IS NOT INITIAL .
  obj-objtype = 'ZAFO'.
  obj-objkey = <head>-afono .
  CREATE OBJECT <io_class>->gos_manager
    EXPORTING
      is_object = obj
    EXCEPTIONS
      OTHERS    = 1.
  cl_gui_cfw=>flush( ).
ENDMODULE.



MODULE set_dynamic_screen OUTPUT.
  <io_class>->set_dynamic_screen( ).
ENDMODULE.
