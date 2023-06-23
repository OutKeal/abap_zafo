*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:lcl_event_receiver_600_top DEFINITION DEFERRED.
CLASS:lcl_event_receiver_600_down DEFINITION DEFERRED.
CLASS:lcl_event_receiver_600_down1 DEFINITION DEFERRED.
CLASS:lcl_event_receiver_600_right DEFINITION DEFERRED.

DATA: g_event_receiver_600_top   TYPE REF TO lcl_event_receiver_600_top,
      g_event_receiver_600_down  TYPE REF TO lcl_event_receiver_600_down,
      g_event_receiver_600_down1 TYPE REF TO lcl_event_receiver_600_down1,
      g_event_receiver_600_right TYPE REF TO lcl_event_receiver_600_right.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_600_top DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column.

    METHODS  handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.

    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.

    METHODS: data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_600_top IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_600_top  USING er_data_changed e_onf4.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM f_handle_double_click_600_top USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_600_top USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_600_top USING e_ucomm.
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_600_top USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_600 USING e_modified et_good_cells.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_600_down DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column.

    METHODS  handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.

ENDCLASS.


CLASS lcl_event_receiver_600_down1 DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column.

    METHODS  handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.

ENDCLASS.


CLASS lcl_event_receiver_600_down IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM F_HANDLE_DOUBLE_CLICK_600_down USING E_ROW E_COLUMN.
  ENDMETHOD.

  METHOD handle_hotspot_click.
*    PERFORM F_HANDLE_HOTSPOT_600_down USING E_ROW_ID E_COLUMN_ID .
  ENDMETHOD.

  METHOD handle_toolbar.
*    PERFORM f_toolbar_600 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
*    PERFORM f_user_command_600 USING e_ucomm.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_event_receiver_600_down1 IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM F_HANDLE_DOUBLE_CLICK_600_down USING E_ROW E_COLUMN.
  ENDMETHOD.

  METHOD handle_hotspot_click.
*    PERFORM F_HANDLE_HOTSPOT_600_down USING E_ROW_ID E_COLUMN_ID .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_600_down1 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_600_down1 USING e_ucomm.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_event_receiver_600_right DEFINITION.

  PUBLIC SECTION.
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.

    METHODS: data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_600_right IMPLEMENTATION.

  METHOD handle_toolbar.
    PERFORM f_toolbar_600_right USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_600_r USING e_modified et_good_cells.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_600_right USING e_ucomm.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION



DATA: g_grid_600_top               TYPE REF TO cl_gui_alv_grid,
      g_grid_600_down              TYPE REF TO cl_gui_alv_grid,
      g_grid_600_right             TYPE REF TO cl_gui_alv_grid,
      gt_fcat_600_top              TYPE lvc_t_fcat,
      gt_fcat_600_down             TYPE lvc_t_fcat,
      gt_fcat_600_down1            TYPE lvc_t_fcat,
      gt_fcat_600_right            TYPE lvc_t_fcat,
      gs_layout_600                TYPE lvc_s_layo,
      gs_layout_600_right          TYPE lvc_s_layo,
      gt_sort_600_top              TYPE lvc_t_sort,
      gt_exclude_600               TYPE ui_functions,
      gt_exclude_600_right         TYPE ui_functions,
      g_docking_container_600      TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_600       TYPE REF TO cl_gui_custom_container,
      g_cumtom_container_600_left  TYPE REF TO cl_gui_custom_container,
      g_cumtom_container_600_right TYPE REF TO cl_gui_custom_container,
      g_container_600_left         TYPE REF TO cl_gui_container,
      g_container_600_right        TYPE REF TO cl_gui_container,
      g_container_600_top          TYPE REF TO cl_gui_container,
      g_container_600_down         TYPE REF TO cl_gui_container,
      g_splitter_600               TYPE REF TO cl_gui_splitter_container,
      g_splitter_600_left          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_600_top            TYPE REF TO cl_gui_toolbar.


FORM f_handle_data_changed_600_top
 USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
   u_onf4    TYPE any.

  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS:
    <fs_changed> TYPE any,
    <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.
  ENDLOOP.

ENDFORM.

FORM f_handle_hotspot_600_top USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
ENDFORM.


FORM f_handle_user_command_600_top USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
ENDFORM.


FORM f_toolbar_600_top USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  IF gs_head-status IS INITIAL.
    IF gs_bustyp-busref IS INITIAL.
      MOVE '&ADD' TO ls_toolbar-function.
      MOVE icon_insert_row TO ls_toolbar-icon.
      MOVE TEXT-020 TO ls_toolbar-quickinfo."插入行
      MOVE ' ' TO ls_toolbar-disabled.
      MOVE TEXT-020 TO ls_toolbar-text."插入行
      APPEND ls_toolbar TO ut_toolbar.
      CLEAR ls_toolbar.
    ENDIF.
  ENDIF.

  MOVE '&DEL' TO ls_toolbar-function.
  MOVE icon_delete_row TO ls_toolbar-icon.
  MOVE TEXT-021 TO ls_toolbar-quickinfo. "删除行
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-021 TO ls_toolbar-text."删除行
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  MOVE '&MASS_MODIFY' TO ls_toolbar-function.
  MOVE icon_ws_confirm_whse_proc_back TO ls_toolbar-icon.
  MOVE TEXT-018 TO ls_toolbar-quickinfo."批量维护
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-018 TO ls_toolbar-text."批量维护
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  MOVE '&BATCH' TO ls_toolbar-function.
  MOVE icon_ws_confirm_whse_proc_back TO ls_toolbar-icon.
  MOVE TEXT-025 TO ls_toolbar-quickinfo."批处理
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-025 TO ls_toolbar-text."批处理
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.


  MOVE '&ROUND' TO ls_toolbar-function.
  MOVE icon_display_tree TO ls_toolbar-icon.
  MOVE TEXT-026 TO ls_toolbar-quickinfo."向上取整
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-026 TO ls_toolbar-text."向上取整
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  MOVE '&COLLECT' TO ls_toolbar-function.
  MOVE icon_ws_confirm_whse_proc_back TO ls_toolbar-icon.
  MOVE TEXT-027 TO ls_toolbar-quickinfo."汇总显示
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-027 TO ls_toolbar-text."汇总显示
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

ENDFORM.



FORM f_toolbar_600_down1 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  MOVE '&DEL_YCL' TO ls_toolbar-function.
  MOVE icon_delete_row TO ls_toolbar-icon.
  MOVE TEXT-050 TO ls_toolbar-quickinfo."删除原材料
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-050 TO ls_toolbar-text."删除原材料
  APPEND ls_toolbar TO ut_toolbar.

  MOVE '&ADD_YCL' TO ls_toolbar-function.
  MOVE icon_select_detail TO ls_toolbar-icon.
  MOVE TEXT-049 TO ls_toolbar-quickinfo."添加原材料
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-049 TO ls_toolbar-text."添加原材料
  APPEND ls_toolbar TO ut_toolbar.


ENDFORM.


MODULE create_object_0600 OUTPUT.

  IF g_grid_600_top IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_600.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_field_catalog_600_top.
    PERFORM f_set_field_catalog_600_down.
    PERFORM f_set_field_catalog_600_down1.
    PERFORM f_set_field_catalog_600_right.
**-- LAYOUT
    PERFORM f_create_grid_layout_600.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_600  CHANGING gt_exclude_600[].
    PERFORM f_create_toolbar_600_right  CHANGING gt_exclude_600_right[].

**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_handlers_600_top CHANGING g_grid_600_top.
    PERFORM f_assign_handlers_600_down CHANGING g_grid_600_down.
    PERFORM f_assign_handlers_600_down1 CHANGING g_grid_600_down.
    PERFORM f_assign_handlers_600_right CHANGING g_grid_600_right.

    PERFORM f_set_drop_down_600_right.

**-- REGISTER EVENT
    PERFORM f_register_event_600 USING g_grid_600_top.
    PERFORM f_register_event_600 USING g_grid_600_down.
    PERFORM f_register_event_600 USING g_grid_600_right.

    CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_600.
*--
    CALL METHOD g_grid_600_top->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    IF g_readonly = 'M'.
      CALL METHOD g_grid_600_top->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
      CALL METHOD g_grid_600_right->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
      CALL METHOD g_grid_600_down->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
    ELSEIF g_readonly = 'D'.
      CALL METHOD g_grid_600_top->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
      CALL METHOD g_grid_600_right->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
      CALL METHOD g_grid_600_down->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
    ENDIF.
  ELSE.
**--
    IF g_readonly = 'M'.
      CALL METHOD g_grid_600_top->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
      CALL METHOD g_grid_600_right->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
      CALL METHOD g_grid_600_down->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
    ELSEIF g_readonly = 'D'.
      CALL METHOD g_grid_600_top->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
      CALL METHOD g_grid_600_right->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
      CALL METHOD g_grid_600_down->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
    ENDIF.

    PERFORM f_refresh_hold_grid_alv USING g_grid_600_top.
    PERFORM f_refresh_hold_grid_alv USING g_grid_600_down.
    PERFORM f_refresh_hold_grid_alv USING g_grid_600_right.
  ENDIF.
ENDMODULE.

FORM f_set_drop_down_600_right.
  DATA: lt_drdn TYPE lvc_t_drop,                            "#EC NEEDED
        ls_drdn TYPE lvc_s_drop,                            "#EC NEEDED
        lt_dral TYPE lvc_t_dral,                            "#EC NEEDED
        ls_dral TYPE lvc_s_dral.                            "#EC NEEDED

  DATA: l_field  TYPE lvc_fname,
        l_handle TYPE i VALUE 2.

  DEFINE mac_get_drdn.
    ls_drdn-handle  = &1.
    ls_drdn-value   = &2.

      APPEND ls_drdn TO lt_drdn.

      ls_dral-handle    = &1.
      ls_dral-value     = &2.
      ADD 1 TO ls_dral-int_value.
      APPEND ls_dral TO lt_dral.

  end-of-definition.

  IF gt_cost[] IS INITIAL.

    SELECT * FROM zafo_cost
      INTO TABLE gt_cost.
  ENDIF.

  LOOP AT gt_cost.
    mac_get_drdn 1 gt_cost-cost_type.
  ENDLOOP.


  IF gt_cost[] IS NOT INITIAL.
    IF g_grid_600_right IS NOT INITIAL.
      CALL METHOD g_grid_600_right->set_drop_down_table
        EXPORTING
          it_drop_down_alias = lt_dral.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_create_container_600 .
  DATA:l_col TYPE i.

  CREATE OBJECT g_cumtom_container_600
    EXPORTING
      container_name = 'ITEM'.

* SPLITTER CONTAINER
  IF g_splitter_600 IS INITIAL.

    CREATE OBJECT g_splitter_600
      EXPORTING
        parent  = g_cumtom_container_600
        rows    = 1
        columns = 2.

    g_container_600_left  = g_splitter_600->get_container( row = 1 column = 1 ).


    CREATE OBJECT g_splitter_600_left
      EXPORTING
        parent  = g_container_600_left
        rows    = 2
        columns = 1.

    g_container_600_top  = g_splitter_600_left->get_container( row = 1 column = 1 ).

    CREATE OBJECT g_grid_600_top
      EXPORTING
        i_parent = g_container_600_top.


    g_container_600_down = g_splitter_600_left->get_container( row = 2 column = 1 ).

    CREATE OBJECT g_grid_600_down
      EXPORTING
        i_parent = g_container_600_down.


    g_container_600_right  = g_splitter_600->get_container( row = 1 column = 2 ).

    CREATE OBJECT g_grid_600_right
      EXPORTING
        i_parent = g_container_600_right.


    CALL METHOD g_splitter_600->set_column_width
      EXPORTING
        id    = 1
        width = 80.

    CALL METHOD g_splitter_600_left->set_row_height
      EXPORTING
        id     = 1
        height = 80.
  ENDIF.

ENDFORM.


FORM f_set_field_catalog_600_top .

  REFRESH: gt_fcat_600_top.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_SITEM'.

* 取得字段的属性
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = l_struc_name
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  PERFORM f_transfer_slis_to_lvc
          CHANGING lt_fieldcat
                   lt_fcat.
* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'ICON' OR 'TEXT' .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.
    ENDCASE.

    READ TABLE gt_screen WITH KEY object = g_object
                                  fieldname = <ls_fcat>-fieldname
                                  fieldalv = 'ITEM'.
    IF sy-subrc EQ 0.
      <ls_fcat>-emphasize = gt_screen-emphasize.

      <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
      <ls_fcat>-scrtext_l = <ls_fcat>-reptext = gt_screen-coltext.

      <ls_fcat>-col_pos = gt_screen-dzaehk.

      IF gt_screen-fedit = 'X'.
        <ls_fcat>-edit = 'X'.
*        <ls_fcat>-auto_value = 'X'.
      ELSEIF gt_screen-fedit = 'C'.
        IF g_action = 'CRE'.
          <ls_fcat>-edit = 'X'.
*          <ls_fcat>-auto_value = 'X'.
        ELSE.
          <ls_fcat>-edit = ''.
        ENDIF.
      ENDIF.

      IF gt_screen-hidde = 'X' OR gt_screen-hidde = 'I'.
        <ls_fcat>-tech = 'X'.
      ELSE.
        <ls_fcat>-tech = ''.
      ENDIF.

      CASE <ls_fcat>-fieldname.
        WHEN 'ZNORMS'.
          <ls_fcat>-lowercase = 'X'.
      ENDCASE.

    ELSE.
      <ls_fcat>-tech = 'X'.
      <ls_fcat>-col_pos = 9999.
    ENDIF.

    <ls_fcat>-no_out = <ls_fcat>-tech.
  ENDLOOP.

  gt_fcat_600_top = lt_fcat.

ENDFORM.


FORM f_set_field_catalog_600_down .
  REFRESH: gt_fcat_600_down.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.
  DATA: lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_SITEM_PO'.

* 取得字段的属性
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = l_struc_name
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  PERFORM f_transfer_slis_to_lvc CHANGING lt_fieldcat lt_fcat.
* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'ICON' OR 'TEXT' .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.
    ENDCASE.

    READ TABLE gt_screen WITH KEY object = g_object
                                  fieldname = <ls_fcat>-fieldname
                                  fieldalv = 'ITPO'.
    IF sy-subrc EQ 0.
      <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
       <ls_fcat>-scrtext_l = <ls_fcat>-reptext = gt_screen-coltext.

      <ls_fcat>-col_pos = gt_screen-dzaehk.
      IF gt_screen-fedit = 'X'.
        <ls_fcat>-edit = 'X'.
      ELSEIF gt_screen-fedit = 'C'.
        IF g_action = 'CRE'.
          <ls_fcat>-edit = 'X'.
        ELSE.
          <ls_fcat>-edit = ''.
        ENDIF.
      ENDIF.

      IF g_readonly = 'D'.
        <ls_fcat>-edit = ''.
      ENDIF.

      IF gt_screen-hidde = 'X'.
        <ls_fcat>-tech = 'X'.
      ELSE.
        <ls_fcat>-tech = ''.
      ENDIF.
    ELSE.
*      <ls_fcat>-tech = 'X'.
*      <ls_fcat>-col_pos = 9999.
    ENDIF.
    <ls_fcat>-no_out = <ls_fcat>-tech.
  ENDLOOP.

  gt_fcat_600_down = lt_fcat.

ENDFORM.


FORM f_set_field_catalog_600_down1.
  CHECK gs_head-bustyp = 'PO003'.

  REFRESH: gt_fcat_600_down1.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.
  DATA: lt_fcat TYPE lvc_t_fcat.

  DATA:lt_fieldcat TYPE slis_t_fieldcat_alv,
       ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_SITEM_PO_CPT'.

* 取得字段的属性
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = l_struc_name
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  PERFORM f_transfer_slis_to_lvc CHANGING lt_fieldcat lt_fcat.

  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'ICON' OR 'TEXT' .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
      WHEN 'KTMNG'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '计划数量'.
      WHEN 'MENGE'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '需求数量'.
        <ls_fcat>-edit = 'X'.
    ENDCASE.
    <ls_fcat>-no_out = <ls_fcat>-tech.
  ENDLOOP.

  gt_fcat_600_down1 = lt_fcat.
ENDFORM.


FORM f_set_field_catalog_600_right .
  DATA: lt_fcat TYPE lvc_t_fcat.
  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  REFRESH: gt_fcat_600_right.

  l_struc_name = 'ZAFO_ITEM_COST'.

* 取得字段的属性
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = l_struc_name
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  PERFORM f_transfer_slis_to_lvc CHANGING lt_fieldcat lt_fcat.

* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.
    CASE <ls_fcat>-fieldname.
      WHEN 'AFONO'.
        <ls_fcat>-tech = 'X'.
      WHEN 'COST_NAME' .
        <ls_fcat>-drdn_hndl = 1.
        <ls_fcat>-edit = 'X'.
*        <ls_fcat>-auto_value = 'X'.
      WHEN 'COST_AMOUNT' OR 'REMARK1' OR 'COLOR_TEXT'.
        <ls_fcat>-edit = 'X'.
*        <ls_fcat>-auto_value = 'X'.
    ENDCASE.
*    IF g_readonly = 'D'.
*      <ls_fcat>-edit = ''.
*    ENDIF.
  ENDLOOP.
  gt_fcat_600_right = lt_fcat.
ENDFORM.


FORM f_create_grid_layout_600 .

  CLEAR: gs_layout_600.
  gs_layout_600-sel_mode   = 'A'.
  gs_layout_600-cwidth_opt = 'X'.
  gs_layout_600-zebra      = 'X'.
  gs_layout_600-no_rowins      = 'X'.
*  GS_LAYOUT-NO_ROWMARK = 'X'.
*  GS_LAYOUT-BOX_FNAME = 'SEL'.

*  GS_LAYOUT-STYLEFNAME = 'CELLTAB'.

*  GS_LAYOUT-NUMC_TOTAL = CNS_CHAR_X.

*  GS_LAYOUT-SGL_CLK_HD    = 'X'.
*  GS_LAYOUT-TOTALS_BEF    = 'X'.             " 合计显示在上面
*  GS_LAYOUT-NO_HGRIDLN    = ' '.
*  GS_LAYOUT-NO_VGRIDLN    = ' '.
*  GS_LAYOUT-NO_TOOLBAR    = SPACE.
*  GS_LAYOUT-GRID_TITLE    = ' '.
*  GS_LAYOUT-SMALLTITLE    = ' '.
*  GS_LAYOUT-EXCP_FNAME    = 'ICON'.          " LED
*  GS_LAYOUT-INFO_FNAME    = 'COLOR'.         " LINE COLOR
*  GS_LAYOUT-CTAB_FNAME    = ' '.             " CELL COLOR
*  GS_LAYOUT-BOX_FNAME     = ' '.
*  GS_LAYOUT-DETAILINIT    = ' '.

  gs_layout_600_right = gs_layout_600.
  gs_layout_600_right-no_rowins = ''.
ENDFORM.


FORM f_create_grid_toolbar_600
  CHANGING  c_t_toolbar TYPE ui_functions.
  CLEAR: c_t_toolbar[].

*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND  LS_EXCLUDE  TO C_T_TOOLBAR.

  APPEND cl_gui_alv_grid=>mc_fc_html TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_views TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_info TO c_t_toolbar.
ENDFORM.


FORM f_create_toolbar_600_right
  CHANGING  c_t_toolbar TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.

  CLEAR: c_t_toolbar[].

*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND  LS_EXCLUDE  TO C_T_TOOLBAR.

  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO c_t_toolbar.
*  IF gs_head-execute_type <> 'PO'.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO c_t_toolbar.
*  ENDIF.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_print  TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_print_prev  TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_info TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_save_variant TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_variant_admin TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_maximum TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_minimum TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_find TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_average TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_html TO c_t_toolbar.
  APPEND cl_gui_alv_grid=>mc_fc_views TO c_t_toolbar.
ENDFORM.


FORM f_assign_handlers_600_top
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_600_top.

  SET HANDLER g_event_receiver_600_top->handle_data_changed
          FOR c_grid .

  SET HANDLER g_event_receiver_600_top->handle_toolbar
          FOR c_grid .

  SET HANDLER g_event_receiver_600_top->data_changed_finished
        FOR c_grid.
  SET HANDLER g_event_receiver_600_top->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_600_top->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_600_top->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .
ENDFORM.


FORM f_assign_handlers_600_down CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_600_down.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_600_down->handle_toolbar FOR c_grid .
  SET HANDLER g_event_receiver_600_down->handle_user_command FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_600_down->handle_hotspot_click FOR c_grid .
  SET HANDLER g_event_receiver_600_down->handle_double_click FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.


FORM f_assign_handlers_600_down1 CHANGING c_grid TYPE REF TO cl_gui_alv_grid.
  CHECK gs_head-bustyp = 'PO003'.

  CREATE OBJECT g_event_receiver_600_down1.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_600_down1->handle_toolbar FOR c_grid .
  SET HANDLER g_event_receiver_600_down1->handle_user_command FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_600_down1->handle_hotspot_click FOR c_grid .
  SET HANDLER g_event_receiver_600_down1->handle_double_click FOR c_grid .

ENDFORM.


FORM f_assign_handlers_600_right
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.
  CREATE OBJECT g_event_receiver_600_right.
  SET HANDLER g_event_receiver_600_right->handle_toolbar FOR c_grid .
  SET HANDLER g_event_receiver_600_right->data_changed_finished  FOR c_grid .
  SET HANDLER g_event_receiver_600_right->handle_user_command  FOR c_grid .

ENDFORM.


FORM f_register_event_600
  USING u_grid TYPE REF TO cl_gui_alv_grid.

* ENTER EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
** MODIFY EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.


FORM f_display_grid_alv_600 .
  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.

  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'g_grid_600_top' CHANGING ls_variant-handle.
  CALL METHOD g_grid_600_top->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_600
      it_toolbar_excluding = gt_exclude_600[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item[]
      it_sort              = gt_sort_600_top[]
      it_fieldcatalog      = gt_fcat_600_top[].



  IF gs_head-bustyp = 'PO003'.
    PERFORM frm_get_alv_handle
      USING sy-repid g_bustyp 'g_grid_600_down1' CHANGING ls_variant-handle.
    CALL METHOD g_grid_600_down->set_table_for_first_display
      EXPORTING
        is_variant           = ls_variant
        i_save               = 'A'
        is_layout            = gs_layout_600
        it_toolbar_excluding = gt_exclude_600[]
        i_default            = 'X'
      CHANGING
        it_outtab            = gt_item_po_cpt[]
*       IT_SORT              = GT_SORT[]
        it_fieldcatalog      = gt_fcat_600_down1[].
  ELSE.

    PERFORM frm_get_alv_handle
      USING sy-repid g_bustyp 'g_grid_600_down' CHANGING ls_variant-handle.


    CALL METHOD g_grid_600_down->set_table_for_first_display
      EXPORTING
        is_variant           = ls_variant
        i_save               = 'A'
        is_layout            = gs_layout_600
        it_toolbar_excluding = gt_exclude_600[]
        i_default            = 'X'
      CHANGING
        it_outtab            = gt_item_po[]
*       IT_SORT              = GT_SORT[]
        it_fieldcatalog      = gt_fcat_600_down[].

  ENDIF.




  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'g_grid_600_right' CHANGING ls_variant-handle.

  CALL METHOD g_grid_600_right->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_600_right
      it_toolbar_excluding = gt_exclude_600_right[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item_cost[]
*     IT_SORT              = GT_SORT[]
      it_fieldcatalog      = gt_fcat_600_right[].

ENDFORM.


FORM f_set_catalog_alv_600.

  PERFORM f_set_field_catalog_600_top.
  CALL METHOD g_grid_600_top->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_600_top.

  CALL METHOD g_grid_600_top->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_600.


  IF gt_fcat_600_down IS NOT INITIAL.
    PERFORM f_set_field_catalog_600_down.

    CALL METHOD g_grid_600_down->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat_600_down.

    CALL METHOD g_grid_600_down->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_600.

  ENDIF.

  IF gt_fcat_600_right IS NOT INITIAL.
    PERFORM f_set_field_catalog_600_right.

    CALL METHOD g_grid_600_right->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat_600_right.

    CALL METHOD g_grid_600_right->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_600.

*    PERFORM f_create_toolbar_600_right  CHANGING gt_exclude_600_right[].
*    it_toolbar_excluding
  ENDIF.

ENDFORM.


FORM f_toolbar_600_right USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE '&ADD' TO ls_toolbar-function.
  MOVE icon_insert_row TO ls_toolbar-icon.
  MOVE TEXT-020 TO ls_toolbar-quickinfo."'增加行'
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-020 TO ls_toolbar-text."'增加行'
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  CLEAR ls_toolbar.
  MOVE '&DELETE' TO ls_toolbar-function.
  MOVE icon_ws_confirm_whse_proc_back TO ls_toolbar-icon.
  MOVE TEXT-021 TO ls_toolbar-quickinfo. "删除行
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-021 TO ls_toolbar-text."删除行
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

ENDFORM.


FORM f_data_changed_finished_600_r USING  e_modified
                                   et_good_cells TYPE lvc_t_modi.
  DATA:ls_refresh TYPE char1.
  DATA:ls_refresh_po TYPE char1.

  CLEAR ls_refresh.
  CLEAR ls_refresh_po.

  LOOP AT et_good_cells INTO DATA(ls_cell).

    READ TABLE gt_item ASSIGNING <gs_item>
                            INDEX ls_cell-row_id.
    CHECK sy-subrc = 0.

    CASE ls_cell-fieldname.
      WHEN  'COST_AMOUNT'.
        PERFORM frm_set_po_cost USING ''.
        PERFORM frm_set_head_data.
        ls_refresh_po = 'X'.
    ENDCASE.
  ENDLOOP.

  IF ls_refresh_po = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_600_down.
  ENDIF.
ENDFORM.


FORM f_user_command_600_right USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

  CHECK g_readonly <> 'D'.

  CASE ok_code.
    WHEN '&ADD'.
      CLEAR gt_item_cost.
      APPEND gt_item_cost.
      PERFORM f_refresh_grid_alv USING g_grid_600_right.
    WHEN '&DELETE'.
      CALL METHOD g_grid_600_right->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      DATA(ls_lines) = lines( lt_index_rows ).
      IF ls_lines <> 1.
        MESSAGE s039  DISPLAY LIKE 'E'.
*        MESSAGE '请选择一行项目删除' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      LOOP AT lt_index_rows INTO ls_index_rows.
        DELETE gt_item_cost INDEX ls_index_rows-index.
      ENDLOOP.

      PERFORM f_refresh_grid_alv USING g_grid_600_right.
  ENDCASE.
ENDFORM.


FORM f_toolbar_600 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE '&SURE' TO ls_toolbar-function.
  MOVE icon_checked TO ls_toolbar-icon.
  MOVE TEXT-030 TO ls_toolbar-quickinfo."'确认选择'
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-030 TO ls_toolbar-text."'确认选择'
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  CLEAR ls_toolbar.
  MOVE '&MASS_MODIFY' TO ls_toolbar-function.
  MOVE icon_ws_confirm_whse_proc_back TO ls_toolbar-icon.
  MOVE TEXT-018 TO ls_toolbar-quickinfo."批量维护
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-018 TO ls_toolbar-text."批量维护
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

ENDFORM.


FORM f_data_changed_finished_600  USING  e_modified
                                   et_good_cells TYPE lvc_t_modi.
  DATA:ls_refresh TYPE char1.
  DATA:ls_refresh_po TYPE char1.

  CLEAR ls_refresh.
  CLEAR ls_refresh_po.

  LOOP AT et_good_cells INTO DATA(ls_cell).

    READ TABLE gt_item ASSIGNING <gs_item>
                            INDEX ls_cell-row_id.
    CHECK sy-subrc = 0.

    CASE ls_cell-fieldname.
      WHEN  'MENGE_CG'.
        IF gs_bustyp-bustyp = 'POT01'.
          PERFORM frm_pot_item_calculation CHANGING <gs_item>.
          PERFORM f_set_amount CHANGING <gs_item>.
        ENDIF.

        ls_refresh_po = 'X'.
        ls_refresh = 'X'.

      WHEN 'EEIND'.
        PERFORM frm_set_po_eeind CHANGING <gs_item>.
        ls_refresh_po = 'X'.
        ls_refresh = 'X'.

      WHEN  'MENGE_ZJ' OR 'MENGE4'.
        PERFORM frm_set_po_menge CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_set_po_amount CHANGING <gs_item>.

        ls_refresh_po = 'X'.
        ls_refresh = 'X'.

      WHEN 'MENGE'.
        PERFORM f_set_menge_cg CHANGING <gs_item>.
        IF gs_bustyp-execute_type <> 'PRO'.
          PERFORM frm_set_po_menge CHANGING <gs_item>.
        ENDIF.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_set_po_amount CHANGING <gs_item>.

        ls_refresh_po = 'X'.
        ls_refresh = 'X'.

      WHEN 'PRICE_LONG'.
        PERFORM frm_set_price  CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_set_po_amount CHANGING <gs_item>.

        ls_refresh_po = 'X'.
        ls_refresh = 'X'.

      WHEN 'PRICE'.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_set_po_amount CHANGING <gs_item>.

        ls_refresh_po = 'X'.
        ls_refresh = 'X'.
    ENDCASE.
  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_600_top.
  ENDIF.

  IF ls_refresh_po = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_600_down.
  ENDIF.

ENDFORM.


FORM f_user_command_600_top USING ok_code.

  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

  CLEAR g_error.

  CASE ok_code.
    WHEN '&ADD'.
      PERFORM f_refresh_grid_alv USING g_grid_600_top.

    WHEN '&DEL'.
      CALL METHOD g_grid_600_top->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      IF lt_index_rows[] IS INITIAL.
        MESSAGE s039 DISPLAY LIKE 'E'."请至少选择一行项目删除
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-022."是否确认删除选中行?'.

      CHECK g_error IS INITIAL.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_item ASSIGNING FIELD-SYMBOL(<fs_item>)
                                    INDEX ls_index_rows-index.
        CHECK sy-subrc = 0.

        SELECT SINGLE * INTO @DATA(lx_item)
          FROM zafo_item WHERE afono = @<fs_item>-afono
                         AND afonr = @<fs_item>-afonr.
        IF sy-subrc EQ 0.
          <fs_item>-icon = icon_delete.
          <fs_item>-del_flag = 'X'.

          LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = <fs_item>-afonr.
            <gs_item_po>-icon = icon_delete.
            <gs_item_po>-loekz = 'X'.
          ENDLOOP.
        ELSE.
          <fs_item>-icon = ''.
          <fs_item>-del_flag = 'X'.
          LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = <fs_item>-afonr.
            <gs_item_po>-icon = ''.
            <gs_item_po>-loekz = 'X'.
          ENDLOOP.
        ENDIF.

      ENDLOOP.

      DELETE gt_item WHERE icon = '' AND del_flag = 'X'.
      DELETE gt_item_po WHERE icon = '' AND loekz = 'X'.

      IF gs_bustyp-busref = 'D' OR gs_bustyp-busref = 'J'."重新获取订单的业务员
        READ TABLE gt_item WITH KEY del_flag = ''.
        PERFORM f_set_po_ywy USING gt_item-zzpino.
      ENDIF.

      PERFORM  f_refresh_grid_alv USING g_grid_600_top.
      PERFORM  f_refresh_grid_alv USING g_grid_600_down.


    WHEN '&MASS_MODIFY'.
      CLEAR ok_code.
      CHECK g_readonly <> 'D'.

      CALL METHOD g_grid_600_top->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      CLEAR lt_item[].

      IF lt_index_rows[] IS INITIAL.
        lt_item[] = gt_item[].
      ELSE.
        LOOP AT lt_index_rows INTO ls_index_rows.
          READ TABLE gt_item INDEX ls_index_rows-index.
          IF sy-subrc EQ 0.
            APPEND gt_item TO lt_item.
          ENDIF.
        ENDLOOP.
      ENDIF.

      PERFORM frm_po_mass_modify TABLES gt_fcat_600_top lt_item.

      IF g_exec_flag EQ 'C'.
        PERFORM  f_refresh_grid_alv USING g_grid_600_top.
        PERFORM  f_refresh_grid_alv USING g_grid_600_down.
        MESSAGE s041."批处理成功
      ENDIF.


    WHEN '&ROUND'.
      CLEAR ok_code.
      CHECK g_readonly <> 'D'.

      CALL METHOD g_grid_600_top->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      CLEAR lt_item[].

      IF lt_index_rows[] IS INITIAL.
        lt_item[] = gt_item[].
      ELSE.
        LOOP AT lt_index_rows INTO ls_index_rows.
          READ TABLE gt_item INDEX ls_index_rows-index.
          IF sy-subrc EQ 0.
            APPEND gt_item TO lt_item.
          ENDIF.
        ENDLOOP.
      ENDIF.

      READ TABLE lt_item INDEX 1.
      IF lt_item-matnr = 'RPMLB00005' OR lt_item-matnr = 'RPMHT00006'.
        PERFORM frm_po_qdl_round TABLES lt_item.
      ELSEIF lt_item-maktx = '胶条' OR lt_item-maktx = '透明胶条'.
        PERFORM frm_po_gg_round TABLES lt_item.
      ELSEIF lt_item-zzpino+0(4) = 'HBBK' AND  lt_item-zcate1 = 'PM' AND  lt_item-zcate2 = 'LB'." 采购陈莲莲，HBBK订单的唛头，每行起订量15
        LOOP AT lt_item.
          LOOP AT gt_item ASSIGNING <gs_item> WHERE afono = lt_item-afono
                                                AND afonr = lt_item-afonr.
            IF <gs_item>-menge_cg < 15.
              <gs_item>-menge_zj = 15 - <gs_item>-menge_cg.
            ENDIF.
            PERFORM frm_set_po_menge CHANGING <gs_item>.
            PERFORM f_set_amount CHANGING <gs_item>.
            PERFORM frm_set_po_amount CHANGING <gs_item>.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        PERFORM frm_po_round TABLES lt_item.
      ENDIF.

      PERFORM f_refresh_grid_alv USING g_grid_600_top.
      PERFORM f_refresh_grid_alv USING g_grid_600_down.

    WHEN '&BATCH'.
      CLEAR ok_code.
      CHECK g_readonly <> 'D'.

      PERFORM frm_mass_batch TABLES gt_fcat_600_top .

      IF g_exec_flag EQ 'C'.
        PERFORM  f_refresh_grid_alv USING g_grid_600_top.
        PERFORM  f_refresh_grid_alv USING g_grid_600_down.
        MESSAGE s041."批处理成功
      ENDIF.

    WHEN '&COLLECT'.
      CALL FUNCTION 'ZAFO_ITEM_COLLECT'
        TABLES
          ut_item = gt_item[]
          ut_fcat = gt_fcat_600_top[].
  ENDCASE.

ENDFORM.


FORM f_user_command_600_down1 USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
  CLEAR g_error.

  CASE ok_code.
    WHEN '&ADD_YCL'.
      PERFORM frm_get_cpt_ycl.
    WHEN '&DEL_YCL'.

      CHECK gt_item_po_cpt[] IS NOT INITIAL.

      CALL METHOD g_grid_600_down->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      IF lt_index_rows[] IS INITIAL.
        PERFORM frm_pop_confirm USING TEXT-051."'没选择，是否取整所有行?'.
        CHECK g_error <> 'X'.
        READ TABLE gt_item_po_cpt INDEX 1.
        REFRESH gt_item_po_cpt.
        DELETE FROM zafo_item_po_cpt WHERE afono = gt_item_po_cpt-afono.
        DELETE FROM resb WHERE ebeln = gt_item_po_cpt-ebeln.
      ELSE.
        LOOP AT lt_index_rows INTO ls_index_rows.
          READ TABLE gt_item_po_cpt INDEX ls_index_rows-index.
          IF sy-subrc EQ 0.
            IF gt_item_po_cpt-afono IS NOT INITIAL.
              IF gt_item_po_cpt-ebeln IS NOT INITIAL.
                DELETE FROM resb WHERE ebeln = gt_item_po_cpt-ebeln AND ebelp = gt_item_po_cpt-ebelp.
              ENDIF.

              DELETE FROM zafo_item_po_cpt WHERE afono = gt_item_po_cpt-afono
                                             AND afonr = gt_item_po_cpt-afonr
                                             AND ebelp = gt_item_po_cpt-ebelp
                                             AND rspos = gt_item_po_cpt-rspos.
            ENDIF.

            DELETE TABLE gt_item_po_cpt.
          ENDIF.
        ENDLOOP.

      ENDIF.

      COMMIT WORK AND WAIT.
  ENDCASE.

  PERFORM f_refresh_grid_alv USING g_grid_600_down.

ENDFORM.
