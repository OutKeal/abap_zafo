*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS: lcl_event_receiver_700_top DEFINITION DEFERRED.

CLASS:lcl_event_receiver_700_down DEFINITION DEFERRED.

DATA: g_event_receiver_700_top  TYPE REF TO lcl_event_receiver_700_top,
      g_event_receiver_700_down TYPE REF TO lcl_event_receiver_700_down.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_700_top DEFINITION.

  PUBLIC SECTION.

    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.

    METHODS  handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        e_row_id
        e_column_id
        es_row_no.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS handle_toolbar
      FOR EVENT toolbar  OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS: data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.
ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_700_top IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_700_top
      USING er_data_changed
            e_onf4.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM f_handle_double_click_700_top USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_700_top USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_700_top USING e_ucomm.
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_700_top USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_700 USING e_modified et_good_cells.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_700_down DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click
      OF cl_gui_alv_grid
      IMPORTING e_row e_column.

    METHODS  handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        e_row_id
        e_column_id
        es_row_no.

    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_700_down IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM F_HANDLE_DOUBLE_CLICK_700_down USING E_ROW E_COLUMN.
  ENDMETHOD.

  METHOD handle_hotspot_click.
*    PERFORM F_HANDLE_HOTSPOT_700_down USING E_ROW_ID E_COLUMN_ID .
  ENDMETHOD.

  METHOD handle_toolbar.
*    PERFORM f_toolbar_700 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
*    PERFORM f_user_command_700 USING e_ucomm.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION




DATA: g_grid_700_top              TYPE REF TO cl_gui_alv_grid,
      g_grid_700_down             TYPE REF TO cl_gui_alv_grid,
      gt_fcat_700_top             TYPE lvc_t_fcat,
      gt_fcat_700_down            TYPE lvc_t_fcat,


      gs_layout_700               TYPE lvc_s_layo,
      gt_sort_700_top             TYPE lvc_t_sort,
      gt_exclude_700              TYPE ui_functions,
      gt_exclude_700_right        TYPE ui_functions,
      g_docking_container_700     TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_700      TYPE REF TO cl_gui_custom_container,
      g_cumtom_container_700_left TYPE REF TO cl_gui_custom_container,
      g_container_700_left        TYPE REF TO cl_gui_container,
      g_container_700_top         TYPE REF TO cl_gui_container,
      g_container_700_down        TYPE REF TO cl_gui_container,
      g_splitter_700              TYPE REF TO cl_gui_splitter_container,
      g_splitter_700_left         TYPE REF TO cl_gui_splitter_container,
      g_toolbar_700_top           TYPE REF TO cl_gui_toolbar.



FORM f_handle_data_changed_700_top USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
                                             u_onf4 TYPE any.

  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS: <fs_changed> TYPE any,
                 <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.
  ENDLOOP.

ENDFORM.


FORM f_handle_hotspot_700_top USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

ENDFORM.


FORM f_handle_user_command_700_top USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

ENDFORM.


FORM f_toolbar_700_top USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  MOVE '&MASS_MODIFY' TO ls_toolbar-function.
  MOVE icon_ws_confirm_whse_proc_back TO ls_toolbar-icon.
  MOVE TEXT-018 TO ls_toolbar-quickinfo."批量维护
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-018 TO ls_toolbar-text."批量维护
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

ENDFORM.


FORM f_user_command_700_top USING ok_code.

  CASE ok_code.
    WHEN '&MASS_MODIFY'.
      CLEAR ok_code.
      CHECK g_readonly <> 'D'.
      PERFORM f_refresh_grid_alv USING g_grid_700_top.
  ENDCASE.

ENDFORM.


FORM f_toolbar_700_right USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE '&ADD' TO ls_toolbar-function.
  MOVE icon_insert_row TO ls_toolbar-icon.
  MOVE TEXT-020 TO ls_toolbar-quickinfo." '增加行'
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-020 TO ls_toolbar-text." '增加行'
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


MODULE create_object_0700 OUTPUT.

  IF g_grid_700_top IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_700.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_field_catalog_700_top.
    PERFORM f_set_field_catalog_700_down.
**-- LAYOUT
    PERFORM f_create_grid_layout_700.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_700  CHANGING gt_exclude_700[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_handlers_700_top CHANGING g_grid_700_top.
    PERFORM f_assign_handlers_700_down CHANGING g_grid_700_down.

    PERFORM f_register_event_700 USING g_grid_700_top.
    PERFORM f_register_event_700 USING g_grid_700_down.


    CALL METHOD cl_gui_cfw=>flush.
    PERFORM f_display_grid_alv_700.

    CALL METHOD g_grid_700_top->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    PERFORM f_refresh_grid_alv USING g_grid_700_top.
    PERFORM f_refresh_grid_alv USING g_grid_700_down.

  ENDIF.

ENDMODULE.


FORM f_create_container_700 .
  DATA:l_col TYPE i.


*  IF G_DOCKING_CONTAINER_700 IS INITIAL.
*
*    CREATE OBJECT G_DOCKING_CONTAINER_700
*      EXPORTING
*        STYLE     = CL_GUI_CONTROL=>WS_CHILD
*        REPID     = SY-REPID
*        DYNNR     = SY-DYNNR
*        SIDE      = G_DOCKING_CONTAINER_700->DOCK_AT_BOTTOM
*        LIFETIME  = CL_GUI_CONTROL=>LIFETIME_IMODE
*        EXTENSION = '7000'
*      EXCEPTIONS
*        OTHERS    = 1.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID
*            TYPE SY-MSGTY
*          NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ENDIF.

  CREATE OBJECT g_cumtom_container_700
    EXPORTING
      container_name = 'ITEM'.



* SPLITTER CONTAINER
  IF g_splitter_700 IS INITIAL.

    CREATE OBJECT g_splitter_700
      EXPORTING
        parent  = g_cumtom_container_700
        rows    = 2
        columns = 1.



    g_container_700_top  = g_splitter_700->get_container( row = 1 column = 1 ).

    CREATE OBJECT g_grid_700_top
      EXPORTING
        i_parent = g_container_700_top.


    g_container_700_down = g_splitter_700->get_container( row = 2 column = 1 ).

    CREATE OBJECT g_grid_700_down
      EXPORTING
        i_parent = g_container_700_down.



    CALL METHOD g_splitter_700->set_row_height
      EXPORTING
        id     = 1
        height = 80.

  ENDIF.






ENDFORM.


FORM f_set_field_catalog_700_top .

  REFRESH: gt_fcat_700_top.

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

      IF g_readonly = 'D'.
        <ls_fcat>-edit = ''.
      ENDIF.


      IF gt_screen-hidde = 'X' OR gt_screen-hidde = 'I'.
        <ls_fcat>-tech = 'X'.
      ELSE.
        <ls_fcat>-tech = ''.
      ENDIF.
    ELSE.
      <ls_fcat>-tech = 'X'.
      <ls_fcat>-col_pos = 9999.
    ENDIF.
    <ls_fcat>-no_out = <ls_fcat>-tech.
  ENDLOOP.


  gt_fcat_700_top = lt_fcat.

ENDFORM.


FORM f_set_field_catalog_700_down .

  REFRESH: gt_fcat_700_down.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.
  DATA: lt_fcat TYPE lvc_t_fcat.

  DATA:lt_fieldcat TYPE slis_t_fieldcat_alv,
       ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZMM_SBARCODE'.

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

  READ TABLE gt_barcode INDEX 1.

  DATA: lt_con_t_i00 TYPE TABLE OF zmm_paking_t_i WITH HEADER LINE.
  DATA: lt_con_t_i TYPE TABLE OF zmm_paking_t_i WITH HEADER LINE.
  DATA: lt_con_i TYPE TABLE OF zmm_paking_t_i WITH HEADER LINE.

  SELECT * INTO TABLE lt_con_t_i00
    FROM zmm_paking_t_i
   WHERE template_type = '00'.

  SELECT * APPENDING TABLE lt_con_t_i
    FROM zmm_paking_t_i
   WHERE template_type = gt_barcode-template_type.

  LOOP AT lt_con_t_i00.
    READ TABLE lt_con_t_i WITH KEY fieldname = lt_con_t_i00-fieldname
                                     coltext = lt_con_t_i00-coltext.
    IF sy-subrc = 0.
      APPEND lt_con_t_i TO lt_con_i.

    ELSE.
      APPEND lt_con_t_i00 TO lt_con_i.
    ENDIF.
  ENDLOOP.

* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'ICON' OR 'TEXT' .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.
      WHEN 'MENGE'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '数量(算料单位)'.
        CONTINUE.
      WHEN 'MENGE_CG'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '数量(报价单位)'.
        CONTINUE.
    ENDCASE.

    READ TABLE lt_con_i INTO DATA(ls_con_i)
                         WITH KEY fieldname = <ls_fcat>-fieldname.
    IF sy-subrc EQ 0.
      <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
      <ls_fcat>-scrtext_l = <ls_fcat>-reptext = ls_con_i-coltext.
      <ls_fcat>-col_pos = ls_con_i-dzaehk.

      IF ls_con_i-fedit = 'X'.
        <ls_fcat>-edit = 'X'.
      ELSEIF ls_con_i-fedit = 'C'.
        IF g_action = 'CRE'.
          <ls_fcat>-edit = 'X'.
        ELSE.
          <ls_fcat>-edit = ''.
        ENDIF.
      ENDIF.

      IF g_readonly = 'D'.
        <ls_fcat>-edit = ''.
      ENDIF.

      IF ls_con_i-hidde = 'X'.
        <ls_fcat>-tech = 'X'.
      ELSE.
        <ls_fcat>-tech = ''.
      ENDIF.
    ELSE.
      CASE <ls_fcat>-fieldname.
        WHEN 'ZSUM_LEVEL'.
        WHEN 'STATUS'.
        WHEN OTHERS.
          <ls_fcat>-tech = 'X'.
          <ls_fcat>-col_pos = 9999.
      ENDCASE.

    ENDIF.

    <ls_fcat>-no_out = <ls_fcat>-tech.

  ENDLOOP.

  gt_fcat_700_down = lt_fcat.

ENDFORM.


FORM f_create_grid_layout_700 .

  CLEAR: gs_layout_700.
  gs_layout_700-sel_mode   = 'A'.
  gs_layout_700-cwidth_opt = 'X'.
  gs_layout_700-zebra      = 'X'.
  gs_layout_700-no_rowins      = 'X'.
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

ENDFORM.


FORM f_create_grid_toolbar_700 CHANGING  c_t_toolbar TYPE ui_functions.

*  DATA: ls_exclude TYPE ui_func.

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


FORM f_assign_handlers_700_top CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_700_top.

  SET HANDLER g_event_receiver_700_top->handle_data_changed
          FOR c_grid .

  SET HANDLER g_event_receiver_700_top->handle_toolbar
          FOR c_grid .

  SET HANDLER g_event_receiver_700_top->data_changed_finished
        FOR c_grid.
  SET HANDLER g_event_receiver_700_top->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_700_top->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_700_top->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.


FORM f_assign_handlers_700_down CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_700_down.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_700_down->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_700_down->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_700_down->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_700_down->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.


FORM f_register_event_700
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


FORM f_display_grid_alv_700 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.
  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'g_grid_700_top' CHANGING ls_variant-handle.

  CALL METHOD g_grid_700_top->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_700
      it_toolbar_excluding = gt_exclude_700[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item[]
      it_sort              = gt_sort_700_top[]
      it_fieldcatalog      = gt_fcat_700_top[].

  PERFORM frm_get_alv_handle USING sy-repid g_bustyp 'g_grid_700_down'
                             CHANGING ls_variant-handle.
  CALL METHOD g_grid_700_down->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_700
      it_toolbar_excluding = gt_exclude_700[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_barcode_dis[]
*     IT_SORT              = GT_SORT[]
      it_fieldcatalog      = gt_fcat_700_down[].
ENDFORM.


FORM f_set_catalog_alv_700.

  PERFORM f_set_field_catalog_700_top.
  CALL METHOD g_grid_700_top->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_700_top.

  CALL METHOD g_grid_700_top->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_700.


  IF gt_fcat_700_down IS NOT INITIAL.
    PERFORM f_set_field_catalog_700_down.

    CALL METHOD g_grid_700_down->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat_700_down.

    CALL METHOD g_grid_700_down->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_700.
  ENDIF.

ENDFORM.


FORM f_toolbar_700 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE '&SURE' TO ls_toolbar-function.
  MOVE icon_checked TO ls_toolbar-icon.
  MOVE TEXT-017 TO ls_toolbar-quickinfo."'确认选择'
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE '确认选择' TO ls_toolbar-text."'确认选择'
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


FORM f_data_changed_finished_700  USING e_modified
                                        et_good_cells TYPE lvc_t_modi.
  DATA:ls_refresh TYPE char1.
  DATA:ls_refresh_po TYPE char1.
  CLEAR ls_refresh.
  CLEAR ls_refresh_po.
  CHECK NOT et_good_cells IS INITIAL.
  DATA:ls_menge_sum TYPE menge_d.
  DATA:ls_menge_sumkey TYPE menge_d.
  DATA:ls_menge_last TYPE menge_d.

  LOOP AT et_good_cells INTO DATA(ls_cell).
    READ TABLE gt_item ASSIGNING <gs_item> INDEX ls_cell-row_id.

    CHECK sy-subrc = 0.

    CASE ls_cell-fieldname.
      WHEN 'MENGE_ZJ'.
        IF gs_bustyp-execute_type = 'PO'.
          PERFORM frm_po_item_calculation CHANGING <gs_item>.
        ELSEIF gs_bustyp-execute_type = 'POC'.
          PERFORM frm_poc_item_calculation CHANGING <gs_item>.
        ENDIF.
        ls_refresh = 'X'.
        CLEAR ls_menge_sum.
        CLEAR ls_menge_sumkey.
        ls_menge_sum = <gs_item>-menge.
        LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = <gs_item>-afonr.
          ls_menge_sum = ls_menge_sum + <gs_item_po>-menge.
          ls_menge_sumkey = ls_menge_sumkey + <gs_item_po>-ktmng.
        ENDLOOP.

        IF ls_menge_sum <> <gs_item>-menge.
          ls_menge_last = <gs_item>-menge.
          LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = <gs_item>-afonr.
            <gs_item_po>-menge = <gs_item_po>-ktmng * <gs_item>-menge / ls_menge_sumkey.
            PERFORM frm_set_round USING <gs_item_po>-meins CHANGING <gs_item_po>-menge.
            ls_menge_last = ls_menge_last - <gs_item_po>-menge.
            <gs_item_po>-amount = <gs_item_po>-price * <gs_item_po>-menge / <gs_item_po>-zmm_tran_rate / <gs_item_po>-peinh.
          ENDLOOP.
          IF <gs_item_po>-menge IS NOT INITIAL.
            <gs_item_po>-menge = <gs_item_po>-menge + ls_menge_last.
          ENDIF.
          ls_refresh_po = 'X'.
        ENDIF.

      WHEN 'PRICE_LONG'.
        PERFORM frm_set_price  CHANGING <gs_item>.
        <gs_item>-amount = <gs_item>-menge * <gs_item>-price / <gs_item>-zmm_tran_rate / <gs_item>-peinh.

        LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = <gs_item>-afonr.
          <gs_item_po>-price = <gs_item>-price.
          <gs_item_po>-amount = <gs_item_po>-price * <gs_item_po>-menge / <gs_item_po>-zmm_tran_rate / <gs_item_po>-peinh.
        ENDLOOP.
        IF sy-subrc EQ 0.
          ls_refresh_po = 'X'.
        ENDIF.
        ls_refresh = 'X'.
      WHEN 'PRICE'.
        <gs_item>-amount = <gs_item>-menge * <gs_item>-price / <gs_item>-zmm_tran_rate / <gs_item>-peinh.

        LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = <gs_item>-afonr.
          <gs_item_po>-price = <gs_item>-price.
          <gs_item_po>-amount = <gs_item_po>-price * <gs_item_po>-menge / <gs_item_po>-zmm_tran_rate / <gs_item_po>-peinh.
        ENDLOOP.
        IF sy-subrc EQ 0.
          ls_refresh_po = 'X'.
        ENDIF.
        ls_refresh = 'X'.
    ENDCASE.

  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_700_top.
  ENDIF.

  IF ls_refresh_po = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_700_down.
  ENDIF.
*  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
*    EXPORTING
*      NEW_CODE = 'ZDATA_CHANGE'.

ENDFORM.
