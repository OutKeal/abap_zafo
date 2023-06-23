*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:
  lcl_event_receiver_grid_300_up DEFINITION DEFERRED.


DATA:
  g_event_receiver_grid_300_up   TYPE REF TO lcl_event_receiver_grid_300_up.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_300_up DEFINITION.

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
      FOR EVENT hotspot_click
      OF cl_gui_alv_grid
      IMPORTING
        e_row_id
        e_column_id
        es_row_no.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_300_up IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_300_up
      USING er_data_changed
            e_onf4.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
    PERFORM f_handle_double_click_300_up USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_click_300_up USING e_row_id e_column_id .
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

DATA: g_grid_300_up           TYPE REF TO cl_gui_alv_grid,
      g_grid_300_down         TYPE REF TO cl_gui_alv_grid,
      gt_fcat_300_up          TYPE lvc_t_fcat,
      gt_fcat_300_down        TYPE lvc_t_fcat,


      gs_layout_300           TYPE lvc_s_layo,
      gt_sort_300_up          TYPE lvc_t_sort,
      gt_exclude_300          TYPE ui_functions,
      g_docking_container_300 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_300  TYPE REF TO cl_gui_custom_container,
      g_container_300_up      TYPE REF TO cl_gui_container,
      g_container_300_down    TYPE REF TO cl_gui_container,
      g_splitter_300          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_300_up        TYPE REF TO cl_gui_toolbar.





FORM f_handle_data_changed_300_up
 USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
   u_onf4    TYPE any.


  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS:
    <fs_changed> TYPE any,
    <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.

*    READ TABLE GT_ALV INDEX LS_MODI-ROW_ID
*                      ASSIGNING FIELD-SYMBOL(<FS_LS_DATA>) .
*    IF SY-SUBRC = 0.
**
*      IF <FS_CHANGED> IS ASSIGNED.
*        UNASSIGN: <FS_CHANGED>.
*      ENDIF.
*      ASSIGN COMPONENT LS_MODI-FIELDNAME OF STRUCTURE <FS_LS_DATA>
*                    TO <FS_CHANGED>.
*      <FS_CHANGED> = LS_MODI-VALUE.
*
*    ENDIF.

  ENDLOOP.

ENDFORM.


FORM f_handle_double_click_300_up USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
  CLEAR gt_item_front_dis[].
  PERFORM frm_get_item_front_dis USING e_row_id-index.

  PERFORM f_refresh_grid_alv USING g_grid_300_down.

ENDFORM.


FORM frm_get_item_front_dis USING l_index.
  READ TABLE gt_head_dis INDEX l_index.
  LOOP AT gt_item_dis WHERE afono = gt_head_dis-afono.
    APPEND gt_item_dis TO gt_item_front_dis.
    CLEAR gt_item_dis.
  ENDLOOP.
ENDFORM.


FORM frm_refresh_300.
  PERFORM frm_get_display IN PROGRAM zafo IF FOUND TABLES gt_head_dis gt_item_dis.

  PERFORM frm_set_group TABLES gt_head_dis gt_item_dis.

  gt_item_front_dis[] = gt_item_dis[].
ENDFORM.


FORM f_handle_hotspot_click_300_up USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

  CHECK e_column_id-fieldname = 'AFONO'.
  READ TABLE gt_head_dis  INDEX e_row_id-index .

  IF sy-subrc EQ 0.
    CALL FUNCTION 'ZAFO_MAINTAIN'
      EXPORTING
        i_bustyp = gt_head_dis-bustyp
        i_afono  = gt_head_dis-afono
*       i_werks  = <gs_head_dis>-werks
      IMPORTING
        es_head  = gt_head_dis
*     TABLES
*       ET_RETURN       =
*       ET_ITEM  =
*     EXCEPTIONS
*       ERROR    = 1
*       OTHERS   = 2
      .

    PERFORM f_refresh_grid_alv USING g_grid_300_up.
    PERFORM f_refresh_grid_alv USING g_grid_300_down.

  ENDIF.

ENDFORM.


FORM f_handle_user_command_300_up USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
*  CASE ok_code.
*    WHEN '&NEW'.
*      SUBMIT zmmr0050 WITH modify = ''
*                WITH create = 'X'
*                VIA SELECTION-SCREEN
*                AND RETURN.
*
*
*    WHEN '&DETAIL'.
*      CLEAR:gt_item_dis.
*
*      CALL METHOD g_grid_300_up->get_selected_rows
*        IMPORTING
*          et_index_rows = lt_index_rows
*          et_row_no     = lt_row_no.
*      IF lt_index_rows[] IS INITIAL.
*        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.
*
*      LOOP AT lt_index_rows INTO ls_index_rows.
*
*        PERFORM get_dis_data USING ls_index_rows-index.
*
*      ENDLOOP.
*
*      CALL METHOD g_grid2->set_frontend_layout
*        EXPORTING
*          is_layout = gs_layout.
*
*  ENDCASE.

ENDFORM.


MODULE create_object_0300 OUTPUT.

  IF g_grid_300_up IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_300.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_field_catalog_300_up.
    PERFORM f_set_field_catalog_300_down.
**-- LAYOUT
    PERFORM f_create_grid_layout_300_up.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_300_up  CHANGING gt_exclude_300[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_grid_handlers_300_up CHANGING g_grid_300_up.
*    PERFORM F_ASSIGN_GRID_EVENT_HANDLERS CHANGING G_GRID2.
**-- REGISTER EVENT
*    PERFORM f_register_grid_event1 USING g_grid_300_up.
*    PERFORM F_REGISTER_GRID_EVENT2 USING G_GRID2.
**--
    CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_300.
*--
    CALL METHOD g_grid_300_up->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid_300_up.

    PERFORM f_refresh_grid_alv USING g_grid_300_down.
  ENDIF.

ENDMODULE.


FORM f_create_container_300 .

  IF g_docking_container_300 IS INITIAL.

    CREATE OBJECT g_docking_container_300
      EXPORTING
        style     = cl_gui_control=>ws_child
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = g_docking_container_300->dock_at_bottom
        lifetime  = cl_gui_control=>lifetime_imode
        extension = '3000'
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* SPLITTER CONTAINER
  IF g_splitter_300 IS INITIAL.
    CREATE OBJECT g_splitter_300
      EXPORTING
        parent  = g_docking_container_300
        rows    = 2
        columns = 1.

    g_container_300_up  = g_splitter_300->get_container( row = 1 column = 1 ).
    g_container_300_down  = g_splitter_300->get_container( row = 2 column = 1 ).

  ENDIF.

  CREATE OBJECT g_grid_300_up
    EXPORTING
      i_parent = g_container_300_up.

  CREATE OBJECT g_grid_300_down
    EXPORTING
      i_parent = g_container_300_down.

ENDFORM.


FORM f_set_field_catalog_300_up .

  REFRESH: gt_fcat_300_up.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_SHEAD'.

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
      WHEN 'ICON' OR 'TEXT'  .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.
      WHEN  'AFONO'.

        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = gs_bustyp-bustyp_name1.
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
        <ls_fcat>-hotspot = 'X'.
        CONTINUE.

      WHEN 'BUSTYP'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.
      WHEN 'BLDAT'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.
      WHEN 'BUDAT'.
        <ls_fcat>-col_pos = 1.
*        CONTINUE.
      WHEN 'STATUS'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.
*      WHEN 'ERDAT'
*        OR 'ERZET'
*        OR 'ERNAM'
*        OR 'AENAM'
*        OR 'AEDAT'
*        OR 'AETIM' .
*        <ls_fcat>-col_pos = 9999.
*        CONTINUE.
    ENDCASE.

    READ TABLE gt_screen WITH KEY object = g_object
                                  fieldname = <ls_fcat>-fieldname
                                  fieldalv = 'HEAD'.
    IF sy-subrc EQ 0.

      <ls_fcat>-emphasize = gt_screen-emphasize.

      <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
      <ls_fcat>-scrtext_l = <ls_fcat>-reptext = gt_screen-coltext.

      <ls_fcat>-col_pos = gt_screen-dzaehk.
*      IF gt_screen-fedit = 'X'.
*        <ls_fcat>-edit = 'X'.
*      ELSEIF gt_screen-fedit = 'C'.
*        IF g_action = 'CRE'.
*          <ls_fcat>-edit = 'X'.
*        ELSE.
*          <ls_fcat>-edit = ''.
*        ENDIF.
*      ENDIF.

*      IF g_readonly = 'D'.
*        <ls_fcat>-edit = ''.
*      ENDIF.


      IF gt_screen-hidde = 'X'.
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
  gt_fcat_300_up = lt_fcat.

ENDFORM.


FORM f_set_field_catalog_300_down .

  REFRESH: gt_fcat_300_down.

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
      WHEN 'AFONO'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = gs_bustyp-bustyp_name1.
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
*      IF gt_screen-fedit = 'X'.
*        <ls_fcat>-edit = 'X'.
*      ELSEIF gt_screen-fedit = 'C'.
*        IF g_action = 'CRE'.
*          <ls_fcat>-edit = 'X'.
*        ELSE.
*          <ls_fcat>-edit = ''.
*        ENDIF.
*      ENDIF.

      IF g_readonly = 'D'.
        <ls_fcat>-edit = ''.
      ENDIF.


      IF gt_screen-hidde = 'X'.
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

  gt_fcat_300_down = lt_fcat.

ENDFORM.


FORM f_create_grid_layout_300_up .

  CLEAR: gs_layout_300.
  gs_layout_300-sel_mode   = 'D'.
  gs_layout_300-cwidth_opt = 'X'.
  gs_layout_300-zebra      = 'X'.
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


FORM f_create_grid_toolbar_300_up CHANGING  c_t_toolbar TYPE ui_functions.

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
*append  '&MB_FILTER' to c_t_toolbar.
ENDFORM.

FORM f_assign_grid_handlers_300_up
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid_300_up.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOOLBAR
*          FOR C_GRID .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_USER_COMMAND
*          FOR C_GRID .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid_300_up->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_300_up->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .
ENDFORM.


FORM f_register_grid_event_300_up
  USING u_grid TYPE REF TO cl_gui_alv_grid.

* ENTER EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
** MODIFY EVENT
*  CALL METHOD u_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.


FORM f_display_grid_alv_300 .
  DATA: ls_variant LIKE disvariant.

  ls_variant-report = sy-repid.

  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'G_GRID_300_UP' CHANGING ls_variant-handle.

  CALL METHOD g_grid_300_up->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_300
      it_toolbar_excluding = gt_exclude_300[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_head_dis[]
      it_sort              = gt_sort_300_up[]
      it_fieldcatalog      = gt_fcat_300_up[].

  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'G_GRID_300_DOWN' CHANGING ls_variant-handle.

  CALL METHOD g_grid_300_down->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_300
      it_toolbar_excluding = gt_exclude_300[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item_front_dis[]
*     it_sort              = gt_sort[]
      it_fieldcatalog      = gt_fcat_300_down[].

ENDFORM.
