*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS: lcl_event_receiver_grid_8001 DEFINITION DEFERRED.

DATA: g_event_receiver_grid_8001   TYPE REF TO lcl_event_receiver_grid_8001.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_8001 DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.

    METHODS  handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no.

    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS: data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_8001 IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_8001 USING er_data_changed  e_onf4.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
    PERFORM f_handle_double_click_8001 USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_click_8001 USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_8001 USING e_object->mt_toolbar.
  ENDMETHOD.
  METHOD handle_user_command.
    PERFORM f_user_command_8001 USING e_ucomm.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_8001 USING e_modified et_good_cells.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION


DATA: g_grid_8001              TYPE REF TO cl_gui_alv_grid,
      gt_fcat_8001             TYPE lvc_t_fcat,
      gs_layout_8001           TYPE lvc_s_layo,
      gt_sort_8001             TYPE lvc_t_sort,
      gt_exclude_8001          TYPE ui_functions,
      g_docking_container_8001 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_8001  TYPE REF TO cl_gui_custom_container,
      g_container_8001         TYPE REF TO cl_gui_container,
      g_splitter_8001          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_8001           TYPE REF TO cl_gui_toolbar.


FORM f_handle_data_changed_8001
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

FORM f_handle_double_click_8001 USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

*  CLEAR gt_item_dis[].
*  PERFORM get_dis_data USING e_row_id-index.
*
*  PERFORM f_refresh_grid_alv_8001 USING g_grid2.

ENDFORM.


FORM f_handle_hotspot_click_8001 USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.

*  CHECK e_column_id-fieldname = 'ZOFNO'.
*  READ TABLE gt_head INTO gs_head INDEX e_row_id-index .
*
*  IF sy-subrc EQ 0.
*    SUBMIT zmmr0050 WITH modify = 'X'
*                    WITH create = ''
*                    WITH p_zofno = gs_head-afono
*                    AND RETURN.
*  ENDIF.



ENDFORM.


FORM f_handle_user_command_8001 USING ok_code.
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
*      CALL METHOD g_grid_8001->get_selected_rows
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

FORM f_toolbar_8001 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

*  CLEAR ls_toolbar.
*
*  IF gs_bustyp-busref IS INITIAL.
*    MOVE '&ADD' TO ls_toolbar-function.
*    MOVE icon_insert_row TO ls_toolbar-icon.
*    MOVE '插入行' TO ls_toolbar-quickinfo.
*    MOVE ' ' TO ls_toolbar-disabled.
*    MOVE '插入行' TO ls_toolbar-text.
*    APPEND ls_toolbar TO ut_toolbar.
*    CLEAR ls_toolbar.
*  ENDIF.
*
*  MOVE '&DEL' TO ls_toolbar-function.
*  MOVE icon_delete_row TO ls_toolbar-icon.
*  MOVE '删除行' TO ls_toolbar-quickinfo.
*  MOVE ' ' TO ls_toolbar-disabled.
*  MOVE '删除行' TO ls_toolbar-text.
*  APPEND ls_toolbar TO ut_toolbar.
*  CLEAR ls_toolbar.

ENDFORM.
FORM f_user_command_8001 USING ok_code.
*  DATA:lt_index_rows TYPE  lvc_t_row,
*       ls_index_rows TYPE  lvc_s_row,
*       lt_row_no     TYPE  lvc_t_roid.
*  DATA:lv_line_id TYPE zline_id .
*  CHECK g_readonly = 'M'.
*  CASE ok_code.
*    WHEN '&ADD'.
*
*      LOOP AT gt_item ASSIGNING <gs_item>.
*        IF <gs_item>-afonr > lv_line_id.
*          lv_line_id = <gs_item>-afonr.
*        ENDIF.
*      ENDLOOP.
*      lv_line_id = lv_line_id + 1.
*
*      CLEAR gs_item.
*      PERFORM init_item_line USING lv_line_id CHANGING gs_item.
*      gs_item-icon = icon_led_inactive.
*      gs_item-text = '初始'.
*      gs_item-afono = gs_head-afono.
*      APPEND gs_item TO gt_item.
*      CLEAR gs_item.
*
*
*
*      PERFORM  f_refresh_grid_alv_8001 USING g_grid_8001.
*    WHEN '&DEL'.
*
*      CALL METHOD g_grid_8001->get_selected_rows
*        IMPORTING
*          et_index_rows = lt_index_rows
*          et_row_no     = lt_row_no.
*      IF lt_index_rows[] IS INITIAL.
*        MESSAGE '请至少选择一行项目删除' TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.
*
*
*      PERFORM frm_pop_confirm USING '是否确认删除选中行?'.
*      CHECK g_error IS INITIAL.
*
*
*      LOOP AT lt_index_rows INTO ls_index_rows.
*        READ TABLE gt_item ASSIGNING FIELD-SYMBOL(<fs_item>)
*                                    INDEX ls_index_rows-index.
*        CHECK sy-subrc = 0.
*
*
*
*        IF <fs_item>-icon =  icon_led_inactive.
*          <fs_item>-icon = icon_delete.
*        ELSE.
*
*          SELECT SINGLE afono INTO @DATA(ls_afono)
*            FROM zafo_item WHERE afono = @<fs_item>-afono AND afonr = @<fs_item>-afonr.
*          IF sy-subrc EQ 0.
*            <fs_item>-icon = icon_delete.
*            DELETE FROM zafo_item WHERE afono = <fs_item>-afono AND afonr = <fs_item>-afonr.
*            COMMIT WORK.
*          ELSE.
*            <fs_item>-icon = icon_delete.
*          ENDIF.
*        ENDIF.
*
*      ENDLOOP.
*
*      DELETE gt_item WHERE icon = icon_delete.
*
*
*
*      PERFORM  f_refresh_grid_alv_8001 USING g_grid_8001.
*  ENDCASE.
ENDFORM.


FORM f_data_changed_finished_8001  USING  e_modified
                                   et_good_cells TYPE lvc_t_modi.


  LOOP AT et_good_cells INTO DATA(ls_cell).
    READ TABLE batch_item ASSIGNING FIELD-SYMBOL(<fs_item>)
                           INDEX ls_cell-row_id.
    IF sy-subrc EQ 0.
      <fs_item>-icon = icon_led_yellow.
    ENDIF.
  ENDLOOP.
  IF sy-subrc EQ 0.

    PERFORM f_refresh_grid_alv_8001 USING g_grid_8001.
  ENDIF.

ENDFORM.



MODULE create_object_8001 OUTPUT.

  IF  g_grid_8001 IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_8001.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_grid_field_catalog_8001.
**-- LAYOUT
    PERFORM f_create_grid_layout_8001.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_8001  CHANGING gt_exclude_8001[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_grid_handlers_8001 CHANGING g_grid_8001.
**-- REGISTER EVENT
    PERFORM f_register_grid_event_8001 USING g_grid_8001.
**--
    CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_8001.
*--
*    CALL METHOD g_grid_8001->set_ready_for_input
*      EXPORTING
*        i_ready_for_input = 1.
  ELSE.

    PERFORM f_refresh_grid_alv_8001 USING g_grid_8001.
*    PERFORM f_refresh_grid_alv_8001 USING g_grid2.
  ENDIF.

ENDMODULE.


FORM f_create_container_8001 .

  IF g_docking_container_8001 IS INITIAL.

    CREATE OBJECT g_docking_container_8001
      EXPORTING
        style     = cl_gui_control=>ws_child
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = g_docking_container_8001->dock_at_bottom
        lifetime  = cl_gui_control=>lifetime_imode
        extension = '260'
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
  CREATE OBJECT g_cumtom_container_8001
    EXPORTING
      container_name = 'ITEM'.


* SPLITTER CONTAINER
  IF g_splitter_8001 IS INITIAL.
    CREATE OBJECT g_splitter_8001
      EXPORTING
        parent  = g_docking_container_8001
        rows    = 1
        columns = 1.

    g_container_8001  = g_splitter_8001->get_container( row = 1 column = 1 ).

  ENDIF.

  CREATE OBJECT g_grid_8001
    EXPORTING
      i_parent = g_container_8001.

ENDFORM.


FORM f_set_grid_field_catalog_8001 .

  REFRESH: gt_fcat_8001.

  FIELD-SYMBOLS:<ls_fcat> TYPE lvc_s_fcat.
  DATA: lt_fcat TYPE lvc_t_fcat.

  DATA:lt_fieldcat TYPE slis_t_fieldcat_alv,
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


  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    IF <ls_fcat>-fieldname =  'EBELN'
      OR <ls_fcat>-fieldname = 'REMARK2'
      OR <ls_fcat>-fieldname = 'ZZPINO'
      OR <ls_fcat>-fieldname = 'MATNR'
      OR <ls_fcat>-fieldname = 'MAKTX'
      OR <ls_fcat>-fieldname = 'IDNLF'
      OR <ls_fcat>-fieldname = 'ZCOLOR_TEXT'
      OR <ls_fcat>-fieldname = 'ZSIZE'
      OR <ls_fcat>-fieldname = 'ZNORMS'
      OR <ls_fcat>-fieldname = 'BPRME'
      OR <ls_fcat>-fieldname = 'MENGE1_BJ'
      OR <ls_fcat>-fieldname = 'MENGE2_BJ'
      OR <ls_fcat>-fieldname = 'MENGE3_BJ'
      OR <ls_fcat>-fieldname = 'MENGE_CG'
      OR <ls_fcat>-fieldname = 'ZMM_TRAN_RATE'
      OR <ls_fcat>-fieldname = 'PRICE_LONG'
      OR <ls_fcat>-fieldname = 'AMOUNT'
      OR <ls_fcat>-fieldname = 'REMARK3'
      .
      <ls_fcat>-tech = ''.
    ELSE.
      <ls_fcat>-tech = 'X'.
    ENDIF.

    CASE <ls_fcat>-fieldname.
      WHEN  'REMARK2'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
       <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '采购编号'.
      WHEN  'REMARK3'.
        <ls_fcat>-emphasize = 'C610'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
       <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '超交比率'.
      WHEN  'MENGE1_BJ'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '采购数量'.
      WHEN  'MENGE2_BJ'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '总收货数'.
      WHEN  'MENGE3_BJ'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '本次收货数'.
      WHEN  'MENGE_CG'.
        <ls_fcat>-emphasize = 'C600'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '超交数量'.
      WHEN  'ZMM_TRAN_RATE'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '转换率'.
      WHEN  'PRICE_LONG'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '单价'.
      WHEN  'AMOUNT'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
         <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '超交金额'.
    ENDCASE.
    <ls_fcat>-edit = ''.
  ENDLOOP.

  gt_fcat_8001 = lt_fcat[].

ENDFORM.


FORM f_create_grid_layout_8001 .

  CLEAR: gs_layout_8001.
  gs_layout_8001-sel_mode   = 'A'.
  gs_layout_8001-cwidth_opt = 'X'.
  gs_layout_8001-zebra      = 'X'.
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


FORM f_create_grid_toolbar_8001
  CHANGING  c_t_toolbar TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.

  CLEAR: c_t_toolbar[].

*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND  LS_EXCLUDE  TO C_T_TOOLBAR.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO c_t_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO c_t_toolbar.
ENDFORM.


FORM f_assign_grid_handlers_8001
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid_8001.

  SET HANDLER g_event_receiver_grid_8001->handle_data_changed
          FOR c_grid .

  SET HANDLER g_event_receiver_grid_8001->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_8001->data_changed_finished
        FOR c_grid.
  SET HANDLER g_event_receiver_grid_8001->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid_8001->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_8001->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .
ENDFORM.


FORM f_register_grid_event_8001
  USING u_grid TYPE REF TO cl_gui_alv_grid.

* ENTER EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
* MODIFY EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.


FORM f_display_grid_alv_8001 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.
  ls_variant-handle = 8001.

  CALL METHOD g_grid_8001->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_8001
      it_toolbar_excluding = gt_exclude_8001[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item[]
      it_sort              = gt_sort_8001[]
      it_fieldcatalog      = gt_fcat_8001[].
*
*  ls_variant-handle = 2.
*
*  CALL METHOD g_grid2->set_table_for_first_display
*    EXPORTING
*      is_variant           = ls_variant
*      i_save               = 'A'
*      is_layout            = gs_layout
*      it_toolbar_excluding = gt_exclude[]
*      i_default            = 'X'
*    CHANGING
*      it_outtab            = gt_item_dis[]
*      it_sort              = gt_sort[]
*      it_fieldcatalog      = gt_fcat2[].

ENDFORM.


FORM f_set_catalog_alv_8001.

  PERFORM f_set_grid_field_catalog_8001.

  CALL METHOD g_grid_8001->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_8001.

  CALL METHOD g_grid_8001->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_8001.

ENDFORM.


FORM f_refresh_grid_alv_8001
   USING u_grid TYPE REF TO cl_gui_alv_grid.

  DATA: ls_scroll TYPE lvc_s_stbl.

  CLEAR: ls_scroll.
  ls_scroll-row = 'X'.
  ls_scroll-col = 'X'.

  CALL METHOD u_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_scroll
      i_soft_refresh = 'X'.

ENDFORM.


FORM f_transfer_slis_to_lvc_8001
  CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv
           ct_fcat     TYPE lvc_t_fcat.

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
    EXPORTING
      it_fieldcat = ct_fieldcat
    IMPORTING
      et_fieldcat = lt_fieldcat.

  CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
    EXPORTING
      it_fieldcat_kkblo = lt_fieldcat
    IMPORTING
      et_fieldcat_lvc   = ct_fcat.

ENDFORM.
