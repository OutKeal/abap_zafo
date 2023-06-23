*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:
  lcl_event_receiver_400_left DEFINITION DEFERRED.

CLASS:
  lcl_event_receiver_400_right DEFINITION DEFERRED.

DATA:
  g_event_receiver_400_left  TYPE REF TO lcl_event_receiver_400_left,
  g_event_receiver_400_right TYPE REF TO lcl_event_receiver_400_right.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_400_left DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4.

    METHODS handle_toolbar
      FOR EVENT toolbar
      OF cl_gui_alv_grid
      IMPORTING e_object.

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
    METHODS handle_user_command
      FOR EVENT user_command
      OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS: data_changed_finished
      FOR EVENT data_changed_finished
      OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.
ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_400_left IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_400_left USING er_data_changed e_onf4.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_toolbar.
    PERFORM f_toolbar_400_left USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM f_handle_double_click_400_left USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_400_left USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_400 USING e_ucomm.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_400_l USING e_modified et_good_cells.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_400_right DEFINITION.

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

    METHODS handle_toolbar
      FOR EVENT toolbar
      OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_user_command
      FOR EVENT user_command
      OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: data_changed_finished
      FOR EVENT data_changed_finished
      OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.


ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_400_right IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM F_HANDLE_DOUBLE_CLICK_400_RIGHT USING E_ROW E_COLUMN.
  ENDMETHOD.

  METHOD handle_hotspot_click.
*    PERFORM F_HANDLE_HOTSPOT_400_RIGHT USING E_ROW_ID E_COLUMN_ID .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_400 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_400 USING e_ucomm.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_400_r USING e_modified et_good_cells.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION


DATA: g_grid_400_left         TYPE REF TO cl_gui_alv_grid,
      g_grid_400_right        TYPE REF TO cl_gui_alv_grid,
      gt_fcat_400_left        TYPE lvc_t_fcat,
      gt_fcat_400_right       TYPE lvc_t_fcat,
      gs_layout_400           TYPE lvc_s_layo,
      gt_sort_400_left        TYPE lvc_t_sort,
      gt_exclude_400          TYPE ui_functions,
      g_docking_container_400 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_400  TYPE REF TO cl_gui_custom_container,
      g_container_400_left    TYPE REF TO cl_gui_container,
      g_container_400_right   TYPE REF TO cl_gui_container,
      g_splitter_400          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_400_left      TYPE REF TO cl_gui_toolbar.


FORM f_handle_data_changed_400_left USING u_changed TYPE REF TO cl_alv_changed_data_protocol
                                           u_onf4   TYPE any.
  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS: <fs_changed> TYPE any,
                 <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.

  ENDLOOP.

ENDFORM.


FORM f_handle_hotspot_400_left USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
ENDFORM.


FORM f_handle_user_command_400_left USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
ENDFORM.


FORM f_toolbar_400 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.
*  CLEAR ls_toolbar.
*  MOVE '&OK' TO ls_toolbar-function.
*  MOVE icon_status_ok TO ls_toolbar-icon.
*  MOVE '确认总数量' TO ls_toolbar-quickinfo.
*  MOVE ' ' TO ls_toolbar-disabled.
*  MOVE '确认总数量' TO ls_toolbar-text.
*  APPEND ls_toolbar TO ut_toolbar.
*  CLEAR ls_toolbar.
ENDFORM.


FORM f_user_command_400 USING ok_code.

  CHECK ok_code = '&MASS_MODIFY'.

  DATA:ls_fieldname TYPE fieldname.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.
  FIELD-SYMBOLS <fs_value> TYPE any.
  CLEAR lt_flds.

  LOOP AT  gt_fcat_400_left INTO DATA(lt_fcat) WHERE edit = 'X'.
    CHECK lt_fcat-fieldname <> 'MANDT'.
    ls_flds-tabname = lt_fcat-ref_table.
    ls_flds-fieldtext = lt_fcat-scrtext_m.
    ls_flds-fieldname = lt_fcat-fieldname.
    APPEND ls_flds TO lt_flds.
    CLEAR ls_flds.
  ENDLOOP.

  CHECK sy-subrc EQ 0.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = '请输入批量修改的值'
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  LOOP AT lt_flds INTO ls_flds.
    IF ls_flds-value IS INITIAL.
      CONTINUE.
    ENDIF.

    IF ls_flds-value = '0'.
      CLEAR ls_flds-value.
    ENDIF.

    LOOP AT gt_qc_line ASSIGNING FIELD-SYMBOL(<fs_qc_line>) .
      ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE <fs_qc_line> TO <fs_value>.
      IF sy-subrc EQ 0.
        CHECK <fs_value> IS INITIAL.
        <fs_value> = ls_flds-value.
        <fs_qc_line>-icon = icon_led_yellow.
        <fs_qc_line>-text = '已维护'.
      ENDIF.

    ENDLOOP.
  ENDLOOP.
  IF sy-subrc EQ 0.
    PERFORM f_refresh_grid_alv USING g_grid_400_left.
  ENDIF.

ENDFORM.



MODULE create_object_0400 OUTPUT.

  IF g_grid_400_left IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_400.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_field_catalog_400_left.
    PERFORM f_set_field_catalog_400_right.
**-- LAYOUT
    PERFORM f_create_grid_layout_400.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_400  CHANGING gt_exclude_400[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_handlers_400_left CHANGING g_grid_400_left.
    PERFORM f_assign_handlers_400_right CHANGING g_grid_400_right.

    PERFORM f_register_event_400_left USING g_grid_400_left.
    PERFORM f_register_event_400_right USING g_grid_400_right.


    CALL METHOD cl_gui_cfw=>flush.

    PERFORM f_display_grid_alv_400.

    IF g_change = 'M'.
      CALL METHOD g_grid_400_left->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.

      CALL METHOD g_grid_400_right->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
    ENDIF.
  ELSE.

    PERFORM f_refresh_grid_alv USING g_grid_400_left.

    PERFORM f_refresh_grid_alv USING g_grid_400_right.
  ENDIF.

ENDMODULE.


FORM f_create_container_400 .
  DATA:l_col TYPE i.


*  IF G_DOCKING_CONTAINER_400 IS INITIAL.
*
*    CREATE OBJECT G_DOCKING_CONTAINER_400
*      EXPORTING
*        STYLE     = CL_GUI_CONTROL=>WS_CHILD
*        REPID     = SY-REPID
*        DYNNR     = SY-DYNNR
*        SIDE      = G_DOCKING_CONTAINER_400->DOCK_AT_BOTTOM
*        LIFETIME  = CL_GUI_CONTROL=>LIFETIME_IMODE
*        EXTENSION = '4000'
*      EXCEPTIONS
*        OTHERS    = 1.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID
*            TYPE SY-MSGTY
*          NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ENDIF.

  CREATE OBJECT g_cumtom_container_400
    EXPORTING
      container_name = 'ITEM'.


  l_col = 2.
* SPLITTER CONTAINER
  IF g_splitter_400 IS INITIAL.

    CREATE OBJECT g_splitter_400
      EXPORTING
        parent  = g_cumtom_container_400
        rows    = 1
        columns = l_col.

    g_container_400_left  = g_splitter_400->get_container( row = 1 column = 1 ).

    CREATE OBJECT g_grid_400_left
      EXPORTING
        i_parent = g_container_400_left.


    g_container_400_right = g_splitter_400->get_container( row = 1 column = 2 ).

    CREATE OBJECT g_grid_400_right
      EXPORTING
        i_parent = g_container_400_right.

    CALL METHOD g_splitter_400->set_column_width
      EXPORTING
        id    = 1
        width = 65.
  ENDIF.




ENDFORM.


FORM f_set_field_catalog_400_left .

  REFRESH: gt_fcat_400_left.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_QC_LINE'.

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
      WHEN 'QCJNR' .
        <ls_fcat>-coltext = '行号'.
      WHEN 'ZMM_MS' .
        <ls_fcat>-no_zero = 'X'.
        <ls_fcat>-edit = 'X'.
      WHEN 'QCNO' OR 'QCNR' .
        <ls_fcat>-tech = 'X'.
      WHEN 'QCJNR' OR 'ICON' OR 'TEXT' .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
      WHEN 'NUMC1' OR 'NUMC2' OR 'NUMC3' OR 'NUMC4' OR 'NUMC_SUM' OR 'DEC_PER_100M'.
        <ls_fcat>-edit = ''.
      WHEN OTHERS.
        <ls_fcat>-edit = 'X'.
*        <ls_fcat>-auto_value = 'X'.
        <ls_fcat>-no_zero = 'X'.
    ENDCASE.

    IF g_change = 'D'.
      <ls_fcat>-edit = ''.
    ENDIF.

    <ls_fcat>-no_out = <ls_fcat>-tech.
  ENDLOOP.

  gt_fcat_400_left = lt_fcat.

ENDFORM.
FORM f_set_field_catalog_400_right .


  REFRESH: gt_fcat_400_right.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_QC_LINE_4FZ'.

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
      WHEN 'QCJNR' .
        <ls_fcat>-coltext = '表体行号'.
      WHEN 'QCNO' OR 'QCNR' .
        <ls_fcat>-tech = 'X'.
      WHEN 'QCJNR' OR 'QCTXTNR'  .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
      WHEN 'NUMC1' OR 'NUMC2' OR 'NUMC3' OR 'NUMC4'.
        <ls_fcat>-edit = 'X'.

        <ls_fcat>-no_zero = 'X'.
*        <ls_fcat>-auto_value = 'X'.

    ENDCASE.

    <ls_fcat>-no_out = <ls_fcat>-tech.
    IF g_change = 'D'.
      <ls_fcat>-edit = ''.
    ENDIF.
  ENDLOOP.

  gt_fcat_400_right = lt_fcat.

ENDFORM.





FORM f_create_grid_layout_400 .

  CLEAR: gs_layout_400.
  gs_layout_400-sel_mode   = 'A'.
  gs_layout_400-cwidth_opt = 'X'.
  gs_layout_400-zebra      = 'X'.
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

FORM f_create_grid_toolbar_400
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

FORM f_assign_handlers_400_left
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_400_left.

  SET HANDLER g_event_receiver_400_left->handle_data_changed
          FOR c_grid .

  SET HANDLER g_event_receiver_400_left->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_400_left->data_changed_finished
        FOR c_grid.
  SET HANDLER g_event_receiver_400_left->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_400_left->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_400_left->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_assign_handlers_400_right
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_400_right.

  SET HANDLER g_event_receiver_400_right->data_changed_finished
      FOR c_grid.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_400_right->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_400_right->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_400_right->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_400_right->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_register_event_400_left
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

FORM f_register_event_400_right
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




FORM f_display_grid_alv_400 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.
  ls_variant-handle = 3.

  CALL METHOD g_grid_400_left->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_400
      it_toolbar_excluding = gt_exclude_400[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_qc_line[]
      it_sort              = gt_sort_400_left[]
      it_fieldcatalog      = gt_fcat_400_left[].

  ls_variant-handle = 4.
*
  CALL METHOD g_grid_400_right->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_400
      it_toolbar_excluding = gt_exclude_400[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_qc_line_4fz_dis[]
*     IT_SORT              = GT_SORT[]
      it_fieldcatalog      = gt_fcat_400_right[].

ENDFORM.

MODULE user_command_0400 INPUT.

  CHECK gv_qcmode <> 'Q'.

  CALL METHOD g_grid_400_left->check_changed_data.
  CALL METHOD g_grid_400_right->check_changed_data.


  CASE sy-ucomm.
    WHEN 'ENTR'.
      DATA:l_numc_sum TYPE zafo_numc_sum.
      CLEAR sy-ucomm.
      CLEAR <gs_item>-menge_qc.
      CLEAR l_numc_sum.
      LOOP AT gt_qc_line .
        l_numc_sum = l_numc_sum + gt_qc_line-numc_sum.
        <gs_item>-menge_qc =  <gs_item>-menge_qc + gt_qc_line-zmm_ms.

      ENDLOOP.
      IF <gs_item>-menge_qc NE 0.
        <gs_item>-dec_per_100m =  l_numc_sum / <gs_item>-menge_qc * 100.
      ENDIF.

    WHEN 'EDIT'.

      CLEAR sy-ucomm.

      CASE <gs_item>-qc_status.
        WHEN  ''.
          MESSAGE '无效操作' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN  'A' OR 'B' OR 'C'.
          IF g_change = 'D'.
            g_change = 'M'.
          ELSEIF g_change = 'M'.
            g_change = 'D'.
          ENDIF.

          PERFORM f_set_catalog_alv_400 .

        WHEN OTHERS.
          MESSAGE '已确认无法修改' TYPE 'S' DISPLAY LIKE 'E'.
      ENDCASE.

    WHEN 'SAVE'.
      CLEAR sy-ucomm.

      CLEAR g_error.

      PERFORM frm_400_check.

      IF g_error = 'X'.

        PERFORM frm_pop_msg.

        RETURN.

      ENDIF.

      PERFORM frm_400_save.
      g_change = 'D'.

      PERFORM f_set_catalog_alv_400.

    WHEN '&APPR'.

      IF g_change <> 'D'.
        MESSAGE '只有显示状态可以审批' TYPE 'S'.
      ENDIF.

      IF  <gs_item>-qc_status = 'B'
          OR <gs_item>-qc_status = 'C' OR <gs_item>-qc_status = 'E'.
      ELSE.
        MESSAGE s000(afo) WITH  '当前状态无法调整质检意见'.
        g_error = 'E'.
      ENDIF.

      PERFORM frm_release.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
      APPEND gt_item_modify.
      CLEAR gt_item_modify.

      PERFORM frm_save_db.

    WHEN 'PRINT'.

      IF g_change <> 'D'.
        MESSAGE '只有显示状态可以打印预览' TYPE 'S'.
      ENDIF.

      CALL SCREEN 300.

    WHEN 'BUT01'.
      CLEAR sy-ucomm.
      DATA:l_count TYPE sy-index.
      DATA:lt_flds TYPE TABLE OF sval.
      DATA:ls_flds TYPE sval.
      DATA: p_gv_ret_code TYPE c.
      DATA:l_qccount TYPE sy-index.

      CLEAR lt_flds.
      CLEAR ls_flds.
      ls_flds-tabname = 'ZAFO_QC_SITEM'.
      ls_flds-fieldname = 'QCCOUNT'.
      ls_flds-value = <gs_item>-qccount.
      ls_flds-field_obl = 'X'.
      APPEND ls_flds TO lt_flds.

      l_qccount = <gs_item>-qccount.

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          popup_title     = '请填入卷数'
        IMPORTING
          returncode      = p_gv_ret_code
        TABLES
          fields          = lt_flds
        EXCEPTIONS
          error_in_fields = 1
          OTHERS          = 2.

      IF sy-subrc EQ 0 AND p_gv_ret_code <> 'A'.
        READ TABLE lt_flds INTO ls_flds INDEX 1.
        <gs_item>-qccount = ls_flds-value.
      ELSE.

        MESSAGE '已取消' TYPE 'S' DISPLAY LIKE 'E'.
        g_error = 'X'.
        RETURN.
      ENDIF.

      IF <gs_item>-qccount > l_qccount .
        l_count = <gs_item>-qccount - l_qccount.

        DO l_count TIMES.
          gt_qc_line-qcno = <gs_item>-qcno.
          gt_qc_line-qcnr = <gs_item>-qcnr.
          gt_qc_line-qcjnr = sy-index + l_qccount .
          gt_qc_line-icon = icon_led_inactive.
          gt_qc_line-text = '初始'.
          gt_qc_line-zmm_jib = 'OK'.
          gt_qc_line-zmm_jb = 'OK'.
          gt_qc_line-zmm_sg = 'OK'.
          gt_qc_line-zmm_syh = 'OK'.
          gt_qc_line-zmm_bz = 'OK'.
          gt_qc_line-zmm_xw = 'OK'.

          APPEND gt_qc_line.
          CLEAR gt_qc_line.
        ENDDO.
        l_qccount = <gs_item>-qccount.

      ELSEIF <gs_item>-qccount < l_qccount .

        CLEAR g_error.
        PERFORM frm_pop_confirm USING '该小行数,可能会丢失已填入数据,是否继续?'.
        IF g_error = 'X'.
          <gs_item>-qccount = l_qccount.
        ELSE.
          DELETE gt_qc_line WHERE qcjnr > <gs_item>-qccount.
        ENDIF.

      ENDIF.

      g_change = 'M'.
      PERFORM f_set_catalog_alv_400 .
  ENDCASE.

ENDMODULE.


FORM f_set_catalog_alv_400.


  DATA:ls_disvariant TYPE disvariant.
  DATA: ls_scroll TYPE lvc_s_stbl.
  CALL METHOD g_grid_400_left->get_variant
    IMPORTING
      es_variant = ls_disvariant
*     e_save     =
    .


  PERFORM f_set_field_catalog_400_left.
  CALL METHOD g_grid_400_left->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_400_left.

  CALL METHOD g_grid_400_left->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_400.


  IF gt_fcat_400_right IS NOT INITIAL.
    PERFORM f_set_field_catalog_400_right.

    CALL METHOD g_grid_400_right->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat_400_right.

    CALL METHOD g_grid_400_right->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_400.

  ENDIF.


  CLEAR: ls_scroll.
  ls_scroll-row = 'X'.
  ls_scroll-col = 'X'.
  CALL METHOD g_grid_400_left->refresh_table_display
    EXPORTING
      is_stable = ls_scroll.
  " i_soft_refresh = 'X'.

  CALL METHOD g_grid_400_left->set_variant
    EXPORTING
      is_variant = ls_disvariant
*     i_save     =
    .
ENDFORM.


FORM frm_400_check.
  CLEAR <gs_item>-menge_qc.

*  PERFORM frm_check_item_not_null USING 'TWIST' '扭度' <gs_item>.
  PERFORM frm_check_item_not_null USING 'JCOUNT' '总卷数' <gs_item>.

  IF gv_qcmode = 'A'.
  ELSEIF gv_qcmode = 'B'.
  ELSEIF gv_qcmode = 'C'.
    PERFORM frm_check_item_not_null USING 'MENGE_DS' '检验批次数量' <gs_item>.
  ENDIF.

*  PERFORM frm_check_item_not_null USING 'MENGE_QC' '检验数量' <gs_item>.
*  PERFORM frm_check_item_not_null USING 'PULL_FORCE' '拉力' <gs_item>.
*  PERFORM frm_check_item_not_null USING 'FASTNESS' '色牢度' <gs_item>.
*  PERFORM frm_check_item_not_null USING 'SHRINKAGE' '缩水率' <gs_item>.


  LOOP AT gt_qc_line ASSIGNING FIELD-SYMBOL(<fs_qc_line>) .
    CHECK <fs_qc_line> IS ASSIGNED.

    IF <fs_qc_line>-icon = icon_led_inactive.
      PERFORM frm_add_msg USING 'E' 'ZAFO'  '000' '卷号' <gs_qc_line>-qcjnr '未录入信息' '' .
      CONTINUE.
    ENDIF.
    PERFORM frm_check_line_not_null USING 'ZMM_MS' CHANGING <fs_qc_line>.
    PERFORM frm_check_line_not_null USING 'ZMM_MF1'CHANGING <fs_qc_line>.
    PERFORM frm_check_line_not_null USING 'ZMM_JIB' CHANGING <fs_qc_line>.
    PERFORM frm_check_line_not_null USING 'ZMM_JB' CHANGING <fs_qc_line>.
    PERFORM frm_check_line_not_null USING 'ZMM_XW' CHANGING <fs_qc_line>.
    PERFORM frm_check_line_not_null USING 'ZMM_KZ' CHANGING <fs_qc_line>.
    PERFORM frm_check_line_not_null USING 'ZMM_SG' CHANGING <fs_qc_line>.
    PERFORM frm_check_line_not_null USING 'ZMM_SYH'CHANGING <fs_qc_line>.
    PERFORM frm_check_line_not_null USING 'ZMM_BZ' CHANGING <fs_qc_line>.
    <gs_item>-menge_qc = <gs_item>-menge_qc + <fs_qc_line>-zmm_ms.
  ENDLOOP.

ENDFORM.


FORM frm_check_item_not_null USING fname text cs_data TYPE zafo_qc_sitem.
  FIELD-SYMBOLS:  <fs_value>  TYPE any.
  ASSIGN COMPONENT fname OF STRUCTURE cs_data TO <fs_value>.
  IF sy-subrc EQ 0.
    IF <fs_value> IS INITIAL.
      PERFORM frm_add_msg USING  'E' 'ZAFO' '000' '抬头字段' text '不能为空' '' .
    ENDIF.
  ENDIF.

ENDFORM.

FORM frm_check_line_not_null USING fname CHANGING cs_data TYPE zafo_qc_line.
  FIELD-SYMBOLS:  <fs_value>  TYPE any.
  ASSIGN COMPONENT fname OF STRUCTURE cs_data TO <fs_value>.
  IF sy-subrc EQ 0.
    IF <fs_value> IS INITIAL.
      READ TABLE gt_fcat_400_left INTO DATA(ls_fcat) WITH KEY fieldname = fname.
      IF sy-subrc EQ 0.
        cs_data-icon = icon_led_red.
        cs_data-text = '字段' && ls_fcat-scrtext_s && '不能为空'.
        PERFORM frm_add_msg USING  'E' 'ZAFO' '000' '卷号' cs_data-qcjnr cs_data-text '' .
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM frm_400_save.
  DATA: l_remark_flag(1) TYPE i VALUE 0."备注标识
  DATA:ls_item LIKE TABLE OF zafo_qc_item WITH HEADER LINE.
  DATA:lt_qc_line LIKE TABLE OF zafo_qc_line WITH HEADER LINE.
  DATA:lt_qc_line_4fz LIKE TABLE OF zafo_qc_line_4fz WITH HEADER LINE.

  MOVE-CORRESPONDING <gs_item> TO ls_item.

  IF <gs_item>-dec_per_100m GT 15.
    l_remark_flag = 1.
  ENDIF.

  LOOP AT gt_qc_line WHERE qcnr IS NOT INITIAL.
    IF l_remark_flag EQ 0 AND ( gt_qc_line-zmm_jb NE 'OK' OR gt_qc_line-zmm_jib NE 'OK' ).
      l_remark_flag = 1.
    ENDIF.

    APPEND gt_qc_line TO lt_qc_line.
  ENDLOOP.

  LOOP AT gt_qc_line_4fz .
    APPEND gt_qc_line_4fz TO lt_qc_line_4fz.
  ENDLOOP.

  LOOP AT gt_qc_line_4fz_dis .
    APPEND gt_qc_line_4fz_dis TO lt_qc_line_4fz.
  ENDLOOP.

  IF ls_item-zremark IS INITIAL AND l_remark_flag EQ 1."备注为空时给值
    <gs_item>-zremark = '工时及损耗由供应商承担'.
    ls_item-zremark = '工时及损耗由供应商承担'.
  ENDIF.


  MODIFY zafo_qc_item FROM ls_item.
  MODIFY zafo_qc_line FROM TABLE lt_qc_line.
  MODIFY zafo_qc_line_4fz FROM TABLE lt_qc_line_4fz.

  COMMIT WORK AND WAIT.

  MESSAGE '保存成功' TYPE 'S'.

ENDFORM.

FORM frm_add_msg USING msgty
                        msgid
                        msgno
                        msgv1
                        msgv2
                        msgv3
                        msgv4.

  CLEAR gt_message.
  gt_message-msgid = msgid .
  gt_message-msgty = msgty .
  gt_message-msgno = msgno .
  gt_message-msgv1 = msgv1 .
  gt_message-msgv2 = msgv2 .
  gt_message-msgv3 = msgv3 .
  gt_message-msgv4 = msgv4 .
  APPEND gt_message.


  CLEAR ot_return.
  IF msgty = 'E' OR msgty =  'A'.
    g_error = 'X'.
  ENDIF.
  ot_return-type = msgty.
  ot_return-id = msgid.
  ot_return-number = msgno.
  ot_return-message_v1 = msgv1.
  ot_return-message_v2 = msgv2.
  ot_return-message_v3 = msgv3.
  ot_return-message_v4 = msgv4.
  MESSAGE ID ot_return-id TYPE ot_return-type NUMBER ot_return-number
     INTO ot_return-message WITH ot_return-message_v1
                                 ot_return-message_v2
                                 ot_return-message_v3
                                 ot_return-message_v4.
  APPEND ot_return.

ENDFORM.

FORM frm_pop_msg .
  IF gt_message[] IS NOT INITIAL.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = gt_message[].
    PERFORM frm_clear_msg.
  ENDIF.

ENDFORM.

FORM frm_clear_msg .
  CLEAR gt_message[].
  CLEAR ot_return[].
ENDFORM.

FORM f_handle_double_click_400_left USING e_row TYPE lvc_s_row e_column.


  LOOP AT gt_qc_line_4fz_dis .
    APPEND gt_qc_line_4fz_dis TO gt_qc_line_4fz.
    DELETE gt_qc_line_4fz_dis.
    CLEAR gt_qc_line_4fz_dis.
  ENDLOOP.

*  CLEAR gt_qc_line_4fz_dis[].


  READ TABLE gt_qc_line ASSIGNING <gs_qc_line> INDEX e_row-index .
  IF sy-subrc EQ  0.
    LOOP AT gt_qc_line_4fz WHERE qcno = <gs_qc_line>-qcno
      AND qcnr = <gs_qc_line>-qcnr
      AND qcjnr = <gs_qc_line>-qcjnr.
      APPEND gt_qc_line_4fz TO gt_qc_line_4fz_dis.
      DELETE gt_qc_line_4fz.
      CLEAR gt_qc_line_4fz.

    ENDLOOP.

    IF sy-subrc NE 0.
      SELECT
        afonr AS qctxtnr,
        qctxt
        FROM
        zafo_qc_model_i
        WHERE qcmodel = @<gs_item>-qcmodel
        INTO CORRESPONDING FIELDS OF TABLE @gt_qc_line_4fz_dis.

      LOOP AT gt_qc_line_4fz_dis.
        gt_qc_line_4fz_dis-qcno = <gs_qc_line>-qcno.
        gt_qc_line_4fz_dis-qcnr = <gs_qc_line>-qcnr.
        gt_qc_line_4fz_dis-qcjnr = <gs_qc_line>-qcjnr.
        MODIFY gt_qc_line_4fz_dis.
      ENDLOOP.

    ENDIF.


    PERFORM f_refresh_grid_alv USING g_grid_400_right.
  ENDIF.

ENDFORM.

FORM f_toolbar_400_left USING ut_toolbar TYPE ttb_button.

  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE '&MASS_MODIFY' TO ls_toolbar-function.
  MOVE icon_ws_confirm_whse_proc_back TO ls_toolbar-icon.
  MOVE '批量维护' TO ls_toolbar-quickinfo.
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE '批量维护' TO ls_toolbar-text.
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.
ENDFORM.



FORM f_data_changed_finished_400_l USING e_modified et_good_cells TYPE lvc_t_modi.
  DATA:ls_refresh TYPE char1.
  DATA:ls_refresh_mx TYPE char1.
  CLEAR ls_refresh.
  CLEAR ls_refresh_mx.

  CHECK NOT et_good_cells IS INITIAL.

  DELETE gt_qc_line WHERE qcnr IS INITIAL.

  LOOP AT et_good_cells INTO DATA(ls_cell).
    READ TABLE gt_qc_line ASSIGNING <gs_qc_line> INDEX ls_cell-row_id.
    CHECK sy-subrc = 0.
    CHECK <gs_qc_line> IS ASSIGNED.

    IF <gs_qc_line>-icon <> icon_led_yellow.
      <gs_qc_line>-icon = icon_led_yellow.
      <gs_qc_line>-text = '已修改'.
      ls_refresh = 'X'.
    ENDIF.

    IF ls_cell-fieldname = 'ZMM_MS'.
      ls_refresh_mx = 'X'.
      IF <gs_qc_line>-zmm_ms IS NOT INITIAL.

        <gs_qc_line>-dec_per_100m = <gs_qc_line>-numc_sum / <gs_qc_line>-zmm_ms * 100.
      ELSE.
        CLEAR <gs_qc_line>-dec_per_100m.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF ls_refresh_mx = 'X'.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = 'ENTR'.

  ENDIF.

  PERFORM f_refresh_grid_alv USING g_grid_400_left.
ENDFORM.


FORM f_data_changed_finished_400_r  USING  e_modified
                                   et_good_cells TYPE lvc_t_modi.

*
  DATA:ls_refresh TYPE char1.
  CLEAR ls_refresh.
  CHECK NOT et_good_cells IS INITIAL.
*
  LOOP AT et_good_cells INTO DATA(ls_cell).


    READ TABLE gt_qc_line_4fz_dis ASSIGNING <gs_qc_line_4fz>
                                  INDEX ls_cell-row_id.

    CHECK sy-subrc = 0.

    <gs_qc_line_4fz>-numc_sum = <gs_qc_line_4fz>-numc1 * 1
                            + <gs_qc_line_4fz>-numc2 * 2
                            + <gs_qc_line_4fz>-numc3 * 3
                            + <gs_qc_line_4fz>-numc4 * 4.

    ls_refresh = 'X'.

  ENDLOOP.

  IF ls_refresh = 'X'.
    CLEAR <gs_qc_line>-numc1.
    CLEAR <gs_qc_line>-numc2.
    CLEAR <gs_qc_line>-numc3.
    CLEAR <gs_qc_line>-numc4.
    CLEAR <gs_qc_line>-numc_sum.
    LOOP AT gt_qc_line_4fz_dis .
      <gs_qc_line>-numc1 = gt_qc_line_4fz_dis-numc1 + <gs_qc_line>-numc1.
      <gs_qc_line>-numc2 = gt_qc_line_4fz_dis-numc2 + <gs_qc_line>-numc2.
      <gs_qc_line>-numc3 = gt_qc_line_4fz_dis-numc3 + <gs_qc_line>-numc3.
      <gs_qc_line>-numc4 = gt_qc_line_4fz_dis-numc4 + <gs_qc_line>-numc4.
      <gs_qc_line>-numc_sum = gt_qc_line_4fz_dis-numc_sum + <gs_qc_line>-numc_sum.

      IF <gs_qc_line>-zmm_ms IS NOT INITIAL.
        <gs_qc_line>-dec_per_100m = <gs_qc_line>-numc_sum / <gs_qc_line>-zmm_ms * 100.
      ELSE.
        CLEAR <gs_qc_line>-dec_per_100m.
      ENDIF.
      IF <gs_qc_line>-icon <> icon_led_yellow.
        <gs_qc_line>-icon = icon_led_yellow.
        <gs_qc_line>-text = '已修改'.
      ENDIF.

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = 'ENTR'.

    ENDLOOP.


*    PERFORM f_refresh_grid_alv USING g_grid_400_left.
*    PERFORM f_refresh_grid_alv USING g_grid_400_right.
  ENDIF.


ENDFORM.
