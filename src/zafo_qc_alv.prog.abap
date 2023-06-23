*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:lcl_event_receiver_100_head DEFINITION DEFERRED.
CLASS:lcl_event_receiver_100_item DEFINITION DEFERRED.

DATA: g_event_receiver_100_head TYPE REF TO lcl_event_receiver_100_head,
      g_event_receiver_100_item TYPE REF TO lcl_event_receiver_100_item.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_100_head DEFINITION.

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
CLASS lcl_event_receiver_100_head IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_100_head
      USING er_data_changed
            e_onf4.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
    PERFORM f_handle_double_click_100_head USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_100_head USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_user_command.
*    PERFORM f_user_command_100_head USING e_ucomm.
  ENDMETHOD.

  METHOD data_changed_finished.
*    PERFORM f_data_changed_finished_100 USING e_modified et_good_cells.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_100_item DEFINITION.

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


ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_100_item IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM F_HANDLE_DOUBLE_CLICK_100_item USING E_ROW E_COLUMN.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_100_item USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_100 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_100_item USING e_ucomm.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION


DATA: g_grid_100_head         TYPE REF TO cl_gui_alv_grid,
      g_grid_100_item         TYPE REF TO cl_gui_alv_grid,
      gt_fcat_100_head        TYPE lvc_t_fcat,
      gt_fcat_100_item        TYPE lvc_t_fcat,


      gs_layout_100           TYPE lvc_s_layo,
      gt_sort_100_head        TYPE lvc_t_sort,
      gt_exclude_100          TYPE ui_functions,
      g_docking_container_100 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_100  TYPE REF TO cl_gui_custom_container,
      g_container_100_head    TYPE REF TO cl_gui_container,
      g_container_100_item    TYPE REF TO cl_gui_container,
      g_splitter_100          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_100_head      TYPE REF TO cl_gui_toolbar.


FORM f_handle_data_changed_100_head USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
                                              u_onf4  TYPE any.
  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS: <fs_changed> TYPE any,
                 <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.
  ENDLOOP.

ENDFORM.


FORM f_handle_hotspot_100_item USING e_row_id TYPE lvc_s_row
                                     e_column_id TYPE lvc_s_col.

  READ TABLE gt_item ASSIGNING <gs_item> INDEX e_row_id-index .
  IF sy-subrc EQ  0.
    CLEAR gs_model_h.

    SELECT SINGLE * INTO gs_model_h
      FROM zafo_qc_model_h
      WHERE qcmodel = <gs_item>-qcmodel.

    IF gs_model_h-qctype = 'A'.

      CLEAR gt_qc_line[].
      CLEAR gt_qc_line_4fz[].
      CLEAR gt_qc_line_4fz_dis[].

      SELECT * INTO TABLE @gt_qc_line
        FROM zafo_qc_line
        WHERE qcno = @<gs_item>-qcno
        AND qcnr = @<gs_item>-qcnr.
      IF sy-subrc EQ 0.
        g_change = 'D'.
      ELSE.
        g_change = 'M'.
      ENDIF.

      SELECT * INTO TABLE @gt_qc_line_4fz
        FROM zafo_qc_line_4fz
        WHERE qcno = @<gs_item>-qcno
        AND qcnr = @<gs_item>-qcnr.

      IF <gs_item>-iqc IS INITIAL.
        <gs_item>-iqc = <gs_item>-ernam.
      ENDIF.

      CALL SCREEN 400 .

    ELSEIF gs_model_h-qctype = 'B'.

      SELECT * INTO TABLE gt_item_i
        FROM zafo_qc_item_i
         WHERE qcno = <gs_item>-qcno
         AND qcnr = <gs_item>-qcnr
         ORDER BY qcnr.
      IF sy-subrc EQ 0.

      ELSE.
        SELECT * INTO TABLE gt_model_i
          FROM zafo_qc_model_i
          WHERE qcmodel = <gs_item>-qcmodel
          ORDER BY afonr  .
        IF sy-subrc EQ 0.
          LOOP AT gt_model_i.
            MOVE-CORRESPONDING gt_model_i TO gt_item_i.
            APPEND gt_item_i.
            CLEAR gt_item_i.
          ENDLOOP.
        ENDIF.
      ENDIF.

      LOOP AT gt_item_i.
        gt_item_i-qcno = <gs_item>-qcno.
        gt_item_i-qcnr = <gs_item>-qcnr.
        gt_item_i-meins = 'EA'." 取整数
        MODIFY gt_item_i.
      ENDLOOP.
      IF <gs_item>-iqc IS INITIAL.
        <gs_item>-iqc = <gs_item>-ernam.
      ENDIF.

      CALL SCREEN 500 .
    ENDIF.

    PERFORM f_refresh_grid_alv USING g_grid_100_item.

  ENDIF.

ENDFORM.

FORM f_handle_hotspot_100_head USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
  IF e_column_id-fieldname = 'QCNO'.
    READ TABLE gt_head ASSIGNING <gs_head> INDEX e_row_id-index .

    IF sy-subrc EQ 0.
      SELECT * FROM zafo_qc_item
        INTO CORRESPONDING FIELDS OF TABLE gt_item
        WHERE qcno = <gs_head>-qcno.
      CHECK sy-subrc EQ 0.
      LOOP AT gt_item ASSIGNING <gs_item>.
        PERFORM frm_set_icon
          USING <gs_item>-qc_status CHANGING <gs_item>-icon <gs_item>-text.
      ENDLOOP.


      PERFORM f_refresh_grid_alv USING g_grid_100_item.
    ENDIF.
  ENDIF.

ENDFORM.


FORM f_handle_user_command_100_head USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.


ENDFORM.


FORM f_toolbar_100 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

*  CLEAR ls_toolbar.
*  MOVE '&MODIFY' TO ls_toolbar-function.
*  MOVE icon_status_ok TO ls_toolbar-icon.
*  MOVE '维护质检报告' TO ls_toolbar-quickinfo.
*  MOVE ' ' TO ls_toolbar-disabled.
*  MOVE '维护质检报告' TO ls_toolbar-text.
*  APPEND ls_toolbar TO ut_toolbar.
*  CLEAR ls_toolbar.

  IF  gv_qcmode = 'C'.
    CLEAR ls_toolbar.
    MOVE '&CREATE' TO ls_toolbar-function.
    MOVE icon_create TO ls_toolbar-icon.
    MOVE '添加报告' TO ls_toolbar-quickinfo.
    MOVE ' ' TO ls_toolbar-disabled.
    MOVE '添加报告' TO ls_toolbar-text.
    APPEND ls_toolbar TO ut_toolbar.
    CLEAR ls_toolbar.
  ENDIF.

*  CLEAR ls_toolbar.
*  MOVE '&APPR' TO ls_toolbar-function.
*  MOVE icon_status_ok TO ls_toolbar-icon.
*  MOVE '质检确认' TO ls_toolbar-quickinfo.
*  MOVE ' ' TO ls_toolbar-disabled.
*  MOVE '质检确认' TO ls_toolbar-text.
*  APPEND ls_toolbar TO ut_toolbar.
*  CLEAR ls_toolbar.
*
*  CLEAR ls_toolbar.
*  MOVE '&ALLOW' TO ls_toolbar-function.
*  MOVE icon_status_ok TO ls_toolbar-icon.
*  MOVE '采购确认' TO ls_toolbar-quickinfo.
*  MOVE ' ' TO ls_toolbar-disabled.
*  MOVE '采购确认' TO ls_toolbar-text.
*  APPEND ls_toolbar TO ut_toolbar.
*  CLEAR ls_toolbar.
ENDFORM.


FORM f_handle_double_click_100_head USING e_row TYPE lvc_s_row e_column.
  READ TABLE gt_head ASSIGNING <gs_head> INDEX e_row-index .
  IF sy-subrc EQ  0.
    SELECT * FROM zafo_qc_item
      INTO CORRESPONDING FIELDS OF TABLE gt_item
      WHERE qcno = <gs_head>-qcno.

    CHECK sy-subrc EQ 0.

    LOOP AT gt_item ASSIGNING <gs_item>.
      PERFORM frm_set_icon
        USING <gs_item>-qc_status CHANGING <gs_item>-icon <gs_item>-text.
    ENDLOOP.

    PERFORM f_refresh_grid_alv USING g_grid_100_item.
  ENDIF.

ENDFORM.


MODULE create_object_100 OUTPUT.

  IF g_grid_100_head IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_100.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_field_catalog_100_head.
    PERFORM f_set_field_catalog_100_item.
**-- LAYOUT
    PERFORM f_create_grid_layout_100.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_100  CHANGING gt_exclude_100[].

    PERFORM f_create_grid_sort_100_head CHANGING gt_sort_100_head[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_handlers_100_head CHANGING g_grid_100_head.
    PERFORM f_assign_handlers_100_item CHANGING g_grid_100_item.


**-- REGISTER EVENT
    PERFORM f_register_event_100_head USING g_grid_100_head.
    PERFORM f_register_event_100_item USING g_grid_100_item.


    CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_100.
*--
    CALL METHOD g_grid_100_head->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid_100_head.

    PERFORM f_refresh_grid_alv USING g_grid_100_item.

  ENDIF.
ENDMODULE.


FORM f_create_container_100 .
  IF g_docking_container_100 IS INITIAL.

    CREATE OBJECT g_docking_container_100
      EXPORTING
        style     = cl_gui_control=>ws_child
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = g_docking_container_100->dock_at_bottom
        lifetime  = cl_gui_control=>lifetime_imode
        extension = '5000'
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

*  CREATE OBJECT g_cumtom_container_100
*    EXPORTING
*      container_name = 'ITEM'.


* SPLITTER CONTAINER
  IF g_splitter_100 IS INITIAL.

    CREATE OBJECT g_splitter_100
      EXPORTING
        parent  = g_docking_container_100
        rows    = 2
        columns = 1.

    g_container_100_head  = g_splitter_100->get_container( row = 1 column = 1 ).

    CREATE OBJECT g_grid_100_head
      EXPORTING
        i_parent = g_container_100_head.


    g_container_100_item = g_splitter_100->get_container( row = 2 column = 1 ).

    CREATE OBJECT g_grid_100_item
      EXPORTING
        i_parent = g_container_100_item.

  ENDIF.
ENDFORM.


FORM f_set_field_catalog_100_head .

  REFRESH: gt_fcat_100_head.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_QC_SHEAD'.

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

  PERFORM f_transfer_slis_to_lvc  CHANGING lt_fieldcat lt_fcat.

  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    IF <ls_fcat>-col_pos < 6.
      <ls_fcat>-fix_column = 'X'.
      <ls_fcat>-emphasize = 'C110'.
    ENDIF.

    CASE <ls_fcat>-fieldname.
      WHEN 'ZCOLOR_TEXT'.
        <ls_fcat>-tech = 'X'.

      WHEN 'MENGE'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '基本单位数量'.

      WHEN 'MENGE_GR'.
        <ls_fcat>-emphasize = 'C300'.
        CASE gv_qcmode.
          WHEN 'A'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '收货数量'.

          WHEN 'B'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '入库数量'.

          WHEN 'C'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '采购数量'.
        ENDCASE.

      WHEN 'MENGE_KC'.
        CASE gv_qcmode.
          WHEN 'B'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '库存数量'.
            <ls_fcat>-emphasize = 'C600'.
          WHEN OTHERS.
            <ls_fcat>-tech = 'X'.
        ENDCASE.

      WHEN 'MENGE_QC'.
        CASE gv_qcmode.
          WHEN 'A' OR 'B'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '合格数量'.
            <ls_fcat>-emphasize = 'C500'.
          WHEN 'C'.
            <ls_fcat>-tech = 'X'.
        ENDCASE.

      WHEN 'MENGE_QC_F'.
        CASE gv_qcmode.
          WHEN 'A' OR 'B'.
            <ls_fcat>-emphasize = 'C600'.
          WHEN 'C'.
            <ls_fcat>-tech = 'X'.
        ENDCASE.

      WHEN 'QCNO'.
        <ls_fcat>-hotspot = 'X'.

      WHEN 'DATUM_GR'.
        CASE gv_qcmode.
          WHEN 'A'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '收货日期'.

          WHEN 'B'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '入库日期'.

          WHEN 'C'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '采购日期'.
        ENDCASE.

    ENDCASE.

  ENDLOOP.

  gt_fcat_100_head = lt_fcat.

ENDFORM.


FORM f_set_field_catalog_100_item .

  REFRESH: gt_fcat_100_item.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_QC_SITEM'.

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
    IF <ls_fcat>-col_pos < 7.
      <ls_fcat>-fix_column = 'X'.
      <ls_fcat>-emphasize = 'C110'.
    ENDIF.

    CASE <ls_fcat>-fieldname.
      WHEN 'ZCOLOR_TEXT'.
        <ls_fcat>-tech = 'X'.

      WHEN 'MENGE_QC'.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
        <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '检验数量'.
        <ls_fcat>-emphasize = 'C300'.

      WHEN 'MENGE_GR'.
        <ls_fcat>-emphasize = 'C300'.
        CASE gv_qcmode.
          WHEN 'A'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '收货数量'.

          WHEN 'B'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '入库数量'.

          WHEN 'C'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '采购数量'.
        ENDCASE.

      WHEN 'QCNO'.
        <ls_fcat>-hotspot = 'X'.

      WHEN 'DATUM_GR'.
        CASE gv_qcmode.
          WHEN 'A'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '收货日期'.

          WHEN 'B'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '入库日期'.

          WHEN 'C'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l = <ls_fcat>-reptext = '采购日期'.
        ENDCASE.

    ENDCASE.

  ENDLOOP.

  gt_fcat_100_item = lt_fcat.

ENDFORM.


FORM f_create_grid_layout_100 .

  CLEAR: gs_layout_100.
  gs_layout_100-sel_mode   = 'A'.
  gs_layout_100-cwidth_opt = 'X'.
  gs_layout_100-zebra      = 'X'.
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


FORM f_create_grid_toolbar_100
  CHANGING  c_t_toolbar TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.

  CLEAR: c_t_toolbar[].

*  ls_exclude = cl_gui_alv_grid=>mc_fc_excl_all.
*  APPEND  ls_exclude  TO c_t_toolbar.

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


FORM f_create_grid_sort_100_head CHANGING  c_t_sort TYPE lvc_t_sort.

  DATA ls_sort TYPE lvc_s_sort .
  ls_sort-spos = '1' .
  ls_sort-fieldname = 'ZZPINO' .
  ls_sort-up = 'X' . "A to Z
  ls_sort-down = space .
  APPEND ls_sort TO c_t_sort.
  ls_sort-spos = '2' .
  ls_sort-fieldname = 'IDNLF' .
  ls_sort-up = space . "A to Z
  ls_sort-down = 'X' .
  APPEND ls_sort TO c_t_sort.
  ls_sort-spos = '3' .
  ls_sort-fieldname = 'ZCOLOR' .
  ls_sort-up = space . "A to Z
  ls_sort-down = 'X' .
  APPEND ls_sort TO c_t_sort.
ENDFORM.

FORM f_assign_handlers_100_head
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_100_head.

  SET HANDLER g_event_receiver_100_head->handle_data_changed
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOOLBAR
*          FOR C_GRID .
  SET HANDLER g_event_receiver_100_head->data_changed_finished
        FOR c_grid.
  SET HANDLER g_event_receiver_100_head->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_100_head->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_100_head->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_assign_handlers_100_item
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_100_item.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_100_item->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_100_item->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_100_item->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_100_item->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_register_event_100_head
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

FORM f_register_event_100_item
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


FORM f_display_grid_alv_100 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.
  ls_variant-handle = 1.

  CALL METHOD g_grid_100_head->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_100
      it_toolbar_excluding = gt_exclude_100[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_head[]
      it_sort              = gt_sort_100_head[]
      it_fieldcatalog      = gt_fcat_100_head[].

  ls_variant-handle = 2.
*
  CALL METHOD g_grid_100_item->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_100
      it_toolbar_excluding = gt_exclude_100[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item[]
*     IT_SORT              = GT_SORT[]
      it_fieldcatalog      = gt_fcat_100_item[].

ENDFORM.


FORM f_set_catalog_alv_100.

  PERFORM f_set_field_catalog_100_head.
  CALL METHOD g_grid_100_head->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_100_head.

  CALL METHOD g_grid_100_head->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_100.


  IF gt_fcat_100_item IS NOT INITIAL.
    PERFORM f_set_field_catalog_100_item.

    CALL METHOD g_grid_100_item->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat_100_item.

    CALL METHOD g_grid_100_item->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_100.
  ENDIF.
ENDFORM.


MODULE user_command_100 INPUT.

  PERFORM frm_user_command_100 USING sy-ucomm.

ENDMODULE.


FORM f_refresh_grid_alv
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



FORM f_transfer_slis_to_lvc
  CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv
           ct_fcat     TYPE lvc_t_fcat..

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

*  IF gt_dd03l[] IS INITIAL.
*    SELECT * INTO TABLE gt_dd03l
*      FROM dd03l
*      WHERE tabname = 'ZAFO_QC_SITEM'.
*  ENDIF.
*
*  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<cs_fcat>).
*    READ TABLE gt_dd03l WITH KEY fieldname = <cs_fcat>-fieldname.
*    IF sy-subrc EQ 0.
*      IF gt_dd03l-checktable IS NOT INITIAL .
*        <cs_fcat>-f4availabl = 'X'.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

ENDFORM.



MODULE exit INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.



MODULE exit_command INPUT.

  CASE sy-ucomm.

    WHEN 'BACK'.

      CLEAR sy-ucomm.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.

      LEAVE PROGRAM.

  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.
