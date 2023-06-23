*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:lcl_event_receiver_grid_100 DEFINITION DEFERRED.

DATA:g_event_receiver_grid_100   TYPE REF TO lcl_event_receiver_grid_100.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_100 DEFINITION.

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

    METHODS: data_changed_finished
      FOR EVENT data_changed_finished
      OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_100 IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_100
      USING er_data_changed e_onf4.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM f_handle_double_click_100 USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_click_100 USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_100 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_100 USING e_modified et_good_cells.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_100 USING e_ucomm.
  ENDMETHOD.

ENDCLASS.


DATA: g_grid_100              TYPE REF TO cl_gui_alv_grid,
      gt_fcat_100             TYPE lvc_t_fcat,
      gs_layout_100           TYPE lvc_s_layo,
      gt_sort_100             TYPE lvc_t_sort,
      gt_exclude_100          TYPE ui_functions,
      g_docking_container_100 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_100  TYPE REF TO cl_gui_custom_container,
      g_container_100         TYPE REF TO cl_gui_container,
      g_splitter_100          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_100           TYPE REF TO cl_gui_toolbar.


FORM f_handle_data_changed_100 USING u_changed TYPE REF TO cl_alv_changed_data_protocol
                                        u_onf4  TYPE any.
  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS: <fs_changed> TYPE any,
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


FORM f_handle_double_click_100 USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
*  CLEAR GT_ITEM_DIS[].
*  PERFORM GET_DIS_DATA USING E_ROW_ID-INDEX.
*
*  PERFORM F_REFRESH_GRID_ALV USING G_GRID2.

ENDFORM.


FORM f_handle_hotspot_click_100 USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
  CASE e_column_id-fieldname.
    WHEN 'AFONO'.
      READ TABLE gt_item_100 ASSIGNING <gs_item> INDEX e_row_id-index .

      IF sy-subrc EQ 0.
        SELECT SINGLE
          bustyp
          INTO @DATA(ls_bustyp)
          FROM zafo_head WHERE afono = @<gs_item>-afono.

        CHECK sy-subrc EQ 0.

        SELECT SINGLE tcode
          INTO @DATA(ls_tcode)
          FROM zafo_bustype
          WHERE bustyp = @ls_bustyp.

        SET PARAMETER ID 'ZAFONO' FIELD <gs_item>-afono.
        SET PARAMETER ID 'ZBUSTYP' FIELD ls_bustyp.

        CALL TRANSACTION ls_tcode AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'EBELN'.
      READ TABLE gt_item_100 ASSIGNING <gs_item> INDEX e_row_id-index .
      CALL FUNCTION 'ZAFO_CALL_TRANSACTION'
        EXPORTING
          afono = <gs_item>-ebeln.


    WHEN 'SELECTED'.

      READ TABLE gt_item_100 ASSIGNING <gs_item> INDEX e_row_id-index.
      IF sy-subrc EQ 0.
        IF <gs_item>-selected = 'X'.
          CLEAR <gs_item>-selected.
        ELSE.
          <gs_item>-selected = 'X'.
        ENDIF.
      ENDIF.

  ENDCASE.
  PERFORM f_refresh_grid_alv USING g_grid_100.

ENDFORM.


FORM f_handle_user_command_100 USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
*  CASE OK_CODE.
*    WHEN '&NEW'.
*      SUBMIT ZMMR0050 WITH MODIFY = ''
*                WITH CREATE = 'X'
*                VIA SELECTION-SCREEN
*                AND RETURN.
*
*
*    WHEN '&DETAIL'.
*      CLEAR:GT_ITEM_DIS.
*
*      CALL METHOD G_GRID_100->GET_SELECTED_ROWS
*        IMPORTING
*          ET_INDEX_ROWS = LT_INDEX_ROWS
*          ET_ROW_NO     = LT_ROW_NO.
*      IF LT_INDEX_ROWS[] IS INITIAL.
*        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.
*
*      LOOP AT LT_INDEX_ROWS INTO LS_INDEX_ROWS.
*
*        PERFORM GET_DIS_DATA USING LS_INDEX_ROWS-INDEX.
*
*      ENDLOOP.
*
*      CALL METHOD G_GRID2->SET_FRONTEND_LAYOUT
*        EXPORTING
*          IS_LAYOUT = GS_LAYOUT.
*
*  ENDCASE.

ENDFORM.


FORM f_toolbar_100 USING ut_toolbar TYPE ttb_button.

  CHECK g_action = 'CRE'.
  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE '&SURE' TO ls_toolbar-function.
  MOVE icon_checked TO ls_toolbar-icon.
  MOVE TEXT-017 TO ls_toolbar-quickinfo."'确认选择'
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-017 TO ls_toolbar-text."'确认选择'
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


FORM f_data_changed_finished_100  USING  e_modified
                                   et_good_cells TYPE lvc_t_modi.

  DATA:ls_refresh TYPE char1.
  DATA:lv_werks TYPE werks_d.

  CLEAR ls_refresh.
  CHECK NOT et_good_cells IS INITIAL.


  LOOP AT et_good_cells INTO DATA(ls_cell).

    READ TABLE gt_item_100 ASSIGNING <gs_item> INDEX ls_cell-row_id.
    CHECK sy-subrc = 0.

    IF ls_cell-fieldname CP 'MENGE*' OR ls_cell-fieldname CP 'PRICE*'.
      IF gs_bustyp-execute_type = 'PO'.
        PERFORM frm_po_item_calculation IN PROGRAM saplzafo IF FOUND CHANGING <gs_item>.
        ls_refresh = 'X'.
      ENDIF.
    ENDIF.

    CASE ls_cell-fieldname.

      WHEN 'EBELN'.
        IF gs_bustyp-bustyp = 'POC01'.
          CLEAR lv_werks.
          SELECT SINGLE werks INTO lv_werks
            FROM zafo_head WHERE ebeln = <gs_item>-ebeln.
          IF sy-subrc EQ 0 .
            LOOP AT gt_item_po_100 ASSIGNING <gs_item_po> WHERE icon = <gs_item>-icon
                                      AND werks = <gs_item>-werks
                                      AND afono = <gs_item>-afono
                                      AND zzpino = <gs_item>-zzpino
                                      AND idnlf = <gs_item>-idnlf
                                      AND matnr = <gs_item>-matnr
                                      AND zcolor = <gs_item>-zcolor
                                      AND zsize = <gs_item>-zsize
                                      AND znorms = <gs_item>-znorms
                                      AND zppflag = <gs_item>-zppflag.
              <gs_item_po>-werks = lv_werks.
              <gs_item_po>-ebeln = <gs_item>-ebeln.
            ENDLOOP.
            <gs_item>-werks = lv_werks.
            ls_refresh = 'X'.
          ENDIF.
        ENDIF.

      WHEN 'PRICE_LONG'.

        PERFORM frm_set_price  CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN 'PRICE'.

        PERFORM f_set_amount CHANGING <gs_item>.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN 'MENGE_ZJ'.

        PERFORM frm_set_po_menge CHANGING <gs_item>.

      WHEN 'MENGE_CG'.

        PERFORM f_set_menge CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN 'MENGE'.

        PERFORM f_set_menge_cg CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN 'SELECTED'.

        IF ( gs_bustyp-busref = 'Z' OR gs_bustyp-busref = 'Y' )
                AND gs_bustyp-execute_type <> 'PRO'.
          LOOP AT gt_item_100 ASSIGNING FIELD-SYMBOL(<fs_item>) .
            IF <fs_item>-afono = <gs_item>-afono.
              <fs_item>-selected = <gs_item>-selected.
              ls_refresh = 'X'.
            ELSE.
              CLEAR <fs_item>-selected.
              ls_refresh = 'X'.
            ENDIF.
          ENDLOOP.
        ENDIF.

      WHEN OTHERS.

        IF <gs_item>-selected = ''.
          <gs_item>-selected = 'X'.
          ls_refresh = 'X'.
        ENDIF.

    ENDCASE.

  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_100.
  ENDIF.
*  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
*    EXPORTING
*      NEW_CODE = 'ZDATA_CHANGE'.

ENDFORM.


FORM f_user_command_100 USING ok_code.
  CLEAR g_error.
  PERFORM frm_clear_msg.

  CASE ok_code.
    WHEN '&SURE'.
      g_ok_code = '&SURE' .

      PERFORM frm_zafo_preprocess.

      PERFORM frm_check_choise.

      PERFORM frm_pop_msg.
      IF g_error EQ 'X'.
        RETURN.
      ELSE.
        PERFORM frm_sure_item .
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&MASS_MODIFY'.

      PERFORM frm_mass_modify_100 TABLES gt_fcat_100.
      PERFORM f_refresh_grid_alv USING g_grid_100.
    WHEN ''.

  ENDCASE.
ENDFORM.



MODULE create_object_0100 OUTPUT.

  IF g_grid_100 IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_100.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_grid_field_catalog_100.
*    PERFORM F_SET_GRID_FIELD_CATALOG2.
**-- LAYOUT
    PERFORM f_create_grid_layout_100.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_100  CHANGING gt_exclude_100[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_grid_handlers_100 CHANGING g_grid_100.
*    PERFORM F_ASSIGN_GRID_EVENT_HANDLERS CHANGING G_GRID2.
**-- REGISTER EVENT
    PERFORM f_register_grid_event_100 USING g_grid_100.
*    PERFORM F_REGISTER_GRID_EVENT2 USING G_GRID2.
**--
    CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_100.
*--
    CALL METHOD g_grid_100->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid_100.

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
  IF g_splitter_100 IS INITIAL.
    CREATE OBJECT g_splitter_100
      EXPORTING
        parent  = g_docking_container_100
        rows    = 1
        columns = 1.

    g_container_100  = g_splitter_100->get_container( row = 1 column = 1 ).
*    G_CONTAINER_2  = G_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

  ENDIF.

  CREATE OBJECT g_grid_100
    EXPORTING
      i_parent = g_container_100.

*  CREATE OBJECT G_GRID2
*    EXPORTING
*      I_PARENT = G_CONTAINER_2.

ENDFORM.


FORM f_set_grid_field_catalog_100 .

  REFRESH: gt_fcat_100.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  DATA:lt_screen TYPE TABLE OF zafo_screen .

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

  PERFORM f_transfer_slis_to_lvc  CHANGING lt_fieldcat  lt_fcat.

  lt_screen[] = gt_screen[].


  CASE gs_bustyp-busref.
    WHEN 'Y'.
      DELETE lt_screen WHERE object = g_object_ref .
    WHEN 'Z'.
      IF g_object_ref IS INITIAL.
        DELETE lt_screen WHERE object = g_object_ref .
      ELSE.
        DELETE lt_screen WHERE object = g_object .
      ENDIF.
  ENDCASE.

* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'SELECTED' .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
        <ls_fcat>-edit = 'X'.
*        <ls_fcat>-auto_value = 'X'.
        <ls_fcat>-checkbox = 'X'.
*        <ls_fcat>-hotspot = 'X'.
        CONTINUE.
      WHEN 'ICON' OR 'TEXT'  .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.

      WHEN 'EBELN' .
        <ls_fcat>-hotspot = 'X'.
      WHEN 'AFONO' .
        IF gs_bustyp_ref-bustyp_name1 IS NOT INITIAL.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
          <ls_fcat>-scrtext_l = <ls_fcat>-reptext = gs_bustyp_ref-bustyp_name1.
        ENDIF.

        <ls_fcat>-hotspot = 'X'.
        IF g_object_ref IS NOT INITIAL.
          <ls_fcat>-fix_column = 'X'.
          <ls_fcat>-col_pos = 1.
          CONTINUE.
        ENDIF.
      WHEN  'AFONR'.
        IF g_object_ref IS NOT INITIAL.
          <ls_fcat>-fix_column = 'X'.
          <ls_fcat>-col_pos = 1.
          CONTINUE.
        ENDIF.
    ENDCASE.

    IF gs_bustyp-busref = 'Z' OR gs_bustyp-busref = 'Y'.
      READ TABLE lt_screen INTO gt_screen WITH KEY fieldname = <ls_fcat>-fieldname .
    ELSE.
      READ TABLE lt_screen INTO gt_screen WITH KEY fieldname = <ls_fcat>-fieldname fieldalv = 'ITEM' .
    ENDIF.

    IF sy-subrc EQ 0.

      <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
      <ls_fcat>-scrtext_l = <ls_fcat>-reptext = gt_screen-coltext.

      <ls_fcat>-col_pos = gt_screen-dzaehk.

      IF gt_screen-fedit = 'X'.
        <ls_fcat>-edit = 'X'.
        <ls_fcat>-hotspot = ''.
*        <ls_fcat>-auto_value = 'X'.
      ELSEIF gt_screen-fedit = 'C'.
        IF g_action = 'CRE'.
          <ls_fcat>-edit = 'X'.
*          <ls_fcat>-auto_value = 'X'.
        ELSE.
          <ls_fcat>-edit = ''.
        ENDIF.
      ENDIF.

      IF gs_bustyp-busref = 'Z' OR gs_bustyp-busref = 'Y'.
        <ls_fcat>-edit = ''.
      ENDIF.
*      IF g_object_ref IS INITIAL.
*        IF gt_screen-fieldname = 'MENGE' .
*          <ls_fcat>-edit = 'X'.
*          <ls_fcat>-auto_value = 'X'.
*        ELSE.
*          <ls_fcat>-edit = ''.
*        ENDIF.
*      ELSE.
*        IF gt_screen-FIELDalv = 'HEAD'.
*          <ls_fcat>-emphasize = 'C310'.
*        ENDIF.
*      ENDIF.

      <ls_fcat>-emphasize = gt_screen-emphasize.

      IF gt_screen-hidde = 'C' OR gt_screen-hidde = 'X'.
        <ls_fcat>-tech = 'X'.
      ELSE.
        <ls_fcat>-tech = ''.
      ENDIF.

    ELSE.
      <ls_fcat>-tech = 'X'.
      <ls_fcat>-col_pos = 9999.
    ENDIF.
    <ls_fcat>-no_out = <ls_fcat>-tech.
    IF g_action = 'DIS'.
      <ls_fcat>-edit = ''.
    ENDIF.
  ENDLOOP.

  gt_fcat_100 = lt_fcat.

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


FORM f_create_grid_toolbar_100 CHANGING  c_t_toolbar TYPE ui_functions.

  CLEAR: c_t_toolbar[].

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


FORM f_assign_grid_handlers_100 CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid_100.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_grid_100->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_100->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_grid_100->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_100->handle_double_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_100->data_changed_finished
          FOR c_grid.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.


FORM f_register_grid_event_100 USING u_grid TYPE REF TO cl_gui_alv_grid.

* ENTER EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
** MODIFY EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.


FORM f_register_grid_event2 USING u_grid TYPE REF TO cl_gui_alv_grid.

* ENTER EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
** MODIFY EVENT
*  CALL METHOD u_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.


FORM f_display_grid_alv_100 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.

  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'G_GRID_100' CHANGING ls_variant-handle.

  CALL METHOD g_grid_100->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_100
      it_toolbar_excluding = gt_exclude_100[]
      i_default            = 'X'
    CHANGING
*     it_outtab            = gt_item[]
      it_outtab            = gt_item_100[]
      it_sort              = gt_sort_100[]
      it_fieldcatalog      = gt_fcat_100[].
*
*  LS_VARIANT-HANDLE = 2.
*
*  CALL METHOD G_GRID2->SET_TABLE_FOR_FIRST_DISPLAY
*    EXPORTING
*      IS_VARIANT           = LS_VARIANT
*      I_SAVE               = 'A'
*      IS_LAYOUT            = GS_LAYOUT
*      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE[]
*      I_DEFAULT            = 'X'
*    CHANGING
*      IT_OUTTAB            = GT_ITEM_DIS[]
*      IT_SORT              = GT_SORT[]
*      IT_FIELDCATALOG      = GT_FCAT2[].

ENDFORM.


FORM frm_set_alv_handle.

ENDFORM.


MODULE user_command_0100.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
  DATA:lt_filt TYPE         lvc_t_fidx,
       ls_filt TYPE LINE OF lvc_t_fidx,
       BEGIN OF ls_index,
         index TYPE sy-tabix,
       END OF ls_index,
       lt_index LIKE STANDARD TABLE OF ls_index.
  CASE sy-ucomm.
    WHEN '&REFRESH'.
      CLEAR gt_item_100[].
      CLEAR gt_item_po_100[].
      PERFORM frm_get_ref
         IN PROGRAM zafo IF FOUND
         TABLES gt_item_100 gt_item_po_100
         USING gs_bustyp-busref.

      PERFORM f_refresh_hold_grid_alv USING g_grid_100.


    WHEN '&SELECTALL'.

      CALL METHOD g_grid_100->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      IF lt_index_rows[] IS INITIAL.


        CALL METHOD g_grid_100->get_filtered_entries
          IMPORTING
            et_filtered_entries = lt_filt.
        CLEAR lt_index[].
        LOOP AT lt_filt INTO ls_filt.
          CLEAR ls_index.
          ls_index-index = ls_filt.
          APPEND ls_index TO lt_index.
        ENDLOOP.

        LOOP AT gt_item_100 ASSIGNING <gs_item>.
          READ TABLE lt_index TRANSPORTING NO FIELDS
                                 WITH KEY index = sy-tabix.
          IF sy-subrc NE 0.
            <gs_item>-selected = 'X'.
          ELSE.
            <gs_item>-selected = ''.
          ENDIF.
        ENDLOOP.
      ELSE.


        LOOP AT lt_index_rows INTO ls_index_rows.
          READ TABLE gt_item_100 ASSIGNING <gs_item> INDEX ls_index_rows-index.
          IF sy-subrc EQ 0.
            <gs_item>-selected = 'X'.
          ENDIF.
        ENDLOOP.
      ENDIF.

*      LOOP AT gt_item ASSIGNING <gs_item>.
*        <gs_item>-selected = 'X'.
*      ENDLOOP.
    WHEN '&SELECTCAN'.
      LOOP AT gt_item_100 ASSIGNING <gs_item>.
        <gs_item>-selected = ''.
      ENDLOOP.
  ENDCASE.

ENDMODULE.
