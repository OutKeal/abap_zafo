*&---------------------------------------------------------------------*
*& 包含               ZAFO_PD_ALV_0300
*&---------------------------------------------------------------------*

  MODULE status_0300 OUTPUT.
    SET PF-STATUS 'S0300'.
    SET TITLEBAR 'T0300'    .

  ENDMODULE.

*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

  CLASS:
    lcl_event_receiver_grid_300 DEFINITION DEFERRED.

  DATA:
    g_event_receiver_grid_300   TYPE REF TO lcl_event_receiver_grid_300.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
  CLASS lcl_event_receiver_grid_300 DEFINITION.

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





  ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
  CLASS lcl_event_receiver_grid_300 IMPLEMENTATION.
* DATA CHANGED
    METHOD handle_data_changed.
      PERFORM f_handle_data_changed_300
        USING er_data_changed
              e_onf4.


    ENDMETHOD.                    "HANDLE_DATA_CHANGED

    METHOD handle_double_click.
      PERFORM f_handle_double_click_300 USING e_row e_column.
    ENDMETHOD.

    METHOD handle_hotspot_click.
      PERFORM f_handle_hotspot_click_300 USING e_row_id e_column_id .
    ENDMETHOD.

    METHOD handle_toolbar.
      PERFORM f_toolbar_300 USING e_object->mt_toolbar.
    ENDMETHOD.

    METHOD data_changed_finished.
      PERFORM f_data_changed_finished_300 USING e_modified et_good_cells.
    ENDMETHOD.

    METHOD handle_user_command.
      PERFORM f_user_command_300 USING e_ucomm.
    ENDMETHOD.

  ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

  DATA: g_grid_300              TYPE REF TO cl_gui_alv_grid,
        gt_fcat_300             TYPE lvc_t_fcat,
        gs_layout_300           TYPE lvc_s_layo,
        gt_sort_300             TYPE lvc_t_sort,
        gt_exclude_300          TYPE ui_functions,
        g_docking_container_300 TYPE REF TO cl_gui_docking_container,
        g_cumtom_container_300  TYPE REF TO cl_gui_custom_container,
        g_container_300         TYPE REF TO cl_gui_container,
        g_splitter_300          TYPE REF TO cl_gui_splitter_container,
        g_toolbar_300           TYPE REF TO cl_gui_toolbar.





  FORM f_handle_data_changed_300
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

  FORM f_handle_double_click_300 USING e_row_id TYPE lvc_s_row
                                     e_column_id TYPE lvc_s_col.

*  CLEAR GT_PD_ITEM_DIS[].
*  PERFORM GET_DIS_DATA USING E_ROW_ID-INDEX.
*
*  PERFORM F_REFRESH_GRID_ALV USING G_GRID2.

  ENDFORM.


  FORM f_handle_hotspot_click_300 USING e_row_id TYPE lvc_s_row
                                     e_column_id TYPE lvc_s_col.


    CASE e_column_id-fieldname .
      WHEN 'AFOPD'.
        READ TABLE gt_pd_head  INDEX e_row_id-index .

        IF sy-subrc EQ 0.
          SELECT * FROM zafo_pd_item
            WHERE afopd = @gt_pd_head-afopd
            INTO TABLE @gt_pd_item.
          g_change = 'D'.
          PERFORM init_head_text.
          CALL SCREEN 200.

        ENDIF.
      WHEN 'AFONO'.
        READ TABLE gt_pd_head  INDEX e_row_id-index .
        IF sy-subrc EQ 0.
          CHECK gt_pd_head-afono IS NOT INITIAL.
          SET PARAMETER ID 'ZAFONO' FIELD   gt_pd_head-afono.

          CALL TRANSACTION 'ZAFO_PD1' AND SKIP FIRST SCREEN.

        ENDIF.
    ENDCASE.




  ENDFORM.


  FORM f_handle_user_command_300 USING ok_code.
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
*      CLEAR:GT_PD_ITEM_DIS.
*
*      CALL METHOD G_GRID_300->GET_SELECTED_ROWS
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

  FORM f_toolbar_300 USING ut_toolbar TYPE ttb_button.
*    DATA: LS_TOOLBAR TYPE STB_BUTTON.
*
*    CLEAR LS_TOOLBAR.
*    MOVE '&SURE' TO LS_TOOLBAR-FUNCTION.
*    MOVE ICON_CHECKED TO LS_TOOLBAR-ICON.
*    MOVE '确认选择' TO LS_TOOLBAR-QUICKINFO.
*    MOVE ' ' TO LS_TOOLBAR-DISABLED.
*    MOVE '确认选择' TO LS_TOOLBAR-TEXT.
*    APPEND LS_TOOLBAR TO UT_TOOLBAR.
*    CLEAR LS_TOOLBAR.




  ENDFORM.


  FORM f_data_changed_finished_300  USING  e_modified
                                     et_good_cells TYPE lvc_t_modi.

*
    DATA:ls_refresh TYPE char1.
    CLEAR ls_refresh.
    CHECK NOT et_good_cells IS INITIAL.
*
    LOOP AT et_good_cells INTO DATA(ls_cell).


      READ TABLE gt_pd_item ASSIGNING <gs_pd_item>
                              INDEX ls_cell-row_id.

      CHECK sy-subrc = 0.

      IF ls_cell-fieldname CP 'MENGE_SP' .
        <gs_pd_item>-menge_cy = <gs_pd_item>-menge_sp - <gs_pd_item>-menge.
        ls_refresh = 'X'.
      ENDIF.
    ENDLOOP.



    IF ls_refresh = 'X'.
      PERFORM f_refresh_grid_alv USING g_grid_300.
    ENDIF.

  ENDFORM.
  FORM f_user_command_300 USING ok_code.
    DATA:lt_index_rows TYPE  lvc_t_row,
         ls_index_rows TYPE  lvc_s_row,
         lt_row_no     TYPE  lvc_t_roid.

    DATA:lt_pd_item TYPE TABLE OF zafo_pd_item WITH HEADER LINE.
    CASE ok_code.

      WHEN '&SURE'.
        gt_pd_head-werks = p_werks.
        gt_pd_head-lgort = p_lgort.
        gt_pd_head-ernam = sy-uname.

        CALL METHOD g_grid_300->get_selected_rows
          IMPORTING
            et_index_rows = lt_index_rows
            et_row_no     = lt_row_no.
        IF lt_index_rows[] IS INITIAL.
          MESSAGE '请至少选择一行库存数据' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        lt_pd_item[] = gt_pd_item[].
        CLEAR lt_pd_item[].

        LOOP AT lt_index_rows INTO ls_index_rows.
          READ TABLE gt_pd_item INDEX ls_index_rows-index .
          IF sy-subrc EQ 0.
            APPEND gt_pd_item TO lt_pd_item.
          ENDIF.
        ENDLOOP.

        CLEAR gt_pd_item[].
        gt_pd_item[] = lt_pd_item[].

        PERFORM init_head_text.

        g_change = 'M'.

        CALL SCREEN 200.

    ENDCASE.

  ENDFORM.



  MODULE create_object_0300 OUTPUT.

    IF g_grid_300 IS INITIAL.
**-- CREATE CONTAINER
      PERFORM f_create_container_300.
**-- FIELD_CATALOG DEFINE
      PERFORM f_set_grid_field_catalog_300.
*    PERFORM F_SET_GRID_FIELD_CATALOG2.
**-- LAYOUT
      PERFORM f_create_grid_layout_300.
**-- TOOLBAR EXCLUDE
      PERFORM f_create_grid_toolbar_300  CHANGING gt_exclude_300[].
**-- GRID EVENT HANDLER DEFINE
      PERFORM f_assign_grid_handlers_300 CHANGING g_grid_300.
*    PERFORM F_ASSIGN_GRID_EVENT_HANDLERS CHANGING G_GRID2.
**-- REGISTER EVENT
      PERFORM f_register_grid_event_300 USING g_grid_300.
*    PERFORM F_REGISTER_GRID_EVENT2 USING G_GRID2.
**--
      CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
      PERFORM f_display_grid_alv_300.
*--
      CALL METHOD g_grid_300->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
    ELSE.
**--
      PERFORM f_refresh_grid_alv USING g_grid_300.

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
          rows    = 1
          columns = 1.

      g_container_300  = g_splitter_300->get_container( row = 1 column = 1 ).
*    G_CONTAINER_2  = G_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

    ENDIF.

    CREATE OBJECT g_grid_300
      EXPORTING
        i_parent = g_container_300.

*  CREATE OBJECT G_GRID2
*    EXPORTING
*      I_PARENT = G_CONTAINER_2.

  ENDFORM.


  FORM f_set_grid_field_catalog_300 .

    REFRESH: gt_fcat_300.

    FIELD-SYMBOLS:
      <ls_fcat> TYPE lvc_s_fcat.
    DATA:
      lt_fcat TYPE lvc_t_fcat.

    DATA:
      lt_fieldcat TYPE slis_t_fieldcat_alv,
      ls_fieldcat TYPE slis_fieldcat_alv.

    DATA: l_struc_name LIKE  dd02l-tabname .

    DATA:lt_screen TYPE TABLE OF zafo_screen .

    l_struc_name = 'ZAFO_PD_HEAD'.

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

        WHEN 'AFOPD' OR 'AFONO'.
          <ls_fcat>-hotspot = 'X'.

      ENDCASE.


      <ls_fcat>-no_out = <ls_fcat>-tech.
    ENDLOOP.


    gt_fcat_300 = lt_fcat.

  ENDFORM.





  FORM f_create_grid_layout_300 .

    CLEAR: gs_layout_300.
    gs_layout_300-sel_mode   = 'A'.
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

  FORM f_create_grid_toolbar_300
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

  FORM f_assign_grid_handlers_300
    CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

    CREATE OBJECT g_event_receiver_grid_300.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

    SET HANDLER g_event_receiver_grid_300->handle_toolbar
            FOR c_grid .
    SET HANDLER g_event_receiver_grid_300->handle_user_command
            FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
    SET HANDLER g_event_receiver_grid_300->handle_hotspot_click
            FOR c_grid .
    SET HANDLER g_event_receiver_grid_300->handle_double_click
            FOR c_grid .
    SET HANDLER g_event_receiver_grid_300->data_changed_finished
            FOR c_grid.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

  ENDFORM.

  FORM f_register_grid_event_300
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




  FORM f_display_grid_alv_300 .

    DATA: ls_variant LIKE disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = 1.

    CALL METHOD g_grid_300->set_table_for_first_display
      EXPORTING
        is_variant           = ls_variant
        i_save               = ''
        is_layout            = gs_layout_300
        it_toolbar_excluding = gt_exclude_300[]
        i_default            = 'X'
      CHANGING
        it_outtab            = gt_pd_head[]
        it_sort              = gt_sort_300[]
        it_fieldcatalog      = gt_fcat_300[].
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
*      IT_OUTTAB            = GT_PD_ITEM_DIS[]
*      IT_SORT              = GT_SORT[]
*      IT_FIELDCATALOG      = GT_FCAT2[].

  ENDFORM.

  MODULE user_command_0300.
    CASE sy-ucomm.
      WHEN '&SELECTALL'.
        LOOP AT gt_pd_item ASSIGNING <gs_pd_item>.
*          <GS_PD_ITEM>-SELECTED = 'X'.
        ENDLOOP.
      WHEN '&SELECTCAN'.
        LOOP AT gt_pd_item ASSIGNING <gs_pd_item>.
*          <GS_PD_ITEM>-SELECTED = ''.
        ENDLOOP.
    ENDCASE.

  ENDMODULE.