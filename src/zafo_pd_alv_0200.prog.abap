*&---------------------------------------------------------------------*
*& 包含               ZAFO_PD_ALV_0200
*&---------------------------------------------------------------------*

  MODULE status_0200 OUTPUT.
    DATA fcode TYPE TABLE OF sy-ucomm.
    CLEAR fcode.
    CASE gt_pd_head-pdstatus.
      WHEN ''.
        APPEND '&EDIT' TO fcode.
        APPEND '&COMMIT' TO fcode.
        APPEND '&GOTOAPP' TO fcode.
        APPEND '&POST' TO fcode.
        APPEND '&DELETE' TO fcode.
        APPEND '&PRINT' TO fcode.
      WHEN 'A'."已保存
        IF g_change = 'D'.
          APPEND '&SAVE' TO fcode.
          APPEND '&GOTOAPP' TO fcode.
        ELSE.
          APPEND '&EDIT' TO fcode.

          APPEND '&DELETE' TO fcode.
        ENDIF.
        APPEND '&POST' TO fcode.
      WHEN 'B'."已提交
        APPEND '&SAVE' TO fcode.
        APPEND '&EDIT' TO fcode.
        APPEND '&COMMIT' TO fcode.
        APPEND '&DELETE' TO fcode.
        APPEND '&POST' TO fcode.
      WHEN 'C'."已确认差异
        APPEND '&SAVE' TO fcode.
        APPEND '&EDIT' TO fcode.
        APPEND '&COMMIT' TO fcode.
        APPEND '&DELETE' TO fcode.
      WHEN 'D'."已作废
        APPEND '&SAVE' TO fcode.
        APPEND '&EDIT' TO fcode.
        APPEND '&POST' TO fcode.
        APPEND '&DELETE' TO fcode.
        APPEND '&PRINT' TO fcode.
        APPEND '&COMMIT' TO fcode.
      WHEN 'S'."已完成
        APPEND '&SAVE' TO fcode.
        APPEND '&EDIT' TO fcode.
        APPEND '&POST' TO fcode.
        APPEND '&COMMIT' TO fcode.
        APPEND '&DELETE' TO fcode.
    ENDCASE.

    SET PF-STATUS 'S0200' EXCLUDING fcode.
    SET TITLEBAR 'T0200' WITH '工厂'  p_werks '库位' p_lgort '盘点单'   .

  ENDMODULE.

  FORM init_head_text.

    IF gt_pd_head-werks IS NOT INITIAL .
      SELECT SINGLE name1
        INTO txt_werks
        FROM t001w
        WHERE werks EQ gt_pd_head-werks.
    ENDIF.


    IF gt_pd_head-lgort IS NOT INITIAL .
      SELECT SINGLE lgobe
        INTO txt_lgort
        FROM t001l
        WHERE werks EQ gt_pd_head-werks
        AND lgort = gt_pd_head-lgort .

    ENDIF.

    IF gt_pd_head-ernam IS NOT INITIAL.
      SELECT SINGLE name_textc
        INTO txt_ernam
        FROM user_addr
        WHERE bname EQ gt_pd_head-ernam .
    ENDIF.



  ENDFORM.

*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

  CLASS:
    lcl_event_receiver_grid_200 DEFINITION DEFERRED.

  DATA:
    g_event_receiver_grid_200   TYPE REF TO lcl_event_receiver_grid_200.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
  CLASS lcl_event_receiver_grid_200 DEFINITION.

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
  CLASS lcl_event_receiver_grid_200 IMPLEMENTATION.
* DATA CHANGED
    METHOD handle_data_changed.
      PERFORM f_handle_data_changed_200
        USING er_data_changed
              e_onf4.


    ENDMETHOD.                    "HANDLE_DATA_CHANGED

    METHOD handle_double_click.
      PERFORM f_handle_double_click_200 USING e_row e_column.
    ENDMETHOD.

    METHOD handle_hotspot_click.
      PERFORM f_handle_hotspot_click_200 USING e_row_id e_column_id .
    ENDMETHOD.

    METHOD handle_toolbar.
      PERFORM f_toolbar_200 USING e_object->mt_toolbar.
    ENDMETHOD.

    METHOD data_changed_finished.
      PERFORM f_data_changed_finished_200 USING e_modified et_good_cells.
    ENDMETHOD.

    METHOD handle_user_command.
      PERFORM f_user_command_200 USING e_ucomm.
    ENDMETHOD.

  ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

  DATA: g_grid_200              TYPE REF TO cl_gui_alv_grid,
        gt_fcat_200             TYPE lvc_t_fcat,
        gs_layout_200           TYPE lvc_s_layo,
        gt_sort_200             TYPE lvc_t_sort,
        gt_exclude_200          TYPE ui_functions,
        g_docking_container_200 TYPE REF TO cl_gui_docking_container,
        g_cumtom_container_200  TYPE REF TO cl_gui_custom_container,
        g_container_200         TYPE REF TO cl_gui_container,
        g_splitter_200          TYPE REF TO cl_gui_splitter_container,
        g_toolbar_200           TYPE REF TO cl_gui_toolbar.





  FORM f_handle_data_changed_200
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

  FORM f_handle_double_click_200 USING e_row_id TYPE lvc_s_row
                                     e_column_id TYPE lvc_s_col.

*  CLEAR gt_pd_item_DIS[].
*  PERFORM GET_DIS_DATA USING E_ROW_ID-INDEX.
*
*  PERFORM F_REFRESH_GRID_ALV USING G_GRID2.

  ENDFORM.


  FORM f_handle_hotspot_click_200 USING e_row_id TYPE lvc_s_row
                                     e_column_id TYPE lvc_s_col.





*    PERFORM f_refresh_grid_alv USING g_grid_200.

  ENDFORM.


  FORM f_handle_user_command_200 USING ok_code.
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
*      CLEAR:gt_pd_item_DIS.
*
*      CALL METHOD G_GRID_200->GET_SELECTED_ROWS
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

  FORM f_toolbar_200 USING ut_toolbar TYPE ttb_button.
    DATA: ls_toolbar TYPE stb_button.

*    CLEAR ls_toolbar.
*    MOVE '&SURE' TO ls_toolbar-function.
*    MOVE icon_checked TO ls_toolbar-icon.
*    MOVE '确认选择' TO ls_toolbar-quickinfo.
*    MOVE ' ' TO ls_toolbar-disabled.
*    MOVE '确认选择' TO ls_toolbar-text.
*    APPEND ls_toolbar TO ut_toolbar.
*    CLEAR ls_toolbar.




  ENDFORM.


  FORM f_data_changed_finished_200  USING  e_modified
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
      PERFORM f_refresh_grid_alv USING g_grid_200.
    ENDIF.

  ENDFORM.
  FORM f_user_command_200 USING ok_code.

    CASE ok_code.
*
*      WHEN '&SURE'.
*
*        g_ok_code = '&SURE' .
*
*        CLEAR g_error.
*
*        PERFORM frm_clear_msg.
*
*        PERFORM frm_check_choise.
*
*        PERFORM frm_pop_msg.
*
*        IF g_error EQ 'X'.
*
*          RETURN.
*
*        ELSE.
*
*          PERFORM frm_sure_item .
*
*          LEAVE TO SCREEN 0.
*
*        ENDIF.
*
*      WHEN '&MASS_MODIFY'.
*
*        PERFORM frm_mass_modify TABLES gt_fcat_200.
*
*        PERFORM f_refresh_grid_alv USING g_grid_200.
*
*      WHEN ''.

    ENDCASE.

  ENDFORM.



  MODULE create_object_0200 OUTPUT.

    IF g_grid_200 IS INITIAL.
**-- CREATE CONTAINER
      PERFORM f_create_container_200.
**-- FIELD_CATALOG DEFINE
      PERFORM f_set_grid_field_catalog_200.
*    PERFORM F_SET_GRID_FIELD_CATALOG2.
**-- LAYOUT
      PERFORM f_create_grid_layout_200.
**-- TOOLBAR EXCLUDE
      PERFORM f_create_grid_toolbar_200  CHANGING gt_exclude_200[].
**-- GRID EVENT HANDLER DEFINE
      PERFORM f_assign_grid_handlers_200 CHANGING g_grid_200.
*    PERFORM F_ASSIGN_GRID_EVENT_HANDLERS CHANGING G_GRID2.
**-- REGISTER EVENT
      PERFORM f_register_grid_event_200 USING g_grid_200.
*    PERFORM F_REGISTER_GRID_EVENT2 USING G_GRID2.
**--
      CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
      PERFORM f_display_grid_alv_200.
*--
      CALL METHOD g_grid_200->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
    ELSE.
**--
      PERFORM f_refresh_grid_alv USING g_grid_200.

    ENDIF.

  ENDMODULE.

  FORM f_create_container_200 .

*  IF g_docking_container_200 IS INITIAL.
*
*    CREATE OBJECT g_docking_container_200
*      EXPORTING
*        style     = cl_gui_control=>ws_child
*        repid     = sy-repid
*        dynnr     = sy-dynnr
*        side      = g_docking_container_200->dock_at_bottom
*        lifetime  = cl_gui_control=>lifetime_imode
*        extension = '260'
*      EXCEPTIONS
*        OTHERS    = 1.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid
*            TYPE sy-msgty
*          NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDIF.
    CREATE OBJECT g_cumtom_container_200
      EXPORTING
        container_name = 'ITEM'.


* SPLITTER CONTAINER
    IF g_splitter_200 IS INITIAL.
      CREATE OBJECT g_splitter_200
        EXPORTING
          parent  = g_cumtom_container_200
          rows    = 1
          columns = 1.

      g_container_200  = g_splitter_200->get_container( row = 1 column = 1 ).
*    g_container_2  = g_splitter->get_container( row = 1 column = 1 ).

    ENDIF.

    CREATE OBJECT g_grid_200
      EXPORTING
        i_parent = g_container_200.


*  CREATE OBJECT g_grid2
*    EXPORTING
*      i_parent = g_container_2.

  ENDFORM.


  FORM f_set_grid_field_catalog_200 .

    REFRESH: gt_fcat_200.

    FIELD-SYMBOLS:
      <ls_fcat> TYPE lvc_s_fcat.
    DATA:
      lt_fcat TYPE lvc_t_fcat.

    DATA:
      lt_fieldcat TYPE slis_t_fieldcat_alv,
      ls_fieldcat TYPE slis_fieldcat_alv.

    DATA: l_struc_name LIKE  dd02l-tabname .

    DATA:lt_screen TYPE TABLE OF zafo_screen .

    l_struc_name = 'ZAFO_PD_ITEM'.

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


        WHEN 'AFOPD' OR 'AFONR' ."OR 'WERKS' OR 'LGORT'.
          <ls_fcat>-tech = 'X'.

        WHEN 'MENGE_SP' OR 'REMARK' OR 'MENGE_YC' OR 'ZSHELVES_UM'.
          <ls_fcat>-edit = 'X'.
      ENDCASE.

      IF g_change = 'D'.
        <ls_fcat>-edit = ''.
      ENDIF.


      <ls_fcat>-no_out = <ls_fcat>-tech.
    ENDLOOP.


    gt_fcat_200 = lt_fcat.

  ENDFORM.





  FORM f_create_grid_layout_200 .

    CLEAR: gs_layout_200.
    gs_layout_200-sel_mode   = 'A'.
    gs_layout_200-cwidth_opt = 'X'.
    gs_layout_200-zebra      = 'X'.
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

  FORM f_create_grid_toolbar_200
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

  FORM f_assign_grid_handlers_200
    CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

    CREATE OBJECT g_event_receiver_grid_200.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

    SET HANDLER g_event_receiver_grid_200->handle_toolbar
            FOR c_grid .
    SET HANDLER g_event_receiver_grid_200->handle_user_command
            FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
    SET HANDLER g_event_receiver_grid_200->handle_hotspot_click
            FOR c_grid .
    SET HANDLER g_event_receiver_grid_200->handle_double_click
            FOR c_grid .
    SET HANDLER g_event_receiver_grid_200->data_changed_finished
            FOR c_grid.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

  ENDFORM.

  FORM f_register_grid_event_200
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





  FORM f_display_grid_alv_200 .

    DATA: ls_variant LIKE disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = 1.

    CALL METHOD g_grid_200->set_table_for_first_display
      EXPORTING
        is_variant           = ls_variant
        i_save               = ''
        is_layout            = gs_layout_200
        it_toolbar_excluding = gt_exclude_200[]
        i_default            = 'X'
      CHANGING
        it_outtab            = gt_pd_item[]
        it_sort              = gt_sort_200[]
        it_fieldcatalog      = gt_fcat_200[].
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
*      IT_OUTTAB            = gt_pd_item_DIS[]
*      IT_SORT              = GT_SORT[]
*      IT_FIELDCATALOG      = GT_FCAT2[].

  ENDFORM.

  MODULE set_screen_0200 OUTPUT.
    LOOP AT SCREEN.
      IF g_change = 'D'.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMODULE.


  MODULE user_command_0200 INPUT.
    CLEAR g_error.

    CASE sy-ucomm.
      WHEN '&EDIT'.
        CLEAR sy-ucomm.

        AUTHORITY-CHECK OBJECT 'ZAFO_PD'
               ID 'WERKS' FIELD p_werks
               ID 'ACTVT' FIELD '02'.
        IF sy-subrc NE 0.
          MESSAGE '无权限' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF g_change = 'D'.
          g_change = 'M'.
        ELSEIF g_change = 'M'.
          g_change = 'D'.
        ENDIF.

        PERFORM f_set_catalog_alv_200 .

      WHEN '&SAVE'.

        CLEAR sy-ucomm.

        AUTHORITY-CHECK OBJECT 'ZAFO_PD'
       ID 'WERKS' FIELD p_werks
       ID 'ACTVT' FIELD '01'.
        IF sy-subrc NE 0.
          MESSAGE '无权限' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        PERFORM frm_save_data.

        g_change = 'D'.
        PERFORM f_set_catalog_alv_200 .

      WHEN '&COMMIT'.
        CLEAR sy-ucomm.

        CLEAR g_error.

        AUTHORITY-CHECK OBJECT 'ZAFO_PD'
         ID 'WERKS' FIELD p_werks
         ID 'ACTVT' FIELD '06'.
        IF sy-subrc NE 0.
          MESSAGE '无权限' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CHECK g_error IS INITIAL.

        PERFORM frm_clear_msg.
        PERFORM frm_pop_confirm USING '是否确认提交审批?'.

        CHECK g_error <> 'X'.

        IF gt_pd_head-pdstatus = 'A'.
          PERFORM frm_commit_appr.
        ENDIF.

        g_change = 'D'.

      WHEN '&GOTOAPP'.
        DATA:ls_key1 TYPE char20.

        CLEAR sy-ucomm.
        CLEAR g_error.

        PERFORM frm_clear_msg.

        ls_key1 = gt_pd_head-afopd.

        CALL FUNCTION 'ZAPP_FLOW_DISPLAY'
          EXPORTING
            object = 'PDSP'
            key1   = ls_key1
*           user1  = gs_head-afnam
*           user2  = gs_head-ernam
          EXCEPTIONS
            nodata = 1
            OTHERS = 2.


        SELECT SINGLE pdstatus FROM zafo_pd_head
          INTO  @gt_pd_head-pdstatus
          WHERE afopd = @gt_pd_head-afopd.

      WHEN '&POST'.

        CLEAR sy-ucomm.

        AUTHORITY-CHECK OBJECT 'ZAFO_PD'
         ID 'WERKS' FIELD p_werks
         ID 'ACTVT' FIELD '06'.
        IF sy-subrc NE 0.
          MESSAGE '无权限' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        PERFORM frm_pop_confirm USING '是否确认过账差异?'.

        CHECK g_error <> 'X'.

        PERFORM frm_post_data.


      WHEN '&PRINT'.


        CLEAR sy-ucomm.

        AUTHORITY-CHECK OBJECT 'ZAFO_PD'
         ID 'WERKS' FIELD p_werks
         ID 'ACTVT' FIELD '04'.
        IF sy-subrc NE 0.
          MESSAGE '无权限' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        DATA:lt_pd_head TYPE TABLE OF zafo_pd_head WITH HEADER LINE.
        APPEND gt_pd_head TO lt_pd_head.

        CALL FUNCTION 'ZAFO_PRINT_PD'
          TABLES
            it_head = lt_pd_head[]
            it_item = gt_pd_item[].



      WHEN '&DELETE'.

        CLEAR sy-ucomm.

        AUTHORITY-CHECK OBJECT 'ZAFO_PD'
         ID 'WERKS' FIELD p_werks
         ID 'ACTVT' FIELD '02'.
        IF sy-subrc NE 0.
          MESSAGE '无权限' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        PERFORM frm_pop_confirm USING '是否确认删除单据?'.

        CHECK g_error <> 'X'.

        g_change = 'D'.

        gt_pd_head-pdstatus = 'D'.
        UPDATE zafo_pd_head SET pdstatus = 'D'
          WHERE afopd = gt_pd_head-afopd.
        COMMIT WORK AND WAIT.

    ENDCASE.

  ENDMODULE.


  FORM f_set_catalog_alv_200.

    PERFORM f_set_grid_field_catalog_200.

    CALL METHOD g_grid_200->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat_200.

    CALL METHOD g_grid_200->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_200.


  ENDFORM.
