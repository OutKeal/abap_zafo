*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:lcl_event_receiver_grid_900 DEFINITION DEFERRED.

DATA:g_event_receiver_grid_900 TYPE REF TO lcl_event_receiver_grid_900.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_900 DEFINITION.

  PUBLIC SECTION.

    METHODS handle_onf4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells.

    METHODS handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed  e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click  OF cl_gui_alv_grid
      IMPORTING e_row e_column.

    METHODS handle_hotspot_click
      FOR EVENT hotspot_click  OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no.

    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_grid_900 IMPLEMENTATION.

  METHOD handle_onf4 .
    PERFORM f_handle_onf4_900 USING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells.
  ENDMETHOD.

  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_900 USING er_data_changed e_onf4.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM f_handle_double_click_900 USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_click_900 USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_900 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_900 USING e_ucomm.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_finished_900 USING e_modified et_good_cells.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION


DATA: g_grid_900              TYPE REF TO cl_gui_alv_grid,
      gt_fcat_900             TYPE lvc_t_fcat,
      gs_layout_900           TYPE lvc_s_layo,
      gt_sort_900             TYPE lvc_t_sort,
      gt_exclude_900          TYPE ui_functions,
      g_docking_container_900 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_900  TYPE REF TO cl_gui_custom_container,
      g_container_900         TYPE REF TO cl_gui_container,
      g_splitter_900          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_900           TYPE REF TO cl_gui_toolbar.



FORM f_handle_data_changed_900 USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
                                      u_onf4    TYPE any.
  DATA: ls_modi LIKE lvc_s_modi.

  SORT u_changed->mt_good_cells BY row_id.

  LOOP AT u_changed->mt_mod_cells INTO ls_modi.
    CASE ls_modi-fieldname.
      WHEN 'MATNR'.

    ENDCASE.

  ENDLOOP.
  PERFORM f_refresh_grid_alv USING g_grid_900.
ENDFORM.


FORM f_handle_onf4_900 USING e_fieldname TYPE  lvc_fname
                              e_fieldvalue  TYPE  lvc_value
                              es_row_no TYPE  lvc_s_roid
                              er_event_data	TYPE REF TO	cl_alv_event_data
                              et_bad_cells  TYPE  lvc_t_modi.
  DATA: ls_modi    TYPE lvc_s_modi,
        lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.
  FIELD-SYMBOLS <modtab> TYPE lvc_t_modi.


  IF e_fieldname = 'MATNR' OR e_fieldname = 'MAKTX'.
    DATA:ls_maktx TYPE maktx.

    READ TABLE gt_item INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    ls_maktx = e_fieldvalue.
    PERFORM f_matnr_f4 TABLES lt_ret_tab  USING ls_maktx.

    IF lt_ret_tab[] IS NOT INITIAL.
      READ TABLE lt_ret_tab INDEX 1.
      IF sy-subrc = 0.
        ls_modi-row_id = es_row_no-row_id.
        ls_modi-fieldname = e_fieldname.
        ls_modi-value = lt_ret_tab-fieldval.
        ASSIGN er_event_data->m_data->* TO <modtab>.
        APPEND ls_modi TO <modtab>.
      ENDIF.
    ENDIF.
    er_event_data->m_event_handled = 'X'."通知系统搜索事件处理完毕，这样就不会调用系统标准的Search Help。
  ENDIF.

ENDFORM.


FORM f_data_changed_finished_900  USING e_modified
                                        et_good_cells TYPE lvc_t_modi.
  DATA:ls_refresh TYPE char1.
  CLEAR ls_refresh.
  CHECK NOT et_good_cells IS INITIAL.

  LOOP AT et_good_cells INTO DATA(ls_cell).

    READ TABLE gt_item ASSIGNING <gs_item>  INDEX ls_cell-row_id.
    CHECK sy-subrc = 0.
    IF <gs_item>-icon <> icon_led_yellow.
      <gs_item>-icon = icon_led_yellow.
      <gs_item>-text = '已维护'.
      ls_refresh = 'X'.
    ENDIF.

    CASE ls_cell-fieldname.

      WHEN 'PRICE_LONG'.
        PERFORM frm_set_price  CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'PRICE'.
        PERFORM f_set_amount CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'AMOUNT'.
        PERFORM frm_set_amount1 CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'MENGE'.
        PERFORM f_set_menge_cg CHANGING <gs_item>.
        PERFORM frm_set_amount1 CHANGING <gs_item>.

        ls_refresh = 'X'.

      WHEN 'MENGE_CG'.
        PERFORM f_set_menge CHANGING <gs_item>.
        PERFORM frm_set_amount1 CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'MENGE4'.
        PERFORM f_set_data_cjd CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'MENGE4_BJ'.
        PERFORM f_set_data_cjd_cg CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'MEINS'.
        IF gs_head-knttp = 'K'." 费用类型
          <gs_item>-bprme = <gs_item>-meins.
          ls_refresh = 'X'.
        ENDIF.

      WHEN 'BPRME'.
        IF gs_head-knttp = 'K'." 费用类型
          <gs_item>-meins = <gs_item>-bprme.
          ls_refresh = 'X'.
        ENDIF.

      WHEN 'MATNR'.
        PERFORM f_set_maktx CHANGING <gs_item>.
        IF gs_head-bustyp = 'DUC01'.
          PERFORM f_set_ebeln_duc01 CHANGING <gs_item>.
        ENDIF.
        ls_refresh = 'X'.

      WHEN 'MATKL'.
        PERFORM f_set_matkl CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'IDNLF'.
        PERFORM f_set_idnlf CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'KUNNR'.
        PERFORM f_set_kunnr CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'ANLN1'.
        PERFORM f_set_anln1 CHANGING <gs_item>.
        ls_refresh = 'X'.
    ENDCASE.

  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_900.
  ENDIF.

ENDFORM.


FORM f_handle_double_click_900 USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
  CASE e_column_id-fieldname.

    WHEN 'ZZBOM_ITEM'.
      DATA:lt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

      READ TABLE gt_item ASSIGNING <gs_item> INDEX e_row_id-index.
      CHECK sy-subrc EQ 0.
      CHECK <gs_item>-zppdhd IS NOT INITIAL.

      PERFORM f_bom_f4 TABLES lt_return_tab USING <gs_item>-zppdhd ''.

      CHECK lt_return_tab[] IS NOT INITIAL.
      LOOP AT lt_return_tab.
        <gs_item>-zzbom_item = lt_return_tab-fieldval.
      ENDLOOP.

      IF <gs_item>-zzbom_item IS NOT INITIAL.
        PERFORM f_set_bom CHANGING <gs_item>.
      ENDIF.

      PERFORM f_refresh_grid_alv USING g_grid_900.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.


FORM f_handle_hotspot_click_900 USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
*  CHECK e_column_id-fieldname = 'ZOFNO'.
*  READ TABLE gt_head INTO gs_head INDEX e_row_id-index .

ENDFORM.


FORM f_handle_user_command_900 USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
*  CASE ok_code.
*    WHEN '&NEW'.
*      SUBMIT zmmr0050 WITH modify = ''
*                WITH create = 'X'
*                VIA SELECTION-SCREEN
*                AND RETURN.
*    WHEN '&DETAIL'.
*      CLEAR:gt_item_dis.
*
*      CALL METHOD g_grid_900->get_selected_rows
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
*  ENDCASE.
ENDFORM.


FORM f_toolbar_900 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  READ TABLE gt_item INDEX 1.
  IF sy-subrc EQ 0 AND gt_item-afono_ref <> ''.
    RETURN."有前序依赖的不能操作
  ENDIF.

  CLEAR ls_toolbar.

  MOVE '&DEL' TO ls_toolbar-function.
  MOVE icon_delete_row TO ls_toolbar-icon.
  MOVE TEXT-021 TO ls_toolbar-quickinfo."删除行
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-021 TO ls_toolbar-text."删除行
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  IF gs_bustyp-busref IS INITIAL.
    MOVE '&ADD' TO ls_toolbar-function.
    MOVE icon_insert_row TO ls_toolbar-icon.
    MOVE TEXT-020 TO ls_toolbar-quickinfo."插入行
    MOVE ' ' TO ls_toolbar-disabled.
    MOVE TEXT-020 TO ls_toolbar-text."插入行
    APPEND ls_toolbar TO ut_toolbar.
    CLEAR ls_toolbar.

    MOVE '&COPY' TO ls_toolbar-function.
    MOVE icon_add_row TO ls_toolbar-icon.
    MOVE TEXT-033 TO ls_toolbar-quickinfo."复制行
    MOVE ' ' TO ls_toolbar-disabled.
    MOVE TEXT-033 TO ls_toolbar-text."复制行
    APPEND ls_toolbar TO ut_toolbar.
    CLEAR ls_toolbar.

    IF gs_bustyp-bustyp EQ 'PR004' OR gs_bustyp-bustyp EQ 'PR005'.
      MOVE '&COPY_BOM' TO ls_toolbar-function.
      MOVE icon_select_detail TO ls_toolbar-icon.
      MOVE TEXT-048 TO ls_toolbar-quickinfo."参考BOM
      MOVE ' ' TO ls_toolbar-disabled.
      MOVE TEXT-048 TO ls_toolbar-text."参考BOM
      APPEND ls_toolbar TO ut_toolbar.
      CLEAR ls_toolbar.
    ENDIF.

  ENDIF.

ENDFORM.


FORM f_user_command_900 USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
  DATA:lv_line_id TYPE zline_id .
  CLEAR g_error.

  CHECK g_readonly = 'M'.
  CASE ok_code.
    WHEN '&ADD'.

      PERFORM frm_get_new_afonr CHANGING lv_line_id.

      CLEAR gs_item.
      PERFORM init_item_line USING lv_line_id CHANGING gs_item.
      gs_item-icon = icon_led_inactive.
      gs_item-text = TEXT-019."'初始'.
      gs_item-afono = gs_head-afono.
      APPEND gs_item TO gt_item.
      CLEAR gs_item.

      PERFORM  f_refresh_grid_alv USING g_grid_900.
    WHEN '&DEL'.

      CALL METHOD g_grid_900->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      IF lt_index_rows[] IS INITIAL.
        MESSAGE s042 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-022."是否确认删除选中行?'.
      CHECK g_error IS INITIAL.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_item ASSIGNING FIELD-SYMBOL(<fs_item>)
                                    INDEX ls_index_rows-index.
        CHECK sy-subrc = 0.

        IF <fs_item>-icon =  icon_led_inactive.
          <fs_item>-icon = ''.
          <fs_item>-del_flag = 'X'.
        ELSE.
          SELECT SINGLE afono INTO @DATA(ls_afono)
            FROM zafo_item WHERE afono = @<fs_item>-afono AND afonr = @<fs_item>-afonr.
          IF sy-subrc EQ 0.
            <fs_item>-icon = icon_delete.
            <fs_item>-del_flag = 'X'.
          ELSE.
            <fs_item>-icon = ''.
            <fs_item>-del_flag = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT gt_item WHERE del_flag = 'X' AND icon = ''.
        IF gt_item-afono_ref IS NOT INITIAL AND gt_item-afonr_ref > 0.
          UPDATE zafo_item SET afono_ref = ''
                               afonr_ref = ''
                               WHERE afono = gt_item-afono_ref
                                 AND afonr = gt_item-afonr_ref.
        ENDIF.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      DELETE gt_item WHERE del_flag = 'X' AND icon = '' .

      PERFORM  f_refresh_grid_alv USING g_grid_900.

    WHEN '&COPY'.
      DATA : line(3) TYPE i.
      DATA : ls_afonr TYPE zafonr.
      CALL METHOD g_grid_900->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      DATA(ls_line) = lines( lt_index_rows ).
      IF ls_line <> 1.
        MESSAGE s043 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      PERFORM frm_pop_get_copy_line CHANGING line .
      CHECK line IS NOT INITIAL.

      READ TABLE lt_index_rows INTO ls_index_rows INDEX 1.
      READ TABLE gt_item INTO gs_item INDEX ls_index_rows-index.

      DELETE gt_item WHERE icon = icon_led_inactive.

      SORT gt_item BY afonr.
      LOOP AT gt_item.
      ENDLOOP.
      ls_afonr = gt_item-afonr.
      DO line TIMES.
        ADD 1 TO ls_afonr.
        gs_item-afonr = ls_afonr.
        APPEND gs_item TO gt_item.
      ENDDO.

      PERFORM  f_refresh_grid_alv USING g_grid_900.

    WHEN  '&COPY_BOM'.
      DATA : ls_zppdhd TYPE ztpp0091-zppdhd.
      DATA : ls_copy_bom_afonr TYPE zafonr.
      DATA:lt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

      PERFORM  frm_pop_get_zppdhd CHANGING ls_zppdhd.
      CHECK ls_zppdhd IS NOT INITIAL.

      PERFORM f_bom_f4 TABLES lt_return_tab USING ls_zppdhd 'X'.

      CHECK lt_return_tab[] IS NOT INITIAL.

      DELETE gt_item WHERE icon = icon_led_inactive.
      DELETE lt_return_tab WHERE fieldval IS INITIAL.

      SORT gt_item BY afonr.
      LOOP AT gt_item.
      ENDLOOP.

      ls_copy_bom_afonr = gt_item-afonr.

      LOOP AT lt_return_tab.

        PERFORM frm_set_icon USING 'A' CHANGING gs_item-icon gs_item-text.
        gs_item-text = TEXT-024."'已维护'.
        gs_item-zppdhd = ls_zppdhd.
        gs_item-zzbom_item = lt_return_tab-fieldval.

        PERFORM f_set_zppdhd CHANGING gs_item.
        ADD 1 TO ls_copy_bom_afonr.
        gs_item-afonr = ls_copy_bom_afonr.

        PERFORM f_set_bom CHANGING gs_item.
        APPEND gs_item TO gt_item.
      ENDLOOP.

      PERFORM  f_refresh_grid_alv USING g_grid_900.
  ENDCASE.

ENDFORM.


MODULE create_object_0900 OUTPUT.

  IF g_grid_900 IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_900.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_grid_field_catalog_900.
**-- LAYOUT
    PERFORM f_create_grid_layout_900.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_900  CHANGING gt_exclude_900[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_grid_handlers_900 CHANGING g_grid_900.
**-- REGISTER EVENT
    PERFORM f_register_grid_event_900 USING g_grid_900.
**--
    CALL METHOD cl_gui_cfw=>flush.

    PERFORM f_set_drop_down_table USING g_grid_900 CHANGING gt_fcat_900   .
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_900.
*--
    CALL METHOD g_grid_900->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid_900.
*    PERFORM f_refresh_grid_alv USING g_grid2.
  ENDIF.

ENDMODULE.


FORM f_create_container_900 .

  CREATE OBJECT g_cumtom_container_900
    EXPORTING
      container_name = 'ITEM'.

  IF g_splitter_900 IS INITIAL.
    CREATE OBJECT g_splitter_900
      EXPORTING
        parent  = g_cumtom_container_900
        rows    = 1
        columns = 1.

    g_container_900  = g_splitter_900->get_container( row = 1 column = 1 ).
*    g_container_2  = g_splitter->get_container( row = 1 column = 1 ).

  ENDIF.

  CREATE OBJECT g_grid_900
    EXPORTING
      i_parent = g_container_900.

*  CREATE OBJECT g_grid2
*    EXPORTING
*      i_parent = g_container_2.

ENDFORM.


FORM f_set_grid_field_catalog_900 .

  REFRESH: gt_fcat_900.

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
      WHEN 'ICON' OR 'TEXT' OR 'AFONR'.
        <ls_fcat>-fix_column = 'X'.
        CONTINUE.
      WHEN 'MATNR'.
        <ls_fcat>-f4availabl = 'X'.
    ENDCASE.

    READ TABLE gt_screen WITH KEY object = g_object
                                  fieldname = <ls_fcat>-fieldname
                                  fieldalv = 'ITEM'.
    IF sy-subrc EQ 0.


      <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m =
       <ls_fcat>-scrtext_l = <ls_fcat>-reptext = gt_screen-coltext.


      <ls_fcat>-emphasize = gt_screen-emphasize.

      <ls_fcat>-col_pos = gt_screen-dzaehk.

      IF gt_screen-fedit = 'X'.
        <ls_fcat>-edit = 'X'.
        <ls_fcat>-auto_value = 'X'.
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

      IF gt_screen-requi = 'X'.
        IF gt_screen-emphasize IS INITIAL.
          <ls_fcat>-emphasize  = 'C300'.
        ELSE.
          <ls_fcat>-emphasize = gt_screen-emphasize.
        ENDIF.
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

  gt_fcat_900 = lt_fcat.

ENDFORM.


FORM f_create_grid_layout_900 .

  CLEAR: gs_layout_900.
  gs_layout_900-sel_mode   = 'A'.
  gs_layout_900-cwidth_opt = 'X'.
  gs_layout_900-zebra      = 'X'.
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


FORM f_create_grid_toolbar_900
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


FORM f_assign_grid_handlers_900 CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_grid_900.

  SET HANDLER g_event_receiver_grid_900->handle_data_changed
          FOR c_grid .

  SET HANDLER g_event_receiver_grid_900->handle_toolbar
          FOR c_grid .

  SET HANDLER g_event_receiver_grid_900->data_changed_finished
        FOR c_grid.

  SET HANDLER g_event_receiver_grid_900->handle_user_command
          FOR c_grid .

  SET HANDLER g_event_receiver_grid_900->handle_onf4
         FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.

  SET HANDLER g_event_receiver_grid_900->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_grid_900->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.


FORM f_register_grid_event_900  USING u_grid TYPE REF TO cl_gui_alv_grid.

  DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE.
  CLEAR lt_f4.
  lt_f4-fieldname = 'MATNR'.
  lt_f4-register = 'X'.
  lt_f4-chngeafter = 'X'.
  APPEND lt_f4.
  CALL METHOD u_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].


* ENTER EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
* MODIFY EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.


FORM f_display_grid_alv_900 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.
  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'G_GRID_900' CHANGING ls_variant-handle.


  CALL METHOD g_grid_900->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_900
      it_toolbar_excluding = gt_exclude_900[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item[]
      it_sort              = gt_sort_900[]
      it_fieldcatalog      = gt_fcat_900[].
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


FORM f_set_catalog_alv_900.

  PERFORM f_set_grid_field_catalog_900.

  CALL METHOD g_grid_900->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_900.

  CALL METHOD g_grid_900->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_900.
ENDFORM.
