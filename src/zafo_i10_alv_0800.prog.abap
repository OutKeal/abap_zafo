*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:
  lcl_event_receiver_800_left DEFINITION DEFERRED.

CLASS:
  lcl_event_receiver_800_right DEFINITION DEFERRED.

DATA:
  g_event_receiver_800_left  TYPE REF TO lcl_event_receiver_800_left,
  g_event_receiver_800_right TYPE REF TO lcl_event_receiver_800_right.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_800_left DEFINITION.

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

    METHODS handle_toolbar
      FOR EVENT toolbar
      OF cl_gui_alv_grid
      IMPORTING e_object.
ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_800_left IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_800_left
      USING er_data_changed
            e_onf4.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
    PERFORM f_handle_double_click_800_left USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_800_left USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_800_left USING e_ucomm.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_800_left USING e_modified et_good_cells.
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_800_left USING e_object->mt_toolbar.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_800_right DEFINITION.

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
CLASS lcl_event_receiver_800_right IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.


  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM F_HANDLE_DOUBLE_CLICK_800_RIGHT USING E_ROW E_COLUMN.
  ENDMETHOD.

  METHOD handle_hotspot_click.
*    PERFORM F_HANDLE_HOTSPOT_800_RIGHT USING E_ROW_ID E_COLUMN_ID .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_800 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_800_left USING e_modified et_good_cells.
    PERFORM f_data_changed_800_right USING e_modified et_good_cells.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_800 USING e_ucomm.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION





DATA: g_grid_800_left         TYPE REF TO cl_gui_alv_grid,
      g_grid_800_right        TYPE REF TO cl_gui_alv_grid,
      gt_fcat_800_left        TYPE lvc_t_fcat,
      gt_fcat_800_right       TYPE lvc_t_fcat,


      gs_layout_800           TYPE lvc_s_layo,
      gt_sort_800_left        TYPE lvc_t_sort,
      gt_exclude_800          TYPE ui_functions,
      g_docking_container_800 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_800  TYPE REF TO cl_gui_custom_container,
      g_container_800_left    TYPE REF TO cl_gui_container,
      g_container_800_right   TYPE REF TO cl_gui_container,
      g_splitter_800          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_800_left      TYPE REF TO cl_gui_toolbar.





FORM f_handle_data_changed_800_left
 USING  u_changed TYPE REF TO cl_alv_changed_data_protocol
   u_onf4    TYPE any.


  DATA: ls_modi LIKE lvc_s_modi.

  FIELD-SYMBOLS:
    <fs_changed> TYPE any,
    <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.
  ENDLOOP.

ENDFORM.

FORM f_handle_double_click_800_left USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.



ENDFORM.


FORM f_handle_hotspot_800_left USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.


ENDFORM.


FORM f_handle_user_command_800_left USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.


ENDFORM.

FORM f_toolbar_800_left USING ut_toolbar TYPE ttb_button.

  DATA: ls_toolbar TYPE stb_button.



  CLEAR ls_toolbar.

*  IF gs_bustyp-busref IS INITIAL OR gs-object-main_dynnr = '0800'.
  MOVE '&ADD' TO ls_toolbar-function.
  MOVE icon_insert_row TO ls_toolbar-icon.
  MOVE text-020 TO ls_toolbar-quickinfo."插入行
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE text-020 TO ls_toolbar-text."插入行
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  MOVE '&DEL' TO ls_toolbar-function.
  MOVE icon_delete_row TO ls_toolbar-icon.
  MOVE text-021 TO ls_toolbar-quickinfo. "删除行
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE text-021 TO ls_toolbar-text."删除行
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.
*  ENDIF.



  CLEAR ls_toolbar.
  MOVE '&MASS_MODIFY' TO ls_toolbar-function.
  MOVE icon_status_ok TO ls_toolbar-icon.
  MOVE text-018 TO ls_toolbar-quickinfo."批量维护
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE text-018 TO ls_toolbar-text."批量维护
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.



ENDFORM.
FORM f_toolbar_800 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.


  CLEAR ls_toolbar.
  MOVE '&MASS_MODIFY' TO ls_toolbar-function.
  MOVE icon_status_ok TO ls_toolbar-icon.
  MOVE text-018 TO ls_toolbar-quickinfo."批量维护
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE text-018 TO ls_toolbar-text."批量维护
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.


ENDFORM.

FORM f_user_command_800_left USING ok_code.
  CHECK g_readonly <> 'D'.
  DATA:lv_line_id TYPE zafonr.
  CASE ok_code.
    WHEN '&MASS_MODIFY'.
      CLEAR ok_code.

      PERFORM frm_mass_modify TABLES gt_fcat_800_left .
      PERFORM f_refresh_grid_alv USING g_grid_800_left.

    WHEN '&ADD'.
      LOOP AT gt_item_serve ASSIGNING <gs_item_serve>.
        IF <gs_item_serve>-afonr > lv_line_id.
          lv_line_id = <gs_item_serve>-afonr.
        ENDIF.
      ENDLOOP.
      lv_line_id = lv_line_id + 1.

      CLEAR gs_item_serve.
      gs_item_serve-afonr = lv_line_id.
      APPEND gs_item_serve TO gt_item_serve.
      CLEAR gs_item_serve.

      PERFORM  f_refresh_grid_alv USING g_grid_800_left.
    WHEN '&DEL'.

      CALL METHOD g_grid_800_left->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      IF lt_index_rows[] IS INITIAL.
        MESSAGE S039 DISPLAY LIKE 'E'."请至少选择一行项目删除
        RETURN.
      ENDIF.


      PERFORM frm_pop_confirm USING text-022."是否确认删除选中行?'.
      CHECK g_error IS INITIAL.


      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_item_serve ASSIGNING FIELD-SYMBOL(<fs_item_serve>)
                                    INDEX ls_index_rows-index.
        CHECK sy-subrc = 0.
        <fs_item_serve>-afonr = 999999.
        SELECT SINGLE afono INTO @DATA(ls_afono)
          FROM zafo_item_serve WHERE afono = @<fs_item_serve>-afono AND afonr = @<fs_item_serve>-afonr.
        IF sy-subrc EQ 0.
          DELETE FROM zafo_item_serve WHERE afono = <fs_item_serve>-afono AND afonr = <fs_item_serve>-afonr.
          COMMIT WORK.
        ELSE.
        ENDIF.

      ENDLOOP.

      DELETE gt_item WHERE afonr = 999999.



      PERFORM  f_refresh_grid_alv USING g_grid_800_left.
  ENDCASE.

ENDFORM.


FORM f_user_command_800 USING ok_code.

  CASE ok_code .
    WHEN '&MASS_MODIFY'.
      PERFORM frm_mass_sum_modify TABLES gt_fcat_800_right .
      PERFORM f_refresh_grid_alv USING g_grid_800_right.

    WHEN '&OK'.

      CHECK gs_bustyp-sum_screen = 'X'.

      FIELD-SYMBOLS:<f_sum_value> TYPE any.
      FIELD-SYMBOLS:<f_item_value> TYPE any.
      FIELD-SYMBOLS:<f_sum_head_value> TYPE any.
      FIELD-SYMBOLS:<f_split_col_value> TYPE any.
      FIELD-SYMBOLS:<f_split_col_sum_value> TYPE any.

      DATA:l_sum TYPE menge_d.

      LOOP AT gt_screen WHERE object = g_object AND split_col <> ''.

        LOOP AT gt_item_sum.

          IF gt_screen-split_col = 'X'.
            ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gt_item_sum TO <f_sum_value>.
            CHECK sy-subrc EQ 0.

            LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.

              ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
              CHECK sy-subrc EQ 0.

              <f_item_value> = <f_sum_value>.

            ENDLOOP.


          ELSE.

            CLEAR l_sum.

            ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gt_item_sum TO <f_sum_value>.
            CHECK sy-subrc EQ 0.

            ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gs_head TO <f_sum_head_value>.
            IF sy-subrc EQ 0.
              IF sy-tabix = 1.
                <f_sum_head_value> = <f_sum_value>.
              ELSE.
                <f_sum_head_value> = <f_sum_value> + <f_sum_head_value>.
              ENDIF.
            ENDIF.

            ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE gt_item_sum TO <f_split_col_sum_value>.
            CHECK sy-subrc EQ 0.

            l_sum = <f_sum_value>.

            LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.

              ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
              CHECK sy-subrc EQ 0.
              CLEAR <f_item_value>.

              ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_value>.
              CHECK sy-subrc EQ 0.
              CHECK <f_split_col_value> IS NOT INITIAL.

              <f_item_value> = <f_sum_value> * <f_split_col_value> / <f_split_col_sum_value>.

              IF <f_item_value> IS NOT INITIAL.

                PERFORM frm_set_round USING <gs_item>-meins CHANGING <f_item_value>.

              ENDIF.

              IF gt_screen-fieldname = 'MENGE'.
                PERFORM f_set_menge_cg CHANGING <gs_item>.
                PERFORM f_set_amount CHANGING <gs_item>.
              ENDIF.

              IF gt_screen-fieldname = 'MENGE_CG'.
                PERFORM f_set_menge CHANGING <gs_item>.
                PERFORM f_set_amount CHANGING <gs_item>.
              ENDIF.


              l_sum = l_sum - <f_item_value>.

            ENDLOOP.

            IF l_sum <> 0.
              <f_item_value> = <f_item_value> + l_sum.
              PERFORM f_set_amount CHANGING <gs_item>.
            ENDIF.

          ENDIF.
        ENDLOOP.

      ENDLOOP.

      IF sy-subrc EQ 0.

        MESSAGE s044(zafo) ."WITH '已更新明细行'.

        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = 'RELOAD'.

        PERFORM   f_refresh_grid_alv USING g_grid_800_left.

      ENDIF.
  ENDCASE.

ENDFORM.



FORM f_data_changed_800_left  USING  e_modified
                                   et_good_cells TYPE lvc_t_modi.

*
  DATA:ls_refresh TYPE char1.
  DATA:ls_refresh_po TYPE char1.
  CLEAR ls_refresh.
  CLEAR ls_refresh_po.
  CHECK NOT et_good_cells IS INITIAL.
  DATA:ls_menge_sum TYPE menge_d.
  DATA:ls_menge_sumkey TYPE menge_d.
  DATA:ls_menge_last TYPE menge_d.

  LOOP AT et_good_cells INTO DATA(ls_cell).


    READ TABLE gt_item ASSIGNING <gs_item>
                            INDEX ls_cell-row_id.

    CHECK sy-subrc = 0.

    IF <gs_item>-icon <> icon_led_yellow.
      <gs_item>-icon = icon_led_yellow.
      <gs_item>-text = text-014."'已维护'.
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

      WHEN 'MENGE_CG'.

        PERFORM f_set_menge CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN 'MATNR'.
        PERFORM f_set_maktx CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'TO_ZZBOM_ITEM'.
        PERFORM f_set_umbom CHANGING <gs_item>.
        ls_refresh = 'X'.


      WHEN 'MENGE'.

        PERFORM f_set_menge_cg CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

    ENDCASE.

  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_800_left.
  ENDIF.

*  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
*    EXPORTING
*      NEW_CODE = 'ZDATA_CHANGE'.

ENDFORM.



FORM f_data_changed_800_right  USING  e_modified
                                   et_good_cells TYPE lvc_t_modi.

*
  DATA:ls_refresh TYPE char1.
  DATA:ls_refresh_po TYPE char1.
  CLEAR ls_refresh.
  CLEAR ls_refresh_po.
  CHECK NOT et_good_cells IS INITIAL.

  LOOP AT et_good_cells INTO DATA(ls_cell).


    READ TABLE gt_item_sum ASSIGNING <gs_item>
                              INDEX ls_cell-row_id.

    CHECK sy-subrc = 0.

    CASE ls_cell-fieldname.

      WHEN 'PRICE_LONG'.

                PERFORM frm_set_price  CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'PRICE'.

        PERFORM f_set_amount CHANGING <gs_item>.
        ls_refresh = 'X'.



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

    ENDCASE.

  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_800_right.
  ENDIF.

*  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
*    EXPORTING
*      NEW_CODE = 'ZDATA_CHANGE'.

ENDFORM.




MODULE create_object_0800 OUTPUT.

  IF g_grid_800_left IS INITIAL.
**-- CREATE CONTAINER
    PERFORM f_create_container_800.
**-- FIELD_CATALOG DEFINE
    PERFORM f_set_field_catalog_800_left.
    PERFORM f_set_field_catalog_800_right.
**-- LAYOUT
    PERFORM f_create_grid_layout_800.
**-- TOOLBAR EXCLUDE
    PERFORM f_create_grid_toolbar_800  CHANGING gt_exclude_800[].
**-- GRID EVENT HANDLER DEFINE
    PERFORM f_assign_handlers_800_left CHANGING g_grid_800_left.
    PERFORM f_assign_handlers_800_right CHANGING g_grid_800_right.

**-- REGISTER EVENT
    PERFORM f_register_event_800_left USING g_grid_800_left.
    PERFORM f_register_event_800_right USING g_grid_800_right.

    CALL METHOD cl_gui_cfw=>flush.
**-- DISPLAY GRID ALV
    PERFORM f_display_grid_alv_800.
*--
    CALL METHOD g_grid_800_left->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
**--
    PERFORM f_refresh_grid_alv USING g_grid_800_left.

    IF gs_bustyp-sum_screen = 'X'.
      PERFORM f_refresh_grid_alv USING g_grid_800_right.
    ENDIF.
  ENDIF.

ENDMODULE.

FORM f_create_container_800 .
  DATA:l_col TYPE i.


*  IF G_DOCKING_CONTAINER_800 IS INITIAL.
*
*    CREATE OBJECT G_DOCKING_CONTAINER_800
*      EXPORTING
*        STYLE     = CL_GUI_CONTROL=>WS_CHILD
*        REPID     = SY-REPID
*        DYNNR     = SY-DYNNR
*        SIDE      = G_DOCKING_CONTAINER_800->DOCK_AT_BOTTOM
*        LIFETIME  = CL_GUI_CONTROL=>LIFETIME_IMODE
*        EXTENSION = '8000'
*      EXCEPTIONS
*        OTHERS    = 1.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID
*            TYPE SY-MSGTY
*          NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ENDIF.

  CREATE OBJECT g_cumtom_container_800
    EXPORTING
      container_name = 'ITEM'.


  IF gs_bustyp-sum_screen = 'X'.
    l_col = 2.
  ELSE.
    l_col = 1.
  ENDIF.

* SPLITTER CONTAINER
  IF g_splitter_800 IS INITIAL.

    CREATE OBJECT g_splitter_800
      EXPORTING
        parent  = g_cumtom_container_800
        rows    = 1
        columns = l_col.

    g_container_800_left  = g_splitter_800->get_container( row = 1 column = 1 ).

    CREATE OBJECT g_grid_800_left
      EXPORTING
        i_parent = g_container_800_left.

    IF gs_bustyp-sum_screen = 'X'.

      g_container_800_right = g_splitter_800->get_container( row = 1 column = 2 ).

      CREATE OBJECT g_grid_800_right
        EXPORTING
          i_parent = g_container_800_right.


      CALL METHOD g_splitter_800->set_column_width
        EXPORTING
          id    = 1
          width = 45.



    ENDIF.

  ENDIF.




ENDFORM.


FORM f_set_field_catalog_800_left .

  REFRESH: gt_fcat_800_left.

  FIELD-SYMBOLS:
    <ls_fcat> TYPE lvc_s_fcat.
  DATA:
    lt_fcat TYPE lvc_t_fcat.

  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_fieldcat TYPE slis_fieldcat_alv.

  DATA: l_struc_name LIKE  dd02l-tabname .

  l_struc_name = 'ZAFO_ITEM_SERVE'.

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
                                  fieldalv = 'SERV'.
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

      <ls_fcat>-emphasize = gt_screen-emphasize.

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



  gt_fcat_800_left = lt_fcat.

ENDFORM.
FORM f_set_field_catalog_800_right .

  CHECK gs_bustyp-sum_screen = 'X'.

  REFRESH: gt_fcat_800_right.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.
  DATA:lt_fcat TYPE lvc_t_fcat.

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

      <ls_fcat>-emphasize = gt_screen-emphasize.

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


  gt_fcat_800_right = lt_fcat.

ENDFORM.





FORM f_create_grid_layout_800 .

  CLEAR: gs_layout_800.
  gs_layout_800-sel_mode   = 'A'.
  gs_layout_800-cwidth_opt = 'X'.
  gs_layout_800-zebra      = 'X'.
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

FORM f_create_grid_toolbar_800
  CHANGING  c_t_toolbar TYPE ui_functions.

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

FORM f_assign_handlers_800_left
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_800_left.

  SET HANDLER g_event_receiver_800_left->handle_data_changed
          FOR c_grid .

  SET HANDLER g_event_receiver_800_left->handle_toolbar
          FOR c_grid .

  SET HANDLER g_event_receiver_800_left->data_changed_finished
        FOR c_grid.
  SET HANDLER g_event_receiver_800_left->handle_user_command
          FOR c_grid .
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_800_left->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_800_left->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_assign_handlers_800_right
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.
  CHECK gs_bustyp-sum_screen = 'X'.

  CREATE OBJECT g_event_receiver_800_right.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_800_right->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_800_right->handle_user_command
          FOR c_grid .
  SET HANDLER g_event_receiver_800_right->data_changed_finished
      FOR c_grid.
*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_ON_F4
*          FOR C_GRID .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_800_right->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_800_right->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .

ENDFORM.

FORM f_register_event_800_left
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

FORM f_register_event_800_right
  USING u_grid TYPE REF TO cl_gui_alv_grid.
  CHECK gs_bustyp-sum_screen = 'X'.
* ENTER EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
** MODIFY EVENT
  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.




FORM f_display_grid_alv_800 .

  DATA: ls_variant LIKE disvariant.
  ls_variant-report = sy-repid.
  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'g_grid_800_left' CHANGING ls_variant-handle.
  CALL METHOD g_grid_800_left->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_800
      it_toolbar_excluding = gt_exclude_800[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item_serve[]
      it_sort              = gt_sort_800_left[]
      it_fieldcatalog      = gt_fcat_800_left[].

  CHECK gs_bustyp-sum_screen = 'X'.
  PERFORM frm_get_alv_handle
    USING sy-repid g_bustyp 'g_grid_800_right' CHANGING ls_variant-handle.
  CALL METHOD g_grid_800_right->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_800
      it_toolbar_excluding = gt_exclude_800[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item[]
*     IT_SORT              = GT_SORT[]
      it_fieldcatalog      = gt_fcat_800_right[].

ENDFORM.

FORM f_set_catalog_alv_800.

  PERFORM f_set_field_catalog_800_left.
  CALL METHOD g_grid_800_left->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_800_left.

  CALL METHOD g_grid_800_left->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_800.


  IF gt_fcat_800_right IS NOT INITIAL.
    PERFORM f_set_field_catalog_800_right.

    CALL METHOD g_grid_800_right->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat_800_right.

    CALL METHOD g_grid_800_right->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_800.

  ENDIF.

ENDFORM.
