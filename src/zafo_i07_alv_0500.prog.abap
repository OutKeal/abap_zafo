*&---------------------------------------------------------------------*
*& 包含               ZMMR0020_ALV
*&---------------------------------------------------------------------*

CLASS:lcl_event_receiver_500_left DEFINITION DEFERRED.

CLASS:lcl_event_receiver_500_right DEFINITION DEFERRED.

DATA: g_event_receiver_500_left  TYPE REF TO lcl_event_receiver_500_left,
      g_event_receiver_500_right TYPE REF TO lcl_event_receiver_500_right.

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_500_left DEFINITION.

  PUBLIC SECTION.

    METHODS handle_onf4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells.

    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.

    METHODS  handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS: data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.

    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.
ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_500_left IMPLEMENTATION.

  METHOD handle_onf4 .
    PERFORM f_handle_onf4_500_left USING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells.
  ENDMETHOD.

  METHOD handle_data_changed.
    PERFORM f_handle_data_changed_500_left  USING er_data_changed e_onf4.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM f_handle_double_click_500_left USING e_row e_column.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    PERFORM f_handle_hotspot_500_left USING e_row_id e_column_id .
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_500_left USING e_ucomm.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_500_left USING e_modified et_good_cells.
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_500_left USING e_object->mt_toolbar.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION

*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER_GRID DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_500_right DEFINITION.

  PUBLIC SECTION.
* DATA CHANGED
    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed e_onf4.

    METHODS handle_double_click
      FOR EVENT double_click  OF cl_gui_alv_grid
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
CLASS lcl_event_receiver_500_right IMPLEMENTATION.
* DATA CHANGED
  METHOD handle_data_changed.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD handle_double_click.
*    PERFORM F_HANDLE_DOUBLE_CLICK_500_RIGHT USING E_ROW E_COLUMN.
  ENDMETHOD.

  METHOD handle_hotspot_click.
*    PERFORM F_HANDLE_HOTSPOT_500_RIGHT USING E_ROW_ID E_COLUMN_ID .
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM f_toolbar_500 USING e_object->mt_toolbar.
  ENDMETHOD.

  METHOD data_changed_finished.
    PERFORM f_data_changed_500_right USING e_modified et_good_cells.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM f_user_command_500_sum USING e_ucomm 'X'.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER_GRID IMPLEMENTATION


DATA: g_grid_500_left         TYPE REF TO cl_gui_alv_grid,
      g_grid_500_right        TYPE REF TO cl_gui_alv_grid,
      gt_fcat_500_left        TYPE lvc_t_fcat,
      gt_fcat_500_right       TYPE lvc_t_fcat,

      gs_layout_500           TYPE lvc_s_layo,
      gt_sort_500_left        TYPE lvc_t_sort,
      gt_exclude_500          TYPE ui_functions,
      g_docking_container_500 TYPE REF TO cl_gui_docking_container,
      g_cumtom_container_500  TYPE REF TO cl_gui_custom_container,
      g_container_500_left    TYPE REF TO cl_gui_container,
      g_container_500_right   TYPE REF TO cl_gui_container,
      g_splitter_500          TYPE REF TO cl_gui_splitter_container,
      g_toolbar_500_left      TYPE REF TO cl_gui_toolbar.



FORM f_handle_onf4_500_left USING e_fieldname TYPE  lvc_fname
                              e_fieldvalue  TYPE  lvc_value
                              es_row_no TYPE  lvc_s_roid
                              er_event_data	TYPE REF TO	cl_alv_event_data
                              et_bad_cells  TYPE  lvc_t_modi.
  DATA: ls_modi    TYPE lvc_s_modi,
        lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  FIELD-SYMBOLS <modtab> TYPE lvc_t_modi.
  READ TABLE gt_item INDEX es_row_no-row_id.
  CHECK sy-subrc = 0.

  REFRESH lt_ret_tab.

  CASE e_fieldname.
    WHEN 'MATNR' OR 'MAKTX' .
      DATA:ls_maktx TYPE maktx.

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

    WHEN 'TO_ZPPDHD'.

      DATA:ls_zppdhd TYPE zppdhd.

      ls_zppdhd = e_fieldvalue.
      PERFORM f_zppdhd_f4  USING e_fieldvalue CHANGING ls_zppdhd.

      IF ls_zppdhd IS NOT INITIAL.
        ls_modi-row_id = es_row_no-row_id.
        ls_modi-fieldname = e_fieldname.
        ls_modi-value = ls_zppdhd.
        ASSIGN er_event_data->m_data->* TO <modtab>.
        APPEND ls_modi TO <modtab>.
      ENDIF.

  ENDCASE.

  er_event_data->m_event_handled = 'X'."通知系统搜索事件处理完毕，这样就不会调用系统标准的Search Help。
ENDFORM.


FORM f_handle_data_changed_500_left USING u_changed TYPE REF TO cl_alv_changed_data_protocol
                                          u_onf4    TYPE any.
  DATA: ls_modi LIKE lvc_s_modi.
  FIELD-SYMBOLS:<fs_changed> TYPE any,
                <fs_mod>     TYPE any.

  LOOP AT u_changed->mt_good_cells INTO ls_modi.
  ENDLOOP.

ENDFORM.


FORM f_handle_double_click_500_left USING e_row_id TYPE lvc_s_row
                                            e_column_id TYPE lvc_s_col.

  IF e_column_id-fieldname = 'ZVAT_NUB'.
    CHECK g_bustyp = 'R1001' OR g_bustyp = 'R1002' OR  g_bustyp = 'R1003'
          OR g_bustyp = 'R1005' OR g_bustyp = 'R1006'.

    READ TABLE gt_item ASSIGNING <gs_item> INDEX e_row_id-index.

    IF gt_item_batch[] IS INITIAL AND gs_head-afono IS NOT INITIAL.
      SELECT *
        INTO TABLE gt_item_batch
        FROM zafo_item_batch
        WHERE afono = gs_head-afono.
      IF sy-subrc NE 0 .
        SELECT * INTO TABLE gt_item_batch
          FROM zafo_item_batch
          FOR ALL ENTRIES IN gt_item
          WHERE afono = gt_item-afono_ref
          AND afonr = gt_item-afonr_ref.
        IF sy-subrc EQ 0.
          LOOP AT gt_item_batch.
            READ TABLE gt_item WITH KEY afono_ref = gt_item-afono afonr_ref = gt_item-afonr.
            IF sy-subrc EQ 0.
              gt_item_batch-afonr = gt_item-afonr.
              MODIFY gt_item_batch.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR gt_item_batch_dis[].
    LOOP AT gt_item_batch WHERE afono = <gs_item>-afono AND afonr = <gs_item>-afonr.
      APPEND gt_item_batch TO gt_item_batch_dis.
      CLEAR gt_item_batch.
    ENDLOOP.

    CALL SCREEN 0510 STARTING AT 10 5 ENDING AT 100 30.

    PERFORM f_refresh_grid_alv USING g_grid_500_left.

  ENDIF.

  IF e_column_id-fieldname = 'TO_ZZBOM_ITEM'.
    READ TABLE gt_item ASSIGNING <gs_item> INDEX e_row_id-index.
    CHECK sy-subrc EQ 0.

    SELECT zppdhd, zzbom_item ,
             matnr,
             maktx,
             idnlf,
             zcolor1,
             zsize,
             zcolor1 AS zcolor,
             zzjsssgg,
             zcqy,
             zzdh,
             meins
      FROM ztpp0091
      WHERE zppdhd = @<gs_item>-to_zppdhd
      AND matnr = @<gs_item>-matnr
      AND idnlf = @<gs_item>-idnlf
      INTO TABLE @DATA(gt_data).

    IF sy-subrc NE 0.
      MESSAGE s038." '该大货单BOM中无此物料'.
    ENDIF.

    DATA(lines) = lines( gt_data ).

    DATA:lt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'ZZBOM_ITEM'
        pvalkey         = 'ZZBOM_ITEM'
        dynprofield     = 'ZZBOM_ITEM'
        value_org       = 'S'
*       multiple_choice = 'X'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
*       callback_program = sy-repid
*       callback_form   = 'FRM_RETURN_VALUE'
      TABLES
        value_tab       = gt_data
        return_tab      = lt_return_tab[]
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    CHECK lt_return_tab[] IS NOT INITIAL.
    LOOP AT lt_return_tab.
      <gs_item>-to_zzbom_item = lt_return_tab-fieldval.
    ENDLOOP.

    IF <gs_item>-to_zzbom_item IS NOT INITIAL.
      PERFORM f_set_umbom CHANGING <gs_item>.
    ENDIF.

    PERFORM f_refresh_grid_alv USING g_grid_500_left.
  ENDIF.

ENDFORM.


FORM f_handle_hotspot_500_left USING e_row_id TYPE lvc_s_row
                                   e_column_id TYPE lvc_s_col.
ENDFORM.


FORM f_handle_user_command_500_left USING ok_code.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.

ENDFORM.

FORM f_toolbar_500_left USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

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

    MOVE '&DEL' TO ls_toolbar-function.
    MOVE icon_delete_row TO ls_toolbar-icon.
    MOVE TEXT-021 TO ls_toolbar-quickinfo. "删除行
    MOVE ' ' TO ls_toolbar-disabled.
    MOVE TEXT-021 TO ls_toolbar-text."删除行
    APPEND ls_toolbar TO ut_toolbar.
    CLEAR ls_toolbar.

  ENDIF.

  CLEAR ls_toolbar.
  MOVE '&MASS_MODIFY' TO ls_toolbar-function.
  MOVE icon_status_ok TO ls_toolbar-icon.
  MOVE TEXT-018 TO ls_toolbar-quickinfo."批量维护
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-018 TO ls_toolbar-text."批量维护
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  MOVE '&QTY_CONF' TO ls_toolbar-function.
  MOVE icon_status_ok TO ls_toolbar-icon.
  MOVE TEXT-023 TO ls_toolbar-quickinfo."确认数量
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-023  TO ls_toolbar-text."确认数量
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.
ENDFORM.


FORM f_toolbar_500 USING ut_toolbar TYPE ttb_button.
  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  MOVE '&QTY_CONF' TO ls_toolbar-function.
  MOVE icon_status_ok TO ls_toolbar-icon.
  MOVE TEXT-023 TO ls_toolbar-quickinfo."确认数量
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-023  TO ls_toolbar-text."确认数量
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.

  CLEAR ls_toolbar.
  MOVE '&MASS_MODIFY' TO ls_toolbar-function.
  MOVE icon_status_ok TO ls_toolbar-icon.
  MOVE TEXT-018 TO ls_toolbar-quickinfo."批量维护
  MOVE ' ' TO ls_toolbar-disabled.
  MOVE TEXT-018 TO ls_toolbar-text."批量维护
  APPEND ls_toolbar TO ut_toolbar.
  CLEAR ls_toolbar.
ENDFORM.


FORM f_user_command_500_left USING ok_code.
  CHECK g_readonly <> 'D'.
  DATA:lv_line_id TYPE zafonr.
  CASE ok_code.
    WHEN '&MASS_MODIFY'.
      PERFORM frm_mass_modify TABLES gt_fcat_500_left .
      PERFORM frm_sum_screen_refresh TABLES gt_fcat_500_right.
      PERFORM f_refresh_grid_alv USING g_grid_500_left.

    WHEN '&QTY_CONF'.

      FIELD-SYMBOLS:<f_value> TYPE any.
      FIELD-SYMBOLS:<f_split_col_value> TYPE any.

      LOOP AT gt_item ASSIGNING <gs_item>.
        READ TABLE gt_screen WITH KEY fieldalv = 'ITEM' fieldname = 'MENGE'.
        IF sy-subrc = 0.
          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_value>.
          ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_value>.
          <f_value> = <f_split_col_value>.
        ENDIF.

        READ TABLE gt_screen WITH KEY fieldalv = 'ITEM' fieldname = 'MENGE_CG'.
        IF sy-subrc = 0.
          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_value>.
          ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_value>.
          <f_value> = <f_split_col_value>.
        ENDIF.
      ENDLOOP.

      PERFORM frm_sum_screen_refresh TABLES gt_fcat_500_right.
      MESSAGE s040(zafo) ."'已更新明细行'.
      PERFORM f_refresh_grid_alv USING g_grid_500_left.

    WHEN '&ADD'.
      LOOP AT gt_item ASSIGNING <gs_item>.
        IF <gs_item>-afonr > lv_line_id.
          lv_line_id = <gs_item>-afonr.
        ENDIF.
      ENDLOOP.
      lv_line_id = lv_line_id + 1.

      CLEAR gs_item.
      PERFORM init_item_line USING lv_line_id CHANGING gs_item.
      gs_item-icon = icon_led_inactive.
      gs_item-text = TEXT-019."'初始'.
      gs_item-afono = gs_head-afono.
      APPEND gs_item TO gt_item.
      CLEAR gs_item.
      PERFORM  f_refresh_grid_alv USING g_grid_500_left.


    WHEN '&COPY'.
      DATA : line(3) TYPE i.
      DATA : ls_afonr TYPE zafonr.
      CALL METHOD g_grid_500_left->get_selected_rows
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

      PERFORM  f_refresh_grid_alv USING g_grid_500_left.


    WHEN '&DEL'.

      CALL METHOD g_grid_500_left->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      IF lt_index_rows[] IS INITIAL.
        MESSAGE s039 DISPLAY LIKE 'E'."'请至少选择一行项目删除'
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
      DELETE gt_item WHERE icon = '' AND del_flag = 'X'.

      PERFORM  f_refresh_grid_alv USING g_grid_500_left.
  ENDCASE.

ENDFORM.


FORM f_user_command_500_sum USING ok_code  pv_refresh.

  CHECK gs_bustyp-sum_screen = 'X'.
  CASE ok_code .
    WHEN '&MASS_MODIFY'.
      PERFORM frm_mass_sum_modify TABLES gt_fcat_500_right .

      PERFORM frm_item_screen_refresh.

      PERFORM f_refresh_grid_alv USING g_grid_500_left.
      PERFORM f_refresh_grid_alv USING g_grid_500_right.

    WHEN '&QTY_CONF'.
      FIELD-SYMBOLS:<f_sum_value> TYPE any.
      FIELD-SYMBOLS:<f_split_col_sum_value> TYPE any.

      LOOP AT gt_item_sum ASSIGNING <gs_item>.
        READ TABLE gt_screen WITH KEY fieldalv = 'SUM' fieldname = 'MENGE'.
        IF sy-subrc = 0.
          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_sum_value>.
          ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_sum_value>.
          <f_sum_value> = <f_split_col_sum_value>.
        ENDIF.

        READ TABLE gt_screen WITH KEY fieldalv = 'SUM' fieldname = 'MENGE_CG'.
        IF sy-subrc = 0.
          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_sum_value>.
          ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_sum_value>.
          <f_sum_value> = <f_split_col_sum_value>.
        ENDIF.
      ENDLOOP.
      PERFORM frm_set_sum_apportion.
      IF pv_refresh EQ 'X' .
        PERFORM frm_sum_screen_refresh TABLES gt_fcat_500_right.
        MESSAGE s040(zafo) ."'已更新明细行'.
        PERFORM f_refresh_grid_alv USING g_grid_500_right.
        PERFORM f_refresh_grid_alv USING g_grid_500_left.
      ENDIF.

    WHEN '&APPORTION'.

      PERFORM frm_set_sum_apportion.

      IF pv_refresh EQ 'X' .
        PERFORM frm_sum_screen_refresh TABLES gt_fcat_500_right.
        MESSAGE s040(zafo) ."'已更新明细行'.
        PERFORM f_refresh_grid_alv USING g_grid_500_right.
        PERFORM f_refresh_grid_alv USING g_grid_500_left.
      ENDIF.

  ENDCASE.

ENDFORM.


FORM f_data_changed_500_left  USING  e_modified
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

    READ TABLE gt_item ASSIGNING <gs_item>  INDEX ls_cell-row_id.

    CHECK sy-subrc = 0.

    IF <gs_item>-icon <> icon_led_yellow.
      <gs_item>-icon = icon_led_yellow.
      <gs_item>-text = TEXT-032."'已维护'.
      ls_refresh = 'X'.
    ENDIF.

    CASE ls_cell-fieldname.

      WHEN 'TO_ZPPDHD'.
        PERFORM f_zppdhd_f4 USING <gs_item>-to_zppdhd CHANGING <gs_item>-to_zppdhd.

        ls_refresh = 'X'.

      WHEN 'PRICE_LONG'.
        PERFORM frm_set_price  CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'PRICE'.
        PERFORM f_set_amount CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'MENGE'.
        PERFORM f_set_menge_cg CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_sum_screen_refresh TABLES gt_fcat_500_right.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN 'MENGE_CG'.
        PERFORM f_set_menge CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_sum_screen_refresh TABLES gt_fcat_500_right.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN 'MATNR'.
        PERFORM f_set_maktx CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN 'MAKTX'.
        IF gs_head-bustyp = 'R1018'.
          <gs_item>-matnr = <gs_item>-maktx.
          PERFORM f_set_maktx CHANGING <gs_item>.
          ls_refresh = 'X'.
        ENDIF.

      WHEN 'IDNLF'.
        IF gs_head-bustyp <> 'R1018'.
          PERFORM f_set_idnlf CHANGING <gs_item>.

          ls_refresh = 'X'.
        ENDIF.
      WHEN 'TO_ZZBOM_ITEM'.
        PERFORM f_set_umbom CHANGING <gs_item>.
        ls_refresh = 'X'.

      WHEN OTHERS.
        PERFORM frm_sum_screen_refresh TABLES gt_fcat_500_right.
        ls_refresh = 'X'.

    ENDCASE.

  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM f_refresh_grid_alv USING g_grid_500_left.

    IF gs_bustyp-sum_screen = 'X' .
      PERFORM f_refresh_grid_alv USING g_grid_500_right.
    ENDIF.

  ENDIF.

ENDFORM.


FORM f_data_changed_500_right  USING e_modified
                                     et_good_cells TYPE lvc_t_modi.
  DATA:ls_refresh TYPE char1.
  DATA:ls_refresh_po TYPE char1.
  CLEAR ls_refresh.
  CLEAR ls_refresh_po.
  CHECK NOT et_good_cells IS INITIAL.

  LOOP AT et_good_cells INTO DATA(ls_cell).
    READ TABLE gt_item_sum ASSIGNING <gs_item> INDEX ls_cell-row_id.

    CHECK sy-subrc = 0.

    CASE ls_cell-fieldname.
      WHEN 'PRICE_LONG'.
        PERFORM frm_set_price  CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_item_screen_refresh.
        ls_refresh = 'X'.

      WHEN 'PRICE'.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_item_screen_refresh.
        ls_refresh = 'X'.

      WHEN 'MENGE_CG'.
        PERFORM f_set_menge CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM f_user_command_500_sum USING '&APPORTION' ''.

        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN 'MENGE'.
        PERFORM f_set_menge_cg CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM f_user_command_500_sum USING '&APPORTION' ''.
        <gs_item>-selected = 'X'.
        ls_refresh = 'X'.

      WHEN OTHERS.
        PERFORM frm_item_screen_refresh.
        ls_refresh = 'X'.

    ENDCASE.

  ENDLOOP.

  IF ls_refresh = 'X'.
    PERFORM frm_sum_screen_refresh TABLES gt_fcat_500_right.
    MESSAGE s040(zafo) ."'已更新明细行'.
    PERFORM f_refresh_grid_alv USING g_grid_500_right.
    PERFORM f_refresh_grid_alv USING g_grid_500_left.
  ENDIF.

ENDFORM.


MODULE create_object_0500 OUTPUT.

  IF g_grid_500_left IS INITIAL.

    PERFORM f_create_container_500.

    PERFORM f_set_field_catalog_500_left.
    PERFORM f_set_field_catalog_500_right.

    PERFORM f_create_grid_layout_500.

    PERFORM f_create_grid_toolbar_500  CHANGING gt_exclude_500[].

    PERFORM f_assign_handlers_500_left CHANGING g_grid_500_left.
    PERFORM f_assign_handlers_500_right CHANGING g_grid_500_right.

    PERFORM f_register_event_500_left USING g_grid_500_left.
    PERFORM f_register_event_500_right USING g_grid_500_right.

    CALL METHOD cl_gui_cfw=>flush.

    PERFORM f_display_grid_alv_500.

    CALL METHOD g_grid_500_left->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.

    PERFORM f_refresh_grid_alv USING g_grid_500_left.

    IF gs_bustyp-sum_screen = 'X'.
      PERFORM f_refresh_grid_alv USING g_grid_500_right.
    ENDIF.
  ENDIF.

ENDMODULE.


FORM f_create_container_500 .
  DATA:l_col TYPE i.

  CREATE OBJECT g_cumtom_container_500
    EXPORTING
      container_name = 'ITEM'.

  IF gs_bustyp-sum_screen = 'X'.
    l_col = 2.
  ELSE.
    l_col = 1.
  ENDIF.

  IF g_splitter_500 IS INITIAL.

    CREATE OBJECT g_splitter_500
      EXPORTING
        parent  = g_cumtom_container_500
        rows    = 1
        columns = l_col.

    g_container_500_left  = g_splitter_500->get_container( row = 1 column = 1 ).

    CREATE OBJECT g_grid_500_left
      EXPORTING
        i_parent = g_container_500_left.

    IF gs_bustyp-sum_screen = 'X'.

      g_container_500_right = g_splitter_500->get_container( row = 1 column = 2 ).

      CREATE OBJECT g_grid_500_right
        EXPORTING
          i_parent = g_container_500_right.

      CALL METHOD g_splitter_500->set_column_width
        EXPORTING
          id    = 1
          width = 35.

    ENDIF.
  ENDIF.

ENDFORM.


FORM f_set_field_catalog_500_left .

  REFRESH: gt_fcat_500_left.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.
  DATA:lt_fcat TYPE lvc_t_fcat.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
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
          CHANGING lt_fieldcat lt_fcat.
* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'ICON' OR 'TEXT' .
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 1.
        CONTINUE.
      WHEN 'TO_ZPPDHD'.
        <ls_fcat>-f4availabl = 'X'.
      WHEN 'MATNR'.
        <ls_fcat>-f4availabl = 'X'.

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

  gt_fcat_500_left = lt_fcat.
ENDFORM.


FORM f_set_field_catalog_500_right .

  CHECK gs_bustyp-sum_screen = 'X'.

  REFRESH: gt_fcat_500_right.

  FIELD-SYMBOLS:<ls_fcat> TYPE lvc_s_fcat.
  DATA:lt_fcat TYPE lvc_t_fcat.

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

  PERFORM f_transfer_slis_to_lvc CHANGING lt_fieldcat lt_fcat.

* 内容编辑
  LOOP AT lt_fcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
      WHEN 'ICON' OR 'TEXT' .
        <ls_fcat>-tech = 'X'.
        <ls_fcat>-fix_column = 'X'.
        <ls_fcat>-col_pos = 9999.
        CONTINUE.
    ENDCASE.


    READ TABLE gt_screen WITH KEY object = g_object
                                  fieldname = <ls_fcat>-fieldname
                                  fieldalv = 'SUM'.
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
        ELSE.
          <ls_fcat>-edit = ''.
        ENDIF.
      ENDIF.

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

  gt_fcat_500_right = lt_fcat.
ENDFORM.


FORM f_create_grid_layout_500 .

  CLEAR: gs_layout_500.
  gs_layout_500-sel_mode   = 'A'.
  gs_layout_500-cwidth_opt = 'X'.
  gs_layout_500-zebra      = 'X'.
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


FORM f_create_grid_toolbar_500
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


FORM f_assign_handlers_500_left
  CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT g_event_receiver_500_left.

  SET HANDLER g_event_receiver_500_left->handle_data_changed
          FOR c_grid .

  SET HANDLER g_event_receiver_500_left->handle_toolbar
          FOR c_grid .

  SET HANDLER g_event_receiver_500_left->data_changed_finished
          FOR c_grid.

  SET HANDLER g_event_receiver_500_left->handle_user_command
          FOR c_grid .

  SET HANDLER g_event_receiver_500_left->handle_onf4
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.

  SET HANDLER g_event_receiver_500_left->handle_hotspot_click
          FOR c_grid .

  SET HANDLER g_event_receiver_500_left->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .
ENDFORM.


FORM f_assign_handlers_500_right CHANGING c_grid TYPE REF TO cl_gui_alv_grid.

  CHECK gs_bustyp-sum_screen = 'X'.

  CREATE OBJECT g_event_receiver_500_right.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_DATA_CHANGED
*          FOR C_GRID .

  SET HANDLER g_event_receiver_500_right->handle_toolbar
          FOR c_grid .
  SET HANDLER g_event_receiver_500_right->handle_user_command
          FOR c_grid .
  SET HANDLER g_event_receiver_500_right->data_changed_finished
      FOR c_grid.

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_TOP_OF_PAGE
*          FOR C_GRID.
  SET HANDLER g_event_receiver_500_right->handle_hotspot_click
          FOR c_grid .
  SET HANDLER g_event_receiver_500_right->handle_double_click
          FOR c_grid .

*  SET HANDLER G_EVENT_RECEIVER_GRID->HANDLE_BUTTON_CLICK
*          FOR C_GRID .
ENDFORM.


FORM f_register_event_500_left USING u_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE.

  CLEAR lt_f4.
  lt_f4-fieldname = 'MATNR'.
  lt_f4-register = 'X'.
  lt_f4-chngeafter = 'X'.
  APPEND lt_f4.
  lt_f4-fieldname = 'TO_ZPPDHD'.
  APPEND lt_f4.
  CALL METHOD u_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.


FORM f_register_event_500_right USING u_grid TYPE REF TO cl_gui_alv_grid.

  CHECK gs_bustyp-sum_screen = 'X'.

  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD u_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.


FORM f_display_grid_alv_500 .

  DATA: ls_variant LIKE disvariant.

  ls_variant-report = sy-repid.

  PERFORM frm_get_alv_handle USING sy-repid g_bustyp 'G_GRID_500_LEFT' CHANGING ls_variant-handle.

  CALL METHOD g_grid_500_left->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_500
      it_toolbar_excluding = gt_exclude_500[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item[]
      it_sort              = gt_sort_500_left[]
      it_fieldcatalog      = gt_fcat_500_left[].

  CHECK gs_bustyp-sum_screen = 'X'.

  PERFORM frm_get_alv_handle USING sy-repid g_bustyp 'G_GRID_500_RIGHT' CHANGING ls_variant-handle.

  CALL METHOD g_grid_500_right->set_table_for_first_display
    EXPORTING
      is_variant           = ls_variant
      i_save               = 'A'
      is_layout            = gs_layout_500
      it_toolbar_excluding = gt_exclude_500[]
      i_default            = 'X'
    CHANGING
      it_outtab            = gt_item_sum[]
*     IT_SORT              = GT_SORT[]
      it_fieldcatalog      = gt_fcat_500_right[].
ENDFORM.


FORM f_set_catalog_alv_500.

  PERFORM f_set_field_catalog_500_left.

  CALL METHOD g_grid_500_left->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = gt_fcat_500_left.

  CALL METHOD g_grid_500_left->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_500.


  IF gt_fcat_500_right IS NOT INITIAL.

    PERFORM f_set_field_catalog_500_right.

    CALL METHOD g_grid_500_right->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat_500_right.

    CALL METHOD g_grid_500_right->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_500.

  ENDIF.

ENDFORM.
