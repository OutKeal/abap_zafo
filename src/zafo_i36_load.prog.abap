*&---------------------------------------------------------------------*
*& 包含               ZAFO_LOAD
*&---------------------------------------------------------------------*

FORM frm_get_tab.

  TYPES: BEGIN OF ty_abap_componentdescr,   "用于生成动态内表
           name       TYPE string,
           type       TYPE REF TO cl_abap_datadescr,
           as_include TYPE abap_bool,
           suffix     TYPE string,
         END OF ty_abap_componentdescr.
  DATA: gcr_ref_tab TYPE REF TO cl_abap_tabledescr.
  DATA: gr_ref_tab TYPE REF TO data.
  DATA: gr_ref_line TYPE REF TO data.
  DATA: gcr_ref_line TYPE REF TO cl_abap_structdescr.
  DATA: lt_abap_componentdescr TYPE STANDARD TABLE OF ty_abap_componentdescr WITH KEY name,
        ls_abap_componentdescr TYPE ty_abap_componentdescr.

  DATA: ls_fieldname TYPE fieldname.

  DATA: lt_dd03l TYPE TABLE OF dd03l WITH HEADER LINE.

  SELECT * FROM dd03l INTO TABLE lt_dd03l WHERE tabname = 'ZAFO_SITEM'.

  SORT gt_screen BY import_index.

  LOOP AT gt_screen WHERE object = g_object AND import_index <> '' AND fieldalv = 'ITEM'.

    READ TABLE lt_dd03l WITH KEY fieldname = gt_screen-fieldname.
    IF sy-subrc NE 0 .
      PERFORM frm_add_msg USING 'E' 'ZAFO' 057  '' '' '' ''."导入模板配置错误
      RETURN.
    ENDIF.

    IF lt_dd03l-rollname IS NOT INITIAL.
      ls_abap_componentdescr-name = gt_screen-fieldname.  "用于生成动态内表
      ls_abap_componentdescr-type ?= cl_abap_typedescr=>describe_by_name( lt_dd03l-rollname ).
    ELSE.
      ls_fieldname = 'ZAFO_SITEM-' && gt_screen-fieldname.
      ls_abap_componentdescr-name = gt_screen-fieldname.  "用于生成动态内表
      ls_abap_componentdescr-type ?= cl_abap_typedescr=>describe_by_data( ls_fieldname ).
    ENDIF.
    APPEND ls_abap_componentdescr TO lt_abap_componentdescr.
    CLEAR ls_abap_componentdescr.

  ENDLOOP.

  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 058  '' '' '' ''."没有配置导入模板
    RETURN.
  ENDIF.

  gcr_ref_line ?= cl_abap_structdescr=>create( p_components = lt_abap_componentdescr ).

  IF sy-subrc = 0.
    CREATE DATA gr_ref_line TYPE HANDLE gcr_ref_line.
    ASSIGN gr_ref_line->* TO <gs_tab>.
    gcr_ref_tab = cl_abap_tabledescr=>create( p_line_type = gcr_ref_line ).
    IF sy-subrc = 0.
      CREATE DATA gr_ref_tab TYPE HANDLE gcr_ref_tab.
      ASSIGN gr_ref_tab->* TO <gt_tab>.
    ENDIF.
  ELSE.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 059  '' '' '' ''."导入模板配置错误
    RETURN.
  ENDIF.


ENDFORM.


FORM frm_factory_download USING gt_table."下载动态模板

  DATA: gr_table   TYPE REF TO cl_salv_table.
  DATA: lv_filename TYPE string.

  cl_salv_table=>factory(   IMPORTING  r_salv_table = gr_table CHANGING t_table = gt_table ).

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = gr_table->get_functions( ).
*lr_functions->set_default( abap_true ).
  lr_functions->set_all( abap_true ).
  DATA: lr_columns TYPE REF TO cl_salv_columns.

  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  DATA: lr_column TYPE REF TO cl_salv_column.
  DATA: lv_short_text TYPE char10.
  DATA: lv_middle_text TYPE char20.
  DATA: lv_long_text TYPE char40.

  LOOP AT gt_screen WHERE object = g_object AND import_index <> '' AND fieldalv = 'ITEM'.
    IF gt_screen-requi IS NOT INITIAL.
      gt_screen-coltext = gt_screen-coltext && '*'.
    ENDIF.
    lr_column = lr_columns->get_column( EXPORTING columnname = gt_screen-fieldname ).
    lv_short_text = gt_screen-coltext.
    lv_middle_text = gt_screen-coltext.
    lv_long_text = gt_screen-coltext.
    lr_column->set_short_text( lv_short_text ) .
    lr_column->set_medium_text( lv_middle_text ) .
    lr_column->set_long_text( lv_long_text ) .
    lr_column->set_alignment( 3 ) .
    lr_column->set_optimized( 'X' ) .
  ENDLOOP.

  DATA xstring TYPE xstring.
  xstring = gr_table->to_xml( '10' ).

  lv_filename = gs_object-object && gs_object-object_name1 && TEXT-041 && '.XLSX' .


  PERFORM download_xml_to_file USING lv_filename xstring.

ENDFORM.

FORM download_xml_to_file USING default_filename TYPE string
                                content          TYPE xstring.

  DATA:
  l_filename TYPE string.

  PERFORM save_dialog USING default_filename CHANGING l_filename.

  cl_salv_data_services=>download_xml_to_file(
    filename = l_filename
    xcontent = content ).

ENDFORM.                    " download_xml_to_file

FORM save_dialog USING    default_filename TYPE string
                 CHANGING filename         TYPE string.

  DATA:
    l_path     TYPE string,
    l_fullpath TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
*      WINDOW_TITLE
*      DEFAULT_EXTENSION
      default_file_name = default_filename
*      WITH_ENCODING
*      FILE_FILTER
*      INITIAL_DIRECTORY
*      PROMPT_ON_OVERWRITE
    CHANGING
      path              = l_path
      filename          = filename
      fullpath          = l_fullpath ).
*      USER_ACTION
*      FILE_ENCODING).

ENDFORM.


FORM frm_import_data USING p_file TYPE rlgrap-filename i_begin_row TYPE i.

  DATA: lv_msg TYPE char40.
  DATA: itab   TYPE STANDARD TABLE OF zalsmex_tabline WITH HEADER LINE.
  DATA: col_count TYPE i.
  CLEAR G_ERROR.

  FIELD-SYMBOLS:<fs_value> TYPE any.

  DATA:lt_screen TYPE TABLE OF zafo_screen WITH HEADER LINE.

  lt_screen[] = gt_screen[].

  DELETE lt_screen WHERE object <> g_object .
  DELETE lt_screen WHERE fieldalv <> 'ITEM' .
  DELETE lt_screen WHERE import_index = ''.
  SORT lt_screen BY import_index.

  DESCRIBE TABLE lt_screen LINES col_count.


  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = '1'
      i_begin_row             = i_begin_row
      i_end_col               = col_count
      i_end_row               = '9999'
    TABLES
      intern                  = itab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 060  '' '' '' ''."导入出错,请重新导入
    RETURN.

  ENDIF.

  IF itab[] IS NOT INITIAL.
    LOOP AT lt_screen.
      IF lt_screen-requi = 'X'.
        lt_screen-coltext = lt_screen-coltext && '*'.
      ENDIF.
      READ TABLE itab WITH KEY row = 1 col = sy-tabix.
      IF sy-subrc EQ 0 AND itab-value = lt_screen-coltext.
      ELSE.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 061 lt_screen-coltext '' '' ''."导入表头与模板不符
        RETURN.
      ENDIF.
    ENDLOOP.


    REFRESH: gt_item[].
    CLEAR: gt_item.
    DATA:ls_line TYPE zafonr.
    CLEAR ls_line.
    LOOP AT itab.
      IF itab-row = '1'.
        CONTINUE.
      ENDIF.

      TRANSLATE  itab-value TO UPPER CASE.

      READ TABLE lt_screen INDEX itab-col.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lt_screen-fieldname  OF STRUCTURE gt_item TO <fs_value>.
        IF sy-subrc EQ 0.

          IF  lt_screen-fieldname = 'EEIND'.
            PERFORM convert_date CHANGING itab-value.
          ENDIF.
          <fs_value> = itab-value.

        ENDIF.
      ENDIF.

      AT END OF row.
        ADD 1 TO ls_line.
        gt_item-afonr = ls_line.

*        IF gt_item-meins IS NOT INITIAL.

*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*            EXPORTING
*              input          = gt_item-meins
*              language       = sy-langu
*            IMPORTING
*              output         = gt_item-meins
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*          IF sy-subrc <> 0.
** Implement suitable error handling here
*          ENDIF.

*        ENDIF.

*        IF gt_item-eeind IS NOT INITIAL.

*
*        ENDIF.
        IF gt_item-matnr IS NOT INITIAL.
          SELECT SINGLE maktx,meins
            INTO ( @gt_item-maktx,@gt_item-meins )
            FROM zmmv0010 WHERE matnr = @gt_item-matnr.
          IF sy-subrc NE 0.
            PERFORM frm_add_msg USING 'E' 'ZAFO' 062  gt_item-matnr   '' '' ''."物料号&1不存在
          ENDIF.
        ENDIF.


        gt_item-werks = g_werks.

        PERFORM frm_set_price
            IN PROGRAM saplzafo IF FOUND
                  CHANGING gt_item.

        IF gt_item-zmm_tran_rate IS INITIAL.
          gt_item-zmm_tran_rate = 1.
        ENDIF.
        gt_item-amount = gt_item-menge * gt_item-price.
        APPEND gt_item .
        CLEAR gt_item.
      ENDAT.
    ENDLOOP.


  ELSE.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 063  '' '' '' ''."请选择正确模板
    RETURN.
  ENDIF.


ENDFORM.                    " IMPORT_DATA

FORM convert_date CHANGING value.
  DATA:year TYPE char4.
  DATA:month TYPE char2.
  DATA:day TYPE char2.
  DATA:len(3) TYPE i.
  DATA:flag TYPE char1.
  DATA:datum TYPE sy-datum.

  len = strlen( value ).

  IF len > 10.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 064  '' '' '' ''."日期格式错误
    RETURN.
  ENDIF.

  FIND '.' IN value.
  IF sy-subrc EQ 0.
    flag = '.'.
  ENDIF.

  FIND '/' IN value.
  IF sy-subrc EQ 0.
    flag = '/'.
  ENDIF.

  FIND '-' IN value.
  IF sy-subrc EQ 0.
    flag = '-'.
  ENDIF.

  IF flag IS INITIAL .
    IF len <> 8.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 064  '' '' '' ''."日期格式错误
      RETURN.
    ELSE.
      datum = value.
    ENDIF.
  ELSE.

    SPLIT value AT flag INTO year month day.



    DO 4 - strlen( year ) TIMES.
      year = '0' && year.
    ENDDO.

    DO 2 - strlen( month ) TIMES.
      month = '0' && month.
    ENDDO.

    DO 2 - strlen( day ) TIMES.
      day = '0' && day.
    ENDDO.

    value = year && month && day.


    datum = value.
  ENDIF.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = datum
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 064  '' '' '' ''."日期格式错误
  ENDIF.
ENDFORM.
