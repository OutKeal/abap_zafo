class ZAFO_PRINT_EXCEL definition
  public
  final
  create public .

public section.

  types:
    tt_rule_detail TYPE TABLE OF zafo_print_ruled .
  types:
    BEGIN OF ty_range_data ,
        name  TYPE char30, "RANGE_NAME
        value TYPE char100,
        table TYPE REF TO data,
      END OF ty_range_data .
  types:
    tt_range_data TYPE TABLE OF ty_range_data .
  types:
    BEGIN OF mty_s_hashed_row,
        row_index TYPE int4,
        row       TYPE REF TO zcl_excel_row,
      END OF mty_s_hashed_row .
  types:
    mty_ts_hashed_row TYPE HASHED TABLE OF mty_s_hashed_row WITH UNIQUE KEY row_index .

  constants CLASSNAME type SBDST_CLASSNAME value 'ZAFO' ##NO_TEXT.
  constants CLASSTYPE type SBDST_CLASSTYPE value 'OT' ##NO_TEXT.
  data RULE_NAME type ZAFO_RULE_NAME .
  data OBJECT_KEY type SBDST_OBJECT_KEY .
  data EXCEL type ref to ZCL_EXCEL .
  data WORKSHEET type ref to ZCL_EXCEL_WORKSHEET .
  constants REPLACE_KEY type CHAR10 value '[VALUE]' ##NO_TEXT.
  data HEAD type ZAFO_SHEAD .
  data T_ITEM type ZAFO_TT_SITEM .
  data BUKRS_BP_INFO type ZAFO_BP_INFO .
  data LIFNR_BP_INFO type ZAFO_BP_INFO .
  data KUNNR_BP_INFO type ZAFO_BP_INFO .
  data WERKS_BP_INFO type ZAFO_BP_INFO .

  methods CONSTRUCTOR
    importing
      !RULE_NAME type ZAFO_EPRINT_RULE .
  class-methods DOWNLOAD_EXCEL_BY_DATA
    importing
      !RULE_NAME type ZAFO_EPRINT_RULE
      !I_HEAD type ZAFO_SHEAD
      !I_ITEM type ZAFO_TT_SITEM
    returning
      value(RV_EXCEL) type ref to ZAFO_PRINT_EXCEL .
  class-methods SHOW_EXCEL_BY_DATA
    importing
      !RULE_NAME type ZAFO_EPRINT_RULE
      !I_HEAD type ANY
      !I_ITEM type TABLE
    returning
      value(RV_EXCEL) type ref to ZAFO_PRINT_EXCEL .
  class-methods GET_XSTRING_FROM_BDS
    importing
      !OBJECT_KEY type SBDST_OBJECT_KEY
    returning
      value(EXCEL_DATA) type XSTRING .
  class-methods GET_EXCEL_FROM_BDS
    importing
      !I_OBJECT_KEY type SBDST_OBJECT_KEY
    returning
      value(R_EXCEL) type ref to ZCL_EXCEL .
  class-methods GET_XSTRING_FROM_EXCEL
    importing
      !I_EXCEL type ref to ZCL_EXCEL
    returning
      value(XSTRING) type XSTRING .
  methods SET_VALUE .
  methods SET_RANGES_VALUE
    importing
      !RANGE_DATA type TT_RANGE_DATA .
  methods SET_RANGE_TABLE
    importing
      !RANGE type ref to ZCL_EXCEL_RANGE
      !TABLE type ref to DATA .
  methods COPY_ROWS
    importing
      value(IP_INDEX) type INT4
      value(IP_LINES) type INT4
      !SHEET_NAME type ZEXCEL_SHEET_TITLE .
  methods GET_RANGE_INFO
    importing
      !I_RANGE type ref to ZCL_EXCEL_RANGE
    exporting
      !E_COLUMN_START type ZEXCEL_CELL_COLUMN_ALPHA
      !E_COLUMN_START_INT type ZEXCEL_CELL_COLUMN
      !E_COLUMN_END type ZEXCEL_CELL_COLUMN_ALPHA
      !E_COLUMN_END_INT type ZEXCEL_CELL_COLUMN
      !E_ROW_START type ZEXCEL_CELL_ROW
      !E_ROW_END type ZEXCEL_CELL_ROW
      !E_SHEET type ZEXCEL_SHEET_TITLE .
  class-methods GET_BP_INFO
    importing
      !I_PARTNER type CLIKE
      !I_LANGU type LANGU default '1'
    returning
      value(R_BP_INFO) type ZAFO_BP_INFO .
  methods DOWNLOAD .
  methods SHOW .
protected section.
private section.

  data RULE type ZAFO_PRINT_RULE .
  data RULE_DETAIL type TT_RULE_DETAIL .
ENDCLASS.



CLASS ZAFO_PRINT_EXCEL IMPLEMENTATION.


  METHOD constructor.

    SELECT SINGLE * FROM zafo_print_rule
      WHERE print_rule = @rule_name
      INTO @rule.
    CHECK sy-subrc EQ 0.

    SELECT  * FROM zafo_print_ruled
    WHERE print_rule = @rule_name
    ORDER BY  afonr
    INTO TABLE @rule_detail.
    CHECK sy-subrc EQ 0.

    object_key = rule-template.
    CHECK object_key IS NOT INITIAL.
    excel = zafo_print_excel=>get_excel_from_bds( i_object_key = object_key ) .

  ENDMETHOD.


  METHOD COPY_ROWS.
    worksheet = excel->get_worksheet_by_name( sheet_name ).
    worksheet->get_rows( )->copy_rows( EXPORTING ip_index = ip_index ip_lines = ip_lines lo_worksheet = worksheet ).
    "下移值数据
    DATA(lt_sheet) = worksheet->sheet_content.
    DELETE worksheet->sheet_content WHERE cell_row > ip_index.
    DO ip_lines TIMES."复制数据行
      DATA(l_index) = sy-index.
      LOOP AT lt_sheet INTO DATA(ls_sheet) WHERE cell_row = ip_index.
        ADD l_index TO ls_sheet-cell_row .
        ls_sheet-cell_coords  = zcl_excel_common=>convert_column_a_row2columnrow( i_column = ls_sheet-cell_column i_row = ls_sheet-cell_row ).
        APPEND ls_sheet TO worksheet->sheet_content.
      ENDLOOP.
    ENDDO.

    LOOP AT lt_sheet INTO ls_sheet WHERE cell_row > ip_index."偏移下面的数据
      ADD ip_lines TO ls_sheet-cell_row .
      ls_sheet-cell_coords  = zcl_excel_common=>convert_column_a_row2columnrow( i_column = ls_sheet-cell_column i_row = ls_sheet-cell_row ).
      APPEND ls_sheet TO worksheet->sheet_content.
    ENDLOOP.


*下移合并行
    DATA(lt_merged) = worksheet->mt_merged_cells.
    DELETE worksheet->mt_merged_cells WHERE row_from > ip_index.
    DO ip_lines TIMES."复制合并
      l_index = sy-index.
      LOOP AT lt_merged INTO DATA(ls_merged) WHERE row_from = ip_index.
        ADD l_index TO ls_merged-row_from .
        ADD l_index TO ls_merged-row_to .
        APPEND ls_merged TO worksheet->mt_merged_cells.
      ENDLOOP.
    ENDDO.

    LOOP AT lt_merged INTO ls_merged WHERE row_from > ip_index."偏移下面的合并
      ADD ip_lines TO ls_merged-row_from .
      ADD ip_lines TO ls_merged-row_to .
      APPEND ls_merged TO worksheet->mt_merged_cells.
    ENDLOOP.


    "下移RANGES
    DATA(lt_ranges) = worksheet->excel->get_ranges_iterator( ).
    DATA l_range TYPE REF TO zcl_excel_range.
    WHILE lt_ranges->has_next( ) EQ abap_true.
      l_range ?= lt_ranges->get_next( ).
      get_range_info( EXPORTING
                                    i_range = l_range
                                IMPORTING
                                    e_column_start = DATA(column_start)
                                   	e_column_end = DATA(column_end)
                                    e_row_start = DATA(row_start)
                                    e_row_end = DATA(row_end)
                                    e_sheet = DATA(name) ).
      CHECK sheet_name = name.
      IF row_start > ip_index.
        ADD ip_lines TO row_start.
        IF row_end IS NOT INITIAL.
          ADD ip_lines TO row_end.
        ENDIF.
      ELSEIF row_start = ip_index.
        IF row_end IS INITIAL.
          row_end = ip_index + ip_lines.
        ELSE.
          ADD ip_lines TO row_end.
        ENDIF.
      ELSEIF row_end > ip_index.
        ADD ip_lines TO row_end.
      ENDIF.
      l_range->set_value( ip_sheet_name = sheet_name
                                      ip_start_row = row_start
                                      ip_start_column = column_start
                                      ip_stop_row = row_end
                                      ip_stop_column = column_end ).
    ENDWHILE.
  ENDMETHOD.


  METHOD download.
    CHECK excel IS NOT INITIAL.
    DATA(fieldname) = zwft_common=>file_get_save_path( ).
    CHECK fieldname IS NOT INITIAL.
    DATA(xstring) = zafo_print_excel=>get_xstring_from_excel( excel ).
    DATA(bytecount) = xstrlen( xstring ).
    DATA(t_rawdata) = cl_bcs_convert=>xstring_to_solix( iv_xstring  =  xstring ).
    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
      filename     = |{ fieldname }|
      filetype     = 'BIN'
    CHANGING data_tab     = t_rawdata
    EXCEPTIONS OTHERS       = 1 ).
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.


  METHOD download_excel_by_data.
    rv_excel = NEW zafo_print_excel( rule_name ).
    rv_excel->head = i_head.
    rv_excel->t_item = i_item.
    rv_excel->set_value( ).
    rv_excel->download( ).
  ENDMETHOD.


  METHOD GET_BP_INFO.
    DATA partner TYPE bu_partner.

    partner = |{ i_partner ALPHA = IN }|.

    r_bp_info-partner = |{ partner ALPHA = OUT }|.

    SELECT SINGLE partner INTO @partner
    FROM but000 WHERE partner = @partner.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE addrnumber INTO @DATA(addrnumber)
          FROM but020 WHERE partner = @partner.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * INTO @DATA(adrc)
          FROM adrc WHERE addrnumber = @addrnumber.

    r_bp_info-name = adrc-name1.
    r_bp_info-tel = adrc-tel_number.
    r_bp_info-fax = adrc-fax_number.
    r_bp_info-name_co = adrc-name_co.
    r_bp_info-address = adrc-street.
    IF adrc-country IS NOT INITIAL.
      SELECT SINGLE landx INTO r_bp_info-country
      FROM t005t WHERE spras = i_langu
      AND land1 = r_bp_info-country.
    ENDIF.

    IF adrc-region IS NOT INITIAL.
      SELECT SINGLE bezei INTO r_bp_info-bland
      FROM t005u WHERE spras = i_langu
      AND land1 = r_bp_info-country
      AND bland = adrc-region.
    ENDIF.
  ENDMETHOD.


  METHOD get_excel_from_bds.
    DATA reader TYPE REF TO zif_excel_reader.
    CREATE OBJECT reader TYPE zcl_excel_reader_2007.
    DATA(xstring) = zafo_print_excel=>get_xstring_from_bds( object_key = i_object_key ) .
    TRY.
        r_excel = reader->load( i_excel2007 = xstring ) .
      CATCH zcx_excel.
    ENDTRY.
  ENDMETHOD.


  METHOD GET_RANGE_INFO.
    DATA(value) = i_range->get_value( ).
    zcl_excel_common=>convert_range2column_a_row( EXPORTING i_range = value
                                                                                        IMPORTING
                                                                                                           e_column_start    = e_column_start
                                                                                                           e_column_start_int    =   e_column_start_int
                                                                                                           e_column_end    =   e_column_end
                                                                                                           e_column_end_int    =     e_column_end_int
                                                                                                           e_row_start   =     e_row_start
                                                                                                           e_row_end     =     e_row_end
                                                                                                           e_sheet       =     e_sheet
                                                                                                    ).

  ENDMETHOD.


  METHOD get_xstring_from_bds.
    DATA: doc_components TYPE sbdst_components,
          content        TYPE sbdst_content.
    DATA lv_filesize     TYPE i.

    cl_bds_document_set=>get_with_table(
    EXPORTING
      classname  = zafo_print_excel=>classname
    classtype  = zafo_print_excel=>classtype
      object_key = object_key
    CHANGING
      content    = content
      components = doc_components
      EXCEPTIONS
      error_kpro = 1
      internal_error = 2
      nothing_found = 3
      no_content = 4
      parameter_error = 5
      not_authorized = 6
      not_allowed = 7
   ).
    CHECK sy-subrc EQ 0.

    READ TABLE doc_components INTO DATA(l_components) INDEX 1.
    CHECK sy-subrc EQ 0.
    lv_filesize = l_components-comp_size.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filesize
*       FIRST_LINE   = 0
*       LAST_LINE    = 0
      IMPORTING
        buffer       = excel_data
      TABLES
        binary_tab   = content
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

  ENDMETHOD.


  METHOD get_xstring_from_excel.
    DATA:cl_writer TYPE REF TO zif_excel_writer.
    CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
    xstring = cl_writer->write_file( i_excel ).
  ENDMETHOD.


  METHOD SET_RANGES_VALUE.
    DATA sheet_name TYPE  zexcel_sheet_title.
    DATA l_range TYPE REF TO zcl_excel_range.
    DATA(lt_ranges)  = excel->get_ranges_iterator( ).

    WHILE lt_ranges->has_next( ) EQ abap_true.
      l_range ?= lt_ranges->get_next( ).
      get_range_info( EXPORTING
                                i_range = l_range
                              IMPORTING
                                e_column_start = DATA(column)
                                e_row_start = DATA(row)
                                e_sheet = sheet_name ).

      READ TABLE range_data INTO DATA(l_range_data) WITH KEY name = l_range->name.
      CHECK sy-subrc EQ 0.
      LOOP AT range_data INTO l_range_data FROM sy-tabix."支持多个VALUE按传入顺序替换
        IF l_range_data-name <> l_range->name.
          EXIT.
        ENDIF.
        worksheet = excel->get_worksheet_by_name( sheet_name ).
        CHECK worksheet IS BOUND.
        IF l_range_data-table IS INITIAL.
          worksheet->get_cell( EXPORTING ip_column = column ip_row = row IMPORTING ep_value = DATA(value) ).
          FIND replace_key IN value.
          IF sy-subrc EQ 0.
            REPLACE replace_key IN value WITH l_range_data-value.
          ELSE.
            value = l_range_data-value.
          ENDIF.
          worksheet->set_cell( ip_column = column ip_row = row ip_value = value ).
        ELSE.
          set_range_table( range = l_range table = l_range_data-table ).
        ENDIF.
      ENDLOOP.
    ENDWHILE.

  ENDMETHOD.


  METHOD SET_RANGE_TABLE.
    FIELD-SYMBOLS:<table> TYPE ANY TABLE.
    ASSIGN table->* TO <table>.
    CHECK sy-subrc EQ 0.

    DATA(lines) = lines( <table> ) .

    get_range_info( EXPORTING
                                 i_range = range
                             IMPORTING
                                e_column_start = DATA(column_start)
                                e_column_start_int = DATA(column_start_int)
                                e_column_end = DATA(column_end)
                                e_column_end_int = DATA(column_end_int)
                                e_row_start = DATA(row_start)
                                e_row_end = DATA(row_end)
                                e_sheet = DATA(sheet_name) ).

    copy_rows( ip_index = row_start ip_lines = lines - 1 sheet_name =  sheet_name ).

    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
      DATA(row) = sy-tabix + row_end - 1 .
      DO column_end_int - column_start_int + 1 TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
        CHECK sy-subrc EQ 0.
        DATA(column) = column_start_int + sy-index - 1.
        worksheet->set_cell( ip_column = column ip_row = row ip_value = <value> ).
      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_value.
    DATA range_data TYPE tt_range_data.
    DATA dfies_tab TYPE dfies_tab.
    DATA tab TYPE REF TO data.
    FIELD-SYMBOLS: <line>  TYPE any,
                   <value> TYPE any,
                   <table> TYPE STANDARD TABLE.
    READ TABLE t_item INDEX 1 INTO DATA(item).
    LOOP AT rule_detail INTO DATA(group_detail)
                                     GROUP BY ( excel_name =  group_detail-excel_name ).
      IF group_detail-excel_name+0(5) <> 'TABLE'.
        LOOP AT GROUP group_detail INTO DATA(ls_detail).
          ASSIGN (ls_detail-from_struc) TO <line>.
          CHECK sy-subrc EQ 0.
          IF <line> IS INITIAL AND ls_detail-from_struc+5(8) = '_BP_INFO'.
            ASSIGN COMPONENT ls_detail-from_struc+0(5) OF STRUCTURE head TO <value>.
            CHECK sy-subrc EQ 0.
            <line> = zafo_print_excel=>get_bp_info( <value> ).
          ENDIF.
          CHECK <line> IS NOT INITIAL.
          ASSIGN COMPONENT ls_detail-from_fname OF STRUCTURE <line> TO <value>.
          CHECK sy-subrc EQ 0.
          APPEND VALUE #( name = ls_detail-excel_name
                                           value = <value>
                                        ) TO range_data.
        ENDLOOP.
      ELSE.
        CLEAR dfies_tab.
        LOOP AT GROUP group_detail INTO ls_detail.
          APPEND VALUE #( tabname = COND #( WHEN ls_detail-from_struc = 'HAED' THEN 'ZAFO_SHEAD'
                                                                           WHEN ls_detail-from_struc = 'ITEM' THEN 'ZAFO_SITEM'
                                                                           WHEN ls_detail-from_struc = '' THEN 'ZAFO_PRINT_I_DATA'
                                                                           ELSE ls_detail-from_struc )
                                        fieldname = ls_detail-from_fname
                                        position = sy-tabix
                                    ) TO dfies_tab.
        ENDLOOP.
        zwft_common=>create_table_dfies(  EXPORTING it_dfies = dfies_tab CHANGING ct_data = tab ).
        ASSIGN tab->* TO <table>.
        CHECK sy-subrc EQ 0.
*        MOVE-CORRESPONDING t_item TO <table>.
        LOOP AT t_item INTO item.
          APPEND INITIAL LINE TO <table> ASSIGNING <line>.
          LOOP AT GROUP group_detail INTO ls_detail.
            ASSIGN COMPONENT ls_detail-from_fname OF STRUCTURE <line> TO <value>.
            CHECK sy-subrc EQ 0.
            IF ls_detail-from_struc IS INITIAL.
              <value> = ls_detail-default_value.
            ELSE.
              ASSIGN (ls_detail-from_struc) TO FIELD-SYMBOL(<from_line>).
              CHECK sy-subrc EQ 0.
              ASSIGN COMPONENT ls_detail-from_fname OF STRUCTURE <from_line> TO FIELD-SYMBOL(<from_value>).
              CHECK sy-subrc EQ 0.
              <value> = <from_value>.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        APPEND VALUE #( name = ls_detail-excel_name
                                       table = tab
                                      ) TO range_data.
      ENDIF.
    ENDLOOP.

    IF range_data IS NOT INITIAL.
      set_ranges_value( range_data ).
    ENDIF.

  ENDMETHOD.


  METHOD SHOW.
    DATA:error       TYPE REF TO i_oi_error,
         t_errors    TYPE STANDARD TABLE OF REF TO i_oi_error WITH NON-UNIQUE DEFAULT KEY,
         cl_control  TYPE REF TO i_oi_container_control, "OIContainerCtrl
         cl_document TYPE REF TO i_oi_document_proxy.   "Office Dokument
    c_oi_container_control_creator=>get_container_control( IMPORTING control = cl_control
                                                               error   = error ).
    APPEND error TO t_errors.

    cl_control->init_control( EXPORTING  inplace_enabled     = abap_true
      no_flush            = abap_true
      r3_application_name = '打印预览'
      parent              = cl_gui_container=>screen5
    IMPORTING  error               = error
    EXCEPTIONS OTHERS              = 2 ).
    APPEND error TO t_errors.

    cl_control->get_document_proxy( EXPORTING document_type  = 'Excel.Sheet'                " EXCEL
      no_flush       = ' '
    IMPORTING document_proxy = cl_document
      error          = error ).
    APPEND error TO t_errors.
*    errorhandling should be inserted here

    DATA(xstring) = zafo_print_excel=>get_xstring_from_excel( excel ).
    DATA(bytecount) = xstrlen( xstring ).
    DATA(t_rawdata) = cl_bcs_convert=>xstring_to_solix( iv_xstring  =  xstring ).

    cl_document->open_document_from_table( EXPORTING document_size    = bytecount
      document_table   = t_rawdata
      open_inplace     = abap_true ).

  ENDMETHOD.


  METHOD SHOW_EXCEL_BY_DATA.
    rv_excel = NEW zafo_print_excel( rule_name ).
    rv_excel->head = i_head.
    rv_excel->t_item = i_item.
    rv_excel->set_value( ).
    rv_excel->show( ).
  ENDMETHOD.
ENDCLASS.
