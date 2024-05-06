class ZAFO_PRINT_EXCEL definition
  public
  final
  create public .

public section.

  types MTY_MAT_INFO type ZAFO_V_MARA .
  types:
    tt_rule_detail TYPE TABLE OF zafo_print_ruled .
  types:
    BEGIN OF ty_range_data ,
        name  TYPE char30, "RANGE_NAME
        value TYPE text100,
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
  data DATA type ZWFT_TT_DATA .
  data BUKRS_BP_INFO type ZAFO_BP_INFO .
  data LIFNR_BP_INFO type ZAFO_BP_INFO .
  data KUNNR_BP_INFO type ZAFO_BP_INFO .
  data WERKS_BP_INFO type ZAFO_BP_INFO .
  data MAT_TT_INFO type ZAFO_TT_MARA .
  data MAT_INFO type MTY_MAT_INFO .

  methods CONSTRUCTOR
    importing
      !RULE_NAME type ZAFO_EPRINT_RULE .
  class-methods DOWNLOAD_EXCEL_BY_DATA
    importing
      !RULE_NAME type ZAFO_EPRINT_RULE
      !I_DATA type ZWFT_TT_DATA optional
      !I_OPEN type ABAP_BOOL optional
      !I_FILENAME type CLIKE optional
    returning
      value(RV_EXCEL) type ref to ZAFO_PRINT_EXCEL .
  class-methods SHOW_EXCEL_BY_DATA
    importing
      !RULE_NAME type ZAFO_EPRINT_RULE
      !I_DATA type ZWFT_TT_DATA
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
  methods GET_MAT_INFO
    importing
      !MATNR type CLIKE
    returning
      value(MAT) type ZAFO_V_MARA .
  class-methods GET_BP_INFO
    importing
      !I_PARTNER type CLIKE
      !I_LANGU type LANGU default '1'
    returning
      value(R_BP_INFO) type ZAFO_BP_INFO .
  methods DOWNLOAD
    importing
      !I_OPEN type ABAP_BOOL optional
      !I_FILENAME type CLIKE optional .
  methods SHOW .
protected section.
private section.

  data RULE type ZAFO_PRINT_RULE .
  data RULE_DETAIL type TT_RULE_DETAIL .
  data DEFAULT_SRC type STRING .

  methods CONV_VALUE_TO_TEXT
    importing
      !I_CONVEXIT type RSBGUI_CONVEXIT optional
      !I_VALUE type ANY
    returning
      value(R_TEXT) type TEXT100 .
  methods PROJECT .
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


  METHOD conv_value_to_text.
    DATA edit_mask TYPE abap_editmask.
    r_text = i_value.
    CONDENSE r_text NO-GAPS.

    IF zwft_common=>number_check( r_text ) = abap_true.
      zwft_common=>number_conv_end_no_zero( CHANGING c_text = r_text ).
    ENDIF.
    CASE i_convexit.
      WHEN 'CNNUM'.
        r_text = zwft_common=>number_to_chinese( i_value ).
      WHEN 'CNDAT'.
        r_text = zwft_common=>date_to_chinese( r_text ).

      WHEN OTHERS.
        IF i_convexit IS NOT INITIAL.
          edit_mask = '==' && i_convexit.
          TRY.
              WRITE r_text USING EDIT MASK edit_mask TO r_text.
            CATCH cx_root INTO DATA(error).
          ENDTRY.
          RETURN.
        ENDIF.

        DATA: elemdescr TYPE REF TO cl_abap_elemdescr.
        elemdescr ?= cl_abap_elemdescr=>describe_by_data( i_value ).
        IF  elemdescr->edit_mask IS NOT INITIAL.
          edit_mask = elemdescr->edit_mask.
          edit_mask = COND #( WHEN edit_mask = '==CUNIT' THEN '==ZMEIN' ELSE edit_mask ).
          TRY.
              WRITE r_text USING EDIT MASK edit_mask TO r_text.
            CATCH cx_root INTO Error.
          ENDTRY.
        ELSEIF elemdescr->type_kind = 'D'."日期
          r_text = |{ CONV datum( r_text ) DATE = ENVIRONMENT }|.
        ELSEIF elemdescr->type_kind = 'T'."时间
          r_text = |{ CONV erzet( r_text ) TIME = ENVIRONMENT }|.
        ELSEIF elemdescr->type_kind = 'N'."数字
          r_text = |{ i_value ALPHA = OUT }|.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD copy_rows.

    CHECK ip_lines > 0.
    worksheet = excel->get_worksheet_by_name( sheet_name )."获取工作表
    worksheet->get_rows( )->copy_rows( EXPORTING ip_index = ip_index ip_lines = ip_lines lo_worksheet = worksheet )."工作表数据赋值行
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
    DATA(lt_merged) = worksheet->mt_merged_cells."这玩意不能修改,只能缓存,删除大于该部分的行,向下偏移
    DELETE worksheet->mt_merged_cells WHERE row_from >= ip_index.
    DO ip_lines + 1 TIMES."插入开头/结尾都是复制的行N-1次
      l_index = sy-index - 1.
      LOOP AT lt_merged INTO DATA(ls_merged) WHERE row_from = ip_index
                                                                                    AND row_to = ip_index.
        ls_merged-row_from += l_index.
        ls_merged-row_to += l_index.
        APPEND ls_merged TO worksheet->mt_merged_cells.
      ENDLOOP.

      IF l_index = 0."拉长开头为复制行,结尾大于的行,只一次
        LOOP AT lt_merged INTO ls_merged WHERE row_from = ip_index
                                                                        AND row_to > ip_index.
          ls_merged-row_to += ip_lines.
          APPEND ls_merged TO worksheet->mt_merged_cells.
        ENDLOOP.
      ENDIF.
    ENDDO.

    LOOP AT lt_merged INTO ls_merged WHERE row_from > ip_index."偏移下面的合并行
      ls_merged-row_from += ip_lines.
      ls_merged-row_to += ip_lines.
      APPEND ls_merged TO worksheet->mt_merged_cells.
    ENDLOOP.

    "下移RANGES表
    DATA(lt_ranges) = worksheet->excel->get_ranges_iterator( ).
    DATA l_range TYPE REF TO zcl_excel_range.
    WHILE lt_ranges->has_next( ) EQ abap_true."迭代所有RANGE,
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
      IF row_start > ip_index."修改后续的RANGE值的坐标,下移
        row_start += ip_lines.
        IF row_end IS NOT INITIAL.
          row_end += ip_lines.
        ENDIF.
      ELSEIF row_start = ip_index.
        IF row_end IS INITIAL.
          row_end += ip_lines.
        ELSE.
          row_end += ip_lines.
        ENDIF.
      ELSEIF row_end > ip_index.
        row_end += ip_lines.
      ENDIF.
      l_range->set_value( ip_sheet_name = sheet_name
                                      ip_start_row = row_start
                                      ip_start_column = column_start
                                      ip_stop_row = row_end
                                      ip_stop_column = column_end )."设置新的RANGE
    ENDWHILE.
  ENDMETHOD.


  METHOD download.
    CHECK excel IS NOT INITIAL.
    DATA(file) = zwft_common=>file_get_save_path( filename = CONV #( I_filename ) ).
    CHECK file IS NOT INITIAL.
    DATA(xstring) = zafo_print_excel=>get_xstring_from_excel( excel ).
    DATA(t_rawdata) = cl_bcs_convert=>xstring_to_solix( iv_xstring  =  xstring ).
    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = xstrlen( xstring )
      filename     = |{ file }|
      filetype     = 'BIN'
    CHANGING data_tab     = t_rawdata
    EXCEPTIONS OTHERS       = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF i_open = 'X'.
      cl_gui_frontend_services=>execute( document = |{ file } | operation = 'OPEN' ).
    ENDIF.
  ENDMETHOD.


  METHOD download_excel_by_data.
    rv_excel = NEW zafo_print_excel( rule_name ).
    rv_excel->data = i_data.
    rv_excel->set_value( ).
    rv_excel->project( ).
    rv_excel->download( i_open = i_open i_filename = i_filename ).
  ENDMETHOD.


  METHOD get_bp_info.
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

    SELECT SINGLE * FROM but0bk
      WHERE partner =  @partner
      AND bkvid = '2000'
      INTO @DATA(ls_but0bk).
    IF sy-subrc EQ 0.
      r_bp_info-bank_num = ls_but0bk-bankn && ls_but0bk-bkref.
      IF ls_but0bk-bankl IS NOT INITIAL.
        SELECT SINGLE banka INTO @r_bp_info-bank_name
          FROM bnka
          WHERE banks = @ls_but0bk-banks
          AND bankl = @ls_but0bk-bankl.
      ENDIF.
    ENDIF.

    SELECT SINGLE stcd5 INTO @r_bp_info-tax_num
      FROM lfa1
      WHERE lifnr = @partner.
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


  METHOD get_mat_info.
    CHECK matnr IS NOT INITIAL.
    READ TABLE mat_tt_info INTO mat WITH KEY matnr = matnr.
    CHECK sy-subrc NE 0.

    SELECT SINGLE * INTO mat FROM zafo_v_mara WHERE matnr = matnr.
    IF sy-subrc EQ 0.
      APPEND mat TO mat_tt_info.
    ENDIF.

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


  METHOD project.
    CHECK rule-password IS NOT INITIAL.

    DATA(worksheets) = excel->get_worksheets_iterator( ).
    WHILE worksheets->has_next( ) EQ abap_true."迭代所有RANGE,
      worksheet ?= worksheets->get_next( ).
      worksheet = excel->get_active_worksheet( ).
      worksheet->zif_excel_sheet_protection~password   = zcl_excel_common=>encrypt_password( rule-password ).
      worksheet->zif_excel_sheet_protection~protected  = zif_excel_sheet_protection=>c_protected.
      worksheet->zif_excel_sheet_protection~sheet      = zif_excel_sheet_protection=>c_active.
      worksheet->zif_excel_sheet_protection~objects    = zif_excel_sheet_protection=>c_active.
      worksheet->zif_excel_sheet_protection~scenarios  = zif_excel_sheet_protection=>c_active.
    ENDWHILE.
  ENDMETHOD.


  METHOD set_ranges_value.
    DATA sheet_name TYPE  zexcel_sheet_title.
    DATA l_range TYPE REF TO zcl_excel_range.
    DATA(lt_ranges)  = excel->get_ranges_iterator( ).

    WHILE lt_ranges->has_next( ) EQ abap_true."迭代EXCEL所有的RANGE
      l_range ?= lt_ranges->get_next( )."迭代
      get_range_info( EXPORTING
                                i_range = l_range
                              IMPORTING
                                e_column_start = DATA(column)
                                e_row_start = DATA(row)
                                e_sheet = sheet_name )."获取该RANGE的信息

      READ TABLE range_data INTO DATA(l_range_data) WITH KEY name = l_range->name."读取赋值
      CHECK sy-subrc EQ 0.
      LOOP AT range_data INTO l_range_data FROM sy-tabix."支持多个VALUE按传入顺序替换
        IF l_range_data-name <> l_range->name.
          EXIT.
        ENDIF.
        worksheet = excel->get_worksheet_by_name( sheet_name )."获取EXCEL的SHEET

*        worksheet->zif_excel_sheet_protection~protected = '1'.
        CHECK worksheet IS BOUND.
        IF l_range_data-table IS INITIAL."处理非标的单值信息
          worksheet->get_cell( EXPORTING ip_column = column ip_row = row IMPORTING ep_value = DATA(value) )."获取模板的原值
          FIND replace_key IN value."查找该值是否有[VALUE],若有则替换,否则覆盖
          IF sy-subrc EQ 0.
            REPLACE replace_key IN value WITH l_range_data-value.
          ELSE.
            value = l_range_data-value.
          ENDIF.
          worksheet->set_cell( ip_column = column ip_row = row ip_value = value )."覆盖模板的值
        ENDIF.

        IF l_range_data-table IS NOT INITIAL.
          set_range_table( range = l_range table = l_range_data-table )."表信息写入.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

  ENDMETHOD.


  METHOD set_range_table.
    FIELD-SYMBOLS:<table> TYPE ANY TABLE.
    ASSIGN table->* TO <table>.
    CHECK sy-subrc EQ 0.


    DATA(lines) = lines( <table> ) ."获取数据的行数
    check lines > 0.

    get_range_info( EXPORTING
                                 i_range = range
                             IMPORTING
                                e_column_start = DATA(column_start)
                                e_column_start_int = DATA(column_start_int)
                                e_column_end = DATA(column_end)
                                e_column_end_int = DATA(column_end_int)
                                e_row_start = DATA(row_start)
                                e_row_end = DATA(row_end)
                                e_sheet = DATA(sheet_name) )."获取表的坐标信息

    copy_rows( ip_index = row_start ip_lines = lines - 1 sheet_name =  sheet_name )."修改表的行数为数据的行数
    DATA(lt_merged) = excel->get_worksheet_by_name( sheet_name )->mt_merged_cells.
    DATA index TYPE sy-index.
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
      clear index.
      DATA(row) = sy-tabix + row_end - 1 ."获取数据的行坐标
      DO column_end_int - column_start_int + 1 TIMES.
        LOOP AT lt_merged TRANSPORTING NO FIELDS WHERE row_from = row AND col_from < sy-index AND col_to >= sy-index.
          EXIT.
        ENDLOOP.
        CHECK sy-subrc NE 0.
        index += 1.

        ASSIGN COMPONENT index OF STRUCTURE <line> TO FIELD-SYMBOL(<value>)."获取第INDEX个目标字段
        CHECK sy-subrc EQ 0.
        DATA(column) = column_start_int + sy-index - 1."获取目标字段的列号
        worksheet->set_cell( ip_column = column ip_row = row ip_value = <value> )."写入目标字段的值

      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_value.
    DATA range_data TYPE tt_range_data.
    DATA dfies_tab TYPE dfies_tab.
    DATA tab TYPE REF TO data.
    DATA default_tab_str TYPE string.

    FIELD-SYMBOLS <l_data> TYPE zwft_ty_data.
    FIELD-SYMBOLS <t_source> TYPE table.
    FIELD-SYMBOLS <l_source> TYPE any.

    FIELD-SYMBOLS: <line>  TYPE any,
                   <value> TYPE any,
                   <table> TYPE STANDARD TABLE.
    IF data IS NOT INITIAL.
      default_src = data[ 1 ]-name.
    ENDIF.

    LOOP AT rule_detail INTO DATA(group_detail)"按EXCEL的字符名称分组
                                     GROUP BY ( excel_name = group_detail-excel_name ).
      IF group_detail-excel_name+0(5) <> 'TABLE'."TABLE开头的EXCEL调用写入表逻辑
        LOOP AT GROUP group_detail INTO DATA(ls_detail).


          READ TABLE data ASSIGNING <l_data> WITH KEY name = ls_detail-from_struc.
          IF sy-subrc NE 0.
            READ TABLE data ASSIGNING <l_data> WITH KEY name = default_src.
            CHECK sy-subrc EQ 0.
          ENDIF.
          ASSIGN <l_data>-data->* TO FIELD-SYMBOL(<data>).
          CASE cl_abap_typedescr=>describe_by_data( <data> )->type_kind.
            WHEN 'u' OR 'v'.
              ASSIGN <l_data>-data->* TO <line>.
            WHEN 'h'.
              ASSIGN <l_data>-data->* TO <t_source>.
              CHECK sy-subrc EQ 0.
              READ TABLE <t_source> INDEX 1 ASSIGNING <line>.
            WHEN OTHERS.
              ASSIGN <l_data>-data->* TO <value>.
              CHECK sy-subrc EQ 0.
              APPEND VALUE #( name = ls_detail-excel_name
                                            value = <value> )"转换例程,转换写入的字段值
                                             TO range_data."插入EXCEL的RANGE
              RETURN.
          ENDCASE.

          IF  ls_detail-from_struc+5(8) = '_BP_INFO'."如果为公司/供应商/客户/工厂的BP信息,则获取BP信息.
            ASSIGN COMPONENT ls_detail-from_struc+0(5) OF STRUCTURE <line> TO <value>."获取抬头结构的公司/供应商/客户/工厂
            CHECK sy-subrc EQ 0.
            DATA(bp_info) = zafo_print_excel=>get_bp_info( <value> ) ."获取公司/供应商/客户/工厂的BP信息
            ASSIGN bp_info TO <l_source>.
          ELSE.
            ASSIGN <line> TO <l_source>.
          ENDIF.
          CHECK <l_source> IS ASSIGNED.
          CHECK <l_source> IS NOT INITIAL.
          ASSIGN COMPONENT ls_detail-from_fname OF STRUCTURE <l_source> TO <value>."获取最终的写入字段值
          CHECK sy-subrc EQ 0.
          APPEND VALUE #( name = ls_detail-excel_name
                                           value = conv_value_to_text( i_convexit = ls_detail-convexit"转换例程,转换写入的字段值
                                                                                        i_value = <value> )
                                        ) TO range_data."插入EXCEL的RANGE
        ENDLOOP.
      ELSE."以下处理表类型
        CLEAR dfies_tab.
        CLEAR default_tab_str.
        LOOP AT GROUP group_detail INTO ls_detail.
          APPEND VALUE #(
                                        rollname = 'TEXT100'
                                        fieldname = ls_detail-from_fname && '_' && ls_detail-afonr
                                        position = sy-tabix
                                    ) TO dfies_tab.
        ENDLOOP."定义表的字段
        zwft_common=>create_table_dfies(  EXPORTING it_dfies = dfies_tab CHANGING ct_data = tab )."定义表
        ASSIGN tab->* TO <table>.
        CHECK sy-subrc EQ 0.

        IF default_tab_str IS INITIAL.
          READ TABLE data ASSIGNING <l_data> WITH KEY name = group_detail-from_struc.
          IF sy-subrc EQ 0.
            default_tab_str = group_detail-from_struc.
          ELSE.
            READ TABLE data ASSIGNING <l_data> WITH KEY name = default_tab_str.
            CHECK sy-subrc EQ 0.
          ENDIF.
          ASSIGN <l_data>-data->* TO <data>.
          CASE cl_abap_typedescr=>describe_by_data( <data> )->type_kind.
            WHEN 'u'.
            WHEN 'h'.
              ASSIGN <l_data>-data->* TO <t_source>.
              CHECK sy-subrc EQ 0.
          ENDCASE.
        ENDIF.

        LOOP AT <t_source> ASSIGNING <l_source>.
          APPEND INITIAL LINE TO <table> ASSIGNING <line>."给表插入行
          LOOP AT GROUP group_detail INTO ls_detail."表字段值循环
            ASSIGN COMPONENT ls_detail-from_fname && '_' && ls_detail-afonr OF STRUCTURE <line> TO <value>."获取表的数据来源值
            CHECK sy-subrc EQ 0.
            IF ls_detail-from_struc IS INITIAL."结构为空则取配置的默认值
              <value> = ls_detail-default_value.
            ELSE.
              IF ls_detail-from_struc = 'MAT_INFO'."若来源值为MAT_INFO,则按照物料获取该物料的行
                ASSIGN COMPONENT 'MATNR' OF STRUCTURE <l_source> TO FIELD-SYMBOL(<matnr>).
                CHECK sy-subrc EQ 0.
                mat_info = get_mat_info( <matnr> ).
              ENDIF.
              ASSIGN (ls_detail-from_struc) TO FIELD-SYMBOL(<from_line>)."获取来源的行结构
              IF sy-subrc NE 0.
                ASSIGN <l_source> TO <from_line>.
              ENDIF.
              ASSIGN COMPONENT ls_detail-from_fname OF STRUCTURE <from_line> TO FIELD-SYMBOL(<from_value>)."获取来源的地段值
              CHECK sy-subrc EQ 0.
              <value> = conv_value_to_text( i_convexit = ls_detail-convexit
                                                                i_value = <from_value>  )."转换并将来源赋值到目标值
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        APPEND VALUE #( name = ls_detail-excel_name
                                       table = tab
                                      ) TO range_data."将表写入EXCEL的RANGE
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
*    DATA(bytecount) = xstrlen( xstring ).
    DATA(t_rawdata) = cl_bcs_convert=>xstring_to_solix( iv_xstring  =  xstring ).

    cl_document->open_document_from_table( EXPORTING document_size    = xstrlen( xstring )
      document_table   = t_rawdata
      open_inplace     = abap_true ).

  ENDMETHOD.


  METHOD show_excel_by_data.
    rv_excel = NEW zafo_print_excel( rule_name ).
    rv_excel->data = i_data.
    rv_excel->set_value( ).
    rv_excel->show( ).
  ENDMETHOD.
ENDCLASS.
