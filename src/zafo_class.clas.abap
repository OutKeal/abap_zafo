class ZAFO_CLASS definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_item_flo ,
        afono         TYPE zafono,
        afonr         TYPE zafonr,
        afono_ref_flo TYPE zafono,
        afonr_ref_flo TYPE zafonr,
        item_status   TYPE zafo_status,
      END OF ty_item_flo .
  types:
    tt_item TYPE TABLE OF zafo_item .
  types:
    BEGIN OF ty_abap_componentdescr,   "用于生成动态内表
        name       TYPE string,
        type       TYPE REF TO cl_abap_datadescr,
        as_include TYPE abap_bool,
        suffix     TYPE string,
      END OF ty_abap_componentdescr .
  types:
    tt_sitem TYPE TABLE OF zafo_sitem .

  data WERKS type WERKS_D .
  data BUSTYPE type ZAFO_BUSTYPE read-only .
  data BUSTYPE_REF type ZAFO_BUSTYPE read-only .
  data OBJECT type ZAFO_OBJECT read-only .
  data SCREEN_CON type ZAFO_TT_SCREEN read-only .
  data SCREEN_REF type ZAFO_TT_SCREEN read-only .
  data DICT type ZAFO_TT_DICT .
  data ITEM_REF type ZAFO_TT_SITEM .
  data ITEM type ZAFO_TT_SITEM .
  data HEAD type ZAFO_SHEAD .
  data HEAD_DB type ZAFO_HEAD .
  data ITEM_DB type TT_ITEM .
  data FALV_REF type ref to ZWFT_FALV .
  data FALV_ITEM type ref to ZWFT_FALV .
  data FALV_DIS_HEAD type ref to ZWFT_FALV .
  data FALV_DIS_ITEM type ref to ZWFT_FALV .
  data LOCKED type ABAP_BOOL .
  data ACTION type CHAR10 .
  data READONLY type ABAP_BOOL .
  data EDITOR type ref to CL_GUI_TEXTEDIT .
  data TEXT_LINES type ZAFO_TT_LONG_TEXT .
  data MESSAGE type ref to ZAFO_MESSAGE .
  data GOS_MANAGER type ref to CL_GOS_MANAGER .
  data GUI_STATUS type TT_FCODES .
  data ACT type ZAFO_TT_OBJECT_ACT .
  data ITEM_ALL type ZAFO_TT_SITEM .
  data HEAD_DIS type ZAFO_TT_SHEAD .
  data ITEM_DIS type ZAFO_TT_SITEM .
  data NO_COMMIT type ABAP_BOOL .
  data CALLED type ABAP_BOOL .
  data PRINT_RULE type ZAFO_TT_PRINT_RULE .
  data DYNAMIC_SCREEN type ref to ZWFT_DYNAMIC_SCREEN .

  methods CONSTRUCTOR
    importing
      value(BUSTYP) type ZAFO_BUSTYP
      value(WERKS) type WERKS_D optional .
  class-methods CREATE_BY_REF
    importing
      value(BUSTYP) type ZAFO_BUSTYP
      value(ITEM) type ZAFO_TT_SITEM
    returning
      value(R_CLASS) type ref to ZAFO_CLASS .
  class-methods CREATE
    importing
      value(WERKS) type WERKS_D
      value(BUSTYP) type ZAFO_BUSTYP
      value(ITEM) type ZAFO_TT_SITEM
    returning
      value(R_CLASS) type ref to ZAFO_CLASS .
  class-methods MAINTAIN
    importing
      !AFONO type ZAFONO
      !BUSTYP type ZAFO_BUSTYP optional
    returning
      value(R_CLASS) type ref to ZAFO_CLASS .
  class-methods REPORT
    importing
      !BUSTYP type ZAFO_BUSTYPE-BUSTYP
      !HEAD type ZAFO_TT_SHEAD
      !ITEM type ZAFO_TT_SITEM
    returning
      value(R_CLASS) type ref to ZAFO_CLASS .
  class-methods CREATE_BY_DATA
    importing
      !I_BUSTYP type ZAFO_BUSTYP
      !I_WERKS type WERKS_D
      !IS_HEAD type ZAFO_BAPI_HEAD
      !IT_ITEM type ZAFO_TT_BAPI_ITEM
    returning
      value(R_CLASS) type ref to ZAFO_CLASS .
  class-methods APPROVAL
    importing
      !AFONO type ZAFONO
      !STATUS type ZAFO_STATUS
    returning
      value(ERROR) type BOOL .
  methods INIT_ALV
    importing
      value(C_FALV) type ref to ZWFT_FALV optional .
  methods SET_READONLY
    importing
      value(I_READONLY) type ABAP_BOOL default ABAP_FALSE .
  methods LOCK .
  methods UNLOCK .
  methods DATA_CHANGED_FINISHED
    importing
      !C_FALV type ref to ZWFT_FALV
      !IT_GOOD_CELLS type LVC_T_MODI .
  methods DATA_CHANGED
    importing
      value(C_FALV) type ref to ZWFT_FALV
      value(CL_DATA_CHANGED) type ref to CL_ALV_CHANGED_DATA_PROTOCOL .
  methods DOUBLE_CLICK
    importing
      value(C_FALV) type ref to ZWFT_FALV
      value(I_ROW) type LVC_S_ROW
      value(I_COLUMN) type LVC_S_COL .
  methods HOTSPOT_CLICK
    importing
      value(C_FALV) type ref to ZWFT_FALV
      value(I_ROW) type LVC_S_ROW
      value(I_COLUMN) type LVC_S_COL .
  methods TOOLBAR
    importing
      value(C_FALV) type ref to ZWFT_FALV
      value(I_OBJECT) type ref to CL_ALV_EVENT_TOOLBAR_SET .
  methods ONF4
    importing
      !C_FALV type ref to ZWFT_FALV
      !C_EVENT_DATA type ref to CL_ALV_EVENT_DATA
      !I_FIELDNAME type FIELDNAME
      !I_INDEX type INDEX
      !I_DISPLAY type ABAP_BOOL optional .
  methods USER_COMMAND_REF
    importing
      !C_FALV type ref to ZWFT_FALV
      !I_UCOMM type SY-UCOMM .
  methods USER_COMMAND_ITEM
    importing
      !I_UCOMM type SY-UCOMM .
  methods USER_COMMAND_REPORT
    importing
      !I_UCOMM type SY-UCOMM .
  methods USER_COMMAND_MAIN
    importing
      value(FCODE) type SY-UCOMM optional .
  methods FREE .
  methods SET_SCREEN_TEXT
    returning
      value(TEXT) type ref to DATA .
  methods SET_MATNR_INFO .
  methods SET_ACTION
    importing
      value(I_ACTION) type CHAR10 optional .
  methods HOLD_SOFT_REFRESH
    importing
      value(C_FALV) type ref to ZWFT_FALV .
  methods SET_ICON
    importing
      value(STATUS) type ZAFO_STATUS optional
    exporting
      value(ICON) type ICON_D
      value(TEXT) type ZAFO_TEXT .
  methods INIT_HEAD_TEXT .
  methods SET_GUI_STATUS_EXCLUDE
    returning
      value(FCODES) type TT_FCODES .
  methods EDITER_READ_TEXT .
  methods EDITER_SAVE_TEXT .
  methods EDITER_EDITOR_TEXT .
  methods GET_FALV_HANDLE
    importing
      !REPID type REPID
      !ALV_NAME type OBJECTNAME
    returning
      value(R_HANDLE) type SLIS_HANDL .
  methods SET_DYNAMIC_SCREEN .
  methods VALUE_FILTER_BY_DICT
    importing
      !FIELDNAME type CLIKE
    changing
      !TABLE type TABLE .
protected section.
private section.

  methods GET_NEXT_AFONR
    returning
      value(NEXT_AFONR) type ZAFONR .
  methods SET_REF_STATUS .
  methods SET_MATERIAL
    changing
      !CS_ITEM type ZAFO_SITEM .
  methods SET_PRICE
    changing
      !CS_ITEM type ZAFO_SITEM .
  methods SET_AMOUNT
    changing
      !CS_ITEM type ZAFO_SITEM .
  methods HEAD_DOUBLE_CLICK .
  methods CHECK_HEAD .
  methods CHECK_ITEM .
  methods SAVE .
  methods POST .
  methods CANCEL .
  methods PRINT .
  methods DELETE .
  methods COMMIT .
  methods UNCOMMIT .
  methods UPDATE_HEAD_STATUS
    importing
      !STATUS type ZAFO_HEAD-STATUS .
  methods SET_STATUS
    importing
      !STATUS type ZAFO_HEAD-STATUS .
  methods DB_SAVE .
  methods PERPARE_SAVE .
  methods SAVE_DATA .
  methods SELECT_UNIQUE_REF
    importing
      !AFONO type ZAFONO
      !SELECTED type CHAR1 .
  methods REGISTER_F4
    importing
      !C_FALV type ref to ZWFT_FALV .
  methods ADD_GOS_RELATIONSHIP .
  methods GET_NEXT_AFONO
    returning
      value(R_AFONO) type ZAFONO .
  methods REGISTER_EDIT_EVENT
    importing
      value(C_FALV) type ref to ZWFT_FALV .
  methods INIT_HEAD .
  methods INIT_ITEM .
  methods INIT_ITEM_LINE
    importing
      value(INDEX) type ANY
    changing
      value(C_ITEM) type ZAFO_SITEM .
  methods SET_FCAT_FROM_SCREEN
    importing
      value(IT_SCREEN) type ZAFO_TT_SCREEN
    changing
      value(FCAT) type LVC_T_FCAT .
  methods SET_FALV_LAYOUT
    changing
      !FALV type ref to ZWFT_FALV .
  methods EXCLUDE_FUNCTION
    importing
      value(C_FALV) type ref to ZWFT_FALV
      value(FUNCTIONS) type UI_FUNCTIONS optional .
  methods SURE_REF .
  methods ITEM_COPY_TO_HEAD .
  methods HEAD_COPY_TO_ITEM .
  methods SET_FALV_INPUT
    importing
      !FALV type ref to ZWFT_FALV .
  methods GOS_CALL .
  methods ACT_RUN
    importing
      !FCODE type SY-UCOMM .
ENDCLASS.



CLASS ZAFO_CLASS IMPLEMENTATION.


  METHOD CHECK_HEAD.
    head-werks = werks.
    DATA(screen_h) = zafo_basic=>get_bustyp_screen( bustyp = bustype-bustyp fieldalv = 'HEAD' ).
    DELETE screen_h WHERE rollname = '' AND requi = ''.
    CHECK screen_h IS NOT INITIAL.
    DATA rule TYPE TABLE OF zwft_check_rule .
    DATA ret TYPE TABLE OF bapiret2 .


    rule = CORRESPONDING #( screen_h
                                                 MAPPING fieldname = fieldname
                                                                  rollname = rollname
                                                                  notnull = requi
                                                                  ddtext = coltext
                                                  ).
    CALL FUNCTION 'ZWFT_CHECK_VALUE'
      EXPORTING
        line = head
      TABLES
        rule = rule
        ret  = ret.

    message->add_table( ret ).

  ENDMETHOD.


  METHOD CHECK_ITEM.
    IF item IS INITIAL.
      message->add_single( msgty = 'E' msgid = 'ZAFO' msgno = '007' )."项目不能为空
      init_item( ).
      RETURN.
    ENDIF.

    DATA(screen_i) = zafo_basic=>get_bustyp_screen( bustyp = bustype-bustyp fieldalv = 'ITEM' ).
    DELETE screen_i WHERE rollname = '' AND requi = ''.
    CHECK screen_i IS NOT INITIAL.
    DATA rule TYPE TABLE OF zwft_check_rule .
    DATA ret TYPE TABLE OF bapiret2 .


    rule = CORRESPONDING #( screen_i
    MAPPING fieldname = fieldname
    rollname = rollname
    notnull = requi
    ddtext = coltext
    ).
    CALL FUNCTION 'ZWFT_CHECK_VALUE'
      TABLES
        tab  = item
        rule = rule
        ret  = ret.

    message->add_table( ret ).
  ENDMETHOD.


  METHOD COMMIT.

    INCLUDE zafo_class_macro.

    CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '02' bustyp = head-bustyp werks = werks ).
    CHECK zwft_common=>confirm( '确认提交么' ) = abap_true.
    CHECK head-status = 'A'.
    set_action( 'COMMIT' ).
    act_run( '&COMMIT' ).

    macro_error_return.

    IF object-app_object IS INITIAL.
      update_head_status( 'C' ).
    ELSE.
      update_head_status( 'B' ).
    ENDIF.
    set_action( 'DISPLAY' ).
  ENDMETHOD.


  METHOD constructor.
    me->bustype = zafo_basic=>get_bustyp( bustyp )."获取业务类型配置
    me->object = zafo_basic=>get_object( me->bustype-object )."获取对象类型配置
    me->werks = werks .
    bustype_ref = COND #( WHEN bustype-bustyp_ref IS INITIAL THEN bustype
    ELSE zafo_basic=>get_bustyp( bustype-bustyp_ref ) ).
    message = NEW zafo_message( ).
    IF me->bustype IS INITIAL OR me->object IS INITIAL.
      message->add_single( msgid = 'ZAFO' msgty = 'E' msgno = '012' ).
      RETURN.
    ENDIF.
    screen_con = zafo_basic=>get_bustyp_screen( bustyp )."获取屏幕控制
    screen_ref = zafo_basic=>get_bustyp_screen( bustype_ref-bustyp )."获取参考屏幕控制
    act = zafo_basic=>get_bustyp_act( bustype-bustyp )."获取动作控制
    gui_status = zafo_basic=>init_gui_status( )."初始化默认GUI按钮清单
    dict = zafo_basic=>get_bustyp_dict( EXPORTING BUSTYP = bustyp werks = werks )."获取字典配置
    print_rule = zafo_basic=>get_bustyp_print( bustyp )."获取打印规则配置
  ENDMETHOD.


  METHOD create.

    r_class = NEW zafo_class( bustyp = bustyp ).
    r_class->werks = werks.

    r_class->init_head( ).
    r_class->init_head_text( ).
    r_class->init_item( ).
    r_class->set_action( 'CREATE' ).

    CALL FUNCTION 'ZAFO_UI'
      EXPORTING
        i_class = r_class.

  ENDMETHOD.


  METHOD create_by_ref.
    r_class = NEW zafo_class( bustyp = bustyp ).

    r_class->item_ref = item.
    r_class->set_action( 'REF' ).
    CALL FUNCTION 'ZAFO_SELECT'
      EXPORTING
        i_class = r_class.

    CHECK r_class IS BOUND.
    r_class->init_head( ).
    r_class->init_head_text( ).
    r_class->set_action( 'CREATE' ).
    CALL FUNCTION 'ZAFO_UI'
      EXPORTING
        i_class = r_class.

  ENDMETHOD.


  METHOD data_changed.

    FIELD-SYMBOLS: <outtab> TYPE zafo_tt_sitem.
    ASSIGN c_falv->outtab->* TO <outtab>.

    LOOP AT cl_data_changed->mt_good_cells INTO DATA(ls_modi).
      READ TABLE <outtab> ASSIGNING FIELD-SYMBOL(<line>) INDEX ls_modi-row_id.
      CHECK sy-subrc EQ 0.
      IF ls_modi-fieldname = 'SELECTED'.
        IF bustype-busref = 'Y'.
          select_unique_ref( EXPORTING afono = <line>-afono selected = <line>-selected ).
        ELSE.
          <line>-row_color = COND #( WHEN <line>-selected = 'X' THEN 'C500' ELSE '' ).
        ENDIF.
        cl_data_changed->modify_cell( i_row_id = ls_modi-row_id
                                                        i_fieldname = ls_modi-fieldname
                                                        i_value = <line>-matnr ).
      ENDIF.
      IF ls_modi-fieldname = 'MATNR'.
        <line>-matnr = ls_modi-value.
        set_material( CHANGING cs_item = <line> ).
*        MODIFY_CELL( ).

        cl_data_changed->modify_cell( i_row_id = ls_modi-row_id
                                                        i_fieldname = ls_modi-fieldname
                                                        i_value = <line>-matnr ).
        cl_data_changed->modify_cell( i_row_id = ls_modi-row_id
                                                          i_fieldname = 'MAKTX'
                                                          i_value = <line>-maktx ).
        cl_data_changed->modify_cell( i_row_id = ls_modi-row_id
                                                          i_fieldname = 'MEINS'
                                                          i_value = <line>-meins ).
      ENDIF.

      READ TABLE screen_con INTO DATA(l_screen)  WITH KEY fieldname = ls_modi-fieldname.
      IF sy-subrc EQ 0 AND l_screen-check_menge IS NOT INITIAL.
        ASSIGN COMPONENT l_screen-check_menge OF STRUCTURE <line> TO FIELD-SYMBOL(<check_value>).
        CHECK sy-subrc EQ 0.
        IF ls_modi-value > <check_value> AND ls_modi-value IS NOT INITIAL.
          cl_data_changed->add_protocol_entry(  EXPORTING
                                                                  i_msgid     = 'ZAFO'
                                                                  i_msgty     = 'W'
                                                                  i_msgno     = '013'
                                                                  i_fieldname = ls_modi-fieldname
                                                                  i_row_id    = ls_modi-row_id ).
          cl_data_changed->modify_cell( i_row_id = ls_modi-row_id
                                                                    i_fieldname = ls_modi-fieldname
                                                                    i_value = 0 ).
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF cl_data_changed->mt_protocol IS NOT INITIAL.
      cl_data_changed->display_protocol( ).
    ENDIF.
  ENDMETHOD.


  METHOD data_changed_finished.
    FIELD-SYMBOLS: <outtab> TYPE zafo_tt_sitem.
    ASSIGN c_falv->outtab->* TO <outtab>.

    LOOP AT it_good_cells INTO DATA(ls_cell).
      READ TABLE <outtab> ASSIGNING FIELD-SYMBOL(<line>) INDEX ls_cell-row_id.
      CHECK sy-subrc EQ 0.
      ASSIGN COMPONENT  ls_cell-fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<fs_value>).
      CASE ls_cell-fieldname.
        WHEN 'SELECTED'.
          IF bustype-busref = 'Y'.
            select_unique_ref( EXPORTING afono = <line>-afono selected = <line>-selected ).
          ELSE.
            <line>-row_color = COND #( WHEN <line>-selected = 'X' THEN 'C500' ELSE '' ).
          ENDIF.
        WHEN 'MENGE_PLAN' OR 'MENGE'.
          IF c_falv = falv_ref .
            <line>-selected = COND char1( WHEN <fs_value> IS INITIAL THEN '' ELSE 'X'  ).
            <line>-row_color = COND #( WHEN <line>-selected = 'X' THEN 'C500' ELSE '' ).
          ENDIF.
          set_amount( CHANGING cs_item = <line> ).
        WHEN 'MATNR'.
          set_material( CHANGING cs_item = <line> ).
        WHEN 'PRICE_LONG'.
          set_price( CHANGING cs_item = <line> ).
        WHEN 'PRICE'.
          set_amount( CHANGING cs_item = <line> ).
      ENDCASE.
      IF c_falv = falv_item.
        <line>-icon = icon_led_yellow.
        <line>-text = TEXT-018."'已维护'.
      ENDIF.
    ENDLOOP.
    IF sy-subrc EQ 0.
      c_falv->soft_refresh( ).
    ENDIF.
  ENDMETHOD.


  METHOD db_save.

    IF head_db IS NOT INITIAL.
      MODIFY zafo_head FROM head_db.
      CLEAR head_db.
    ENDIF.

    IF item_db IS NOT INITIAL.
      MODIFY zafo_item FROM TABLE item_db.
      CLEAR item_db.
    ENDIF.

*    IF gs_head_modify_ref IS NOT INITIAL.
*      MODIFY zafo_head FROM gs_head_modify_ref.
*      CLEAR gs_head_modify_ref.
*    ENDIF.

    IF no_commit IS INITIAL.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD delete.
    CHECK zwft_common=>confirm( '是否确认删除?' ) = abap_true.
    set_action( 'DELETE' ).
    ACT_RUN( '&DELETE' ).

    macro_error_return.

    update_head_status( 'D' ).
    set_ref_status( ).
  ENDMETHOD.


  METHOD double_click.
    FIELD-SYMBOLS: <outtab_head> TYPE zafo_tt_shead.
    FIELD-SYMBOLS: <outtab_item> TYPE zafo_tt_sitem.
    IF c_falv = falv_dis_head.
      ASSIGN c_falv->outtab->* TO <outtab_head>.
      READ TABLE <outtab_head> INDEX i_row-index  ASSIGNING FIELD-SYMBOL(<line_head>).
      CHECK sy-subrc EQ 0.
      CHECK zwft_common=>call_transation_by_line( EXPORTING line = <line_head>"字段双击
      fieldname = i_column-fieldname  ) EQ abap_false.
      CHECK item_all IS NOT INITIAL."显示明细行
      item_dis = VALUE #( FOR item IN item_all WHERE ( afono = <line_head>-afono ) ( item ) ).
      falv_dis_item->soft_refresh( ).
    ELSE.
      ASSIGN c_falv->outtab->* TO <outtab_item>.
      READ TABLE <outtab_item> INDEX i_row-index  ASSIGNING FIELD-SYMBOL(<line_item>).
      CHECK sy-subrc EQ 0.
      zwft_common=>call_transation_by_line( EXPORTING line = <line_item>
      fieldname = i_column-fieldname  ).
    ENDIF.
  ENDMETHOD.


  METHOD editer_read_text.
    DATA:
      thead TYPE thead.
    DATA:tline TYPE TABLE OF tline.
    thead-tdid = 'ZAFO'.
    thead-tdobject =  'ZAFO'.
    thead-tdspras = sy-langu.
    CONCATENATE head-afono thead-tdid INTO thead-tdname.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = thead-tdid
        language                = thead-tdspras
        name                    = thead-tdname
        object                  = thead-tdobject
      TABLES
        lines                   = tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        language    = sy-langu
      TABLES
        itf_text    = tline
        text_stream = text_lines.

    IF text_lines IS NOT INITIAL.
      editor->set_text_as_stream(  text = text_lines ) .
    ENDIF.
  ENDMETHOD.


  METHOD editer_save_text.

    DATA:
          thead TYPE thead.
    DATA:tline TYPE TABLE OF tline.

    CHECK editor IS BOUND.
    CHECK head-afono IS NOT INITIAL.

    CLEAR tline[].
    CALL METHOD cl_gui_cfw=>flush.
    editor->get_text_as_stream( IMPORTING text = text_lines ).

    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        language    = sy-langu
      TABLES
        text_stream = text_lines
        itf_text    = tline.

    IF head-afono IS NOT INITIAL.
      thead-tdid = 'ZAFO'.
      thead-tdobject =  'ZAFO'.
      thead-tdspras = sy-langu.
      CONCATENATE head-afono thead-tdid INTO thead-tdname.
    ENDIF.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = thead
        savemode_direct = 'X'
      TABLES
        lines           = tline[]
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD exclude_function.
    IF functions IS INITIAL.
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_copy_row ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_delete_row ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_append_row ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_insert_row ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_move_row ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_copy ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_cut ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_paste ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_loc_undo ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_graph ).
      c_falv->exclude_function( cl_gui_alv_grid=>mc_fc_info ).
    ELSE.
      LOOP AT functions INTO DATA(l_fun).
        c_falv->exclude_function( l_fun ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD free.
    unlock( ).
  ENDMETHOD.


  METHOD get_falv_handle.
    DATA:ls_alv_handle TYPE zafo_alv_handle.

    ls_alv_handle-repid = repid.
    ls_alv_handle-bustyp = bustype-bustyp.
    ls_alv_handle-alv_name = alv_name.

    TRANSLATE ls_alv_handle-alv_name TO UPPER CASE."

    SELECT
      SINGLE handle INTO ls_alv_handle-handle
      FROM zafo_alv_handle
      WHERE repid = ls_alv_handle-repid
      AND bustyp = ls_alv_handle-bustyp
      AND alv_name = ls_alv_handle-alv_name.
    IF sy-subrc EQ 0.
      r_handle = ls_alv_handle-handle.
      RETURN.
    ENDIF.


    SELECT MAX( handle )
        FROM zafo_alv_handle
        INTO r_handle.
    ADD 1 TO r_handle.
    ls_alv_handle-handle = r_handle.
    MODIFY  zafo_alv_handle FROM  ls_alv_handle.
    COMMIT WORK .

  ENDMETHOD.


  METHOD get_next_afono.
    CHECK object-nrnr IS NOT INITIAL.
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'       " 锁定编码对象
      EXPORTING
        object           = 'ZAFONO'
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL FUNCTION 'NUMBER_GET_NEXT'            " 获取流水号
      EXPORTING
        nr_range_nr             = object-nrnr
        object                  = 'ZAFONO'
      IMPORTING
        number                  = r_afono
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        internal_overflow       = 6
        OTHERS                  = 7.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'       " 释放编码对象锁
      EXPORTING
        object = 'ZAFONO'.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD head_copy_to_item.
    DATA(screen_h) = zafo_basic=>get_bustyp_screen( bustyp = bustype-bustyp fieldalv = 'HEAD' ).
    DATA(screen_i) = zafo_basic=>get_bustyp_screen( bustyp = bustype-bustyp fieldalv = 'ITEM' ).

    DELETE screen_h WHERE fieldname+0(6) = 'REMARK'.
    DELETE screen_h WHERE fieldname+0(6) = 'AMOUNT'.
    DELETE screen_h WHERE fieldname+0(5) = 'MENGE'.

    LOOP AT screen_h INTO DATA(l_screen_h).
      ASSIGN COMPONENT l_screen_h-fieldname OF STRUCTURE head TO FIELD-SYMBOL(<fs_value_head>).
      CHECK sy-subrc EQ 0.
      LOOP AT item ASSIGNING FIELD-SYMBOL(<fs_item>).
        ASSIGN COMPONENT l_screen_h-fieldname OF STRUCTURE <fs_item> TO FIELD-SYMBOL(<fs_value_item>).
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
        IF <fs_value_item> IS INITIAL.
          <fs_value_item> = <fs_value_head>.
        ELSEIF l_screen_h-requi = 'X'.
          <fs_value_item> = <fs_value_head>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD head_double_click.
    CHECK readonly = abap_true.
    DATA cursor TYPE fieldname.
    DATA stuctname TYPE fieldname.
    DATA fieldname TYPE fieldname.
    ASSIGN ('(SAPLZAFO)CURSOR')  TO FIELD-SYMBOL(<cursor>).
    CHECK sy-subrc EQ 0.
    cursor = <cursor>.
    REPLACE ALL OCCURRENCES OF '->' IN cursor WITH '%%'.
    SPLIT cursor AT '-' INTO stuctname fieldname.
    IF fieldname = 'AFONO'.
      gos_call( ).
    ELSE.
      zwft_common=>call_transation_by_line( line = head fieldname = fieldname ).
    ENDIF.

  ENDMETHOD.


  METHOD hold_soft_refresh.
    c_falv->get_variant( IMPORTING es_variant = DATA(ls_disvariant) ).
    c_falv->soft_refresh( ).
    c_falv->set_variant( ls_disvariant ).
  ENDMETHOD.


  METHOD hotspot_click.
    FIELD-SYMBOLS: <outtab_head> TYPE zafo_tt_shead.
    FIELD-SYMBOLS: <outtab_item> TYPE zafo_tt_sitem.
    IF c_falv = falv_dis_head.
      ASSIGN c_falv->outtab->* TO <outtab_head>.
      READ TABLE <outtab_head> INDEX i_row-index  ASSIGNING FIELD-SYMBOL(<line_head>).
      CASE i_column.
        WHEN 'AFONO'.
          CLEAR:head, item.
          READ TABLE head_dis INDEX i_row ASSIGNING FIELD-SYMBOL(<head>).
          DATA(l_class) = zafo_class=>maintain( EXPORTING afono = <head>-afono bustyp = <head>-bustyp  ).
          <head> = l_class->head.
          CALL FUNCTION 'ZAFO_GET_CLASS'
            EXPORTING
              i_class = me.
      ENDCASE.
    ELSE.
      ASSIGN c_falv->outtab->* TO <outtab_item>.
      READ TABLE <outtab_item> INDEX i_row-index  ASSIGNING FIELD-SYMBOL(<line_item>).
      CASE i_column.
        WHEN 'AFONO'.
          CHECK <line_item>-afono IS NOT INITIAL.
          zafo_class=>maintain( EXPORTING afono = <line_item>-afono ).
          <head> = l_class->head.
          CALL FUNCTION 'ZAFO_GET_CLASS'
            EXPORTING
              i_class = me.
        WHEN OTHERS.
          zwft_common=>call_transation_by_line( EXPORTING line = <line_item>
                                                                                        fieldname = i_column-fieldname  ).
      ENDCASE.
    ENDIF.
    c_falv->soft_refresh( ).
  ENDMETHOD.


  METHOD init_alv.
    DATA:lt_screen TYPE zafo_tt_screen.
    CASE action.
      WHEN 'REF'.
        lt_screen = screen_ref.
        LOOP AT lt_screen ASSIGNING FIELD-SYMBOL(<fs_screen>)
          WHERE edit = 'X' .
          <fs_screen>-edit = ''.
        ENDLOOP.

      WHEN 'REPORT'.
        lt_screen = screen_con.
        IF c_falv = falv_dis_head.
          DELETE lt_screen WHERE fieldalv <> 'HEAD'.
        ELSEIF c_falv = falv_dis_item.
          DELETE lt_screen WHERE fieldalv <> 'ITEM'.
        ENDIF.
      WHEN OTHERS.
        lt_screen = screen_con.
        DELETE lt_screen WHERE fieldalv <> 'ITEM'.
    ENDCASE.

    set_fcat_from_screen(  EXPORTING it_screen = lt_screen
                                        CHANGING  fcat = c_falv->fcat ).

    register_edit_event( c_falv ).
    register_f4( c_falv ).

    set_falv_layout( CHANGING falv = c_falv ).
    exclude_function( EXPORTING c_falv = c_falv ).
    c_falv->title_v1 = bustype-bustyp_name1.
    set_falv_input( c_falv ).


  ENDMETHOD.


  METHOD init_head.
    me->head-bustyp = me->bustype-bustyp.
    me->head-object = me->bustype-object.
    me->head-category = me->bustype-category.
*    me->head-execute_type = me->object-execute_type.

    me->head-bldat = sy-datum.
    me->head-budat = sy-datum.

    IF me->head-werks IS INITIAL.
      me->head-werks = me->werks.
    ENDIF.

    IF me->head-vkorg IS INITIAL.
      SELECT SINGLE vkorg INTO me->head-vkorg
      FROM t001w
      WHERE werks = me->werks.
    ENDIF.
    IF me->head-bukrs IS INITIAL.
      SELECT SINGLE bukrs INTO me->head-bukrs
      FROM t001k
      WHERE bwkey = me->werks.
      SELECT SINGLE butxt FROM t001
      INTO me->head-bukrs_name
      WHERE bukrs = me->head-bukrs.
    ENDIF.
    IF me->head-ekorg IS INITIAL.
      SELECT SINGLE ekorg INTO me->head-ekorg
      FROM t001w
      WHERE werks = me->werks.
    ENDIF.


    LOOP AT me->screen_con INTO DATA(ls_screen) WHERE fieldalv = 'HEAD' AND zdefault <> ''.
      ASSIGN COMPONENT ls_screen-fieldname OF STRUCTURE me->head TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc EQ 0.
        IF <fs_value> IS INITIAL.
          ASSIGN (ls_screen-zdefault) TO FIELD-SYMBOL(<fs_globle>).
          IF sy-subrc EQ 0.
            <fs_value> = <fs_globle>.
          ELSE.
            <fs_value> = ls_screen-zdefault.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF me->object-app_object IS NOT INITIAL.
      me->head-app_status = 'A'.
    ELSE.
      me->head-app_status = ''.
    ENDIF.


  ENDMETHOD.


  METHOD init_head_text.

    IF me->head-werks IS NOT INITIAL.
      SELECT SINGLE name1
      INTO me->head-werks_name
      FROM t001w
      WHERE werks EQ me->head-werks.
      IF sy-subrc NE 0.
        me->head-werks = ''.
        me->head-werks_name = ''.
        MESSAGE s001."'工厂不存在' TYPE 'S'.
      ENDIF.
    ENDIF.

    IF me->head-lgort IS NOT INITIAL.
      SELECT SINGLE lgobe
      INTO me->head-lgort_name
      FROM t001l
      WHERE werks EQ me->head-werks
      AND lgort = me->head-lgort .
      IF sy-subrc NE 0.
        me->head-lgort = ''.
        me->head-lgort_name = ''.
        MESSAGE s002."'库存地点不存在' TYPE 'S'.
      ENDIF.
    ENDIF.

    IF me->head-umwrk IS NOT INITIAL.
      SELECT SINGLE name1
      INTO me->head-umwrk_name
      FROM t001w
      WHERE werks EQ me->head-umwrk.
      IF sy-subrc NE 0.
        me->head-umwrk = ''.
        me->head-umwrk_name = ''.
        MESSAGE s001."'工厂不存在' TYPE 'S'.
      ENDIF.
    ENDIF.

    IF me->head-umlgo IS NOT INITIAL.
      IF me->head-umwrk IS INITIAL.
        SELECT SINGLE lgobe
        INTO me->head-umlgo_name
        FROM t001l
        WHERE werks EQ me->head-werks
        AND lgort =  me->head-umlgo .
        IF sy-subrc NE 0.
          me->head-umlgo = ''.
          me->head-umlgo_name = ''.
          MESSAGE s003." '收货库存地点不存在' TYPE 'S'.
        ENDIF.
      ELSE.

        SELECT SINGLE lgobe
        INTO me->head-umlgo_name
        FROM t001l
        WHERE werks EQ me->head-umwrk
        AND lgort =  me->head-umlgo .
        IF sy-subrc NE 0.
          me->head-umlgo = ''.
          me->head-umlgo_name = ''.
          MESSAGE s003."'收货库存地点不存在' TYPE 'S'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF me->head-lifnr IS NOT INITIAL AND me->head-lifnr_name IS INITIAL.
      SELECT SINGLE name1
      INTO me->head-lifnr_name
      FROM lfa1
      WHERE lifnr EQ me->head-lifnr.
      IF sy-subrc NE 0.
        me->head-lifnr = ''.
        me->head-lifnr_name = ''.
        MESSAGE s004."'供应商不存在' TYPE 'S'.
      ENDIF.
    ENDIF.

    IF  me->head-lifnr IS NOT INITIAL.
      SELECT SINGLE waers FROM lfm1 INTO me->head-waers
      WHERE lifnr = me->head-lifnr .
      SELECT SINGLE land1 FROM lfa1 INTO me->head-land1
      WHERE lifnr = me->head-lifnr .
    ENDIF.

    IF me->head-bukrs IS NOT INITIAL.
      SELECT SINGLE butxt
      INTO me->head-bukrs_name
      FROM t001 WHERE bukrs EQ me->head-bukrs  .
      IF sy-subrc NE 0.
        CLEAR me->head-bukrs.
        CLEAR me->head-bukrs_name.
        MESSAGE s005." '公司代码填写错误' TYPE 'S'.
      ENDIF.
    ENDIF.

    IF me->head-kunnr IS NOT INITIAL.
      SELECT SINGLE name1, land1
      INTO ( @me->head-kunnr_name,@me->head-land1 )
      FROM kna1
      WHERE kunnr EQ @me->head-kunnr.
      IF sy-subrc NE 0.
        me->head-kunnr = ''.
        me->head-kunnr_name = ''.
        MESSAGE s004."'供应商不存在' TYPE 'S'.
      ELSE.
        SELECT SINGLE  waers , name1,land1 FROM kna1
        INNER JOIN knvv ON kna1~kunnr = knvv~kunnr
        INTO  ( @me->head-waers , @me->head-lifnr_name , @me->head-land1 )
        WHERE kna1~kunnr = @me->head-kunnr .
      ENDIF.
    ENDIF.

    IF me->head-kostl IS NOT INITIAL.
      SELECT SINGLE ktext
      INTO me->head-kostl_name
      FROM csks INNER JOIN cskt
      ON csks~kokrs = cskt~kokrs
      AND csks~kostl = cskt~kostl
      AND csks~datbi = cskt~datbi

      WHERE csks~kostl EQ me->head-kostl
      AND csks~bukrs EQ me->head-bukrs.
      IF sy-subrc NE 0.
        CLEAR me->head-kostl_name .
        CLEAR me->head-kostl .
        MESSAGE s006."'请选择对应公司的成本中心' TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD init_item.
    CHECK item IS INITIAL.

    DO 10 TIMES.
      APPEND INITIAL LINE TO item ASSIGNING FIELD-SYMBOL(<item>).
      init_item_line( EXPORTING index =  sy-index CHANGING c_item = <item> ).
    ENDDO.

  ENDMETHOD.


  METHOD init_item_line.
    c_item-afono = me->head-afono.
    c_item-afonr = index.
    c_item-werks = me->werks.
    c_item-eeind = head-eeind.
    set_icon( EXPORTING status = '' IMPORTING icon = c_item-icon text = c_item-text ).

    LOOP AT me->screen_con INTO DATA(ls_screen)  WHERE fieldalv = 'ITEM' AND zdefault <> ''.
      ASSIGN COMPONENT ls_screen-fieldname OF STRUCTURE c_item TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc EQ 0 .
        CHECK <fs_value> IS INITIAL.
        ASSIGN (ls_screen-zdefault) TO FIELD-SYMBOL(<fs_gvalue>).
        IF sy-subrc EQ 0.
          <fs_value> = <fs_gvalue>.
        ELSE.
          <fs_value> = ls_screen-zdefault.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD item_copy_to_head.
    LOOP AT item INTO DATA(l_item).
      DATA(l_tabix) = sy-tabix.
      LOOP AT screen_con INTO DATA(l_screen) WHERE fieldalv = 'HEAD'
                                                                       AND requi = 'X'.
        ASSIGN COMPONENT l_screen-fieldname OF STRUCTURE head TO FIELD-SYMBOL(<fs_head_value>).
        CHECK sy-subrc EQ 0.

        ASSIGN COMPONENT l_screen-fieldname OF STRUCTURE l_item TO FIELD-SYMBOL(<fs_item_value>).
        CHECK sy-subrc EQ 0.

        IF l_tabix = 1.
          me->werks = l_item-werks.
          <fs_head_value> = <fs_item_value>.
        ELSE.
          IF <fs_head_value> <> <fs_item_value> .
            message->add_single( EXPORTING msgty = 'E' msgid = 'ZAFO' msgno = '008' msgv1 = | { l_screen-coltext }| ).
            "单据抬头&1不唯一,请重新选择
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD lock.
    locked = abap_false.
    CHECK me->head-afono IS NOT INITIAL.

    CALL FUNCTION 'ENQUEUE_EZAFO_HEAD'
      EXPORTING
        mode_zafo_head = 'E'
        mandt          = sy-mandt
        afono          = me->head-afono
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
      TYPE 'S'
      NUMBER sy-msgno
      WITH sy-msgv1
      sy-msgv2
      sy-msgv3
      sy-msgv4 DISPLAY LIKE 'E'.
      locked = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD maintain.
    DATA i_bustyp TYPE zafo_bustyp.

    IF bustyp IS INITIAL.
      SELECT SINGLE bustyp INTO i_bustyp
        FROM zafo_head
        WHERE afono = afono.
    ELSE.
      i_bustyp = bustyp.
    ENDIF.

    IF i_bustyp IS INITIAL.
      MESSAGE e023."订单不存在
      RETURN.
    ENDIF.

    r_class = NEW zafo_class( bustyp = i_bustyp ).

    SELECT SINGLE * FROM zafo_head
      INTO CORRESPONDING FIELDS OF r_class->head
      WHERE afono = afono .

    SELECT * FROM zafo_item
      INTO CORRESPONDING FIELDS OF TABLE r_class->item
       WHERE afono = afono.
    IF r_class->head IS INITIAL OR r_class->item IS INITIAL.
      r_class->message->add_single( msgty = 'E' msgid = 'ZAFO' msgno = '009' ).
      r_class->message->pop_msg( 'X' ).
      RETURN.
    ENDIF.

    r_class->werks = r_class->head-werks.
    r_class->set_action( 'DISPLAY' ).

    r_class->set_icon( EXPORTING status = r_class->head-status
                                 IMPORTING icon = r_class->head-icon text = r_class->head-text ).

    LOOP AT r_class->item ASSIGNING FIELD-SYMBOL(<fs_item>).
      r_class->set_icon( EXPORTING status = <fs_item>-item_status
                                   IMPORTING icon = <fs_item>-icon text = <fs_item>-text ).
    ENDLOOP.
*    r_class->init_head_text( ).
    CALL FUNCTION 'ZAFO_UI'
      EXPORTING
        i_class = r_class.

  ENDMETHOD.


  METHOD perpare_save.
    CLEAR head-menge.
    CLEAR head-amount.
    CLEAR head-netwr.

    LOOP AT item  ASSIGNING FIELD-SYMBOL(<fs_item>).
      IF <fs_item>-icon = icon_led_inactive .
        DELETE item.
        CONTINUE.
      ENDIF.
      IF <fs_item>-del_flag = 'X' .
        CONTINUE.
      ENDIF.
      head-menge = head-menge + <fs_item>-menge.
      head-amount = head-amount + <fs_item>-amount.
    ENDLOOP.
  ENDMETHOD.


  METHOD register_edit_event.
    c_falv->register_edit_event( c_falv->mc_evt_modified ).
  ENDMETHOD.


  METHOD report.
    r_class = NEW zafo_class( bustyp = bustyp ).

    r_class->head_dis = head.
    r_class->item_all = item.
    r_class->item_dis = item.
    r_class->set_action( 'REPORT' ).
    CALL FUNCTION 'ZAFO_REPORT'
      EXPORTING
        i_class = r_class.

  ENDMETHOD.


  METHOD SAVE.
    perpare_save( ).

    check_head( ).

    check_item( ).

    macro_error_return.

    head_copy_to_item( ).

    act_run( '&SAVE' ).

    macro_error_return.

    save_data( ).

    set_ref_status( ).

    set_action( 'DISPLAY' ).
  ENDMETHOD.


  METHOD save_data.

    IF head-afono IS INITIAL.
      head-afono = get_next_afono( ).
    ENDIF.

    IF action = 'CREATE'.
      head-status = COND #( WHEN head-status = '' THEN 'A' ELSE head-status ).
      head-erdat = COND #( WHEN head-erdat IS INITIAL THEN sy-datum ELSE head-erdat ).
      head-erzet = COND #( WHEN head-erzet IS INITIAL THEN sy-uzeit ELSE head-erzet ).
      head-ernam = COND #( WHEN head-ernam IS INITIAL THEN sy-uname ELSE head-ernam ).
      add_gos_relationship( ).
      lock( ) .
    ENDIF.

    head-aenam = sy-uname.
    head-aedat = sy-datum.
    head-aetim = sy-uzeit .
    head-tcode = sy-tcode .
    set_status( head-status ).

    LOOP AT item  ASSIGNING FIELD-SYMBOL(<fs_item>).
      <fs_item>-afono = head-afono.
      IF <fs_item>-icon = icon_led_inactive.
        DELETE item.
        CONTINUE.
      ENDIF.
      set_icon( EXPORTING status = <fs_item>-item_status IMPORTING icon = <fs_item>-icon text = <fs_item>-text ).
    ENDLOOP.

    head_db = CORRESPONDING #( head ).
    item_db = CORRESPONDING #( item ).
    editer_save_text( ).
    db_save( ).
    IF sy-subrc EQ 0.
      message->add_single( msgty = 'S' msgid = 'ZAFO' msgno = '010' )."保存成功
    ENDIF.
  ENDMETHOD.


  METHOD set_action.

    CHECK action <> i_action.
    action = i_action.
    CASE action.
      WHEN 'REF'.
        set_readonly(  ).
      WHEN 'CREATE'.
        set_readonly(  ).
      WHEN 'CHANGE'.
        set_readonly(  ).
      WHEN 'DISPLAY'.
        set_readonly( abap_true ).
      WHEN 'REPORT'.
        set_readonly( abap_true ).
      WHEN 'SAVE'.
        set_readonly( abap_true ).
      WHEN 'POST'.
        set_readonly( abap_true ).
        head-status = 'S'.
      WHEN 'FOLLOW'.
        head-status = 'T'.
      WHEN 'LOCK'.
        set_readonly( abap_true ).
        head-status = 'L'.
      WHEN 'DELETE'.
        set_readonly( abap_true ).
        head-status = 'D'.
    ENDCASE.
    set_icon( EXPORTING status = head-status IMPORTING icon = head-icon text = head-text ).
  ENDMETHOD.


  METHOD set_amount.
    cs_item-peinh = COND #( WHEN cs_item-peinh IS INITIAL THEN 1 ELSE cs_item-peinh   ).
    cs_item-amount = cs_item-price * cs_item-menge / cs_item-peinh.
  ENDMETHOD.


  METHOD set_falv_input.
    CASE readonly.
      WHEN abap_true.
        falv->set_ready_for_input( 0 ).
      WHEN abap_false.
        falv->set_ready_for_input( 1 ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_falv_layout.
    falv->layout->set_cwidth_opt( abap_false ).
    falv->layout->set_zebra( abap_true ).
    falv->layout->set_sel_mode( 'A' ).
    falv->layout->set_info_fname( 'ROW_COLOR' ).
*    falv->layout->set_ctab_fname( 'ROW_COLOR' ).
  ENDMETHOD.


  METHOD set_fcat_from_screen.

    LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      <fs_fcat>-no_init_ch = 'B'.
      <fs_fcat>-key_sel = ''.
      <fs_fcat>-row_pos = 1.
      CASE <fs_fcat>-fieldname.
        WHEN 'ICON'.
          <fs_fcat> = VALUE #( BASE <fs_fcat>
                                              coltext = TEXT-001
                                              scrtext_l = TEXT-001
                                              scrtext_m = TEXT-001
                                              scrtext_s = TEXT-001
                                              col_pos = 1
                                              fix_column = abap_true
                                              outputlen = 2
                                            ).
        WHEN 'TEXT'.
          <fs_fcat> = VALUE #( BASE <fs_fcat>
                                              coltext = TEXT-002
                                              scrtext_l = TEXT-002
                                              scrtext_m = TEXT-002
                                              scrtext_s = TEXT-002
                                              col_pos = 2
                                              fix_column = abap_true
                                              outputlen = 5
                                              ).
          CONTINUE.
        WHEN 'SELECTED'.
          IF action = 'REF'.
            <fs_fcat> = VALUE #( BASE <fs_fcat>
                                                coltext = TEXT-003
                                                scrtext_l = TEXT-003
                                                scrtext_m = TEXT-003
                                                scrtext_s = TEXT-003
                                                col_pos = 0
                                                edit = abap_true
                                                checkbox = abap_true
                                                fix_column = abap_true
                                                ).
          ELSE .
            <fs_fcat>-tech = 'X'.
            <fs_fcat>-col_pos = 999.
          ENDIF.
        WHEN 'AFONO'.
          IF ( action = 'REPORT'
             OR ( action = 'REF' AND bustype-busref CA 'YZ' ) ).
            <fs_fcat> = VALUE #( BASE <fs_fcat>
                                                coltext = TEXT-004
                                                scrtext_l = TEXT-004
                                                scrtext_m = TEXT-004
                                                scrtext_s = TEXT-004
                                                col_pos = 4
                                                hotspot = abap_true
                                                fix_column = abap_true
                                                outputlen = 10
                                                ).
          ELSE .
            <fs_fcat>-tech = 'X'.
            <fs_fcat>-col_pos = 999.
          ENDIF.
        WHEN 'AFONR'.
          IF ( action = 'REF' AND bustype-busref CA 'YZ' ) OR action <> 'REF'.
            <fs_fcat> = VALUE #( BASE <fs_fcat>
                                              coltext = TEXT-005
                                              scrtext_l = TEXT-005
                                              scrtext_m = TEXT-005
                                              scrtext_s = TEXT-005
                                              col_pos = 5
                                              fix_column = abap_true
                                              outputlen = 2
                                              ).
          ELSE .
            <fs_fcat>-tech = 'X'.
            <fs_fcat>-col_pos = 999.
          ENDIF.
        WHEN OTHERS.
          READ TABLE it_screen INTO DATA(ls_screen) WITH KEY fieldname = <fs_fcat>-fieldname.
          IF sy-subrc EQ 0.
            <fs_fcat> = VALUE #( BASE <fs_fcat>
                                                col_pos = ls_screen-dzaehk
                                                edit = ls_screen-edit
                                                emphasize = ls_screen-emphasize
                                                coltext = ls_screen-coltext
                                                scrtext_l = ls_screen-coltext
                                                scrtext_m = ls_screen-coltext
                                                scrtext_s = ls_screen-coltext
                                                outputlen = ls_screen-outputlen
                                                no_zero = abap_true
                                                tabname = 'ZAFO_SITEM'
                                                ).
            IF ls_screen-edit = 'C' AND action <> 'CREATE'.
              <fs_fcat>-edit = 'X'.
            ENDIF.

            IF ls_screen-hidde = 'C' AND action = 'REF'.
              <fs_fcat>-tech = 'X'.
              <fs_fcat>-col_pos = 999.
            ELSEIF ls_screen-hidde = 'I' AND action = 'CREATE'.
              <fs_fcat>-tech = 'X'.
              <fs_fcat>-col_pos = 999.
            ELSEIF ls_screen-hidde = 'X'.
              <fs_fcat>-tech = 'X'.
              <fs_fcat>-col_pos = 999.
            ENDIF.
          ELSE.
            <fs_fcat>-tech = 'X'.
            <fs_fcat>-col_pos = 999.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    SORT fcat BY col_pos.
    DELETE fcat WHERE tech = 'X'.
  ENDMETHOD.


  METHOD set_gui_status_exclude.
    DATA(lt_act) = act.
    CASE head-status.
      WHEN ''.
        DELETE lt_act WHERE act <> '&SAVE'.
      WHEN 'A'.
        IF readonly = abap_true.
          DELETE lt_act WHERE act = '&SAVE'.
          DELETE lt_act WHERE act = '&CANCEL'.
          DELETE lt_act WHERE act = '&UNCOMMIT'.
        ELSE.
          DELETE lt_act WHERE act <> '&SAVE'.
        ENDIF.
      WHEN 'B'.
        DELETE lt_act WHERE act <> '&UNCOMMIT'.
      WHEN 'C'.
        DELETE lt_act WHERE act <> '&UNCOMMIT' AND act <> '&PRINT'.
      WHEN 'S'.
        DELETE lt_act WHERE act <> '&CANCEL' AND act <> '&PRINT'.
      WHEN 'T'.
        CLEAR lt_act.
      WHEN 'D'.
        CLEAR lt_act.
      WHEN 'L'.
        CLEAR lt_act.
    ENDCASE.

    IF print_rule IS INITIAL.
      DELETE lt_act WHERE act = '&PRINT'.
    ENDIF.

    LOOP AT gui_status INTO DATA(l_status).
      READ TABLE lt_act TRANSPORTING NO FIELDS WITH KEY act = l_status.
      IF sy-subrc NE 0.
        APPEND l_status TO fcodes.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_icon.
    CASE status.
      WHEN ''.
        icon = icon_led_inactive.
        text = TEXT-009."'未保存'.

      WHEN 'A'.
        icon = icon_led_yellow.
        text = TEXT-010."'已保存'.

      WHEN 'B'.
        icon = icon_structure.
        text = TEXT-011."'审核中'.

      WHEN 'C'.
        icon = icon_led_green.
        text = TEXT-012."'已审核'.

      WHEN 'S'.
        icon = icon_complete.
        text = TEXT-013."'已完成'.

      WHEN 'T'.
        icon = icon_allow.
        text = TEXT-014."'后续已完成'.

      WHEN 'L'.
        icon = icon_locked.
        text = TEXT-015."'已锁定'.

      WHEN 'D'.
        icon = icon_delete.
        text = TEXT-016."'已作废'.

    ENDCASE.

  ENDMETHOD.


  METHOD set_matnr_info.
    SELECT * FROM zafo_v_mara
      INTO TABLE @DATA(lt_vmara)
      FOR ALL ENTRIES IN @item
        WHERE matnr = @item-matnr.
    CHECK sy-subrc EQ 0.
    SORT lt_vmara BY matnr.
    LOOP AT item ASSIGNING FIELD-SYMBOL(<item>).
      READ TABLE lt_vmara INTO DATA(ls_vmara) WITH KEY matnr = <item>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_vmara TO <item>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_price.
    DATA:price TYPE string.
    DATA: int TYPE char30.
    DATA:dec TYPE char30.
    DATA:len TYPE i.
    cs_item-peinh = 1.
    price = cs_item-price_long.
    CONDENSE price NO-GAPS.
    SHIFT  price RIGHT DELETING TRAILING  '0' . "去掉没用的小数位的0
    SHIFT  price RIGHT DELETING TRAILING  '.' . "去掉没用的小数位的0

    SPLIT price AT '.' INTO int dec.
    IF strlen( dec )  > 2.
      DO strlen( dec ) - 2 TIMES.
        cs_item-peinh = cs_item-peinh * 10.
        price = price * 10.
      ENDDO.
    ENDIF.
    cs_item-price = price.

    set_amount( CHANGING cs_item = cs_item ).
  ENDMETHOD.


  METHOD set_readonly.
    CHECK i_readonly <> me->readonly.
    me->readonly = i_readonly.
    CHECK me->falv_item IS BOUND.
    set_falv_input( me->falv_item ).
  ENDMETHOD.


  METHOD set_ref_status.
    DATA:lt_item_flo TYPE TABLE OF ty_item_flo.
    DATA:ls_item_flo TYPE  ty_item_flo.
    DATA:l_all TYPE char1.

    LOOP AT item INTO DATA(l_item) WHERE afono_ref_per IS NOT INITIAL.
      ls_item_flo-afono = l_item-afono_ref_per.
      ls_item_flo-afonr = l_item-afonr_ref_per.

      IF l_item-del_flag = 'X' OR ls_item_flo-item_status = 'D'.
        ls_item_flo-afono_ref_flo =  ''.
        ls_item_flo-afonr_ref_flo =  ''.
        ls_item_flo-item_status = 'C'.
      ELSE.
        ls_item_flo-afono_ref_flo =  l_item-afono.
        ls_item_flo-afonr_ref_flo =  l_item-afonr.
        ls_item_flo-item_status = 'T'.
      ENDIF.

      APPEND ls_item_flo TO lt_item_flo.
      CLEAR ls_item_flo.
    ENDLOOP.

    CHECK lt_item_flo IS NOT INITIAL.
    LOOP AT lt_item_flo INTO DATA(p_item)
                                     GROUP BY ( afono = p_item-afono
                                                        ) INTO DATA(group_item).
      LOOP AT GROUP group_item INTO DATA(group_line_item).
        UPDATE zafo_item SET item_status = group_line_item-item_status
                                                afono_ref_flo = group_line_item-afono_ref_flo
                                                afonr_ref_flo = group_line_item-afonr_ref_flo
                                        WHERE afono = group_line_item-afono
                                        AND afonr = group_line_item-afonr.
      ENDLOOP.

      SELECT item_status,COUNT( item_status ) AS count FROM zafo_item
        WHERE afono = @group_item-afono AND del_flag = ''
        GROUP BY item_status
        INTO TABLE @DATA(lt_item_status).
      IF lt_item_status IS NOT INITIAL.
        SORT lt_item_status BY item_status DESCENDING .
        DATA(status) = lt_item_status[ 1 ]-item_status.
        UPDATE zafo_head SET status = status WHERE afono = group_item-afono.
      ENDIF.
    ENDLOOP.
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_screen_text.
    DATA: lt_abap_componentdescr TYPE STANDARD TABLE OF ty_abap_componentdescr WITH KEY name,
          ls_abap_componentdescr TYPE ty_abap_componentdescr.
    DATA: lcr_ref_line TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS:<fs_value> TYPE any.
    FIELD-SYMBOLS:<fs_screen> TYPE zafo_screen.
    FIELD-SYMBOLS:<fs_text> TYPE any.

    SELECT * INTO TABLE @DATA(lt_text)
    FROM zafo_screen_text
       WHERE langu = @sy-langu
       AND fieldalv <> ''
       AND fieldname <> ''
       AND ( object = @me->bustype-object
       OR object = @me->bustype_ref-object
       OR object = '' ).

    SORT lt_text BY fieldname object DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_text COMPARING fieldname.
    SORT me->screen_con BY dzaehk.

    LOOP AT me->screen_con ASSIGNING <fs_screen> WHERE object = me->bustype-object AND fieldalv = 'HEAD' .
      READ TABLE lt_abap_componentdescr TRANSPORTING NO FIELDS
      WITH KEY name = <fs_screen>-fieldname.
      IF sy-subrc NE 0.
        ls_abap_componentdescr-name = <fs_screen>-fieldname.  "用于生成动态内表
        ls_abap_componentdescr-type ?= cl_abap_typedescr=>describe_by_name( 'CHAR40' ).
        APPEND ls_abap_componentdescr TO lt_abap_componentdescr.
        CLEAR ls_abap_componentdescr.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_text INTO DATA(ls_text).
      READ TABLE me->screen_con ASSIGNING <fs_screen>
              WITH KEY object = ls_text-object
              fieldalv = ls_text-fieldalv
              fieldname = ls_text-fieldname.
      IF sy-subrc EQ 0.
        <fs_screen>-coltext = ls_text-coltext.
      ENDIF.

      READ TABLE me->screen_ref ASSIGNING <fs_screen>
            WITH KEY object = ls_text-object
            fieldalv = ls_text-fieldalv
            fieldname = ls_text-fieldname.
      IF sy-subrc EQ 0.
        <fs_screen>-coltext = ls_text-coltext.
      ENDIF.

      IF ls_text-object = '' OR ls_text-object = me->bustype-object.
        READ TABLE lt_abap_componentdescr TRANSPORTING NO FIELDS
        WITH KEY name = ls_text-fieldname.
        IF sy-subrc NE 0.
          ls_abap_componentdescr-name = ls_text-fieldname.  "用于生成动态内表
          ls_abap_componentdescr-type ?= cl_abap_typedescr=>describe_by_name( 'CHAR40' ).
          APPEND ls_abap_componentdescr TO lt_abap_componentdescr.
          CLEAR ls_abap_componentdescr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    lcr_ref_line ?= cl_abap_structdescr=>create( p_components = lt_abap_componentdescr ).
    CREATE DATA text TYPE HANDLE lcr_ref_line.
    ASSIGN text->* TO <fs_text>.


    LOOP AT me->screen_con ASSIGNING <fs_screen> WHERE object = me->bustype-object AND fieldalv = 'HEAD' .
      ASSIGN COMPONENT <fs_screen>-fieldname OF STRUCTURE <fs_text> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = <fs_screen>-coltext.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_text INTO ls_text WHERE fieldalv = 'HEAD' AND ( object = '' OR object = me->bustype-object ).
      ASSIGN COMPONENT ls_text-fieldname OF STRUCTURE <fs_text> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF <fs_value> IS INITIAL.
          <fs_value> = ls_text-coltext.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_status.
    head-status = status.
    set_icon( EXPORTING status = status IMPORTING icon = head-icon text = head-text  ).
    CASE status.
      WHEN 'A' OR 'B' OR 'C' OR 'S' OR 'T' OR 'L' .
        set_readonly( abap_true ).
    ENDCASE.

    LOOP AT item ASSIGNING FIELD-SYMBOL(<item>).
      <item>-item_status = status.
      set_icon( EXPORTING status = status IMPORTING icon = <item>-icon text = <item>-text  ).
      IF status = 'D'.
        <item>-del_flag = 'X'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD sure_ref.
    CLEAR me->item.

    LOOP AT item_ref ASSIGNING FIELD-SYMBOL(<line>) WHERE selected = 'X'.
      IF <line>-item_status <> 'C' AND <line>-item_status <> 'S'.
        message->add_single( EXPORTING msgty = 'E' msgid = 'ZAFO' msgno = '011'  )."只有已审核状态可引用
      ENDIF.
      APPEND <line> TO me->item.
    ENDLOOP.

    IF message->get_error( ) EQ abap_true.
      CLEAR: head,item.
      message->pop_msg( ).
      RETURN.
    ENDIF.

    IF me->item IS NOT INITIAL.
      SORT me->item BY matnr afono_ref_per afonr_ref_per.
      LOOP AT me->item ASSIGNING <line>.
        <line>-afono_ref_per = <line>-afono.
        <line>-afonr_ref_per = <line>-afonr.
        <line>-afonr = sy-tabix.
        CLEAR: <line>-afono,<line>-row_color.
      ENDLOOP.
      item_copy_to_head( ).

    ELSE.
      message->add_single( EXPORTING msgty = 'E' msgid = 'ZAFO' msgno = '020' ).
    ENDIF.

    IF message->get_error( ) EQ abap_true.
      CLEAR: head,item.
      message->pop_msg( 'X' ).
      RETURN.
    ENDIF.

    LEAVE TO SCREEN 0.

  ENDMETHOD.


  METHOD TOOLBAR.
    CASE action.
      WHEN 'REPORT'.

      WHEN 'REF'.

      WHEN OTHERS.
        CHECK readonly NE abap_true.
        IF bustype-busref_control NE 2.
          APPEND VALUE #( function = '&ADD'
                                        icon = icon_insert_row
                                        quickinfo = TEXT-006
                                        disabled = ''
                                        text = TEXT-006
                                      ) TO i_object->mt_toolbar.
        ENDIF.

        APPEND VALUE #( function = '&DEL'
                                        icon = icon_delete
                                        quickinfo = TEXT-007
                                        disabled = ''
                                        text = TEXT-007
                                        ) TO i_object->mt_toolbar.

    ENDCASE.
  ENDMETHOD.


  METHOD uncommit.
    CHECK head-status CA 'BC'.
    CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '02' bustyp = head-bustyp werks = werks ).
    CHECK zwft_common=>confirm( '确认取消提交么' ) = abap_true.

    act_run( '&UNCOMMIT' ).
    macro_error_return.

    update_head_status( 'A' ).
*    IF object-app_object IS INITIAL.
*      update_head_status( 'A' ).
*    ELSE.
*      MESSAGE e000 WITH '无法取消提交'.
*    ENDIF.
  ENDMETHOD.


  METHOD unlock.
    CHECK head-afono IS NOT INITIAL.
    CALL FUNCTION 'DEQUEUE_EZAFO_HEAD'
      EXPORTING
        mode_zafo_head = 'E'
        mandt          = sy-mandt
        afono          = head-afono.
    locked = abap_false.

  ENDMETHOD.


  METHOD update_head_status.
    set_status( status ).
    head-aenam = sy-uname.
    head-aedat = sy-datum.
    head-aetim = sy-uzeit.

    DATA(del_flag) = COND #( WHEN status = 'D' THEN 'X' ELSE '' ).

    head_db = CORRESPONDING #( head ).
    MODIFY zafo_head FROM head_db.

    UPDATE zafo_item
            SET item_status = status
                   del_flag = del_flag
            WHERE afono = head-afono
            AND del_flag = ''.


    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD user_command_main.

    falv_item->check_changed_data( ).
    CASE fcode.
      WHEN '&ENTR'.
        head_double_click( ).
      WHEN '&SAVE'.
        save( ).
      WHEN '&EDIT'.
        set_action( 'CHANGE' ).
      WHEN '&DELETE'.
        delete( ).
      WHEN '&COMMIT'.
        commit( ).
      WHEN '&UNCOMMIT'.
        uncommit( ).
      WHEN '&POST'.
        post( ).
      WHEN '&CANCEL'.
        cancel( ).
      WHEN '&PRINT'.
        print( ).
      WHEN '&ZOOMIN'.
        editer_editor_text( ).
    ENDCASE.
  ENDMETHOD.


  METHOD user_command_ref.
    FIELD-SYMBOLS: <outtab> TYPE zafo_tt_sitem.

    ASSIGN c_falv->outtab->* TO <outtab>.
    CASE i_ucomm.
      WHEN '&SELECTALL'.
        c_falv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).
        IF lt_rows IS INITIAL.
          LOOP AT <outtab> ASSIGNING FIELD-SYMBOL(<line>).
            <line>-selected = 'X'.
            <line>-row_color = 'C500'.
          ENDLOOP.
        ELSE.
          LOOP AT lt_rows INTO DATA(ls_rows).
            READ TABLE <outtab> INDEX ls_rows-index ASSIGNING <line>.
            IF sy-subrc EQ 0.
              <line>-selected = 'X'.
              <line>-row_color = 'C500'.
            ENDIF.
          ENDLOOP.
        ENDIF.

      WHEN '&SELECTCAN'.
        LOOP AT <outtab> ASSIGNING <line> WHERE selected = 'X'.
          <line>-selected = ''.
          <line>-row_color = ''.
        ENDLOOP.

      WHEN '&SURE'.
        c_falv->check_changed_data( ).
        sure_ref( ).

    ENDCASE.


  ENDMETHOD.


  METHOD act_run.
    READ TABLE act INTO DATA(ls_cat) WITH KEY act = fcode action = action.
    IF sy-subrc NE 0.
      READ TABLE act INTO ls_cat WITH KEY act = fcode action = ''.
    ENDIF.
    CHECK ls_cat-rule_name IS NOT INITIAL.
    zafo_run=>run( rule_name = ls_cat-rule_name  head = head item = item )->call_bapi( IMPORTING ret = DATA(ret) ).
    message->add_table( ret-return ).
    IF ret-head IS NOT INITIAL.
      head = ret-head.
      item = ret-item.
    ENDIF.
  ENDMETHOD.


  METHOD add_gos_relationship.

    DATA: borident1 TYPE borident.
    DATA: borident2 TYPE borident.
    CHECK head-afono IS NOT INITIAL.
    CHECK head-objtype IS NOT INITIAL.

    IF gos_manager IS BOUND.
      FREE gos_manager.
    ENDIF.

    IF head-docnr IS NOT INITIAL.

      borident1-objkey = head-docnr.
      borident1-objtype = head-objtype.
      borident2-objkey = head-afono.
      borident2-objtype = 'ZAFO'.

      CALL FUNCTION 'BINARY_RELATION_CREATE'
        EXPORTING
          obj_rolea      = borident1
          obj_roleb      = borident2
          relationtype   = 'VORL'
        EXCEPTIONS
          no_model       = 1
          internal_error = 2
          unknown        = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.


    LOOP AT item INTO DATA(l_item) WHERE docnr <> ''.
      borident1-objkey = l_item-docnr.
      borident1-objtype = head-objtype.
      borident2-objkey = head-afono.
      borident2-objtype = 'ZAFO'.

      CALL FUNCTION 'BINARY_RELATION_CREATE'
        EXPORTING
          obj_rolea      = borident1
          obj_roleb      = borident2
          relationtype   = 'VORL'
        EXCEPTIONS
          no_model       = 1
          internal_error = 2
          unknown        = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD cancel.
    CHECK head-status = 'S'.
    CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '06' bustyp = head-bustyp werks = werks ).
    CHECK zwft_common=>confirm( '确认冲销么' ) = abap_true.
    ACT_RUN( '&CANCEL' ).

    macro_error_return.
    update_head_status( 'D' ).
    set_ref_status( ).
  ENDMETHOD.


  METHOD create_by_data.
    r_class = NEW zafo_class( bustyp = i_bustyp werks = i_werks ).
    r_class->called = abap_true.
    macro_static_error_return.

    MOVE-CORRESPONDING is_head TO r_class->head.
    MOVE-CORRESPONDING it_item TO r_class->item.

    r_class->init_head( ).
    r_class->init_head_text( ).
    LOOP AT r_class->item ASSIGNING FIELD-SYMBOL(<item>).
      r_class->init_item_line( EXPORTING index = sy-tabix
                                           CHANGING c_item = <item> ).
      IF <item>-price_long IS NOT INITIAL.
        r_class->set_price( CHANGING cs_item = <item> ).
      ENDIF.
      r_class->set_amount( CHANGING cs_item = <item> ).
    ENDLOOP.
    r_class->set_matnr_info( ).
    r_class->set_status( 'A' ).
    r_class->set_action( 'CREATE' ).
    r_class->save( ).
  ENDMETHOD.


  METHOD editer_editor_text.
    DATA:
          thead TYPE thead.
    DATA: change_made TYPE flag.
    DATA: texteditor TYPE hrpad_t_texteditor.

    CHECK editor IS BOUND.

    CALL METHOD cl_gui_cfw=>flush.

    editor->get_text_as_stream( IMPORTING text = texteditor ).

    CALL FUNCTION 'HR_CALL_TEXTEDITOR'
      IMPORTING
        change_made = change_made
      CHANGING
        editor_text = texteditor.

    CHECK change_made IS NOT INITIAL.

    IF readonly = abap_true.
      MESSAGE text-014 TYPE 'I'.
      RETURN.
    ENDIF.

    editor->set_text_as_stream(  text = texteditor ) .

  ENDMETHOD.


  METHOD get_next_afonr.
    LOOP AT item ASSIGNING FIELD-SYMBOL(<item>).
      IF <item>-afonr > next_afonr.
        next_afonr = <item>-afonr.
      ENDIF.
    ENDLOOP.

    IF head-afono IS NOT INITIAL.
      SELECT MAX( afonr ) INTO @DATA(max_afonr)
            FROM zafo_item WHERE afono = @head-afono.
      IF sy-subrc EQ 0 AND max_afonr > next_afonr.
        next_afonr = max_afonr.
      ENDIF.
    ENDIF.

    ADD 1 TO next_afonr.
  ENDMETHOD.


  METHOD gos_call.
    CHECK head-afono IS NOT INITIAL.
    CHECK gos_manager IS NOT INITIAL.
    DATA:l_sgs_srvnam TYPE sgs_srvnam.
    DATA:l_object TYPE borident.
    l_sgs_srvnam = 'SRELATIONS'.
    l_object-objkey = head-afono.
    l_object-objtype = 'ZAFO'.
    CALL METHOD gos_manager->start_service_direct
      EXPORTING
        ip_service       = l_sgs_srvnam
        is_object        = l_object
      EXCEPTIONS
        no_object        = 1
        object_invalid   = 2
        execution_failed = 3
        OTHERS           = 4.
  ENDMETHOD.


  METHOD post.
    CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '06' bustyp = head-bustyp werks = werks ).
    CHECK zwft_common=>confirm( '确认过账么' ) = abap_true.
    CHECK head-status = 'A'.
    act_run( '&POST' ).
    macro_error_return.

    add_gos_relationship( ).
    item_db = CORRESPONDING #( item ).
    db_save( ).
    update_head_status( 'S' ).
    set_ref_status( ).

  ENDMETHOD.


  METHOD print.
    CHECK head-status <> '' .
    CHECK head-status <> 'D' .
    CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '04' bustyp = head-bustyp werks = werks ).

    zafo_print_excel=>show_excel_by_data( rule_name = 'PO_TEST' i_head = head i_item = item ).

  ENDMETHOD.


  METHOD user_command_item.
    FIELD-SYMBOLS: <outtab> TYPE zafo_tt_sitem.
    falv_item->check_changed_data( ).
    ASSIGN falv_item->outtab->* TO <outtab>.
    CASE i_ucomm.
      WHEN '&ADD'.
        APPEND INITIAL LINE TO <outtab> ASSIGNING FIELD-SYMBOL(<item>).
        init_item_line( EXPORTING index = get_next_afonr( ) CHANGING c_item = <item> ) .
        falv_item->soft_refresh( ).
      WHEN '&DEL'.
        falv_item->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).
        CHECK lt_rows IS NOT INITIAL.
        CHECK zwft_common=>confirm( TEXT-008 ) = abap_true.
        LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>).
          READ TABLE <outtab> INDEX <row>-index ASSIGNING <item>.
          CHECK sy-subrc EQ 0.
          IF <item>-item_status IS INITIAL.
            <item>-item_status = 'E'.
          ELSE.
            SELECT SINGLE afono INTO @DATA(ls_afono)
              FROM zafo_item WHERE afono = @<item>-afono AND afonr = @<item>-afonr.
            IF sy-subrc EQ 0.
              <item>-item_status = 'D'.
              <item>-del_flag = 'X'.
              set_icon( EXPORTING status = <item>-item_status IMPORTING icon = <item>-icon text = <item>-text  ).
            ELSE.
              <item>-item_status = 'E'.
            ENDIF.
          ENDIF.
        ENDLOOP.
        DELETE <outtab> WHERE item_status = 'E' .
        falv_item->soft_refresh( ).
    ENDCASE.
  ENDMETHOD.


  METHOD user_command_report.
    CASE i_ucomm.
      WHEN '&DETAIL'.
        CLEAR item_dis.
        falv_dis_head->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).
        LOOP AT lt_rows INTO DATA(row).
          READ TABLE head_dis INTO DATA(head_line) INDEX row-index.
          DATA(lt_item) = VALUE zafo_tt_sitem( FOR wa IN item_all WHERE ( afono = head_line-afono ) ( wa )  ).
          APPEND LINES OF lt_item TO item_dis.
        ENDLOOP.
        falv_dis_item->soft_refresh( ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_material.
    DATA matnr TYPE matnr.
    DATA range_mtart TYPE md_range_t_mtart.
    range_mtart = VALUE #( FOR wa IN dict
                                             WHERE ( fieldname = 'MTART' )
                                             ( sign = 'I' option = 'EQ' low = wa-dict_value ) ) .
    zwft_common=>search_material( EXPORTING werks = head-werks range_mtart = range_mtart
    CHANGING matnr = cs_item-matnr ).
    IF cs_item-matnr IS NOT INITIAL.
      SELECT SINGLE mara~matnr,meins,maktx FROM mara INNER JOIN makt
      ON mara~matnr = makt~matnr
      WHERE mara~matnr = @cs_item-matnr
      INTO CORRESPONDING FIELDS OF @cs_item.
    ELSE.
      CLEAR: cs_item-meins,cs_item-maktx.
    ENDIF.
  ENDMETHOD.


  METHOD onf4.
    READ TABLE item INDEX i_index ASSIGNING FIELD-SYMBOL(<line>).
    CASE i_fieldname.
      WHEN 'MATNR' .
        IF <line>-matnr IS INITIAL.
          <line>-matnr = '*'.
        ENDIF.
        set_material( CHANGING cs_item = <line> ).
        c_event_data->m_event_handled = abap_true.
    ENDCASE.
    c_falv->soft_refresh( ).
  ENDMETHOD.


  METHOD register_f4.
    DATA f4 TYPE lvc_t_f4.

    APPEND VALUE #( fieldname = 'MATNR'
                                    register  = abap_true
                                    getbefore  = abap_true
                                    chngeafter = abap_true
                                    internal   = space
                                ) TO f4.
*    READ TABLE c_falv->fcat WITH KEY fieldname = 'MATNR' ASSIGNING FIELD-SYMBOL(<fcat>).
*    IF sy-subrc EQ 0.
*      <fcat>-f4availabl = ''.
*    ENDIF.

    c_falv->register_f4_for_fields( f4 ).
  ENDMETHOD.


  METHOD select_unique_ref.
    LOOP AT item_ref ASSIGNING FIELD-SYMBOL(<line>).
      IF <line>-afono = afono.
        <line>-selected = selected.
      ELSE.
        <line>-selected = COND #( WHEN selected = 'X' THEN '' ELSE 'X' ).
      ENDIF.

      <line>-row_color = COND #( WHEN <line>-selected = 'X' THEN 'C500' ELSE '' ).
    ENDLOOP.
  ENDMETHOD.


  METHOD set_dynamic_screen.
    DATA sturcname TYPE fieldname.
    DATA fieldname TYPE fieldname.
    DATA count TYPE i.
    CHECK object-head_dynnr >= 9000.
    CHECK dynamic_screen IS INITIAL.
    DATA(screens) = VALUE zafo_tt_screen( FOR wa IN screen_con
                                               WHERE ( fieldalv = 'HEAD' ) ( wa ) ).
    DATA(fix_screens) = zwft_common=>get_dynnr_field( dynnr = '0400' progname = 'SAPLZAFO' ).
    LOOP AT fix_screens INTO DATA(fix_screen).
      REPLACE '->' IN fix_screen-fnam WITH '%%'.
      SPLIT fix_screen-fnam AT '-' INTO sturcname fieldname.
      READ TABLE screens WITH KEY fieldname = fieldname ASSIGNING FIELD-SYMBOL(<screen>).
      IF sy-subrc EQ 0.
        <screen>-fieldname = ''.
      ENDIF.
    ENDLOOP.
    DELETE screens WHERE fieldname = ''.
    DELETE screens WHERE fieldname CP '*_NAME'.

    dynamic_screen = zWFT_dynamic_screen=>create( prog = 'SAPLZAFO' dynnr = object-head_dynnr type = 'I' ).
    dynamic_screen->set_matchcode( ).
    dynamic_screen->set_flow_logic( pbo = VALUE #( ( line = '  MODULE SCREEN_MOD_HEAD.' ) )
                                                         pai = VALUE #( ( line = '  MODULE GET_CURSOR.' ) ) ).
    DATA(half) = lines( screens ) / 2.
    CLEAR count.
    LOOP AT screens ASSIGNING <screen>.
      ADD 1 TO count.
      dynamic_screen->add_input_field( EXPORTING name = |<GS_TEXT>-{ <screen>-fieldname }| leng = 8 input = '' ).
      dynamic_screen->add_input_field( EXPORTING name = |<IO_CLASS>->HEAD-{ <screen>-fieldname }| f4 = abap_true ).
      dynamic_screen->add_flow_logic_chain( EXPORTING name = |<IO_CLASS>->HEAD-{ <screen>-fieldname }|
                                                                                           code = |FIELD <IO_CLASS>->HEAD-{ <screen>-fieldname } MODULE HEAD-{ <screen>-fieldname } ON CHAIN-REQUEST.| ).
      fieldname = |{ <screen>-fieldname }_NAME|.
      ASSIGN COMPONENT fieldname OF STRUCTURE head TO FIELD-SYMBOL(<value>).
      IF sy-subrc EQ 0.
        dynamic_screen->add_input_field( EXPORTING name = |<IO_CLASS>->HEAD-{ fieldname }| ).
      ENDIF.
      dynamic_screen->move_right( dynamic_screen->default_text_shift ).
      IF count = half.
        dynamic_screen->move_down( 1 ) .
      ENDIF.
    ENDLOOP.
    dynamic_screen->generate( ).
  ENDMETHOD.


  METHOD value_filter_by_dict.
    DATA(lt_dict) = VALUE zafo_tt_dict( FOR wa IN me->dict
                                  WHERE ( fieldname = fieldname
                                                  AND ( werks = me->werks OR werks = '' ) ) ( wa ) ).
    CHECK lt_dict IS NOT INITIAL.


    LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
      DATA(tabix) = sy-tabix.
      ASSIGN COMPONENT fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
      CHECK sy-subrc EQ 0.
      READ TABLE lt_dict INTO DATA(ls_dict) WITH KEY dict_value = <value>.
      IF sy-subrc NE 0.
        DELETE table INDEX tabix.
      ELSE.
        IF ls_dict-dict_name IS NOT INITIAL.
          ASSIGN COMPONENT 'NAME' OF STRUCTURE <line> TO FIELD-SYMBOL(<name>).
          CHECK sy-subrc EQ 0.
          <name> = ls_dict-dict_name.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD approval.
    SELECT SINGLE bustyp INTO @DATA(i_bustyp)
      FROM zafo_head
      WHERE afono = @afono.
    IF sy-subrc NE 0.
      error = abap_true.
      RETURN.
    ENDIF.

    DATA(r_class) = NEW zafo_class( bustyp = i_bustyp ).
    r_class->lock( ).
    IF r_class->locked EQ abap_true.
      error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zafo_head
      INTO CORRESPONDING FIELDS OF r_class->head
      WHERE afono = afono .

    IF ( r_class->head-status = 'B' AND status = 'C' )
      OR (  r_class->head-status = 'C' AND status = 'B' ).
    ELSE.
      error = abap_true.
      RETURN.
    ENDIF.
    r_class->update_head_status( status ).

  ENDMETHOD.
ENDCLASS.
