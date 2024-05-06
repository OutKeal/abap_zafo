class ZAFO_RUN definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF msty_bapi_ret ,
        head   TYPE zafo_shead,
        item   TYPE zafo_tt_sitem,
        msgty  TYPE msgty,
        msgno  TYPE msgno,
        return TYPE bapiret2_t,
      END OF msty_bapi_ret .
  types:
    BEGIN OF msty_bapi_variable.
    TYPES bapi_paramclass TYPE rs38l_kind.
    TYPES bapi_parameter TYPE rs38l_par_.
    TYPES bapi_kind TYPE abap_typekind.
    TYPES bapi_variable TYPE REF TO data.
    TYPES END OF msty_bapi_variable .
  types:
    mtty_bapi_variables TYPE TABLE OF msty_bapi_variable .
  types:
    BEGIN OF msty_rule_detail .
        INCLUDE TYPE zafo_rule_detail.
    TYPES: routine_no TYPE progname.
    TYPES:  END OF msty_rule_detail .
  types:
    mtty_rule_detail TYPE TABLE OF msty_rule_detail .
  types:
    mtty_rule_code TYPE TABLE OF zafo_rule_code .
  types MSTY_CODE type STRING .
  types:
    mtty_code      TYPE STANDARD TABLE OF msty_code .

  data RULE type ZAFO_RULE .
  data RULE_DETAIL type MTTY_RULE_DETAIL .
  data PARAMS type BGRFC_FUNINT_T .
  data VARS type MTTY_BAPI_VARIABLES .
  data HEAD type ZAFO_SHEAD .
  data ITEM type ZAFO_TT_SITEM .
  data L_ITEM type ZAFO_SITEM .
  data RET type MSTY_BAPI_RET .
  data RULE_CODE type MTTY_RULE_CODE .
  data PROGNAME type PROGNAME .
  class-data ABAP_TEMPLATE type PROGNAME value 'zafo_RULE_ROUTINE_TEMPLATE' ##NO_TEXT.

  class-methods RUN
    importing
      !RULE_NAME type ZAFO_RULE_NAME
      value(HEAD) type ZAFO_SHEAD
      value(ITEM) type ZAFO_TT_SITEM
    returning
      value(R_OBJ) type ref to ZAFO_RUN .
  methods CONSTRUCTOR
    importing
      !RULE_NAME type ZAFO_RULE_NAME
      !HEAD type ZAFO_SHEAD
      !ITEM type ZAFO_TT_SITEM .
  methods CALL_BAPI
    exporting
      value(RET) type MSTY_BAPI_RET .
  class-methods GET_FUNCTION_INTERFACE
    importing
      value(I_FUNCNAME) type RS38L_FNAM
      value(ET_RSEXC) type RSFB_EXC optional
    returning
      value(RT_PARAMS) type BGRFC_FUNINT_T .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS trans_key_data .
    METHODS define_vars .
    METHODS define_var
      IMPORTING
        !params TYPE rfc_funint
      CHANGING
        !var    TYPE msty_bapi_variable .
    METHODS trans_data
      EXCEPTIONS
        error .
    METHODS move_value
      IMPORTING
        VALUE(is_rule) TYPE msty_rule_detail
      CHANGING
        !value         TYPE any .
    METHODS move_valuex
      IMPORTING
        VALUE(value_old) TYPE any
      CHANGING
        !value           TYPE any .
    METHODS trans_return
      IMPORTING
        VALUE(ptab) TYPE abap_func_parmbind_tab
        VALUE(etab) TYPE abap_func_excpbind_tab .
    METHODS break
      IMPORTING
        VALUE(iv_param) TYPE usr05-parid DEFAULT 'ESP' .
    METHODS set_routine .
    METHODS get_source
      IMPORTING
        VALUE(template) TYPE progname
      EXPORTING
        VALUE(source)   TYPE mtty_code .
    METHODS generic_routine_code
      IMPORTING
        VALUE(suffix)       TYPE char20
        VALUE(code)         TYPE mtty_code
      CHANGING
        VALUE(source)       TYPE mtty_code
      RETURNING
        VALUE(r_routine_no) TYPE progname .
    METHODS generic_routine_program
      IMPORTING
        VALUE(source)   TYPE mtty_code
      RETURNING
        VALUE(progname) TYPE progname .
    METHODS routine_move_value
      IMPORTING
        !is_rule      TYPE msty_rule_detail
        VALUE(fline)  TYPE any OPTIONAL
        VALUE(fvalue) TYPE any OPTIONAL
      CHANGING
        VALUE(tvalue) TYPE any .
    METHODS routine_before_post .
    METHODS routine_after_post .
ENDCLASS.



CLASS ZAFO_RUN IMPLEMENTATION.


  METHOD break.
    DATA lv_param_val TYPE xuvalue .
    CALL FUNCTION 'G_GET_USER_PARAMETER'
      EXPORTING
        parameter_id    = iv_param
      IMPORTING
        parameter_value = lv_param_val
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.
    IF lv_param_val = abap_true.
      BREAK-POINT.
    ENDIF.
  ENDMETHOD.


  METHOD call_bapi.

    DATA lt_ptab TYPE abap_func_parmbind_tab.
    DATA ls_ptab TYPE LINE OF abap_func_parmbind_tab.
    DATA lt_etab TYPE abap_func_excpbind_tab.
    DATA ls_etab TYPE LINE OF abap_func_excpbind_tab.

    ret = me->ret.
    CHECK me->ret-msgty <> 'E'.

    routine_before_post( ).
    LOOP AT vars INTO DATA(l_vars).
      READ TABLE params INTO DATA(l_params) WITH KEY parameter = l_vars-bapi_parameter.
      CHECK sy-subrc EQ 0.
      ls_ptab-name = l_vars-bapi_parameter.
      ls_ptab-value = l_vars-bapi_variable.
      CASE l_params-paramclass.
        WHEN 'I'.
          ls_ptab-kind = abap_func_exporting.
        WHEN 'E'.
          ls_ptab-kind = abap_func_importing.
        WHEN 'T'.
          ls_ptab-kind = abap_func_tables.
        WHEN 'C'.
          ls_ptab-kind = abap_func_changing.
      ENDCASE.
      INSERT ls_ptab INTO TABLE lt_ptab.
    ENDLOOP.


    CALL FUNCTION rule-bapi_name
      PARAMETER-TABLE lt_ptab
      EXCEPTION-TABLE lt_etab.
    IF lt_etab[] IS NOT INITIAL.
      ret = me->ret.
*      macro_go_error_return 008.
    ENDIF.
    routine_after_post( ).
    trans_return( EXPORTING ptab = lt_ptab etab = lt_etab  ).
    trans_key_data( ).
    ret = me->ret.

  ENDMETHOD.


  METHOD constructor.


    SELECT SINGLE * FROM zafo_rule INTO rule
    WHERE rule_name = rule_name."获取配置规则
    macro_go_error_return 015.

    SELECT * FROM zafo_rule_detail INTO TABLE rule_detail
    WHERE rule_name = rule_name."获取配置规则明细
    macro_go_error_return 015.

    SELECT * FROM zafo_rule_code INTO TABLE rule_code
    WHERE rule_name = rule_name."代码例程

    set_routine( ).
    me->head = head.
    me->item = item.

    params = zafo_run=>get_function_interface( rule-bapi_name ).
    macro_go_error_return 015.

    define_vars( )."定义参数传入结构VARS
    trans_data( )."给VARS参数传入结构赋值

  ENDMETHOD.


  METHOD define_var.

    var-bapi_parameter = params-parameter.
    var-bapi_paramclass = params-paramclass.
    CASE params-paramclass.
      WHEN 'I' OR 'E' OR 'C'."创建BAPI的传入/传出/修改结构,单行
        IF params-fieldname IS INITIAL."创建BAPI的单值结构
          CREATE DATA var-bapi_variable TYPE (params-tabname).
        ELSE.
          CREATE DATA var-bapi_variable TYPE (params-tabname).
          ASSIGN var-bapi_variable->* TO FIELD-SYMBOL(<data>).
          ASSIGN COMPONENT params-fieldname OF STRUCTURE <data> TO FIELD-SYMBOL(<field>).
          GET REFERENCE OF <field> INTO var-bapi_variable.
        ENDIF.
      WHEN 'T'."表对象
        CREATE DATA var-bapi_variable TYPE TABLE OF (params-tabname).
    ENDCASE.
    ASSIGN var-bapi_variable->* TO FIELD-SYMBOL(<ref>).
    var-bapi_kind = cl_abap_typedescr=>describe_by_data( <ref> )->type_kind.
  ENDMETHOD.


  METHOD define_vars.
    LOOP AT rule_detail  INTO DATA(l_rule)
          GROUP BY ( to_tabname = l_rule-to_tabname )
          INTO DATA(ls_group).
      READ TABLE params INTO DATA(l_params) WITH KEY parameter = ls_group-to_tabname.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO vars ASSIGNING FIELD-SYMBOL(<var>).
        define_var( EXPORTING params = l_params CHANGING var = <var> )."定义函数的配置内结构
        READ TABLE params INTO DATA(l_paramsx) WITH KEY parameter = ls_group-to_tabname && 'X'.
        IF sy-subrc EQ 0.
          APPEND INITIAL LINE TO vars ASSIGNING FIELD-SYMBOL(<varx>)."定义函数的配置内附加X结构
          define_var( EXPORTING params = l_paramsx CHANGING var = <varx> ).
        ENDIF.
      ELSE.
        macro_go_error_return 016.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD generic_routine_code.
    DATA idx TYPE sy-tabix.
    DATA(l_suffix) = suffix.
    TRANSLATE l_suffix TO LOWER  CASE.

    FIND |* implementation_{ l_suffix }|  IN TABLE source MATCH LINE idx.
    macro_go_error_return 020.

    DELETE source INDEX idx.
    INSERT LINES OF code INTO source INDEX idx.
    macro_go_error_return 020.

    r_routine_no = |ROUTINE_{ suffix }|.
  ENDMETHOD.


  METHOD generic_routine_program.
    DATA: mess     TYPE string,
          lin      TYPE i ##needed,
          wrd      TYPE string ##needed,
          warnings TYPE  STANDARD TABLE OF rslinlmsg.

    SYNTAX-CHECK FOR source MESSAGE mess LINE lin WORD wrd
    ID 'MSG' TABLE warnings
    PROGRAM abap_template.
    macro_go_error_return 021.
    TRY.
        GENERATE SUBROUTINE POOL source NAME progname.
      CATCH cx_sy_generate_subpool_full.
        MESSAGE TEXT-srf TYPE 'I' DISPLAY LIKE 'E'.
        sy-subrc = 4.
    ENDTRY.
    macro_go_error_return 021.

  ENDMETHOD.


  METHOD get_function_interface.

    DATA:lt_params TYPE bgrfc_funint_t.
    CALL FUNCTION 'RFC_GET_FUNCTION_INTERFACE'
      EXPORTING
        funcname             = i_funcname
      TABLES
        params               = lt_params
        resumable_exceptions = et_rsexc
      EXCEPTIONS
        fu_not_found         = 1
        nametab_fault        = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    SELECT * INTO TABLE @DATA(lt_fupa) FROM fupararef
          WHERE funcname = @i_funcname
          ORDER BY paramtype, pposition.

    LOOP AT lt_fupa INTO DATA(ls_fupa) WHERE paramtype = 'I'.
      READ TABLE lt_params INTO DATA(ls_params) WITH KEY parameter = ls_fupa-parameter.
      IF sy-subrc EQ 0.
        APPEND ls_params TO rt_params.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_fupa INTO ls_fupa WHERE paramtype = 'E'.
      READ TABLE lt_params INTO ls_params WITH KEY parameter = ls_fupa-parameter.
      IF sy-subrc EQ 0.
        APPEND ls_params TO rt_params.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_fupa INTO ls_fupa WHERE paramtype = 'C'.
      READ TABLE lt_params INTO ls_params WITH KEY parameter = ls_fupa-parameter.
      IF sy-subrc EQ 0.
        APPEND ls_params TO rt_params.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_fupa INTO ls_fupa WHERE paramtype = 'T'.
      READ TABLE lt_params INTO ls_params WITH KEY parameter = ls_fupa-parameter.
      IF sy-subrc EQ 0.
        APPEND ls_params TO rt_params.
      ENDIF.
    ENDLOOP.




  ENDMETHOD.


  METHOD get_source.
    TRY.
        READ REPORT template INTO source.
      CATCH cx_sy_read_src_line_too_long.
        sy-subrc = 4.
    ENDTRY.
  ENDMETHOD.


  METHOD move_value.
    CASE  is_rule-from_fieldalv.
      WHEN ' '.
        IF is_rule-default_value+0(3) = 'SY-'.
          ASSIGN is_rule-default_value TO FIELD-SYMBOL(<default_value>).
          IF sy-subrc EQ 0.
            is_rule-default_value = <default_value>.
          ENDIF.
        ENDIF.
        routine_move_value( EXPORTING is_rule = is_rule fvalue = is_rule-default_value CHANGING tvalue = value ).
      WHEN 'HEAD'.
        ASSIGN COMPONENT is_rule-from_fname OF STRUCTURE head TO FIELD-SYMBOL(<from_f>).
        macro_go_error_return 017.
        routine_move_value( EXPORTING is_rule = is_rule fline = head fvalue = <from_f> CHANGING tvalue = value ).
      WHEN 'ITEM'.
        ASSIGN COMPONENT is_rule-from_fname OF STRUCTURE l_item TO <from_f>.
        macro_go_error_return 017.
        routine_move_value( EXPORTING is_rule = is_rule fline = l_item fvalue = <from_f> CHANGING tvalue = value ).
    ENDCASE.
  ENDMETHOD.


  METHOD move_valuex.
    IF cl_abap_typedescr=>describe_by_data( value_old )->absolute_name
    = cl_abap_typedescr=>describe_by_data( value )->absolute_name .
      value = value_old.
    ELSE.
      value = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD routine_after_post.

    DATA class TYPE string.
    CHECK progname IS NOT INITIAL.
    IF progname IS NOT INITIAL.
      class = `\PROGRAM=` && progname && `\CLASS=ROUTINE`.
      TRY.
          CALL METHOD (class)=>routine_after_post
            EXPORTING
              run  = me
            CHANGING
              vars = vars.
        CATCH cx_root INTO DATA(exc) ##CATCH_ALL.
          MESSAGE exc->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
          sy-subrc = 4.
          RETURN.
      ENDTRY.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD routine_before_post.

    DATA class TYPE string.
    CHECK progname IS NOT INITIAL.
    IF progname IS NOT INITIAL.
      class = `\PROGRAM=` && progname && `\CLASS=ROUTINE`.
      TRY.
          CALL METHOD (class)=>routine_before_post
            EXPORTING
              run   = me
            CHANGING
              vars = vars.
        CATCH cx_root INTO DATA(exc) ##CATCH_ALL.
          MESSAGE exc->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
          sy-subrc = 4.
          RETURN.
      ENDTRY.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD routine_move_value.
    DATA class TYPE string.
    IF progname IS NOT INITIAL
    AND is_rule-routine_no IS NOT INITIAL.
      class = `\PROGRAM=` && progname && `\CLASS=ROUTINE`.
      TRY.
          CALL METHOD (class)=>(is_rule-routine_no)
            EXPORTING
              run     = me
              is_rule = is_rule
              i_line  = fline
              i_value = fvalue
            CHANGING
              c_value = tvalue.
        CATCH cx_root INTO DATA(exc) ##CATCH_ALL.
          MESSAGE exc->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
          sy-subrc = 4.
          RETURN.
      ENDTRY.
      RETURN.
    ENDIF.

    TRY.
        tvalue = fvalue.
      CATCH cx_root.
        sy-subrc = 8.
        macro_go_error_return 022.
    ENDTRY.

  ENDMETHOD.


  METHOD RUN.
    r_obj = NEW zafo_run(
    rule_name = rule_name
    head = head
    item = item ).
  ENDMETHOD.


  METHOD set_routine.
    DATA:code TYPE mtty_code.
    DATA:l_suffix TYPE char20.
    DATA:l_value_index TYPE int1.
    CHECK rule_code IS NOT INITIAL.
    l_value_index = 100.
    get_source( EXPORTING template = abap_template IMPORTING source = DATA(source) ).
    macro_go_error_return 018.
    LOOP AT rule_code INTO DATA(group1) WHERE action <> ''
          GROUP BY ( action = group1-action
          to_tabname = group1-to_tabname
          to_fieldname = group1-to_fieldname )
          INTO DATA(lg_rule_code).

      CASE lg_rule_code-action.
        WHEN 'MOVE_VALUE'.
          ADD 1 TO l_value_index.
          IF l_value_index > 110.
            sy-subrc = 4.
            macro_go_error_return 019.
          ELSE.
            l_suffix = l_value_index.
            CONDENSE l_suffix NO-GAPS.
          ENDIF.
          READ TABLE rule_detail ASSIGNING FIELD-SYMBOL(<fs_rule_detail>)
          WITH KEY  to_tabname = lg_rule_code-to_tabname
          to_fieldname = lg_rule_code-to_fieldname.
          IF sy-subrc EQ 0.
            CLEAR code.
            LOOP AT GROUP lg_rule_code INTO DATA(ls_rule_code).
              APPEND INITIAL LINE TO code ASSIGNING FIELD-SYMBOL(<fs_code>).
              IF ls_rule_code-code IS NOT INITIAL.
                <fs_code> = ls_rule_code-code.
              ENDIF.
            ENDLOOP.
            CHECK code IS NOT INITIAL.
            <fs_rule_detail>-routine_no = generic_routine_code( EXPORTING suffix = l_suffix code = code CHANGING source = source ).
          ENDIF.
        WHEN 'BEFORE_POST'.
          CHECK lg_rule_code-to_tabname IS INITIAL AND lg_rule_code-to_fieldname IS INITIAL.
          l_suffix = lg_rule_code-action.
          CLEAR code.
          LOOP AT GROUP lg_rule_code INTO ls_rule_code.
            APPEND INITIAL LINE TO code ASSIGNING <fs_code>.
            IF ls_rule_code-code IS NOT INITIAL.
              <fs_code> = ls_rule_code-code.
            ENDIF.
          ENDLOOP.
          CHECK code IS NOT INITIAL.
          generic_routine_code( EXPORTING suffix = l_suffix code = code CHANGING source = source ).
        WHEN 'AFTER_POST'.
          CHECK lg_rule_code-to_tabname IS INITIAL AND lg_rule_code-to_fieldname IS INITIAL.
          l_suffix = lg_rule_code-action.
          CLEAR code.
          LOOP AT GROUP lg_rule_code INTO ls_rule_code.
            APPEND INITIAL LINE TO code ASSIGNING <fs_code>.
            IF ls_rule_code-code IS NOT INITIAL.
              <fs_code> = ls_rule_code-code.
            ENDIF.
          ENDLOOP.
          CHECK code IS NOT INITIAL.
          generic_routine_code( EXPORTING suffix = l_suffix code = code CHANGING source = source ).
      ENDCASE.
    ENDLOOP.
    IF source IS NOT INITIAL.
      progname = generic_routine_program( source ).
    ENDIF.

  ENDMETHOD.


  METHOD trans_data.


    FIELD-SYMBOLS:
      <to_t>  TYPE table,
      <to_tx> TYPE table,
      <to_s>  TYPE any,
      <to_sx> TYPE any,
      <to_f>  TYPE any,
      <to_fx> TYPE any.

    SORT rule_detail BY  rule_type to_tabname.

    LOOP AT rule_detail INTO DATA(lg_rule)
          WHERE from_fieldalv <> 'RET'
          GROUP BY ( rule_type = lg_rule-rule_type
          to_tabname = lg_rule-to_tabname
    ) INTO DATA(l_rule).
      READ TABLE vars ASSIGNING FIELD-SYMBOL(<vars>) WITH KEY bapi_parameter = l_rule-to_tabname.
      macro_go_error_return 017.
      READ TABLE vars ASSIGNING FIELD-SYMBOL(<varsx>) WITH KEY bapi_parameter = l_rule-to_tabname && 'X'.
      DATA(xfield) = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_true ELSE abap_false ).
      CASE <vars>-bapi_kind.
        WHEN cl_abap_typedescr=>typekind_char
              OR cl_abap_typedescr=>typekind_int
              OR cl_abap_typedescr=>typekind_date
              OR cl_abap_typedescr=>typekind_packed."'C' OR 'I' OR 'P'.
          ASSIGN <vars>-bapi_variable->* TO <to_f>.
          LOOP AT GROUP l_rule INTO DATA(ls_rule).
            move_value( EXPORTING is_rule = ls_rule CHANGING value = <to_f> ).
            macro_go_error_return 017.
          ENDLOOP.
        WHEN cl_abap_typedescr=>typekind_struct1."'u'.
          ASSIGN <vars>-bapi_variable->* TO <to_s>.
          LOOP AT GROUP l_rule INTO ls_rule.
            ASSIGN COMPONENT ls_rule-to_fieldname OF STRUCTURE <to_s> TO <to_f>.
            macro_go_error_return 017.
            move_value( EXPORTING is_rule = ls_rule CHANGING value = <to_f> ).
            macro_go_error_return 017.
            IF xfield = abap_true.
              ASSIGN <varsx>-bapi_variable->* TO <to_sx>.
              ASSIGN COMPONENT ls_rule-to_fieldname OF STRUCTURE <to_sx> TO <to_fx>.
              CHECK sy-subrc EQ 0.
              move_valuex( EXPORTING value_old = <to_f> CHANGING value = <to_fx> ).
            ENDIF.
          ENDLOOP.
        WHEN cl_abap_typedescr=>typekind_table. "'h'.
          ASSIGN <vars>-bapi_variable->* TO <to_t>.
          IF xfield = abap_true.
            ASSIGN <varsx>-bapi_variable->* TO <to_tx>.
          ENDIF.
          LOOP AT item INTO l_item.
            APPEND INITIAL LINE TO <to_t> ASSIGNING <to_s>.
            IF xfield = abap_true.
              APPEND INITIAL LINE TO <to_tx> ASSIGNING <to_sx>.
            ENDIF.
            LOOP AT GROUP l_rule INTO ls_rule.
              ASSIGN COMPONENT ls_rule-to_fieldname  OF STRUCTURE <to_s> TO <to_f>.
              macro_go_error_return 017.
              move_value( EXPORTING is_rule = ls_rule CHANGING value = <to_f> ).
              macro_go_error_return 017.
              IF xfield = abap_true.
                ASSIGN COMPONENT ls_rule-to_fieldname OF STRUCTURE <to_sx> TO <to_fx>.
                CHECK sy-subrc EQ 0.
                move_valuex( EXPORTING value_old = <to_f> CHANGING value = <to_fx> ).
              ENDIF.
            ENDLOOP.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD trans_key_data.
    DATA docnr TYPE docnr.
    DATA mjahr TYPE mjahr.

    ret-head-objtype = rule-objtype.
    CASE rule-objtype.
      WHEN 'BUS2017'.
        LOOP AT ret-item ASSIGNING FIELD-SYMBOL(<item>).
          <item>-mblnr = ret-head-mblnr.
          <item>-mjahr = ret-head-mjahr.
        ENDLOOP.
        ret-head-docnr = ret-head-mblnr && ret-head-mjahr.
      WHEN 'BUS2012'.
        ret-head-ebeln = COND ebeln( WHEN ret-head-ebeln IS INITIAL THEN  ret-head-docnr ELSE ret-head-ebeln ).
        LOOP AT ret-item ASSIGNING <item>.
          <item>-ebeln = ret-head-ebeln.
          <item>-ebelp = <item>-afonr.
        ENDLOOP.
      WHEN 'BUS2105'.
        ret-head-banfn = COND banfn( WHEN ret-head-banfn IS INITIAL THEN  ret-head-docnr ELSE ret-head-banfn ).
        LOOP AT ret-item ASSIGNING <item>.
          <item>-banfn = ret-head-banfn.
          <item>-bnfpo = <item>-afonr.
        ENDLOOP.
      WHEN 'BUS2032' OR 'VBAK'.
        ret-head-vbeln_va = COND vbeln_va( WHEN ret-head-vbeln_va IS INITIAL THEN  ret-head-docnr ELSE ret-head-vbeln_va ).
        LOOP AT ret-item ASSIGNING <item>.
          <item>-vbeln_va = ret-head-vbeln_va.
          <item>-posnr_va = <item>-afonr.
        ENDLOOP.
      WHEN 'LIKP'.
        ret-head-vbeln_vl = COND vbeln_vl( WHEN ret-head-vbeln_vl IS INITIAL THEN  ret-head-docnr ELSE ret-head-vbeln_vl ).
        LOOP AT ret-item ASSIGNING <item>.
          <item>-vbeln_vl = ret-head-vbeln_vl.
          <item>-vbeln_vl = <item>-afonr.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.


  METHOD trans_return.

    DATA:fieldname TYPE char40.

    FIELD-SYMBOLS:
      <from_t> TYPE table,
      <from_s> TYPE any,
      <from_f> TYPE any,
      <value>  TYPE any.

    ret-head = head.
    ret-item = item.

    LOOP AT rule_detail INTO DATA(lg_rule)
          WHERE rule_type = 'R'
          GROUP BY ( rule_type = lg_rule-rule_type
          to_tabname = lg_rule-to_tabname
    ) INTO DATA(l_rule).
      READ TABLE ptab ASSIGNING FIELD-SYMBOL(<ptab>) WITH KEY name = l_rule-to_tabname.
      IF sy-subrc NE 0.
        macro_go_error_return 016.
      ENDIF.

      READ TABLE params INTO DATA(l_params) WITH KEY parameter = l_rule-to_tabname.
      IF sy-subrc NE 0.
        macro_go_error_return 016.
      ENDIF.

      CASE <ptab>-kind.
        WHEN  abap_func_importing.
          IF l_params-fieldname IS INITIAL.
            ASSIGN <ptab>-value->* TO <from_s>.
            IF l_params-tabname = 'BAPIRET1'
            OR l_params-tabname = 'BAPIRET2'
            OR l_params-tabname = 'BAPIVBRKERRORS'.
              APPEND INITIAL LINE TO ret-return ASSIGNING FIELD-SYMBOL(<line>).
              MOVE-CORRESPONDING <from_s> TO <line>.
            ELSE.
              LOOP AT GROUP l_rule INTO DATA(ls_rule).
                ASSIGN COMPONENT ls_rule-to_fieldname OF STRUCTURE <from_s> TO <from_f>.
                CHECK sy-subrc EQ 0.
                fieldname = |{ ls_rule-from_fieldalv }-{ ls_rule-from_fname }| .
                ASSIGN (fieldname) TO <value>.
                IF sy-subrc EQ 0.
                  <value> = <from_f>.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ELSE.

            LOOP AT GROUP l_rule INTO ls_rule.
              ASSIGN <ptab>-value->* TO <from_f>.
              fieldname = |{ ls_rule-from_fieldalv }-{ ls_rule-from_fname }| .
              ASSIGN (fieldname) TO <value>.
              macro_go_error_return 016.
              <value> = <from_f>.
            ENDLOOP.
          ENDIF.
        WHEN abap_func_tables.
          ASSIGN <ptab>-value->* TO <from_t>.
          IF l_params-tabname = 'BAPIRET1'
          OR l_params-tabname = 'BAPIRET2'
          OR l_params-tabname = 'BAPI_CORU_RETURN'
          OR l_params-tabname = 'BAPIRETURN'
          OR l_params-tabname = 'BAPIVBRKERRORS'.
            LOOP AT <from_t> ASSIGNING <from_s>.
              APPEND INITIAL LINE TO ret-return ASSIGNING <line>.
              MOVE-CORRESPONDING <from_s> TO <line>.
              IF l_params-tabname = 'BAPIRETURN'.
                ASSIGN COMPONENT 'CODE' OF STRUCTURE <from_s> TO FIELD-SYMBOL(<code>).
                CHECK sy-subrc EQ 0.
                <line>-id = <code>+0(2).
                <line>-number = <code>+2(3).
              ENDIF.
            ENDLOOP.
          ELSE.
            LOOP AT <from_t> ASSIGNING <from_s>.
              LOOP AT GROUP l_rule INTO ls_rule.
                ASSIGN COMPONENT ls_rule-to_fieldname OF STRUCTURE <from_s> TO <from_f>.
                macro_go_error_return 016.
                ASSIGN COMPONENT ls_rule-from_fname OF STRUCTURE me->ret TO <value>.
                IF sy-subrc EQ 0.
                  <value> = <from_f>.
                  CONTINUE.
                ENDIF.
                APPEND INITIAL LINE TO ret-return ASSIGNING FIELD-SYMBOL(<return>).
                ASSIGN COMPONENT ls_rule-from_fname OF STRUCTURE <return> TO <value>.
                macro_go_error_return 016.
                <value> = <from_f>.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    ret-head = head.
    ret-item = item.

  ENDMETHOD.
ENDCLASS.
