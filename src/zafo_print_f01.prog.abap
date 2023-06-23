*&---------------------------------------------------------------------*
*& 包含               ZAFO_PRINT_F01
*&---------------------------------------------------------------------*


FORM frm_pop_model USING u_print_type.
  DATA:lt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  SELECT * FROM zafo_print
    INTO TABLE @DATA(lt_print)
    WHERE print_type = @u_print_type.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE   = ' '
      retfield         = 'MODEL'
      pvalkey          = 'MODEL'
      value_org        = 'S'
      callback_program = sy-repid
    TABLES
      value_tab        = lt_print
      return_tab       = lt_return_tab[]
*     DYNPFLD_MAPPING  =
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.

  READ TABLE lt_return_tab INDEX 1.
  IF sy-subrc EQ 0  AND lt_return_tab-fieldval IS NOT INITIAL..
    READ TABLE lt_print INTO gs_print WITH KEY model = lt_return_tab-fieldval.
  ELSE.
    CLEAR gs_print.
  ENDIF.

ENDFORM.


FORM frm_initial_smartforms USING smartform TYPE tdsfname .
  CLEAR control_parameters.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = smartform "SMARTFORMS名字
    IMPORTING
      fm_name            = func_module_name "得到的函数名
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.


FORM frm_print_report.
  DATA: l_ssfcrespd TYPE ssfcrespd.

  DATA:lt_print_i TYPE TABLE OF zafo_print_i.
  lt_print_i = gt_print_i[].
  DELETE lt_print_i WHERE afono <> gs_print_h-afono.

  CALL FUNCTION func_module_name
    EXPORTING
      control_parameters   = control_parameters
      gs_print_h           = gs_print_h
    IMPORTING
      document_output_info = l_ssfcrespd
    TABLES
      gt_print_i           = lt_print_i
    EXCEPTIONS
      user_cancled         = 4.

  IF sy-subrc = '4'.
  ELSEIF sy-subrc <> 0 .
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.



FORM frm_fix_head CHANGING cs_head TYPE zafo_shead.
  IF cs_head-bustyp = 'H4008'.

    cs_head-lgort = cs_head-umlgo.
    cs_head-lgort_name = cs_head-umlgo_name.
    cs_head-lifnr = cs_head-werks.
    cs_head-lifnr_name = cs_head-werks_name.
    cs_head-werks = cs_head-umwrk.
  ENDIF.
ENDFORM.


FORM frm_convert_meins USING us_item TYPE zafo_sitem CHANGING name1.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input  = us_item-meins
    IMPORTING
      output = name1.
  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_convert_bprme USING us_item TYPE zafo_sitem CHANGING name1.
  IF us_item-bprme IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = us_item-bprme
      IMPORTING
        output = name1.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = us_item-meins
      IMPORTING
        output = name1.
  ENDIF.
  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_convert_menge USING us_item TYPE zafo_sitem CHANGING name1.
  IF us_item-menge_cg IS NOT INITIAL.
    PERFORM frm_convert_zero USING us_item-menge_cg CHANGING name1.
  ELSE.
    PERFORM frm_convert_zero USING us_item-menge CHANGING name1.
  ENDIF.
  CONDENSE name1 NO-GAPS.

ENDFORM.


FORM frm_convert_zero USING value CHANGING name1.
  DATA : l_str  TYPE  string .

  l_str = value.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      value = l_str.
  FIND '.' IN l_str.
  CHECK value IS NOT INITIAL.
  CHECK sy-subrc EQ 0.

  IF  value  IS  NOT  INITIAL .
    CONDENSE  l_str .  " 去掉没用的小数位的0
    SHIFT  l_str RIGHT DELETING TRAILING  '0' . "去掉没用的小数位的0
    SHIFT  l_str RIGHT DELETING TRAILING  '.' . "去掉没用的小数位的0
  ELSE .
    l_str  =  '0' . "为空就给个空值
  ENDIF .
  CONDENSE  l_str .
  name1  =  l_str .
  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_convert_price USING value CHANGING name1.
  DATA:ls_price TYPE zafo_price_long.
  ls_price = value.
  PERFORM frm_convert_zero USING ls_price CHANGING name1.

ENDFORM.

FORM frm_get_menge_name USING  menge CHANGING name1.
  DATA:ls_menge TYPE char20.
  PERFORM frm_convert_zero USING menge CHANGING ls_menge.
  name1 = '总数:' && ls_menge.
  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_get_amount_name USING amount CHANGING name1.
  DATA:ls_amount TYPE char20.
  PERFORM frm_convert_zero USING amount CHANGING ls_amount.
  name1 = '总额:' && ls_amount.

  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_get_zdr_name USING text CHANGING name1.
  SELECT SINGLE name INTO @DATA(ls_name)
    FROM zapp_addr WHERE person = @text.

  name1 = '制单人:' &&  ls_name.
  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_get_cg_name USING text CHANGING name1.
  SELECT SINGLE name INTO @DATA(ls_name)
    FROM zapp_addr WHERE person = @text.
  IF sy-subrc EQ 0 .
    name1 = '仓管:' &&  ls_name.
  ELSE.
    SELECT SINGLE name INTO @ls_name
      FROM zapp_addr
      WHERE department = '仓库'
      AND zposition = '主任'.
    name1 = '仓库主管:' &&  ls_name.
  ENDIF.

  IF gs_print-print_type = 'GR40'.
    name1 = '仓库主管:' .
  ENDIF.

  CONDENSE name1 NO-GAPS.

ENDFORM.


FORM frm_get_cgy_name USING ebeln CHANGING name1.
  CHECK ebeln IS NOT INITIAL.
  SELECT SINGLE ernam INTO @DATA(ls_ernam)
    FROM ekko WHERE ebeln = @ebeln.

  SELECT SINGLE name INTO @DATA(ls_name)
    FROM zapp_addr WHERE person = @ls_ernam.
  name1 = '采购员:' && ls_name.
  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_get_date_name USING date CHANGING name1.
  DATA:year TYPE char4.
  DATA:month TYPE char2.
  DATA:day TYPE char2.

  IF date IS INITIAL .
    date = sy-datum.
  ENDIF.

  year = date+0(4).
  month = date+4(2).
  day = date+6(2).
  IF month+0(1) = '0'.
    month+0(1) = ''.
  ENDIF.

  IF day+0(1) = '0'.
    day+0(1) = ''.
  ENDIF.

  name1 = year && '年' && month && '月' && day && '日'.
  CONDENSE name1 NO-GAPS.

ENDFORM.


FORM frm_get_lifnr_name USING lifnr_name CHANGING name1.
  name1 = '供应商:' && lifnr_name.
  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_get_bukrs_name USING is_head TYPE zafo_shead CHANGING name1.

  CASE gs_print-model+0(2).
    WHEN 'RK'.
      IF is_head-umwrk_name IS NOT INITIAL.
        gt_print_h-title_txt1 = is_head-umwrk_name.
      ELSE.
        gt_print_h-title_txt1 = is_head-bukrs_name.
      ENDIF.
    WHEN 'CK'.
      gt_print_h-title_txt1 = is_head-bukrs_name.
  ENDCASE.

  CONDENSE name1 NO-GAPS.
ENDFORM.



FORM frm_get_kostl_name USING is_head TYPE zafo_shead CHANGING name1.

  IF is_head-kostl_name IS NOT INITIAL.
    CONCATENATE '部门:' is_head-kostl_name INTO name1 SEPARATED BY space.
  ENDIF.

  IF is_head-lifnr_name IS NOT INITIAL.
    CONCATENATE '供应商:' is_head-lifnr_name INTO name1 SEPARATED BY space.
  ENDIF.

  IF is_head-kunnr_name IS NOT INITIAL.
    CONCATENATE '客户:' is_head-kunnr_name INTO name1 SEPARATED BY space.

  ELSEIF is_head-umwrk_name IS NOT INITIAL.
    CONCATENATE '客户:' is_head-umwrk_name INTO name1 SEPARATED BY space.
  ENDIF.

  CONDENSE name1 NO-GAPS.

ENDFORM.


FORM frm_get_afono_name USING is_head TYPE zafo_shead CHANGING name1.
  DATA:ls_afono TYPE zafono.

  IF is_head-print_no IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = is_head-afono
      IMPORTING
        output = ls_afono.
    name1 = '单号' && ':' && ls_afono.
  ELSE.
    name1 = 'No.' && is_head-print_no.
  ENDIF.
  CONDENSE name1 NO-GAPS.
ENDFORM.


FORM frm_get_bustyp_name USING bustyp CHANGING name1.
  IF gs_print-ztitle IS NOT INITIAL.
    name1 = gs_print-ztitle.
    RETURN.
  ENDIF.

  SELECT SINGLE bustyp_name1 INTO name1
    FROM zafo_bustype WHERE bustyp = bustyp.
  IF sy-subrc NE 0 .
    name1 = '未指定单据类型'.
  ENDIF.

ENDFORM.


FORM frm_get_lgort_name USING werks lgort CHANGING name1.
  CHECK werks IS NOT INITIAL.
  CHECK lgort IS NOT INITIAL.

  SELECT SINGLE lgobe INTO name1
    FROM t001l
    WHERE werks = werks
    AND lgort = lgort.

  CONCATENATE '仓库:'  name1 INTO name1 SEPARATED BY space.
  CONDENSE name1 NO-GAPS.
ENDFORM.

FORM frm_get_x_name CHANGING name.
  IF name IS INITIAL.
    name = '否'.
  ELSE.
    name = '是'.
  ENDIF.

ENDFORM.
