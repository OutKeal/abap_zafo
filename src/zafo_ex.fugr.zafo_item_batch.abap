FUNCTION zafo_item_batch.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     REFERENCE(E_MODEL) TYPE  CHAR1
*"  TABLES
*"      UT_ITEM STRUCTURE  ZAFO_SITEM
*"      UT_FCAT TYPE  LVC_T_FCAT
*"      ET_ITEM STRUCTURE  ZAFO_SITEM
*"----------------------------------------------------------------------
  CLEAR gt_fcat[].

  gt_fcat[] = ut_fcat[].

  CLEAR g_model.
  CLEAR gt_model[].
  gt_model-model = 'A'.
  gt_model-name = '工厂款号/颜色/尺码/说明'.
  APPEND gt_model.

  gt_model-model = 'B'.
  gt_model-name = '工厂款号'.
  APPEND gt_model.

  gt_model-model = 'C'.
  gt_model-name = '说明-客户款号'.
  APPEND gt_model.

  gt_model-model = 'D'.
  gt_model-name = '销售合同'.
  APPEND gt_model.


  PERFORM frm_pop_model CHANGING g_model.

  CHECK g_model IS NOT INITIAL.

  e_model = g_model.

  CLEAR batch_item[].


  LOOP AT ut_item.
    batch_item-icon = icon_led_inactive.
    batch_item-matnr = ut_item-matnr.
    batch_item-maktx = ut_item-maktx.

    CASE g_model.
      WHEN 'A'.
        batch_item-idnlf = ut_item-idnlf.
        batch_item-zcolor = ut_item-zcolor.
        batch_item-zcolor_text_zh = ut_item-zcolor_text_zh.
        batch_item-zsize = ut_item-zsize.
        batch_item-znorms = ut_item-znorms.
        batch_item-zmm_tran_rate = ut_item-bprme.
      WHEN 'B'.
        batch_item-idnlf = ut_item-idnlf.
      WHEN 'C'.
        batch_item-znorms = ut_item-znorms.
      WHEN 'D'.
        batch_item-zzpino = ut_item-zzpino.
    ENDCASE.

    batch_item-menge_cg = ut_item-menge_cg.
    batch_item-bprme = ut_item-bprme.
    COLLECT batch_item.
    CLEAR batch_item.
  ENDLOOP.

  CLEAR sy-ucomm.

  CALL SCREEN 9001 STARTING AT 10 5 ENDING AT 130 20.

  et_item[] = batch_item[].

ENDFUNCTION.


FORM frm_pop_model CHANGING model.

  DATA:lt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE   = ' '
      retfield         = 'MODEL'
      pvalkey          = 'MODEL'
      value_org        = 'S'
      callback_program = sy-repid
    TABLES
      value_tab        = gt_model[]
      return_tab       = lt_return_tab[]
*     DYNPFLD_MAPPING  =
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.

  ENDIF.
  READ TABLE lt_return_tab INDEX 1.
  CHECK lt_return_tab-fieldval IS NOT INITIAL.

  READ TABLE gt_model INTO model WITH KEY model = lt_return_tab-fieldval.
  CLEAR gt_model[].

ENDFORM.
