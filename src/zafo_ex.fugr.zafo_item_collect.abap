FUNCTION zafo_item_collect.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      UT_ITEM STRUCTURE  ZAFO_SITEM
*"      UT_FCAT TYPE  LVC_T_FCAT
*"----------------------------------------------------------------------

  CLEAR gt_fcat[].
  gt_fcat[] = ut_fcat[].
*  gt_item[] = ct_item[].
  CLEAR g_model.
  CLEAR gt_model[].
  gt_model-model = 'A'.
  gt_model-name = '单价汇总'.
  APPEND gt_model.

  gt_model-model = 'B'.
  gt_model-name = '工厂款号汇总'.
  APPEND gt_model.

  gt_model-model = 'C'.
  gt_model-name = '尺码汇总'.
  APPEND gt_model.

  gt_model-model = 'D'.
  gt_model-name = '说明汇总'.
  APPEND gt_model.

  PERFORM frm_pop_model CHANGING g_model.

  CHECK g_model IS NOT INITIAL.

  CLEAR batch_item[].

  LOOP AT ut_item.
    CASE g_model .
      WHEN 'A'.
        batch_item-maktx = ut_item-price_long.
      WHEN 'B'.
        batch_item-matnr = ut_item-matnr.
        batch_item-idnlf = ut_item-idnlf.
        batch_item-maktx = ut_item-maktx.
      WHEN 'C'.
        batch_item-matnr = ut_item-matnr.
        batch_item-maktx = ut_item-maktx.
        batch_item-zsize = ut_item-zsize.
      WHEN 'D'.
        batch_item-matnr = ut_item-matnr.
        batch_item-maktx = ut_item-maktx.
        batch_item-znorms = ut_item-znorms.
    ENDCASE.
    batch_item-menge = ut_item-menge.
    batch_item-amount = ut_item-amount.
    COLLECT batch_item.
    CLEAR batch_item.
  ENDLOOP.

  IF g_model = 'A'.
    LOOP AT batch_item.
      batch_item-price_long = batch_item-maktx.
      MODIFY batch_item.
    ENDLOOP.
  ENDIF.

  PERFORM factory_display USING batch_item[].

ENDFUNCTION.


FORM factory_display USING gt_table."salv动态显示

  DATA: gr_table   TYPE REF TO cl_salv_table.
  DATA: lv_filename TYPE string.

  cl_salv_table=>factory( IMPORTING  r_salv_table = gr_table CHANGING t_table = gt_table ).

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = gr_table->get_functions( ).
  lr_functions->set_default( abap_true ).
  lr_functions->set_all( abap_true ).
  DATA: lr_columns TYPE REF TO cl_salv_columns.

  lr_columns = gr_table->get_columns( ).

  lr_columns->set_optimize( abap_true ).

  DATA: lr_column TYPE REF TO cl_salv_column.
  LOOP AT gt_fcat.
    CASE g_model.
      WHEN 'A'.
        IF gt_fcat-fieldname = 'PRICE_LONG'
          OR gt_fcat-fieldname = 'MENGE'
          OR gt_fcat-fieldname = 'AMOUNT'.
        ELSE.
          lr_column = lr_columns->get_column( EXPORTING columnname = gt_fcat-fieldname ).
          lr_column->set_technical( 'X' ).
        ENDIF.
      WHEN 'B'.
        IF gt_fcat-fieldname = 'MATNR'
          OR gt_fcat-fieldname = 'IDNLF'
          OR gt_fcat-fieldname = 'MAKTX'
          OR gt_fcat-fieldname = 'MENGE'
          OR gt_fcat-fieldname = 'AMOUNT'
          .
        ELSE.
          lr_column = lr_columns->get_column( EXPORTING columnname = gt_fcat-fieldname ).
          lr_column->set_technical( 'X' ).
        ENDIF.
      WHEN 'C'.
        IF gt_fcat-fieldname = 'MATNR'
          OR gt_fcat-fieldname = 'ZSIZE'
          OR gt_fcat-fieldname = 'MAKTX'
          OR gt_fcat-fieldname = 'MENGE'
          OR gt_fcat-fieldname = 'AMOUNT'
          .
        ELSE.
          lr_column = lr_columns->get_column( EXPORTING columnname = gt_fcat-fieldname ).
          lr_column->set_technical( 'X' ).
        ENDIF.
      WHEN 'D'.
        IF gt_fcat-fieldname = 'MATNR'
          OR gt_fcat-fieldname = 'ZNORMS'
          OR gt_fcat-fieldname = 'MAKTX'
          OR gt_fcat-fieldname = 'MENGE'
          OR gt_fcat-fieldname = 'AMOUNT'
          .
        ELSE.
          lr_column = lr_columns->get_column( EXPORTING columnname = gt_fcat-fieldname ).
          lr_column->set_technical( 'X' ).
        ENDIF.

    ENDCASE.


  ENDLOOP.

  gr_table->display( ) .

ENDFORM.
