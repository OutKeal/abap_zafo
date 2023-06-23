FUNCTION zafo_print_mb.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_MODEL) TYPE  ZAFO_PRINT_MODEL OPTIONAL
*"     VALUE(I_PRINT_TYPE) TYPE  ZAFO_PRINT_TYPE
*"     VALUE(I_TITLE) TYPE  NAME1 OPTIONAL
*"  TABLES
*"      IT_HEAD STRUCTURE  ZAFO_SHEAD
*"      IT_ITEM STRUCTURE  ZAFO_SITEM
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  SELECT SINGLE * INTO @gs_print
    FROM zafo_print
    WHERE model = @i_model
    AND print_type = @i_print_type.

  IF i_title IS NOT INITIAL ." 传递指定标题
    gs_print-ztitle = i_title.
  ENDIF.

  IF i_model IS INITIAL.
    PERFORM frm_pop_model USING i_print_type.
    IF gs_print IS INITIAL.
      RETURN .
    ENDIF.
  ENDIF.

  PERFORM frm_set_print_data TABLES it_head it_item.

  PERFORM frm_initial_smartforms USING gs_print-smart_form.

  LOOP AT gt_print_h INTO gs_print_h.

    AT FIRST.

      control_parameters-no_close = 'X'.

    ENDAT.
    AT LAST.

      control_parameters-no_close = space.

    ENDAT.

    PERFORM frm_print_report.

    control_parameters-no_open = 'X'.

  ENDLOOP.

ENDFUNCTION.
