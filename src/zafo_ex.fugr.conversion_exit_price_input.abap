FUNCTION conversion_exit_price_input.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"  EXCEPTIONS
*"      LENGTH_ERROR
*"----------------------------------------------------------------------

  TRY.
      REPLACE ALL OCCURRENCES OF ',' IN input WITH space.
      output = input.
    CATCH cx_root.
      output = ''.
      MESSAGE '价格输入有误' TYPE 'S'.
  ENDTRY.



ENDFUNCTION.
