FUNCTION CONVERSION_EXIT_PRICE_OUTPUT.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"  EXCEPTIONS
*"      LENGTH_ERROR
*"----------------------------------------------------------------------

  OUTPUT = INPUT .
  PERFORM FRM_CONVERT_ZERO USING input CHANGING output.


ENDFUNCTION.
