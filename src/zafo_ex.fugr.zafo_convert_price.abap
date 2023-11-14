FUNCTION zafo_convert_price.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(PRICE_LONG) TYPE  ZAFO_PRICE_LONG
*"  EXPORTING
*"     VALUE(PRICE)
*"     VALUE(PEINH)
*"----------------------------------------------------------------------
  DATA:ls_prcie TYPE char30.
  DATA:ls_prcie1 TYPE char30.
  DATA:ls_prcie2 TYPE char30.
  DATA:ls_len(10) TYPE i.

  price = price_long.
  peinh = 1.

  CHECK price_long IS NOT INITIAL.

  PERFORM frm_convert_zero USING price_long CHANGING ls_prcie.

  SPLIT ls_prcie AT '.' INTO ls_prcie1 ls_prcie2.

  CHECK ls_prcie2 IS NOT INITIAL.

  ls_len = strlen( ls_prcie2 ) .

  CHECK ls_len > 2.

  ls_len = ls_len - 2.

  DO ls_len TIMES.
    price_long = price_long * 10.
    peinh = peinh * 10.
  ENDDO.
  price = price_long.



ENDFUNCTION.
