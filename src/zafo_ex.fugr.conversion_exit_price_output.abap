FUNCTION conversion_exit_price_output.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"  EXCEPTIONS
*"      LENGTH_ERROR
*"----------------------------------------------------------------------

  DATA : l_str TYPE string.
  DATA : l_neg TYPE abap_bool.
  DATA : l_int TYPE string.
  DATA : l_dec TYPE string.
  DATA : curnt_off TYPE I.
  DATA: elemdescr TYPE REF TO cl_abap_elemdescr.
  CHECK INPUT IS NOT INITIAL.

  elemdescr ?= cl_abap_elemdescr=>describe_by_data( INPUT ).
  IF elemdescr->decimals = 2 OR elemdescr->decimals = 6 .
    DATA(l_curr) = 'X'.
  ENDIF.

  l_str = INPUT.
  CLEAR OUTPUT.

  FIND '-' IN l_str.
  IF sy-subrc EQ 0.
    l_neg = 'X'.
    REPLACE '-' IN l_str  WITH ''.
  ENDIF.
  SPLIT l_str AT '.' INTO l_int l_dec.

  curnt_off = STRLEN( l_int ) - 1.

  WHILE curnt_off >= 0.
    OUTPUT = l_int+curnt_off(1) && OUTPUT.
    IF sy-INDEX MOD 3 = 0   .
      OUTPUT = ',' && OUTPUT.
    ENDIF.
    curnt_off -= 1.
  ENDWHILE.
  IF OUTPUT+0(1) = ','.
    OUTPUT+0(1) = ''.
    CONDENSE OUTPUT NO-GAPS.
  ENDIF.
  IF OUTPUT IS INITIAL.
    OUTPUT = '0'.
  ENDIF.


  IF  l_dec  IS  NOT  INITIAL .
    CONDENSE l_dec NO-GAPS.
    SHIFT l_dec RIGHT DELETING TRAILING '0'.
    CONDENSE l_dec NO-GAPS.
  ENDIF.
  IF l_dec IS NOT INITIAL.
    IF l_curr = 'X' AND STRLEN( l_dec ) = 1.
      l_dec = l_dec && '0'.
    ENDIF.
    OUTPUT  =  OUTPUT && '.' &&  l_dec.
ELSEIF l_curr = 'X'.
    OUTPUT  =  OUTPUT && '.' &&  '00'.
  ENDIF .

  IF l_neg = 'X'.
    OUTPUT = '-' && OUTPUT.
  ENDIF.

  IF OUTPUT = '0'.
    CLEAR OUTPUT.
  ENDIF.
  CHECK OUTPUT IS NOT INITIAL.
*  DATA(lenth) = elemdescr->length + elemdescr->decimals.
*  lenth -= strlen( output ).
*  CHECK lenth > 0.
*  DO lenth + 1 TIMES.
*    output = | { output }|.
*  ENDDO.


ENDFUNCTION.
