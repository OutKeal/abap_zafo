FUNCTION zafo_print_get_mb_no.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(IS_BUSTYP) TYPE  ZAFO_BUSTYPE
*"  CHANGING
*"     REFERENCE(CS_HEAD) TYPE  ZAFO_SHEAD
*"----------------------------------------------------------------------

  DATA:l_print_per TYPE zafo_print_per,
       l_werks     TYPE werks_d,
       l_lgort     TYPE lgort_d,
       l_year2     TYPE zafo_year,
       l_num       TYPE zafo_num,
       l_print_no  TYPE zafo_print_no.

  CLEAR:l_print_per,l_werks,l_lgort,l_year2,l_print_no.

  l_print_per = is_bustyp-print_per.
  l_werks = cs_head-werks.
  l_lgort = cs_head-lgort.
  l_year2 = cs_head-budat+2(2).

  IF l_werks IS INITIAL .
    SELECT SINGLE werks INTO l_werks
      FROM zafo_item WHERE afono = cs_head-afono.
  ENDIF.

  IF l_lgort IS INITIAL .
    SELECT SINGLE lgort INTO l_lgort
      FROM zafo_item WHERE afono = cs_head-afono.
  ENDIF.

  SELECT SINGLE * INTO @DATA(ls_mb_no)
    FROM zafo_print_mb_no
    WHERE print_per = @l_print_per
    AND werks = @l_werks
    AND lgort = @l_lgort
    AND year2 = @l_year2.
  IF sy-subrc EQ 0.
    ADD 1 TO ls_mb_no-num .
    l_num = ls_mb_no-num.
    cs_head-print_no = l_print_per && l_werks && l_lgort && l_year2 && l_num.
  ELSE.
    l_num = 1.
    ls_mb_no-print_per = l_print_per.
    ls_mb_no-werks = l_werks.
    ls_mb_no-lgort = l_lgort.
    ls_mb_no-year2 = l_year2.
    ls_mb_no-num = l_num.
    cs_head-print_no = l_print_per && l_werks && l_lgort && l_year2 && l_num.
  ENDIF.

  UPDATE zafo_head SET print_no = cs_head-print_no WHERE afono = cs_head-afono.

  MODIFY zafo_print_mb_no FROM ls_mb_no.
  COMMIT WORK.

ENDFUNCTION.
