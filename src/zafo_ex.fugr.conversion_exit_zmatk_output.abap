FUNCTION CONVERSION_EXIT_ZMATK_OUTPUT .
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"     VALUE(LANGUAGE) LIKE  SY-LANGU DEFAULT SY-LANGU
*"  EXPORTING
*"     VALUE(OUTPUT)
*"  EXCEPTIONS
*"      UNIT_NOT_FOUND
*"--------------------------------------------------------------------

  LOOP AT gt_t023t INTO DATA(l_t023t) WHERE matkl = INPUT OR wgbez = INPUT.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    OUTPUT = l_t023t-wgbez.
  ELSE.
    SELECT SINGLE * INTO l_t023t FROM t023t WHERE matkl = INPUT OR wgbez = INPUT.
    IF sy-subrc EQ 0.
      APPEND l_t023t TO gt_t023t.
      OUTPUT = l_t023t-wgbez.
    ENDIF.
  ENDIF.

ENDFUNCTION.
