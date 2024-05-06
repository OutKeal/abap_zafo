FUNCTION CONVERSION_EXIT_ZMATK_INPUT .
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
  LOOP AT gt_t023t INTO DATA(l_t023t) WHERE matkl = input OR wgbez = input.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    output = l_t023t-matkl.
  ELSE.
    SELECT SINGLE * INTO l_t023t FROM t023t WHERE matkl = input OR wgbez = input.
    IF sy-subrc EQ 0.
      APPEND l_t023t TO gt_t023t.
      output = l_t023t-matkl.
    ENDIF.
  ENDIF.

ENDFUNCTION.
