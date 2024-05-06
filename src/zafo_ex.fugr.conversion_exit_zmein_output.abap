FUNCTION conversion_exit_zmein_output .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"     VALUE(LANGUAGE) LIKE  SY-LANGU DEFAULT SY-LANGU
*"  EXPORTING
*"     VALUE(OUTPUT)
*"  EXCEPTIONS
*"      UNIT_NOT_FOUND
*"----------------------------------------------------------------------
  READ TABLE gt_t006a INTO DATA(ls_t006a) WITH KEY msehi = input spras = language.
  IF sy-subrc EQ 0.
    output = ls_t006a-mseht.
    RETURN.
  ENDIF.

  SELECT SINGLE * INTO ls_t006a
    FROM t006a
    WHERE msehi = input
    AND spras = language.
  IF sy-subrc EQ 0.
    APPEND ls_t006a TO gt_t006a.
    output = ls_t006a-mseht.
  ENDIF.
ENDFUNCTION.
