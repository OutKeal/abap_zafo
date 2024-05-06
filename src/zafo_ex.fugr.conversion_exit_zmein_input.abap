FUNCTION conversion_exit_zmein_input .
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
  DATA input_upper TYPE text10.
  DATA input_lower TYPE text10.
  input_upper = to_upper( input ).
  input_lower = to_lower( input ).

  LOOP AT gt_t006a INTO DATA(ls_t006a) WHERE ( msehi = input_upper
                                                                           OR mseht = input_upper
                                                                           OR msehi = input_lower
                                                                           OR mseht = input_lower )
                                                                        AND spras = language.
    output = ls_t006a-msehi.
    RETURN.
  ENDLOOP.


  SELECT SINGLE t006a~* INTO @ls_t006a FROM t006a INNER JOIN t006
    ON t006~msehi = t006a~msehi
    WHERE ( t006a~mseht = @input_upper
              OR t006a~mseht = @input_lower
              OR t006a~msehi = @input_upper
              OR t006a~msehi = @input_lower
              )
    AND t006~kzkeh = 'X'
    AND t006a~spras = @language.
  IF sy-subrc NE 0.
    MESSAGE |计量单位{ input }不存在| TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    output = ls_t006a-msehi.
    APPEND ls_t006a TO gt_t006a.
  ENDIF.

ENDFUNCTION.
