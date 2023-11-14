FUNCTION conversion_exit_user_output .
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"--------------------------------------------------------------------

  DATA:l_uname TYPE char40.
  SELECT SINGLE
    name_textc INTO l_uname
    FROM user_addr
    WHERE bname = input.
  IF sy-subrc EQ 0.
    output = l_uname.
  ELSE.
    output = input.
  ENDIF.

ENDFUNCTION.
