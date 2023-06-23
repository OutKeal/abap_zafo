FUNCTION conversion_exit_user_input.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------

  DATA:l_user TYPE char40.
  SELECT SINGLE
    bname INTO l_user
    FROM user_addr
    WHERE name_textc = input
    OR bname = input.
  IF sy-subrc EQ 0.
    output = l_user.
  ELSE.
    output = input.
  ENDIF.

ENDFUNCTION.
