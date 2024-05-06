FUNCTION conversion_exit_user_output .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"----------------------------------------------------------------------
  LOOP AT gt_user WHERE  bname = input.
    output = gt_user-name_textc.
    RETURN.
  ENDLOOP.


  SELECT SINGLE
    bname,name_textc INTO @gt_user
    FROM user_addr
    WHERE bname = @input.
  IF sy-subrc EQ 0.
    output = gt_user-name_textc.
    APPEND gt_user.
  ELSE.
    output = input.
  ENDIF.

ENDFUNCTION.
