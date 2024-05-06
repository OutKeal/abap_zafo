FUNCTION conversion_exit_user_input.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------

  LOOP AT gt_user WHERE name_textc = input OR bname = input.
    output = gt_user-bname.
    RETURN.
  ENDLOOP.


  SELECT SINGLE
    bname,name_textc INTO @gt_user
    FROM user_addr
    WHERE name_textc = @input
    OR bname = @input.
  IF sy-subrc EQ 0.
    output = gt_user-bname.
    APPEND gt_user.
  ELSE.
    output = input.
  ENDIF.

ENDFUNCTION.
