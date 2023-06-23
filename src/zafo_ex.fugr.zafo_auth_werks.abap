FUNCTION zafo_auth_werks.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IV_ACTVT) TYPE  ACTIV_AUTH
*"     VALUE(IV_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(IV_OBJECT) TYPE  XUOBJECT
*"  TABLES
*"      S_WERKS STRUCTURE  RANGE_WERKS OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------


  IF iv_object IS INITIAL.
    CLEAR:s_werks[].
    RAISE error.
    RETURN.
  ENDIF.

  IF iv_werks IS NOT INITIAL."单值优先
    SELECT * FROM t001w INTO TABLE @DATA(lt_t001w)
     WHERE werks EQ @iv_werks.
      return.
  ELSE.
    SELECT * FROM t001w INTO TABLE @lt_t001w
     WHERE werks IN @s_werks[].
  ENDIF.

  CLEAR:s_werks[].
  LOOP AT lt_t001w INTO DATA(ls_t001w).

    AUTHORITY-CHECK OBJECT iv_object ID 'WERKS' FIELD ls_t001w-werks
                                     ID 'ACTVT' FIELD iv_actvt.
    IF sy-subrc = 0..
      s_werks-sign = 'I'.
      s_werks-option = 'EQ'.
      s_werks-low = ls_t001w-werks.
      APPEND s_werks.
      CLEAR:s_werks.
    ENDIF.

  ENDLOOP.


  IF s_werks[] IS INITIAL.

    RAISE error.
  ENDIF.




ENDFUNCTION.
