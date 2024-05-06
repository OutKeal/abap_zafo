FUNCTION zafo_select .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_CLASS) TYPE REF TO  ZAFO_CLASS
*"----------------------------------------------------------------------
  CALL FUNCTION 'ZAFO_GET_CLASS'
    EXPORTING
      i_class = i_class.
  IF i_class->action = 'REF_IN'.
    CALL SCREEN '110'.
*    CALL SCREEN '110' STARTING AT 1 1
*                                    ENDING AT 150 60.
  ELSE.
    CALL SCREEN '100'.
  ENDIF.
ENDFUNCTION.
