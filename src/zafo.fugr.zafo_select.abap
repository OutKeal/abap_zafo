FUNCTION ZAFO_SELECT .
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     REFERENCE(I_CLASS) TYPE REF TO ZAFO_CLASS
*"--------------------------------------------------------------------
  CALL FUNCTION 'ZAFO_GET_CLASS'
    EXPORTING
      i_class = i_class.

  CALL SCREEN '100'.
ENDFUNCTION.
