FUNCTION zafo_ui .
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     REFERENCE(I_CLASS) TYPE REF TO ZAFO_CLASS OPTIONAL
*"--------------------------------------------------------------------
  CALL FUNCTION 'ZAFO_GET_CLASS'
    EXPORTING
      i_class = i_class.
  CHECK <io_class>->werks IS NOT INITIAL.
  DATA(ob_text) = <io_class>->set_screen_text( ).
  ASSIGN ob_text->* TO <gs_text>.
  CALL SCREEN '200'.

ENDFUNCTION.
