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
  ASSIGN <io_class>->head TO <head>.
  DATA(ob_text) = <io_class>->set_screen_text( ).
  ASSIGN ob_text->* TO <text>.
  IF sy-dynnr NE '0200' .
    CALL SCREEN '200'.
  ENDIF.


ENDFUNCTION.
