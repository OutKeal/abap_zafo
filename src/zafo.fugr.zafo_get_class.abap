FUNCTION ZAFO_GET_CLASS .
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     REFERENCE(I_CLASS) TYPE REF TO ZAFO_CLASS
*"--------------------------------------------------------------------
  IF NOT line_exists( tab_class[ table_line = i_class ] ).
    INSERT i_class INTO TABLE tab_class.
  ENDIF.
  ASSIGN tab_class[ table_line = i_class ] TO <io_class>.


ENDFUNCTION.
