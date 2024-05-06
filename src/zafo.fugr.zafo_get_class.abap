FUNCTION zafo_get_class .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_CLASS) TYPE REF TO  ZAFO_CLASS
*"----------------------------------------------------------------------
  IF NOT line_exists( zafo_basic=>tab_class[ afono = i_class->head-afono ] ).
    INSERT VALUE #( afono = i_class->head-afono
                                  class = i_class ) INTO TABLE zafo_basic=>tab_class.
  ENDIF.
  ASSIGN zafo_basic=>tab_class[ class = i_class ]-class TO <io_class>.

ENDFUNCTION.
