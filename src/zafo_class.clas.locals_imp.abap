*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*&---------------------------------------------------------------------*
*& Include ZAFO_CLASS_MACRO
*&---------------------------------------------------------------------*
DEFINE macro_error_return.
IF MESSAGE->get_error( ) = abap_true.
  IF called = abap_true.
    RETURN.
  ENDIF.
  MESSAGE->pop_msg( ABAP_TRUE ).
  RETURN.
ENDIF.
END-OF-DEFINITION.

DEFINE macro_static_error_return.
  IF r_class->message->get_error( ) = abap_true.
    IF r_class->called = abap_true.
      RETURN.
    ENDIF.
    r_class->message->pop_msg( ABAP_TRUE ).
    RETURN.
  ENDIF.
END-OF-DEFINITION.

DEFINE macro_go_error_return.
  IF sy-subrc NE 0.
    me->ret-msgty = 'E'.
    me->ret-msgno = &1.
    APPEND VALUE #(  TYPE = me->ret-msgty
    ID = 'ZAFO'
    NUMBER = &1
    ) TO ret-RETURN.
    break( ).
    RETURN.
  ENDIF.
END-OF-DEFINITION.
