FUNCTION zafo_create .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IS_HEAD) TYPE  ZAFO_BAPI_HEAD
*"     VALUE(I_POST) TYPE  BAPIFLAG-BAPIFLAG OPTIONAL
*"     VALUE(NO_COMMIT) TYPE  BAPIFLAG-BAPIFLAG OPTIONAL
*"  EXPORTING
*"     VALUE(E_AFONO) TYPE  ZAFO_SHEAD-AFONO
*"     VALUE(ES_HEAD) TYPE  ZAFO_SHEAD
*"     VALUE(ET_ITEM) TYPE  ZAFO_TT_SITEM
*"  TABLES
*"      IT_ITEM STRUCTURE  ZAFO_BAPI_ITEM
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
  DATA(afo_class) = zafo_class=>create_by_data( i_bustyp = is_head-bustyp
        i_werks = is_head-werks
        is_head = is_head
        it_item = it_item[]
        ).
  et_return[] = afo_class->message->get_return( ).
  IF afo_class->message->get_error( ) = abap_true.
    RETURN.
  ENDIF.
  es_head = afo_class->head.
  et_item[] = afo_class->item.
  e_afono = es_head-afono.

ENDFUNCTION.
