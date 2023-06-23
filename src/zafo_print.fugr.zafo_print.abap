FUNCTION zafo_print.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IS_BUSTYP) TYPE  ZAFO_BUSTYPE OPTIONAL
*"  TABLES
*"      IT_HEAD STRUCTURE  ZAFO_SHEAD OPTIONAL
*"      IT_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"----------------------------------------------------------------------

  CASE is_bustyp-print_type.
    WHEN 'LL'.
      CALL FUNCTION 'ZAFO_PRINT_FL'
        TABLES
          it_head = it_head[]
          it_item = it_item[].

  ENDCASE.

ENDFUNCTION.
