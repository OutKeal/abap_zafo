FUNCTION zafo_call_transaction.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(AFONO) TYPE  ZAFONO OPTIONAL
*"     VALUE(FIRST) TYPE  CHAR1 DEFAULT 'X'
*"----------------------------------------------------------------------

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = afono
    IMPORTING
      output = afono.

  SELECT SINGLE *
    INTO @DATA(ls_head)
    FROM zafo_head WHERE afono = @afono.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE tcode
    INTO @DATA(ls_tcode)
    FROM zafo_bustype
    WHERE bustyp = @ls_head-bustyp.

  SET PARAMETER ID 'WRK' FIELD ls_head-werks.
  SET PARAMETER ID 'ZAFONO' FIELD afono.
  SET PARAMETER ID 'ZBUSTYP' FIELD ls_head-bustyp.

  IF first = abap_true.
    CALL TRANSACTION ls_tcode AND SKIP FIRST SCREEN.
  ELSE.
    LEAVE TO TRANSACTION ls_tcode AND SKIP FIRST SCREEN."add by at-yuxs 20211231
  ENDIF.

ENDFUNCTION.
