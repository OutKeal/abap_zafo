FUNCTION zafo_download_template .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_BUSTYP) TYPE  ZAFO_BUSTYP
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CLEAR g_error.

  g_bustyp = i_bustyp.

  PERFORM frm_get_config.

  IF g_error = 'X'.
    PERFORM frm_pop_msg .
    et_return[] = ot_return[].
    RETURN.
  ENDIF.

  PERFORM frm_get_tab .

  IF g_error = 'X'.
    PERFORM frm_pop_msg .
    et_return[] = ot_return[].
    RETURN.
  ENDIF.

  PERFORM frm_factory_download USING <gt_tab>.


ENDFUNCTION.
