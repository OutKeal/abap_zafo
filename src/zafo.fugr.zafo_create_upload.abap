FUNCTION ZAFO_CREATE_UPLOAD .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(I_BUSTYP) TYPE  ZAFO_BUSTYP
*"     VALUE(I_FILENAME) TYPE  RLGRAP-FILENAME
*"     VALUE(I_BEGIN_ROW) TYPE  I
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------


  CALL FUNCTION 'ZAFO_CLEAR'.

  g_bustyp = i_bustyp.
  g_action = 'CRE'.
  g_werks = i_werks.

  PERFORM frm_get_config.
  IF g_error = 'X'.
    PERFORM frm_pop_msg .
    et_return[] = ot_return[].
    RETURN.
  ENDIF.

  PERFORM frm_set_text.

  PERFORM init_head.

  PERFORM init_item.

  PERFORM frm_import_data USING i_filename i_begin_row.
  IF g_error = 'X'.
    PERFORM frm_pop_msg .
    et_return[] = ot_return[].
    RETURN.
  ENDIF.

  PERFORM frm_set_save_head.

  PERFORM frm_check_item_data.
  IF g_error = 'X'.
    PERFORM frm_pop_msg .
    et_return[] = ot_return[].
    RETURN.
  ENDIF.

  LOOP AT gt_item ASSIGNING <gs_item>.
    PERFORM frm_set_icon USING 'A' CHANGING <gs_item>-icon <gs_item>-text .
    <gs_item>-text = text-014."'已维护'.
  ENDLOOP.

  CALL SCREEN gs_object-main_dynnr.

ENDFUNCTION.
