FUNCTION zafo_create_scan.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_BUSTYP) TYPE  ZAFO_BUSTYP
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      CT_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"      CT_SBARCODE STRUCTURE  ZMM_SBARCODE OPTIONAL
*"  CHANGING
*"     VALUE(CS_HEAD) TYPE  ZAFO_SHEAD OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CALL FUNCTION 'ZAFO_CLEAR'.

  g_bustyp = i_bustyp.
  g_action = 'CRE'.
  g_werks = i_werks.


  IF cs_head IS NOT INITIAL.
    MOVE-CORRESPONDING cs_head TO gs_head .
  ENDIF.

  gt_item[] = ct_item[].
  gt_barcode_dis[] = ct_sbarcode[].

  PERFORM frm_get_config.

  IF g_error = 'X'.
    PERFORM frm_pop_msg .
    et_return[] = ot_return[].
    RETURN.
  ENDIF.

  PERFORM frm_set_text.

  PERFORM init_head.
  PERFORM init_item.
  PERFORM frm_set_save_head.

  CALL SCREEN gs_object-main_dynnr .

  MOVE-CORRESPONDING gs_head TO cs_head.

ENDFUNCTION.
