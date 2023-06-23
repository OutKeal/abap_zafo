FUNCTION zafo_create_save.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_BUSTYP) TYPE  ZAFO_BUSTYP
*"     VALUE(NO_COMMIT) TYPE  CHAR1 OPTIONAL
*"     VALUE(NO_AUTHCHECK) TYPE  CHAR1 OPTIONAL
*"     VALUE(NO_SAVE) TYPE  CHAR1 OPTIONAL
*"     VALUE(I_POST) TYPE  CHAR1 OPTIONAL
*"     VALUE(I_ZAPP) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      CT_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"      CT_ITEM_PO STRUCTURE  ZAFO_SITEM_PO OPTIONAL
*"      CT_ITEM_COST STRUCTURE  ZAFO_ITEM_COST OPTIONAL
*"  CHANGING
*"     VALUE(CS_HEAD) TYPE  ZAFO_HEAD OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CALL FUNCTION 'ZAFO_CONVERSION'
    EXPORTING
      i_exec_env = 'START'.


  CALL FUNCTION 'ZAFO_CLEAR'.

  g_bustyp = i_bustyp.
  g_action = 'CRE'.
  g_no_commit = no_commit.
  g_no_authcheck = no_authcheck.

  IF cs_head IS NOT INITIAL.
    MOVE-CORRESPONDING cs_head TO gs_head .
  ENDIF.

  IF g_werks IS INITIAL .
    g_werks = gs_head-werks.
  ENDIF.

  gt_item[] = ct_item[].
  gt_item_po[] = ct_item_po[].
  gt_item_cost[] = ct_item_cost[].

  PERFORM frm_clear_msg.

  PERFORM frm_get_config.

  IF g_error <> 'X'.
    PERFORM init_item_zdefault.
    PERFORM init_head.
    PERFORM init_item.
    PERFORM frm_set_save_head.
  ENDIF.

  IF g_error <> 'X'.
    PERFORM frm_before_save_data.
  ENDIF.

  IF g_error <> 'X'.
    PERFORM frm_check_head_data.
  ENDIF.

  IF g_error <> 'X'.
    PERFORM frm_check_item_data.
  ENDIF.

  IF g_error <> 'X'.
    PERFORM frm_move_head_to_item.
  ENDIF.

  g_no_save = no_save.

  IF g_error <> 'X' AND no_save <> 'X'.
    PERFORM frm_save_data.
  ENDIF.

  IF g_error <> 'X' AND i_post EQ 'X'.
    PERFORM frm_post.
    IF g_error = 'X'.
      PERFORM frm_delete_data.
    ENDIF.
  ENDIF.

  IF g_error <> 'X' AND i_zapp EQ 'X'.
    PERFORM frm_commit.
    IF g_error = 'X'.
      PERFORM frm_delete_data.
    ENDIF.
  ENDIF.

  DATA(lv_error) = g_error.

  MOVE-CORRESPONDING gs_head TO cs_head.


  CALL FUNCTION 'ZAFO_CONVERSION'
    EXPORTING
      i_exec_env = 'END'.


  READ TABLE ot_return WITH  KEY type = 'E'.
  IF sy-subrc EQ 0 .
    lv_error = 'X'.
  ENDIF.

  IF lv_error = 'X'.
    et_return[] = ot_return[].
  ENDIF.

ENDFUNCTION.
