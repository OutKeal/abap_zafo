FUNCTION zafo_display .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(I_BUSTYP) TYPE  ZAFO_BUSTYP
*"  TABLES
*"      CT_HEAD STRUCTURE  ZAFO_SHEAD
*"      CT_ITEM STRUCTURE  ZAFO_SITEM
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  g_bustyp = i_bustyp.
*  g_werks = i_werks.

  g_action = 'DIS'.

  PERFORM frm_get_config.

  IF g_error = 'X'.

    PERFORM frm_pop_msg .

    et_return[] = ot_return[].

    RETURN.

  ENDIF.


  DATA: lr_zzpino TYPE RANGE OF zzpino.
  PERFORM frm_set_zzpino_hide TABLES lr_zzpino.
  IF lr_zzpino[] IS NOT INITIAL.
    DELETE ct_item WHERE zzpino IN lr_zzpino.
  ENDIF.

  PERFORM frm_set_group TABLES ct_head ct_item.

  gt_head_dis[] = ct_head[].
  gt_item_dis[] = ct_item[].
  gt_item_front_dis[] = gt_item_dis[].

  CALL SCREEN '300'.

ENDFUNCTION.
