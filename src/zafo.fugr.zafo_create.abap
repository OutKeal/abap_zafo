FUNCTION zafo_create.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_BUSTYP) TYPE  ZAFO_BUSTYP
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      CT_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"      CT_ITEM_PO STRUCTURE  ZAFO_SITEM_PO OPTIONAL
*"      CT_ITEM_COST STRUCTURE  ZAFO_ITEM_COST OPTIONAL
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
  gt_item_po[] = ct_item_po[].
  gt_item_cost[] = ct_item_cost[].


  PERFORM frm_get_config.

  IF g_error = 'X'.
    PERFORM frm_pop_msg .
    et_return[] = ot_return[].
    RETURN.
  ENDIF.

  PERFORM frm_set_text.

  IF  gs_bustyp-busref IS NOT INITIAL.
    IF gt_item[] IS NOT INITIAL." 选择明细创建

      PERFORM init_item_zdefault.

      gt_item_100[] = gt_item[].
      gt_item_po_100[] = gt_item_po[].

      CLEAR gt_item[].
      CLEAR gt_item_po[].

      CALL FUNCTION 'ZAFO_CHOISE'.

      IF gt_item[] IS INITIAL.
        IF gs_object-main_dynnr <> '0800'.
          MESSAGE s009 DISPLAY LIKE 'W'."&1不唯一，请重新选择
          RETURN.
        ENDIF.
      ENDIF.

    ELSE." 直接创建

      IF gs_object-main_dynnr <> '0800'.
        MESSAGE s006 DISPLAY LIKE 'E'."无数据
        RETURN.
      ENDIF.

    ENDIF.

  ELSE.

    PERFORM init_initial_item.

  ENDIF.

  PERFORM init_head.

  PERFORM init_head_text.

  PERFORM init_item.

  CALL SCREEN gs_object-main_dynnr .

  MOVE-CORRESPONDING gs_head TO cs_head.

ENDFUNCTION.
