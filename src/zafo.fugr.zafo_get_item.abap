FUNCTION zafo_get_item.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(SET) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      CT_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"      CT_ITEM_PO STRUCTURE  ZAFO_SITEM_PO OPTIONAL
*"----------------------------------------------------------------------

  IF set = 'X'.
    gt_item_po[] = ct_item_po[].
  ELSE.
    ct_item[] = gt_item[].
    ct_item_po[] = gt_item_po[].
  ENDIF.

ENDFUNCTION.
