FUNCTION zafo_item_selected.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      UT_ITEM STRUCTURE  ZAFO_SITEM
*"      UT_FCAT TYPE  LVC_T_FCAT
*"      ET_ITEM STRUCTURE  ZAFO_SITEM
*"----------------------------------------------------------------------

  REFRESH gt_fcat.
  REFRESH gt_item.

  gt_fcat[] = ut_fcat[].
  gt_item[] = ut_item[].

  CLEAR sy-ucomm.
  CLEAR g_confirm.

  CALL SCREEN 7001 STARTING AT 10 5 ENDING AT 130 20.

  REFRESH selected_item[].

  IF g_confirm = 'X' .
    LOOP AT gt_item WHERE selected = 'X'.
      MOVE-CORRESPONDING gt_item TO selected_item.
      APPEND selected_item.
    ENDLOOP.

    CLEAR gt_item.
    REFRESH gt_item.
  ENDIF.

  et_item[] = selected_item[].
ENDFUNCTION.
