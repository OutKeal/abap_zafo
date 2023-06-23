FUNCTION zafo_choise .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
  CLEAR g_ok_code.

  LOOP AT gt_item_100 ASSIGNING <gs_item> WHERE icon <> icon_led_red.

    CHECK <gs_item>-bukrs IS NOT INITIAL AND <gs_item>-werks IS NOT INITIAL.
    IF gs_bustyp-execute_type = 'MB' AND <gs_item>-bukrs <> <gs_item>-werks.
      PERFORM frm_set_icon USING 'E' CHANGING <gs_item>-icon <gs_item>-text .
      <gs_item>-text = TEXT-031."'无法处理'.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  DATA: lr_zzpino TYPE RANGE OF zzpino.
  PERFORM frm_set_zzpino_hide TABLES lr_zzpino.
  IF lr_zzpino[] IS NOT INITIAL.
    DELETE gt_item_100 WHERE zzpino IN lr_zzpino.
  ENDIF.

  " 把产前样排在前面，入库分摊避免最后一行为产前样
  SORT gt_item_100 BY afono ebeln zzpino zppdhd matnr zcolor zsize znorms zppflag DESCENDING.

  CALL SCREEN '100'.

  LOOP AT gt_item_100 WHERE selected = 'X'.
    APPEND gt_item_100 TO gt_item[].
  ENDLOOP.

  LOOP AT gt_item_po_100 .
    READ TABLE gt_item WITH KEY afonr = gt_item_po_100-afonr.
    IF sy-subrc EQ 0.
      APPEND gt_item_po_100 TO gt_item_po.
    ENDIF.
  ENDLOOP.

  IF g_ok_code NE '&SURE'.
    REFRESH gt_item_100.
    REFRESH gt_item.
  ENDIF.


  LOOP AT gt_item ASSIGNING <gs_item>.
    DATA(l_tabix) = sy-tabix.

    IF l_tabix = 1 AND ( gs_bustyp-busref = 'D' OR gs_bustyp-busref = 'J')."获取转BOM时的业务员
      PERFORM f_set_po_ywy USING <gs_item>-zzpino.
    ELSEIF l_tabix = 1 AND gs_bustyp-busref = 'A'.
      gs_head-afnam = <gs_item>-afnam." 收货赋值采购员
    ENDIF.

    PERFORM frm_set_copy_head USING l_tabix <gs_item>.

    PERFORM frm_set_icon USING 'A' CHANGING <gs_item>-icon <gs_item>-text .
    <gs_item>-text = TEXT-001." '已选择'.

    PERFORM frm_set_change_icon USING <gs_item>-change_flag CHANGING <gs_item>-icon <gs_item>-text.

  ENDLOOP.

  IF g_error = 'X'.
    CLEAR gt_item[].
    PERFORM frm_pop_msg.
  ENDIF.

ENDFUNCTION.


FORM frm_set_copy_head USING index ls_item TYPE zafo_sitem ." 拷贝表体数据到表头

  FIELD-SYMBOLS:<fs_head_value> TYPE any.
  FIELD-SYMBOLS:<fs_item_value> TYPE any.

  IF index = 1.
    g_werks = ls_item-werks.
  ENDIF.

  gs_head-werks = g_werks.

  IF g_werks <> ls_item-werks.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '007' ''  '' '' ''.
  ENDIF.

  gs_head-werks = g_werks.

  LOOP AT gt_screen WHERE object = g_object AND fieldalv = 'HEAD' AND requi = 'X'.

    ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gs_head TO <fs_head_value>.
    CHECK sy-subrc EQ 0.

    ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE ls_item TO <fs_item_value>.
    CHECK sy-subrc EQ 0.

    IF index = 1.
      <fs_head_value> = <fs_item_value>.
      CONTINUE.
    ENDIF.

    IF <fs_head_value> <> <fs_item_value> .
      PERFORM frm_add_msg USING 'E' 'ZAFO' '008'  gt_screen-coltext '' '' ''." &1不唯一，请重新选择
      <fs_head_value> = <fs_item_value>.
    ENDIF.

  ENDLOOP.


  CASE g_bustyp.
    WHEN 'R2001'.
      gs_head-lifnr = ls_item-lifnr.
  ENDCASE.


ENDFORM.
