FUNCTION zafo_delete.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_AFONO) TYPE  ZAFONO
*"  EXPORTING
*"     VALUE(ES_HEAD) TYPE  ZAFO_SHEAD
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CHECK i_afono IS NOT INITIAL.

  CALL FUNCTION 'ZAFO_CONVERSION'
    EXPORTING
      i_exec_env = 'START'.

  CALL FUNCTION 'ZAFO_CLEAR'.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_head
    FROM zafo_head WHERE afono = i_afono.
  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '010' '' '' '' ''."该单号不存在
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item
    FROM zafo_item WHERE afono = i_afono
    AND del_flag = ''.

  g_bustyp = gs_head-bustyp.
  g_object = gs_head-object.
  g_action = 'MOD'.

  PERFORM frm_get_config.

  PERFORM frm_set_icon  USING gs_head-status CHANGING gs_head-icon gs_head-text .

  LOOP AT gt_item ASSIGNING <gs_item>.
    PERFORM frm_set_icon  USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text .
  ENDLOOP.


  IF gs_bustyp-execute_type = 'PO' OR gs_bustyp-execute_type = 'POC'
    OR gs_bustyp-execute_type = 'PRO' OR gs_bustyp-execute_type = 'POP'.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item_po
      FROM zafo_item_po WHERE afono = i_afono
      AND loekz = ''.

    LOOP AT gt_item_po ASSIGNING <gs_item_po>.
      READ TABLE gt_item WITH KEY afono = <gs_item_po>-afono afonr = <gs_item_po>-afonr.
      IF sy-subrc EQ 0.
        <gs_item_po>-icon = gt_item-icon.
        <gs_item_po>-text = gt_item-text.
      ENDIF.
    ENDLOOP.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item_cost
      FROM zafo_item_cost WHERE afono = i_afono.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item_po_cpt
      FROM zafo_item_po_cpt WHERE afono = i_afono.
  ENDIF.

  g_readonly = 'M'.

  PERFORM frm_check_delete.

  IF g_error <> 'X'.
    PERFORM frm_delete_po.
  ENDIF.

  IF g_error <> 'X'.
    PERFORM frm_delete_data.
  ENDIF.

  IF g_error = 'X'.
    et_return[] = ot_return[].
  ENDIF.

  es_head = gs_head.

  CALL FUNCTION 'ZAFO_CONVERSION'
    EXPORTING
      i_exec_env = 'END'.

ENDFUNCTION.
