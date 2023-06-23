FUNCTION zafo_post.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_AFONO) TYPE  ZAFONO OPTIONAL
*"  EXPORTING
*"     VALUE(ES_HEAD) TYPE  ZAFO_SHEAD
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      ET_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CALL FUNCTION 'ZAFO_CONVERSION'
    EXPORTING
      i_exec_env = 'START'.

  CALL FUNCTION 'ZAFO_CLEAR'.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_head
    FROM zafo_head WHERE afono = i_afono.
  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '010' '' '' '' ''."该单号不存在
    PERFORM frm_pop_msg .
    RETURN.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item
    FROM zafo_item WHERE afono = i_afono
    AND del_flag = ''.

  g_bustyp = gs_head-bustyp.
  g_object = gs_head-object.
  g_action = 'MOD'.

  PERFORM frm_get_config.
  IF g_error = 'X'.
*    PERFORM frm_pop_msg .
    et_return[] = ot_return[].
    RETURN.
  ENDIF.

  PERFORM frm_set_icon
    USING gs_head-status CHANGING gs_head-icon gs_head-text .

  LOOP AT gt_item ASSIGNING <gs_item>.
    PERFORM frm_set_icon
     USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text .
  ENDLOOP.


  IF gs_bustyp-execute_type = 'PO'
    OR gs_bustyp-execute_type = 'POC'
    OR gs_bustyp-execute_type = 'PRO'
    OR gs_bustyp-execute_type = 'POP'.
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

  ENDIF.

  g_readonly = 'M'.

  PERFORM frm_post.

  IF g_error IS NOT INITIAL.
    et_return[] = ot_return[].
    RAISE error.
  ENDIF.

  CASE gs_bustyp-execute_type.
    WHEN 'POC' or 'POP'.
      PERFORM frm_save_po_data.
    WHEN OTHERS.
      PERFORM frm_save_data.
  ENDCASE.


  IF g_error = 'X'.
    RAISE error.
  ENDIF.

  es_head = gs_head.
  et_item[] = gt_item[].


  CALL FUNCTION 'ZAFO_CONVERSION'
    EXPORTING
      i_exec_env = 'END'.

ENDFUNCTION.
