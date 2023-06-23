FUNCTION zafo_maintain.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_BUSTYP) TYPE  ZAFO_BUSTYP
*"     VALUE(I_AFONO) TYPE  ZAFONO
*"  EXPORTING
*"     VALUE(ES_HEAD) TYPE  ZAFO_SHEAD
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      ET_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CALL FUNCTION 'ZAFO_CLEAR'.

  CALL FUNCTION 'ZAFO_AFO_INIT'
    EXPORTING
      i_bustyp  = i_bustyp
      i_afono   = i_afono
    TABLES
      et_return = et_return[].

  IF g_error = 'X'.
    PERFORM frm_pop_msg .
    RETURN.
  ENDIF.

  g_action = 'MOD'.


  PERFORM frm_set_text.

  REFRESH gt_head.

  APPEND gs_head  TO gt_head.

  PERFORM frm_set_group TABLES gt_head gt_item.

  READ TABLE gt_head INTO gs_head INDEX 1.
  REFRESH gt_head.

  PERFORM frm_set_icon USING gs_head-status CHANGING gs_head-icon gs_head-text .

  LOOP AT gt_item ASSIGNING <gs_item>.
    PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text .
    PERFORM frm_set_change_icon USING <gs_item>-change_flag CHANGING <gs_item>-icon <gs_item>-text .

    CASE gs_head-bustyp.
      WHEN 'PR004'." 补料比率添加
        DATA:lv_zzdh TYPE zzdh.
        SELECT SUM( zzdh ) INTO lv_zzdh
          FROM ztpp0091
          WHERE zppdhd = <gs_item>-zppdhd
            AND zzbom_item = <gs_item>-zzbom_item.
        IF sy-subrc = 0 AND lv_zzdh > 0.
          <gs_item>-menge1 = lv_zzdh.
          lv_zzdh = <gs_item>-menge / lv_zzdh * 100.
          <gs_item>-remark3 = lv_zzdh && '%'.
        ENDIF.
    ENDCASE.

  ENDLOOP.

  IF <gs_item> IS ASSIGNED .
    UNASSIGN <gs_item>.
  ENDIF.


  IF gs_object-main_dynnr = '0800'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item_serve
      FROM zafo_item_serve WHERE afono = i_afono.
  ENDIF.

  IF gs_bustyp-execute_type = 'PO' OR gs_bustyp-execute_type = 'POC'
    OR gs_bustyp-execute_type = 'PRO' OR gs_bustyp-execute_type = 'POP'.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item_po
      FROM zafo_item_po
      WHERE afono = i_afono AND loekz = ''.

    LOOP AT gt_item_po ASSIGNING <gs_item_po>.
      READ TABLE gt_item WITH KEY afono = <gs_item_po>-afono afonr = <gs_item_po>-afonr.
      IF sy-subrc EQ 0.
        <gs_item_po>-icon = gt_item-icon.
        <gs_item_po>-text = gt_item-text.
      ENDIF.
    ENDLOOP.

    IF <gs_item_po> IS ASSIGNED .
      UNASSIGN <gs_item_po>.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item_cost
      FROM zafo_item_cost
      WHERE afono = i_afono.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item_po_cpt
      FROM zafo_item_po_cpt
      WHERE afono = i_afono.
    IF sy-subrc EQ 0 .
      LOOP AT gt_item_po_cpt ASSIGNING <gs_item_po_cpt>.
        READ TABLE gt_item WITH KEY afono = <gs_item_po_cpt>-afono afonr = <gs_item_po_cpt>-afonr.
        IF sy-subrc EQ 0.
          <gs_item_po_cpt>-icon = gt_item-icon.
          <gs_item_po_cpt>-text = gt_item-text.
        ENDIF.
      ENDLOOP.

      IF <gs_item_po_cpt> IS ASSIGNED .
        UNASSIGN <gs_item_po_cpt>.
      ENDIF.

    ENDIF.

  ENDIF.

  PERFORM frm_auth_bustyp_show_status USING gs_head-bustyp g_werks CHANGING g_readonly.

  PERFORM init_head_text.

  PERFORM frm_lock .

  CALL SCREEN gs_object-main_dynnr.

  es_head = gs_head.
  et_item[] = gt_item[].

ENDFUNCTION.
