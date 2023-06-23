*&---------------------------------------------------------------------*
*& 包含               ZAFO_I30_SAVE
*&---------------------------------------------------------------------*

FORM frm_set_save_head.
  DATA:l_tabix TYPE sy-tabix.

  CLEAR l_tabix.

  LOOP AT gt_item.
    ADD 1 TO l_tabix.
    PERFORM frm_set_copy_head USING l_tabix gt_item.
  ENDLOOP.

  PERFORM init_head_text.

ENDFORM.


FORM frm_set_head_data.
  CLEAR gs_head-menge.
  CLEAR gs_head-amount.

  IF gs_head-execute_type = 'PO'
    OR gs_head-execute_type = 'PRO'
    OR gs_head-execute_type = 'POC'
    OR gs_head-execute_type = 'POP'.

    LOOP AT gt_item ASSIGNING <gs_item> WHERE del_flag <> 'X'.
      IF  <gs_item>-menge_cg IS NOT INITIAL.
        gs_head-menge = gs_head-menge + <gs_item>-menge_cg + <gs_item>-menge_zj.
      ELSE.
        gs_head-menge = gs_head-menge + <gs_item>-menge.
      ENDIF.
      gs_head-amount = gs_head-amount + <gs_item>-amount.
    ENDLOOP.

  ELSE.
    LOOP AT gt_item ASSIGNING <gs_item> WHERE del_flag <> 'X'
                                          AND icon <> icon_led_inactive
                                          AND menge_cg <> 0.
      gs_head-menge = gs_head-menge + <gs_item>-menge_cg.
      PERFORM f_set_amount CHANGING <gs_item>.
      gs_head-amount = gs_head-amount + <gs_item>-amount.
    ENDLOOP.

    IF gs_head-menge IS INITIAL.
      CLEAR gs_head-menge.
      CLEAR gs_head-amount.
      LOOP AT gt_item  ASSIGNING <gs_item> WHERE del_flag <> 'X'
                                             AND icon <> icon_led_inactive.
        gs_head-menge = gs_head-menge + <gs_item>-menge.
        PERFORM f_set_amount CHANGING <gs_item>.
        gs_head-amount = gs_head-amount + <gs_item>-amount.
      ENDLOOP.
    ENDIF.

  ENDIF.

  gs_head-amount = gs_head-amount + gs_head-cost_amount.

ENDFORM.


FORM frm_set_po_data.

  CASE gs_head-bustyp.
    WHEN 'PO005'.
      LOOP AT gt_item_po WHERE aufnr IS INITIAL.
        SELECT SINGLE zzscgdh INTO gt_item_po-aufnr
          FROM ztpp0089
          WHERE zppdhd = gt_item_po-zppdhd
            AND zzscgdh <> ''.
        MODIFY gt_item_po TRANSPORTING aufnr.
      ENDLOOP.

  ENDCASE.

  IF gs_head-ebeln IS INITIAL AND gt_item_po[] IS NOT INITIAL.
    LOOP AT gt_item ASSIGNING <gs_item>.
      PERFORM frm_set_price CHANGING <gs_item>.
      PERFORM f_set_amount CHANGING <gs_item>.
      PERFORM frm_set_po_amount CHANGING <gs_item>.
    ENDLOOP.

    PERFORM f_refresh_grid_alv USING g_grid_600_top.
    PERFORM f_refresh_grid_alv USING g_grid_600_down.
  ENDIF.

ENDFORM.


FORM frm_before_save_data.

  DELETE gt_item WHERE icon = icon_led_inactive.

  PERFORM frm_set_head_data.

  PERFORM frm_move_head_to_item.

  PERFORM frm_set_po_data.

ENDFORM.


FORM frm_save_data.

  CHECK g_no_save NE 'X'.

  IF gs_head-afono IS INITIAL.
    PERFORM frm_get_next_afono USING gs_object-nrnr CHANGING gs_head-afono.

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-afono = gs_head-afono.
    ENDLOOP.

    gs_head-erdat = sy-datum.
    gs_head-erzet = sy-uzeit.
    gs_head-ernam = sy-uname.

    PERFORM frm_set_status USING 'A'.

    PERFORM frm_lock .

  ENDIF.

  IF sy-uname NE '10015894' .
    gs_head-aenam = sy-uname.
    gs_head-aedat = sy-datum.
    gs_head-aetim = sy-uzeit .
    gs_head-tcode = sy-tcode .
  ENDIF.


  LOOP AT gt_item ASSIGNING <gs_item>.

    IF <gs_item>-icon = icon_led_inactive.
      DELETE gt_item.
      CONTINUE.
    ENDIF.

    IF <gs_item>-del_flag = 'X'.
      PERFORM frm_set_icon USING 'D' CHANGING <gs_item>-icon <gs_item>-text .
    ELSE.
      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text .
    ENDIF.

    PERFORM frm_set_change_icon USING <gs_item>-change_flag CHANGING <gs_item>-icon <gs_item>-text.

    MOVE-CORRESPONDING <gs_item> TO gt_item_modify.

    APPEND gt_item_modify.
    CLEAR gt_item_modify.

  ENDLOOP.

  PERFORM frm_set_ref_save.

  MOVE-CORRESPONDING gs_head TO gs_head_modify.

  PERFORM frm_db_modify.

  IF sy-subrc EQ 0.

    PERFORM frm_save_trigger.

    PERFORM frm_add_msg USING 'S' 'ZAFO' '026' '' '' '' ''."保存成功

  ENDIF.

ENDFORM.


FORM frm_save_trigger.
  DATA:lt_head TYPE TABLE OF zafo_shead  WITH HEADER LINE.

  APPEND gs_head TO lt_head.
  PERFORM frm_set_group TABLES lt_head gt_item.
  READ TABLE lt_head INTO gs_head INDEX 1.
  REFRESH lt_head.

  CASE gs_head-bustyp.
    WHEN 'CJD01'.
*      PERFORM frm_cgrkcjqr_trigger.
  ENDCASE.

ENDFORM.


FORM frm_save_data_serve.
  CHECK gt_item_serve[] IS NOT INITIAL.

  LOOP AT gt_item_serve ASSIGNING <gs_item_serve>.
    gs_head-amount = gs_head-amount + <gs_item_serve>-amount.
    IF <gs_item_serve> IS NOT INITIAL.
      <gs_item_serve>-afono = gs_head-afono.
    ENDIF.
  ENDLOOP.

  MODIFY zafo_item_serve FROM TABLE gt_item_serve .
  COMMIT WORK.

ENDFORM.


FORM frm_save_batch_data.
  CHECK gt_item_batch[] IS NOT INITIAL.

  DELETE FROM zafo_item_batch WHERE afono = gs_head-afono.

  LOOP AT gt_item_batch.
    gt_item_batch-afono = gs_head-afono.
    MODIFY gt_item_batch.
  ENDLOOP.

  CHECK gt_item_batch[] IS NOT INITIAL.

  MODIFY zafo_item_batch FROM TABLE gt_item_batch.

  COMMIT WORK AND WAIT.

ENDFORM.


FORM frm_save_po_data.

  IF gs_head-afono IS INITIAL.

    PERFORM frm_get_next_afono USING gs_object-nrnr CHANGING gs_head-afono.
    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-afono = gs_head-afono.
    ENDLOOP.

    IF sy-uname <> '10015894' .
      gs_head-erdat = sy-datum.
      gs_head-erzet = sy-uzeit.
      gs_head-ernam = sy-uname.
    ENDIF.

    PERFORM frm_set_status USING 'A'.
  ENDIF.

  IF sy-uname <> '10015894' .

    IF gs_head-erdat IS INITIAL.
      gs_head-erdat = sy-datum.
      gs_head-erzet = sy-uzeit.
      gs_head-ernam = sy-uname.
    ENDIF.

    gs_head-aedat = sy-datum.
    gs_head-aetim = sy-uzeit .
    gs_head-tcode = sy-tcode .
  ENDIF.

  LOOP AT gt_item  ASSIGNING <gs_item>.

    IF <gs_item>-del_flag = 'X'.
      PERFORM frm_set_icon
       USING 'D' CHANGING <gs_item>-icon <gs_item>-text .
    ELSE.
      PERFORM frm_set_icon
        USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text .
    ENDIF.

    MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
    APPEND gt_item_modify.
    CLEAR gt_item_modify.

  ENDLOOP.

  PERFORM frm_set_ref_save.

  LOOP AT gt_item_po  ASSIGNING <gs_item_po>.

    <gs_item_po>-afono = gs_head-afono.

    IF <gs_item_po>-loekz = 'X'.
      PERFORM frm_set_icon  USING 'D' CHANGING <gs_item_po>-icon <gs_item_po>-text .
    ELSE.

      PERFORM frm_set_icon
       USING gs_head-status CHANGING <gs_item_po>-icon <gs_item_po>-text .
    ENDIF.


    MOVE-CORRESPONDING <gs_item_po> TO gt_item_po_modify.
    APPEND gt_item_po_modify.
    CLEAR gt_item_po_modify.

  ENDLOOP.


  REFRESH gt_item_po_cpt_modify.
  LOOP AT gt_item_po_cpt ASSIGNING <gs_item_po_cpt>.
    <gs_item_po_cpt>-afono = gs_head-afono.
    MOVE-CORRESPONDING <gs_item_po_cpt> TO gt_item_po_cpt_modify.
    APPEND gt_item_po_cpt_modify.
    CLEAR gt_item_po_cpt_modify.
  ENDLOOP.

  CLEAR gs_head-cost_amount.
  LOOP AT gt_item_cost ASSIGNING FIELD-SYMBOL(<gs_item_cost>).
    gs_head-cost_amount = gs_head-cost_amount + <gs_item_cost>-cost_amount.

    <gs_item_cost>-afono = gs_head-afono.
    <gs_item_cost>-line_id = sy-tabix.
    MOVE-CORRESPONDING <gs_item_cost> TO gt_item_cost_modify.
    APPEND gt_item_cost_modify.
    CLEAR gt_item_cost_modify.
  ENDLOOP.

  MOVE-CORRESPONDING gs_head TO gs_head_modify.

  PERFORM frm_db_modify.

  IF gt_item_po_modify[] IS NOT INITIAL.
    MODIFY zafo_item_po FROM TABLE gt_item_po_modify.
    CLEAR gt_item_po_modify[].
  ENDIF.

  IF gt_item_po_cpt_modify[] IS NOT INITIAL.
    MODIFY zafo_item_po_cpt FROM TABLE gt_item_po_cpt_modify.
    CLEAR gt_item_po_cpt_modify[].
  ENDIF.



  DELETE FROM zafo_item_cost WHERE afono = gs_head-afono.

  IF gt_item_cost_modify[] IS NOT INITIAL.
    MODIFY zafo_item_cost FROM TABLE gt_item_cost_modify.
    CLEAR gt_item_po_modify[].
  ENDIF.

  IF zscmt0011 IS NOT INITIAL.
    zscmt0011-lifnr = gs_head-lifnr.
    zscmt0011-uname = sy-uname.
    MODIFY zscmt0011.
  ENDIF.

  COMMIT WORK AND WAIT.

  IF sy-subrc EQ 0.
    PERFORM frm_add_msg USING 'S' 'ZAFO' '026' '' '' '' ''."保存成功
  ENDIF.

ENDFORM.


FORM frm_lsave_po_data.

  IF gs_head-afono IS INITIAL.

    PERFORM frm_get_next_afono USING gs_object-nrnr CHANGING gs_head-afono.

    CHECK g_error IS INITIAL .

    LOOP AT gt_item ASSIGNING <gs_item>.
      <gs_item>-afono = gs_head-afono.
    ENDLOOP.

    gs_head-erdat = sy-datum.
    gs_head-erzet = sy-uzeit.
    gs_head-ernam = sy-uname.
  ENDIF.

  PERFORM frm_set_status USING 'E'.

  IF gs_head-erdat IS INITIAL.
    gs_head-erdat = sy-datum.
    gs_head-erzet = sy-uzeit.
    gs_head-ernam = sy-uname.
  ENDIF.

  gs_head-aenam = sy-uname.
  gs_head-aedat = sy-datum.
  gs_head-aetim = sy-uzeit .
  gs_head-tcode = sy-tcode .

  LOOP AT gt_item  ASSIGNING <gs_item>.

    PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text .

    IF <gs_item>-del_flag = 'X' .
      PERFORM frm_set_icon USING 'D' CHANGING <gs_item>-icon <gs_item>-text .
    ELSE.
      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text .
    ENDIF.

    MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
    APPEND gt_item_modify.
    CLEAR gt_item_modify.

  ENDLOOP.

  PERFORM frm_set_ref_save.

  LOOP AT gt_item_po ASSIGNING <gs_item_po>.

    <gs_item_po>-afono = gs_head-afono.

    IF <gs_item_po>-loekz = 'X' .
      PERFORM frm_set_icon USING 'D' CHANGING <gs_item_po>-icon <gs_item_po>-text .
    ELSE.
      PERFORM frm_set_icon USING gs_head-status CHANGING <gs_item_po>-icon <gs_item_po>-text .
    ENDIF.

    MOVE-CORRESPONDING <gs_item_po> TO gt_item_po_modify.
    APPEND gt_item_po_modify.
    CLEAR gt_item_po_modify.

  ENDLOOP.

  REFRESH gt_item_po_cpt_modify.
  LOOP AT gt_item_po_cpt ASSIGNING <gs_item_po_cpt>.
    <gs_item_po_cpt>-afono = gs_head-afono.
    MOVE-CORRESPONDING <gs_item_po_cpt> TO gt_item_po_cpt_modify.
    APPEND gt_item_po_cpt_modify.
    CLEAR gt_item_po_cpt_modify.
  ENDLOOP.

  LOOP AT gt_item_cost ASSIGNING FIELD-SYMBOL(<gs_item_cost>).
    <gs_item_cost>-afono = gs_head-afono.
    <gs_item_cost>-line_id = sy-tabix.
    MOVE-CORRESPONDING <gs_item_cost> TO gt_item_cost_modify.
    APPEND gt_item_cost_modify.
    CLEAR gt_item_cost_modify.
  ENDLOOP.

  MOVE-CORRESPONDING gs_head TO gs_head_modify.

  PERFORM frm_db_modify.

  IF gt_item_po_modify[] IS NOT INITIAL.
    MODIFY zafo_item_po FROM TABLE gt_item_po_modify.
    CLEAR gt_item_po_modify[].
  ENDIF.


  IF gt_item_po_cpt_modify[] IS NOT INITIAL.
    MODIFY zafo_item_po_cpt FROM TABLE gt_item_po_cpt_modify.
    CLEAR gt_item_po_cpt_modify[].
  ENDIF.

  DELETE FROM zafo_item_cost WHERE afono = gs_head-afono.
  IF gt_item_cost_modify[] IS NOT INITIAL.
    MODIFY zafo_item_cost FROM TABLE gt_item_cost_modify.
    CLEAR gt_item_po_modify[].
  ENDIF.

  IF zscmt0011 IS NOT INITIAL.
    zscmt0011-lifnr = gs_head-lifnr.
    zscmt0011-uname = sy-uname.
    MODIFY zscmt0011.
  ENDIF.

  COMMIT WORK AND WAIT.

  IF sy-subrc EQ 0.
    PERFORM frm_add_msg USING 'S' 'ZAFO' '026' '' '' '' ''."保存成功
  ENDIF.

ENDFORM.


FORM frm_db_modify.

  IF gs_head_modify IS NOT INITIAL.
    MODIFY zafo_head FROM gs_head_modify.
    CLEAR gs_head_modify.
  ENDIF.

  IF gt_item_modify[] IS NOT INITIAL.
    MODIFY zafo_item FROM TABLE gt_item_modify.
    CLEAR gt_item_modify[].
  ENDIF.

  IF gs_head_modify_ref IS NOT INITIAL.
    MODIFY zafo_head FROM gs_head_modify_ref.
    CLEAR gs_head_modify_ref.
  ENDIF.

  IF g_no_commit IS INITIAL.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.


FORM frm_delete_po.
  DATA: po_header  TYPE TABLE OF  bapimepoheader WITH HEADER LINE,
        po_headerx TYPE TABLE OF  bapimepoheaderx WITH HEADER LINE.
  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CHECK gs_bustyp-execute_type = 'PO' OR gs_bustyp-execute_type = 'PRO'.
  CHECK gs_head-ebeln IS NOT INITIAL.

  po_header-po_number = gs_head-ebeln.
  po_header-delete_ind = 'X'.
  po_headerx-delete_ind = 'X'.

  APPEND po_header.
  APPEND po_headerx.


  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = gs_head-ebeln
      poheader      = po_header
      poheaderx     = po_headerx
    TABLES
      return        = lt_return.

  LOOP AT lt_return.
    PERFORM frm_add_msg USING lt_return-type
                          lt_return-id
                          lt_return-number
                          lt_return-message_v1
                          lt_return-message_v2
                          lt_return-message_v3
                          lt_return-message_v4.
  ENDLOOP.

  IF g_error = 'X'.
    ROLLBACK WORK .
    RETURN.
  ELSE.
    UPDATE zafo_item SET del_flag = 'X' WHERE afono = gs_head-afono.
    UPDATE zafo_item_po SET loekz = 'X'  WHERE afono = gs_head-afono.
  ENDIF.
ENDFORM.

FORM frm_delete_data.
  PERFORM frm_set_ref_delete.
  PERFORM frm_update_head_status USING 'D'.
ENDFORM.


FORM frm_delete_sd.
  DATA: so_header TYPE bapisdh1.
  DATA: so_headerx TYPE bapisdh1x.

  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.


  CHECK gs_bustyp-execute_type = 'SO'.

  CHECK gs_head-vbeln_va IS NOT INITIAL.


  so_headerx-updateflag = 'D'.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = gs_head-vbeln_va
      order_header_in  = so_header
      order_header_inx = so_headerx
    TABLES
      return           = lt_return.


  LOOP AT lt_return.
    PERFORM frm_add_msg USING lt_return-type
                          lt_return-id
                          lt_return-number
                          lt_return-message_v1
                          lt_return-message_v2
                          lt_return-message_v3
                          lt_return-message_v4.
  ENDLOOP.

  IF g_error = 'X'.
    ROLLBACK WORK .
    RETURN.
  ELSE.
    UPDATE zafo_item SET del_flag = 'X' WHERE afono = gs_head-afono.
  ENDIF.
ENDFORM.
