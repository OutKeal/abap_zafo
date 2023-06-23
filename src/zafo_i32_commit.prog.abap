*&---------------------------------------------------------------------*
*& 包含               frm_commit
*&---------------------------------------------------------------------*


FORM frm_commit.
  DATA:ls_text TYPE zmsg_text.
  DATA:ls_object_id TYPE zmsg_object_id.
  DATA:lt_user TYPE TABLE OF zmsg_suser WITH HEADER LINE.

  IF gs_bustyp-execute_type = 'PO' OR gs_bustyp-execute_type = 'PRO' .
    PERFORM frm_commit_po_appr.
    RETURN.
  ENDIF.

  IF gs_bustyp-execute_type = 'PR'.
    PERFORM frm_commit_pr_appr.
    RETURN.
  ENDIF.

  IF gs_bustyp-bustyp = 'POF01'.
    PERFORM frm_pof01_appr.
  ENDIF.

  IF gs_bustyp-bustyp = 'DUC01'.
    PERFORM frm_duc01_appr.
  ENDIF.


  IF gs_head-app_status = 'A'.
    PERFORM frm_commit_appr.
    EXIT.
  ENDIF.

  IF gs_head-app_status = ''.
    PERFORM frm_update_head_status USING 'C'.

    CHECK gs_head-nenam IS NOT INITIAL.

    ls_object_id = gs_head-afono.
    lt_user-uname = gs_head-nenam.
    SELECT SINGLE name INTO lt_user-name1
      FROM zapp_addr WHERE person = lt_user-uname.
    APPEND lt_user.
    CLEAR lt_user.

    ls_text = '合同号:' && gs_head-group1.

    PERFORM frm_send_msg TABLES lt_user USING ls_text ls_object_id 'ZAFO'.

  ENDIF.


ENDFORM.


FORM frm_uncommit.

  IF gs_head-app_status = '' AND gs_head-status = 'C'.
    PERFORM frm_update_head_status USING 'A'.
  ENDIF.
ENDFORM.

FORM frm_commit_appr.
  DATA:lt_flow_head LIKE TABLE OF  zapp_flow_head WITH HEADER LINE.
  DATA:lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.

  CHECK gs_head-app_status = 'A'.

  CLEAR lt_return[].
  PERFORM frm_clear_msg.

  CALL FUNCTION 'ZAPP_FLOW_CREATE'
    EXPORTING
      object       = gs_object-app_object
      line         = gs_head
*     key1         = gs_mepoheader-ebeln
*     key2         = gv_key2
*     key4         = gv_key4
      user1        = gs_head-afnam
      user2        = ''
      commit       = ''
    TABLES
      et_flow_head = lt_flow_head[]
*     ET_FLOW_ITEM =
      et_return    = lt_return
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    g_error = 'X'.
  ENDIF.
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
  ELSE.
    gs_head-app_status = 'B'.
    PERFORM frm_update_head_status USING 'B'.
  ENDIF.

  PERFORM frm_pop_msg.
ENDFORM.


FORM frm_pof01_appr.
  DATA:lt_flow_head LIKE TABLE OF  zapp_flow_head WITH HEADER LINE.
  DATA:lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: lv_yw_user TYPE sy-uname.

  CLEAR lt_return[].
  PERFORM frm_clear_msg.

  PERFORM frm_get_yw_user CHANGING lv_yw_user.

  CALL FUNCTION 'ZAPP_FLOW_CREATE'
    EXPORTING
      object       = gs_object-app_object
      line         = gs_head
*     key1         = gs_mepoheader-ebeln
*     key2         = gv_key2
*     key4         = gv_key4
      user1        = gs_head-afnam
      user2        = lv_yw_user
      commit       = ''
    TABLES
      et_flow_head = lt_flow_head[]
*     ET_FLOW_ITEM =
      et_return    = lt_return
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    g_error = 'X'.
  ENDIF.
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
  ELSE.
    gs_head-app_status = 'B'.
    PERFORM frm_update_head_status USING 'B'.
  ENDIF.

  PERFORM frm_pop_msg.

ENDFORM.


FORM frm_duc01_appr.
  DATA:lt_flow_head LIKE TABLE OF  zapp_flow_head WITH HEADER LINE.
  DATA:lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: pv_user TYPE sy-uname.

  CHECK gs_head-app_status = 'A'.

  CLEAR pv_user.

  CLEAR lt_return[].
  PERFORM frm_clear_msg.

  PERFORM frm_get_yw_manager CHANGING pv_user.

  CALL FUNCTION 'ZAPP_FLOW_CREATE'
    EXPORTING
      object       = gs_object-app_object
      line         = gs_head
*     key1         = gs_mepoheader-ebeln
*     key2         = gv_key2
*     key4         = gv_key4
      user1        = gs_head-afnam
      user2        = pv_user
      commit       = ''
    TABLES
      et_flow_head = lt_flow_head[]
*     ET_FLOW_ITEM =
      et_return    = lt_return
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    g_error = 'X'.
  ENDIF.
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
  ELSE.
    gs_head-app_status = 'B'.
    PERFORM frm_update_head_status USING 'B'.
  ENDIF.

  PERFORM frm_pop_msg.

ENDFORM.


FORM frm_get_cg_user CHANGING pv_user.

  READ TABLE gt_item INTO DATA(ls_item) INDEX 1.
  CHECK ls_item-ebeln IS NOT INITIAL.

  SELECT SINGLE ernam
    FROM zafo_head INTO pv_user
   WHERE ebeln = ls_item-ebeln
     AND afono = ls_item-ebeln.

ENDFORM.


FORM frm_get_yw_user CHANGING pv_user." 获取明细行成衣合同业务员
  READ TABLE gt_item INTO DATA(ls_item) INDEX 1.
  CHECK ls_item-zzpino IS NOT INITIAL.

  SELECT SINGLE ernam
    FROM zsdsch INTO pv_user
    WHERE zzpino = ls_item-zzpino.
ENDFORM.


FORM frm_get_yw_manager CHANGING pv_user." 获取明细行成衣合同业务员经理

  READ TABLE gt_item INTO DATA(ls_item) INDEX 1.
  CHECK ls_item-zzpino IS NOT INITIAL.

  IF ls_item-zzpino = 'ATASPY22001'
    OR ls_item-zzpino = 'YN2022'.
    pv_user = '10006646'.
    RETURN.
  ENDIF.

  SELECT SINGLE ernam
    FROM zsdsch INTO pv_user
    WHERE zzpino = ls_item-zzpino.

  SELECT SINGLE department INTO @DATA(lv_department)
    FROM zapp_addr
    WHERE person = @pv_user.

  SELECT SINGLE manager
    INTO @pv_user
    FROM zapp_manager
    WHERE department = @lv_department.

ENDFORM.


FORM frm_commit_po_appr.
  DATA:lt_flow_head LIKE TABLE OF  zapp_flow_head WITH HEADER LINE.
  DATA:lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA:lv_user TYPE sy-uname.
  DATA:gv_key1 TYPE char20.
  DATA:gv_key2 TYPE char20.
  DATA:gv_key3 TYPE char20.
  DATA:gv_key4 TYPE char20.
  DATA:gv_key5 TYPE char20.
  DATA:gv_key6 TYPE char20.


  CHECK gs_head-app_status = 'A'.
  CLEAR lt_return[].
  PERFORM frm_clear_msg.

  SELECT SINGLE
      zafo_head~bustyp,
      zafo_head~remark1,
      zafo_head~cost_amount,
      ekko~bukrs,
      ekko~waers,
      ekko~ekgrp,
      ekko~bsart,
      ekko~ebeln,
      ekko~bedat
    INTO @DATA(ls_ekko)
    FROM ekko
    LEFT JOIN zafo_head ON zafo_head~ebeln = ekko~ebeln
    WHERE ekko~ebeln = @gs_head-ebeln.


  CASE gs_bustyp-bustyp.
    WHEN 'PO001' OR 'PO002' OR 'PO004'.
      PERFORM frm_get_yw_manager CHANGING lv_user.
    WHEN 'PO003'  OR 'PO005'.
      IF gs_head-bukrs EQ '1010' .
        lv_user = '10006305'."肖庆英
      ELSE.
        PERFORM frm_get_yw_manager CHANGING lv_user.
      ENDIF.
    WHEN OTHERS.
      PERFORM frm_get_cg_user CHANGING lv_user.
  ENDCASE.

  gv_key2 = gs_head-lifnr_name.
  gv_key4 = gs_head-amount.


  CALL FUNCTION 'BAPI_PO_RELEASE'
    EXPORTING
      purchaseorder          = gs_head-ebeln
      po_rel_code            = 'P0'
      no_commit              = 'X'
    TABLES
      return                 = lt_return
    EXCEPTIONS
      authority_check_fail   = 1
      document_not_found     = 2
      enqueue_fail           = 3
      prerequisite_fail      = 4
      release_already_posted = 5
      responsibility_fail    = 6
      OTHERS                 = 7.
  IF sy-subrc NE 0.
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
      PERFORM frm_pop_msg.
      RETURN.
    ENDIF.
  ENDIF.

  IF gs_head-afnam IS INITIAL.
    READ TABLE gt_item INDEX 1.
    gs_head-afnam = gt_item-afnam.
  ENDIF.
  CALL FUNCTION 'ZAPP_FLOW_CREATE'
    EXPORTING
      object       = gs_object-app_object
      line         = ls_ekko
*     key1         = gs_mepoheader-ebeln
      key2         = gv_key2
      key4         = gv_key4
      key6         = gv_key6
      user1        = gs_head-afnam
      user2        = lv_user
      commit       = ''
    TABLES
      et_flow_head = lt_flow_head[]
*     ET_FLOW_ITEM =
      et_return    = lt_return
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

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
    PERFORM frm_pop_msg.
    RETURN.
  ENDIF.

  gs_head-app_status = 'B'.
  PERFORM frm_update_head_status USING 'B'.

  COMMIT WORK AND WAIT.

  PERFORM frm_pop_msg.

ENDFORM.


FORM frm_commit_pr_appr.
  DATA:lt_flow_head LIKE TABLE OF  zapp_flow_head WITH HEADER LINE.
  DATA:lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: lv_user TYPE sy-uname.
  DATA:gv_key1 TYPE char20.
  DATA:gv_key2 TYPE char20.
  DATA:gv_key3 TYPE char20.
  DATA:gv_key4 TYPE char20.
  DATA:gv_key5 TYPE char20.
  DATA:gv_key6 TYPE char20.

  CHECK gs_head-app_status = 'A'.

  CLEAR lt_return[].
  PERFORM frm_clear_msg.


  READ TABLE gt_item INDEX 1.

  CASE gs_bustyp-bustyp.
    WHEN 'PR004'.
      IF gs_head-bukrs EQ '1010' .
        SELECT SINGLE manager
          INTO @lv_user
          FROM zapp_manager
          WHERE department = '山东生产部'.
      ELSE.
        SELECT SINGLE zsfmbdhd INTO @DATA(lv_zsfmbdhd)
          FROM ztpp0089 WHERE zppdhd = @gt_item-zppdhd.
        IF sy-subrc EQ 0 AND lv_zsfmbdhd = 'X'.
          SELECT SINGLE manager
            INTO @lv_user
            FROM zapp_manager
            WHERE department = '模杯分厂'.
        ELSE.
          PERFORM frm_get_yw_manager CHANGING lv_user.
        ENDIF.

      ENDIF.
  ENDCASE.


  SELECT MIN( bnfpo ) INTO gs_head-bnfpo FROM eban WHERE banfn EQ gs_head-banfn.

  CALL FUNCTION 'BAPI_REQUISITION_RELEASE'
    EXPORTING
      number                 = gs_head-banfn
      rel_code               = 'R0'
      item                   = gs_head-bnfpo
*     USE_EXCEPTIONS         = 'X'
      no_commit_work         = 'X'
*     IMPORTING
*     REL_STATUS_NEW         =
*     REL_INDICATOR_NEW      =
    TABLES
      return                 = lt_return
    EXCEPTIONS
      authority_check_fail   = 1
      requisition_not_found  = 2
      enqueue_fail           = 3
      prerequisite_fail      = 4
      release_already_posted = 5
      responsibility_fail    = 6
      OTHERS                 = 7.
  IF sy-subrc NE 0.
    g_error = 'X'.
    MESSAGE s027 DISPLAY LIKE 'E'."审批提交错误
  ELSE.
    CALL FUNCTION 'ZAPP_FLOW_CREATE'
      EXPORTING
        object       = gs_object-app_object
        line         = gs_head
*       key1         = gs_mepoheader-ebeln
*       key2         = gv_key2
*       key4         = gv_key4
        user1        = gs_head-afnam
        user2        = lv_user
        commit       = ''
      TABLES
        et_flow_head = lt_flow_head[]
*       ET_FLOW_ITEM =
        et_return    = lt_return
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      g_error = 'X'.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

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
  ELSE.
    gs_head-app_status = 'B'.
    PERFORM frm_update_head_status USING 'B'.

    COMMIT WORK AND WAIT.
  ENDIF.

  PERFORM frm_pop_msg.

ENDFORM.


FORM frm_goto_app.
  DATA:ls_key1 TYPE char20.

  ls_key1 = gs_head-afono.
  CALL FUNCTION 'ZAPP_FLOW_DISPLAY'
    EXPORTING
      object = gs_object-app_object
      key1   = ls_key1
*     user1  = gs_head-afnam
*     user2  = gs_head-ernam
    EXCEPTIONS
      nodata = 1
      OTHERS = 2.
  IF sy-subrc <> 0.
  ENDIF.

  SELECT SINGLE status,app_status FROM zafo_head
    INTO ( @gs_head-status,@gs_head-app_status )
    WHERE afono = @gs_head-afono.

ENDFORM.
