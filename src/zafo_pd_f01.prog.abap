*&---------------------------------------------------------------------*
*& 包含               ZAFO_PD_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form frm_set_sel_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_sel_screen .



  LOOP AT SCREEN.


    IF p_cre = 'X'.
      IF screen-group1 = 'CRE'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'DIS'.
        screen-active = '0'.
      ENDIF.
    ELSEIF p_dis = 'X'.
      IF screen-group1 = 'DIS'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'CRE'.
        screen-active = '0'.
      ENDIF.
    ENDIF.



    MODIFY SCREEN.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_stock
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_stock .

  CLEAR gt_pd_item[].
  SELECT
    a~werks      ,
    a~lgort      ,
    a~vbeln      ,
    a~posnr      ,
    a~charg      ,
    d~zcate1     ,
    d~zcate2     ,
    a~matnr      ,
    d~maktx      ,
*    D~IDNLF      ,
    c~zcolor     ,
    c~zsize      ,
    c~znorms     ,
    c~zppflag    ,
    c~zvat_nub    ,
    c~zshelves   ,
    b~zzpino     ,
    b~zppdhd     ,
    b~zkunnr_mat ,
    b~kunnr      ,
    e~name1      ,
    d~idnlf      ,
    a~kalab AS menge      ,
    a~kalab AS menge_sp      ,
    d~meins
    INTO CORRESPONDING FIELDS OF TABLE @gt_pd_item
    FROM mska AS a
    INNER JOIN ztpp0089 AS b
    ON a~vbeln = b~vbeln AND a~posnr = b~posnr
    INNER JOIN zmch1 AS c
    ON a~charg = c~charg
    INNER JOIN zmmv0010 AS d
    ON a~matnr = d~matnr
    INNER JOIN kna1 AS e
    ON b~kunnr = e~kunnr
    WHERE a~werks = @p_werks
    AND a~lgort = @p_lgort
    AND d~zcate1 IN @s_zcate1
    AND d~zcate2 IN @s_zcate2
    AND a~matnr  IN @s_matnr
    AND d~idnlf  IN @s_idnlf
    AND b~zzpino IN @s_zzpino
    AND b~zppdhd IN @s_zppdhd
    AND b~kunnr  IN @s_kunnr
    AND e~name1  IN @s_name1
    AND kalab <> 0.


  SELECT
     a~werks      ,
     a~lgort      ,
     a~charg      ,
     d~zcate1     ,
     d~zcate2     ,
     a~matnr      ,
     d~maktx      ,
     d~idnlf      ,
     c~zcolor     ,
     c~zsize      ,
     c~znorms     ,
     c~zppflag    ,
     c~zvat_nub    ,
     c~zshelves   ,
*     b~zzpino     ,
*     b~zppdhd     ,
*     b~zkunnr_mat ,
*     b~kunnr      ,
*     e~name1      ,
     a~clabs AS menge      ,
     a~clabs AS menge_sp      ,
     d~meins
     APPENDING CORRESPONDING FIELDS OF TABLE @gt_pd_item
     FROM mchb AS a
*     INNER JOIN ztpp0089 AS b
*     ON a~vbeln = b~vbeln AND a~posnr = b~posnr
     INNER JOIN zmch1 AS c
     ON a~charg = c~charg
     INNER JOIN zmmv0010 AS d
     ON a~matnr = d~matnr
*     INNER JOIN kna1 AS e
*     ON b~kunnr = e~kunnr
     WHERE a~werks = @p_werks
     AND a~lgort = @p_lgort
     AND d~zcate1 IN @s_zcate1
     AND d~zcate2 IN @s_zcate2
     AND a~matnr  IN @s_matnr
     AND d~idnlf  IN @s_idnlf
     AND clabs    <> 0.
*     AND b~zzpino IN @s_zzpino
*     AND b~zppdhd IN @s_zppdhd
*     AND b~kunnr  IN @s_kunnr
*     AND e~name1  IN @s_name1.




ENDFORM.


FORM  frm_get_pd .

  SELECT * FROM zafo_pd_head
    INTO TABLE @gt_pd_head
    WHERE afopd     IN @s_afopd
      AND werks     EQ @p_werks
      AND lgort     IN @s_lgort
      AND remark    IN @s_remark
      AND pdstatus  IN @s_status
      AND erdat     IN @s_erdat .
  IF sy-subrc NE 0.
    MESSAGE '无数据' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

FORM frm_clear_msg .
  CLEAR gt_message[].
ENDFORM.

FORM frm_pop_msg .
  IF gt_message[] IS NOT INITIAL.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = gt_message[].
    PERFORM frm_clear_msg.
  ENDIF.

ENDFORM.

FORM frm_pop_confirm USING text .
  DATA: l_answer(1) TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = 'N'
      textline1      = text
      titel          = '确认'
      start_column   = 15
      start_row      = 5
      cancel_display = ''
    IMPORTING
      answer         = l_answer.
  IF l_answer <> 'J'.
    g_error = 'X'.

    MESSAGE s000(zafo) WITH '已取消操作'.
  ELSE.
  ENDIF.

ENDFORM.


FORM frm_save_data.


  IF gt_pd_head-afopd IS INITIAL.

    PERFORM frm_get_next_afopd
            CHANGING gt_pd_head-afopd.
    LOOP AT gt_pd_item ASSIGNING <gs_pd_item>.
      <gs_pd_item>-afopd = gt_pd_head-afopd.
      <gs_pd_item>-afonr = sy-tabix.
    ENDLOOP.

    gt_pd_head-erdat = sy-datum.
    gt_pd_head-erzet = sy-uzeit.
    gt_pd_head-ernam = sy-uname.
    gt_pd_head-pdstatus = 'A'.
  ENDIF.

  gt_pd_head-modat = sy-datum.
  gt_pd_head-mozet = sy-uzeit .
  gt_pd_head-monam = sy-uname .





  MODIFY zafo_pd_head FROM gt_pd_head.
  MODIFY zafo_pd_item FROM TABLE gt_pd_item.

  COMMIT WORK AND WAIT.


ENDFORM.



FORM frm_get_next_afopd  CHANGING ls_afopd.
  CLEAR ls_afopd.
  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'       " 锁定编码对象
    EXPORTING
      object           = 'ZAFOPD'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'            " 获取流水号
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZAFOPD'
    IMPORTING
      number                  = ls_afopd
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      internal_overflow       = 6
      OTHERS                  = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'       " 释放编码对象锁
    EXPORTING
      object = 'ZAFOPD'.
  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

FORM frm_post_data.

  DATA:ls_head TYPE  zafo_head .
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE .

  LOOP AT gt_pd_item WHERE menge_cy <> 0 .
    MOVE-CORRESPONDING gt_pd_item TO lt_item.
    lt_item-menge1 = gt_pd_item-menge_sp.
    lt_item-menge2 = gt_pd_item-menge_yc.
    lt_item-menge4 = gt_pd_item-menge.
    lt_item-menge = gt_pd_item-menge_cy.
    lt_item-vbeln_va = gt_pd_item-vbeln.
    lt_item-posnr_va = gt_pd_item-posnr.
    APPEND lt_item.
    CLEAR lt_item.
  ENDLOOP.

  IF lt_item[] IS INITIAL.
    PERFORM frm_pop_confirm USING '盘点无差异,是否关闭盘点任务?'.
    CHECK g_error IS INITIAL.
    gt_pd_head-pdstatus = 'S'.
    UPDATE zafo_pd_head SET pdstatus = 'S'
      WHERE afopd = gt_pd_head-afopd.
    COMMIT WORK AND WAIT.
    RETURN.


  ELSE.
    ls_head-werks = gt_pd_head-werks.
    ls_head-lgort = gt_pd_head-lgort.


    CALL FUNCTION 'ZAFO_CREATE_SAVE'
      EXPORTING
        i_bustyp     = '5001'
        no_authcheck = 'X'
      TABLES
        et_return    = lt_return
        ct_item      = lt_item
*       CT_ITEM_PO   =
*       CT_ITEM_COST =
      CHANGING
        cs_head      = ls_head
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.

    ENDIF.


    IF ls_head-afono IS NOT INITIAL.
      gt_pd_head-afono = ls_head-afono.
      gt_pd_head-pdstatus = 'S'.
      UPDATE zafo_pd_head
      SET pdstatus = 'S'
          afono = ls_head-afono
      WHERE afopd = gt_pd_head-afopd.
      COMMIT WORK AND WAIT.
      MESSAGE '盘点单差异生成成功' TYPE 'S'  .

      SET PARAMETER ID 'ZAFONO' FIELD ls_head-afono.

      CALL TRANSACTION 'ZAFO_PD1' AND SKIP FIRST SCREEN.
      RETURN.
    ELSE.
      MESSAGE '盘点单差异生成失败' TYPE 'S'  DISPLAY LIKE 'E'.
    ENDIF.

    LEAVE TO SCREEN 0.


  ENDIF.


ENDFORM.

FORM frm_commit_appr.

  DATA:lt_flow_head LIKE TABLE OF  zapp_flow_head WITH HEADER LINE.
  DATA:lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE.


  CLEAR lt_return[].
  PERFORM frm_clear_msg.

  CALL FUNCTION 'ZAPP_FLOW_CREATE'
    EXPORTING
      object       = 'PDSP'
      line         = gt_pd_head
*     key1         = gs_mepoheader-ebeln
*     key2         = gv_key2
*     key4         = gv_key4
*     user1        =
*     user2        =
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
    gt_pd_head-pdstatus = 'B'.
    gt_pd_head-modat = sy-datum.
    gt_pd_head-mozet = sy-uzeit.
    gt_pd_head-monam = sy-uname.

    MODIFY zafo_pd_head FROM gt_pd_head.
    COMMIT WORK AND WAIT.
  ENDIF.

  PERFORM frm_pop_msg.
ENDFORM.


FORM frm_add_msg USING msgty
                        msgid
                        msgno
                        msgv1
                        msgv2
                        msgv3
                        msgv4.

  CLEAR gt_message.
  gt_message-msgid = msgid .
  gt_message-msgty = msgty .
  gt_message-msgno = msgno .
  gt_message-msgv1 = msgv1 .
  gt_message-msgv2 = msgv2 .
  gt_message-msgv3 = msgv3 .
  gt_message-msgv4 = msgv4 .
  APPEND gt_message.


  CLEAR ot_return.
  IF msgty = 'E' OR msgty =  'A'.
    g_error = 'X'.
  ENDIF.
  ot_return-type = msgty.
  ot_return-id = msgid.
  ot_return-number = msgno.
  ot_return-message_v1 = msgv1.
  ot_return-message_v2 = msgv2.
  ot_return-message_v3 = msgv3.
  ot_return-message_v4 = msgv4.
  MESSAGE ID ot_return-id TYPE ot_return-type NUMBER ot_return-number
     INTO ot_return-message WITH ot_return-message_v1
                                 ot_return-message_v2
                                 ot_return-message_v3
                                 ot_return-message_v4.
  APPEND ot_return.

ENDFORM.
