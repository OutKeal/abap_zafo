*&---------------------------------------------------------------------*
*& 包含               ZAFO_I02_F01
*&---------------------------------------------------------------------*

FORM frm_conversion_alpha CHANGING c_value.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_value
    IMPORTING
      output = c_value.
ENDFORM.


FORM frm_call_transaction USING tr_type.

  CASE tr_type.
    WHEN 'GS_HEAD-MBLNR'.
*      SET PARAMETER ID 'MBN' FIELD gs_head-mblnr.
*      SET PARAMETER ID 'MJA' FIELD gs_head-mjahr.
*      CALL TRANSACTION 'MIGO' AND SKIP FIRST SCREEN.
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_action            = 'A04'
          i_refdoc            = 'R02'
          i_notree            = 'X'
          i_skip_first_screen = 'X'
          i_okcode            = 'OK_GO'
          i_mblnr             = gs_head-mblnr
          i_mjahr             = gs_head-mjahr
        EXCEPTIONS
          illegal_combination = 0
          OTHERS              = 0.

    WHEN 'GS_HEAD-EBELN'.
      SET PARAMETER ID 'BES' FIELD gs_head-ebeln .
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

    WHEN 'GS_HEAD-VBELN_VA'.
      SET PARAMETER ID 'AUN' FIELD gs_head-vbeln_va .
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    WHEN 'GS_HEAD-BANFN'.
      SET PARAMETER ID 'BAN' FIELD gs_head-banfn .
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

  ENDCASE.
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
    MESSAGE s029."已取消操作'.
  ELSE.
  ENDIF.

ENDFORM.


FORM frm_pop_get_info USING text CHANGING date remark .
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.

  CLEAR ls_flds.
  ls_flds-tabname = 'ZAFO_HEAD'.
  ls_flds-fieldname = 'BUDAT'.
  ls_flds-value = date.
  ls_flds-field_obl = 'X'.
  APPEND ls_flds TO lt_flds.

  CLEAR ls_flds.
  ls_flds-tabname = 'ZAFO_HEAD'.
  ls_flds-fieldname = 'REMARK1'.
  ls_flds-value = remark.
  APPEND ls_flds TO lt_flds.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = text
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc EQ 0 AND p_gv_ret_code <> 'A'.

    READ TABLE lt_flds INTO ls_flds INDEX 1.
    date = ls_flds-value.

    READ TABLE lt_flds INTO ls_flds INDEX 2.
    remark = ls_flds-value.

  ELSE.
    MESSAGE s029 DISPLAY LIKE 'E'."已取消操作
    g_error = 'X'.
    RETURN.
  ENDIF.

ENDFORM.


FORM frm_set_round USING meins CHANGING value.
  DATA: lv_round LIKE gs_head-round.

  CHECK meins IS NOT INITIAL.
  CHECK g_bustyp <> 'R2001'.

  SELECT SINGLE andec INTO @DATA(ls_andec) FROM t006 WHERE msehi = @meins.
  IF sy-subrc = 0.
    lv_round = ls_andec.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals = lv_round
        input    = value
        sign     = '+'
      IMPORTING
        output   = value.
  ENDIF.
ENDFORM.


FORM frm_set_round_gr USING meins CHANGING value." 入库特殊取整
  DATA:new_value TYPE menge_d.
  DATA:cy_value TYPE menge_d.

  CHECK meins IS NOT INITIAL.
  CHECK g_bustyp <> 'R2001'.

  IF gs_head-round IS  INITIAL OR gs_head-round = 0.
    SELECT SINGLE andec INTO @DATA(ls_andec) FROM t006 WHERE msehi = @meins.
    gs_head-round = ls_andec.
  ENDIF.

  IF value > 20.
    gs_head-round = 0.
  ENDIF.

  CALL FUNCTION 'ROUND'
    EXPORTING
      decimals = gs_head-round
      input    = value
      sign     = '+'
    IMPORTING
      output   = new_value.

  IF new_value <> value.
    cy_value = new_value - value.
    IF cy_value > '0.5' AND new_value > 1.
      new_value = new_value - 1.
    ENDIF.
  ENDIF.

  value = new_value.
ENDFORM.


FORM frm_pop_round CHANGING round_type  .
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.

  CLEAR ls_flds.
  ls_flds-tabname = 'ZAFO_SROUND'.
  ls_flds-fieldname = 'ZAFO_ROUND'.
  ls_flds-field_obl = 'X'.
  ls_flds-value = '0'.
  APPEND ls_flds TO lt_flds.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-029 "'请输入取整的小数位'
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc EQ 0 AND p_gv_ret_code <> 'A'.
    READ TABLE lt_flds INTO ls_flds INDEX 1.
    round_type = ls_flds-value.
  ELSE.
    g_error = 'X'.
    MESSAGE s042 DISPLAY LIKE 'E'."已取消
    RETURN.
  ENDIF.

ENDFORM.


" 特殊物料起订量取整
FORM frm_set_qdl_round USING round_type input_lo CHANGING output_ln.
  DATA:lv_ys TYPE menge_d.
  CASE round_type.
    WHEN '250'.
      lv_ys = input_lo MOD 250.
      IF lv_ys > 0.
        output_ln = 250 - lv_ys.
      ELSEIF input_lo = 0.
        output_ln = 250.
      ENDIF.

  ENDCASE.

  output_ln = input_lo + output_ln.
ENDFORM.


FORM frm_pop_get_zppdhd CHANGING pv_zppdhd.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.

  CLEAR pv_zppdhd .
  CLEAR ls_flds.

  ls_flds-tabname = 'ZTPP0091'.
  ls_flds-fieldname = 'ZPPDHD'.
  ls_flds-value = 1.
  ls_flds-field_obl = 'X'.
  APPEND ls_flds TO lt_flds.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-034
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc EQ 0 AND p_gv_ret_code <> 'A'.

    READ TABLE lt_flds INTO ls_flds INDEX 1.
    pv_zppdhd = ls_flds-value.

  ELSE.

    MESSAGE s029 DISPLAY LIKE 'E'."已取消操作
    g_error = 'X'.
    RETURN.

  ENDIF.

ENDFORM.


FORM frm_pop_get_lifnr CHANGING pv_lifnr.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.

  CLEAR pv_lifnr.
  CLEAR ls_flds.

  ls_flds-tabname = 'ZSCMT0010'.
  ls_flds-fieldname = 'PARTNER'.
  ls_flds-value = 1.
  ls_flds-field_obl = 'X'.
  APPEND ls_flds TO lt_flds.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = '请输入供应商编号'
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc EQ 0 AND p_gv_ret_code <> 'A'.

    READ TABLE lt_flds INTO ls_flds INDEX 1.
    pv_lifnr = ls_flds-value.

    DATA: lt_zscmt0010 TYPE TABLE OF zscmt0010 WITH HEADER LINE.

    SELECT * FROM zscmt0010
      INTO TABLE @lt_zscmt0010
      WHERE partner = @pv_lifnr.
    IF sy-subrc = 0.

      READ TABLE lt_zscmt0010 WITH KEY bukrs = gs_head-bukrs.
      IF sy-subrc = 0.
        gs_head-lifnr = pv_lifnr.
        gs_head-waers = lt_zscmt0010-waers.
        gs_head-lifnr_name = lt_zscmt0010-zname1.
        gs_head-land1 = lt_zscmt0010-country.
      ELSE.
        MESSAGE s000  WITH '该供应商未扩展到该公司' DISPLAY LIKE 'E'.
        CLEAR pv_lifnr.
      ENDIF.

    ELSE.

      CLEAR pv_lifnr.
    ENDIF.

  ELSE.
    MESSAGE s029 DISPLAY LIKE 'E'."已取消操作
    CLEAR pv_lifnr.
  ENDIF.

ENDFORM.


FORM frm_lock.
  CLEAR g_locked.
  CHECK gs_head-afono IS NOT INITIAL.

  CALL FUNCTION 'ENQUEUE_EZAFO_HEAD'
    EXPORTING
      mode_zafo_head = 'E'
      mandt          = sy-mandt
      afono          = gs_head-afono
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'I'.
    g_locked = 'L'.
  ENDIF.

ENDFORM.


FORM frm_unlock.

  CHECK gs_head-afono IS NOT INITIAL.
  CALL FUNCTION 'DEQUEUE_EZAFO_HEAD'
    EXPORTING
      mode_zafo_head = 'E'
      mandt          = sy-mandt
      afono          = gs_head-afono.

  CLEAR g_locked.

ENDFORM.


FORM process USING p_info.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 100
      text       = p_info
    EXCEPTIONS
      OTHERS     = 1.
ENDFORM.


FORM frm_add_msg USING msgty msgid msgno msgv1 msgv2 msgv3 msgv4.

  PERFORM frm_conversion_msg CHANGING msgty msgid msgno msgv1 msgv2 msgv3 msgv4.

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


FORM frm_conversion_msg CHANGING msgty msgid msgno msgv1 msgv2 msgv3 msgv4.

  CASE msgid.
    WHEN 'F5'.
      CASE msgno.
        WHEN 123.
          msgid = 'ZAFO'.
          msgno = 000.
          msgv1 = '公司' && msgv1 && '，'.
          msgv2 = '公司' && msgv2 && '，'.
          msgv3 = '不能直接处理，'.
          msgv4 = '存在跨公司业务！'.
      ENDCASE.
  ENDCASE.
ENDFORM.


FORM frm_clear_msg .
  CLEAR gt_message[].
  CLEAR ot_return[].

ENDFORM.


FORM frm_pop_msg.
  DATA: lt_message LIKE TABLE OF  gt_message WITH HEADER LINE.

  CHECK gt_message[] IS NOT INITIAL.

  READ TABLE gt_message WITH KEY msgty = 'E'.
  IF sy-subrc EQ 0.
    REFRESH lt_message.
    LOOP AT gt_message INTO lt_message WHERE msgty = 'E'.
      APPEND lt_message.
    ENDLOOP.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_message[].
    PERFORM frm_clear_msg.
    RETURN.
  ENDIF.

  READ TABLE gt_message WITH KEY msgty = 'W'.
  IF sy-subrc EQ 0.
    REFRESH lt_message.
    LOOP AT gt_message INTO lt_message WHERE msgty = 'W'.
      APPEND lt_message.
    ENDLOOP.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_message[].
    DELETE gt_message WHERE msgty = 'W'.
  ENDIF.

  LOOP AT gt_message.
    EXIT.
  ENDLOOP.
  MESSAGE ID gt_message-msgid TYPE gt_message-msgty
          NUMBER gt_message-msgno
          WITH gt_message-msgv1 gt_message-msgv2 gt_message-msgv3 gt_message-msgv4.
  CLEAR gt_message[].

ENDFORM.


FORM frm_send_msg TABLES lt_user STRUCTURE zmsg_suser
                   USING u_text
                         u_object_id
                         object.
  DATA:ls_datah TYPE zmsg_data_h .
  ls_datah-object = object.
  ls_datah-object_id = u_object_id.
  ls_datah-text = u_text.

  IF g_bustyp = 'AF05'.
    lt_user-uname = '10000162'.
    SELECT SINGLE name INTO lt_user-name1
      FROM zapp_addr WHERE person = lt_user-uname.
    APPEND lt_user.
    CLEAR lt_user.
  ENDIF.

  CALL FUNCTION 'ZMSG_SAVE_DATA'
    EXPORTING
      is_datah = ls_datah
*     URGENT   =
*   IMPORTING
*     ES_RETURN =
    TABLES
      it_data  = lt_user
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.


FORM f4callback TABLES record_tab STRUCTURE seahlpres
               CHANGING      shlp  TYPE shlp_descr_t
                       callcontrol LIKE ddshf4ctrl.

  DATA: interface LIKE LINE OF shlp-interface,
        fieldprop LIKE LINE OF shlp-fieldprop.

  interface-shlpfield  = 'SAKNR'.
  interface-valfield   = 'SKA1-SAKNR'.
  APPEND interface TO shlp-interface.
  fieldprop-shlpoutput = 'X'.
  MODIFY shlp-fieldprop FROM fieldprop TRANSPORTING shlpoutput WHERE fieldname = 'SAKNR'.

ENDFORM.


FORM frm_get_alv_handle USING repid bustyp alv_name CHANGING handle.

  DATA:ls_alv_handle TYPE zafo_alv_handle.

  ls_alv_handle-repid = repid.
  ls_alv_handle-bustyp = bustyp.
  ls_alv_handle-alv_name = alv_name.

  CLEAR handle.

  TRANSLATE ls_alv_handle-alv_name TO UPPER CASE.

  SELECT SINGLE handle INTO ls_alv_handle-handle
    FROM zafo_alv_handle
    WHERE repid = ls_alv_handle-repid
    AND bustyp = ls_alv_handle-bustyp
    AND alv_name = ls_alv_handle-alv_name.
  IF sy-subrc EQ 0.
    handle = ls_alv_handle-handle.
    RETURN.
  ENDIF.

  SELECT MAX( handle ) INTO handle FROM zafo_alv_handle.

  ADD 1 TO handle.
  ls_alv_handle-handle = handle.
  MODIFY  zafo_alv_handle FROM  ls_alv_handle.

  COMMIT WORK.

ENDFORM.
