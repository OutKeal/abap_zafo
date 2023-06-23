*&---------------------------------------------------------------------*
*& 执行相关from               ZAFO_I02_F02
*&---------------------------------------------------------------------*

FORM frm_before_command.
  CLEAR g_refresh_catalog.
  CLEAR g_error.
  PERFORM frm_clear_msg.
ENDFORM.

FORM frm_after_command.
  CLEAR g_refresh_catalog.

ENDFORM.


FORM frm_get_new_afonr CHANGING l_afonr.

  LOOP AT gt_item ASSIGNING <gs_item>.
    IF <gs_item>-afonr > l_afonr.
      l_afonr = <gs_item>-afonr.
    ENDIF.
  ENDLOOP.

  IF gs_head-afono IS NOT INITIAL.
    SELECT MAX( afonr ) INTO @DATA(max_afonr)
      FROM zafo_item WHERE afono = @gs_head-afono.
    IF sy-subrc EQ 0 AND max_afonr > l_afonr.
      l_afonr = max_afonr.
    ENDIF.
  ENDIF.

  ADD 1 TO l_afonr.
ENDFORM.


FORM frm_get_next_afono USING nrnr CHANGING ls_afono.

  IF nrnr = 'I'.
    READ TABLE gt_item INDEX 1.
    ls_afono = gt_item-afono.
    RETURN.
  ELSEIF nrnr = 'A'.
    RETURN.
  ENDIF.


  IF gs_bustyp-execute_type = 'PO' OR gs_bustyp-execute_type = 'PRO'.
    IF gs_head-bsart IS INITIAL OR gs_head-ekgrp IS INITIAL.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '024' '' '' '' ''."请选择采购类型和采购组

      RETURN.
    ELSE.
      CLEAR nrnr.
      SELECT SINGLE numke INTO nrnr
        FROM t161 WHERE bstyp = 'F'
        AND bsart = gs_head-bsart.
      IF sy-subrc NE 0 OR nrnr IS INITIAL.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '025' '' '' '' ''."缺少采购类型号段配置
      ENDIF.
    ENDIF.

  ENDIF.

  CLEAR ls_afono.
  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'       " 锁定编码对象
    EXPORTING
      object           = 'ZAFONO'
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
      nr_range_nr             = nrnr
      object                  = 'ZAFONO'
    IMPORTING
      number                  = ls_afono
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
      object = 'ZAFONO'.
  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.


FORM frm_set_edit_status CHANGING lv_refresh_catalog.

  PERFORM frm_auth_bustyp_check  USING '02' gs_head-bustyp g_werks CHANGING g_error .

  CHECK g_error IS INITIAL.

  IF g_locked = 'L'.
    MESSAGE s052 DISPLAY LIKE 'E'."'该单已被锁定,无法修改'
    RETURN.
  ENDIF.

  CASE gs_head-status.
    WHEN  ''.
      MESSAGE s053 DISPLAY LIKE 'E'."'无效操作' TYPE 'S' DISPLAY LIKE 'E'.
    WHEN  'A' OR 'E'.
      IF g_readonly = 'D'.
        g_readonly = 'M'.
      ELSEIF g_readonly = 'M'.
        g_readonly = 'D'.
      ENDIF.
      lv_refresh_catalog = 'X'.
    WHEN OTHERS.
      MESSAGE s054 DISPLAY LIKE 'E'." '请先保存单据' TYPE 'S' DISPLAY LIKE 'E'.
  ENDCASE.
ENDFORM.


FORM frm_set_edit_status_01 CHANGING lv_refresh_catalog.

  IF g_locked = 'L'.
    MESSAGE s052 DISPLAY LIKE 'E'."'该单已被锁定,无法修改'
    RETURN.
  ENDIF.

  PERFORM frm_auth_bustyp_check
   USING '02' gs_head-bustyp g_werks
         CHANGING g_error .
  CHECK g_error IS INITIAL.

  CASE gs_head-status.
    WHEN  ''.
      MESSAGE s053 DISPLAY LIKE 'E'."'无效操作' TYPE 'S' DISPLAY LIKE 'E'.
    WHEN  'A'.
      IF g_readonly = 'D'.
        g_readonly = 'M'.
        g_action = 'MOD'.
      ELSEIF g_readonly = 'M'.
        g_action = 'DIS'.
        g_readonly = 'D'.
      ENDIF.
    WHEN 'E'.
      IF g_readonly = 'D'.
        g_readonly = 'M'.
        g_action = 'CRE'.
      ELSEIF g_readonly = 'M'.
        g_action = 'DIS'.
        g_readonly = 'D'.
      ENDIF.
    WHEN OTHERS.
      MESSAGE s054 DISPLAY LIKE 'E'." '请先保存单据' TYPE 'S' DISPLAY LIKE 'E'.
  ENDCASE.
ENDFORM.


FORM frm_set_edit_status_02 CHANGING lv_refresh_catalog.

  IF g_locked = 'L'.
    MESSAGE s052 DISPLAY LIKE 'E'."'该单已被锁定,无法修改'
    RETURN.
  ENDIF.

  PERFORM frm_auth_bustyp_check USING '02' gs_head-bustyp g_werks CHANGING g_error .
  CHECK g_error IS INITIAL.

  IF g_locked = 'L'.
    MESSAGE s052 DISPLAY LIKE 'E'."'该单已被锁定,无法修改' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CASE gs_head-status.
    WHEN  ''.
      MESSAGE s053 DISPLAY LIKE 'E'."'无效操作' TYPE 'S' DISPLAY LIKE 'E'.
    WHEN  'A' OR 'E'.
      IF g_readonly = 'D'.
        g_readonly = 'M'.
        g_action = 'MOD'.
      ELSEIF g_readonly = 'M'.
        g_readonly = 'D'.
        g_action = 'DIS'.
      ENDIF.

      lv_refresh_catalog = 'X'.

    WHEN OTHERS.
      MESSAGE s054 DISPLAY LIKE 'E'." '请先保存单据' TYPE 'S' DISPLAY LIKE 'E'.
  ENDCASE.
ENDFORM.


FORM frm_set_status USING status.
  gs_head-status = status.
  PERFORM frm_set_icon USING status CHANGING gs_head-icon gs_head-text .
  CASE status.
    WHEN ''.
      g_readonly = 'M'.

    WHEN 'E'.
      g_readonly = 'M'.

    WHEN 'A'.
      g_readonly = 'M'.

    WHEN 'B'.
      g_readonly = 'D'.

    WHEN 'C'.
      g_readonly = 'D'.

    WHEN 'S'.
      g_readonly = 'D'.

    WHEN 'T'.
      g_readonly = 'D'.

    WHEN 'L'.
      g_readonly = 'D'.

    WHEN 'D'.
      g_readonly = 'D'.

    WHEN 'F'.
      g_readonly = 'D'.
  ENDCASE.
ENDFORM.


FORM frm_set_icon USING status CHANGING icon text.
  CASE status.
    WHEN ''.
      icon = icon_led_inactive.
      text = TEXT-002."'未保存'.

    WHEN 'A'.
      icon = icon_led_yellow.
      text = TEXT-003."'已保存'.

    WHEN 'B'.
      icon = icon_structure.
      text = TEXT-004."'审核中'.

    WHEN 'C'.
      icon = icon_led_green.
      text = TEXT-005."'已审核'.

    WHEN 'S'.
      icon = icon_complete.
      text = TEXT-006."'已完成'.

    WHEN 'T'.
      icon = icon_allow.
      text = TEXT-007."'后续已完成'.

    WHEN 'L'.
      icon = icon_locked.
      text = TEXT-008."'已锁定'.

    WHEN 'D'.
      icon = icon_delete.
      text = TEXT-009."'已作废'.

    WHEN 'E'.
      icon = icon_led_yellow.
      text = TEXT-010."'暂存'.

    WHEN 'F'.
      icon = icon_status.
      text = TEXT-047."'关闭'.
  ENDCASE.
ENDFORM.


FORM frm_set_change_icon USING change_flag CHANGING icon text.

  CHECK change_flag IS NOT INITIAL.
  CASE change_flag.
    WHEN 'I'.
      icon = icon_insert_row.
      text = TEXT-011."'新增BOM行'.

    WHEN 'M'.
      icon = icon_led_yellow.
      text = TEXT-012."'变更'.

    WHEN 'D'.
      icon = icon_delete_row.
      text = TEXT-013."'删除BOM行'.

  ENDCASE.
ENDFORM.


FORM frm_update_head_status USING status .

  PERFORM frm_set_status USING status.
  gs_head-aenam = sy-uname.
  gs_head-aedat = sy-datum.
  gs_head-aetim = sy-uzeit.
  IF status = 'D'.
    gs_head-del_flag = 'X'.
  ENDIF.
  CLEAR gs_head_modify.
  MOVE-CORRESPONDING gs_head TO gs_head_modify.

  MODIFY zafo_head FROM gs_head_modify.

  LOOP AT gt_item ASSIGNING <gs_item>.
    PERFORM frm_set_icon  USING gs_head-status CHANGING <gs_item>-icon <gs_item>-text .
  ENDLOOP.

  COMMIT WORK AND WAIT.
ENDFORM.


FORM frm_set_ref_delete.
  DATA:status TYPE char1.
  DATA:item_status TYPE char1.

  CHECK gt_item[] IS NOT INITIAL.

  SELECT bustyp,afono,status,app_status,execute_type INTO TABLE @DATA(lt_head)
    FROM zafo_head
    FOR ALL ENTRIES IN @gt_item
    WHERE afono = @gt_item-afono_ref.
  CHECK sy-subrc EQ 0.

  LOOP AT gt_item WHERE afono_ref IS NOT INITIAL.
    CLEAR: status.
    READ TABLE lt_head INTO DATA(ls_head) WITH  KEY afono = gt_item-afono_ref.
    IF sy-subrc EQ 0.
      IF gs_bustyp_ref-execute_type = 'MB' OR ls_head-execute_type = 'MB'.
        status = 'S'.
      ELSE.
        status = 'C'.
      ENDIF.
      item_status = 'C'.

      IF ls_head-bustyp = 'CJD01'.
        status = ls_head-app_status.
        item_status = ls_head-app_status.
      ENDIF.

      UPDATE zafo_head SET status = status afono_ref = ''
        WHERE afono = gt_item-afono_ref AND status <> status.

      UPDATE zafo_item SET item_status = item_status
                            afono_ref = ''
                            afonr_ref = 0
        WHERE afono = gt_item-afono_ref AND afonr = gt_item-afonr_ref.
    ENDIF.
  ENDLOOP.
ENDFORM.


FORM frm_set_ref_save.
  DATA:status TYPE char1.
  DATA:lv_afono TYPE zafono.

  CHECK gt_item[] IS NOT INITIAL.

  SELECT afono,status,execute_type,bustyp
    INTO TABLE @DATA(lt_head)
    FROM zafo_head
    FOR ALL ENTRIES IN @gt_item
    WHERE afono = @gt_item-afono_ref.

  CHECK sy-subrc EQ 0.

  SORT lt_head BY afono.

  CLEAR:lv_afono.
  LOOP AT gt_item WHERE afono_ref IS NOT INITIAL.
    CLEAR: status.
    READ TABLE lt_head INTO DATA(ls_head) WITH  KEY afono = gt_item-afono_ref.
    IF sy-subrc EQ 0.
      status = 'T'.
      IF lv_afono <> ls_head-afono.
        lv_afono = ls_head-afono.
        PERFORM frm_set_afo_relation USING gt_item-afono gt_item-afono_ref.
        UPDATE zafo_head SET status = status
                             afono_ref = gt_item-afono
          WHERE afono = gt_item-afono_ref AND status <> status.
      ENDIF.
    ENDIF.
    IF gt_item-del_flag = 'X'.
      UPDATE zafo_item SET item_status = ''
                            afono_ref = ''
                            afonr_ref = ''
        WHERE afono = gt_item-afono_ref
          AND afonr = gt_item-afonr_ref.
    ELSE.
      UPDATE zafo_item SET item_status = 'T'
                            afono_ref = gt_item-afono
                            afonr_ref = gt_item-afonr
        WHERE afono = gt_item-afono_ref
          AND afonr = gt_item-afonr_ref.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM frm_set_afo_relation USING afono1 afono2.
  DATA:ls_borident1 TYPE borident.
  DATA:ls_borident2 TYPE borident.

  ls_borident1-objkey = afono1.
  ls_borident2-objkey = afono2.
  ls_borident1-objtype = 'ZAFO'.
  ls_borident2-objtype = 'ZAFO'.

  CALL FUNCTION 'BINARY_RELATION_CREATE'
    EXPORTING
      obj_rolea      = ls_borident1
      obj_roleb      = ls_borident2
      relationtype   = 'VORL'
*     FIRE_EVENTS    = 'X'
*     IMPORTING
*     BINREL         =
*     TABLES
*     BINREL_ATTRIB  =
    EXCEPTIONS
      no_model       = 1
      internal_error = 2
      unknown        = 3
      OTHERS         = 4.
ENDFORM.


FORM frm_del_afo_relation USING afono1 afono2.
  DATA:ls_borident1 TYPE borident.
  DATA:ls_borident2 TYPE borident.

  ls_borident1-objkey = afono1.
  ls_borident2-objkey = afono2.
  ls_borident1-objtype = 'ZAFO'.
  ls_borident2-objtype = 'ZAFO'.

  CALL FUNCTION 'BINARY_RELATION_DELETE'
    EXPORTING
      obj_rolea      = ls_borident1
      obj_roleb      = ls_borident2
      relationtype   = 'VORL'
*     FIRE_EVENTS    = 'X'
    EXCEPTIONS
      no_model       = 1
      internal_error = 2
      unknown        = 3
      OTHERS         = 4.
  .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
