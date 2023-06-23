*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_F01
*&---------------------------------------------------------------------*


FORM frm_set_icon USING status CHANGING icon text..
  CASE status.
    WHEN 'A'.
      icon = icon_led_inactive.
      text = '待检验'.

    WHEN 'B'.
      icon = icon_structure.
      text = '检验中'.

    WHEN 'C'.
      icon = icon_led_green.
      text = '检验完成'.

    WHEN 'E'.
      icon = icon_led_red.
      text = '不合格'.

    WHEN 'D'.
      icon = icon_delete.
      text = '已作废'.

    WHEN 'L'.
      icon = icon_locked.
      text = '已锁定'.

    WHEN 'F'.
      icon = icon_complete.
      text = '采购已确认'.

  ENDCASE.

ENDFORM.


FORM frm_get_qcno CHANGING qcno.

  IF qcno IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZQCNO'
      IMPORTING
        number                  = qcno
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.

    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.
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
  ENDIF.

ENDFORM.
