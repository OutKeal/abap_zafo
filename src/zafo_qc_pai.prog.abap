*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0103 INPUT.
  CLEAR g_ucomm.
  CASE sy-ucomm.
    WHEN '&OK'.

      IF g_menge_qc IS INITIAL AND g_menge_qc_f IS INITIAL.
        MESSAGE '请输入合格数或不合格数' TYPE 'I'.
        RETURN.
      ENDIF.
      g_ucomm = '&OK'.
      LEAVE TO SCREEN 0.
    WHEN '&CANCEL'.
      g_ucomm = '&CANCEL'.
      CLEAR g_menge_qc_f1.
      CLEAR g_menge_qc.
      CLEAR g_menge_qc_f.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.


MODULE user_command_0101 INPUT.
  CLEAR g_ucomm.

  CASE sy-ucomm.
    WHEN '&OK'.
      IF gs_head-qc_count IS INITIAL.
        MESSAGE '请输入质检份数' TYPE 'I'.
        RETURN.
      ENDIF.

      IF gs_head-qcmodel IS INITIAL.
        MESSAGE '请选择质检模板' TYPE 'I'.
        RETURN.
      ENDIF.
      g_ucomm = '&OK'.
      LEAVE TO SCREEN 0.

    WHEN '&CANCEL'.
      g_ucomm = '&CANCEL'.
      gs_head-qc_count = 1.
      CLEAR gs_head-qcmodel.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.



FORM frm_user_command_100 USING ok_code.
  DATA:lv_msg TYPE char40.
  DATA:lv_lines TYPE i.
  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
  DATA:lt_head TYPE TABLE OF zafo_qc_shead WITH HEADER LINE." 质检数据单
  DATA: lv_qcno LIKE gs_head-qcno.

  CLEAR g_error.
  REFRESH lt_head.

  CASE ok_code.
    WHEN '&DETAIL'.
      CLEAR ok_code.

      CALL METHOD g_grid_100_head->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      IF lt_index_rows[] IS INITIAL.
        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR gt_item[].

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_head ASSIGNING <gs_head> INDEX ls_index_rows-index.
        IF sy-subrc EQ 0.
          IF <gs_head>-qcno IS NOT INITIAL.
            SELECT * FROM zafo_qc_item
              APPENDING CORRESPONDING FIELDS OF TABLE gt_item
              WHERE qcno = <gs_head>-qcno.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT gt_item ASSIGNING <gs_item>.
        PERFORM frm_set_icon USING <gs_item>-qc_status CHANGING <gs_item>-icon <gs_item>-text.
      ENDLOOP.
      IF sy-subrc NE 0.
        MESSAGE '无已生成的明细行' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      PERFORM f_refresh_grid_alv USING g_grid_100_item.

    WHEN '&CREATE'.

      CLEAR ok_code.
      CLEAR gs_head.

      CALL METHOD g_grid_100_head->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      DATA(lv_line) = lines( lt_index_rows ).
      IF lv_line < 1.
        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      DATA:new_key TYPE char200.
      DATA:last_key TYPE char200.
      CLEAR new_key.
      CLEAR last_key.
      LOOP AT lt_index_rows INTO ls_index_rows .

        READ TABLE gt_head ASSIGNING <gs_head> INDEX ls_index_rows-index.
        IF <gs_head>-qc_status <> 'A' AND <gs_head>-qcno IS NOT INITIAL.
          MESSAGE '不是待检验状态，无法创建' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        new_key = <gs_head>-lifnr && <gs_head>-maktx.
        IF last_key IS INITIAL.
          last_key = new_key.
        ELSE.
          IF new_key <> last_key.
            MESSAGE '关键字不同，无法合并检验' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDIF.

        gs_head-afono = <gs_head>-afono.
      ENDLOOP.

      gs_head-qc_count = 1.

      CALL SCREEN 101 STARTING AT 50 5 ENDING AT 100 10.

      CHECK g_ucomm = '&OK'.

      PERFORM frm_get_qcno CHANGING gs_head-qcno.

      DATA:ls_qcnr TYPE zafonr.
      CLEAR ls_qcnr.

      CLEAR gs_head-menge_gr.
      CLEAR gs_head-menge_qc.

      REFRESH gt_head_modify.

      LOOP AT lt_index_rows INTO ls_index_rows .

        READ TABLE gt_head ASSIGNING <gs_head> INDEX ls_index_rows-index.

        ADD 1 TO ls_qcnr.

        <gs_head>-qcno = gs_head-qcno.
        <gs_head>-qcnr = ls_qcnr.
        <gs_head>-qc_status = 'B'.

        PERFORM frm_set_icon USING <gs_head>-qc_status CHANGING <gs_head>-icon <gs_head>-text.

        <gs_head>-qc_count = gs_head-qc_count.
        <gs_head>-qcmodel = gs_head-qcmodel.
        <gs_head>-erdat = sy-datum.
        <gs_head>-erzet = sy-uzeit.
        <gs_head>-ernam = sy-uname.

        <gs_head>-monam = sy-uname.
        <gs_head>-modat = sy-datum.
        <gs_head>-mozet = sy-uzeit.

        MOVE-CORRESPONDING <gs_head> TO gt_head_modify.

        APPEND gt_head_modify.
        CLEAR gt_head_modify.

        gs_head-menge_gr = gs_head-menge_gr + <gs_head>-menge_gr.
        gs_head-menge_qc = gs_head-menge_qc + <gs_head>-menge_qc.

      ENDLOOP.


      SELECT SINGLE * INTO @DATA(ls_0010)
        FROM zmmt0010
        WHERE matnr = @<gs_head>-matnr.

      CLEAR gt_item[].

      DO <gs_head>-qc_count TIMES.

        MOVE-CORRESPONDING <gs_head> TO gt_item.
        CALL FUNCTION 'CONVERSION_EXIT_ZMENG_OUTPUT'
          EXPORTING
            input  = ls_0010-zmm_mf1
          IMPORTING
            output = gt_item-width.
        gt_item-width = gt_item-width && ls_0010-zmm_mf_gc.

        gt_item-weight = ls_0010-zmm_kz.
        gt_item-datum_qc = sy-datum.
        gt_item-zzpino = <gs_head>-zzpino.
        gt_item-zppdhd = <gs_head>-zppdhd.

        gt_item-menge_gr = gs_head-menge_gr.
        CASE gv_qcmode.
          WHEN 'A'  OR 'B'.
            gt_item-menge_ds = gt_item-menge_gr.
          WHEN 'C'.
            CLEAR gt_item-menge_ds.
        ENDCASE.

        gt_item-menge_qc = gs_head-menge_qc.
        gt_item-qcnr = sy-index.
        gt_item-qc_status = 'A'.

        APPEND gt_item.


        MOVE-CORRESPONDING gt_item TO gt_item_modify .
        APPEND gt_item_modify.
        CLEAR gt_item_modify.
        CLEAR gt_item.
      ENDDO.

      PERFORM frm_save_afo_db USING 'B'.
      IF g_error = 'X'.
        ROLLBACK WORK.
        MESSAGE '质检报告创建失败' TYPE 'E' .
      ELSE.
        PERFORM frm_save_db.
        MESSAGE '质检报告创建成功' TYPE 'S' .
      ENDIF.

    WHEN '&DELETE'.
      CLEAR ok_code.

      CALL METHOD g_grid_100_head->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      lv_line = lines( lt_index_rows ).
      IF lv_line <> 1.
        MESSAGE '请选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      READ TABLE lt_index_rows INTO ls_index_rows INDEX 1.

      READ TABLE gt_head INTO gs_head  INDEX ls_index_rows-index.
      IF sy-subrc EQ 0 .
        PERFORM frm_del_check.
      ELSE.
        g_error = 'X'.
      ENDIF.

      CHECK g_error IS INITIAL.

      gs_head-qc_status = 'D'.

      LOOP AT gt_item ASSIGNING <gs_item> WHERE qcno = gs_head-qcno.
        <gs_item>-qc_status = 'D'.
        MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
        APPEND gt_item_modify.
        CLEAR gt_item_modify.
      ENDLOOP.
      IF sy-subrc NE 0.
        SELECT * INTO TABLE gt_item_modify
          FROM zafo_qc_item
          WHERE qcno = gs_head-qcno.
        IF sy-subrc EQ 0.
          LOOP AT gt_item_modify.
            gt_item_modify-qc_status = 'D'.
            MODIFY gt_item_modify.
          ENDLOOP.
        ENDIF.
      ENDIF.

      REFRESH gt_head_modify.

      LOOP AT gt_head ASSIGNING <gs_head> WHERE qcno = gs_head-qcno.

        PERFORM frm_set_icon USING gs_head-qc_status CHANGING <gs_head>-icon <gs_head>-text.

        <gs_head>-qc_status = gs_head-qc_status.
        <gs_head>-monam = sy-uname.
        <gs_head>-modat = sy-datum.
        <gs_head>-mozet = sy-uzeit.

        MOVE-CORRESPONDING <gs_head> TO gt_head_modify.

        APPEND gt_head_modify.
        CLEAR gt_head_modify.

      ENDLOOP.


      PERFORM frm_del_afo_db USING 'A'.
      IF g_error = 'X'.
        ROLLBACK WORK.
        MESSAGE '质检报告删除失败' TYPE 'E' .
      ELSE.
        PERFORM frm_save_db.
        MESSAGE '质检报告删除成功' TYPE 'S' .
      ENDIF.


    WHEN '&OKAY'."质检完成确认
      CLEAR ok_code.

      CALL METHOD g_grid_100_head->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      lv_line = lines( lt_index_rows ).
      IF lv_line < 1.
        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING '确认检验完成?' .

      CHECK g_error IS INITIAL.

      REFRESH gt_head_modify.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_head ASSIGNING <gs_head> INDEX ls_index_rows-index.
        IF sy-subrc EQ 0.
          CHECK <gs_head>-qc_status = 'B' OR <gs_head>-qc_status  = 'C'.
          <gs_head>-qc_status = 'C'.
          <gs_head>-monam = sy-uname.
          <gs_head>-modat = sy-datum.
          <gs_head>-mozet = sy-uzeit.
          PERFORM frm_set_icon USING <gs_head>-qc_status CHANGING <gs_head>-icon <gs_head>-text.

          MOVE-CORRESPONDING <gs_head> TO gt_head_modify.
          APPEND gt_head_modify.
        ENDIF.
      ENDLOOP.

      PERFORM frm_save_afo_db USING 'C'.
      IF g_error = 'X'.
        MESSAGE '质检报告确认失败' TYPE 'E' .
      ELSE.
        PERFORM frm_save_db.
        MESSAGE '质检报告确认成功' TYPE 'S' .
      ENDIF.


    WHEN '&SURE'.

      CLEAR ok_code.

      CALL METHOD g_grid_100_head->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      lv_line = lines( lt_index_rows ).
      IF lv_line <> 1.
        MESSAGE '请选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR gs_head.
      READ TABLE lt_index_rows INTO ls_index_rows INDEX 1 .
      READ TABLE gt_head INTO gs_head INDEX ls_index_rows-index.
      IF sy-subrc EQ 0.
        IF gs_head-qc_status <> 'B' AND gs_head-qc_status  <> 'C'.
          MESSAGE '当前状态不允许修改合格数量.' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        REFRESH gt_head_modify.

        PERFORM frm_pop_sure.

        CHECK g_ucomm = '&OK'.

        MOVE-CORRESPONDING gs_head TO gt_head_modify.

        APPEND gt_head_modify.

        REFRESH gt_head_sure.
        gt_head_sure[] = gt_head[].
        MODIFY gt_head_sure FROM gs_head INDEX ls_index_rows-index.

        PERFORM frm_save_afo_db USING 'C'.
        IF g_error = 'X'.
          ROLLBACK WORK.
          MESSAGE '质检报告确认失败' TYPE 'E' .
        ELSE.
          MODIFY gt_head FROM gs_head INDEX ls_index_rows-index.
          PERFORM frm_save_db.
          MESSAGE '质检报告确认成功' TYPE 'S' .
        ENDIF.

      ENDIF.

    WHEN '&ALLSURE'.

      CLEAR ok_code.

      CALL METHOD g_grid_100_head->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      lv_line = lines( lt_index_rows ).
      IF lv_line < 1.
        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR lv_qcno.
      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_head INTO gs_head INDEX ls_index_rows-index.
        IF sy-subrc EQ 0 .
          IF lv_qcno IS INITIAL.
            lv_qcno = gs_head-qcno.
          ELSE.
            IF lv_qcno <> gs_head-qcno.
              g_error = 'X'.
              MESSAGE '一次只能操作同一质检单' TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CHECK g_error IS INITIAL.

      PERFORM frm_pop_confirm USING '确认全部未检数量合格?' .

      CHECK g_error IS INITIAL.

      REFRESH gt_head_modify.

      LOOP AT lt_index_rows INTO ls_index_rows.
        CLEAR lt_head.
        CLEAR gt_head_modify.
        READ TABLE gt_head INTO lt_head INDEX ls_index_rows-index.
        IF sy-subrc EQ 0.
          CHECK lt_head-qc_status = 'B' OR lt_head-qc_status  = 'C'.
          lt_head-menge_qc = lt_head-menge_gr - lt_head-menge_qc_f.
          lt_head-qc_status = 'C'.
          lt_head-monam = sy-uname.
          lt_head-modat = sy-datum.
          lt_head-mozet = sy-uzeit.
          PERFORM frm_set_icon USING lt_head-qc_status CHANGING lt_head-icon lt_head-text.

          MOVE-CORRESPONDING lt_head TO gt_head_modify.
          APPEND gt_head_modify.
          APPEND lt_head.
        ENDIF.
      ENDLOOP.

      REFRESH gt_head_sure.
      gt_head_sure[] = gt_head[].
      LOOP AT gt_head_sure.
        READ TABLE lt_head INTO gs_head WITH KEY qcno = gt_head_sure-qcno qcnr = gt_head_sure-qcnr.
        IF sy-subrc EQ 0 .
          MODIFY gt_head_sure FROM gs_head.
        ENDIF.
      ENDLOOP.
      PERFORM frm_save_afo_db USING 'C'.
      IF g_error = 'X'.
        MESSAGE '质检报告确认失败' TYPE 'E' .
      ELSE.
        LOOP AT gt_head.
          READ TABLE lt_head INTO gs_head WITH KEY qcno = gt_head-qcno qcnr = gt_head-qcnr.
          IF sy-subrc EQ 0 .
            MODIFY gt_head FROM gs_head.
          ENDIF.
        ENDLOOP.

        PERFORM frm_save_db.
        MESSAGE '质检报告确认成功' TYPE 'S' .
      ENDIF.


    WHEN '&ALLUNSURE'.

      CLEAR ok_code.

      CALL METHOD g_grid_100_head->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      lv_line = lines( lt_index_rows ).
      IF lv_line < 1.
        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.


      CLEAR lv_qcno.
      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_head INTO gs_head INDEX ls_index_rows-index.
        IF sy-subrc EQ 0 .
          IF lv_qcno IS INITIAL.
            lv_qcno = gs_head-qcno.
          ELSE.
            IF lv_qcno <> gs_head-qcno.
              g_error = 'X'.
              MESSAGE '一次只能操作同一质检单' TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
          ENDIF.

          PERFORM frm_unsure_check.
          CHECK g_error IS INITIAL.
        ENDIF.
      ENDLOOP.

      CHECK g_error IS INITIAL.

      PERFORM frm_pop_confirm USING '确认全部未检数量不合格?' .

      CHECK g_error IS INITIAL.

      REFRESH gt_head_modify.

      LOOP AT lt_index_rows INTO ls_index_rows.
        CLEAR lt_head.
        CLEAR gt_head_modify.
        READ TABLE gt_head INTO lt_head INDEX ls_index_rows-index.
        IF sy-subrc EQ 0.
          CHECK lt_head-qc_status = 'B' OR lt_head-qc_status  = 'C'.
          lt_head-menge_qc_f = lt_head-menge_gr - lt_head-menge_qc .
          lt_head-qc_status = 'C'.
          lt_head-monam = sy-uname.
          lt_head-modat = sy-datum.
          lt_head-mozet = sy-uzeit.

          PERFORM frm_set_icon USING lt_head-qc_status CHANGING lt_head-icon lt_head-text.

          MOVE-CORRESPONDING lt_head TO gt_head_modify.
          APPEND gt_head_modify.
          APPEND lt_head.
        ENDIF.
      ENDLOOP.


      REFRESH gt_head_sure.
      gt_head_sure[] = gt_head[].
      LOOP AT gt_head_sure.
        READ TABLE lt_head INTO gs_head WITH KEY qcno = gt_head_sure-qcno qcnr = gt_head_sure-qcnr.
        IF sy-subrc EQ 0 .
          MODIFY gt_head_sure FROM gs_head.
        ENDIF.
      ENDLOOP.

      PERFORM frm_save_afo_db USING 'C'.
      IF g_error = 'X'.
        ROLLBACK WORK.
        MESSAGE '质检报告确认失败' TYPE 'E' .
      ELSE.
        LOOP AT gt_head.
          READ TABLE lt_head INTO gs_head WITH KEY qcno = gt_head-qcno qcnr = gt_head-qcnr.
          IF sy-subrc EQ 0 .
            MODIFY gt_head FROM gs_head.
          ENDIF.
        ENDLOOP.

        PERFORM frm_save_db.
        MESSAGE '质检报告确认成功' TYPE 'S' .
      ENDIF.
  ENDCASE.

ENDFORM.



FORM f_user_command_100_item USING ok_code.

  DATA:lt_index_rows TYPE  lvc_t_row,
       ls_index_rows TYPE  lvc_s_row,
       lt_row_no     TYPE  lvc_t_roid.
  CASE ok_code.

    WHEN '&MODIFY'.

      CLEAR ok_code.

      CALL METHOD g_grid_100_item->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      DATA(lv_line) = lines( lt_index_rows ).
      IF lv_line <> 1.
        MESSAGE '请选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      READ TABLE lt_index_rows INTO ls_index_rows INDEX 1.

      READ TABLE gt_item ASSIGNING <gs_item> INDEX ls_index_rows-index.

*      IF <gs_head>-qc_status <> 'A' .
*        MESSAGE '不是待检验状态，无法创建' TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.

      CALL SCREEN 300 .

      CHECK g_ucomm = '&OK'.

*      PERFORM frm_get_qcno CHANGING <gs_head>-qcno.
*      <gs_head>-qc_status = 'B'.
*
*      PERFORM frm_set_icon
*        USING <gs_head>-qc_status CHANGING <gs_head>-icon <gs_head>-text.


      <gs_item>-monam = sy-uname.
      <gs_item>-modat = sy-datum.
      <gs_item>-mozet = sy-uzeit.

*      MOVE-CORRESPONDING <gs_head> TO gt_head_modify.
*
*      APPEND gt_head_modify.
*      CLEAR gt_head_modify.

      PERFORM frm_save_db.

      MESSAGE '质检报告维护成功' TYPE 'S' .

      PERFORM f_refresh_grid_alv USING g_grid_100_item.

    WHEN '&CREATE'.

      CHECK gv_qcmode = 'C'.

      READ TABLE gt_item INDEX 1.
      IF sy-subrc <> 0.
        MESSAGE '无可参考的质检报告，请选择质检报告！' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      DATA:ls_qcnr TYPE zafonr.
      CLEAR ls_qcnr.

      LOOP AT gt_head ASSIGNING <gs_head> WHERE qcno = gt_item-qcno.
        IF ls_qcnr IS INITIAL.
          ls_qcnr = <gs_head>-qc_count + 1.
        ENDIF.

        <gs_head>-qc_count = ls_qcnr.

        MOVE-CORRESPONDING <gs_head> TO gt_head_modify.
        APPEND gt_head_modify.
        CLEAR gt_head_modify.
      ENDLOOP.

      gt_item-qcnr = ls_qcnr.
      gt_item-qc_status = 'A'.

      gt_item-datum_qc = sy-datum.
      gt_item-erdat = sy-datum.
      gt_item-erzet = sy-uzeit.
      gt_item-ernam = sy-uname.

      gt_item-monam = sy-uname.
      gt_item-modat = sy-datum.
      gt_item-mozet = sy-uzeit.
      gt_item-zremark = ''.

      APPEND gt_item.

      MOVE-CORRESPONDING gt_item TO gt_item_modify .
      APPEND gt_item_modify.
      CLEAR gt_item_modify.
      CLEAR gt_item.

      PERFORM frm_save_db.
      PERFORM f_refresh_grid_alv USING g_grid_100_item.

    WHEN '&APPR'.

      CLEAR ok_code.

      CALL METHOD g_grid_100_item->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

      lv_line = lines( lt_index_rows ).
      IF lv_line = 0.
        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR g_error.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_item ASSIGNING <gs_item> INDEX ls_index_rows-index.
        IF  <gs_item>-qc_status = 'B'
        OR <gs_item>-qc_status = 'C' OR <gs_item>-qc_status = 'E'.
        ELSE.
          MESSAGE s000(afo) WITH  '当前状态无法调整质检意见'.
          g_error = 'E'.
        ENDIF.
      ENDLOOP.

      CHECK g_error IS INITIAL  .


      PERFORM frm_release.

      gs_item = <gs_item>.
      IF <gs_item>-qc_status = 'C' OR <gs_item>-qc_status = 'E' OR <gs_item>-qc_status = 'B'.
      ELSE.
        MESSAGE s000(afo) WITH  '不支持调整为此状态'.
        RETURN.
      ENDIF.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_item ASSIGNING <gs_item> INDEX ls_index_rows-index.

        <gs_item>-qc_status = gs_item-qc_status.
        <gs_item>-qc_result = gs_item-qc_result.
        <gs_item>-kms = gs_item-kms.
        <gs_item>-approver = gs_item-approver.
        <gs_item>-app_date = gs_item-app_date.

        PERFORM frm_set_icon USING <gs_item>-qc_status CHANGING <gs_item>-icon <gs_item>-text.


        MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
        APPEND gt_item_modify.
        CLEAR gt_item_modify.

      ENDLOOP.
      PERFORM frm_save_db.

      PERFORM f_refresh_grid_alv USING g_grid_100_item.

    WHEN '&ALLOW'.

      CLEAR ok_code.

      CALL METHOD g_grid_100_item->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.
      lv_line = lines( lt_index_rows ).
      IF lv_line = 0.
        MESSAGE '请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR g_error.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_item ASSIGNING <gs_item> INDEX ls_index_rows-index.
        IF <gs_item>-qc_status = 'C' OR <gs_item>-qc_status = 'E'.
        ELSE.
          MESSAGE s000(afo) WITH  '只有质检盘点合格/不合格后才可以录入采购意见'.
          g_error = 'E'.
          RETURN.
        ENDIF.
      ENDLOOP.

      CHECK g_error IS INITIAL  .

      DATA:lv_status TYPE char1.
      IF <gs_item>-qc_status = 'C'.
        lv_status = 'F'.
      ELSE.
        <gs_item>-qc_status = 'E'.
        lv_status = ''.
      ENDIF.

      PERFORM frm_allow USING lv_status.

      gs_item = <gs_item>.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_item ASSIGNING <gs_item> INDEX ls_index_rows-index.

        <gs_item>-qc_status = gs_item-qc_status.
        <gs_item>-pu_result = gs_item-pu_result.
        <gs_item>-ipu = gs_item-ipu.
        <gs_item>-pu_date = gs_item-pu_date.

        PERFORM frm_set_icon USING <gs_item>-qc_status CHANGING <gs_item>-icon <gs_item>-text.
        MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
        APPEND gt_item_modify.
        CLEAR gt_item_modify.

      ENDLOOP.

      PERFORM frm_save_db.

      PERFORM f_refresh_grid_alv USING g_grid_100_item.

  ENDCASE.

ENDFORM.
