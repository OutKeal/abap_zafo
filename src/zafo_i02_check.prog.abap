*&---------------------------------------------------------------------*
*& 包含               ZAFO_I02_CHECK
*&---------------------------------------------------------------------*


FORM frm_auth_bustyp_show_status USING u_typ u_werks CHANGING g_readonly.
  DATA : lv_busty_auth TYPE sy-subrc,
         lv_werks_auth TYPE sy-subrc.

  CHECK g_no_authcheck IS INITIAL.

  CLEAR: lv_busty_auth,lv_werks_auth.

  AUTHORITY-CHECK OBJECT 'ZAFO_BUSTY'
               ID 'ZAFO_BUSTY' FIELD u_typ
               ID 'ACTVT' FIELD '01'.
  lv_busty_auth = sy-subrc.

  AUTHORITY-CHECK OBJECT 'ZAFO_WERKS'
             ID 'WERKS' FIELD u_werks
             ID 'ACTVT' FIELD '01'.
  lv_werks_auth = sy-subrc.

  IF lv_werks_auth NE 0  OR lv_busty_auth NE 0.
    g_readonly = 'D'.
    RETURN.
  ENDIF.

  CLEAR: lv_busty_auth,lv_werks_auth.

  AUTHORITY-CHECK OBJECT 'ZAFO_BUSTY'
               ID 'ZAFO_BUSTY' FIELD u_typ
               ID 'ACTVT' FIELD '02'.
  lv_busty_auth = sy-subrc.

  AUTHORITY-CHECK OBJECT 'ZAFO_WERKS'
             ID 'WERKS' FIELD u_werks
             ID 'ACTVT' FIELD '02'.
  lv_werks_auth = sy-subrc.

  IF lv_werks_auth NE 0  OR lv_busty_auth NE 0.
    g_readonly = 'D'.
    RETURN.
  ENDIF.

  CASE u_typ.
    WHEN 'CJD01'.
      g_readonly = 'D'.
    WHEN OTHERS.

      IF gs_head-status = 'A' OR gs_head-status = 'E'.
        g_readonly = 'M'.

      ELSE.
        g_readonly = 'D'.
      ENDIF.

  ENDCASE.

  IF sy-uname = '10015894' OR sy-uname = 'AT-WTH'.
    g_readonly = 'D'.
  ENDIF.

ENDFORM.


FORM frm_auth_bustyp_check USING u_active u_typ u_werks CHANGING error.

  CHECK g_no_authcheck IS INITIAL.
  CLEAR error.
  AUTHORITY-CHECK OBJECT 'ZAFO_BUSTY'
               ID 'ZAFO_BUSTY' FIELD u_typ
               ID 'ACTVT' FIELD u_active.
  IF sy-subrc NE 0 .
    MESSAGE s030 DISPLAY LIKE 'E'."'无业务类型权限'
    error = 'X'.
    RETURN.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZAFO_WERKS'
             ID 'WERKS' FIELD u_werks
             ID 'ACTVT' FIELD u_active.
  IF sy-subrc NE 0 .
    MESSAGE s031 DISPLAY LIKE 'E'."'无工厂权限'
    error = 'X'.
    RETURN.
  ENDIF.

  IF gs_head-bustyp = 'R1011' AND sy-uname = '10005503'." 王翔要通过1020做
    MESSAGE s030 DISPLAY LIKE 'E'."'无业务类型权限'
    error = 'X'.
    RETURN.
  ENDIF.


ENDFORM.


FORM frm_auth_po_check USING u_active u_bsart u_ekgrp CHANGING error.

  CHECK g_no_authcheck IS INITIAL.
  CLEAR error.
  AUTHORITY-CHECK OBJECT 'ZAFO_BSART'
               ID 'BSART' FIELD u_bsart
               ID 'ACTVT' FIELD u_active.
  IF sy-subrc NE 0 .
    MESSAGE s032 DISPLAY LIKE 'E'." '无此采购订单类型的权限'
    error = 'X'.
    RETURN.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZAFO_EKGRP'
             ID 'EKGRP' FIELD u_ekgrp
             ID 'ACTVT' FIELD u_active.
  IF sy-subrc NE 0 .
    MESSAGE s033 DISPLAY LIKE 'E'." '无此采购组的权限'
    error = 'X'.
    RETURN.
  ENDIF.

ENDFORM.


FORM frm_auth_swerks_check TABLES ct_werks STRUCTURE range_werks
                           USING u_active CHANGING error.

  CALL FUNCTION 'ZAFO_AUTH_WERKS'
    EXPORTING
      iv_actvt  = u_active
*     IV_WERKS  =
      iv_object = 'ZAFO_WERKS'
    TABLES
      s_werks   = ct_werks
    EXCEPTIONS
      error     = 1.
  IF sy-subrc NE 0.
    error = 'X'.
    MESSAGE s031 DISPLAY LIKE 'E'."'无工厂权限'
  ENDIF.

ENDFORM.


FORM frm_check_head_data.
  DATA:lt_screen_h TYPE TABLE OF zafo_screen WITH HEADER LINE.

  DATA rule TYPE TABLE OF zcheck_rule WITH HEADER LINE.
  DATA ret TYPE TABLE OF bapiret2 WITH HEADER LINE.
  CLEAR rule[].
  CLEAR ret[].

  CLEAR lt_screen_h[].

  LOOP AT gt_screen WHERE object = g_object AND fieldalv = 'HEAD'.
    IF gt_screen-rollname IS NOT INITIAL OR gt_screen-requi IS NOT INITIAL.
      APPEND gt_screen TO lt_screen_h.
    ENDIF.
  ENDLOOP.

  IF lt_screen_h[] IS NOT INITIAL.

    CLEAR gt_head[].
    APPEND gs_head TO gt_head.
    CLEAR rule[].
    LOOP AT lt_screen_h.
      CLEAR rule.
      rule-fieldname = lt_screen_h-fieldname.
      rule-rollname = lt_screen_h-rollname.
      rule-notnull = lt_screen_h-requi.
      rule-ddtext = lt_screen_h-coltext.
      APPEND rule.
    ENDLOOP.

    CALL FUNCTION 'ZCHECK_TAB_VALUE'
      TABLES
        tab  = gt_head[]
        rule = rule
        ret  = ret.

    LOOP AT ret.
      PERFORM frm_add_msg USING ret-type ret-id ret-number ret-message_v1
                                ret-message_v2 ret-message_v3 ret-message_v4.
    ENDLOOP.
  ENDIF.

  PERFORM frm_zz_check_head.

ENDFORM.


FORM frm_check_item_data.
  DATA:lt_screen_i TYPE TABLE OF zafo_screen WITH HEADER LINE.
  DATA:lt_item TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
  DATA rule TYPE TABLE OF zcheck_rule WITH HEADER LINE.
  DATA ret TYPE TABLE OF bapiret2 WITH HEADER LINE.
  CLEAR rule[].
  CLEAR ret[].

  CLEAR lt_screen_i[].

  LOOP AT gt_screen WHERE object = g_object AND fieldalv = 'ITEM'.
    IF gt_screen-rollname IS NOT INITIAL OR gt_screen-requi IS NOT INITIAL.
      APPEND gt_screen TO lt_screen_i.
    ENDIF.
  ENDLOOP.

  IF lt_screen_i[] IS NOT INITIAL.
    CLEAR rule[].

    LOOP AT lt_screen_i.
      CLEAR rule.
      rule-fieldname = lt_screen_i-fieldname.
      rule-rollname = lt_screen_i-rollname.
      rule-notnull = lt_screen_i-requi.
      rule-ddtext = lt_screen_i-coltext.
      APPEND rule.
    ENDLOOP.

    REFRESH lt_item.

    lt_item[] = gt_item[].

    CALL FUNCTION 'ZCHECK_TAB_VALUE'
      TABLES
        tab  = lt_item[]
        rule = rule
        ret  = ret.

    LOOP AT ret.
      PERFORM frm_add_msg USING ret-type
                                ret-id
                                ret-number
                                ret-message_v1
                                ret-message_v2
                                ret-message_v3
                                ret-message_v4.
    ENDLOOP.
  ENDIF.

  PERFORM frm_zz_check_item.

ENDFORM.


FORM frm_zz_check_head.
  DATA:lv_msg TYPE char100.

  CASE gs_head-object.
    WHEN 'DUC'.
      PERFORM frm_zz_duc_check_head.
    WHEN '1009' OR '1010'.
      PERFORM frm_zz_gr_check_head.
    WHEN 'REC'.
      PERFORM frm_zz_rec_check_head.
  ENDCASE.

  CASE gs_head-execute_type.
    WHEN 'PO' OR 'PRO' OR 'PR'.
      SELECT SINGLE * INTO @DATA(ls_pokz_cgz)
        FROM zconf_pokz_cgz
        WHERE bsart = @gs_head-bsart
        AND ekgrp = @gs_head-ekgrp.
      IF sy-subrc <> 0 .
        PERFORM frm_add_msg USING 'E' 'ZAFO' '127' gs_head-ekgrp gs_head-bsart '' ''."采购组:&1不能用于采购类型:&2!
      ENDIF.
  ENDCASE.

ENDFORM.


FORM frm_zz_check_item.
  DATA:lv_msg TYPE char100.

  CLEAR: lv_msg.
  CASE gs_head-bustyp.
    WHEN 'R1002'.
      SELECT qcno,qc_status INTO TABLE @DATA(lt_qc_head)
        FROM zafo_qc_head
        FOR ALL ENTRIES IN @gt_item
        WHERE qcno = @gt_item-qcno
        AND qc_status = 'D'.
      IF sy-subrc EQ 0 .
        LOOP AT lt_qc_head INTO DATA(ls_qc_head).
          IF lv_msg IS INITIAL.
            lv_msg = ls_qc_head-qcno.
          ELSE.
            lv_msg = lv_msg && ',' && ls_qc_head-qcno.
          ENDIF.
        ENDLOOP.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '126' lv_msg '' '' ''."质检单&1已删除！
      ENDIF.
    WHEN 'PO003'.
      IF gt_item_po_cpt[] IS INITIAL.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '请输入原材料' '' '' ''."请输入原材料
      ENDIF.
    WHEN 'R2001' OR 'R2002'.
      PERFORM frm_check_gd.

  ENDCASE.

  CHECK g_error <> 'X'.

  LOOP AT gt_item.

    IF gt_item-afonr IS INITIAL .
      PERFORM frm_add_msg USING 'E' 'ZAFO' '125' '' '' '' ''."明细行行号不能为空！
    ENDIF.

    IF gt_item-zmm_tran_rate IS NOT INITIAL AND  gt_item-zmm_tran_rate <> 1
      AND gt_item-meins = gt_item-bprme.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '139' '' '' '' ''."采购单位错误！
    ENDIF.

    IF gt_item-afono_ref IS NOT INITIAL AND gs_head-status = ''
      AND gs_head-bustyp NE 'R1002' AND gs_head-bustyp NE 'R4006'.
      IF gs_bustyp-busref = 'R'.
        SELECT SINGLE i~afono_ref INTO @DATA(lref_zafono)
          FROM zafo_item AS i
          INNER JOIN zafo_head AS h  ON i~afono = h~afono
          WHERE i~afono_ref = @gt_item-afono_ref
          AND i~afonr_ref = @gt_item-afonr_ref
          AND h~bustyp = @gs_bustyp-bustyp
          AND h~status <> 'D' AND i~del_flag <> 'X'.
        IF sy-subrc EQ 0 AND lref_zafono IS NOT INITIAL.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '022' '' '' '' ''."选中的数据已被引用,请刷新页面重新选择
        ENDIF.
      ELSE.
        SELECT SINGLE i~afono_ref INTO @lref_zafono
          FROM zafo_item AS i
          INNER JOIN zafo_head AS h  ON i~afono = h~afono
          WHERE i~afono_ref = @gt_item-afono_ref
          AND i~afonr_ref = @gt_item-afonr_ref
          AND h~status <> 'D' AND i~del_flag <> 'X'.
        IF sy-subrc EQ 0 AND lref_zafono IS NOT INITIAL.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '022' '' '' '' ''."选中的数据已被引用,请刷新页面重新选择
        ENDIF.
      ENDIF.

    ENDIF.

    CASE gs_head-execute_type.
      WHEN 'MB'.
        IF gt_item-menge_cg < 0 OR gt_item-menge < 0.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '110' '' '' '' ''."此业务明细数量不能小于0!
        ENDIF.
    ENDCASE.

    CASE gs_head-bustyp.
      WHEN 'R1014'.
        SELECT SINGLE  i~zppdhd INTO @DATA(lv_zppdhd)
          FROM zafo_item AS i
          INNER JOIN zafo_head AS h ON i~afono = h~afono
          WHERE i~zppdhd = @gt_item-zppdhd
          AND h~status <> 'D'
          AND h~bustyp = 'PO003'
          AND i~del_flag <> 'X'.
        IF sy-subrc <> 0 .
*          PERFORM frm_add_msg USING 'E' 'ZAFO' '129' gt_item-zppdhd '' '' ''."大货通知单:&1没有加工合同！
        ENDIF.
      WHEN 'AF01' OR 'AF02' OR 'AF03' OR 'AF06' OR 'AF07'.
        IF gt_item-menge > gt_item-menge4.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '108' '' '' '' ''."申请数量必须小于库存数！
        ENDIF.
      WHEN 'R1001'.
        IF gt_item-menge_cg < 0 OR gt_item-menge < 0.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '110' '' '' '' ''."此业务明细数量不能小于0!
        ENDIF.
      WHEN 'R1004'.
        IF gt_item-menge > gt_item-menge3.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '134' '' '' '' ''."输入数量不能大于未清数！
        ENDIF.

      WHEN 'DUC01'.
        IF gt_item-menge_cg <> 0 OR gt_item-menge <> 0.
          IF gt_item-ebeln IS INITIAL AND gt_item-zzpino IS INITIAL .
            PERFORM frm_add_msg USING 'E' 'ZAFO' '123' '销售合同' '' '' ''."扣款单有数量必须填销售合同！
          ENDIF.

          IF gt_item-matnr IS INITIAL .
            PERFORM frm_add_msg USING 'E' 'ZAFO' '123' '物料编号' '' '' ''."扣款单有数量必须填物料编号！
          ENDIF.

          IF gt_item-ebeln IS INITIAL.
            PERFORM frm_add_msg USING 'E' 'ZAFO' '123' '采购凭证' '' '' ''."扣款单有数量必须填采购凭证！
          ENDIF.
        ENDIF.
      WHEN 'POC01'.
        IF gt_item-zcolor IS NOT INITIAL AND gt_item-zcolor = gt_item-zcolor_text.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '颜色有问题，请联系魏天华' '' '' ''.
        ENDIF.
    ENDCASE.

  ENDLOOP.

  LOOP AT gt_item_po_cpt.
    IF gt_item_po_cpt-charg IS INITIAL.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '130' '' '' '' ''."原材料批次不能为空!
    ENDIF.
  ENDLOOP.

  CASE gs_head-execute_type.
    WHEN 'CC'.
      PERFORM frm_cc_per_post.
  ENDCASE.

ENDFORM.


FORM frm_check_commit CHANGING l_msg.
  CASE gs_head-execute_type.
    WHEN 'PO' OR 'PRO' OR 'POP'.
      IF gs_head-ebeln IS NOT INITIAL OR gs_head-afono IS NOT INITIAL..
        PERFORM frm_read_text.
        READ TABLE gt_line INTO DATA(ls_line) INDEX 1.
        IF strlen( ls_line-line ) < 3.
          g_error = 'X'.
          l_msg = '合同条款为空!'.
          RETURN.
        ENDIF.
      ENDIF.
  ENDCASE.

  LOOP AT gt_item.
    CASE gs_head-execute_type.
      WHEN 'PO' OR 'PRO' OR 'POP'.
        READ TABLE gt_item_po WITH KEY  afonr = gt_item-afonr.
        IF sy-subrc = 0.
          IF gt_item-price_long <= 0 OR gt_item-price <= 0 OR gt_item_po-price <= 0.
            l_msg = '存在单价小于或等于0的明细行,是否提交？'.
          ENDIF.
        ELSE.
          IF gt_item-price_long <= 0 OR gt_item-price <= 0.
            l_msg = '存在单价小于或等于0的明细行,是否提交？'.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  IF gt_item_po[] IS NOT INITIAL.
    DATA:lv_amount TYPE zafo_amount.
    CLEAR lv_amount.

    CASE gs_head-execute_type.
      WHEN  'POC'.
        LOOP AT gt_item_po WHERE loekz <> 'X'.
          lv_amount =  lv_amount + gt_item_po-amount.
        ENDLOOP.

      WHEN OTHERS.
        LOOP AT gt_item_po WHERE loekz <> 'X'.
          lv_amount =  lv_amount + gt_item_po-amount + gt_item_po-cost_amount.
        ENDLOOP.
    ENDCASE.

    IF lv_amount <> gs_head-amount.
      g_error = 'X'.
      l_msg = '明细金额存在异常，请截图此采购订单给魏天华!'.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.


FORM frm_check_choise.
  DATA: l_tabix TYPE sy-tabix.
  DATA: lv_pn TYPE char1 , " 正数标识
        lv_sn TYPE char1 . " 负数标识

  CASE gs_bustyp-busref .
    WHEN 'A'.
      CLEAR:lv_pn,lv_sn.
      LOOP AT gt_item_100 WHERE selected = 'X' .
        READ TABLE gt_screen WITH KEY fieldname = 'MENGE3'.
        IF sy-subrc EQ 0.
          IF gt_item_100-menge3 > 0.
            lv_pn = 'X'.
          ELSEIF gt_item_100-menge3 < 0.
            lv_sn = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_sn = 'X' AND lv_pn = 'X'.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '112' '' '' '' ''."此业务类型，明细行未清数量不能同时存在正负数！
      ENDIF.

    WHEN 'B'.
      LOOP AT gt_item_100 WHERE selected = 'X' .
        READ TABLE gt_screen WITH KEY fieldname = 'MENGE4'.
        IF sy-subrc EQ 0.
          IF gt_item_100-menge > gt_item_100-menge4.
            PERFORM frm_add_msg USING 'E' 'ZAFO' '019' '' '' '' ''."申请数量不应该大于库存数量
          ENDIF.
        ENDIF.
      ENDLOOP.

      READ TABLE gt_item_100 WITH KEY selected = 'X'.
      IF sy-subrc NE 0.
        CHECK gs_object-main_dynnr <> '0800'.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '020' '' '' '' ''."无有效数据
      ENDIF.

    WHEN 'D' OR 'L'.
      DATA:lv_zshd TYPE zshd.
      CLEAR lv_zshd.
      LOOP AT gt_item_100 WHERE selected = 'X' AND zppflag <> 'X'.
        IF lv_zshd IS INITIAL.
          lv_zshd = gt_item_100-zshd.
        ELSE.
          IF lv_zshd <> gt_item_100-zshd.
            PERFORM frm_add_msg USING 'E' 'ZAFO' '132' lv_zshd gt_item_100-zshd '' ''."大货生产地&1和&2不能同时做合同！
            RETURN.
          ENDIF.
        ENDIF.
      ENDLOOP.

    WHEN 'Z' OR 'Y'.

      LOOP AT gt_item_100 WHERE selected = 'X'.
        IF gt_item_100-icon <> icon_led_green AND gt_item_100-icon <> icon_complete.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '011' '' '' '' ''."单据未审核,无法参照
          RETURN.
        ENDIF.
      ENDLOOP.

  ENDCASE.


  IF gs_bustyp-execute_type = 'POC'.

    DATA:lv_ebeln TYPE ebeln.
    CLEAR lv_ebeln.

    LOOP AT gt_item_100 WHERE selected = 'X'.
      IF gt_item_100-ebeln IS INITIAL .
        PERFORM frm_add_msg USING 'E' 'ZAFO' '012' '' '' '' ''."未选择原单合同,请选择
        RETURN.
      ENDIF.

      IF lv_ebeln IS INITIAL .
        lv_ebeln = gt_item_100-ebeln.

        SELECT SINGLE * INTO @DATA(ls_head)
          FROM zafo_head
          WHERE afono = @lv_ebeln
            AND bustyp = 'PO001'.
        IF sy-subrc NE 0.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '013' '' '' '' ''."选择的合同不存在,请重新选择
          RETURN.
        ENDIF.

        IF ls_head-status <> 'B' AND ls_head-status <> 'C'.
          PERFORM frm_add_msg USING 'E' 'ZAFO' '014' '' '' '' ''."选择的合同不为审核中或已审核，请重新选择
          RETURN.
        ENDIF.

      ELSEIF lv_ebeln <> gt_item_100-ebeln.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '138' '' '' '' ''."不能同时变更两个采购订单
        RETURN.
      ENDIF.

      gt_item_100-bsart = ls_head-bsart.
      gt_item_100-mwskz = ls_head-mwskz.
      gt_item_100-bukrs = ls_head-bukrs.
      gt_item_100-ekorg = ls_head-ekorg.
      gt_item_100-ekgrp = ls_head-ekgrp.
      gt_item_100-eeind = ls_head-eeind.
      MODIFY gt_item_100.
    ENDLOOP.
  ENDIF.

  CLEAR l_tabix.
  LOOP AT gt_item_100 WHERE selected = 'X' .
    ADD 1 TO l_tabix.
    PERFORM frm_set_copy_head USING l_tabix gt_item_100.
    IF gt_item_100-icon = icon_led_red.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '021' '' '' '' ''."不能选择红灯状态行
      EXIT.
    ENDIF.

    IF gt_item_100-afono IS NOT INITIAL AND gs_bustyp-bustyp_ref EQ 'R'.

      SELECT SINGLE i~afono_ref INTO @DATA(lref_zafono)
        FROM zafo_item AS i
        INNER JOIN zafo_head AS h ON i~afono = h~afono
        WHERE i~afono_ref = @gt_item_100-afono
        AND i~afonr_ref = @gt_item_100-afonr
        AND h~bustyp = @gs_bustyp-bustyp
        AND h~status <> 'D' AND i~del_flag <> 'X'.
      IF sy-subrc EQ 0 AND lref_zafono IS NOT INITIAL.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '022' '' '' '' ''."选中的数据已被引用,请刷新页面重新选择
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.


FORM frm_check_save.

  PERFORM frm_auth_bustyp_check USING '01' gs_head-bustyp g_werks CHANGING g_error .
  CHECK g_error IS INITIAL.

  PERFORM frm_check_head_data.

  PERFORM frm_check_item_data.
ENDFORM.


FORM frm_check_delete.
  DATA:ls_msg TYPE char40.
  DATA:ls_afono   TYPE zafono,
       lfo_afono  TYPE zafono,
       lfo_bustyp TYPE zafo_bustyp.

  PERFORM frm_auth_bustyp_check USING '02' gs_head-bustyp g_werks CHANGING g_error .
  CHECK g_error <> 'X'.

  CLEAR ls_msg.

  SELECT SINGLE zafo_head~afono,zafo_head~bustyp
    INTO (@lfo_afono,@lfo_bustyp)
    FROM zafo_item
    INNER JOIN zafo_head ON zafo_head~afono = zafo_item~afono
    WHERE zafo_item~afono_ref = @gs_head-afono
    AND zafo_head~status <> 'D' " 不能是删除的
    AND zafo_head~del_flag <> 'X'
    AND zafo_head~status <> 'T' " T 一般是前序单据，避免
    AND zafo_item~item_status <> 'F'
    AND zafo_item~del_flag = ''.
  IF sy-subrc EQ 0.
    SELECT SINGLE  bustyp_name1 INTO ls_msg
       FROM zafo_bustype
      WHERE bustyp = lfo_bustyp.
    ls_msg = '存在后续单据：' && ls_msg && '(' && lfo_afono && ')'.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '000' ls_msg '' '' ''."只有已保存状态A才可以作废
  ENDIF.

  IF gs_head-bustyp = 'DUC01'.
    SELECT SINGLE i~*  INTO @DATA(ls_ztmm013)
      FROM ztmm013_cost AS i
      LEFT JOIN ztmm013_head AS h ON i~lelnr = h~lelnr
      WHERE h~zstat <> 'D'
      AND afono = @gs_head-afono.
    IF sy-subrc EQ 0 .
      ls_msg = '扣款单已对账'&& ls_msg && '(' && ls_ztmm013-lelnr && '),不能删除'.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '000' ls_msg '' '' ''.
    ENDIF.
  ENDIF.

  IF gs_head-status <> 'A' AND gs_head-status <> 'E'.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '028' '' '' '' ''."只有已保存状态A才可以作废
  ENDIF.

ENDFORM.


FORM frm_check_post.
  DATA: ls_msg TYPE char100.

  CLEAR ls_msg.

  IF g_readonly <> 'D'.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '066' '' '' '' ''."只有显示状态才可以过账
  ENDIF.

  IF gs_head-status <> 'A'.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '067' '' '' '' ''."只有保存状态可以过账
  ENDIF.

  CASE gs_bustyp-bustyp.
    WHEN 'H4007'.
      IF gs_head-werks = gs_head-umwrk.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '068' '' '' '' ''."收发工厂不能一样
      ENDIF.
    WHEN 'R2001'.
      PERFORM frm_check_gd.

    WHEN 'ASN02' OR 'R1019' OR 'R1020'.

      IF gs_head-remark3 IS INITIAL .
        ls_msg = '出口装箱单需要提供货柜编号，请联系单证！'.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '000' ls_msg '' '' '.'.
      ELSE.

      ENDIF.

    WHEN 'R1002' OR 'R1003' OR 'R1004' OR 'R1006'.
      SELECT SINGLE i~*  INTO @DATA(ls_ztmm013)
        FROM ztmm013_item AS i
        LEFT JOIN ztmm013_head AS h ON i~lelnr = h~lelnr
        WHERE h~zstat <> 'D' AND afono = @gs_head-afono.
      IF sy-subrc EQ 0.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 105 ls_ztmm013-lelnr '' '' ''."已经创建对账单,无法冲销
      ENDIF.
  ENDCASE.

ENDFORM.


FORM frm_post_after_check.

  CHECK g_error IS INITIAL.

  IF gs_head-execute_type = 'PO' OR gs_head-execute_type = 'PRO'.

    DATA:lv_brtwr TYPE bbwert.
    SELECT SUM( brtwr ) INTO lv_brtwr
      FROM ekpo
      WHERE ebeln = gs_head-ebeln
        AND loekz = ''.
    IF sy-subrc <> 0 OR lv_brtwr <> gs_head-amount.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 140 '' '' '' ''."采购标准PO价格异常,请联系信息部！
    ENDIF.

  ENDIF.

  PERFORM frm_pop_msg.
  CLEAR g_error.

ENDFORM.



FORM frm_check_ok_510.

  DATA:ls_menge TYPE menge_d.
  DATA:ls_zvat_nub TYPE zvat_nub.
  CLEAR g_error.
  CLEAR ls_menge.
  CLEAR ls_zvat_nub.
  SORT gt_item_batch_dis BY zvat_nub.

  DATA(l_lines)  = lines( gt_item_batch_dis[] ).
  IF l_lines = 0.
    LEAVE TO SCREEN 0.
  ENDIF.

  IF l_lines = 1.
    READ TABLE gt_item_batch_dis INDEX 1.
    IF gt_item_batch_dis-zvat_nub IS INITIAL OR gt_item_batch_dis-menge IS INITIAL.
      g_error = 'X'.
      MESSAGE s035.
*      MESSAGE '缸号与数量不能位空' TYPE 'S'.
      EXIT.
    ENDIF.

    IF gt_item_batch_dis-menge <> <gs_item>-menge.
      MESSAGE s036.
*      MESSAGE '合计数量不等于总数量' TYPE 'S'.
      RETURN.
    ENDIF.

    <gs_item>-zvat_nub = gt_item_batch_dis-zvat_nub.


    LEAVE TO SCREEN 0.
  ENDIF.

  LOOP AT gt_item_batch_dis.
    IF gt_item_batch_dis-zvat_nub IS INITIAL OR gt_item_batch_dis-menge IS INITIAL.
      g_error = 'X'.
      MESSAGE s035.
*      MESSAGE '缸号与数量不能位空' TYPE 'S'.
      EXIT.
    ENDIF.
    IF ls_zvat_nub = gt_item_batch_dis-zvat_nub.
      MESSAGE s037.
*      MESSAGE '缸号重复，请检查数据' TYPE 'S'.
    ENDIF.
    ls_zvat_nub = gt_item_batch_dis-zvat_nub.

    ls_menge = ls_menge + gt_item_batch_dis-menge.
  ENDLOOP.

  CHECK g_error IS INITIAL.


  IF ls_menge <> <gs_item>-menge.
    MESSAGE s036.
*      MESSAGE '合计数量不等于总数量' TYPE 'S'.
    RETURN.
  ENDIF.

  DELETE gt_item_batch WHERE afono = <gs_item>-afono AND afonr = <gs_item>-afonr.

  LOOP AT gt_item_batch_dis.
    gt_item_batch_dis-afono = <gs_item>-afono.
    gt_item_batch_dis-afonr = <gs_item>-afonr.
    gt_item_batch_dis-line_id = sy-tabix.
    gt_item_batch_dis-meins = <gs_item>-meins.
    APPEND gt_item_batch_dis TO gt_item_batch.
    CLEAR gt_item_batch_dis.
  ENDLOOP.

  <gs_item>-zvat_nub = TEXT-016." '多重分配'.

  LEAVE TO SCREEN 0.
ENDFORM.


FORM frm_check_gd.
  DATA: ls_msg TYPE char100.

  CLEAR ls_msg.

  SELECT zppdhd,zyxfl,zdhdzt,zzscgdh,zdhdsl
    INTO TABLE @DATA(lt_ztpp0089)
    FROM ztpp0089
    FOR ALL ENTRIES IN @gt_item
    WHERE zppdhd = @gt_item-zppdhd.
  IF sy-subrc = 0.
    SORT lt_ztpp0089 BY zppdhd.

    CLEAR ls_msg.
    LOOP AT lt_ztpp0089 INTO DATA(ls_ztpp0089) WHERE zdhdzt = 'D' OR zdhdsl <= 0..
      IF ls_msg IS INITIAL.
        ls_msg = ls_ztpp0089-zppdhd.
      ELSE.
        ls_msg = ls_msg && ',' && ls_ztpp0089-zppdhd.
      ENDIF.
    ENDLOOP.
    IF sy-subrc EQ 0 .
      PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '大货通知单:' ls_msg '已删除，请转移物料订单' '.'.
      DELETE lt_ztpp0089 WHERE zdhdzt = 'D'.
    ENDIF.

    CLEAR ls_msg.
    LOOP AT lt_ztpp0089 INTO ls_ztpp0089 WHERE zzscgdh IS INITIAL.
      IF ls_msg IS INITIAL.
        ls_msg = ls_ztpp0089-zppdhd.
      ELSE.
        ls_msg = ls_msg && ',' && ls_ztpp0089-zppdhd.
      ENDIF.
    ENDLOOP.
    IF sy-subrc EQ 0 .
      PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '大货通知单:' ls_msg '工单为空' '.'.
      DELETE lt_ztpp0089 WHERE zzscgdh IS INITIAL.
    ENDIF.

    DATA:lv_objnr LIKE  jest-objnr.
    DATA:lv_line LIKE  bsvx-sttxt.
    DATA:ls_msg1 TYPE char100.
    DATA:ls_msg2 TYPE char100.

    CLEAR: ls_msg1,ls_msg2,lv_objnr,lv_line.

    LOOP AT lt_ztpp0089 INTO ls_ztpp0089.

      CONCATENATE 'OR' ls_ztpp0089-zzscgdh INTO lv_objnr.
      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          objnr            = lv_objnr
          spras            = sy-langu
          bypass_buffer    = '1'
        IMPORTING
          line             = lv_line
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

      FIND 'CRTD' IN lv_line.
      IF sy-subrc EQ 0 .
        IF ls_msg1 IS INITIAL.
          ls_msg1 = ls_ztpp0089-zppdhd.
        ELSE.
          ls_msg1 = ls_msg1 && ',' && ls_ztpp0089-zppdhd.
        ENDIF.

      ENDIF.
      FIND 'TECO' IN lv_line.
      IF sy-subrc EQ 0 .
        IF ls_msg2 IS INITIAL.
          ls_msg2 = ls_ztpp0089-zppdhd.
        ELSE.
          ls_msg2 = ls_msg2 && ',' && ls_ztpp0089-zppdhd.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ls_msg1 IS NOT INITIAL.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '大货通知单:' ls_msg1 '未下达，请联系生产' '.'.
    ENDIF.

    IF ls_msg2 IS NOT INITIAL.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '大货通知单:' ls_msg2 '已关闭，请联系生产' '.'.
    ENDIF.
  ENDIF.

ENDFORM.
