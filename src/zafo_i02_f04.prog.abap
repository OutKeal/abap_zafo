*&---------------------------------------------------------------------*
*& 包含               ZAFO_I02_F04
*&---------------------------------------------------------------------*

FORM frm_zafo_preprocess." 前置数据处理
  DATA:lv_lifnr      TYPE zafo_sitem-lifnr,
       lv_lifnr_name TYPE zafo_sitem-lifnr_name.

  CASE gs_bustyp-busref.
    WHEN 'D'.
      LOOP AT gt_item_100 WHERE selected = 'X'.
        IF gt_item_100-lifnr IS NOT INITIAL.
          lv_lifnr = gt_item_100-lifnr.
          lv_lifnr_name = gt_item_100-lifnr_name.
        ENDIF.
      ENDLOOP.

      LOOP AT gt_item_100 WHERE selected = 'X'.
        IF gt_item_100-lifnr IS INITIAL.
          gt_item_100-lifnr = lv_lifnr.
          gt_item_100-lifnr_name = lv_lifnr_name.
          MODIFY gt_item_100.
        ENDIF.
      ENDLOOP.

      IF gt_item_po_100[] IS NOT INITIAL.
        LOOP AT gt_item_po_100.
          READ TABLE gt_item_100 WITH KEY icon  = gt_item_po_100-icon
                                          werks  = gt_item_po_100-werks
                                          afono = gt_item_po_100-afono
                                          zzpino = gt_item_po_100-zzpino
                                          idnlf = gt_item_po_100-idnlf
                                          matnr = gt_item_po_100-matnr
                                          zcolor = gt_item_po_100-zcolor
                                          zsize = gt_item_po_100-zsize
                                          znorms = gt_item_po_100-znorms
                                          zppflag = gt_item_po_100-zppflag.
          IF sy-subrc EQ 0.
            IF gt_item_100-selected = 'X'.
              gt_item_po_100-lifnr = lv_lifnr.
              gt_item_po_100-lifnr_name = lv_lifnr_name.
              MODIFY gt_item_po_100.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
  ENDCASE.
ENDFORM.


FORM frm_zz_rebuild_item.
  CHECK gt_item[] IS NOT INITIAL.

  CASE g_object.
    WHEN 'PO001'." 采购合同 gt_item_po 进行数量金额分摊
      CHECK gt_item_po[] IS NOT INITIAL.
      LOOP AT gt_item ASSIGNING <gs_item>.
        PERFORM frm_set_po_menge CHANGING <gs_item>." 采购填写追加数,或者存在大货通知单已购买数分配不均时重算数量金额
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_set_po_amount CHANGING <gs_item>.
      ENDLOOP.

    WHEN 'PO102'.
      LOOP AT gt_item WHERE afnam IS NOT INITIAL.
        gs_head-afnam  = gt_item-afnam.
      ENDLOOP.

    WHEN 'R1002'.
      SELECT * FROM zafo_item_batch
        FOR ALL ENTRIES IN @gt_item
        WHERE afono = @gt_item-afono_ref
        AND afonr = @gt_item-afonr_ref
        INTO TABLE @gt_item_batch.

      LOOP AT gt_item_batch.
        READ TABLE gt_item WITH KEY afono_ref = gt_item_batch-afono
                                    afonr_ref = gt_item_batch-afonr.
        IF sy-subrc EQ 0.
          gt_item_batch-afonr = gt_item-afonr.
          CLEAR gt_item_batch-afono.
        ENDIF.
      ENDLOOP.

    WHEN 'R1001' OR 'R1003' OR 'R1006' .
      SELECT SINGLE land1 FROM lfa1 INTO @DATA(l_land1)
        WHERE lifnr = @gs_head-lifnr .
      IF l_land1 NE 'CN' .
        LOOP AT gt_item ASSIGNING <gs_item>.
          <gs_item>-lgort = '1003'.
        ENDLOOP.
      ENDIF.

    WHEN 'R1019' OR 'R1020'.
      LOOP AT gt_item ASSIGNING <gs_item>.
        <gs_item>-brgew = 1.
        <gs_item>-ntgew = 1.
        <gs_item>-boxnum = 1."箱数
        <gs_item>-remark2 = '1'." 箱号
      ENDLOOP.
  ENDCASE.

ENDFORM.


" 将表头与表体相同字段的值赋值过去
FORM frm_move_head_to_item.

  FIELD-SYMBOLS: <fs_value1> TYPE any.
  FIELD-SYMBOLS: <fs_value2> TYPE any.

  DATA:lt_screen_h TYPE TABLE OF zafo_screen WITH HEADER LINE.
  DATA:lt_screen_i TYPE TABLE OF zafo_screen WITH HEADER LINE.

  CLEAR lt_screen_h[].
  CLEAR lt_screen_i[].

  LOOP AT gt_screen WHERE object = g_object.

    CHECK gt_screen-fieldname+0(6)  <> 'REMARK'.
    CHECK gt_screen-fieldname+0(6)  <> 'AMOUNT'.
    CHECK gt_screen-fieldname+0(11) <> 'COST_AMOUNT'.
    CHECK gt_screen-fieldname+0(5)  <> 'MENGE'.

    IF gt_screen-fieldalv = 'HEAD'.
      APPEND gt_screen  TO lt_screen_h.
    ENDIF.

    IF gt_screen-fieldalv = 'ITEM'.
      APPEND gt_screen  TO lt_screen_i.
    ENDIF.

  ENDLOOP.

  LOOP AT lt_screen_h.

    ASSIGN COMPONENT lt_screen_h-fieldname OF STRUCTURE gs_head TO <fs_value1>.
    CHECK sy-subrc EQ 0.

    LOOP AT gt_item ASSIGNING <gs_item>.
      ASSIGN COMPONENT lt_screen_h-fieldname OF STRUCTURE <gs_item> TO <fs_value2>.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      IF <fs_value2> IS INITIAL.
        <fs_value2> = <fs_value1>.
      ELSEIF lt_screen_h-requi = 'X'.
        <fs_value2> = <fs_value1>.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_item_po ASSIGNING <gs_item_po>.
      ASSIGN COMPONENT lt_screen_h-fieldname OF STRUCTURE <gs_item_po> TO <fs_value2>.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      IF <fs_value2> IS INITIAL.
        <fs_value2> = <fs_value1>.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.


FORM frm_set_screen_sum ." 生成合并屏幕行
  DATA:lt_screen_sum TYPE TABLE OF zafo_screen WITH HEADER LINE.
  DATA: lv_color TYPE char40.
  FIELD-SYMBOLS:<fs_sum> TYPE any.
  FIELD-SYMBOLS:<fs_item> TYPE any.

  CHECK gt_item[] IS NOT INITIAL .

  lt_screen_sum[] = gt_screen[].

  REFRESH gt_item_sum.

  DELETE lt_screen_sum WHERE object <> g_object.

  DELETE lt_screen_sum WHERE fieldalv <> 'SUM'.

  CHECK lt_screen_sum[] IS NOT INITIAL.

  LOOP AT gt_item ASSIGNING <gs_item>.

    CLEAR gt_item_sum.
    CLEAR <gs_item>-sum_key.

    gt_item_sum-icon = <gs_item>-icon.
    gt_item_sum-text = <gs_item>-text.

    LOOP AT lt_screen_sum .

      ASSIGN COMPONENT lt_screen_sum-fieldname OF STRUCTURE gt_item_sum TO <fs_sum>.
      CHECK sy-subrc EQ 0.
      ASSIGN COMPONENT lt_screen_sum-fieldname OF STRUCTURE <gs_item> TO <fs_item>.
      CHECK sy-subrc EQ 0.

      <fs_sum> = <fs_item>.

      IF lt_screen_sum-fieldname+0(5) = 'MENGE' OR lt_screen_sum-fieldname+0(6) = 'AMOUNT'.
        " 数量金额字段不求和
      ELSE.
        CLEAR lv_color.
        IF lt_screen_sum-fieldname = 'ZCOLOR'." 颜色特殊处理
          CALL FUNCTION 'CONVERSION_EXIT_ZCOLN_OUTPUT'
            EXPORTING
              input  = <fs_item>
            IMPORTING
              output = lv_color.

          IF lv_color IS INITIAL.
            <gs_item>-sum_key = <gs_item>-sum_key && '-'.
          ELSE.
            <gs_item>-sum_key = <gs_item>-sum_key && lv_color.
            gt_item_sum-zcolor = lv_color.
          ENDIF.

        ELSE.
          IF <fs_item> IS INITIAL.
            <gs_item>-sum_key = <gs_item>-sum_key && '-'.
          ELSE.
            <gs_item>-sum_key = <gs_item>-sum_key && <fs_item>.
          ENDIF.
        ENDIF.

      ENDIF.

      gt_item_sum-sum_key = <gs_item>-sum_key.

    ENDLOOP.

    COLLECT gt_item_sum .
  ENDLOOP.
ENDFORM.


FORM frm_item_screen_refresh.
  FIELD-SYMBOLS:<f_sum_value> TYPE any.
  FIELD-SYMBOLS:<f_item_value> TYPE any.

  LOOP AT gt_screen WHERE object = g_object AND split_col <> ''.
    LOOP AT gt_item_sum.
      IF gt_screen-split_col = 'X'.
        ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gt_item_sum TO <f_sum_value>.
        CHECK sy-subrc EQ 0.
        LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.
          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
          CHECK sy-subrc EQ 0.
          <f_item_value> = <f_sum_value>.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.


FORM frm_sum_screen_refresh  TABLES lt_fcat STRUCTURE lvc_s_fcat.
  DATA:lv_tabix TYPE sy-tabix.
  FIELD-SYMBOLS:<f_sum_value> TYPE any.
  FIELD-SYMBOLS:<f_item_value> TYPE any.

  CHECK gs_bustyp-sum_screen = 'X'.

  LOOP AT gt_item_sum ASSIGNING <gs_item> .
    LOOP AT gt_item WHERE sum_key = <gs_item>-sum_key.
      ADD 1 TO  lv_tabix.
      LOOP AT lt_fcat WHERE edit = 'X' AND tech <> 'X'.
        ASSIGN COMPONENT lt_fcat-fieldname OF STRUCTURE gt_item TO <f_item_value>.
        ASSIGN COMPONENT lt_fcat-fieldname OF STRUCTURE <gs_item> TO <f_sum_value>.
        IF lt_fcat-fieldname+0(5) = 'MENGE' OR lt_fcat-fieldname+0(6) = 'AMOUNT'.
          IF lv_tabix EQ 1 .
            CLEAR <f_sum_value>.
          ENDIF.
          <f_sum_value> = <f_item_value> + <f_sum_value>.
        ELSE.
          <f_sum_value> = <f_item_value>.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    CLEAR lv_tabix.
  ENDLOOP.

ENDFORM.


FORM frm_set_sum_apportion." 合并屏幕分摊逻辑
  CHECK gs_bustyp-sum_screen = 'X'.
  DATA:l_sum TYPE menge_d.

  FIELD-SYMBOLS:<f_sum_value> TYPE any.
  FIELD-SYMBOLS:<f_item_value> TYPE any.
  FIELD-SYMBOLS:<f_sum_head_value> TYPE any.
  FIELD-SYMBOLS:<f_split_col_value> TYPE any.
  FIELD-SYMBOLS:<f_split_col_sum_value> TYPE any.

  CHECK gt_item_sum[] IS NOT INITIAL.

  LOOP AT gt_screen WHERE object = g_object AND split_col <> ''.

  LOOP AT gt_item_sum.

    IF gt_screen-split_col = 'X'.
      ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gt_item_sum TO <f_sum_value>.
      CHECK sy-subrc EQ 0.
      LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.
        ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
        CHECK sy-subrc EQ 0.
        <f_item_value> = <f_sum_value>.
      ENDLOOP.

    ELSEIF gt_screen-fieldname = 'MENGE'.

      CLEAR l_sum.

      ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gt_item_sum TO <f_sum_value>.
      CHECK sy-subrc EQ 0.

      ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gs_head TO <f_sum_head_value>.
      IF sy-subrc EQ 0.
        IF sy-tabix = 1.
          <f_sum_head_value> = <f_sum_value>.
        ELSE.
          <f_sum_head_value> = <f_sum_value> + <f_sum_head_value>.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE gt_item_sum TO <f_split_col_sum_value>.
      CHECK sy-subrc EQ 0.

      l_sum = <f_sum_value>.
      " 收货数等于未清数量
      IF <f_sum_value> = <f_split_col_sum_value>.
        LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.

          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
          CHECK sy-subrc EQ 0.
          CLEAR <f_item_value>.

          ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_value>.
          CHECK sy-subrc EQ 0.
          CHECK <f_split_col_value> IS NOT INITIAL.

          "收货数 = 明细未清数
          <f_item_value> =  <f_split_col_value>.
          IF l_sum <= 0. " 在取整情况下，余数可能小于等于0
            <f_item_value> = 0.
          ENDIF.

          IF gt_screen-fieldname = 'MENGE'.
            PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
            PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
            PERFORM f_set_amount CHANGING <gs_item>.
          ENDIF.


          l_sum = l_sum - <f_item_value>.
          IF l_sum < 0.
            <f_item_value>  = <f_item_value> + l_sum.
            CLEAR l_sum.
            IF gt_screen-fieldname = 'MENGE'.
              PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
              PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
              PERFORM f_set_amount CHANGING <gs_item>.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " 拆分列汇总为小于等于0的情况
      ELSEIF <f_split_col_sum_value> <= 0.

        gt_screen-split_col = 'MENGE1'.
        ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE gt_item_sum TO <f_split_col_sum_value>.

        LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.

          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
          CHECK sy-subrc EQ 0.
          CLEAR <f_item_value>.

          ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_value>.
          CHECK sy-subrc EQ 0.
          CHECK <f_split_col_value> IS NOT INITIAL.


          IF <gs_item>-zppflag = 'X'." 产前样特殊处理
            IF  <f_split_col_value> > 0.
              <f_item_value> =  <f_split_col_value>. "收货数 = 明细未清数
            ENDIF.

          ELSE.
            "收货数 =  明细未清数  *  总收货数 / 总未清数量
            <f_item_value> = <f_split_col_value> * <f_sum_value> / <f_split_col_sum_value>.
          ENDIF.

          IF l_sum <= 0. " 在取整情况下，余数可能小于等于0
            <f_item_value> = 0.
          ENDIF.

          IF gt_screen-fieldname = 'MENGE'.
            PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
            PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
            PERFORM f_set_amount CHANGING <gs_item>.
          ENDIF.


          l_sum = l_sum - <f_item_value>.
          IF l_sum < 0.
            <f_item_value>  = <f_item_value> + l_sum.
            CLEAR l_sum.
            IF gt_screen-fieldname = 'MENGE'.
              PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
              PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
              PERFORM f_set_amount CHANGING <gs_item>.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " 收货数大于未清数量
      ELSEIF <f_sum_value> > <f_split_col_sum_value>.
        DATA: lv_item_line TYPE sy-tabix.
        DATA(lv_cj_num) =  <f_sum_value> - <f_split_col_sum_value>.
        CLEAR lv_item_line.
        LOOP AT gt_item WHERE sum_key = gt_item_sum-sum_key.
          lv_item_line  = 1 + lv_item_line." 要分配的明细行数
        ENDLOOP.

        LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.

          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
          CHECK sy-subrc EQ 0.
          CLEAR <f_item_value>.

          ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_value>.
          CHECK sy-subrc EQ 0.
          CHECK <f_split_col_value> IS NOT INITIAL.

          IF <gs_item>-zppflag = 'X'." 产前样特殊处理
            <f_item_value> =  <f_split_col_value>. "收货数 = 明细未清数

          ELSEIF lv_cj_num >= lv_item_line.
            "收货数 =  明细未清数  *  总收货数 / 总未清数量
            <f_item_value> = <f_split_col_value> * <f_sum_value> / <f_split_col_sum_value>.
          ELSE.
            <f_item_value> =  <f_split_col_value>.  "收货数 = 明细未清数
          ENDIF.

          IF l_sum <= 0. " 在取整情况下，余数可能小于等于0
            <f_item_value> = 0.
          ENDIF.

          IF gt_screen-fieldname = 'MENGE'.
            PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
            PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
            PERFORM f_set_amount CHANGING <gs_item>.
          ENDIF.


          l_sum = l_sum - <f_item_value>.
          IF l_sum < 0.
            <f_item_value>  = <f_item_value> + l_sum.
            CLEAR l_sum.
            IF gt_screen-fieldname = 'MENGE'.
              PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
              PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
              PERFORM f_set_amount CHANGING <gs_item>.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ELSE.            " 收货数小于未清数量
        LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.

          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
          CHECK sy-subrc EQ 0.
          CLEAR <f_item_value>.

          ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE <gs_item> TO <f_split_col_value>.
          CHECK sy-subrc EQ 0.
          CHECK <f_split_col_value> IS NOT INITIAL.

          IF <gs_item>-zppflag = 'X'." 产前样特殊处理
            <f_item_value> =  <f_split_col_value>. "收货数 = 明细未清数

          ELSE.
            "收货数 =  明细未清数  *  总收货数 / 总未清数量
            <f_item_value> = <f_split_col_value> * <f_sum_value> / <f_split_col_sum_value>.
          ENDIF.

          IF l_sum <= 0. " 在取整情况下，余数可能小于等于0
            <f_item_value> = 0.
          ENDIF.

          IF gt_screen-fieldname = 'MENGE'.
            PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
            PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
            PERFORM f_set_amount CHANGING <gs_item>.
          ENDIF.

          l_sum = l_sum - <f_item_value>.
          IF l_sum < 0.
            <f_item_value>  = <f_item_value> + l_sum.
            CLEAR l_sum.
            IF gt_screen-fieldname = 'MENGE'.
              PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
              PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
              PERFORM f_set_amount CHANGING <gs_item>.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.

      IF l_sum <> 0.
        <f_item_value> = <f_item_value> + l_sum.
        IF <f_item_value> < 0.
          <f_item_value> = 0.
        ENDIF.
        IF gt_screen-fieldname = 'MENGE'.
          PERFORM frm_set_round USING <gs_item>-meins CHANGING <gs_item>-menge.
          PERFORM f_set_menge_cg_dis CHANGING <gs_item>.
          PERFORM f_set_amount CHANGING <gs_item>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDLOOP.


LOOP AT gt_item_sum.

  CLEAR l_sum.
  ASSIGN COMPONENT 'MENGE_CG' OF STRUCTURE gt_item_sum TO <f_sum_value>.
  IF sy-subrc EQ 0 .
    l_sum = <f_sum_value>.
  ENDIF.

  LOOP AT gt_item ASSIGNING <gs_item> WHERE menge > 0 AND sum_key = gt_item_sum-sum_key .
    ASSIGN COMPONENT 'MENGE_CG' OF STRUCTURE <gs_item> TO <f_item_value>.
    IF <f_item_value> IS ASSIGNED .
      l_sum  = l_sum  - <f_item_value>.
    ENDIF.
  ENDLOOP.

  IF sy-subrc EQ 0 AND l_sum <> 0.

    <f_item_value> = <f_item_value> + l_sum.
  ENDIF.
ENDLOOP.

ENDFORM.


FORM frm_set_menge_bj USING fieldname .
  DATA:l_sum TYPE menge_d.
  FIELD-SYMBOLS:<f_sum_value> TYPE any.
  FIELD-SYMBOLS:<f_item_value> TYPE any.

  LOOP AT gt_item_sum.
    CLEAR l_sum.
    ASSIGN COMPONENT fieldname OF STRUCTURE gt_item_sum TO <f_sum_value>.
    IF sy-subrc EQ 0 .
      l_sum = <f_sum_value>.
    ENDIF.
    LOOP AT gt_item ASSIGNING <gs_item> WHERE sum_key = gt_item_sum-sum_key.
      ASSIGN COMPONENT fieldname OF STRUCTURE <gs_item> TO <f_item_value>.
      IF <f_item_value> IS ASSIGNED .
        l_sum  = l_sum  - <f_item_value>.
      ENDIF.

    ENDLOOP.
    IF sy-subrc EQ 0 AND   l_sum <> 0.
      <f_item_value> = <f_item_value> + l_sum.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM frm_pop_get_copy_line CHANGING line ." 拷贝行
  CLEAR line.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.

  CLEAR ls_flds.
  ls_flds-tabname = 'ZAFO_ITEM'.
  ls_flds-fieldname = 'AFONR'.
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
    line = ls_flds-value.

  ELSE.
    MESSAGE s029 DISPLAY LIKE 'E'."已取消操作
    g_error = 'X'.
    RETURN.
  ENDIF.
ENDFORM.


FORM frm_set_po_cost USING pv_save.

  CHECK gs_head-execute_type = 'PO' OR gs_head-execute_type = 'POP'.

  CLEAR gs_head-cost_amount.

  IF pv_save EQ 'X'.
    DELETE gt_item_cost WHERE cost_name = ''.
    DELETE gt_item_cost WHERE cost_amount = ''.
  ENDIF.

  LOOP AT gt_item_cost.
    gt_item_cost-line_id = sy-tabix.
    gs_head-cost_amount = gt_item_cost-cost_amount + gs_head-cost_amount.
  ENDLOOP.


  DATA:it_sa_rule  TYPE TABLE OF  zcct_sa_rule WITH HEADER LINE.
  DATA:it_sa_amount  TYPE TABLE OF  zcct_sa_amount WITH HEADER LINE .


  it_sa_rule-to_fname = 'COST_AMOUNT'.
  it_sa_rule-key_fname = 'AFONO'.
  it_sa_rule-fr_fname = 'AMOUNT'.

  APPEND it_sa_rule.

  it_sa_amount-to_fname   = 'COST_AMOUNT'.
  it_sa_amount-key_value   = gs_head-afono.
  it_sa_amount-amount   = gs_head-cost_amount.
  APPEND it_sa_amount.


  CALL FUNCTION 'ZCCT_SPLIT_AMOUNT'
    TABLES
      it_sa_rule   = it_sa_rule
      it_sa_amount = it_sa_amount
      ct_tab       = gt_item_po[]
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

ENDFORM.


FORM frm_set_pro_cost USING pv_save.

  CHECK gs_head-execute_type = 'PRO'.

  CLEAR gs_head-cost_amount.

  IF pv_save EQ 'X'.
    DELETE gt_item_cost WHERE cost_name = ''.
    DELETE gt_item_cost WHERE cost_amount = ''.
  ENDIF.

  LOOP AT gt_item_cost.
    gt_item_cost-line_id = sy-tabix.
    gs_head-cost_amount = gt_item_cost-cost_amount + gs_head-cost_amount.
  ENDLOOP.


  DATA:it_sa_rule  TYPE TABLE OF  zcct_sa_rule WITH HEADER LINE.
  DATA:it_sa_amount  TYPE TABLE OF  zcct_sa_amount WITH HEADER LINE .


  it_sa_rule-to_fname = 'COST_AMOUNT'.
  it_sa_rule-key_fname = 'AFONO'.
  it_sa_rule-fr_fname = 'AMOUNT'.

  APPEND it_sa_rule.

  it_sa_amount-to_fname   = 'COST_AMOUNT'.
  it_sa_amount-key_value   = gs_head-afono.
  it_sa_amount-amount   = gs_head-cost_amount.
  APPEND it_sa_amount.


  CALL FUNCTION 'ZCCT_SPLIT_AMOUNT'
    TABLES
      it_sa_rule   = it_sa_rule
      it_sa_amount = it_sa_amount
      ct_tab       = gt_item[]
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

ENDFORM.



FORM frm_sure_item.
  DATA:l_line_id TYPE posnr.

  l_line_id = 0.

  LOOP AT gt_item_100 WHERE selected = 'X'.
    IF gs_bustyp-busref = 'Z' OR gs_bustyp-busref = 'Y'.
      IF gt_item_100-afono IS NOT INITIAL AND gt_item_100-afono_ref IS INITIAL.
        IF gs_bustyp-execute_type <> 'POP'.
          gt_item_100-menge1 = gt_item_100-menge.
          gt_item_100-afono_ref = gt_item_100-afono.
          gt_item_100-afonr_ref = gt_item_100-afonr.

          CLEAR gt_item_100-afono.
        ENDIF.
      ENDIF.
    ENDIF.

    ADD 1 TO l_line_id.
    gt_item_100-afonr = l_line_id.
    MODIFY gt_item_100.
  ENDLOOP.


  IF gt_item_po_100[] IS NOT INITIAL.
    LOOP AT gt_item_po_100.
      IF g_bustyp = 'PO003' OR g_bustyp = 'PO005'.
        READ TABLE gt_item_100 WITH KEY icon = gt_item_po_100-icon
                                       werks = gt_item_po_100-werks
                                       afono = gt_item_po_100-afono
                                      zzpino = gt_item_po_100-zzpino
                                       idnlf = gt_item_po_100-idnlf
                                       matnr = gt_item_po_100-matnr
                                      zcolor = gt_item_po_100-zcolor
                                      zsize  = gt_item_po_100-zsize
                                      znorms = gt_item_po_100-znorms
                                      zppdhd = gt_item_po_100-zppdhd     "add by at-yuxs 20220402
                                  zzbom_item = gt_item_po_100-zzbom_item "add by at-yuxs 工序委外这个字段存的是工序编号
                                     zppflag = gt_item_po_100-zppflag.
      ELSEIF g_bustyp = 'POC01'.
        READ TABLE gt_item_100 WITH KEY icon = gt_item_po_100-icon
                                       werks = gt_item_po_100-werks
                                      afono  = gt_item_po_100-afono
                                      ebeln  = gt_item_po_100-ebeln
                                      zzpino = gt_item_po_100-zzpino
                                      idnlf  = gt_item_po_100-idnlf
                                      matnr  = gt_item_po_100-matnr
                                      zcolor = gt_item_po_100-zcolor
                                      zsize  = gt_item_po_100-zsize
                                      znorms = gt_item_po_100-znorms
                                     zppflag = gt_item_po_100-zppflag.
      ELSE.
        READ TABLE gt_item_100 WITH KEY icon = gt_item_po_100-icon
                                       werks = gt_item_po_100-werks
                                      afono  = gt_item_po_100-afono
                                      zzpino = gt_item_po_100-zzpino
                                      idnlf  = gt_item_po_100-idnlf
                                      matnr  = gt_item_po_100-matnr
                                      zcolor = gt_item_po_100-zcolor
                                      zsize  = gt_item_po_100-zsize
                                      znorms = gt_item_po_100-znorms
                                     zppflag = gt_item_po_100-zppflag.
      ENDIF.
      IF sy-subrc EQ 0.
        IF gt_item_100-selected IS INITIAL.
          DELETE gt_item_po_100.
        ELSE.
*add by at-yuxs 20220328,委外采购订单选择的大货通知单号也需一样 begin
          IF g_bustyp = 'PO003' OR g_bustyp = 'PO005'.
            IF gt_item_100-zppdhd NE gt_item_po_100-zppdhd.
              DELETE gt_item_po_100.
              CONTINUE.
            ENDIF.
          ENDIF.
*add by at-yuxs 20220328,委外采购订单选择的大货通知单号也需一样 end
          CLEAR gt_item_po_100-afono.
          gt_item_po_100-afonr = gt_item_100-afonr.
          MODIFY gt_item_po_100.
        ENDIF.
      ELSE.
        DELETE gt_item_po_100.
      ENDIF.
    ENDLOOP.

    SORT gt_item_po_100 BY afonr.

    l_line_id = 0.
    LOOP AT gt_item_po_100.
      ADD 1 TO l_line_id.
      gt_item_po_100-ebelp = l_line_id.
      MODIFY gt_item_po_100.
    ENDLOOP.

  ENDIF.

  IF gt_item_cost[] IS NOT INITIAL.
    LOOP AT gt_item_cost .
      READ TABLE gt_item WITH KEY
                         afono =  gt_item_cost-afono.
      IF sy-subrc EQ 0.
        CLEAR gt_item_cost-afono.
        MODIFY gt_item_cost.
      ELSE.
        DELETE gt_item_cost.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.


FORM frm_mass_modify_100 TABLES lt_fcat STRUCTURE lvc_s_fcat.
  DATA:ls_fieldname TYPE fieldname.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.
  FIELD-SYMBOLS <fs_value> TYPE any.
  CLEAR lt_flds.

  READ TABLE gt_item_100 ASSIGNING <gs_item> WITH KEY selected = 'X'.
  IF sy-subrc NE 0.
    MESSAGE s023(zafo) ."WITH '请选择修改行' .
    RETURN.
  ENDIF.

  LOOP AT  lt_fcat WHERE edit = 'X'.
    CHECK lt_fcat-fieldname <> 'SELECTED'.
    CHECK lt_fcat-fieldname+0(5) <> 'MENGE'.
    CHECK lt_fcat-fieldname+0(5) <> 'PRICE'.
    CHECK lt_fcat-fieldname+0(6) <> 'AMOUNT'.
    ls_flds-tabname = lt_fcat-ref_table.
    ls_flds-fieldtext = lt_fcat-scrtext_m.
    ls_flds-fieldname = lt_fcat-fieldname.
    APPEND ls_flds TO lt_flds.
    CLEAR ls_flds.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-015 "请输入批量修改的值
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  LOOP AT lt_flds INTO ls_flds.

    IF ls_flds-value IS INITIAL.
      CONTINUE.
    ENDIF.
    IF ls_flds-value = '0'.
      CLEAR ls_flds-value.
    ENDIF.
    LOOP AT gt_item_100 ASSIGNING <gs_item> WHERE selected = 'X'.
      ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE <gs_item> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = ls_flds-value.
      ENDIF.
    ENDLOOP.

    READ TABLE gt_screen WITH KEY fieldalv = 'HEAD' fieldname = ls_flds-fieldname.
    IF sy-subrc EQ 0.
      IF gt_screen-fedit = 'X'.
        ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE gs_head TO <fs_value>.
        IF sy-subrc EQ 0.
          <fs_value> = ls_flds-value.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL METHOD cl_gui_cfw=>update_view
    EXPORTING
      called_by_system = 'X'.

  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = 'ENTR'.

ENDFORM.


FORM frm_mass_modify TABLES lt_fcat STRUCTURE lvc_s_fcat.
  DATA:ls_fieldname TYPE fieldname.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.
  FIELD-SYMBOLS <fs_value> TYPE any.
  CLEAR lt_flds.

  LOOP AT  lt_fcat WHERE edit = 'X'.
    CHECK lt_fcat-fieldname <> 'SELECTED'.
    CHECK lt_fcat-fieldname+0(5) <> 'MENGE'.
    CHECK lt_fcat-fieldname+0(5) <> 'PRICE'.
    CHECK lt_fcat-fieldname+0(6) <> 'AMOUNT'.
    ls_flds-tabname = lt_fcat-ref_table.
    ls_flds-fieldtext = lt_fcat-scrtext_m.
    ls_flds-fieldname = lt_fcat-fieldname.
    APPEND ls_flds TO lt_flds.
    CLEAR ls_flds.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-015 "请输入批量修改的值
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  LOOP AT lt_flds INTO ls_flds.

    IF ls_flds-value IS INITIAL.
      CONTINUE.
    ENDIF.
    IF ls_flds-value = '0'.
      CLEAR ls_flds-value.
    ENDIF.

    LOOP AT gt_item ASSIGNING <gs_item>.
      ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE <gs_item> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = ls_flds-value.
      ENDIF.
    ENDLOOP.

    READ TABLE gt_screen WITH KEY fieldalv = 'HEAD' fieldname = ls_flds-fieldname.
    IF sy-subrc EQ 0.
      IF gt_screen-fedit = 'X'.
        ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE gs_head TO <fs_value>.
        IF sy-subrc EQ 0.
          <fs_value> = ls_flds-value.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL METHOD cl_gui_cfw=>update_view
    EXPORTING
      called_by_system = 'X'.

  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = 'ENTR'.

ENDFORM.


FORM frm_mass_sum_modify TABLES lt_fcat STRUCTURE lvc_s_fcat.

  DATA:ls_fieldname TYPE fieldname.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.
  FIELD-SYMBOLS <fs_value> TYPE any.
  CLEAR lt_flds.

  LOOP AT  lt_fcat WHERE edit = 'X'.
    CHECK lt_fcat-fieldname <> 'SELECTED'.
    CHECK lt_fcat-fieldname+0(5) <> 'MENGE'.
    CHECK lt_fcat-fieldname+0(5) <> 'PRICE'.
    CHECK lt_fcat-fieldname+0(6) <> 'AMOUNT'.
    ls_flds-tabname = lt_fcat-ref_table.
    ls_flds-fieldtext = lt_fcat-scrtext_m.
    ls_flds-fieldname = lt_fcat-fieldname.
    APPEND ls_flds TO lt_flds.
    CLEAR ls_flds.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-015 "'请输入批量修改的值'
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  LOOP AT lt_flds INTO ls_flds.

    IF ls_flds-value IS INITIAL.
      CONTINUE.
    ENDIF.
    IF ls_flds-value = '0'.
      CLEAR ls_flds-value.
    ENDIF.
    LOOP AT gt_item_sum ASSIGNING <gs_item> .
      ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE <gs_item> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = ls_flds-value.
      ENDIF.
    ENDLOOP.

    READ TABLE gt_screen WITH KEY fieldalv = 'HEAD' fieldname = ls_flds-fieldname.
    IF sy-subrc EQ 0.
      IF gt_screen-fedit = 'X'.
        ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE gs_head TO <fs_value>.
        IF sy-subrc EQ 0.
          <fs_value> = ls_flds-value.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CALL METHOD cl_gui_cfw=>update_view
    EXPORTING
      called_by_system = 'X'.

  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = 'ENTR'.

ENDFORM.


FORM frm_po_mass_modify TABLES lt_fcat STRUCTURE lvc_s_fcat
                        lt_item STRUCTURE zafo_sitem.
  DATA:ls_fieldname TYPE fieldname.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA:lv_amount TYPE zafo_amount.
  DATA: p_gv_ret_code TYPE c.
  FIELD-SYMBOLS <fs_value> TYPE any.

  CLEAR lt_flds.
  CLEAR g_exec_flag.

  LOOP AT  lt_fcat WHERE edit = 'X'.
    CHECK lt_fcat-fieldname <> 'SELECTED'.

    CHECK lt_fcat-fieldname+0(5) <> 'MENGE'.
    CHECK lt_fcat-fieldname+0(6) <> 'AMOUNT'.
*    CHECK lt_fcat-fieldname+0(5) <> 'PRICE'.
    ls_flds-tabname = lt_fcat-ref_table.
    ls_flds-fieldtext = lt_fcat-scrtext_m.
    ls_flds-fieldname = lt_fcat-fieldname.
    APPEND ls_flds TO lt_flds.
    CLEAR ls_flds.
  ENDLOOP.

  CHECK sy-subrc EQ 0.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-015 "'请输入批量修改的值'
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.


  LOOP AT lt_flds INTO ls_flds.
    IF ls_flds-value IS INITIAL.
      CONTINUE.
    ENDIF.

    IF ls_flds-value = '0'.
      CLEAR ls_flds-value.
    ENDIF.

    LOOP AT lt_item.
      LOOP AT  gt_item ASSIGNING <gs_item> WHERE idnlf = lt_item-idnlf
                        AND matnr   = lt_item-matnr
                        AND zcolor  = lt_item-zcolor
                        AND zsize   = lt_item-zsize
                        AND znorms  = lt_item-znorms
                        AND zppflag  = lt_item-zppflag.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE <gs_item> TO <fs_value>.
          IF sy-subrc EQ 0.

            <fs_value> = ls_flds-value.

            IF ls_flds-fieldname = 'PRICE_LONG'.
              PERFORM frm_set_price  CHANGING <gs_item>.
              PERFORM f_set_amount CHANGING <gs_item>.
              PERFORM frm_set_po_amount CHANGING <gs_item>.
            ELSEIF ls_flds-fieldname = 'EEIND'.
              PERFORM frm_set_po_eeind CHANGING <gs_item>.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
  IF sy-subrc EQ 0.
    g_exec_flag = 'C'.
  ENDIF.

ENDFORM.


FORM frm_mass_batch_set TABLES lt_fcat STRUCTURE lvc_s_fcat
                                lt_item STRUCTURE zafo_sitem.

  FIELD-SYMBOLS:<fs_value1> TYPE any.
  FIELD-SYMBOLS:<fs_value2> TYPE any.

  LOOP AT lt_fcat INTO DATA(ls_fcat) WHERE edit = 'X'.

    ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE lt_item TO <fs_value2>.
    CHECK sy-subrc EQ 0.
    CHECK <fs_value2> IS NOT INITIAL .

    ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <gs_item> TO <fs_value1>.
    CHECK sy-subrc EQ 0.

    <fs_value1> = <fs_value2>.
    CASE ls_fcat-fieldname .

      WHEN 'MENGE_ZJ'.
        PERFORM frm_set_po_menge CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_set_po_amount CHANGING <gs_item>.

      WHEN 'PRICE_LONG'.
        PERFORM frm_set_price  CHANGING <gs_item>.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_set_po_amount CHANGING <gs_item>.

      WHEN 'PRICE'.
        PERFORM f_set_amount CHANGING <gs_item>.
        PERFORM frm_set_po_amount CHANGING <gs_item>.
    ENDCASE.
  ENDLOOP.

ENDFORM.


FORM frm_mass_batch  TABLES lt_fcat STRUCTURE lvc_s_fcat.
  DATA:l_model TYPE char1.
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  CHECK g_readonly <> 'D'.
  CLEAR g_exec_flag.

  CALL FUNCTION 'ZAFO_ITEM_BATCH'
    IMPORTING
      e_model = l_model
    TABLES
      ut_item = gt_item[]
      ut_fcat = lt_fcat
      et_item = lt_item[].

  DELETE lt_item WHERE icon <> icon_led_yellow.

  CASE l_model.
    WHEN 'A'.
      LOOP AT lt_item.
        LOOP AT gt_item ASSIGNING <gs_item>
                          WHERE matnr   = lt_item-matnr
                          AND maktx    = lt_item-maktx
                          AND idnlf    = lt_item-idnlf
                          AND zcolor   = lt_item-zcolor
                          AND zsize    = lt_item-zsize
                          AND znorms   = lt_item-znorms.
          PERFORM frm_mass_batch_set TABLES lt_fcat lt_item.
        ENDLOOP.
      ENDLOOP.
      IF sy-subrc EQ 0.
        g_exec_flag = 'C'.
      ENDIF.

    WHEN 'B'.
      LOOP AT lt_item.
        LOOP AT gt_item ASSIGNING <gs_item>
                          WHERE matnr   = lt_item-matnr
                          AND maktx    = lt_item-maktx
                          AND idnlf    = lt_item-idnlf.

          PERFORM frm_mass_batch_set TABLES lt_fcat lt_item.

        ENDLOOP.
      ENDLOOP.
      IF sy-subrc EQ 0.
        g_exec_flag = 'C'.
      ENDIF.
    WHEN 'C'.
      LOOP AT lt_item.
        LOOP AT gt_item ASSIGNING <gs_item>
                          WHERE matnr   = lt_item-matnr
                          AND maktx    = lt_item-maktx
                          AND znorms    = lt_item-znorms.
          PERFORM frm_mass_batch_set TABLES lt_fcat lt_item.

        ENDLOOP.
      ENDLOOP.
      IF sy-subrc EQ 0.
        g_exec_flag = 'C'.
      ENDIF.
    WHEN 'D'.
      LOOP AT lt_item.
        LOOP AT gt_item ASSIGNING <gs_item>
                          WHERE matnr   = lt_item-matnr
                          AND maktx    = lt_item-maktx
                          AND zzpino    = lt_item-zzpino.
          PERFORM frm_mass_batch_set TABLES lt_fcat lt_item.

        ENDLOOP.
      ENDLOOP.
      IF sy-subrc EQ 0.
        g_exec_flag = 'C'.
      ENDIF.

  ENDCASE.
ENDFORM.


FORM frm_po_round TABLES lt_item STRUCTURE zafo_sitem." 按行向上取整

  DATA:round_type TYPE zafo_round.
  DATA:round_type1 TYPE i.
  DATA:menge_lo TYPE p DECIMALS 9.
  DATA:menge_ln TYPE p DECIMALS 9.


  PERFORM frm_pop_round CHANGING round_type.

  round_type1 = round_type.

  CHECK g_error <> 'X'.

  SORT lt_item BY idnlf matnr zcolor zsize znorms zppflag.

  DELETE ADJACENT DUPLICATES FROM lt_item COMPARING idnlf matnr zcolor zsize znorms zppflag.

  LOOP AT lt_item.
    CLEAR: menge_lo,menge_ln.

    LOOP AT gt_item WHERE idnlf   = lt_item-idnlf
                        AND matnr   = lt_item-matnr
                        AND zcolor  = lt_item-zcolor
                        AND zsize   = lt_item-zsize
                        AND znorms  = lt_item-znorms
                        AND zppflag  = lt_item-zppflag.
      CLEAR menge_lo.
      menge_lo = gt_item-menge_cg + gt_item-menge_zj.

      sy-subrc = 0.

      CALL FUNCTION 'ROUND'
        EXPORTING
          decimals = round_type
          input    = menge_lo
          sign     = '+'
        IMPORTING
          output   = menge_ln.
      IF menge_ln = 0.
        CASE round_type.
          WHEN '0'.
            menge_ln = menge_ln + 1.
          WHEN '1'.
            menge_ln = menge_ln + '0.1'.
          WHEN '2'.
            menge_ln = menge_ln + '0.01'.
          WHEN '3'.
            menge_ln = menge_ln + '0.001'.
        ENDCASE.
      ENDIF.

      IF menge_ln <> menge_lo.
        gt_item-menge_zj = gt_item-menge_zj + menge_ln - menge_lo.
      ENDIF.
      PERFORM frm_set_po_menge CHANGING gt_item.
      PERFORM f_set_amount CHANGING gt_item.
      PERFORM frm_set_po_amount CHANGING gt_item.
      MODIFY gt_item .
    ENDLOOP.
  ENDLOOP.

ENDFORM.


FORM frm_po_gg_round TABLES lt_item STRUCTURE zafo_sitem." 按规格向上取整
  DATA:lt_sum_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  DATA:round_type TYPE char10.

  DATA:l_sum TYPE menge_d.
  DATA:lv_menge_zj TYPE menge_d.
  DATA:menge_lo TYPE p DECIMALS 9.
  DATA:menge_ln TYPE p DECIMALS 9.

  CLEAR: lt_sum_item[].

  SORT lt_item BY zzpino matnr idnlf zcolor zsize znorms zppflag.

  LOOP AT lt_item.
    CASE lt_item-maktx.
      WHEN '胶条' OR '透明胶条'.
        round_type = '0'.
        lt_sum_item-maktx =  lt_item-maktx.
        lt_sum_item-zcolor_text = lt_item-zcolor_text.
        lt_sum_item-idnlf = lt_item-idnlf.
        lt_sum_item-znorms = lt_item-znorms.
        lt_sum_item-menge_cg = lt_item-menge_cg.
      WHEN OTHERS.
        MESSAGE s113  DISPLAY LIKE 'E'.
        g_error = 'X'.
        RETURN.
    ENDCASE.

    COLLECT lt_sum_item.
  ENDLOOP.

  SORT lt_sum_item BY zzpino matnr zcolor zsize znorms zppflag.
  SORT gt_item BY idnlf znorms DESCENDING.

  LOOP AT lt_sum_item.
    CLEAR: menge_lo,menge_ln,l_sum.

    menge_lo = lt_sum_item-menge_cg.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals = round_type
        input    = menge_lo
        sign     = '+'
      IMPORTING
        output   = menge_ln.
    IF menge_ln = 0.
      CASE round_type.
        WHEN '0'.
          menge_ln = menge_ln + 1.
        WHEN '1'.
          menge_ln = menge_ln + '0.1'.
        WHEN '2'.
          menge_ln = menge_ln + '0.01'.
        WHEN '3'.
          menge_ln = menge_ln + '0.001'.
      ENDCASE.
    ENDIF.

    IF menge_ln <> menge_lo.
      lt_sum_item-menge_zj = menge_ln - menge_lo.
    ENDIF.

    l_sum = lt_sum_item-menge_zj.

    CASE lt_sum_item-maktx.
      WHEN '胶条' OR '透明胶条'.
        LOOP AT gt_item ASSIGNING <gs_item> WHERE idnlf = lt_sum_item-idnlf
                                            AND zcolor_text  = lt_sum_item-zcolor_text
                                            AND znorms  = lt_sum_item-znorms.
          CLEAR <gs_item>-menge_zj .
          IF menge_ln <> menge_lo AND l_sum > 0.
            CLEAR lv_menge_zj.
            lv_menge_zj = l_sum.
            <gs_item>-menge_zj = lv_menge_zj.
            l_sum = l_sum - <gs_item>-menge_zj.
          ENDIF.
          PERFORM frm_set_po_menge CHANGING <gs_item>.
          PERFORM f_set_amount CHANGING <gs_item>.
          PERFORM frm_set_po_amount CHANGING <gs_item>.
        ENDLOOP.

      WHEN OTHERS.
        MESSAGE s113  DISPLAY LIKE 'E'.
        g_error = 'X'.
        RETURN.
    ENDCASE.
  ENDLOOP.

ENDFORM.


FORM frm_po_qdl_round TABLES lt_item STRUCTURE zafo_sitem." 起订量向上取整

  DATA:lt_sum_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  DATA:round_type TYPE char10.

  DATA:l_sum TYPE menge_d.
  DATA:lv_menge_zj TYPE menge_d.
  DATA:menge_lo TYPE p DECIMALS 9.
  DATA:menge_ln TYPE p DECIMALS 9.

  CLEAR: lt_sum_item[].

  SORT lt_item BY zzpino matnr zcolor zsize znorms zppflag.

  LOOP AT lt_item.
    CASE lt_item-matnr.
      WHEN 'RPMLB00005'.
        round_type = '250'.
*        lt_sum_item-zzpino = lt_item-zzpino.
        lt_sum_item-matnr = lt_item-matnr.
*        lt_sum_item-zcolor = lt_item-zcolor.
        lt_sum_item-zsize = lt_item-zsize.
        lt_sum_item-znorms = lt_item-znorms.
        lt_sum_item-menge_cg = lt_item-menge_cg.
*        lt_sum_item-menge_zj = lt_item-menge_zj.
      WHEN 'RPMHT00006'.
        round_type = '250'.
        lt_sum_item-matnr = lt_item-matnr.
        lt_sum_item-zsize = lt_item-zsize.
        lt_sum_item-menge_cg = lt_item-menge_cg.
*        lt_sum_item-menge_zj = lt_item-menge_zj.
      WHEN OTHERS.
        MESSAGE s113  DISPLAY LIKE 'E'.
        g_error = 'X'.
        RETURN.
    ENDCASE.

    COLLECT lt_sum_item.
  ENDLOOP.

  SORT lt_sum_item BY zzpino matnr zcolor zsize znorms zppflag.
  SORT gt_item BY zzpino matnr zcolor zsize znorms zppflag DESCENDING.
  LOOP AT lt_sum_item.
    CLEAR: menge_lo,menge_ln,l_sum.

    menge_lo = lt_sum_item-menge_cg.
    PERFORM frm_set_qdl_round USING round_type menge_lo CHANGING menge_ln.

    IF menge_ln <> menge_lo.
      lt_sum_item-menge_zj = menge_ln - menge_lo.
    ENDIF.

    CASE lt_sum_item-matnr.
        l_sum = lt_sum_item-menge_zj.

      WHEN 'RPMLB00005'.

        LOOP AT gt_item ASSIGNING <gs_item> WHERE matnr   = lt_sum_item-matnr
*                                               AND zzpino = lt_sum_item-zzpino
*                                               AND zcolor  = lt_sum_item-zcolor
                                               AND zsize   = lt_sum_item-zsize
                                               AND znorms  = lt_sum_item-znorms.
          IF <gs_item>-zppflag IS NOT INITIAL.
            CONTINUE.
          ENDIF.
          IF menge_ln <> menge_lo.
            CLEAR lv_menge_zj.

            lv_menge_zj = lt_sum_item-menge_zj * ( <gs_item>-menge_cg / lt_sum_item-menge_cg ).
            CALL FUNCTION 'ROUND'
              EXPORTING
                decimals = '0'
                input    = lv_menge_zj
                sign     = '+'
              IMPORTING
                output   = <gs_item>-menge_zj.
            l_sum = l_sum - <gs_item>-menge_zj.
          ENDIF.
          PERFORM frm_set_po_menge CHANGING <gs_item>.
          PERFORM f_set_amount CHANGING <gs_item>.
          PERFORM frm_set_po_amount CHANGING <gs_item>.
        ENDLOOP.
        IF l_sum <> 0.
          <gs_item>-menge_zj = l_sum + <gs_item>-menge_zj.
          PERFORM frm_set_po_menge CHANGING <gs_item>.
          PERFORM f_set_amount CHANGING <gs_item>.
          PERFORM frm_set_po_amount CHANGING <gs_item>.
        ENDIF.
        CLEAR l_sum.
      WHEN 'RPMHT00006'.
        LOOP AT gt_item ASSIGNING <gs_item> WHERE matnr = lt_sum_item-matnr
                                             AND zsize = lt_sum_item-zsize.
          IF <gs_item>-zppflag IS NOT INITIAL.
            CONTINUE.
          ENDIF.
          IF menge_ln <> menge_lo.
            CLEAR lv_menge_zj.
            lv_menge_zj = lt_sum_item-menge_zj * ( <gs_item>-menge_cg / lt_sum_item-menge_cg ).
            CALL FUNCTION 'ROUND'
              EXPORTING
                decimals = '0'
                input    = lv_menge_zj
                sign     = '+'
              IMPORTING
                output   = <gs_item>-menge_zj.
            l_sum = l_sum - <gs_item>-menge_zj.
          ENDIF.
          PERFORM frm_set_po_menge CHANGING <gs_item>.
          PERFORM f_set_amount CHANGING <gs_item>.
          PERFORM frm_set_po_amount CHANGING <gs_item>.
        ENDLOOP.
        IF l_sum <> 0.
          <gs_item>-menge_zj = l_sum + <gs_item>-menge_zj.
          PERFORM frm_set_po_menge CHANGING <gs_item>.
          PERFORM f_set_amount CHANGING <gs_item>.
          PERFORM frm_set_po_amount CHANGING <gs_item>.
        ENDIF.
        CLEAR l_sum.
      WHEN OTHERS.
        MESSAGE s113  DISPLAY LIKE 'E'.
        g_error = 'X'.
        RETURN.
    ENDCASE.
  ENDLOOP.
ENDFORM.


FORM frm_set_group TABLES ct_head STRUCTURE zafo_shead
                          ct_item STRUCTURE zafo_sitem.
  DATA:BEGIN OF lt_group OCCURS 0,
         afono TYPE zafono,
         value TYPE char20,
       END OF lt_group.

  FIELD-SYMBOLS:<fs_value> TYPE any.
  FIELD-SYMBOLS:<fs_value_h> TYPE any.

  DATA:ls_group(255) TYPE c.
  DATA:ls_afono TYPE zafono.

  CHECK gt_screen[] IS NOT INITIAL.

  SORT ct_item BY afono afonr.

  LOOP AT gt_screen WHERE object = g_object
                       AND fieldalv = 'HEAD'
                       AND fieldname+(5) = 'GROUP'.

    CHECK gt_screen-split_col IS NOT INITIAL.
    CLEAR lt_group[].

    LOOP AT ct_item.
      ASSIGN COMPONENT gt_screen-split_col OF STRUCTURE ct_item TO <fs_value>.
      CHECK sy-subrc EQ 0.
      CHECK <fs_value> IS NOT INITIAL.
      lt_group-afono = ct_item-afono.
      lt_group-value = <fs_value>.
      APPEND lt_group.
      CLEAR lt_group.
    ENDLOOP.

    CHECK lt_group[] IS NOT INITIAL.

    SORT lt_group.
    DELETE ADJACENT DUPLICATES FROM lt_group.

    CLEAR ls_afono.
    LOOP AT lt_group.
      IF ls_afono IS INITIAL.
        ls_afono = lt_group-afono.
      ENDIF.

      IF ls_afono = lt_group-afono.
        ls_group =  ls_group && ';'  && lt_group-value .
      ELSE.
        READ TABLE ct_head ASSIGNING  FIELD-SYMBOL(<fs_head>) WITH KEY afono = ls_afono.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <fs_head> TO <fs_value_h>.
          IF sy-subrc EQ 0.
            <fs_value_h> = ls_group+1(254).
            CLEAR ls_group.
          ENDIF.
        ENDIF.
        ls_afono = lt_group-afono.
        ls_group =  ';' && lt_group-value.
      ENDIF.
    ENDLOOP.

    IF sy-subrc EQ 0.
      READ TABLE ct_head ASSIGNING  <fs_head> WITH KEY afono = ls_afono.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <fs_head> TO <fs_value_h>.
        IF sy-subrc EQ 0.
          <fs_value_h> = ls_group+1(254).
          CLEAR ls_group.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
