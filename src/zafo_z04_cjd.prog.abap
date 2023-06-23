*&---------------------------------------------------------------------*
*& 包含               ZAFO_Z04_CJD
*&---------------------------------------------------------------------*

***********超交显示
FORM frm_zz_rkcj USING lv_exec_env lv_include.
  DATA lv_continue TYPE char1.
  CHECK g_error NE 'X'.
  CHECK  g_bustyp = 'R1001'
     OR g_bustyp = 'R1002' OR g_bustyp = 'R1003' OR g_bustyp = 'R1006'
     OR g_bustyp = 'R1013' OR g_bustyp = 'R1015' OR g_bustyp = 'R1019'
     OR g_bustyp = 'ASN02' OR g_bustyp = 'ASN03'.

  CALL FUNCTION 'ZAFO_PO_SUPERB'
    EXPORTING
      i_exec_env = lv_exec_env
      i_include  = lv_include
    IMPORTING
      o_continue = lv_continue
    TABLES
      ut_item    = gt_item[].
  IF lv_continue IS INITIAL AND lv_exec_env <> 'DIS'.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '042' '' '' '' ''.
  ENDIF.
ENDFORM.


***********入库超交单
FORM frm_zz_cjd_post.
  DATA:ct_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:cjt_head TYPE TABLE OF zafo_head WITH HEADER LINE.

  DATA lv_continue TYPE char1.
  DATA:lv_afonr TYPE zafonr.

  DATA:BEGIN OF ls_ebeln,
         ebeln TYPE ebeln,
       END OF ls_ebeln.
  DATA:lt_ebeln LIKE TABLE OF ls_ebeln.

  CHECK g_error NE 'X'.

  CHECK gs_head-bustyp = 'R1002' OR gs_head-bustyp = 'R1003' OR g_bustyp = 'R1006'
     OR gs_head-bustyp = 'R1004' OR gs_head-bustyp = 'R1005'
     OR gs_head-bustyp = 'R1013' OR gs_head-bustyp = 'R1015' OR gs_head-bustyp = 'R1019'
     OR gs_head-bustyp = 'ASN02' OR gs_head-bustyp = 'ASN03'.


  CASE gs_head-bustyp.
    WHEN 'R1004' OR 'R1005'." 退货
      LOOP AT gt_item.
        ls_ebeln-ebeln = gt_item-ebeln.
        COLLECT ls_ebeln INTO lt_ebeln.
      ENDLOOP.

    WHEN OTHERS.

      CALL FUNCTION 'ZAFO_PO_SUPERB'
        EXPORTING
          i_exec_env = 'POST'
          i_include  = ''
        IMPORTING
          o_continue = lv_continue
        TABLES
          ut_item    = gt_item[]
          ct_item    = ct_item[].

      CHECK ct_item[] IS NOT INITIAL.

      LOOP AT ct_item.
        ls_ebeln-ebeln = ct_item-ebeln.
        COLLECT ls_ebeln INTO lt_ebeln.
      ENDLOOP.
  ENDCASE.

  LOOP AT lt_ebeln INTO ls_ebeln.
    SELECT SINGLE * INTO cjt_head
      FROM zafo_head
      WHERE bustyp = 'CJD01'
      AND ebeln = ls_ebeln-ebeln.
    IF sy-subrc EQ 0 .
      PERFORM frm_cjsx USING ls_ebeln-ebeln.
    ELSE.
      PERFORM frm_cjcre USING ls_ebeln-ebeln.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM frm_mb_cancel_cjsx.
  DATA:BEGIN OF ls_ebeln,
         ebeln TYPE ebeln,
       END OF ls_ebeln.
  DATA:lt_ebeln LIKE TABLE OF ls_ebeln.

  DATA:ct_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:cjt_head TYPE TABLE OF zafo_head WITH HEADER LINE.

  CHECK g_error NE 'X'.
  CHECK gs_head-bustyp = 'R1002' OR gs_head-bustyp = 'R1003' OR g_bustyp = 'R1006'
     OR gs_head-bustyp = 'R1004' OR gs_head-bustyp = 'R1005'
     OR gs_head-bustyp = 'R1013' OR gs_head-bustyp = 'R1015' OR gs_head-bustyp = 'R1019'
     OR gs_head-bustyp = 'ASN02' OR gs_head-bustyp = 'ASN03'.


  LOOP AT gt_item.
    ls_ebeln-ebeln = gt_item-ebeln.
    COLLECT ls_ebeln INTO lt_ebeln.
  ENDLOOP.


  LOOP AT lt_ebeln INTO ls_ebeln.
    SELECT SINGLE * INTO cjt_head
      FROM zafo_head
      WHERE bustyp = 'CJD01'
      AND ebeln = ls_ebeln-ebeln.
    IF sy-subrc EQ 0 .
      PERFORM frm_cjsx USING ls_ebeln-ebeln.
    ENDIF.
  ENDLOOP.

ENDFORM.



FORM frm_cjcre USING pv_ebeln.  " 超交创建
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:ct_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:rkt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:cs_head TYPE TABLE OF zafo_head WITH HEADER LINE.
  DATA lv_continue TYPE char1.
  DATA:lv_afonr TYPE zafonr.
  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CALL FUNCTION 'ZAFO_PO_SUPERB'
    EXPORTING
      i_exec_env = 'POST'
      i_include  = ''
      i_ebeln    = pv_ebeln
    IMPORTING
      o_continue = lv_continue
    TABLES
      ut_item    = rkt_item[]
      ct_item    = lt_item[].

  CHECK lt_item[] IS NOT INITIAL.

  LOOP AT lt_item .
    ADD 1 TO lv_afonr.
    lt_item-afonr = lv_afonr.
    APPEND lt_item TO ct_item.
  ENDLOOP.

  READ TABLE ct_item INDEX 1.
  cs_head-nenam = ct_item-ponam.
  cs_head-afnam = ct_item-afnam.
  cs_head-remark2 = ct_item-remark2.
  cs_head-ebeln = ct_item-ebeln.

  CALL FUNCTION 'ZAFO_CREATE_SAVE'
    EXPORTING
      i_bustyp  = 'CJD01'
*     NO_COMMIT =
    TABLES
      et_return = lt_return[]
      ct_item   = ct_item[]
*     CT_ITEM_PO         =
*     CT_ITEM_COST       =
    CHANGING
      cs_head   = cs_head
*   EXCEPTIONS
*     ERROR     = 1
*     OTHERS    = 2
    .
  LOOP AT lt_return WHERE type = 'E'.
    APPEND lt_return TO  ot_return.
    g_error = 'X'.
  ENDLOOP.
  CHECK g_error <> 'X'.
  PERFORM frm_cgrkcjqr_trigger USING cs_head.
ENDFORM.


FORM frm_cjsx USING pv_ebeln.  " 超交刷新
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  DATA:rkt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  DATA:cjt_item TYPE TABLE OF zafo_item WITH HEADER LINE.
  DATA:cjs_head TYPE zafo_head.
  DATA lv_continue TYPE char1.
  DATA:lv_cjbl TYPE menge_d.


  CHECK pv_ebeln IS NOT INITIAL.

  CALL FUNCTION 'ZAFO_PO_SUPERB'
    EXPORTING
      i_exec_env = 'POST'
      i_include  = ''
      i_ebeln    = pv_ebeln
    IMPORTING
      o_continue = lv_continue
    TABLES
      ut_item    = rkt_item[]
      ct_item    = lt_item[].


  SELECT SINGLE * INTO cjs_head
    FROM zafo_head
    WHERE bustyp = 'CJD01'
    AND ebeln = pv_ebeln.

  SELECT * INTO TABLE cjt_item
    FROM zafo_item
    WHERE afono = cjs_head-afono
    AND ebeln = pv_ebeln.

  CHECK cjs_head IS NOT INITIAL.

  IF cjs_head-status = 'B' OR  cjs_head-status = 'C' OR cjs_head-status = 'T'.
    RETURN.
  ENDIF.

  IF lt_item[] IS INITIAL .
    LOOP AT cjt_item ASSIGNING FIELD-SYMBOL(<cjs_item>).
      CLEAR <cjs_item>-remark1.
      CLEAR <cjs_item>-remark3.
      <cjs_item>-menge = 0.
      <cjs_item>-menge_cg = 0.
      <cjs_item>-amount = 0.
    ENDLOOP.
  ENDIF.

  LOOP AT cjt_item ASSIGNING <cjs_item>.
    READ TABLE lt_item WITH  KEY ebeln = <cjs_item>-ebeln
                      zzpino = <cjs_item>-zzpino
                      matnr = <cjs_item>-matnr
                      idnlf = <cjs_item>-idnlf
                      zcolor_text = <cjs_item>-zcolor_text
                      zsize = <cjs_item>-zsize
                      znorms = <cjs_item>-znorms .
    IF sy-subrc EQ 0 ." 修改行
      <cjs_item>-menge1 = lt_item-menge1.
      <cjs_item>-menge2 = lt_item-menge2.
      <cjs_item>-menge3 = lt_item-menge3.
      <cjs_item>-menge1_bj = lt_item-menge1_bj.
      <cjs_item>-menge2_bj = lt_item-menge2_bj.
      <cjs_item>-menge3_bj = lt_item-menge3_bj.
      <cjs_item>-menge5    = lt_item-menge5.
      <cjs_item>-menge5_bj = lt_item-menge5_bj.

      " 收货超交数
      <cjs_item>-menge5 = lt_item-menge." 超交数

      "实际超交数  =  超交数 - 赠送数
      <cjs_item>-menge = lt_item-menge - <cjs_item>-menge4.
      <cjs_item>-menge_cg = lt_item-menge_cg - <cjs_item>-menge4_bj.
      <cjs_item>-amount = lt_item-amount - <cjs_item>-amount1.

      <cjs_item>-remark1 = lt_item-remark1.
      IF <cjs_item>-menge4_bj > 0.
        lv_cjbl = <cjs_item>-menge_cg / <cjs_item>-menge5_bj * 100.
        <cjs_item>-remark3 = lv_cjbl && '%'.
      ELSE.
        <cjs_item>-remark3 = lt_item-remark3.
      ENDIF.

      DELETE lt_item WHERE ebeln = <cjs_item>-ebeln
                       AND zzpino = <cjs_item>-zzpino
                       AND matnr = <cjs_item>-matnr
                       AND idnlf = <cjs_item>-idnlf
                       AND zcolor_text = <cjs_item>-zcolor_text
                       AND zsize = <cjs_item>-zsize
                       AND znorms = <cjs_item>-znorms .
    ELSE." 删除行
      CLEAR <cjs_item>-remark1.
      CLEAR <cjs_item>-remark3.
      <cjs_item>-menge = 0.
      <cjs_item>-menge_cg = 0.
      <cjs_item>-amount = 0.
    ENDIF.

    IF <cjs_item>-amount <= 0.
      <cjs_item>-del_flag = 'X'.
    ELSE.
      <cjs_item>-del_flag = ''.
    ENDIF.
  ENDLOOP.

  IF lt_item[] IS NOT INITIAL ." 新增行
    DATA: lv_afonr TYPE zafonr.

    SORT cjt_item BY afonr.
    LOOP AT cjt_item.
    ENDLOOP.

    lv_afonr = cjt_item-afonr.

    LOOP AT lt_item.
      CLEAR cjt_item.
      MOVE-CORRESPONDING lt_item TO cjt_item.
      cjt_item-afono = cjs_head-afono.

      ADD 1 TO lv_afonr.
      cjt_item-afonr = lv_afonr.
      APPEND cjt_item.
    ENDLOOP.

    SORT cjt_item BY afonr.
  ENDIF.

  CLEAR  cjs_head-menge.
  CLEAR  cjs_head-amount.

  LOOP AT cjt_item.
    cjs_head-menge = cjs_head-menge + cjt_item-menge_cg.
    cjs_head-amount = cjs_head-amount + cjt_item-amount.
  ENDLOOP.

  IF cjs_head-amount <= 0.
    cjs_head-del_flag = 'X'.
    cjs_head-status = 'D'.
  ELSE.
    cjs_head-del_flag = ''.
    cjs_head-status = 'A'.
  ENDIF.

  MODIFY zafo_head FROM cjs_head.
  MODIFY zafo_item FROM TABLE cjt_item.
  COMMIT WORK AND WAIT.
ENDFORM.


FORM frm_cjqr USING pv_ebeln.  " 超交确认
  DATA:rkt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA lv_continue TYPE char1.
  DATA:lv_cjbl TYPE menge_d.

  CHECK pv_ebeln IS NOT INITIAL.

  CALL FUNCTION 'ZAFO_PO_SUPERB'
    EXPORTING
      i_exec_env = 'POST'
      i_include  = ''
      i_ebeln    = pv_ebeln
    IMPORTING
      o_continue = lv_continue
    TABLES
      ut_item    = rkt_item[]
      ct_item    = lt_item[].

  IF lt_item[] IS INITIAL .
    LOOP AT gt_item ASSIGNING <gs_item>.
      CLEAR <gs_item>-remark1.
      CLEAR <gs_item>-remark3.
      <gs_item>-menge = 0.
      <gs_item>-menge_cg = 0.
      <gs_item>-amount = 0.
    ENDLOOP.
  ENDIF.

  LOOP AT gt_item ASSIGNING <gs_item>.
    READ TABLE lt_item WITH KEY ebeln = <gs_item>-ebeln
                                zzpino = <gs_item>-zzpino
                                matnr = <gs_item>-matnr
                                idnlf = <gs_item>-idnlf
                                zcolor_text = <gs_item>-zcolor_text
                                zsize = <gs_item>-zsize
                                znorms = <gs_item>-znorms .
    IF sy-subrc EQ 0 .
      <gs_item>-menge1 = lt_item-menge1.
      <gs_item>-menge2 = lt_item-menge2.
      <gs_item>-menge3 = lt_item-menge3.
      <gs_item>-menge1_bj = lt_item-menge1_bj.
      <gs_item>-menge2_bj = lt_item-menge2_bj.
      <gs_item>-menge3_bj = lt_item-menge3_bj.
      <gs_item>-menge5    = lt_item-menge5."收货超交数
      <gs_item>-menge5_bj = lt_item-menge5_bj." 超交基准数

      <gs_item>-menge = lt_item-menge - <gs_item>-menge4.
      <gs_item>-menge_cg = lt_item-menge_cg - <gs_item>-menge4_bj.
      <gs_item>-amount = lt_item-amount - <gs_item>-amount1.
      <gs_item>-remark1 = lt_item-remark1.
      IF <gs_item>-menge4_bj > 0.
        lv_cjbl = <gs_item>-menge_cg / <gs_item>-menge5_bj * 100.
        <gs_item>-remark3 = lv_cjbl && '%'.
      ELSE.
        <gs_item>-remark3 = lt_item-remark3.
      ENDIF.

      DELETE lt_item WHERE ebeln = <gs_item>-ebeln
                       AND zzpino = <gs_item>-zzpino
                       AND matnr = <gs_item>-matnr
                       AND idnlf = <gs_item>-idnlf
                       AND zcolor_text = <gs_item>-zcolor_text
                       AND zsize = <gs_item>-zsize
                       AND znorms = <gs_item>-znorms .
    ELSE.
      CLEAR <gs_item>-remark1.
      CLEAR <gs_item>-remark3.
      <gs_item>-menge = 0.
      <gs_item>-menge_cg = 0.
      <gs_item>-amount = 0.
    ENDIF.
  ENDLOOP.

  IF lt_item[] IS NOT INITIAL .
    DATA: lv_afonr TYPE zafonr.
    SORT gt_item BY afonr.
    LOOP AT gt_item.
    ENDLOOP.

    lv_afonr = gt_item-afonr.

    LOOP AT lt_item.
      CLEAR gs_item.
      MOVE-CORRESPONDING lt_item TO gs_item.
      PERFORM frm_set_icon USING 'A' CHANGING gs_item-icon gs_item-text.
      gs_item-text = TEXT-024."'已维护'.

      gs_item-afono = gs_head-afono.

      ADD 1 TO lv_afonr.
      gs_item-afonr = lv_afonr.

      APPEND gs_item TO gt_item.
    ENDLOOP.

    SORT gt_item BY afonr.
  ENDIF.


  " 超交重置--------------

  PERFORM frm_before_save_data.
  PERFORM frm_check_save.

  IF g_error = 'X'.
    PERFORM frm_pop_msg.
    RETURN.
  ENDIF.

  PERFORM frm_move_head_to_item.

  DATA: lv_duc TYPE char1.
  DATA: lv_commit TYPE char1.
  DATA: lv_confirm_msg TYPE char50.

  CLEAR: lv_duc , lv_commit, lv_confirm_msg.

  LOOP AT gt_item WHERE menge4 > 0.
    lv_duc = 'X'.
  ENDLOOP.

  LOOP AT gt_item WHERE amount > 0.
    lv_commit = 'X' .
  ENDLOOP.

  IF lv_duc = 'X'.
    SELECT SINGLE h~afono INTO @DATA(lref_afono)
      FROM zafo_item AS i
      INNER JOIN zafo_head AS h ON i~afono = h~afono
      WHERE h~bustyp = 'DUC01'
        AND i~afono_ref = @gs_head-afono
        AND h~status <> 'D'
        AND i~del_flag <> 'X'.
    IF sy-subrc = 0.
      CLEAR lv_duc.
    ENDIF.
  ENDIF.

  IF lv_duc = 'X' AND lv_commit = 'X'.
    lv_confirm_msg = '将生成超交扣款单和超交审批，是否继续？'.
  ELSEIF lv_duc = 'X'.
    lv_confirm_msg = '将生成超交扣款单，是否继续？'.
  ELSEIF lv_commit = 'X'.
    lv_confirm_msg = '将生成超交审批，是否继续？'.
  ENDIF.

  IF lv_confirm_msg IS NOT INITIAL.
    PERFORM frm_pop_confirm USING lv_confirm_msg.
  ENDIF.

  CHECK g_error <> 'X'.

  IF lv_duc = 'X' AND lv_commit = 'X'.
    PERFORM frm_save_data.
    PERFORM frm_cjqr_duc.
    CHECK g_error <> 'X'.
    gs_head-status = 'A'.
    PERFORM frm_commit.
  ELSEIF lv_duc = 'X'.
    PERFORM frm_save_data.
    PERFORM frm_cjqr_duc.
  ELSEIF lv_commit = 'X'.
    PERFORM frm_save_data.
    PERFORM frm_commit.
  ELSE.
    PERFORM frm_delete_data.
  ENDIF.

ENDFORM.


FORM f_set_data_cjd CHANGING cs_item TYPE zafo_sitem." 设置采购单位数量
  DATA:ls_tran_rate TYPE zmm_tran_rate.
  DATA:l_menge4_bj TYPE p DECIMALS 9.
  DATA:lv_cjbl TYPE menge_d.

  CHECK gs_head-bustyp = 'CJD01'.

  PERFORM frm_set_round USING cs_item-meins CHANGING cs_item-menge4.
  ls_tran_rate = cs_item-zmm_tran_rate.

  TRY .
      DATA(ll_menge) = 1 / ls_tran_rate.
    CATCH cx_sy_zerodivide.
      ls_tran_rate = 1.
      cs_item-zmm_tran_rate = ls_tran_rate.
  ENDTRY.

  l_menge4_bj = cs_item-menge4 / ls_tran_rate.

  CALL FUNCTION 'ROUND'
    EXPORTING
      decimals = 3
      input    = l_menge4_bj
      sign     = '+'
    IMPORTING
      output   = cs_item-menge4_bj.

  IF ls_tran_rate <> 1 AND cs_item-menge4_bj IS NOT INITIAL.
    cs_item-amount1 = cs_item-price * cs_item-menge4_bj / cs_item-peinh.
  ELSE.
    cs_item-amount1 = cs_item-price * cs_item-menge4 / cs_item-peinh / ls_tran_rate.
  ENDIF.

  cs_item-menge = cs_item-menge5 - cs_item-menge4.
  PERFORM f_set_menge_cg CHANGING <gs_item>.
  PERFORM f_set_amount CHANGING <gs_item>.

  IF cs_item-menge4_bj > 0.
    lv_cjbl = cs_item-menge_cg / cs_item-menge5_bj * 100.
    cs_item-remark3 = lv_cjbl && '%'.
  ENDIF.

ENDFORM.


FORM f_set_data_cjd_cg CHANGING cs_item TYPE zafo_sitem." 设置采购单位数量
  DATA:ls_tran_rate TYPE zmm_tran_rate.
  DATA:ls_menge4_bj TYPE menge_d.
  DATA:lv_cjbl TYPE menge_d.

  CLEAR ls_menge4_bj.

  CHECK gs_head-bustyp = 'CJD01'.

  ls_menge4_bj = cs_item-menge4_bj.

  ls_tran_rate = cs_item-zmm_tran_rate.

  TRY .
      DATA(ll_menge) = 1 / ls_tran_rate.
    CATCH cx_sy_zerodivide.
      ls_tran_rate = 1.
      cs_item-zmm_tran_rate = ls_tran_rate.
  ENDTRY.

  cs_item-menge4 = cs_item-menge4_bj * ls_tran_rate.
  PERFORM frm_set_round USING <gs_item>-meins CHANGING cs_item-menge4.

  IF ls_tran_rate <> 1 AND cs_item-menge4_bj IS NOT INITIAL.
    cs_item-amount1 = cs_item-price * cs_item-menge4_bj / cs_item-peinh.
  ELSE.
    cs_item-amount1 = cs_item-price * cs_item-menge4 / cs_item-peinh / ls_tran_rate.
  ENDIF.

  cs_item-menge = cs_item-menge5 - cs_item-menge4.
  PERFORM f_set_menge_cg CHANGING <gs_item>.
  PERFORM f_set_amount CHANGING <gs_item>.

  IF cs_item-menge4_bj > 0.
    lv_cjbl = cs_item-menge_cg / cs_item-menge5_bj * 100.
    cs_item-remark3 = lv_cjbl && '%'.
  ENDIF.
ENDFORM.


FORM frm_cgrkcjqr_trigger USING ps_head TYPE zafo_head.
  DATA:ls_text TYPE zmsg_text.
  DATA:ls_object_id TYPE zmsg_object_id.
  DATA:lt_user TYPE TABLE OF zmsg_suser WITH HEADER LINE.

  ls_object_id = ps_head-afono.
  ls_text = '采购订单:' && ps_head-remark2.

  lt_user-uname = ps_head-nenam.

  SELECT SINGLE name INTO lt_user-name1
    FROM zapp_addr WHERE person = lt_user-uname.
  APPEND lt_user.
  CLEAR lt_user.

  PERFORM frm_send_msg TABLES lt_user USING ls_text ls_object_id 'CGRKCJQR'.
ENDFORM.


FORM frm_cjqr_duc.
  DATA:cs_head TYPE  zafo_head.
  DATA:ct_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:lv_afonr TYPE zafonr.

  CLEAR lv_afonr.
  LOOP AT gt_item WHERE menge4 > 0.
    ADD 1 TO lv_afonr.
    MOVE-CORRESPONDING gt_item TO ct_item.
    ct_item-afonr = lv_afonr.
    ct_item-menge = gt_item-menge4.
    ct_item-menge_cg = gt_item-menge4_bj.
    ct_item-amount = gt_item-amount1.
    ct_item-remark1 = '入库单:' && gt_item-afono_ref.
    ct_item-afono_ref = ct_item-afono.
    APPEND ct_item.
  ENDLOOP.

  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.
  IF ct_item[] IS NOT INITIAL.
    cs_head-reason = '赠送扣款'.
    cs_head-remark1 = '超交单处理'.
    cs_head-ebeln = gs_head-ebeln.
    cs_head-remark2 = gs_head-remark2.
    cs_head-lifnr = gs_head-lifnr.
    cs_head-lifnr_name = gs_head-lifnr_name.
    CALL FUNCTION 'ZAFO_CREATE_SAVE'
      EXPORTING
        i_bustyp  = 'DUC01'
        i_zapp    = 'X'
      TABLES
        et_return = lt_return[]
        ct_item   = ct_item[]
*       CT_ITEM_PO         =
*       CT_ITEM_COST       =
      CHANGING
        cs_head   = cs_head
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.

    LOOP AT lt_return WHERE type = 'E'.
      APPEND lt_return TO  ot_return.
      g_error = 'X'.
    ENDLOOP.

    CHECK g_error <> 'X'.

    gs_head-status = 'T'.
  ENDIF.

ENDFORM.
