FUNCTION zafo_po_superb.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_EXEC_ENV) TYPE  CHAR5 DEFAULT 'DIS'
*"     VALUE(I_INCLUDE) TYPE  CHAR1 OPTIONAL
*"     VALUE(I_EBELN) TYPE  EBELN OPTIONAL
*"  EXPORTING
*"     VALUE(O_CONTINUE) TYPE  CHAR1
*"  TABLES
*"      UT_ITEM STRUCTURE  ZAFO_SITEM
*"      CT_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"----------------------------------------------------------------------
  DATA:pot_item     TYPE TABLE OF zafo_sitem WITH HEADER LINE,
       pot_item_sum TYPE TABLE OF zafo_sitem WITH HEADER LINE,
       rkt_item     TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  FIELD-SYMBOLS <fs_item> TYPE zafo_sitem.
  DATA:lv_tsdd TYPE char1." 特殊成衣订单
  DATA:lv_cjbl TYPE menge_d.
  DATA:lv_qdl TYPE menge_d.
  DATA:lv_po_basenqty TYPE menge_d.
  DATA:lv_cj_baseline TYPE menge_d.
  DATA:lv_cj_basenqty TYPE menge_d.

  g_exec_env = i_exec_env.

  REFRESH rkt_item.
  REFRESH ct_item.

  IF i_ebeln IS NOT INITIAL .
    rkt_item-ebeln = i_ebeln.
    APPEND rkt_item.
  ELSE.
    rkt_item[] = ut_item[]."入库单
  ENDIF.

  PERFORM frm_get_po TABLES rkt_item pot_item.

  SELECT i~ebeln,i~matnr,i~idnlf,i~zcolor,i~zcolor_text,i~zsize,i~znorms,
    i~price_long,i~price,i~peinh ,f~zmm_tran_rate,f~zmm_qdl,f~zmm_qrl,f~zcguser
    INTO TABLE @DATA(po_item)
   FROM zafo_item_po AS p
   LEFT JOIN zafo_item AS i ON i~afono = p~afono AND i~afonr = p~afonr
   LEFT JOIN zmmt0051 AS f ON p~matnr = f~matnr
   FOR ALL ENTRIES IN @pot_item
   WHERE p~ebeln = @pot_item-ebeln
*    AND p~ebelp = @pot_item-ebelp
    .
  IF po_item[] IS INITIAL.
    SELECT i~ebeln,i~matnr,i~idnlf,i~zcolor,i~zcolor_text,i~zsize,i~znorms,
      i~price_long,i~price,i~peinh,f~zmm_tran_rate,f~zmm_qdl,f~zmm_qrl,f~zcguser
      INTO TABLE @po_item
     FROM zafo_item AS i
     LEFT JOIN zmmt0051 AS f ON i~matnr = f~matnr
     FOR ALL ENTRIES IN @pot_item
     WHERE i~ebeln = @pot_item-ebeln
*      AND i~ebelp = @pot_item-ebelp
      .
  ENDIF.

  DATA:lv_ml   TYPE char1,
       pv_hbgz TYPE zconf_hbgz.

  CLEAR:lv_ml,pv_hbgz.

  PERFORM frm_get_cjconf TABLES pot_item.


  READ TABLE pot_item INDEX 1." 取合同第一行来做执行标准
  IF sy-subrc EQ 0 .
    IF pot_item-zcate1 = 'FA'.  " 面料不分 说明
      lv_ml = 'X'.
    ENDIF.
    PERFORM frm_get_hbgz USING pot_item CHANGING pv_hbgz.
  ENDIF.


  LOOP AT rkt_item ASSIGNING FIELD-SYMBOL(<fs_pt_item>).
    IF lv_ml = 'X'.
      CLEAR <fs_pt_item>-znorms.
    ENDIF.
    IF pv_hbgz <> 'CPO' ." 不按成衣合同合并的
      CLEAR <fs_pt_item>-zzpino.
    ENDIF.
  ENDLOOP.

  CLEAR lv_tsdd.
  LOOP AT pot_item ASSIGNING FIELD-SYMBOL(<fs_lt_item>).
    IF  <fs_lt_item>-zzpino+0(4) = 'CMND' ." 特殊订单超交处理
      lv_tsdd = 'X'.
    ENDIF.

    IF lv_ml = 'X'.
      CLEAR <fs_lt_item>-znorms.
    ENDIF.
    IF pv_hbgz <> 'CPO' ." 不按成衣合同合并的
      CLEAR <fs_lt_item>-zzpino.
    ENDIF.

  ENDLOOP.

  LOOP AT po_item ASSIGNING FIELD-SYMBOL(<fs_po_item>).
    IF lv_ml = 'X'.
      CLEAR <fs_po_item>-znorms.
    ENDIF.
  ENDLOOP.


  LOOP AT pot_item .
    CLEAR pot_item-ebelp.
    CLEAR pot_item-vbeln_va.
    CLEAR pot_item-posnr_va.
    CLEAR pot_item-zppdhd.
    CLEAR pot_item-zppflag.
    CLEAR pot_item-price_long.
    CLEAR pot_item-price.
    CLEAR pot_item-peinh.
    MOVE-CORRESPONDING pot_item TO pot_item_sum.
    COLLECT pot_item_sum.
  ENDLOOP.

  IF i_include EQ 'X'." 包含当前入库单数据
    LOOP AT pot_item_sum ASSIGNING <fs_item>.
      LOOP AT rkt_item WHERE ebeln  = <fs_item>-ebeln
                          AND zzpino = <fs_item>-zzpino
                          AND matnr  = <fs_item>-matnr
                          AND idnlf  = <fs_item>-idnlf
                          AND zcolor = <fs_item>-zcolor
                          AND zsize  = <fs_item>-zsize
                          AND znorms = <fs_item>-znorms .
        <fs_item>-menge2 = <fs_item>-menge2 + rkt_item-menge." 累计收货数
        <fs_item>-menge3 = <fs_item>-menge3 + rkt_item-menge." 本次收货数
      ENDLOOP.
    ENDLOOP.
  ENDIF.


  REFRESH gt_item.

  LOOP AT pot_item_sum.
    lv_qdl = 0.
    lv_cjbl = 0.

    READ TABLE po_item INTO DATA(ls_po_item) WITH KEY ebeln = pot_item_sum-ebeln
                                                      matnr = pot_item_sum-matnr
                                                      idnlf = pot_item_sum-idnlf
                                                      zcolor = pot_item_sum-zcolor
                                                      zsize = pot_item_sum-zsize
                                                      znorms = pot_item_sum-znorms .
    IF sy-subrc EQ 0 .

      TRY .
          DATA(ll_menge) = 1 / ls_po_item-zmm_tran_rate.
        CATCH cx_sy_zerodivide.
          ls_po_item-zmm_tran_rate = 1.
      ENDTRY.

      IF ls_po_item-zmm_qdl > 0.
        lv_qdl = ls_po_item-zmm_qdl.
      ENDIF.

      IF ls_po_item-zmm_qrl > ls_po_item-zmm_qdl .
        lv_qdl = ls_po_item-zmm_qrl.
      ENDIF.

      IF ls_po_item-zcguser IS NOT INITIAL.
        pot_item_sum-ponam = ls_po_item-zcguser.
      ENDIF.


      PERFORM frm_convert_zero USING ls_po_item-zmm_tran_rate CHANGING pot_item_sum-zmm_tran_rate.
      pot_item_sum-menge1_bj = pot_item_sum-menge1 / ls_po_item-zmm_tran_rate." 合同数量
      pot_item_sum-menge2_bj = pot_item_sum-menge2 / ls_po_item-zmm_tran_rate." 合同收货数量
      pot_item_sum-menge3_bj = pot_item_sum-menge3 / ls_po_item-zmm_tran_rate." 本次收货数量


      CLEAR: lv_cj_baseline,lv_cj_basenqty.

      IF lv_tsdd = 'X' ." 特殊订单超交处理
        lv_cj_baseline = 3.
      ENDIF.

      IF lv_cj_baseline IS INITIAL.
        PERFORM frm_set_baseline USING pot_item_sum  CHANGING lv_cj_baseline lv_cj_basenqty.
      ENDIF.

      CLEAR lv_po_basenqty." 超交计算基数
      IF pot_item_sum-menge1_bj < lv_qdl." 合同数小于等于起订量的，起订量由报价单决定
        lv_po_basenqty = lv_qdl.
      ELSE.
        lv_po_basenqty = pot_item_sum-menge1_bj.
      ENDIF.

      " 超交数和比例
      CASE g_exec_env.
        WHEN 'POST'." 超过超交基线的部分提交，以采购合同数为基准
          IF lv_cj_basenqty IS NOT INITIAL." 按固定数
            pot_item_sum-menge_cg = pot_item_sum-menge2_bj - lv_po_basenqty - lv_cj_basenqty .
          ELSE." 按百分比
            pot_item_sum-menge_cg = pot_item_sum-menge2_bj - lv_po_basenqty * ( 100 + lv_cj_baseline ) / 100 .
          ENDIF.
        WHEN OTHERS.
          pot_item_sum-menge_cg = pot_item_sum-menge2_bj - lv_po_basenqty .
      ENDCASE.

      pot_item_sum-menge = pot_item_sum-menge_cg * ls_po_item-zmm_tran_rate." 超交数量

      pot_item_sum-menge5 = pot_item_sum-menge." 超交数量保存
      pot_item_sum-menge5_bj = lv_po_basenqty." 超交基准数保存

      pot_item_sum-price_long = ls_po_item-price_long.
      pot_item_sum-price = ls_po_item-price.
      pot_item_sum-peinh = ls_po_item-peinh.
      pot_item_sum-amount = ls_po_item-price_long * pot_item_sum-menge_cg.

      lv_cjbl = pot_item_sum-menge_cg / lv_po_basenqty * 100.

      IF pot_item_sum-amount <= 0." 赠送的，无单价
        lv_cjbl = 0.
      ENDIF.

      IF lv_cjbl > 0 .
        IF lv_cj_basenqty IS NOT INITIAL .
          pot_item_sum-remark1 = lv_cj_basenqty && pot_item_sum-bprme.
        ELSE.
          pot_item_sum-remark1 = lv_cj_baseline && '%'.
        ENDIF.
        pot_item_sum-remark3 = lv_cjbl && '%'.
        APPEND pot_item_sum TO ct_item.
      ENDIF.

      CLEAR pot_item_sum.

    ENDIF.

  ENDLOOP.


  CLEAR o_continue.

  IF ct_item[] IS NOT INITIAL.
    CLEAR sy-ucomm.

    CASE g_exec_env.
      WHEN 'POST'.

      WHEN OTHERS.
        LOOP AT ct_item.
          READ TABLE ut_item WITH KEY matnr = ct_item-matnr
                                     zcolor_text = ct_item-zcolor_text
                                     zsize = ct_item-zsize
                                     znorms = ct_item-znorms .
          APPEND ct_item TO gt_item.
        ENDLOOP.
        CALL SCREEN 8001 STARTING AT 10 5 ENDING AT 150 25.
        IF g_confirm EQ 'X' .
          o_continue = 'X'.
        ENDIF.
    ENDCASE.
  ELSE.
    o_continue = 'X'.
  ENDIF.

ENDFUNCTION.


DATA: gt_cj_01 TYPE TABLE OF zconf_cgcj_01 WITH HEADER LINE.
DATA: gt_cj_02 TYPE TABLE OF zconf_cgcj_02 WITH HEADER LINE.
DATA: gt_cj_03 TYPE TABLE OF zconf_cgcj_03 WITH HEADER LINE.

" 配置逻辑:
" 按大类 -> 中类 -> 小类 -> 供应商 -> 数量 取超交线
FORM frm_get_cjconf TABLES pt_item STRUCTURE zafo_sitem.

  REFRESH: gt_cj_01,gt_cj_02,gt_cj_03.

  SELECT * INTO TABLE @gt_cj_01
    FROM zconf_cgcj_01
    FOR ALL ENTRIES IN @pt_item
    WHERE zcate1 = @pt_item-zcate1.

  SELECT * INTO TABLE @DATA(lgt_cj_02)
    FROM zconf_cgcj_02
    FOR ALL ENTRIES IN @pt_item
    WHERE lifnr = @pt_item-lifnr.

  SELECT * APPENDING TABLE @lgt_cj_02
    FROM zconf_cgcj_02
    FOR ALL ENTRIES IN @gt_cj_01
    WHERE conf_line1 = @gt_cj_01-conf_line1
    AND lifnr = ''.

  IF lgt_cj_02[] IS NOT INITIAL .
    LOOP AT lgt_cj_02 INTO DATA(ls_cj_02).
      READ TABLE gt_cj_01 WITH KEY conf_line1 = ls_cj_02-conf_line1.
      IF sy-subrc EQ 0 .
        APPEND ls_cj_02 TO gt_cj_02.
      ENDIF.
    ENDLOOP.

    SELECT * INTO TABLE @gt_cj_03
      FROM zconf_cgcj_03
      FOR ALL ENTRIES IN @gt_cj_02
      WHERE conf_line1 = @gt_cj_02-conf_line1
      AND conf_line2 = @gt_cj_02-conf_line2.
  ENDIF.

ENDFORM.


FORM frm_get_hbgz USING ps_item TYPE zafo_sitem CHANGING pv_hbgz.
  CLEAR:gt_cj_01.

  READ TABLE gt_cj_01 WITH KEY zcate1 = ps_item-zcate1 zcate2 = ps_item-zcate2 zcate3 = ps_item-zcate3." 小类
  IF sy-subrc <> 0 .
    READ TABLE gt_cj_01 WITH KEY zcate1 = ps_item-zcate1 zcate2 = ps_item-zcate2." 小类
    IF sy-subrc <> 0 .
      READ TABLE gt_cj_01 WITH KEY zcate1 = ps_item-zcate1." 大类
    ENDIF.
  ENDIF.
  pv_hbgz = gt_cj_01-zconf_hbgz.
ENDFORM.


FORM frm_set_baseline USING ps_item TYPE zafo_sitem CHANGING pv_baseline pv_basenqty.
  IF g_exec_env EQ 'DIS'.
    pv_baseline = - 100.
    RETURN.
  ENDIF.

  CLEAR:gt_cj_01,gt_cj_02,gt_cj_03.

  READ TABLE gt_cj_01 WITH KEY zcate1 = ps_item-zcate1 zcate2 = ps_item-zcate2 zcate3 = ps_item-zcate3." 小类
  IF sy-subrc <> 0 .
    READ TABLE gt_cj_01 WITH KEY zcate1 = ps_item-zcate1 zcate2 = ps_item-zcate2." 小类
    IF sy-subrc <> 0 .
      READ TABLE gt_cj_01 WITH KEY zcate1 = ps_item-zcate1." 大类
    ENDIF.
  ENDIF.

  READ TABLE gt_cj_02 WITH KEY conf_line1 = gt_cj_01-conf_line1 lifnr = ps_item-lifnr." 按供应商的
  IF sy-subrc <> 0 .
    READ TABLE gt_cj_02 WITH KEY conf_line1 = gt_cj_01-conf_line1 lifnr = ''." 不按按供应商的
  ENDIF.

  IF gt_cj_02-conf_line2 IS NOT INITIAL .
    LOOP AT gt_cj_03 WHERE conf_line1 = gt_cj_01-conf_line1 AND conf_line2 = gt_cj_02-conf_line2
                       AND start_menge < ps_item-menge1_bj AND end_menge >= ps_item-menge1_bj.
      IF gt_cj_03-gd_menge > 0.
        pv_basenqty =  gt_cj_03-gd_menge.
      ENDIF.
      IF gt_cj_03-percent > 0 .
        pv_baseline = gt_cj_03-percent.
      ENDIF.
    ENDLOOP.
    IF pv_baseline IS INITIAL .
      LOOP AT gt_cj_03 WHERE conf_line1 = gt_cj_01-conf_line1 AND conf_line2 = gt_cj_02-conf_line2
                         AND start_menge < ps_item-menge1_bj AND end_menge = 0.
        IF gt_cj_03-gd_menge > 0.
          pv_basenqty =  gt_cj_03-gd_menge.
        ENDIF.
        IF gt_cj_03-percent > 0 .
          pv_baseline = gt_cj_03-percent.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF pv_baseline = 0 AND pv_basenqty = 0.
      pv_baseline = gt_cj_02-percent.
    ENDIF.
  ENDIF.

  IF pv_baseline = 0 AND pv_basenqty = 0.
    pv_baseline = gt_cj_01-percent.
  ENDIF.

ENDFORM.


FORM frm_get_po TABLES pt_item STRUCTURE zafo_sitem
                       ct_item STRUCTURE zafo_sitem.
  SELECT k~bukrs,
       h~bukrs_name,
       h~remark1 AS remark2,"采购编号
       h~afnam," 业务员
       h~ernam AS ponam,"采购合同创建人
       p~werks,
       k~lifnr,
       a~zname1 AS lifnr_name,
       h~remark1 AS remark1,
       k~ebeln,
*       p~ebelp,
       p~zvbeln AS vbeln_va,
       p~zposnr AS posnr_va,
       p~zzpino,
       p~zppdhd,
       m~zcate1,
       m~zcate2,
       m~zcate3,
       p~matnr,
       p~idnlf,
       p~txz01 AS maktx,
       p~zcolor,
       p~zcolor_text,
       p~zsize,
       p~znorms,
       p~zppflag,
       p~meins,
       p~bprme,
       p~zkostl AS kostl,
       p~kzwi1 AS amount," 合同金额
       k~waers,
       t~menge AS menge1," 合同数量
       t~wemng AS menge2 " 收货数量
 INTO CORRESPONDING FIELDS OF TABLE @ct_item
 FROM ekko AS k
    INNER JOIN zafo_head AS h ON k~ebeln = h~afono
    INNER JOIN ekpo AS p ON k~ebeln = p~ebeln
    INNER JOIN zmmt0010 AS m ON m~matnr = p~matnr
    INNER JOIN eket AS t ON p~ebeln = t~ebeln AND p~ebelp = t~ebelp
    INNER JOIN zscmt0010 AS a ON k~lifnr = a~partner AND k~bukrs = a~bukrs
 FOR ALL ENTRIES IN @pt_item
 WHERE k~ebeln = @pt_item-ebeln
*   AND p~ebelp = @pt_item-ebelp
    .

ENDFORM.
