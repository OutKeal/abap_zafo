*&---------------------------------------------------------------------*
*& 包含               ZAFO_F03
*&---------------------------------------------------------------------*

FORM  frm_get_ref TABLES ct_item STRUCTURE zafo_sitem
                          ct_item_po STRUCTURE zafo_sitem_po
                   USING busref.
  PERFORM frm_auth_check.

  CASE busref."QTVX
    WHEN 'Z'. "申请单转采购
      PERFORM frm_ref_z TABLES ct_item.

    WHEN 'Q'. "徳誉采购
      PERFORM frm_ref_q TABLES ct_item.

    WHEN 'D'. "BOM 采购
      PERFORM frm_ref_d TABLES ct_item ct_item_po.

    WHEN 'L'. "BOM采购合同变更
      PERFORM frm_ref_l TABLES ct_item ct_item_po.

    WHEN 'J'. "大货经销采购
      PERFORM frm_ref_j TABLES ct_item.

    WHEN 'A'. "采购合同收货
      PERFORM frm_ref_a TABLES ct_item.

    WHEN 'M'. "海外材料收货
      PERFORM frm_ref_m TABLES ct_item.
    WHEN 'G'. "合格入库
      PERFORM frm_ref_g TABLES ct_item.

    WHEN 'N'. "大货材料采购入库冲销/退货
      PERFORM frm_ref_n TABLES ct_item.
    WHEN 'P'. "库存物料采购入库冲销
      PERFORM frm_ref_p TABLES ct_item.
    WHEN 'I'. "生产退料
      PERFORM frm_ref_i TABLES ct_item.
    WHEN 'R'. "出库冲销
      PERFORM frm_ref_r TABLES ct_item.

    WHEN 'E'. "产前样领料
      PERFORM frm_ref_e TABLES ct_item.
    WHEN 'F'. "生产发料
      PERFORM frm_ref_f TABLES ct_item.
    WHEN 'H'. "领料转移单
      PERFORM frm_ref_h TABLES ct_item.

    WHEN 'B'. "库存
      PERFORM frm_ref_b TABLES ct_item.
    WHEN 'C'."订单库存
      PERFORM frm_ref_c TABLES ct_item.
    WHEN 'K'."常规库存 + 订单库存
      PERFORM frm_ref_k TABLES ct_item.
    WHEN 'Y'."仓库调拨单
      PERFORM frm_ref_y TABLES ct_item.
    WHEN 'W'."半成品委外加工发料
      PERFORM frm_ref_w TABLES ct_item.
    WHEN 'U'."工序委外采购申请
      PERFORM frm_ref_u TABLES ct_item ct_item_po.

    WHEN 'O'."清关业务
      PERFORM frm_ref_o TABLES ct_item.

    WHEN 'S'."销售分销发货业务
      PERFORM frm_ref_s TABLES ct_item.

  ENDCASE.
ENDFORM.


FORM frm_get_barcode_dis TABLES ct_barcode_dis STRUCTURE zmm_sbarcode USING p_typ.
  DATA lv_where TYPE char100.
  REFRESH ct_barcode_dis.
  CASE p_typ.
    WHEN 'R1006' OR 'R1003'.
      lv_where = `rk_scan_flag = 'X' AND afono_rk = ''`.
    WHEN 'R4011' OR 'R6005'.
      lv_where = `afono_rk <> '' AND ck_scan_flag = 'X' AND afono_ck = ''`.
  ENDCASE.

  CHECK lv_where IS NOT INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE @ct_barcode_dis
    FROM zmm_barcode
    WHERE status <> 'D'
    AND freight_path LIKE 'GTA%'
    AND bukrs IN @s_werks
    AND (lv_where).
ENDFORM.


FORM frm_ref_a TABLES ct_item STRUCTURE zafo_sitem.
  DATA:ls_menge_sh TYPE menge_d,
       ls_menge_th TYPE menge_d.
  DATA lv_retpo TYPE retpo.
  RANGES: lr_bustyp FOR zafo_head-bustyp.


  CLEAR lv_retpo.
  IF p_typ = 'R1005'.
    lv_retpo = 'X'.
  ENDIF.

  CLEAR lr_bustyp.
  REFRESH lr_bustyp.
  IF gs_bustyp-bustyp_ref IS NOT INITIAL.
    lr_bustyp-sign = 'I'.
    lr_bustyp-option = 'EQ'.
    lr_bustyp-low = gs_bustyp-bustyp_ref.
    APPEND lr_bustyp.
  ENDIF.
  IF gs_bustyp-bustyp_bak IS NOT INITIAL.
    DATA:BEGIN OF lt_bustyp OCCURS 0,
           bustyp LIKE zafo_bustype-bustyp,
         END OF lt_bustyp.

    SPLIT gs_bustyp-bustyp_bak AT ',' INTO TABLE lt_bustyp.
    LOOP AT lt_bustyp.
      lr_bustyp-sign = 'I'.
      lr_bustyp-option = 'EQ'.
      lr_bustyp-low = lt_bustyp-bustyp.
      APPEND lr_bustyp.
    ENDLOOP.
  ENDIF.

  SELECT
    k~bukrs,
    k~ebeln,
    k~lifnr,
    k~waers,
    k~frgrl AS text,
    f~remark1,
    f~ernam AS afnam," 合同创建人
    p~ebelp,
    p~matnr,
    p~idnlf,
    coalesce( m~vbeln , p~zvbeln ) AS vbeln_va,
    coalesce( m~posnr , p~zposnr ) AS posnr_va,
    p~txz01  AS maktx,
    p~meins,
    p~bprme,
    p~werks,
    p~lgort,
    CAST( CAST( p~peinh AS CHAR ) AS NUMC( 5 ) ) AS peinh,
    p~zcolor,
    p~zcolor_text,
    p~zsize,
    m~zzpino,
    c~name1 AS zshd,
    p~zppdhd,
    p~znorms,
    p~zppflag,
    p~zkostl AS kostl,
    p~zanln1 AS anln1,
    p~kzwi1 AS amount,
    CAST( p~bpumn AS CHAR ) AS zmm_tran_rate ,
    a~zname1 AS lifnr_name,
    t~menge AS menge1,
    t~wemng AS menge2,
    ( t~menge - t~wemng ) AS menge3,   " 需收货采购数量   合同数- 已收数
    s~ktext AS kostl_name
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    FROM ekko AS k
    INNER JOIN zafo_head AS f ON k~ebeln = f~afono
    INNER JOIN ekpo AS p ON k~ebeln = p~ebeln
    INNER JOIN eket AS t ON p~ebeln = t~ebeln AND p~ebelp = t~ebelp
    LEFT JOIN ztpp0089 AS m ON m~zppdhd = p~zppdhd
    LEFT JOIN ztpp0093 AS c ON m~zwerks = c~zwerks
    INNER JOIN zscmt0010 AS a ON k~lifnr = a~partner AND k~bukrs = a~bukrs
    LEFT JOIN cskt AS s ON p~zkostl = s~kostl AND s~spras = @sy-langu
    WHERE p~werks   IN @s_werks
      AND f~bustyp  IN @lr_bustyp
      AND f~remark1 IN @s_potext
      AND p~matnr   IN @s_matnr
      AND p~idnlf   IN @s_idnlf
      AND p~mtart   IN @s_mtart
      AND p~zppdhd  IN @s_zppdhd
      AND p~zzpino  IN @s_zzpino
      AND k~ebeln   IN @s_ebeln
      AND k~bsart   IN @s_bsart
      AND k~ekgrp   IN @s_ekgrp
      AND k~lifnr   IN @s_lifnr
      AND k~ernam   IN @s_pernam
      AND a~zname1  IN @s_zname1
*     AND ( p~webre = 'X' OR repos = '' )
      AND p~loekz = '  '
      AND p~retpo = @lv_retpo.

  CHECK ct_item[] IS NOT INITIAL.

  IF p_typ = 'R1001' OR p_typ = 'R1003' OR p_typ = 'R1006'
    OR p_typ = 'R1013' OR p_typ = 'R1014' OR p_typ = 'R1015'
    OR p_typ = 'R1016' OR p_typ = 'R1019'
    OR p_typ = 'R1011' OR p_typ = 'R1020'.
    " 已收货数量
    SELECT i~* FROM zafo_item AS i
       INNER JOIN zafo_head AS h ON h~afono = i~afono
       FOR ALL ENTRIES IN @ct_item
      WHERE i~ebeln = @ct_item-ebeln
       AND i~ebelp = @ct_item-ebelp
       AND ( bustyp = 'R1001'OR bustyp = 'R1003'OR bustyp = 'R1006'
          OR bustyp = 'R1013' OR bustyp = 'R1014' OR bustyp = 'R1015'
          OR bustyp = 'R1016' OR bustyp = 'R1019'
          OR bustyp = 'R1011' OR bustyp = 'R1020'
          OR bustyp = 'ASN02' OR bustyp = 'ASN03' )
       AND ( h~status <> 'D' AND h~status <> 'F' )
       AND i~item_status <> 'F'
       INTO TABLE @DATA(lt_afo_item).

    " 采购质检不合格数量
    SELECT i~*
      FROM zafo_item AS i
      INNER JOIN zafo_head AS h  ON h~afono = i~afono
     FOR ALL ENTRIES IN @ct_item
     WHERE i~ebeln = @ct_item-ebeln
       AND i~ebelp = @ct_item-ebelp
       AND ( bustyp = 'R1002')
       AND ( h~status = 'S' OR h~status = 'T')
     INTO TABLE @DATA(lt_afo_item3).

    " 采购退货单的退货数量
    SELECT i~* FROM zafo_item AS i
       INNER JOIN zafo_head AS h
       ON h~afono = i~afono
       FOR ALL ENTRIES IN @ct_item
       WHERE i~ebeln = @ct_item-ebeln
       AND i~ebelp = @ct_item-ebelp
       AND ( h~bustyp = 'R1004' OR h~bustyp = 'R1005')
       AND h~status = 'S'
       INTO TABLE @DATA(lt_afo_item4).
  ENDIF.

  SELECT a~afono,a~afonr,b~ebelp,a~price_long,a~price,a~peinh
    INTO TABLE @DATA(lt_price)
    FROM zafo_item AS a
    LEFT JOIN zafo_item_po AS b ON a~afono = b~afono AND a~afonr = b~afonr
    FOR ALL ENTRIES IN @ct_item
    WHERE a~afono = @ct_item-ebeln.

  SELECT afono,bustyp
    INTO TABLE @DATA(lt_po_bustyp)
    FROM zafo_head
    FOR ALL ENTRIES IN @ct_item
    WHERE afono = @ct_item-ebeln.

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.

  REFRESH lt_matnr.
  LOOP AT ct_item.
    CLEAR lt_matnr.
    lt_matnr-matnr = ct_item-matnr.
    lt_matnr-meins = ct_item-meins.
    COLLECT lt_matnr.
  ENDLOOP.

  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.


  LOOP AT ct_item.

    IF ct_item-zcolor IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ZCOL1_OUTPUT'
        EXPORTING
          input  = ct_item-zcolor
        IMPORTING
          output = ct_item-zcolor_text.
    ENDIF.

    IF p_typ <> 'R1005'.
      CLEAR ct_item-lgort.
    ENDIF.

    IF lt_afo_item[] IS NOT INITIAL.
      CLEAR ls_menge_sh.
      CLEAR ls_menge_th.

      LOOP AT lt_afo_item INTO DATA(ls_afo_item) WHERE ebeln = ct_item-ebeln
                                                   AND ebelp = ct_item-ebelp.
        ls_menge_sh = ls_menge_sh + ls_afo_item-menge. " 已收货数量
      ENDLOOP.

      LOOP AT lt_afo_item3 INTO DATA(ls_afo_item3) WHERE ebeln = ct_item-ebeln
                                                     AND ebelp = ct_item-ebelp.
        ls_menge_th = ls_menge_th + ls_afo_item3-menge3. "  不合格数
      ENDLOOP.

      LOOP AT lt_afo_item4 INTO DATA(ls_afo_item4) WHERE ebeln = ct_item-ebeln
                                                     AND ebelp = ct_item-ebelp.
        "ls_menge_sh = ls_menge_sh - ls_afo_item4-menge. " 已收货数量
        ls_menge_th = ls_menge_th + ls_afo_item4-menge. "  退货
      ENDLOOP.

      ct_item-menge2 = ls_menge_sh ." 已收货数量
      ct_item-menge4 = ls_menge_th. " 退货数量
      ct_item-menge3 = ct_item-menge1 - ( ct_item-menge2 - ls_menge_th )." 未清数量

    ENDIF.


    READ TABLE lt_matnr WITH KEY matnr = ct_item-matnr.
    IF sy-subrc EQ 0 .
      ct_item-zcate1 = lt_matnr-zcate1.
      ct_item-zcate2 = lt_matnr-zcate2.
      ct_item-zcate3 = lt_matnr-zcate3.
      ct_item-bprme = lt_matnr-bprme.
      ct_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    ct_item-menge3_bj = ct_item-menge3 / ct_item-zmm_tran_rate.

    READ TABLE lt_price INTO DATA(ls_price) WITH KEY afono = ct_item-ebeln ebelp = ct_item-ebelp.
    IF sy-subrc EQ 0.
      ct_item-price_long = ls_price-price_long.
      ct_item-price = ls_price-price.
      ct_item-peinh = ls_price-peinh.
    ELSE.
      READ TABLE lt_price INTO ls_price WITH KEY afono = ct_item-ebeln afonr = ct_item-ebelp.
      IF sy-subrc EQ 0.
        ct_item-price_long = ls_price-price_long.
        ct_item-price = ls_price-price.
        ct_item-peinh = ls_price-peinh.
      ELSE.
        ct_item-price_long = ct_item-amount / ct_item-menge1 * ct_item-zmm_tran_rate.
        PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND CHANGING ct_item.
      ENDIF.
    ENDIF.

    IF ct_item-text = 'X'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-009."'未审批'.
    ELSE.
      IF ct_item-menge3 <= 0.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'S' CHANGING ct_item-icon ct_item-text .
      ELSE.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
      ENDIF.
    ENDIF.

    IF p_typ = '1009' AND ct_item-menge2 > 0." 费用采购签收, 数量大于就完成(清除少收的情况)
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'S' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    IF  ( ct_item-menge2 - ct_item-menge4 ) / ct_item-menge1  > ( 95 / 100 )." 有效收货 95% 以上算完成
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'S' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    MODIFY ct_item.

  ENDLOOP.

  IF p_typ = 'R1013' OR p_typ = 'R1015' OR p_typ = 'R1019'. " 代收去除产前样
    DELETE ct_item WHERE zppflag = 'X'.
  ENDIF.

  IF p_fin = ''.
    DELETE ct_item WHERE icon = icon_complete .
  ENDIF.
ENDFORM.


FORM frm_ref_p TABLES ct_item STRUCTURE zafo_sitem. " 库存采购退货
  RANGES:s_clabs FOR mchb-clabs.

  CLEAR s_clabs.
  IF p_fin = ''.
    s_clabs-sign = 'I'.
    s_clabs-option = 'NE'.
    s_clabs-low = 0.
    APPEND s_clabs.
  ENDIF.

  DATA:ls_menge TYPE menge_d.
  DATA: lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  CLEAR lt_item[].

  SELECT
    k~bukrs,
    k~ebeln,
    k~lifnr,
    k~waers,
    k~frgrl AS text,
    f~remark1,
    p~ebelp,
    p~matnr,
    p~idnlf,
    p~zvbeln AS vbeln_va,
    p~zposnr AS posnr_va,
    p~txz01  AS maktx,
    p~meins,
    p~bprme,
    p~werks,
*     p~lgort,
    CAST( CAST( p~peinh AS CHAR ) AS NUMC( 5 ) ) AS peinh,
    p~zcolor,
    p~zcolor_text,
    p~zsize,
    p~zzpino,
    p~zppdhd,
    p~znorms,
    p~zppflag,
    p~afnam,
    p~zkostl AS kostl,
    p~zanln1 AS anln1,
    p~kzwi1 AS amount,
    CAST( p~bpumn AS CHAR ) AS zmm_tran_rate ,
    a~zname1 AS lifnr_name,
    t~menge AS menge1,"计划数量
    t~wemng AS menge2"收到货物数量
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    FROM ekko AS k
    INNER JOIN zafo_head AS f ON k~ebeln = f~afono
    INNER JOIN ekpo AS p  ON k~ebeln = p~ebeln
    INNER JOIN eket AS t ON p~ebeln = t~ebeln AND p~ebelp = t~ebelp
    INNER JOIN zscmt0010 AS a  ON k~lifnr = a~partner AND k~bukrs = a~bukrs
    WHERE  p~werks IN @s_werks
    AND p~matnr IN @s_matnr
    AND p~idnlf IN @s_idnlf
    AND p~mtart IN @s_mtart
    AND p~zppdhd IN @s_zppdhd
    AND p~zzpino IN @s_zzpino
    AND k~ebeln IN @s_ebeln
    AND k~bsart IN @s_bsart
    AND k~ekgrp IN @s_ekgrp
    AND k~lifnr IN @s_lifnr
    AND k~ernam IN @s_pernam
    AND a~zname1 IN @s_zname1
    AND p~loekz = '  '.

  CHECK sy-subrc EQ 0.

  SELECT
    c~matnr,
    c~werks,
    c~werks AS bukrs,
    c~werks AS umwrk,
    c~lgort,
    c~charg,
    clabs AS menge4,
    a~idnlf,
    a~maktx,
    a~meins,
    a~bprme,
    a~zmm_tran_rate,
    b~sgt_scat,
    b~zcolor,
    b~zcolor_text,
    b~zsize,
    b~znorms,
    b~zshelves,
    b~zvat_nub
*     B~LIFNR
    FROM mchb AS c
       INNER JOIN zvmat AS a  ON c~matnr =  a~matnr
       LEFT JOIN zmch1 AS b ON c~charg = b~charg
       INTO TABLE @DATA(lt_mchb)
       FOR ALL ENTRIES IN @ct_item
         WHERE a~matnr = @ct_item-matnr
           AND a~maktx = @ct_item-maktx
           AND c~werks = @ct_item-werks
           AND a~idnlf = @ct_item-idnlf
           AND c~lgort IN @s_lgort
           AND a~maktx = @ct_item-maktx
           AND clabs > 0.


  SELECT afono,afonr,ebelp,price_long,price,peinh
    INTO TABLE @DATA(lt_price)
    FROM zafo_item
    FOR ALL ENTRIES IN @ct_item
    WHERE afono = @ct_item-ebeln.


  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.
  LOOP AT ct_item.
    lt_matnr-matnr = ct_item-matnr.
    lt_matnr-meins = ct_item-meins.
    COLLECT lt_matnr.
  ENDLOOP.
  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.

  LOOP AT ct_item.

    READ TABLE lt_matnr WITH KEY matnr = ct_item-matnr.
    IF sy-subrc EQ 0 .
      ct_item-zcate1 = lt_matnr-zcate1.
      ct_item-zcate2 = lt_matnr-zcate2.
      ct_item-zcate3 = lt_matnr-zcate3.
      ct_item-bprme = lt_matnr-bprme.
      ct_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    ct_item-menge1_bj = ct_item-menge1 / ct_item-zmm_tran_rate .
    ct_item-menge2_bj = ct_item-menge2 / ct_item-zmm_tran_rate.


    READ TABLE lt_price INTO DATA(ls_price) WITH KEY afono = ct_item-ebeln ebelp = ct_item-ebelp.
    IF sy-subrc EQ 0.
      ct_item-price_long = ls_price-price_long.
      ct_item-price = ls_price-price.
      ct_item-peinh = ls_price-peinh.
    ELSE.
      ct_item-price_long = ct_item-amount / ct_item-menge1 * ct_item-zmm_tran_rate.
      PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND
         CHANGING ct_item.
    ENDIF.

    LOOP AT lt_mchb INTO DATA(ls_mchb) WHERE matnr  = ct_item-matnr
                                              AND idnlf  = ct_item-idnlf
                                              AND werks  = ct_item-werks
                                              AND zcolor = ct_item-zcolor
                                              AND zsize  = ct_item-zsize
                                              AND znorms = ct_item-znorms .
      ct_item-zvat_nub = ls_mchb-zvat_nub.
      ct_item-charg = ls_mchb-charg.
      ct_item-lgort = ls_mchb-lgort.
      ct_item-zshelves = ls_mchb-zshelves.
      ct_item-menge4 = ls_mchb-menge4.

      ct_item-menge4_bj = ls_mchb-menge4 / ct_item-zmm_tran_rate .
      ct_item-menge3 = ls_mchb-menge4.
      ct_item-menge3_bj = ls_mchb-menge4 / ct_item-zmm_tran_rate.

      IF ct_item-text = 'X'.
        ct_item-icon = icon_led_red.
        ct_item-text = TEXT-009."'未审批'.
      ELSE.
        IF ct_item-menge2 <= 0.
          PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
            USING 'F' CHANGING ct_item-icon ct_item-text .
        ELSE.
          PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
            USING '' CHANGING ct_item-icon ct_item-text .
        ENDIF.
      ENDIF.
      APPEND ct_item TO lt_item.

    ENDLOOP.

  ENDLOOP.

  ct_item[] = lt_item[].

  IF p_fin = ''.
    DELETE ct_item WHERE icon NE icon_led_inactive .
  ENDIF.

  PERFORM frm_get_h_price TABLES ct_item.
ENDFORM.


FORM frm_ref_r TABLES ct_item STRUCTURE zafo_sitem." 领用退库
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:lv_bustyp TYPE zafo_bustyp.

  CHECK g_bustyp IS NOT INITIAL.

  lv_bustyp = g_bustyp+0(3) && '0' && g_bustyp+4(1).

  SELECT
    h~status AS icon,
    i~*
    FROM zafo_head AS h
    INNER JOIN zafo_item AS i ON h~afono =  i~afono
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    WHERE bustyp = @lv_bustyp AND h~status IN ( 'S','T' )
    AND h~werks = @p_werks
    AND h~afono IN @s_afono
    AND h~ernam IN @s_ernam
    AND ( i~lgort IN @s_lgort OR i~umcha IN @s_lgort )
    AND i~zppdhd IN @s_zppdhd
    AND i~zzpino IN @s_zzpino
    AND h~erdat IN @s_erdat
    AND h~budat IN @s_budat
    AND i~del_flag = ''.

  CHECK ct_item[] IS NOT INITIAL.

  SELECT
    h~status AS icon,
    i~*
    FROM zafo_head AS h INNER JOIN zafo_item AS i
    ON h~afono =  i~afono
    INTO CORRESPONDING FIELDS OF TABLE @lt_item
    FOR ALL ENTRIES IN @ct_item
    WHERE bustyp = @g_bustyp AND h~status IN ( 'A','S' )
    AND i~afono_ref = @ct_item-afono
    AND i~afonr_ref = @ct_item-afonr
    AND i~del_flag = ''
    .

  LOOP AT ct_item.

    ct_item-afono_ref = ct_item-afono.
    ct_item-afonr_ref = ct_item-afonr.

    IF ct_item-umwrk IS NOT INITIAL.
      ct_item-bukrs = ct_item-umwrk.
    ENDIF.

    CLEAR ct_item-umcha .
    ct_item-menge2 = ct_item-menge."出库数量

    LOOP AT lt_item  WHERE afono_ref = ct_item-afono AND afonr_ref = ct_item-afonr.
      ct_item-menge3 = lt_item-menge + ct_item-menge3.
    ENDLOOP.

    ct_item-menge = ct_item-menge2 -  ct_item-menge3.

    IF ct_item-menge > 0.
      ct_item-icon = ''.
    ELSE.
      ct_item-icon = 'S'.
    ENDIF.

    CASE ct_item-icon.
      WHEN ''.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING ''  CHANGING ct_item-icon ct_item-text .
      WHEN 'S'.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
         USING 'S'  CHANGING ct_item-icon ct_item-text .
      WHEN OTHERS.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
         USING 'E'  CHANGING ct_item-icon ct_item-text .
    ENDCASE.

    MODIFY ct_item.
  ENDLOOP.

  IF p_fin IS INITIAL.
    DELETE ct_item WHERE icon = icon_complete.
  ENDIF.
ENDFORM.


FORM frm_ref_n TABLES ct_item STRUCTURE zafo_sitem. " 大货材料采购退货/入库冲销
  DATA:ls_menge TYPE menge_d.
  DATA: lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  CLEAR lt_item[].

  SELECT
    k~bukrs,
    k~ebeln,
    k~lifnr,
    k~waers,
    k~frgrl AS text,
    f~remark1,
    p~ebelp,
    p~matnr,
    p~idnlf,
    p~zvbeln AS vbeln_va,
    p~zposnr AS posnr_va,
    p~txz01  AS maktx,
    p~meins,
    p~bprme,
    p~werks,
    CAST( CAST( p~peinh AS CHAR ) AS NUMC( 5 ) ) AS peinh,
    p~zcolor,
    p~zcolor_text,
    p~zsize,
    p~znorms,
    p~zppflag,
    p~zzpino,
    p~zppdhd,
    p~afnam,
    p~zkostl AS kostl,
    p~zanln1 AS anln1,
    p~kzwi1 AS amount,
    CAST( p~bpumn AS CHAR ) AS zmm_tran_rate ,
    a~zname1 AS lifnr_name,
    t~menge AS menge1,"计划数量
    e~menge AS menge2,"收到货物数量
    e~gjahr AS lfgja,
    e~belnr AS lfbnr,
    e~buzei AS lfpos
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    FROM ekko AS k
    INNER JOIN zafo_head AS f ON k~ebeln = f~afono
    INNER JOIN ekpo AS p ON k~ebeln = p~ebeln
    INNER JOIN eket AS t ON p~ebeln = t~ebeln AND p~ebelp = t~ebelp
    INNER JOIN ekbe AS e ON p~ebeln = e~ebeln AND p~ebelp = e~ebelp
    INNER JOIN zscmt0010 AS a ON k~lifnr = a~partner AND k~bukrs = a~bukrs
    WHERE  p~werks IN @s_werks
    AND p~zzpino IN @s_zzpino
    AND p~zppdhd IN @s_zppdhd
    AND p~matnr IN @s_matnr
    AND p~idnlf IN @s_idnlf
    AND p~mtart IN @s_mtart
    AND k~ebeln IN @s_ebeln
    AND k~bsart IN @s_bsart
    AND k~ekgrp IN @s_ekgrp
    AND k~lifnr IN @s_lifnr
    AND k~ernam IN @s_pernam
    AND a~zname1 IN @s_zname1
    AND p~loekz = '  '
    AND e~bwart = '101'.

  CHECK ct_item[] IS NOT INITIAL.


  " 去掉已经冲销的数量
  SELECT * INTO TABLE @DATA(lt_ekbe_102)
    FROM ekbe
    FOR ALL ENTRIES IN @ct_item
    WHERE ebeln = @ct_item-ebeln
      AND ebelp = @ct_item-ebelp
      AND bwart = '102'.

  LOOP AT ct_item ASSIGNING <gs_item>.

    LOOP AT lt_ekbe_102 INTO DATA(ls_ekbe_102) WHERE ebeln = <gs_item>-ebeln
                                                 AND ebelp = <gs_item>-ebelp
                                                 AND lfgja = <gs_item>-lfgja
                                                 AND lfbnr = <gs_item>-lfbnr
                                                 AND lfpos = <gs_item>-lfpos.
      <gs_item>-menge2 = <gs_item>-menge2 - ls_ekbe_102-menge.
    ENDLOOP.

    <gs_item>-menge3  = <gs_item>-menge2.
  ENDLOOP.

  DELETE ct_item WHERE menge3 = 0.


  CHECK ct_item[] IS NOT INITIAL.

  SELECT
    c~vbeln AS vbeln_va,
    c~posnr AS posnr_va,
    c~matnr,
    c~werks,
    c~werks AS bukrs,
    c~lgort,
    c~charg,
    kalab AS menge4,
    a~idnlf,
    a~maktx,
    a~meins,
    a~bprme,
    a~zmm_tran_rate,
    b~sgt_scat,
    b~zcolor,
    b~zcolor_text,
    b~zsize,
    b~znorms,
    b~zshelves,
    b~zvat_nub,
    p~zzpino,
    p~zppdhd,
    b~zppflag
  FROM mska AS c
    INNER JOIN zvmat AS a ON c~matnr = a~matnr
    INNER JOIN ztpp0089 AS p ON c~vbeln = p~vbeln AND c~posnr = p~posnr
    LEFT JOIN zmch1 AS b ON c~charg = b~charg
  INTO TABLE @DATA(lt_mska)
  FOR ALL ENTRIES IN @ct_item
  WHERE a~matnr = @ct_item-matnr
    AND c~vbeln = @ct_item-vbeln_va
    AND c~posnr = @ct_item-posnr_va
    AND a~idnlf = @ct_item-idnlf
    AND c~werks = @ct_item-werks
    AND c~werks = @ct_item-werks
    AND c~lgort IN ( '1001' ,'1002', '1003' ,'5001','5002' ) " 退货只能退这几个仓库的
    AND b~zcolor = @ct_item-zcolor
    AND b~zsize  = @ct_item-zsize
    AND b~znorms = @ct_item-znorms
    AND b~zppflag = @ct_item-zppflag
    AND c~kalab > 0.

  SELECT a~afono,a~afonr,b~ebelp,a~price_long,a~price,a~peinh,a~ponam
    INTO TABLE @DATA(lt_price)
    FROM zafo_item  AS a
    INNER JOIN zafo_item_po AS b
    ON a~afono = b~afono AND a~afonr = b~afonr
    FOR ALL ENTRIES IN @ct_item
    WHERE a~afono = @ct_item-ebeln.

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.
  LOOP AT ct_item.
    lt_matnr-matnr = ct_item-matnr.
    lt_matnr-meins = ct_item-meins.
    COLLECT lt_matnr.
  ENDLOOP.
  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.


  SORT ct_item BY ebeln ebelp lfbnr.

  LOOP AT ct_item.

    ct_item-retpo = 'X'.

    READ TABLE lt_matnr WITH KEY matnr = ct_item-matnr.
    IF sy-subrc EQ 0 .
      ct_item-maktx_kp = lt_matnr-maktx_kp.
      ct_item-maktx_en = lt_matnr-maktx_en.
      ct_item-zmm_mf1 = lt_matnr-zmm_mf1.
      ct_item-zmm_cf = lt_matnr-zmm_cf.
      ct_item-zcate1 = lt_matnr-zcate1.
      ct_item-zcate2 = lt_matnr-zcate2.
      ct_item-zcate3 = lt_matnr-zcate3.
      ct_item-bprme = lt_matnr-bprme.
      ct_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    ct_item-menge1_bj = ct_item-menge1 / ct_item-zmm_tran_rate .
    ct_item-menge2_bj = ct_item-menge2 / ct_item-zmm_tran_rate.
    ct_item-menge3_bj = ct_item-menge3 / ct_item-zmm_tran_rate.

    LOOP AT lt_mska ASSIGNING FIELD-SYMBOL(<fs_mska>) WHERE matnr = ct_item-matnr
                                         AND vbeln_va  = ct_item-vbeln_va
                                         AND posnr_va  = ct_item-posnr_va
                                         AND idnlf  = ct_item-idnlf
                                         AND werks  = ct_item-werks
                                         AND zcolor = ct_item-zcolor
                                         AND zsize  = ct_item-zsize
                                         AND znorms = ct_item-znorms.
      ct_item-zvat_nub = <fs_mska>-zvat_nub.
      ct_item-charg = <fs_mska>-charg.
      ct_item-lgort = <fs_mska>-lgort.
      ct_item-zshelves = <fs_mska>-zshelves.
      IF <fs_mska>-menge4 > ct_item-menge3."库存大于未清数
        ct_item-menge4 = ct_item-menge3.
      ELSE.
        ct_item-menge4 = <fs_mska>-menge4.
      ENDIF.
      " 剩余库存
      <fs_mska>-menge4 = <fs_mska>-menge4 - ct_item-menge4.

    ENDLOOP.

    ct_item-menge4_bj = ct_item-menge4 / ct_item-zmm_tran_rate .

    READ TABLE lt_price INTO DATA(ls_price) WITH KEY afono = ct_item-ebeln ebelp = ct_item-ebelp.
    IF sy-subrc EQ 0.
      ct_item-price_long = ls_price-price_long.
      ct_item-price = ls_price-price.
      ct_item-peinh = ls_price-peinh.
      ct_item-ponam = ls_price-ponam.
    ELSE.
      ct_item-price_long = ct_item-amount / ct_item-menge1 * ct_item-zmm_tran_rate.
      PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND CHANGING ct_item.
    ENDIF.

    IF ct_item-text = 'X'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-009."'未审批'.
    ELSE.

      IF ct_item-menge2 <= 0.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'F' CHANGING ct_item-icon ct_item-text .
      ELSEIF ct_item-menge4 <= 0.
        ct_item-icon = icon_led_red.
        ct_item-text = '无库存'."'无库存'.
      ELSE.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
      ENDIF.
    ENDIF.

    APPEND ct_item TO lt_item.
  ENDLOOP.

  ct_item[] = lt_item[].

  IF p_fin = ''.
    DELETE ct_item WHERE icon NE icon_led_inactive .
  ENDIF.
ENDFORM.


FORM frm_ref_b TABLES ct_item STRUCTURE zafo_sitem.
  RANGES:s_clabs FOR mchb-clabs.

  CLEAR s_clabs.
  IF p_fin = ''.
    s_clabs-sign = 'I'.
    s_clabs-option = 'NE'.
    s_clabs-low = 0.
    APPEND s_clabs.
  ENDIF.

  SELECT
    c~matnr,
    c~werks,
    c~werks AS bukrs,
    c~werks AS umwrk,
    c~lgort,
    c~charg,
    c~clabs AS menge4,
    a~idnlf,
    a~maktx,
    a~meins,
    a~bprme,
    a~zmm_tran_rate,
    b~sgt_scat,
    b~zcolor,
    b~zcolor_text,
    b~zsize,
    b~znorms,
    b~zshelves,
    b~zvat_nub
    FROM mchb AS c
    INNER JOIN zvmat AS a ON c~matnr =  a~matnr
    LEFT JOIN zmch1 AS b ON c~charg = b~charg
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    WHERE c~werks = @p_werks
      AND c~lgort IN @s_lgort
      AND a~mtart IN @s_mtart
      AND a~matnr IN @s_matnr
      AND a~maktx IN @s_maktx
      AND a~idnlf IN @s_idnlf
      AND c~clabs IN @s_clabs.

  CHECK ct_item[] IS NOT INITIAL.

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.
  LOOP AT ct_item INTO DATA(cs_item).
    lt_matnr-matnr = cs_item-matnr.
    COLLECT lt_matnr.
  ENDLOOP.
  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.


  SELECT h~status,i~werks,i~lgort,i~matnr,i~charg,i~menge
    FROM zafo_item AS i
     INNER JOIN zafo_head AS h  ON h~afono = i~afono
     FOR ALL ENTRIES IN @ct_item
     WHERE i~werks = @ct_item-werks
     AND i~lgort = @ct_item-lgort
     AND i~matnr = @ct_item-matnr
     AND i~charg = @ct_item-charg
     AND  h~bustyp = @gs_bustyp-bustyp
     AND  h~bustyp <> 'SO001'
     AND ( h~status <> 'D' AND h~status <> 'T'  )
     AND i~del_flag <> 'X'
     INTO TABLE @DATA(lt_afo_item).

  PERFORM frm_get_h_price TABLES ct_item.

  LOOP AT ct_item.
    LOOP AT lt_afo_item INTO DATA(ls_afo_item) WHERE werks = ct_item-werks
                                                   AND lgort = ct_item-lgort
                                                   AND matnr = ct_item-matnr
                                                   AND charg = ct_item-charg.
      CASE gs_bustyp-bustyp+0(3) .
        WHEN 'AF0'.
          ct_item-menge4 = ct_item-menge4 - ls_afo_item-menge. " 领用单已经占用的数量

        WHEN OTHERS.
          IF ls_afo_item-status EQ 'A' .
            ct_item-menge4 = ct_item-menge4 - ls_afo_item-menge. " 已经占用的数量
          ENDIF.
      ENDCASE.
    ENDLOOP.

    IF g_object+1(4) = '4001'." 库存移动
      ct_item-zcolor_um    = ct_item-zcolor.
      ct_item-zsize_um     = ct_item-zsize    .
      ct_item-znorms_um    = ct_item-znorms   .
      ct_item-zshelves_um  = ct_item-zshelves .
      ct_item-zvat_nub_um  = ct_item-zvat_nub .
    ENDIF.

    IF gs_bustyp-execute_type = 'CC'.
      CLEAR ct_item-umwrk.
      CLEAR ct_item-bukrs.
    ELSE.
      ct_item-bukrs = ct_item-werks.
      ct_item-umwrk = ct_item-werks.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = ct_item-matnr.
    IF sy-subrc EQ 0 .
      ct_item-bprme = lt_matnr-bprme.
      ct_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    ct_item-menge4_bj = ct_item-menge4 / ct_item-zmm_tran_rate.
    PERFORM frm_set_round USING ct_item-bprme CHANGING ct_item-menge4_bj.

    IF ct_item-menge4 IS NOT INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
    ELSE.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'E' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    MODIFY ct_item.
  ENDLOOP.

  DELETE ct_item WHERE menge4 <= 0.
ENDFORM.


FORM frm_get_h_price TABLES ct_item STRUCTURE zafo_sitem.
  CHECK gs_bustyp-bustyp+0(1) = 'H' OR gs_bustyp-bustyp = 'AF03'  OR gs_bustyp-bustyp = 'AF07'.
  DATA:ls_kdgrp TYPE kdgrp.
  DATA:ls_vkorg TYPE vkorg.
  ls_kdgrp = 'A1'.
  ls_vkorg = '1008'.

  IF gs_bustyp-bustyp = 'H4007' OR gs_bustyp-bustyp = 'H4008' .
    ls_kdgrp = 'A0'.
    ls_vkorg = '1006'.
  ENDIF.
  SELECT
    matnr,
    kbetr
    FROM
    a901 INNER JOIN konp
    ON a901~knumh = konp~knumh
    FOR ALL ENTRIES IN @ct_item
    WHERE matnr =  @ct_item-matnr
    AND a901~kschl = 'ZPR1'
    AND vkorg = @ls_vkorg
    AND kdgrp = @ls_kdgrp
    AND datbi >= @sy-datum
    AND datab <= @sy-datum
    AND loevm_ko = ''
    INTO TABLE @DATA(lt_price).

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<cs_item>).
    READ TABLE lt_price INTO DATA(ls_price) WITH KEY matnr = <cs_item>-matnr.
    IF sy-subrc EQ 0 AND <cs_item>-price_long IS INITIAL.
      <cs_item>-price_long = ls_price-kbetr.
      PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND CHANGING <cs_item>.
    ENDIF.

    PERFORM f_set_amount IN PROGRAM saplzafo IF FOUND CHANGING <cs_item>.
  ENDLOOP.
ENDFORM.


FORM frm_ref_c TABLES ct_item STRUCTURE zafo_sitem. "订单库存采购

  RANGES:s_clabs FOR mchb-clabs.

  CLEAR s_clabs.
  IF p_fin = ''.
    s_clabs-sign = 'I'.
    s_clabs-option = 'NE'.
    s_clabs-low = 0.
    APPEND s_clabs.
  ENDIF.

  SELECT
    c~vbeln AS vbeln_va,
    c~posnr AS posnr_va,
    c~matnr,
    c~werks,
    c~werks AS bukrs,
    c~lgort,
    c~charg,
    t~name1 AS zshd,
    kalab AS menge4,
    a~idnlf,
    a~maktx,
    a~meins,
    a~bprme,
    a~zmm_tran_rate,
    b~sgt_scat,
    b~zcolor,
    b~zcolor_text,
    b~zsize,
    b~znorms,
    b~zppflag,
    b~zshelves,
    b~zvat_nub,
*     B~LIFNR,
    k~kunnr,
    p~zzpino,
    p~zppdhd
    FROM mska AS c
       INNER JOIN zvmat AS a ON c~matnr =  a~matnr
       INNER JOIN vbak AS k ON c~vbeln = k~vbeln
       INNER JOIN ztpp0089 AS p ON c~vbeln = p~vbeln AND c~posnr = p~posnr
       LEFT JOIN ztpp0093 AS t ON t~zwerks = p~zwerks
       LEFT JOIN zmch1 AS b ON c~charg = b~charg
       INTO CORRESPONDING FIELDS OF TABLE @ct_item
    WHERE a~matnr IN @s_matnr
          AND a~mtart IN @s_mtart
          AND p~zzpino IN @s_zzpino
          AND p~zppdhd IN @s_zppdhd
          AND a~idnlf IN @s_idnlf
          AND c~werks = @p_werks
          AND c~lgort IN @s_lgort
          AND kalab IN @s_clabs.


  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.
  LOOP AT ct_item INTO DATA(cs_item).
    lt_matnr-matnr = cs_item-matnr.
    COLLECT lt_matnr.
  ENDLOOP.
  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.

  LOOP AT ct_item.

    IF gs_bustyp-execute_type = 'CC'.
      ct_item-umwrk = ct_item-bukrs.
    ELSE.
      ct_item-bukrs = ct_item-werks.
      ct_item-umwrk = ct_item-werks.
    ENDIF.

    ct_item-ummat = ct_item-matnr.
    ct_item-zcolor_um = ct_item-zcolor.
    ct_item-zsize_um = ct_item-zsize.
    ct_item-znorms_um = ct_item-znorms.
    ct_item-zppflag_um = ct_item-zppflag.
    ct_item-zshelves_um = ct_item-zshelves.
    ct_item-zvat_nub_um = ct_item-zvat_nub.

    READ TABLE lt_matnr WITH KEY matnr = ct_item-matnr.
    IF sy-subrc EQ 0 .
      ct_item-bprme = lt_matnr-bprme.
      ct_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    ct_item-menge4_bj = ct_item-menge4 / ct_item-zmm_tran_rate.
    PERFORM frm_set_round USING ct_item-bprme CHANGING ct_item-menge4_bj.

    IF ct_item-menge4 IS NOT INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
    ELSE.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'E' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    MODIFY ct_item.

  ENDLOOP.

ENDFORM.


FORM  frm_ref_d TABLES ct_item STRUCTURE zafo_sitem
                       ct_item_po STRUCTURE zafo_sitem_po. "获取BOM
  DATA:ls_menge TYPE menge_d.

  SELECT
    a~zppdhd,
    a~zzpino,
    a~zkunnr_mat,
    a~vbeln,
    a~posnr,
    a~kunnr,
    a~zmldat,
    a~zfldat,
    a~zbzdat,
    b~idnlf,
    b~matnr,
    b~zcolor1,
    b~zsize,
    b~zzjsssgg,
    SUM( b~zzdh ) AS zzdh,
    SUM( b~zqrsl ) AS zqrsl,
    SUM( b~zqrsl_cg ) AS zqrsl_cg,
    SUM( b~zsyckl ) AS zsyckl,
    b~zqrzt,
    b~zbcg_flag,
    b~zzwlly,
    m~zcate1,
    m~meins,
    m~bprme,
    b~zofno,
    b~line_id,
    m~maktx,
    m~maktx_kp,
    m~maktx_en,
    b~zcqy,
    c~name1,
    c~cwerks AS werks
    FROM ztpp0089 AS a
    INNER JOIN ztpp0091 AS b ON a~zppdhd = b~zppdhd
    LEFT JOIN ztpp0093 AS c ON a~zwerks = c~zwerks
    INNER JOIN zmmv0010 AS m ON b~matnr = m~matnr
    WHERE a~zzpino IN @s_zzpino
    AND a~zppdhd IN @s_zppdhd
    AND c~cwerks IN @s_werks
    AND b~matnr IN @s_matnr
    AND b~lifnr IN @s_lifnr
    AND m~maktx IN @s_maktx
    AND a~zdhdzt NOT IN ( 'D','Y' ) AND a~loevm = ''
    AND a~zsctype <> '3'
    AND b~zzdh <> '0.000'
    AND m~zcate1 IN @s_cate1
    AND m~zcate2 IN @s_cate2
    AND m~idnlf IN @s_idnlf
    AND b~zzwlly IN @s_zzwlly
    AND b~zzsfmb <> 'Y'
    GROUP BY a~zppdhd,
             a~zzpino,
             a~zkunnr_mat,
             a~vbeln,
             a~posnr,
             a~kunnr,
             a~zmldat,
             a~zfldat,
             a~zbzdat,
             b~idnlf,
             b~matnr,
             b~zcolor1,
             b~zsize,
             b~zzjsssgg,
             b~zbcg_flag,
             b~zqrzt,
             b~zzwlly,
             m~zcate1,
             m~meins,
             m~bprme,
             b~zofno,
             b~line_id,
             m~maktx,
             m~maktx_kp,
             m~maktx_en,
             b~zcqy,
             c~name1,
             c~cwerks
             INTO TABLE @DATA(lt_bom).

  CHECK lt_bom[] IS NOT INITIAL.

  CASE g_bustyp.
    WHEN 'PO003'.
      SELECT zppdhd,zyxfl,zdhdzt,zzscgdh,zdhdsl
        INTO TABLE @DATA(lt_ztpp0089)
        FROM ztpp0089 FOR ALL ENTRIES IN @lt_bom
        WHERE zppdhd = @lt_bom-zppdhd.
  ENDCASE.


  SELECT i~* INTO TABLE @DATA(lt_offer)
    FROM  zmmt0050 AS h INNER JOIN zmmt0051 AS i ON h~zofno = i~zofno
    FOR ALL ENTRIES IN @lt_bom
    WHERE h~zofno = @lt_bom-zofno
    AND line_id = @lt_bom-line_id.

  SORT lt_offer BY zofno line_id.

  IF lt_offer IS NOT INITIAL.
    SELECT DISTINCT partner,zname1
    FROM zscmt0010
    FOR ALL ENTRIES IN @lt_offer
    WHERE partner = @lt_offer-lifnr
    INTO TABLE @DATA(lt_zscmt0010).
  ENDIF.

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.

  LOOP AT lt_bom INTO DATA(ls_bom).
    lt_matnr-matnr = ls_bom-matnr.
    COLLECT lt_matnr.
  ENDLOOP.

  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.

  SELECT zafo_item_po~afono,
         zafo_item_po~afonr,
         zafo_item_po~zppdhd,
         zafo_item_po~matnr,
         zafo_item_po~idnlf,
         zafo_item_po~zcolor,
         zafo_item_po~zsize,
         zafo_item_po~znorms,
         zafo_item_po~zppflag,
         zafo_item_po~menge
    FROM zafo_item_po
     INNER JOIN zafo_head ON zafo_head~afono = zafo_item_po~afono
    FOR ALL ENTRIES IN @lt_bom
    WHERE zafo_item_po~zppdhd = @lt_bom-zppdhd
      AND zafo_item_po~matnr = @lt_bom-matnr
      AND zafo_item_po~idnlf = @lt_bom-idnlf
      AND zafo_item_po~zcolor = @lt_bom-zcolor1
      AND zafo_item_po~zsize = @lt_bom-zsize
      AND zafo_item_po~znorms = @lt_bom-zzjsssgg
      AND zafo_item_po~zppflag = @lt_bom-zcqy
      AND zafo_head~bustyp = @g_bustyp
      AND zafo_item_po~loekz = ''
      AND zafo_head~status <> 'D'
    INTO TABLE @DATA(lt_item_po).


  IF lt_item_po[] IS NOT INITIAL." 有使用库存的
    SELECT zafo_item~afono,
            zafo_item~afonr,
            zafo_item~zzpino,
            zafo_item~matnr,
            zafo_item~idnlf,
            zafo_item~zcolor,
            zafo_item~zsize,
            zafo_item~znorms,
            zafo_item~zppflag,
            zafo_item~menge4
      FROM zafo_item
       INNER JOIN zafo_head ON zafo_head~afono = zafo_item~afono
      FOR ALL ENTRIES IN @lt_item_po
      WHERE zafo_item~afono = @lt_item_po-afono
        AND zafo_item~afonr = @lt_item_po-afonr
        AND zafo_head~bustyp = @g_bustyp
        AND zafo_item~del_flag = ''
        AND zafo_item~menge4 > 0
        AND zafo_head~status <> 'D'
       INTO TABLE @DATA(lt_item_menge4).
  ENDIF.

  SORT lt_bom BY zzpino matnr idnlf zcolor1 zsize zzjsssgg zcqy.

  SELECT DISTINCT spras,zcolor,zcolor_text
    FROM zvcolor
  INTO TABLE @DATA(lt_zcolor)
    FOR ALL ENTRIES IN @lt_bom
    WHERE zcolor EQ @lt_bom-zcolor1.

  LOOP AT lt_bom INTO ls_bom.
    READ TABLE lt_offer INTO DATA(ls_offer)
                        WITH KEY zofno = ls_bom-zofno
                                line_id = ls_bom-line_id BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR ls_offer.
    ENDIF.

    READ TABLE lt_zscmt0010 INTO DATA(ls_zscmt0010)
                            WITH KEY  partner = ls_offer-lifnr.
    IF sy-subrc NE 0.
      CLEAR ls_zscmt0010.
    ENDIF.

    CLEAR gt_item.
    CLEAR gt_item_po.

    gt_item_po-werks = ls_bom-werks.

    gt_item_po-lifnr = ls_offer-lifnr.
    gt_item_po-lifnr_name = ls_zscmt0010-zname1.

    gt_item_po-vbeln_va = ls_bom-vbeln.
    gt_item_po-posnr_va = ls_bom-posnr.

    gt_item_po-zzpino = ls_bom-zzpino.
    gt_item_po-zppdhd = ls_bom-zppdhd.
    gt_item_po-zkunnr_mat = ls_bom-zkunnr_mat.

    gt_item_po-matnr = ls_bom-matnr.
    gt_item_po-idnlf = ls_bom-idnlf.
    gt_item_po-maktx_zh = ls_bom-maktx.
    IF gt_item_po-maktx_zh IS INITIAL.
      gt_item_po-maktx_zh = ls_offer-maktx.
    ENDIF.
    gt_item_po-maktx_en = ls_bom-maktx_en.
    IF  gt_item_po-maktx_en IS INITIAL.
      gt_item_po-maktx_en = ls_offer-maktx_en.
    ENDIF.

    IF ls_bom-zcolor1 IS NOT INITIAL.
      gt_item_po-zcolor = ls_bom-zcolor1.
      READ TABLE lt_zcolor INTO DATA(ls_zcolor) WITH KEY zcolor = gt_item_po-zcolor spras = sy-langu.
      IF sy-subrc EQ 0 .
        gt_item_po-zcolor_text = ls_zcolor-zcolor_text.
      ENDIF.
    ELSE.
      CLEAR gt_item_po-zcolor.
      CLEAR gt_item_po-zcolor_text.
    ENDIF.

    gt_item_po-zsize = ls_bom-zsize.
    gt_item_po-znorms = ls_bom-zzjsssgg.
    gt_item_po-zppflag = ls_bom-zcqy.

    gt_item_po-meins = ls_bom-meins.
    IF gt_item_po-meins IS NOT INITIAL.
      gt_item_po-meins = ls_offer-meins.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = gt_item_po-matnr.
    IF sy-subrc EQ 0 .
      gt_item_po-bstme = lt_matnr-bprme.
      gt_item_po-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    gt_item_po-waers = ls_offer-waers.
    gt_item_po-price = ls_offer-netpr.

    gt_item_po-peinh = ls_offer-peinh.

    gt_item_po-menge = ls_bom-zzdh.
    gt_item_po-ktmng = ls_bom-zzdh.

    CLEAR ls_menge.

    LOOP AT lt_item_po INTO DATA(ls_item_po)" 去掉已采购数
                             WHERE zppdhd = ls_bom-zppdhd
                               AND matnr = ls_bom-matnr
                               AND idnlf = ls_bom-idnlf
                               AND zcolor = ls_bom-zcolor1
                               AND zsize = ls_bom-zsize
                               AND znorms = ls_bom-zzjsssgg
                               AND zppflag = ls_bom-zcqy.
      gt_item_po-menge = gt_item_po-menge - ls_item_po-menge.
      IF gt_item_po-menge <= 0 .
        gt_item_po-menge = 0.
      ENDIF.
      ls_menge = ls_menge + ls_item_po-menge.
    ENDLOOP.

    IF gt_item_po-menge > 0.
      gt_item_po-amount = ls_offer-netpr * gt_item_po-menge / ls_offer-peinh / gt_item_po-zmm_tran_rate.

      IF ls_bom-zbcg_flag = 'X'.
        gt_item_po-icon = icon_led_red.
        gt_item_po-text = '不采购'.

      ELSEIF ls_bom-zzwlly = 'J'.
        gt_item_po-icon = icon_led_inactive.
        gt_item_po-text = '加工物料'.
        IF ls_bom-name1 = 'VN越南' OR ls_bom-name1 = 'ET埃塞'.
          gt_item_po-icon = icon_led_red.
        ENDIF.
      ELSEIF ls_bom-zqrzt = 'X'.
        gt_item_po-icon = icon_led_inactive.
        gt_item_po-text = TEXT-010."'初始'.
      ELSE.
        gt_item_po-icon = icon_led_inactive.
        gt_item_po-text = TEXT-010."'初始'.
*        gt_item_po-icon = icon_led_red.
*        gt_item_po-text = TEXT-011."'业务未确认'.
      ENDIF.

      CASE g_bustyp.
        WHEN 'PO003'.
          READ TABLE lt_ztpp0089 INTO DATA(ls_ztpp0089) WITH  KEY zppdhd = ls_bom-zppdhd.
          IF sy-subrc = 0 AND ls_ztpp0089-zyxfl = 'Y'.
            IF ls_ztpp0089-zdhdzt EQ 'Y' .
              gt_item_po-icon = icon_led_red.
              gt_item_po-text = '订单已关闭'.
            ELSEIF ls_ztpp0089-zdhdzt = 'D'.
              gt_item_po-icon = icon_led_red.
              gt_item_po-text = '订单删除'.
            ELSE.
              gt_item_po-aufnr = ls_ztpp0089-zzscgdh.
              gt_item_po-icon = icon_led_inactive.
              gt_item_po-text = TEXT-010."'初始'.
            ENDIF.

          ELSE.
            gt_item_po-icon = icon_led_red.
            gt_item_po-text = '未发料'.
          ENDIF.
      ENDCASE.


      IF gt_item_po-menge IS INITIAL.
        CLEAR gt_item_po.
      ELSE.
        APPEND gt_item_po.
        CLEAR gt_item_po.
      ENDIF.
    ENDIF.

    gt_item-zzpino = ls_bom-zzpino.
    gt_item-idnlf = ls_bom-idnlf.
    gt_item-matnr = ls_bom-matnr.
    gt_item-maktx = ls_bom-maktx.
    gt_item-maktx_kp = ls_bom-maktx_kp.
    gt_item-maktx_en = ls_bom-maktx_en.

    IF gt_item-maktx IS INITIAL.
      gt_item-maktx = ls_offer-maktx.
    ENDIF.

    IF gt_item-maktx_kp IS INITIAL.
      gt_item-maktx_kp = ls_offer-maktx_kp.
    ENDIF.

    IF gt_item-maktx_en IS INITIAL.
      gt_item-maktx_en = ls_offer-maktx_en.
    ENDIF.

    IF ls_bom-zcolor1 IS NOT INITIAL.
      gt_item-zcolor = ls_bom-zcolor1.
      READ TABLE lt_zcolor INTO ls_zcolor WITH KEY zcolor = gt_item-zcolor spras = sy-langu.
      IF sy-subrc EQ 0 .
        gt_item-zcolor_text = ls_zcolor-zcolor_text.
      ENDIF.

      READ TABLE lt_zcolor INTO ls_zcolor WITH KEY zcolor = gt_item-zcolor spras = '1'.
      IF sy-subrc EQ 0 .
        gt_item-zcolor_text_zh = ls_zcolor-zcolor_text.
      ENDIF.

      READ TABLE lt_zcolor INTO ls_zcolor WITH KEY zcolor = gt_item-zcolor spras = 'E'.
      IF sy-subrc EQ 0 .
        gt_item-zcolor_text_en = ls_zcolor-zcolor_text.
      ENDIF.

    ELSE.
      CLEAR gt_item-zcolor.
      CLEAR gt_item-zcolor_text.
      CLEAR gt_item-zcolor_text_zh.
      CLEAR gt_item-zcolor_text_en.
    ENDIF.

    gt_item-zsize = ls_bom-zsize.
    gt_item-znorms = ls_bom-zzjsssgg.
    gt_item-zppflag = ls_bom-zcqy.

    gt_item-menge = ls_bom-zzdh.

    IF ls_bom-meins IS NOT INITIAL.
      gt_item-meins = ls_bom-meins.
    ELSE  .
      gt_item-meins = ls_offer-meins.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = gt_item-matnr.
    IF sy-subrc EQ 0 .
      gt_item-zcate1 = lt_matnr-zcate1.
      gt_item-zcate2 = lt_matnr-zcate2.
      gt_item-zcate3 = lt_matnr-zcate3.
      gt_item-bprme = lt_matnr-bprme.
      gt_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    gt_item-waers = ls_offer-waers.
    gt_item-ponam = ls_offer-zcguser.

    gt_item-zmm_mf1 = ls_offer-zmm_mf1.
    gt_item-zmm_mf_gc = ls_offer-zmm_mf_gc.
    gt_item-zmm_kz = ls_offer-zmm_kz.
    gt_item-zmm_cf = ls_offer-zmm_cf.
    gt_item-zmm_jybz = ls_offer-zmm_jybz.

    gt_item-zofno = ls_offer-zofno.
    gt_item-line_id = ls_offer-line_id.
    gt_item-lifnr = ls_offer-lifnr.
    gt_item-lifnr_name = ls_zscmt0010-zname1.

    gt_item-price = ls_offer-netpr.
    gt_item-peinh = ls_offer-peinh.

    gt_item-menge1 = ls_bom-zzdh.
    gt_item-menge2 = gt_item-menge2 + ls_menge.
    gt_item-menge3 = gt_item-menge1 - gt_item-menge2.
    gt_item-menge4 = ls_bom-zsyckl.
    gt_item-menge5 = ls_bom-zqrsl_cg.

    gt_item-zshd = ls_bom-name1.
    gt_item-werks = ls_bom-werks.
    gt_item-bukrs = ls_bom-werks.
    gt_item-ekorg = ls_bom-werks.
    IF gt_item-zppflag = 'X'.
      gt_item-zshd = 'CN德清'.
    ENDIF.

    IF ls_bom-zbcg_flag = 'X'.
      gt_item-icon = icon_led_red.
      gt_item-text = '不采购'.

    ELSEIF ls_bom-zzwlly = 'J'.
      gt_item-icon = icon_led_inactive.
      gt_item-text = '加工物料'.
      IF ls_bom-name1 = 'VN越南' OR ls_bom-name1 = 'ET埃塞'.
        gt_item_po-icon = icon_led_red.
      ENDIF.

    ELSEIF ls_bom-zqrzt = 'X'.
      gt_item-icon = icon_led_inactive.
      gt_item-text = TEXT-010."'初始'.
    ELSE.

      gt_item-icon = icon_led_inactive.
      gt_item-text = TEXT-010."'初始'.
*      gt_item-icon = icon_led_red.
*      gt_item-text = TEXT-011."'业务未确认'.
    ENDIF.

    CASE g_bustyp.
      WHEN 'PO003'.
        gt_item-zppdhd = ls_bom-zppdhd.
        gt_item-vbeln_va = ls_bom-vbeln.
        gt_item-posnr_va = ls_bom-posnr.
        READ TABLE lt_ztpp0089 INTO ls_ztpp0089 WITH  KEY zppdhd = ls_bom-zppdhd.
        IF sy-subrc = 0 AND ls_ztpp0089-zyxfl = 'Y'.
          IF ls_ztpp0089-zdhdzt EQ 'Y' .
            gt_item-icon = icon_led_red.
            gt_item-text = '订单已关闭'.
          ELSEIF ls_ztpp0089-zdhdzt = 'D' OR ls_ztpp0089-zdhdsl <= 0.
            gt_item-icon = icon_led_red.
            gt_item-text = '订单已删除'.
          ELSE.
            gt_item-aufnr = ls_ztpp0089-zzscgdh.
            gt_item-icon = icon_led_inactive.
            gt_item-text = TEXT-010."'初始'.
          ENDIF.
        ELSE.
          gt_item-icon = icon_led_red.
          gt_item-text = '未发料'.
        ENDIF.
        gt_item-matkl = 'Z200'.
    ENDCASE.


    COLLECT gt_item.
    CLEAR gt_item.

  ENDLOOP.

  LOOP AT gt_item.
    IF gt_item-menge3 <= 0.
      gt_item-icon = icon_led_green.
      gt_item-text = TEXT-012."'已完成'.
      LOOP AT gt_item_po ASSIGNING FIELD-SYMBOL(<gs_item_po>)
                         WHERE icon  = gt_item_po-icon
                           AND afono = gt_item_po-afono
                           AND zzpino = gt_item_po-zzpino
                           AND idnlf = gt_item_po-idnlf
                           AND matnr = gt_item_po-matnr
                           AND zcolor = gt_item_po-zcolor
                           AND zsize = gt_item_po-zsize
                           AND znorms = gt_item_po-znorms
                           AND zppflag = gt_item_po-zppflag.
        <gs_item_po>-icon = icon_led_green.
        <gs_item_po>-text = TEXT-012."'已完成'.
      ENDLOOP.
    ENDIF.

    gt_item-menge = gt_item-menge3.

    READ TABLE lt_offer INTO ls_offer WITH KEY zofno = gt_item-zofno line_id = gt_item-line_id.
    IF sy-subrc EQ 0.
      gt_item-zmm_mf1 = ls_offer-zmm_mf1.

      SELECT SINGLE netpr INTO @DATA(ls_netpr)
        FROM zmmt0052
        WHERE del_flag EQ ''
        AND zofno = @gt_item-zofno AND line_id = @gt_item-line_id
        AND  start_menge < @gt_item-menge_cg
        AND end_menge >= @gt_item-menge_cg.
      IF sy-subrc EQ 0.
        gt_item-price_long = ls_netpr.
      ELSE.
        SELECT SINGLE netpr INTO @ls_netpr
          FROM zmmt0052
          WHERE del_flag EQ ''
          AND zofno = @gt_item-zofno AND line_id = @gt_item-line_id
          AND  start_menge = 0
          AND end_menge = 0
          AND zsize =  @gt_item-zsize.
        IF sy-subrc EQ 0.
          gt_item-price_long = ls_netpr.
        ELSE.
          gt_item-price_long = ls_offer-price_long.
        ENDIF.
      ENDIF.
    ENDIF.

    PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND CHANGING gt_item.
    PERFORM frm_po_item_calculation IN PROGRAM saplzafo IF FOUND CHANGING gt_item.
    PERFORM f_set_amount IN PROGRAM saplzafo IF FOUND CHANGING gt_item.

    MODIFY gt_item.
  ENDLOOP.

  IF gs_bustyp-execute_type = 'POC'.
    CHECK ct_item[] IS NOT INITIAL.

    DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

    DELETE ct_item WHERE menge3 = ''.

    SELECT i~* INTO CORRESPONDING FIELDS OF TABLE @lt_item
      FROM zafo_item AS i INNER JOIN zafo_head AS h
      ON i~afono = h~afono
       FOR ALL ENTRIES IN @ct_item
      WHERE i~zzpino = @ct_item-zzpino
      AND i~matnr = @ct_item-matnr
      AND i~idnlf = @ct_item-idnlf
      AND i~zcolor = @ct_item-zcolor
      AND i~zsize = @ct_item-zsize
      AND i~znorms = @ct_item-znorms
      AND i~zppflag = @ct_item-zppflag
      AND h~bustyp IN ('PO001', 'PO003')
      AND h~status IN ( 'B', 'C' )
      AND i~ebeln <> ''.

    LOOP AT ct_item.
      READ TABLE lt_item WITH KEY zzpino = ct_item-zzpino
                                  matnr = ct_item-matnr
                                  idnlf = ct_item-idnlf
                                  zcolor = ct_item-zcolor
                                  zsize = ct_item-zsize
                                  znorms = ct_item-znorms
                                  zppflag = ct_item-zppflag.
      IF sy-subrc EQ 0.
        ct_item-ebeln = lt_item-ebeln.
        ct_item-price_long = lt_item-price_long.
        ct_item-price = lt_item-price.
        ct_item-peinh = lt_item-peinh.
        PERFORM frm_po_item_calculation IN PROGRAM saplzafo IF FOUND CHANGING gt_item.
        MODIFY ct_item.
      ELSE.
        IF ct_item-menge3 IS INITIAL.
          DELETE ct_item.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE @gt_item_cost
      FROM zafo_item_cost
      FOR ALL ENTRIES IN @ct_item
      WHERE afono = @ct_item-afono.
    IF sy-subrc EQ 0.
*       LOOP AT GT_ITEM_COST ASSIGNING <GS_ITEM_COST>.
*         CLEAR <GS_ITEM_COST>-AFONO.
*       ENDLOOP.
    ENDIF.

  ENDIF.

  IF p_fin = ''.
    DELETE gt_item WHERE icon = icon_led_green.
  ENDIF.

  ct_item_po[] = gt_item_po[].

  SORT gt_item BY zzpino idnlf zcolor zsize znorms zppflag.
ENDFORM.


FORM frm_ref_l TABLES ct_item STRUCTURE zafo_sitem
                       ct_item_po STRUCTURE zafo_sitem_po. "获取BOM

  DATA:BEGIN OF lt_jq OCCURS 0,
         zzpino TYPE zzpino,
         zmldat TYPE zmldat,
         zfldat TYPE zfldat,
         zbzdat TYPE zbzdat,
       END OF lt_jq.

  SELECT
    a~zppdhd,
    a~zzpino,
    a~zkunnr_mat,
    a~vbeln,
    a~posnr,
    a~kunnr,
    a~zmldat,
    a~zfldat,
    a~zbzdat,
    a~zdhdzt,
    a~loevm,
    b~idnlf,
    b~matnr,
    b~zcolor1,
    b~zsize,
    b~zzjsssgg,
    SUM( b~zzdh ) AS zzdh,
    SUM( b~zqrsl ) AS zqrsl,
    SUM( b~zqrsl_cg ) AS zqrsl_cg,
    b~zqrzt,
    m~zcate1,
    m~meins,
    m~bprme,
    b~zofno,
    b~line_id,
    m~maktx,
    m~maktx_kp,
    m~maktx_en,
    b~zcqy,
    c~name1,
    c~cwerks AS werks
    FROM ztpp0089 AS a
    INNER JOIN ztpp0091 AS b ON a~zppdhd = b~zppdhd
    LEFT JOIN ztpp0093 AS c ON a~zwerks = c~zwerks
    INNER JOIN zmmv0010 AS m ON b~matnr = m~matnr
    WHERE zzpino IN @s_zzpino
      AND a~zppdhd IN @s_zppdhd
      AND c~cwerks IN @s_werks
      AND b~matnr IN @s_matnr
      AND b~lifnr IN @s_lifnr
      AND m~maktx IN @s_maktx
      AND a~zdhdzt NOT IN ( 'D','Y' ) AND a~loevm = ''
      AND a~zsctype <> '3'
      AND b~zzdh <> '0.000'
      AND m~zcate1 IN @s_cate1
      AND m~zcate2 IN @s_cate2
      AND m~idnlf IN @s_idnlf
      AND b~zzwlly IN @s_zzwlly
      AND zzsfmb <> 'Y'
    GROUP BY a~zppdhd,
             a~zzpino,
             a~zkunnr_mat,
             a~vbeln,
             a~posnr,
             a~kunnr,
             a~zmldat,
             a~zfldat,
             a~zbzdat,
             a~zdhdzt,
             a~loevm,
             b~idnlf,
             b~matnr,
             b~zcolor1,
             b~zsize,
             b~zzjsssgg,
             b~zqrzt,
             m~zcate1,
             m~meins,
             m~bprme,
             b~zofno,
             b~line_id,
             m~maktx,
             m~maktx_kp,
             m~maktx_en,
             b~zcqy,
             c~name1,
             c~cwerks
             INTO TABLE @DATA(lt_bom).

  CHECK sy-subrc EQ 0.

  SELECT i~* INTO TABLE @DATA(lt_offer)
    FROM zmmt0050 AS h
    INNER JOIN zmmt0051 AS i ON h~zofno = i~zofno
    FOR ALL ENTRIES IN @lt_bom
    WHERE h~zofno = @lt_bom-zofno
    AND line_id = @lt_bom-line_id.

  SORT lt_offer BY zofno line_id.

  IF lt_offer IS NOT INITIAL.
    SELECT DISTINCT partner,zname1
      FROM zscmt0010
      FOR ALL ENTRIES IN @lt_offer
      WHERE partner = @lt_offer-lifnr
    INTO TABLE @DATA(lt_zscmt0010).
  ENDIF.

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.

  LOOP AT lt_bom INTO DATA(ls_bom).
    lt_matnr-matnr = ls_bom-matnr.
    COLLECT lt_matnr.
  ENDLOOP.

  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.


  DATA:lt_item_po TYPE TABLE OF zafo_item_po WITH HEADER LINE.
  DATA:sum_item_po TYPE TABLE OF zafo_item_po WITH HEADER LINE.
  DATA:sum_complate TYPE menge_d .

  SELECT h~remark1,
         i~werks,
         i~lifnr,
         i~lifnr_name,
         i~afono,
         i~afonr,
         i~ebelp,
         i~ebeln,
         i~zzpino,
         i~zppdhd,
         i~matnr,
         i~idnlf,
         i~zcolor,
         i~zsize,
         i~znorms,
         i~zppflag,
         i~menge,
         i~ktmng
  INTO CORRESPONDING FIELDS OF TABLE @lt_item_po
  FROM zafo_item_po AS i
   INNER JOIN zafo_head AS h ON h~afono = i~afono
   LEFT JOIN zmmv0010 ON i~matnr = zmmv0010~matnr
   FOR ALL ENTRIES IN @lt_bom
  WHERE i~zzpino = @lt_bom-zzpino
    AND h~lifnr IN @s_lifnr
    AND zmmv0010~zcate1 IN @s_cate1
    AND zmmv0010~zcate2 IN @s_cate2
    AND zmmv0010~maktx IN @s_maktx
    AND i~idnlf IN @s_idnlf
    AND h~bustyp = 'PO001'
    AND h~status <> 'D'
    AND i~loekz = ''.

  CLEAR sum_item_po[].

  LOOP AT lt_item_po.
    MOVE-CORRESPONDING lt_item_po TO sum_item_po.
    CLEAR sum_item_po-afono.
    CLEAR sum_item_po-afonr.
    CLEAR sum_item_po-ebelp.
    CLEAR sum_item_po-ebeln.
    CLEAR sum_item_po-remark1.

    COLLECT sum_item_po.
    CLEAR sum_item_po.
  ENDLOOP.

  SORT lt_bom BY zzpino zppdhd idnlf zcolor1 zsize zzjsssgg zcqy.

  SELECT DISTINCT spras,zcolor,zcolor_text
    FROM zvcolor
  INTO TABLE @DATA(lt_zcolor)
    FOR ALL ENTRIES IN @lt_bom
    WHERE zcolor EQ @lt_bom-zcolor1.


  LOOP AT lt_bom INTO ls_bom.
    IF ls_bom-zdhdzt = 'D' AND ls_bom-loevm = 'X'.
      CLEAR ls_bom-zzdh.
      CLEAR ls_bom-zqrsl.
      CLEAR ls_bom-zqrsl_cg.
    ENDIF.

    READ TABLE lt_offer INTO DATA(ls_offer) WITH KEY
                                              zofno = ls_bom-zofno
                                              line_id = ls_bom-line_id BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR ls_offer.
    ENDIF.
    READ TABLE lt_zscmt0010 INTO DATA(ls_zscmt0010) WITH KEY
                                               partner = ls_offer-lifnr.
    IF sy-subrc NE 0.
      CLEAR ls_zscmt0010.
    ENDIF.

    CLEAR gt_item.
    CLEAR gt_item_po.

*     GT_ITEM_PO-ZOFNO = LS_OFFER-ZOFNO.
*     GT_ITEM_PO-LINE_ID = LS_OFFER-LINE_ID.
    gt_item_po-werks = ls_bom-werks.
    gt_item_po-idnlf = ls_bom-idnlf.
    gt_item_po-matnr = ls_bom-matnr.
    gt_item_po-maktx_zh = ls_bom-maktx.

    IF gt_item_po-maktx_zh IS INITIAL.
      gt_item_po-maktx_zh = ls_offer-maktx.
    ENDIF.

    gt_item_po-maktx_en = ls_bom-maktx_en.
    IF  gt_item_po-maktx_en IS INITIAL.
      gt_item_po-maktx_en = ls_offer-maktx_en.
    ENDIF.
    gt_item_po-menge = ls_bom-zzdh.
    gt_item_po-ktmng = ls_bom-zzdh.

    gt_item_po-meins = ls_bom-meins.
    IF gt_item_po-meins IS NOT INITIAL.
      gt_item_po-meins = ls_offer-meins.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = gt_item_po-matnr.
    IF sy-subrc EQ 0 .
      gt_item_po-bstme = lt_matnr-bprme.
      gt_item_po-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.
    gt_item_po-price = ls_offer-netpr.
    gt_item_po-waers = ls_offer-waers.
    gt_item_po-peinh = ls_offer-peinh.

    gt_item_po-werks = ls_bom-werks.
    gt_item_po-vbeln_va = ls_bom-vbeln.
    gt_item_po-posnr_va = ls_bom-posnr.
    gt_item_po-zzpino = ls_bom-zzpino.
    gt_item_po-zppdhd = ls_bom-zppdhd.
    gt_item_po-zkunnr_mat = ls_bom-zkunnr_mat.

    IF ls_bom-zcolor1 IS NOT INITIAL.
      gt_item_po-zcolor = ls_bom-zcolor1.
      READ TABLE lt_zcolor INTO DATA(ls_zcolor) WITH KEY zcolor = gt_item_po-zcolor spras = sy-langu.
      IF sy-subrc EQ 0 .
        gt_item_po-zcolor_text = ls_zcolor-zcolor_text.
      ENDIF.

    ENDIF.

    gt_item_po-zsize = ls_bom-zsize.
    gt_item_po-znorms = ls_bom-zzjsssgg.
    gt_item_po-zppflag = ls_bom-zcqy.
    gt_item_po-lifnr = ls_offer-lifnr.
    gt_item_po-lifnr_name = ls_zscmt0010-zname1.

    CLEAR sum_item_po.
    READ TABLE sum_item_po WITH KEY zzpino = gt_item_po-zzpino
                                    zppdhd = gt_item_po-zppdhd
                                     matnr = gt_item_po-matnr
                                     idnlf = gt_item_po-idnlf
                                    zcolor = gt_item_po-zcolor
                                     zsize = gt_item_po-zsize
                                    znorms = gt_item_po-znorms
                                   zppflag = gt_item_po-zppflag.
    IF sy-subrc EQ 0.
      READ TABLE lt_item_po WITH KEY zzpino = gt_item_po-zzpino
                                     zppdhd = gt_item_po-zppdhd
                                      matnr = gt_item_po-matnr
                                      idnlf = gt_item_po-idnlf
                                     zcolor = gt_item_po-zcolor
                                      zsize = gt_item_po-zsize
                                     znorms = gt_item_po-znorms
                                    zppflag = gt_item_po-zppflag.

      gt_item_po-ebeln = lt_item_po-afono.
      gt_item_po-remark1 = lt_item_po-remark1.
      gt_item_po-menge = gt_item_po-menge - sum_item_po-menge.
      gt_item_po-ktmng = gt_item_po-ktmng - sum_item_po-ktmng.

      gt_item-menge2 = gt_item-menge2 + sum_item_po-menge.

      gt_item_po-werks = sum_item_po-werks.
      gt_item_po-lifnr = sum_item_po-lifnr.
      gt_item_po-lifnr_name = sum_item_po-lifnr_name.

      gt_item_po-icon = icon_led_yellow.
      gt_item_po-text = TEXT-013."'变更数量'.
      gt_item_po-change_flag = 'M'.

    ELSE.
      gt_item_po-icon = icon_insert_row.
      gt_item_po-text = TEXT-014."''新增行'.
      gt_item_po-change_flag = 'I'.
    ENDIF.

    IF gt_item_po-ktmng = 0 OR gt_item_po-menge = 0.
      gt_item_po-menge = 0.
      gt_item_po-icon = icon_led_green.
      gt_item_po-text = TEXT-012."'已完成下单'.
      gt_item_po-change_flag = 'M'.
    ENDIF.

    gt_item_po-amount = ls_offer-netpr * gt_item_po-menge / ls_offer-peinh / gt_item_po-zmm_tran_rate.

    APPEND gt_item_po.

    gt_item-idnlf = ls_bom-idnlf.
    gt_item-matnr = ls_bom-matnr.
    gt_item-maktx = ls_bom-maktx.

    gt_item-maktx_kp = ls_bom-maktx_kp.
    gt_item-maktx_en = ls_bom-maktx_en.
    IF gt_item-maktx IS INITIAL.
      gt_item-maktx = ls_offer-maktx.
    ENDIF.

    IF gt_item-maktx_kp IS INITIAL.
      gt_item-maktx_kp = ls_offer-maktx_kp.
    ENDIF.

    IF gt_item-maktx_en IS INITIAL.
      gt_item-maktx_en = ls_offer-maktx_en.
    ENDIF.

    gt_item-menge = ls_bom-zzdh.

    IF ls_bom-meins IS NOT INITIAL.
      gt_item-meins = ls_bom-meins.
    ELSE  .
      gt_item-meins = ls_offer-meins.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = gt_item-matnr.
    IF sy-subrc EQ 0 .
      gt_item-zcate1 = lt_matnr-zcate1.
      gt_item-zcate2 = lt_matnr-zcate2.
      gt_item-zcate3 = lt_matnr-zcate3.
      gt_item-bprme = lt_matnr-bprme.
      gt_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    gt_item-waers = ls_offer-waers.
    gt_item-ponam = ls_offer-zcguser.

*     GT_ITEM-VBELN_VA = LS_BOM-VBELN.
*     GT_ITEM-POSNR_VA = LS_BOM-POSNR.

    IF ls_bom-zcolor1 IS NOT INITIAL.
      gt_item-zcolor = ls_bom-zcolor1.
      READ TABLE lt_zcolor INTO ls_zcolor WITH KEY zcolor = gt_item-zcolor spras = sy-langu.
      IF sy-subrc EQ 0 .
        gt_item-zcolor_text = ls_zcolor-zcolor_text.
      ENDIF.

      READ TABLE lt_zcolor INTO ls_zcolor WITH KEY zcolor = gt_item-zcolor spras = '1'.
      IF sy-subrc EQ 0 .
        gt_item-zcolor_text_zh = ls_zcolor-zcolor_text.
      ENDIF.

      READ TABLE lt_zcolor INTO ls_zcolor WITH KEY zcolor = gt_item-zcolor spras = 'E'.
      IF sy-subrc EQ 0 .
        gt_item-zcolor_text_en = ls_zcolor-zcolor_text.
      ENDIF.
    ENDIF.

    gt_item-zsize = ls_bom-zsize.
    gt_item-znorms = ls_bom-zzjsssgg.
*     GT_ITEM-ZPPDHD = LS_BOM-ZPPDHD.
    gt_item-zzpino = ls_bom-zzpino.
*     GT_ITEM-ZZPICGXM = LS_BOM-ZZPICGXM.
*     gt_item-zkunnr_mat = ls_bom-zkunnr_mat.
    gt_item-zppflag = ls_bom-zcqy.
    gt_item-zmm_mf1 = ls_offer-zmm_mf1.
    gt_item-zmm_mf_gc = ls_offer-zmm_mf_gc.
    gt_item-zmm_kz = ls_offer-zmm_kz.
    gt_item-zmm_cf = ls_offer-zmm_cf.
    gt_item-zmm_jybz = ls_offer-zmm_jybz.
    gt_item-zofno = ls_offer-zofno.
    gt_item-line_id = ls_offer-line_id.

    gt_item-price = ls_offer-netpr.
    gt_item-peinh = ls_offer-peinh.

    gt_item-menge1 = ls_bom-zzdh.
    gt_item-menge3 = gt_item-menge1 - gt_item-menge2.
    gt_item-menge5 = ls_bom-zqrsl_cg.

    gt_item-lifnr = ls_offer-lifnr.
    gt_item-lifnr_name = ls_zscmt0010-zname1.

    IF gt_item_po-lifnr IS NOT INITIAL.
      gt_item-lifnr = gt_item_po-lifnr.
      gt_item-lifnr_name = gt_item_po-lifnr_name.
    ENDIF.

    gt_item-zshd = ls_bom-name1.
    gt_item-werks = gt_item_po-werks.
    gt_item-bukrs = gt_item_po-werks.
    gt_item-ekorg = gt_item_po-werks.
    IF gt_item-zppflag = 'X'.
      gt_item-zshd = 'CN德清'.
    ENDIF.

    gt_item-ebeln = gt_item_po-ebeln.
    gt_item-remark1 = gt_item_po-remark1.

    gt_item-icon = gt_item_po-icon.
    gt_item-text = gt_item_po-text.
    gt_item-change_flag = gt_item_po-change_flag.

    COLLECT gt_item.
    CLEAR gt_item.
    CLEAR gt_item_po.

  ENDLOOP.

  LOOP AT gt_item.

    READ TABLE lt_offer INTO ls_offer WITH KEY zofno = gt_item-zofno line_id = gt_item-line_id.
    IF sy-subrc EQ 0.
      gt_item-price = ls_offer-netpr.
      gt_item-peinh = ls_offer-peinh.
      gt_item-zmm_mf1 = ls_offer-zmm_mf1.
    ENDIF.

    PERFORM frm_poc_item_calculation CHANGING gt_item.
    gt_item-menge = gt_item-menge3.

    PERFORM frm_set_price_long IN PROGRAM saplzafo IF FOUND
       USING gt_item-price gt_item-peinh CHANGING gt_item-price_long.

    MODIFY gt_item.
  ENDLOOP.


  CHECK ct_item[] IS NOT INITIAL.

  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  DATA:dt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:dt_item_po TYPE TABLE OF zafo_sitem_po WITH HEADER LINE.
  CLEAR dt_item_po[].

  SORT lt_item_po BY afono DESCENDING.

  LOOP AT lt_item_po.
    READ TABLE gt_item_po WITH KEY zzpino = lt_item_po-zzpino
                                   zppdhd = lt_item_po-zppdhd
                                   matnr = lt_item_po-matnr
                                   idnlf = lt_item_po-idnlf
                                   zcolor = lt_item_po-zcolor
                                   zsize = lt_item_po-zsize
                                   znorms = lt_item_po-znorms
                                   zppflag = lt_item_po-zppflag.
    IF sy-subrc EQ 0.
    ELSE.
      MOVE-CORRESPONDING lt_item_po TO dt_item_po.
      APPEND  dt_item_po.
      CLEAR dt_item_po.
    ENDIF.
  ENDLOOP.

  IF dt_item_po[] IS NOT INITIAL.

    SELECT * FROM zafo_item
     INTO CORRESPONDING FIELDS OF TABLE dt_item
     FOR ALL ENTRIES IN dt_item_po
     WHERE afono = dt_item_po-afono
     AND afonr = dt_item_po-afonr.

    SELECT * FROM zafo_item_po
      INTO CORRESPONDING FIELDS OF TABLE dt_item_po
      FOR ALL ENTRIES IN dt_item_po
      WHERE afono = dt_item_po-afono
      AND afonr = dt_item_po-afonr
      AND ebelp = dt_item_po-ebelp.

    LOOP AT dt_item.

      CLEAR dt_item-menge1.
      CLEAR dt_item-menge2.
      CLEAR dt_item-menge3.
      CLEAR dt_item-menge.
      CLEAR dt_item-menge_cg.
      CLEAR dt_item-amount.
      MODIFY dt_item.
    ENDLOOP.

    LOOP AT dt_item_po.
      CLEAR dt_item_po-ktmng.

      READ TABLE dt_item ASSIGNING FIELD-SYMBOL(<ds_item>) WITH KEY afono = dt_item_po-afono afonr = dt_item_po-afonr.
      IF sy-subrc EQ 0.
        <ds_item>-menge2 = <ds_item>-menge2 + dt_item_po-menge.
        <ds_item>-menge3 = - <ds_item>-menge2.
        <ds_item>-menge = - <ds_item>-menge2.
        <ds_item>-menge_cg = - <ds_item>-menge2 / <ds_item>-zmm_tran_rate.
        <ds_item>-amount =  <ds_item>-amount + dt_item_po-amount.
      ENDIF.

      dt_item_po-menge = - dt_item_po-menge.
      dt_item_po-amount = - dt_item_po-amount.

      dt_item_po-icon = icon_delete_row.
      dt_item_po-text = TEXT-016."'已删除BOM'.
      dt_item-change_flag = 'D'.
      APPEND dt_item_po TO gt_item_po.
    ENDLOOP.
  ENDIF.

  LOOP AT dt_item.
    READ TABLE lt_item_po WITH KEY afono = dt_item-afono.
    dt_item-remark1 = lt_item_po-remark1.
    dt_item-amount = - dt_item-amount.
    dt_item-icon = icon_delete_row.
    dt_item-text = TEXT-016."'已删除BOM'.
    dt_item-change_flag = 'D'.
    APPEND dt_item TO ct_item.
  ENDLOOP.

  IF p_fin = ''.
    DELETE gt_item WHERE icon = icon_led_green.
  ENDIF.

  ct_item_po[] = gt_item_po[].

  SORT gt_item BY zzpino idnlf zcolor zsize znorms zppflag.

ENDFORM.


FORM frm_poc_item_calculation CHANGING ct_item TYPE zafo_sitem.

  DATA:l_menge TYPE p DECIMALS 9..

  CONDENSE ct_item-zmm_tran_rate NO-GAPS.

  IF ct_item-zmm_tran_rate IS INITIAL OR ct_item-zmm_tran_rate = '0.000'  OR ct_item-zmm_tran_rate = '1.000' .
    ct_item-zmm_tran_rate = 1.
  ENDIF.

  IF ct_item-peinh  IS INITIAL.
    ct_item-zmm_tran_rate = 1.
  ENDIF.
  ct_item-menge3 = ct_item-menge1 - ct_item-menge2 - ct_item-menge4.

  IF ct_item-menge3 < 0 AND gs_bustyp-busref <> 'L'.
    ct_item-menge3 = 0.
  ENDIF.

  ct_item-menge_cg = ct_item-menge3 / ct_item-zmm_tran_rate.

  l_menge = ct_item-menge_cg + ct_item-menge_zj .
  ct_item-menge = l_menge * ct_item-zmm_tran_rate.
  ct_item-amount = ct_item-price * ct_item-menge / ct_item-peinh / ct_item-zmm_tran_rate.

ENDFORM.


FORM frm_ref_e TABLES ct_item STRUCTURE zafo_sitem. "产前样发料
  DATA:ls_first TYPE posnr.

  SELECT
    h~zppdhd,
    h~zzpino,
    h~zkunnr_mat,
    h~vbeln AS vbeln_va,
    h~posnr AS posnr_va,
    h~zzscgdh AS aufnr,
    m~matnr,
    m~werks,
    m~lgort,
    v~idnlf,
    v~maktx,
    v~meins,
    v~bprme,
    v~zmm_tran_rate,
    c~zcolor,
    c~zsize,
    c~znorms,
    c~zshelves,
    c~zvat_nub,
    m~kalab AS menge4,
    m~charg,
    m~ersda
    FROM ztpp0089 AS h
    INNER JOIN mska AS m
    ON h~vbeln = m~vbeln AND h~posnr = m~posnr
    INNER JOIN zmmv0010 AS v ON m~matnr = v~matnr
    INNER JOIN zmch1 AS c ON m~charg = c~charg
    WHERE m~werks IN @s_werks
    AND m~lgort IN @s_lgort
    AND h~zppdhd IN @s_zppdhd
    AND h~zzpino IN @s_zzpino
    AND v~idnlf IN @s_idnlf
    AND v~zcate1 IN @s_cate1
    AND v~zcate2 IN @s_cate2
    AND kalab > 0
    AND zppflag = 'X'
    INTO TABLE @DATA(lt_stock).

  SELECT
    h~zppdhd,
    h~zzpino,
    h~zkunnr_mat,
    h~vbeln AS vbeln_va,
    h~posnr AS posnr_va,
    h~zzscgdh AS aufnr,
    h~werks ,
    i~zzbom_item,
    v~idnlf,
    v~bprme,
    v~zmm_tran_rate,
    i~matnr,
    i~zcolor1 AS zcolor,
    i~zsize,
    i~zzjsssgg AS znorms,
    i~zzdh AS menge1,
    i~zcqy AS zppflag,
    v~maktx,
    v~meins
    FROM ztpp0089 AS h
    INNER JOIN ztpp0091 AS i ON h~zppdhd = i~zppdhd
    INNER JOIN zmmv0010 AS v ON i~matnr = v~matnr
    WHERE h~loevm = ''
    AND h~zsfmbdhd <> 'X'
"      AND  zqrzt = 'X'
    AND h~werks IN @s_werks
    AND h~zppdhd IN @s_zppdhd
    AND h~zzpino IN @s_zzpino
    AND ( i~zzwlly = '' OR i~zzwlly = 'J')
    AND v~idnlf IN @s_idnlf
    AND v~zcate1 IN @s_cate1
    AND v~zcate2 IN @s_cate2
    AND zcqy = 'X'
    INTO TABLE @DATA(lt_data).
*产前样领料单，第一次全部领完，第二次有增加产前样数量只显示增加差额数

  IF sy-subrc EQ 0.
    SELECT
     h~bustyp  ,
     i~zzpino  ,
     i~zppdhd  ,
     i~vbeln_va  ,
     i~posnr_va  ,
     i~matnr   ,
     i~maktx   ,
     i~idnlf   ,
     i~zcolor  ,
     i~zsize   ,
     i~znorms  ,
     i~zppflag ,
     i~zvat_nub ,
     i~bukrs ,
     i~werks ,
     i~aufnr ,
     i~meins ,
     i~bprme,
     i~zmm_tran_rate,
     i~menge
      FROM zafo_head AS h
      INNER JOIN zafo_item AS i  ON h~afono = i~afono
      FOR ALL ENTRIES IN @lt_data
      WHERE i~zppdhd = @lt_data-zppdhd
      AND i~matnr = @lt_data-matnr
      AND i~idnlf = @lt_data-idnlf
      AND i~zcolor = @lt_data-zcolor
      AND i~zsize = @lt_data-zsize
      AND i~znorms = @lt_data-znorms
      AND i~zppflag = @lt_data-zppflag
      AND h~del_flag <> 'X'
      AND i~del_flag <> 'X'
      AND h~bustyp = 'AF05'
      AND h~status = 'C'
     INTO TABLE @DATA(lt_afo).

    DATA:sum_afo TYPE TABLE OF ty_afo WITH HEADER LINE.

    LOOP AT lt_afo INTO DATA(ls_afo).
      COLLECT ls_afo INTO sum_afo.
    ENDLOOP.
  ENDIF.


  LOOP AT lt_data INTO DATA(ls_data).
    MOVE-CORRESPONDING ls_data TO ct_item.
    CLEAR ls_first.

    READ TABLE sum_afo WITH KEY zppdhd  = ls_data-zppdhd
                  matnr   = ls_data-matnr
                  idnlf   = ls_data-idnlf
                  zcolor  = ls_data-zcolor
                  zsize   = ls_data-zsize
                  znorms  = ls_data-znorms
                  zppflag = ls_data-zppflag.
    IF sy-subrc EQ 0.
      ct_item-menge2 = sum_afo-menge.
    ENDIF.


    LOOP AT lt_stock INTO DATA(ls_stock)
      WHERE zppdhd = ls_data-zppdhd
      AND idnlf = ls_data-idnlf
      AND matnr = ls_data-matnr
      AND zcolor = ls_data-zcolor
      AND zsize = ls_data-zsize
      AND znorms = ls_data-znorms.

      DATA(tabix) = sy-tabix.
      ADD 1 TO ls_first.

      IF ls_first  = 1.

      ELSE.
        CLEAR ct_item-menge1.
        CLEAR ct_item-menge2.
      ENDIF.

      ct_item-zvat_nub = ls_stock-zvat_nub.
      ct_item-menge4 = ls_stock-menge4.
      ct_item-charg = ls_stock-charg.
      ct_item-werks = ls_stock-werks.
      ct_item-lgort = ls_stock-lgort.
      ct_item-remark_date = ls_stock-ersda.
      APPEND ct_item.

      DELETE lt_stock INDEX tabix.

    ENDLOOP.

    IF sy-subrc NE 0.
      APPEND ct_item.
    ENDIF.

    CLEAR ct_item.
    CLEAR ls_data.

  ENDLOOP.

  LOOP AT lt_stock INTO ls_stock.
    MOVE-CORRESPONDING ls_stock TO ct_item.
    APPEND ct_item.
  ENDLOOP.

  LOOP AT ct_item.
    IF ct_item-werks <> '1010'.
      ct_item-werks = '1000'.
    ENDIF.
    IF ct_item-bprme IS INITIAL.
      ct_item-bprme = ct_item-meins.
    ENDIF.

    ct_item-menge3 = ct_item-menge1 - ct_item-menge2.
    ct_item-menge = ct_item-menge3.

    PERFORM f_set_menge_cg IN PROGRAM saplzafo IF FOUND CHANGING ct_item.

    IF ct_item-menge2 >= ct_item-menge1 AND ct_item-menge4 IS INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
       USING 'S' CHANGING ct_item-icon ct_item-text .
    ELSE.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
       USING '' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    MODIFY ct_item.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE icon = icon_complete.
  ENDIF.
ENDFORM.


FORM frm_ref_h TABLES ct_item STRUCTURE zafo_sitem."
  DATA:ls_first TYPE posnr.
  DATA:lt_ztpp0089 TYPE TABLE OF ztpp0089 WITH HEADER LINE.

  SELECT
    h~zdhdzt,
    h~zppdhd,
    h~zzpino,
    h~zkunnr_mat,
    h~vbeln AS vbeln_va,
    h~posnr AS posnr_va,
    h~zzscgdh AS aufnr,
    h~zwwgys AS lifnr,
    t~name1 AS zshd,
    m~matnr,
    m~werks AS bukrs,
    m~werks,
    m~lgort,
    v~idnlf,
    v~maktx,
    v~meins,
    v~bprme,
    v~zmm_tran_rate,
    c~zcolor,
    c~zsize,
    c~znorms,
    c~zshelves,
    c~zvat_nub,
    m~kalab AS menge4,
    m~charg
    FROM ztpp0089 AS h
    INNER JOIN mska AS m ON h~vbeln = m~vbeln AND h~posnr = m~posnr
    INNER JOIN zmmv0010 AS v ON m~matnr = v~matnr
    INNER JOIN ztpp0093 AS t ON t~zwerks = h~zwerks
    INNER JOIN zmch1 AS c ON m~charg = c~charg
    WHERE m~werks IN @s_werks
    AND m~lgort <> '1009' AND m~lgort IN @s_lgort
    AND h~zppdhd IN @s_zppdhd
    AND h~zzpino IN @s_zzpino
    AND v~idnlf IN @s_idnlf
    AND v~zcate1 IN @s_cate1
    AND v~zcate2 IN @s_cate2
    AND v~maktx IN @s_maktx
    AND m~kalab > 0
    AND c~zppflag = ''
    INTO TABLE @DATA(lt_stock).

  IF sy-subrc EQ 0.
    SELECT  vbeln,posnr,matnr,bwkey,salk3,lbkum
      FROM ebew
      FOR ALL ENTRIES IN @lt_stock
      WHERE vbeln = @lt_stock-vbeln_va
      AND posnr = @lt_stock-posnr_va
      AND matnr = @lt_stock-matnr
      AND bwkey = @lt_stock-werks
      INTO TABLE @DATA(lt_ebew).
  ENDIF.

  SELECT
    h~zdhdzt,
    h~zppdhd,
    h~zzpino,
    h~zkunnr_mat,
    h~vbeln AS vbeln_va,
    h~posnr AS posnr_va,
    h~zzscgdh AS aufnr,
    h~werks ,
    h~werks AS bukrs ,
    h~zwwgys AS lifnr,
    t~name1 AS zshd,
    i~zzbom_item,
    v~idnlf,
    v~bprme,
    v~zmm_tran_rate,
    i~matnr,
    i~zcolor1 AS zcolor,
    i~zsize,
    i~zzjsssgg AS znorms,
    i~zzdh AS menge1,
    i~zcqy AS zppflag,
    v~maktx,
    v~meins,
    i~zzsfmb
    FROM ztpp0089 AS h
    INNER JOIN ztpp0091 AS i ON h~zppdhd = i~zppdhd
    INNER JOIN ztpp0093 AS t ON t~zwerks = h~zwerks
    INNER JOIN zmmv0010 AS v ON i~matnr = v~matnr
    WHERE h~zppdhd IN @s_zppdhd
    AND h~zzpino IN @s_zzpino
    AND h~werks IN @s_werks
    AND h~loevm = ''
    AND v~idnlf IN @s_idnlf
    AND v~zcate1 IN @s_cate1
    AND v~zcate2 IN @s_cate2
    AND v~maktx IN @s_maktx
    AND zzdh > 0
    AND i~zzwlly = ''
    AND zcqy = ''
    INTO TABLE @DATA(lt_data).

  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE zzsfmb = 'Y'.
    CLEAR <fs_data>-znorms.
    CLEAR <fs_data>-zppflag.
  ENDLOOP.

  IF sy-subrc EQ 0.
    SELECT
      h~bustyp  ,
      i~umlgo,
      i~zzpino  ,
      i~zppdhd  ,
      i~vbeln_va  ,
      i~posnr_va  ,
      i~matnr   ,
      i~maktx   ,
      i~idnlf   ,
      i~zcolor  ,
      i~zsize   ,
      i~znorms  ,
      i~zppflag ,
      i~zvat_nub ,
      i~bukrs ,
      i~werks ,
      i~aufnr ,
      i~meins ,
      i~bprme,
      i~zmm_tran_rate,
      i~menge
       FROM zafo_head AS h
      INNER JOIN zafo_item AS i ON h~afono = i~afono
       FOR ALL ENTRIES IN @lt_data
       WHERE i~zppdhd = @lt_data-zppdhd
         AND i~matnr = @lt_data-matnr
         AND i~idnlf = @lt_data-idnlf
         AND i~zcolor = @lt_data-zcolor
         AND i~zsize = @lt_data-zsize
         AND i~znorms = @lt_data-znorms
         AND i~zppflag = @lt_data-zppflag
         AND h~bustyp IN ('R4011','R2002','R2003','C4001','R1013','R1014','R1015','R1019','ASN02','ASN03')
         AND h~del_flag <> 'X'
         AND i~del_flag <> 'X'
         AND h~status <> 'D'
      INTO TABLE @DATA(lt_afo).

    DELETE lt_afo WHERE bustyp = 'C4001' AND umlgo <> '1009'.

    DATA:sum_afo TYPE TABLE OF ty_afo WITH HEADER LINE.

    LOOP AT lt_afo INTO DATA(ls_afo)." 退料
      IF ls_afo-bustyp = 'R2002'.
        ls_afo-menge = - ls_afo-menge.
      ENDIF.
      MOVE-CORRESPONDING ls_afo TO sum_afo.
      CLEAR sum_afo-bustyp.
      COLLECT sum_afo INTO sum_afo.
    ENDLOOP.

  ENDIF.

  CLEAR lt_ztpp0089[].
  LOOP AT lt_data INTO DATA(ls_data).

    lt_ztpp0089-zppdhd = ls_data-zppdhd.
    APPEND lt_ztpp0089.
    CLEAR lt_ztpp0089.

    MOVE-CORRESPONDING ls_data TO ct_item.
    CLEAR ls_first.

    READ TABLE sum_afo WITH KEY zppdhd  = ls_data-zppdhd
                                matnr   = ls_data-matnr
                                idnlf   = ls_data-idnlf
                                zcolor  = ls_data-zcolor
                                zsize   = ls_data-zsize
                                znorms  = ls_data-znorms
                                zppflag = ls_data-zppflag.
    IF sy-subrc EQ 0.
      ct_item-menge2 = sum_afo-menge." 已发料数
    ENDIF.

    LOOP AT lt_stock INTO DATA(ls_stock) WHERE zppdhd = ls_data-zppdhd
                                            AND idnlf = ls_data-idnlf
                                            AND matnr = ls_data-matnr
                                            AND zcolor = ls_data-zcolor
                                            AND zsize = ls_data-zsize
                                            AND znorms = ls_data-znorms.
      DATA(tabix) = sy-tabix.

      ADD 1 TO ls_first.

      IF ls_first  = 1.
      ELSE.
        CLEAR ct_item-menge1.
        CLEAR ct_item-menge2.
      ENDIF.

      ct_item-zvat_nub = ls_stock-zvat_nub.
      ct_item-menge4 = ls_stock-menge4.

      ct_item-charg = ls_stock-charg.
      ct_item-werks = ls_stock-werks.
      ct_item-lgort = ls_stock-lgort.
      ct_item-zshelves = ls_stock-zshelves.
      ct_item-zshd = ls_stock-zshd.

      APPEND ct_item.

      DELETE lt_stock INDEX tabix.

    ENDLOOP.

    IF sy-subrc NE 0.
      APPEND ct_item.
      CLEAR ct_item.
    ENDIF.

    CLEAR ct_item.
    CLEAR ls_data.
  ENDLOOP.


  LOOP AT lt_stock INTO ls_stock.

    lt_ztpp0089-zppdhd = ls_stock-zppdhd.
    APPEND lt_ztpp0089.
    CLEAR lt_ztpp0089.
    MOVE-CORRESPONDING ls_stock TO ct_item.

    APPEND ct_item.
    CLEAR ct_item.

  ENDLOOP.

  SORT lt_ztpp0089 BY zppdhd.
  DELETE ADJACENT DUPLICATES FROM lt_ztpp0089 COMPARING zppdhd.
  IF lt_ztpp0089[] IS NOT INITIAL.
    SELECT * FROM ztpp0089
      FOR ALL ENTRIES IN @lt_ztpp0089
      WHERE zppdhd = @lt_ztpp0089-zppdhd
      INTO TABLE @lt_ztpp0089.
    SORT lt_ztpp0089 BY zppdhd.
  ENDIF.


  DATA:lt_afpo TYPE TABLE OF ztpp0089 WITH HEADER LINE.
  lt_afpo[] = lt_ztpp0089[].
  DELETE lt_afpo WHERE zzscgdh IS INITIAL.
  IF lt_afpo[] IS NOT INITIAL.
    SELECT p~aufnr,p~dwerk,c~name1 INTO TABLE @DATA(lt_scd)
    FROM afpo AS p
    LEFT JOIN ztpp0093 AS c ON c~werks = p~dwerk
    FOR ALL ENTRIES IN @lt_afpo
    WHERE p~aufnr = @lt_afpo-zzscgdh.
  ENDIF.


  LOOP AT ct_item .
    ct_item-umwrk = ct_item-werks.
    ct_item-ummat = ct_item-matnr.
    ct_item-zcolor_um = ct_item-zcolor.
    ct_item-zsize_um = ct_item-zsize.
    ct_item-znorms_um = ct_item-znorms.
    ct_item-zppflag_um = ct_item-zppflag.
    ct_item-zshelves_um = ct_item-zshelves.
    ct_item-zvat_nub_um = ct_item-zvat_nub.
    ct_item-umlgo = '1009'.
    ct_item-umcha = ct_item-charg.

    ct_item-menge3 = ct_item-menge1 - ct_item-menge2." 未清数量
    ct_item-menge5 = ct_item-menge2 + ct_item-menge4." 已入库数

    CONDENSE ct_item-zmm_tran_rate NO-GAPS.
    IF ct_item-zmm_tran_rate IS INITIAL OR ct_item-zmm_tran_rate = '0.000'.
      ct_item-zmm_tran_rate = 1.
    ENDIF.

    ct_item-menge1_bj = ct_item-menge1 / ct_item-zmm_tran_rate.
    ct_item-menge3_bj = ct_item-menge3 / ct_item-zmm_tran_rate.
    ct_item-menge4_bj = ct_item-menge4 / ct_item-zmm_tran_rate.

    CLEAR ct_item-icon.

    READ TABLE lt_ztpp0089 WITH KEY zppdhd = ct_item-zppdhd BINARY SEARCH.
    IF sy-subrc = 0.
      ct_item-bukrs = lt_ztpp0089-werks.
      ct_item-aufnr = lt_ztpp0089-zzscgdh.
    ELSE.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-017."'大货单不存在'.
    ENDIF.

    IF lt_ztpp0089-zzscgdh IS NOT INITIAL .
      READ TABLE lt_scd INTO DATA(ls_scd) WITH  KEY aufnr = lt_ztpp0089-zzscgdh." 以生产工单的工厂为准
      IF sy-subrc = 0.
        lt_ztpp0089-werks = ls_scd-dwerk.
        ct_item-bukrs = ls_scd-dwerk.
        ct_item-zshd = ls_scd-name1.
      ENDIF.
    ENDIF.

    IF ct_item-menge4 IS INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'S' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    IF ct_item-icon IS INITIAL AND ct_item-werks = lt_ztpp0089-werks AND gs_bustyp-execute_type = 'CC'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-018."'跨公司业务公司不能一致'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND ct_item-werks+0(1) <> lt_ztpp0089-werks+0(1) AND gs_bustyp-execute_type = 'CC'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-019."'跨公司业务公司不能一致'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND  ct_item-werks <> lt_ztpp0089-werks AND gs_bustyp-execute_type = 'MB'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-021."'发料公司必须一致'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND ( lt_ztpp0089-zdhdsl <= 0
      OR lt_ztpp0089-loevm = 'X'
      OR lt_ztpp0089-zdhdzt = 'D' ).
      ct_item-icon = icon_led_yellow.
      ct_item-text = '大货单已删除'."'大货单已删除.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'A'.
      ct_item-icon = icon_led_yellow.
      ct_item-text = '大货单未下达'."'大货单未下达
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zzscgdh IS INITIAL.
      ct_item-icon = icon_led_yellow.
      ct_item-text = '工单不存在'."'生产工单不存在.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'Y'.
      ct_item-icon = icon_led_yellow.
      ct_item-text = '大货单已关闭'."'大货单已关闭'.
      DATA:lv_objnr LIKE  jest-objnr.
      DATA:t_status TYPE TABLE OF jstat WITH HEADER LINE.
      CONCATENATE 'OR' lt_ztpp0089-zzscgdh INTO lv_objnr.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          client           = sy-mandt
          objnr            = lv_objnr
        TABLES
          status           = t_status
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      LOOP AT t_status WHERE stat = 'I0056' AND inact IS INITIAL.
        IF t_status-stat = 'I0056'.
          ct_item-icon = icon_led_yellow.
          ct_item-text = '工单已结算'.
        ELSEIF t_status-stat = 'I0045'.
          ct_item-icon = icon_led_yellow.
          ct_item-text = '工单已关闭'."'生产工单已关闭'.
        ELSEIF t_status-stat = 'I0001'.
          ct_item-icon = icon_led_yellow.
          ct_item-text = '工单未下达'."'生产工单未下达.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF ct_item-icon IS INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    MODIFY ct_item.

  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE icon = icon_complete.
  ENDIF.

ENDFORM.


FORM frm_ref_f TABLES ct_item STRUCTURE zafo_sitem. "生产发料
  DATA:lt_ztpp0089 TYPE TABLE OF ztpp0089 WITH HEADER LINE.

  SELECT
    h~zdhdzt,
    h~zyxfl,
    h~zppdhd,
    h~zzpino,
    h~zkunnr_mat,
    h~vbeln AS vbeln_va,
    h~posnr AS posnr_va,
    h~zzscgdh AS aufnr,
    h~zwwgys AS lifnr,
    t~name1 AS zshd,
    m~matnr,
    m~werks,
    m~lgort,
    v~idnlf,
    v~maktx,
    v~meins,
    v~bprme,
    v~zmm_tran_rate,
    c~zcolor,
    c~zsize,
    c~znorms,
    c~zshelves,
    c~zvat_nub,
    m~kalab AS menge4,
    m~charg
    FROM ztpp0089 AS h
    INNER JOIN mska AS m ON h~vbeln = m~vbeln AND h~posnr = m~posnr
    INNER JOIN zmmv0010 AS v ON m~matnr = v~matnr
    INNER JOIN ztpp0093 AS t ON t~zwerks = h~zwerks
    INNER JOIN zmch1 AS c ON m~charg = c~charg
    WHERE m~werks IN @s_werks
      AND m~lgort IN @s_lgort
      AND h~zppdhd IN @s_zppdhd
      AND h~zzpino IN @s_zzpino
      AND v~idnlf IN @s_idnlf
      AND v~zcate1 IN @s_cate1
      AND v~zcate2 IN @s_cate2
      AND v~maktx IN @s_maktx
      AND m~kalab > 0
      AND c~zppflag = ''
    INTO TABLE @DATA(lt_stock).

  IF sy-subrc EQ 0.
    SELECT
      h~bustyp  ,
      i~zzpino  ,
      i~zppdhd  ,
      i~vbeln_va  ,
      i~posnr_va  ,
      i~matnr   ,
      i~maktx   ,
      i~idnlf   ,
      i~zcolor  ,
      i~zsize   ,
      i~znorms  ,
      i~zppflag ,
      i~zvat_nub ,
      i~bukrs ,
      i~werks ,
      i~aufnr ,
      i~meins ,
      i~bprme,
      i~zmm_tran_rate,
      i~menge
       FROM zafo_head AS h
      INNER JOIN zafo_item AS i ON h~afono = i~afono
       FOR ALL ENTRIES IN @lt_stock
       WHERE i~zppdhd = @lt_stock-zppdhd
       AND   i~matnr = @lt_stock-matnr
       AND   i~idnlf = @lt_stock-idnlf
       AND   i~zcolor = @lt_stock-zcolor
       AND   i~zsize = @lt_stock-zsize
       AND   i~znorms = @lt_stock-znorms
       AND h~bustyp IN ('R2001','R2002','R2003')
       AND h~del_flag <> 'X'
       AND i~del_flag <> 'X'
       AND ( h~status = 'S' OR h~status = 'A' OR h~status = 'T' )
    INTO TABLE @DATA(lt_afo).

    DATA:sum_afo TYPE TABLE OF ty_afo WITH HEADER LINE.

    LOOP AT lt_afo INTO DATA(ls_afo)." 退料
      IF ls_afo-bustyp = 'R2002'.
        ls_afo-menge = - ls_afo-menge.
      ENDIF.
      MOVE-CORRESPONDING ls_afo TO sum_afo.
      CLEAR sum_afo-bustyp.
      COLLECT sum_afo INTO sum_afo.
    ENDLOOP.

  ENDIF.

  CLEAR lt_ztpp0089[].
  LOOP AT lt_stock INTO DATA(ls_stock).

    lt_ztpp0089-zppdhd = ls_stock-zppdhd.
    APPEND lt_ztpp0089.
    CLEAR lt_ztpp0089.
    MOVE-CORRESPONDING ls_stock TO ct_item.

    READ TABLE sum_afo WITH KEY zppdhd  = ls_stock-zppdhd
                                 matnr   = ls_stock-matnr
                                 idnlf   = ls_stock-idnlf
                                 zcolor  = ls_stock-zcolor
                                 zsize   = ls_stock-zsize
                                 znorms  = ls_stock-znorms.
    IF sy-subrc EQ 0.
      ct_item-menge2 = sum_afo-menge." 已发料数
    ENDIF.
    ct_item-zvat_nub = ls_stock-zvat_nub.
    ct_item-menge4 = ls_stock-menge4.

    ct_item-charg = ls_stock-charg.
    ct_item-werks = ls_stock-werks.
    ct_item-lgort = ls_stock-lgort.
    ct_item-zshelves = ls_stock-zshelves.
    ct_item-zshd = ls_stock-zshd.

    APPEND ct_item.
    CLEAR ct_item.
  ENDLOOP.


  SORT lt_ztpp0089 BY zppdhd.
  DELETE ADJACENT DUPLICATES FROM lt_ztpp0089 COMPARING zppdhd.
  IF lt_ztpp0089[] IS NOT INITIAL.
    SELECT * FROM ztpp0089
      FOR ALL ENTRIES IN @lt_ztpp0089
      WHERE zppdhd = @lt_ztpp0089-zppdhd
      INTO TABLE @lt_ztpp0089.
    SORT lt_ztpp0089 BY zppdhd.
  ENDIF.

  DATA:lt_afpo TYPE TABLE OF ztpp0089 WITH HEADER LINE.
  lt_afpo[] = lt_ztpp0089[].
  DELETE lt_afpo WHERE zzscgdh IS INITIAL.
  IF lt_afpo[] IS NOT INITIAL.
    SELECT p~aufnr,p~dwerk,c~name1 INTO TABLE @DATA(lt_scd)
    FROM afpo AS p
    LEFT JOIN ztpp0093 AS c ON c~werks = p~dwerk
    FOR ALL ENTRIES IN @lt_afpo
    WHERE p~aufnr = @lt_afpo-zzscgdh.
  ENDIF.

  LOOP AT ct_item .

    ct_item-menge3 = ct_item-menge1 - ct_item-menge2." 未清数量
    ct_item-menge5 = ct_item-menge2 + ct_item-menge4." 已入库数

    CONDENSE ct_item-zmm_tran_rate NO-GAPS.
    IF ct_item-zmm_tran_rate IS INITIAL OR ct_item-zmm_tran_rate = '0.000'.
      ct_item-zmm_tran_rate = 1.
    ENDIF.

    ct_item-menge3_bj = ct_item-menge3 / ct_item-zmm_tran_rate.
    ct_item-menge4_bj = ct_item-menge4 / ct_item-zmm_tran_rate.

    ct_item-menge = ct_item-menge4.
    ct_item-menge_cg = ct_item-menge4_bj.

    CLEAR ct_item-icon.

    SORT lt_ztpp0089 BY zppdhd.
    READ TABLE lt_ztpp0089 WITH KEY zppdhd = ct_item-zppdhd BINARY SEARCH.
    IF sy-subrc = 0.
      ct_item-bukrs = lt_ztpp0089-werks.
      ct_item-aufnr = lt_ztpp0089-zzscgdh.
    ELSE.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-017."'大货单不存在'.
    ENDIF.

    IF lt_ztpp0089-zzscgdh IS NOT INITIAL .
      READ TABLE lt_scd INTO DATA(ls_scd) WITH  KEY aufnr = lt_ztpp0089-zzscgdh." 以生产工单的工厂为准
      IF sy-subrc = 0.
        lt_ztpp0089-werks = ls_scd-dwerk.
        ct_item-bukrs = ls_scd-dwerk.
        ct_item-zshd = ls_scd-name1.
      ENDIF.
    ENDIF.

    IF ct_item-icon IS INITIAL AND ct_item-werks = lt_ztpp0089-werks AND gs_bustyp-execute_type = 'CC'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-018."'跨公司业务公司不能一致'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND ct_item-werks+0(1) <> lt_ztpp0089-werks+0(1) AND gs_bustyp-execute_type = 'CC'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-019."'跨公司业务公司不能一致'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND ct_item-werks <> lt_ztpp0089-werks AND gs_bustyp-execute_type = 'MB'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-021."'发料公司必须一致'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND ( lt_ztpp0089-zdhdsl <= 0
                                   OR lt_ztpp0089-loevm = 'X'
                                   OR lt_ztpp0089-zdhdzt = 'D' ).
      ct_item-icon = icon_led_red.
      ct_item-text = '大货单已删除'."'大货单已删除.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'A'.
      ct_item-icon = icon_led_yellow.
      ct_item-text = '大货单未下达'."'大货单未下达
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zzscgdh IS INITIAL.
      ct_item-icon = icon_led_red.
      ct_item-text = '工单不存在'."'生产工单不存在.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'Y'.
      ct_item-icon = icon_led_red.
      ct_item-text = '大货单已关闭'."'大货单已关闭'.

      DATA:lv_objnr LIKE jest-objnr.
      DATA:t_status TYPE TABLE OF jstat WITH HEADER LINE.
      CONCATENATE 'OR' lt_ztpp0089-zzscgdh INTO lv_objnr.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          client           = sy-mandt
          objnr            = lv_objnr
        TABLES
          status           = t_status
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      LOOP AT t_status WHERE stat = 'I0056' AND inact IS INITIAL.
        IF t_status-stat = 'I0056'.
          ct_item-icon = icon_led_red.
          ct_item-text = '工单已结算'.
        ELSEIF t_status-stat = 'I0045'.
          ct_item-icon = icon_led_red.
          ct_item-text = '工单已关闭'."'生产工单已关闭'.
        ELSEIF t_status-stat = 'I0001'.
          ct_item-icon = icon_led_red.
          ct_item-text = '工单未下达'."'生产工单未下达.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF ct_item-icon IS INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    MODIFY ct_item.

  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE icon = icon_led_yellow.
  ENDIF.

ENDFORM.


FORM frm_ref_g TABLES ct_item STRUCTURE zafo_sitem. "质检收货

  SELECT
   h~bukrs,
   i~lifnr,
   i~lifnr_name,
   i~afono,
   i~afonr,
   i~qcno,
   i~qcnr,
   i~qc_status,
   i~zppdhd,
   i~zzpino,
   i~werks,
   i~lgort,
   i~vbeln_va,
   i~posnr_va,
   i~ebeln,
   i~ebelp,
   i~idnlf,
   i~matnr,
   i~zcolor,
   i~zcolor_text,
   i~zsize,
   i~znorms ,
   i~zmm_tran_rate ,
   i~zppflag ,
   i~zvat_nub ,
   i~menge AS menge1 ,
   i~meins,
   i~bprme,
   i~peinh,
   i~maktx,
   i~amount,
   i~price,
   i~price_long,
   h~remark2,
   h~remark_date,
   i~item_status
   FROM zafo_head AS h
   INNER JOIN zafo_item AS i ON h~afono = i~afono
   INNER JOIN ekko AS k ON i~ebeln = k~ebeln
   WHERE i~werks IN @s_werks
   AND k~ekgrp IN @s_ekgrp
   AND i~zppdhd IN @s_zppdhd
   AND i~zzpino IN @s_zzpino
   AND i~lifnr IN @s_lifnr
   AND i~lifnr_name IN @s_zname1
   AND i~ebeln IN @s_ebeln
   AND i~qcno IN @s_qcno
   AND i~afono IN @s_afono
   AND h~del_flag <> 'X'
   AND h~status <> 'D' AND h~status <> 'F'
   AND i~item_status <> 'F' AND i~item_status <> 'T'
   AND h~ernam IN @s_ernam
   AND h~erdat IN @s_erdat
   AND h~budat IN @s_budat
   AND i~menge <> 0
*    AND i~qc_status <> ''
*    AND i~qc_status <> 'A'
   AND h~bustyp = 'R1001'
   INTO TABLE @DATA(lt_data).

  CHECK sy-subrc EQ 0.

  SORT lt_data BY qcno qcnr.

  SELECT * FROM zafo_qc_head
    FOR ALL ENTRIES IN @lt_data
    WHERE qcno = @lt_data-qcno
*     AND qcnr = @lt_data-qcnr
    INTO  TABLE @DATA(lt_qc_head).

  DATA:lt_afo_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:sum_afo_item LIKE TABLE OF zafo_sitem WITH HEADER LINE.

  SELECT h~status AS icon, i~*
    FROM zafo_item AS i
    INNER JOIN zafo_head AS h ON h~afono = i~afono
    FOR ALL ENTRIES IN @lt_data
    WHERE i~qcno = @lt_data-qcno
    AND bustyp = 'R1002'
    AND h~status IN ( 'A','S','T' )
    INTO CORRESPONDING FIELDS OF TABLE @lt_afo_item.

  IF sy-subrc EQ 0.
    LOOP AT lt_afo_item INTO DATA(ls_afo_item).
      sum_afo_item-icon = ls_afo_item-icon.
      sum_afo_item-qcno = ls_afo_item-qcno.
      sum_afo_item-zppdhd = ls_afo_item-zppdhd.
      sum_afo_item-afono_ref = ls_afo_item-afono_ref.
      sum_afo_item-matnr = ls_afo_item-matnr.
      sum_afo_item-idnlf = ls_afo_item-idnlf.
      sum_afo_item-zcolor = ls_afo_item-zcolor.
      sum_afo_item-zsize = ls_afo_item-zsize.
      sum_afo_item-znorms = ls_afo_item-znorms.
      sum_afo_item-zppflag = ls_afo_item-zppflag.

*       sum_afo_item-zvat_nub = ls_afo_item-zvat_nub.

      sum_afo_item-menge = ls_afo_item-menge.
      sum_afo_item-menge2 = ls_afo_item-menge2." 合格数
      sum_afo_item-menge3 = ls_afo_item-menge3." 不合格数

      COLLECT sum_afo_item.
    ENDLOOP.
  ENDIF.

  SORT lt_qc_head BY qcno.

  DATA:l_menge_qc TYPE menge_d.
  DATA:l_menge_qc_f TYPE menge_d.


  LOOP AT lt_qc_head INTO  DATA(ls_qc_head).
    IF ls_qc_head-zmm_tran_rate IS INITIAL.
      ls_qc_head-zmm_tran_rate = 1.
    ENDIF.

    l_menge_qc = ls_qc_head-menge_qc * ls_qc_head-zmm_tran_rate.
    l_menge_qc_f = ls_qc_head-menge_qc_f * ls_qc_head-zmm_tran_rate.
    LOOP AT lt_data INTO DATA(ls_data) WHERE qcno = ls_qc_head-qcno
                                             AND qcnr = ls_qc_head-qcnr
                                             AND zzpino = ls_qc_head-zzpino
                                             AND zppdhd = ls_qc_head-zppdhd
                                             AND matnr = ls_qc_head-matnr
                                             AND idnlf = ls_qc_head-idnlf
                                             AND zcolor = ls_qc_head-zcolor
                                             AND zsize = ls_qc_head-zsize
                                             AND zvat_nub = ls_qc_head-zvat_nub
                                             AND znorms = ls_qc_head-znorms
                                             AND zppflag = ls_qc_head-zppflag.
      DATA(tabix) = sy-tabix.
      MOVE-CORRESPONDING ls_data TO ct_item.
      ct_item-menge2 = ct_item-menge1 * ls_qc_head-menge_qc / ls_qc_head-menge_gr.
      ct_item-menge3 = ct_item-menge1 * ls_qc_head-menge_qc_f / ls_qc_head-menge_gr.
      l_menge_qc = l_menge_qc - ct_item-menge2.
      l_menge_qc_f = l_menge_qc_f - ct_item-menge3.
      ct_item-afono_ref = ls_data-afono.
      ct_item-afonr_ref = ls_data-afonr.
      APPEND ct_item.
      CLEAR ct_item.
      DELETE lt_data INDEX tabix.
    ENDLOOP.

    IF sy-subrc EQ 0.
      IF l_menge_qc <> 0 OR l_menge_qc_f <> 0.
        READ TABLE ct_item ASSIGNING FIELD-SYMBOL(<cs_item>) WITH KEY qcno = ls_qc_head-qcno  qcnr = ls_qc_head-qcnr.
        IF sy-subrc EQ 0.
          <cs_item>-menge2 = <cs_item>-menge2 + l_menge_qc.
          <cs_item>-menge3 = <cs_item>-menge3 + l_menge_qc_f.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  LOOP AT lt_data INTO ls_data.

    MOVE-CORRESPONDING ls_data TO ct_item.
    ct_item-afono_ref = ls_data-afono.
    ct_item-afonr_ref = ls_data-afonr.
    APPEND ct_item.
    CLEAR ct_item.
  ENDLOOP.

  LOOP AT ct_item ASSIGNING <cs_item>.
    IF <cs_item>-zmm_tran_rate IS INITIAL.
      <cs_item>-zmm_tran_rate = 1.
    ENDIF.
    IF <cs_item>-peinh IS INITIAL OR <cs_item>-peinh = '0.000' OR <cs_item>-peinh = '1.000'.
      <cs_item>-peinh = 1.
    ENDIF.

    CLEAR sum_afo_item.
    READ TABLE sum_afo_item WITH KEY qcno = <cs_item>-qcno
                                     zppdhd = <cs_item>-zppdhd
                                    afono_ref = <cs_item>-afono
                                    matnr = <cs_item>-matnr
                                    idnlf = <cs_item>-idnlf
                                    zcolor = <cs_item>-zcolor
                                    zsize = <cs_item>-zsize
                                    znorms = <cs_item>-znorms
                                    zppflag = <cs_item>-zppflag .

    IF sy-subrc EQ 0.
      <cs_item>-menge4 = sum_afo_item-menge.

    ENDIF.

    <cs_item>-menge5 = <cs_item>-menge2 - <cs_item>-menge4.
    <cs_item>-menge = <cs_item>-menge5.
    <cs_item>-menge_cg = <cs_item>-menge / <cs_item>-zmm_tran_rate.
    IF <cs_item>-menge IS NOT INITIAL.
      <cs_item>-amount =  <cs_item>-price_long * <cs_item>-menge.
*       <cs_item>-price = <cs_item>-amount / <cs_item>-menge2 * <cs_item>-peinh.
*       PERFORM frm_set_price_long IN PROGRAM saplzafo IF FOUND
*        USING ct_item-price ct_item-peinh CHANGING ct_item-price_long.

    ENDIF.


    IF <cs_item>-menge5 <= 0 AND  <cs_item>-menge3 = <cs_item>-menge1.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
       USING 'E' CHANGING <cs_item>-icon <cs_item>-text .
      <cs_item>-text = TEXT-027." '全部不合格'.
    ELSEIF <cs_item>-menge5 <= 0 AND <cs_item>-menge2  = 0.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
       USING 'E' CHANGING <cs_item>-icon <cs_item>-text .
      <cs_item>-text = TEXT-026." '无合格数'.
    ELSEIF <cs_item>-menge5 <= 0 AND <cs_item>-menge2  <> 0.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
       USING 'S' CHANGING <cs_item>-icon <cs_item>-text .
    ELSE.

      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
       USING '' CHANGING <cs_item>-icon <cs_item>-text .
    ENDIF.

    IF <cs_item>-menge1 <=  sum_afo_item-menge3.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
       USING 'S' CHANGING <cs_item>-icon <cs_item>-text .
    ENDIF.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE icon = icon_complete.
  ENDIF.
ENDFORM.


FORM frm_ref_i TABLES ct_item STRUCTURE zafo_sitem. "生产退料

  SELECT h~bustyp  ,
      i~zzpino  ,
      i~zppdhd  ,
      i~vbeln_va  ,
      i~posnr_va  ,
      i~matnr   ,
      i~maktx,
      i~idnlf   ,
      i~zcolor  ,
      i~zsize   ,
      i~znorms  ,
      i~zppflag ,
      i~zvat_nub ,
      i~bukrs ,
      i~werks ,
      i~aufnr ,
      i~meins ,
      i~bprme,
      i~zmm_tran_rate,
      i~menge
    FROM zafo_head AS h
    INNER JOIN zafo_item AS i ON h~afono = i~afono
    WHERE i~werks IN @s_werks
       AND i~zzpino IN @s_zzpino
       AND i~zppdhd IN @s_zppdhd
       AND i~matnr IN @s_matnr
       AND i~idnlf IN @s_idnlf
       AND h~bustyp IN ('R2001')
       AND h~status = 'S'

  INTO TABLE @DATA(lt_afo).

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.
  LOOP AT lt_afo INTO DATA(ls_afo).
    lt_matnr-matnr = ls_afo-matnr.
    COLLECT lt_matnr.
  ENDLOOP.

  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.

  DATA:sum_afo TYPE TABLE OF ty_afo WITH HEADER LINE.

  LOOP AT lt_afo ASSIGNING FIELD-SYMBOL(<fs_afo>).

    READ TABLE lt_matnr WITH KEY matnr = <fs_afo>-matnr.
    IF sy-subrc EQ 0 .
      <fs_afo>-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    IF <fs_afo>-bustyp = 'R2002'.
      <fs_afo>-menge = - <fs_afo>-menge.
    ENDIF.

    CLEAR <fs_afo>-bustyp.

    COLLECT <fs_afo> INTO sum_afo.
  ENDLOOP.


  LOOP AT sum_afo.
    MOVE-CORRESPONDING sum_afo TO ct_item.
    ct_item-menge1 = ct_item-menge.
    ct_item-menge1_bj = ct_item-menge / ct_item-zmm_tran_rate.
    CLEAR ct_item-menge.
    CLEAR ct_item-menge2.
    CLEAR ct_item-menge3.
    CLEAR ct_item-menge4.
    CLEAR ct_item-menge5.
    IF ct_item-menge1 > 0.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
    ELSE.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'E' CHANGING ct_item-icon ct_item-text .
    ENDIF.
    APPEND ct_item.
  ENDLOOP.

  IF p_fin IS INITIAL.
    DELETE ct_item WHERE icon <> icon_led_inactive.
  ENDIF.

ENDFORM.


FORM frm_ref_j TABLES ct_item STRUCTURE zafo_sitem."外采业务
  DATA: lt_zmmv0010 TYPE TABLE OF zmmv0010 WITH HEADER LINE.

  DATA: lv_zsctype TYPE zsctype.

  lv_zsctype = '3'.

  SELECT
   h~ebeln,
   h~zzpino,
   h~zppdhd,
   h~zkunnr_mat,
   h~matnr,
   h~maktx,
   h~werks,
   h~vbeln AS vbeln_va,
   h~posnr AS posnr_va,
   h~kunnr,
   i~zcolor ,
   i~kbmeng AS menge1 ,
   i~kbmeng AS menge  ,
   i~zcolor AS zcolor_text_zh,
   i~zzcmtx AS zsize,
   i~zzsclx AS zppflag,
   k~name1 AS kunnr_name
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
   FROM ztpp0089 AS h
   JOIN ztpp0090 AS i ON h~zppdhd = i~zppdhd
   LEFT JOIN kna1 AS k ON h~kunnr = k~kunnr
   WHERE h~werks IN @s_werks
    AND h~loevm = ''
    AND h~zppdhd IN @s_zppdhd
    AND zzpino IN @s_zzpino
    AND h~kunnr IN @s_kunnr
    AND h~werks = @p_werks
    AND zsctype = @lv_zsctype .

  CHECK ct_item[] IS NOT INITIAL.

  SELECT * FROM zmmv0010
    INTO TABLE @lt_zmmv0010
    FOR ALL ENTRIES IN  @ct_item
    WHERE matnr = @ct_item-matnr.

  LOOP AT ct_item.
    IF ct_item-zppflag = 'D'.
      ct_item-zppflag = 'X'.
    ELSE.
      ct_item-zppflag = ''.
    ENDIF.

    ct_item-bukrs = ct_item-werks.
    ct_item-ekorg = ct_item-werks.

    REPLACE '自制棉杯_棉杯_' WITH '' INTO ct_item-maktx.

    READ TABLE lt_zmmv0010 WITH KEY matnr = ct_item-matnr.
    IF sy-subrc = 0.
      ct_item-meins = lt_zmmv0010-meins.
      ct_item-bprme = lt_zmmv0010-bprme.
      ct_item-zmm_tran_rate = lt_zmmv0010-zmm_tran_rate.
    ENDIF.

    IF ct_item-bprme IS INITIAL .
      ct_item-bprme = ct_item-meins.
    ENDIF.

    ct_item-peinh = 1.
    TRY.
        ct_item-menge_cg = ct_item-menge / ct_item-zmm_tran_rate.
      CATCH cx_sy_zerodivide.
        ct_item-zmm_tran_rate = 1.
        ct_item-menge_cg = ct_item-menge / ct_item-zmm_tran_rate.
    ENDTRY.

    IF ct_item-ebeln IS INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
         USING '' CHANGING ct_item-icon ct_item-text .
    ELSE.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
         USING 'E' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    MODIFY ct_item.
  ENDLOOP.

  IF p_fin IS INITIAL.
    DELETE ct_item WHERE icon <> icon_led_inactive.
  ENDIF.

  LOOP AT ct_item.
    MOVE-CORRESPONDING ct_item TO gt_item_po.
    APPEND gt_item_po.

  ENDLOOP.

ENDFORM.


FORM frm_ref_k TABLES ct_item STRUCTURE zafo_sitem.
  RANGES:s_clabs FOR mchb-clabs.

  CLEAR s_clabs.
  IF p_fin = ''.
    s_clabs-sign = 'I'.
    s_clabs-option = 'NE'.
    s_clabs-low = 0.
    APPEND s_clabs.
  ENDIF.

  SELECT
    c~vbeln AS vbeln_va,
    c~posnr AS posnr_va,
    c~matnr,
    c~werks,
    c~werks AS bukrs,
    c~lgort,
    c~charg,
    kalab AS menge4,
    a~idnlf,
    a~maktx,
    a~meins,
    a~bprme,
    a~zmm_tran_rate,
    b~sgt_scat,
    b~zcolor,
    b~zcolor_text,
    b~zsize,
    b~znorms,
    b~zshelves,
    b~zvat_nub,
*     B~LIFNR,
    k~kunnr,
    p~zzpino,
    p~zppdhd,
    b~zppflag

    FROM mska AS c
    INNER JOIN zvmat AS a ON c~matnr =  a~matnr
    INNER JOIN vbak AS k ON c~vbeln = k~vbeln
    INNER JOIN ztpp0089 AS p ON c~vbeln = p~vbeln AND c~posnr = p~posnr
    LEFT JOIN zmch1 AS b ON c~charg = b~charg
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    WHERE c~werks = @p_werks
         AND a~matnr IN @s_matnr
*           AND C~VBELN IN @S_VBELN
*           AND C~POSNR IN @S_POSNR
          AND a~mtart IN @s_mtart
          AND p~zzpino IN @s_zzpino
          AND p~zppdhd IN @s_zppdhd
          AND a~idnlf IN @s_idnlf
          AND c~werks = @p_werks
          AND c~lgort IN @s_lgort
*           AND c~charg IN @s_charg
          AND kalab IN @s_clabs.

  IF s_zzpino[] IS  INITIAL AND s_zppdhd[] IS  INITIAL.
    SELECT
        c~matnr,
        c~werks,
        c~werks AS bukrs,
        c~werks AS umwrk,
        c~lgort,
        c~charg,
        clabs AS menge4,
        a~idnlf,
        a~maktx,
        a~meins,
        a~bprme,
        a~zmm_tran_rate,
        b~sgt_scat,
        b~zcolor,
        b~zcolor_text,
        b~zsize,
        b~znorms,
        b~zshelves,
        b~zvat_nub
    FROM mchb AS c
    INNER JOIN zvmat AS a ON c~matnr =  a~matnr
    LEFT JOIN zmch1 AS b ON c~charg = b~charg
    APPENDING CORRESPONDING FIELDS OF TABLE @ct_item
    WHERE a~matnr IN @s_matnr
    AND a~mtart IN @s_mtart
    AND c~werks = @p_werks
    AND a~idnlf IN @s_idnlf
    AND c~lgort IN @s_lgort
    AND clabs IN @s_clabs.
  ENDIF.

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.
  LOOP AT ct_item INTO DATA(cs_item).
    lt_matnr-matnr = cs_item-matnr.
    COLLECT lt_matnr.
  ENDLOOP.
  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.

  LOOP AT ct_item.

    IF gs_bustyp-execute_type = 'CC'.
      ct_item-umwrk = ct_item-bukrs.
    ELSE.
      ct_item-bukrs = ct_item-werks.
      ct_item-umwrk = ct_item-werks.
    ENDIF.

    ct_item-ummat = ct_item-matnr.
    ct_item-zcolor_um = ct_item-zcolor.
    ct_item-zsize_um = ct_item-zsize.
    ct_item-znorms_um = ct_item-znorms.
    ct_item-zppflag_um = ct_item-zppflag.
    ct_item-zshelves_um = ct_item-zshelves.
    ct_item-zvat_nub_um = ct_item-zvat_nub.
    READ TABLE lt_matnr WITH KEY matnr = ct_item-matnr.
    IF sy-subrc EQ 0 .
      ct_item-bprme = lt_matnr-bprme.
      ct_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    ct_item-menge4_bj = ct_item-menge4 / ct_item-zmm_tran_rate.
    PERFORM frm_set_round USING ct_item-bprme CHANGING ct_item-menge4_bj.

    IF ct_item-menge4 IS NOT INITIAL.

      ct_item-menge = ct_item-menge4.

      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
    ELSE.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'E' CHANGING ct_item-icon ct_item-text .
      ct_item-text = '无库存'.
    ENDIF.

    MODIFY ct_item.
  ENDLOOP.

ENDFORM.


FORM frm_ref_y TABLES ct_item STRUCTURE zafo_sitem. "过账单据引用

  CHECK gs_bustyp-bustyp_ref IS NOT INITIAL.

  DATA:ls_item TYPE zafo_sitem.

  SELECT h~status AS icon, i~*
    FROM zafo_head AS h
    INNER JOIN zafo_item AS i ON h~afono =  i~afono
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    WHERE bustyp = @gs_bustyp-bustyp_ref
    AND h~afono IN @s_afono
    AND h~ernam IN @s_ernam
    AND ( h~werks IN @s_werks OR h~umwrk IN @s_werks )
    AND i~umcha IN @s_lgort
    AND h~erdat IN @s_erdat
    AND h~budat IN @s_budat
    AND h~status IN @s_status
    AND i~del_flag = ''.

  LOOP AT ct_item.

    ls_item = ct_item.
    IF ct_item-umwrk IS NOT INITIAL.
      ct_item-bukrs = ct_item-umwrk.
    ENDIF.

    CLEAR ct_item-umcha .
    ct_item-charg  = ls_item-umcha.
    ct_item-menge1 = ct_item-menge.

    CASE ct_item-icon.
      WHEN 'S'.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
         USING 'S'  CHANGING ct_item-icon ct_item-text .
      WHEN 'T'.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
         USING 'T'  CHANGING ct_item-icon ct_item-text .
      WHEN OTHERS.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
         USING 'E'  CHANGING ct_item-icon ct_item-text .
    ENDCASE.

    MODIFY ct_item.
  ENDLOOP.

  IF p_fin IS INITIAL.
    DELETE ct_item WHERE icon <> icon_complete.
  ENDIF.

ENDFORM.


FORM frm_ref_q TABLES ct_item STRUCTURE zafo_sitem. "徳誉采购
  DATA: lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA: it_item TYPE TABLE OF zafo_item WITH HEADER LINE.
  DATA:ls_menge TYPE menge_d.


  SELECT h~zzbukrs_cg," 采购公司
    h~zzcepno,"内部订单号
    h~lifnr," 供应商
    h~zzcgbb," 采购币别
    i~zzdycghtno,
    i~zzdycghtnoxm,
    i~matnr,
    i~zzkppm,
    i~zzwpmc,
    i~zzywpm,
    i~zzsupl_kh,
    i~meins,
    i~zzsupl_cl," 中文颜色
    i~zsize,
    i~zzdygg,
    i~zzcgdj,
    i~zzcgje,
    i~zzcgsl,
    i~zzdat_supl
    INTO TABLE @DATA(lt_data)
    FROM zsddycght_hed AS h
    LEFT JOIN zsddycght_itm AS i ON i~zzdycghtno = h~zzdycghtno
    WHERE h~vtweg = '20'" 内销
    AND lifnr <> ''
    AND i~zzcgsl > 0
    AND i~matnr <> ''
    AND h~zzbukrs_cg IN @s_werks.

  LOOP AT lt_data INTO DATA(ls_data).
    lt_item-zzdycghtno = ls_data-zzdycghtno.
    lt_item-zzdycghtnoxm =  ls_data-zzdycghtnoxm.
    lt_item-werks = ls_data-zzbukrs_cg.
    lt_item-bukrs = ls_data-zzbukrs_cg.
    lt_item-ekorg = ls_data-zzbukrs_cg.
    lt_item-remark1 = ls_data-zzcepno.
    lt_item-lifnr = ls_data-lifnr.
    lt_item-eeind = ls_data-zzdat_supl.
    lt_item-waers = ls_data-zzcgbb.

    lt_item-idnlf = ls_data-zzsupl_kh.
    lt_item-matnr = ls_data-matnr.
    lt_item-maktx = ls_data-zzwpmc.
    lt_item-maktx_kp = ls_data-zzkppm.
    lt_item-maktx_en = ls_data-zzywpm.
    lt_item-zcolor_text = ls_data-zzsupl_cl.
    lt_item-zsize = ls_data-zsize.
    lt_item-znorms = ls_data-zzdygg.

    lt_item-meins = ls_data-meins.

    lt_item-price_long  = ls_data-zzcgdj.
    PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND CHANGING lt_item.
    lt_item-menge1 = ls_data-zzcgsl.
    APPEND lt_item.
  ENDLOOP.

  CHECK lt_item[] IS NOT INITIAL.

  SELECT * INTO TABLE @it_item
    FROM zafo_item
    FOR ALL ENTRIES IN @lt_item
    WHERE zzdycghtno = @lt_item-zzdycghtno
    AND zzdycghtnoxm =  @lt_item-zzdycghtnoxm
    AND del_flag <> 'X'.

  SELECT DISTINCT partner,zname1
    FROM zscmt0010
    FOR ALL ENTRIES IN @lt_item
  WHERE partner = @lt_item-lifnr
  INTO TABLE @DATA(lt_zscmt0010).

  SELECT * FROM zmmv0010
    FOR ALL ENTRIES IN @lt_item
  WHERE matnr = @lt_item-matnr
  INTO TABLE @DATA(lt_zmmv0010).

  SELECT DISTINCT zcolor,zcolor_text
    FROM zvcolor
  INTO TABLE @DATA(lt_zcolor)
    FOR ALL ENTRIES IN @lt_item
    WHERE zcolor EQ @lt_item-zcolor_text
    AND spras = @sy-langu.

  LOOP AT lt_item.

    MOVE-CORRESPONDING lt_item TO ct_item.

    ct_item-icon = icon_led_inactive.
    ct_item-text = TEXT-010."'初始'.

    LOOP AT it_item WHERE zzdycghtno = ct_item-zzdycghtno
                   AND zzdycghtnoxm =  ct_item-zzdycghtnoxm.
      ct_item-menge2 = ct_item-menge2 + it_item-menge.
    ENDLOOP.


    READ TABLE lt_zscmt0010 INTO DATA(ls_zscmt0010) WITH KEY partner = ct_item-lifnr.
    IF sy-subrc EQ 0 .
      ct_item-lifnr_name = ls_zscmt0010-zname1.
    ENDIF.

    READ TABLE lt_zcolor INTO DATA(ls_zcolor) WITH KEY zcolor_text = ct_item-zcolor_text.
    IF sy-subrc = 0.
      ct_item-zcolor = ls_zcolor-zcolor.
    ENDIF.

    READ TABLE lt_zmmv0010 INTO DATA(ls_zmmv0010) WITH KEY matnr = ct_item-matnr.
    IF sy-subrc = 0.
      ct_item-meins = ls_zmmv0010-meins.
      ct_item-zmm_tran_rate = ls_zmmv0010-zmm_tran_rate.
    ENDIF.

    TRY.
        ct_item-menge_cg = ct_item-menge / ct_item-zmm_tran_rate.
      CATCH cx_sy_zerodivide.
        ct_item-zmm_tran_rate = 1.
        ct_item-menge_cg = ct_item-menge / ct_item-zmm_tran_rate.
    ENDTRY.

    IF ct_item-bprme IS INITIAL .
      ct_item-bprme = ct_item-meins.
    ENDIF.

    ct_item-menge3 = ct_item-menge1 - ct_item-menge2.
    PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND CHANGING ct_item.
    PERFORM frm_po_item_calculation IN PROGRAM saplzafo IF FOUND CHANGING ct_item.
    PERFORM f_set_amount IN PROGRAM saplzafo IF FOUND CHANGING ct_item.

    IF ct_item-menge3 <= 0 .
      ct_item-icon = icon_led_green.
      ct_item-text = TEXT-012."'已完成'.
    ENDIF.


    APPEND ct_item.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE icon = icon_led_green.
  ENDIF.

ENDFORM.



FORM frm_ref_z TABLES ct_item STRUCTURE zafo_sitem. "申请单引用

  CHECK gs_bustyp-bustyp_ref IS NOT INITIAL.

  IF gs_bustyp-bustyp_bak IS INITIAL.
    SELECT h~status AS icon, i~*
      INTO CORRESPONDING FIELDS OF TABLE @ct_item
      FROM zafo_head AS h
      INNER JOIN zafo_item AS i ON h~afono =  i~afono
      WHERE bustyp = @gs_bustyp-bustyp_ref
      AND h~afono IN @s_afono
      AND h~ernam IN @s_ernam
      AND ( h~werks IN @s_werks OR h~umwrk IN @s_werks OR i~werks IN @s_werks )
      AND h~erdat IN @s_erdat
      AND h~budat IN @s_budat
      AND h~status IN @s_status
      AND i~zzpino IN @s_zzpino
      AND i~zppdhd IN @s_zppdhd
      AND h~remark1 IN @s_potext
      AND h~nenam IN @s_nenam
      AND h~ekgrp IN @s_ekgrp
      AND i~del_flag = '' .

  ELSE.

    DATA:BEGIN OF lt_bustyp OCCURS 0,
           bustyp LIKE zafo_bustype-bustyp,
         END OF lt_bustyp.

    SPLIT gs_bustyp-bustyp_bak AT ',' INTO TABLE lt_bustyp.

    SELECT h~status AS icon, i~*
    APPENDING CORRESPONDING FIELDS OF TABLE @ct_item
    FROM zafo_head AS h
     INNER JOIN zafo_item AS i ON h~afono =  i~afono
    FOR ALL ENTRIES IN @lt_bustyp
    WHERE bustyp = @lt_bustyp-bustyp
      AND h~afono IN @s_afono
      AND h~ernam IN @s_ernam
      AND ( h~werks IN @s_werks OR h~umwrk IN @s_werks OR i~werks IN @s_werks )
      AND h~erdat IN @s_erdat
      AND h~budat IN @s_budat
      AND h~status IN @s_status
      AND i~zzpino IN @s_zzpino
      AND i~zppdhd IN @s_zppdhd
      AND h~remark1 IN @s_potext
      AND h~nenam IN @s_nenam
      AND h~ekgrp IN @s_ekgrp
      AND i~del_flag = ''.
  ENDIF.


  LOOP AT ct_item.

    PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING ct_item-icon
                                                   CHANGING ct_item-icon ct_item-text .

    IF gs_bustyp-execute_type = 'PRO' AND gs_bustyp-busref IS NOT INITIAL.
      IF ct_item-afono_ref IS INITIAL .
        IF ct_item-text = TEXT-023."'后续已完成'.
          PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
           USING 'C' CHANGING ct_item-icon ct_item-text .
        ENDIF.
      ELSEIF ct_item-afono_ref IS NOT INITIAL AND ct_item-text = TEXT-024." '已审核'.
        SELECT SINGLE del_flag INTO @DATA(l_del_flag) FROM zafo_item
          WHERE afono = @ct_item-afono_ref
          AND afonr = @ct_item-afonr_ref.
        IF sy-subrc = 0 AND l_del_flag IS INITIAL.
          PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'T'
                                                         CHANGING ct_item-icon ct_item-text .
        ENDIF.
      ENDIF.

    ENDIF.

    IF gs_bustyp-execute_type = 'POP' AND gs_bustyp-busref IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_PRICE_OUTPUT'
        EXPORTING
          input  = ct_item-price_long
        IMPORTING
          output = ct_item-remark3.

      CLEAR ct_item-afono_ref.
      CLEAR ct_item-afonr_ref.

    ENDIF.
    MODIFY ct_item.

  ENDLOOP.


  IF gs_bustyp-execute_type = 'POP'.
    SELECT * FROM zafo_item_po
      INTO CORRESPONDING FIELDS OF TABLE gt_item_po
      FOR ALL ENTRIES IN ct_item
      WHERE afono = ct_item-afono
      AND afonr = ct_item-afonr
      AND loekz = ''.

    SELECT * FROM zafo_item_cost
      INTO CORRESPONDING FIELDS OF TABLE gt_item_cost
      FOR ALL ENTRIES IN ct_item
      WHERE afono = ct_item-afono.

    LOOP AT gt_item_po ASSIGNING <gs_item_po>.
      READ TABLE ct_item WITH KEY afono = <gs_item_po>-afono
                                  afonr = <gs_item_po>-afonr.
      IF sy-subrc EQ 0.
        <gs_item_po>-icon = ct_item-icon.
        <gs_item_po>-text = ct_item-text.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_fin IS INITIAL.
    DELETE ct_item WHERE icon = icon_allow.
  ENDIF.

ENDFORM.


FORM frm_ref_m TABLES ct_item STRUCTURE zafo_sitem. "海外材料收货
  DATA:lt_ztpp0089 TYPE TABLE OF ztpp0089 WITH HEADER LINE.

  SELECT
    c~mat_kdauf AS vbeln_va,
    c~mat_kdpos AS posnr_va,
    c~matnr,
    c~werks,
    c~werks AS bukrs,
    c~lgort,
    c~charg,
    c~menge AS menge1,
    d~long_vgbel AS remark1,
    a~idnlf,
    a~maktx,
    a~meins,
    a~bprme,
    a~zmm_tran_rate,
    b~sgt_scat,
    b~zcolor,
    b~zcolor_text,
    b~zsize,
    b~znorms,
    b~zshelves,
    b~zvat_nub,
    k~kunnr,
    p~zzpino,
    p~zppdhd,
    b~zppflag
  FROM mseg AS c
      LEFT JOIN zcct_vgbel_head AS d ON c~xblnr_mkpf = d~vgbel
      LEFT JOIN zvmat AS a ON c~matnr =  a~matnr
      LEFT JOIN vbak AS k ON c~mat_kdauf = k~vbeln
      LEFT JOIN ztpp0089 AS p ON c~mat_kdauf = p~vbeln AND c~mat_kdpos = p~posnr
      LEFT JOIN zmch1 AS b ON c~charg = b~charg
 INTO CORRESPONDING FIELDS OF TABLE @ct_item
        WHERE c~matnr IN @s_matnr
        AND a~mtart IN @s_mtart
        AND p~zzpino IN @s_zzpino
        AND p~zppdhd IN @s_zppdhd
        AND a~idnlf IN @s_idnlf
        AND c~werks = @p_werks
        AND c~lgort IN @s_lgort
        AND d~long_vgbel IN @s_vgbel
        AND c~bwart = 'Z05'.

  CHECK ct_item[] IS NOT INITIAL.

  SELECT * FROM ztpp0089
    FOR ALL ENTRIES IN @ct_item
    WHERE zppdhd = @ct_item-zppdhd
    INTO TABLE @lt_ztpp0089.

  SELECT
    c~vbeln,
    c~posnr,
    c~matnr,
    c~werks,
    c~lgort,
    c~charg,
    kains AS menge4
    FROM mska AS c
    FOR ALL ENTRIES IN @ct_item
    WHERE c~vbeln = @ct_item-vbeln_va
    AND c~posnr = @ct_item-posnr_va
    AND c~matnr = @ct_item-matnr
    AND c~werks = @ct_item-werks
    AND c~lgort = @ct_item-lgort
    AND c~charg = @ct_item-charg
    AND c~kains > 0
    INTO TABLE @DATA(lt_mska).

  LOOP AT ct_item.
    ct_item-umwrk = ct_item-werks.
    ct_item-ummat = ct_item-matnr.
*     ct_item-umcha = ct_item-charg.
    ct_item-zcolor_um = ct_item-zcolor.
    ct_item-zsize_um = ct_item-zsize.
    ct_item-znorms_um = ct_item-znorms.
    ct_item-zshelves_um = ct_item-zshelves.
    ct_item-zvat_nub_um = ct_item-zvat_nub.
    ct_item-zppflag_um = ct_item-zppflag.
    READ TABLE lt_mska INTO DATA(ls_mska) WITH KEY vbeln = ct_item-vbeln_va
                                                   posnr = ct_item-posnr_va
                                                   matnr = ct_item-matnr
                                                   werks = ct_item-werks
                                                   lgort = ct_item-lgort
                                                   charg = ct_item-charg.
    IF sy-subrc = 0.
      ct_item-menge4 = ls_mska-menge4.
    ENDIF.


    CLEAR ct_item-icon.
    SORT lt_ztpp0089 BY zppdhd.
    READ TABLE lt_ztpp0089 WITH KEY zppdhd = ct_item-zppdhd BINARY SEARCH.
    IF sy-subrc NE 0.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-017."'大货单不存在'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdsl <= 0.
      ct_item-icon = icon_led_red.
      ct_item-text = '大货单已删除'."'大货单已删除.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'A'.
      ct_item-icon = icon_led_yellow.
      ct_item-text = '大货单未下达'."'大货单未下达
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'D'.
      ct_item-icon = icon_led_red.
      ct_item-text = '大货单已删除'."'大货单已删除.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'Y'.
      ct_item-icon = icon_led_red.
      ct_item-text = '大货单已关闭'."'大货单已关闭'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zzscgdh IS INITIAL.
      ct_item-icon = icon_led_red.
      ct_item-text = '工单不存在'."'生产工单不存在.
    ENDIF.

    IF ct_item-icon IS INITIAL.
      DATA:lv_objnr LIKE  jest-objnr.
      DATA:lv_line LIKE  bsvx-sttxt.

      CONCATENATE 'OR' lt_ztpp0089-zzscgdh INTO lv_objnr.

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
        ct_item-icon = icon_led_red.
        ct_item-text = '工单未下达'."'生产工单未下达.
      ENDIF.

      FIND 'TECO' IN lv_line.
      IF sy-subrc EQ 0 .
        ct_item-icon = icon_led_red.
        ct_item-text = '工单已关闭'."'生产工单已关闭'.
      ENDIF.
    ENDIF.

    IF ct_item-icon IS INITIAL.
      IF ct_item-menge4 IS NOT INITIAL.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
      ELSE.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'S' CHANGING ct_item-icon ct_item-text .
      ENDIF.
    ENDIF.

    MODIFY ct_item.

  ENDLOOP.

  IF p_fin IS INITIAL.
    DELETE ct_item WHERE icon = icon_complete.
  ENDIF.

ENDFORM.


FORM frm_ref_w TABLES ct_item STRUCTURE zafo_sitem. "委外加工发料

*step1.查询委外工单未收货完成的
  SELECT
    k~bukrs,
    k~ebeln,
    p~ebelp,
    k~lifnr,
    a~zname1 AS lifnr_name,
    k~waers,
    k~frgrl AS text,
    f~remark1,
    r~matnr,
    t~vbeln AS vbeln_va,
    t~posnr AS posnr_va,
    r~meins,
    p~werks,
    g~name1 AS zshd,
    p~zzpino,
    p~zppdhd,
    p~zppflag,
    p~afnam,
    p~zanln1 AS anln1,
    r~charg,
    c~zcolor,
    c~zsize,
    c~znorms,
    c~zshelves,
    c~zvat_nub,
    r~rsnum,
    r~rspos,
    r~erfmg AS menge1" 计划需求数
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    FROM ekko AS k
    INNER JOIN zafo_head AS f ON k~ebeln = f~afono
    INNER JOIN ekpo AS p ON k~ebeln = p~ebeln
    INNER JOIN resb AS r ON r~ebeln = p~ebeln AND r~ebelp = p~ebelp
    INNER JOIN zscmt0010 AS a ON k~lifnr = a~partner AND k~bukrs = a~bukrs
    INNER JOIN ztpp0089 AS t ON t~zppdhd = p~zppdhd
    LEFT JOIN ztpp0093 AS g ON g~zwerks = t~zwerks
    LEFT JOIN zmch1 AS c ON c~charg = r~charg
    WHERE p~werks = @p_werks
      AND p~matnr IN @s_matnr
      AND p~idnlf IN @s_idnlf
      AND p~mtart IN @s_mtart
      AND p~zppdhd IN @s_zppdhd
      AND p~zzpino IN @s_zzpino
      AND k~ebeln IN @s_ebeln
      AND k~bsart IN @s_bsart
      AND k~ekgrp IN @s_ekgrp
      AND k~lifnr IN @s_lifnr
      AND k~ernam IN @s_pernam
      AND a~zname1 IN @s_zname1
      AND p~loekz = '  '
*      AND p~elikz = ' '
      AND t~loevm = ''.

  CHECK ct_item[] IS NOT INITIAL.

  " 原材料已发料
  SELECT i~afono,i~afonr,i~ebeln,i~ebelp,i~matnr,i~charg,i~menge
    INTO TABLE @DATA(lt_fls)
    FROM zafo_item AS i
    INNER JOIN zafo_head AS h ON h~afono = i~afono
   FOR ALL ENTRIES IN @ct_item
    WHERE h~status IN ( 'S','T' )
      AND h~bustyp = 'R1012'
      AND i~del_flag <> 'X'
      AND i~ebeln = @ct_item-ebeln
      AND i~ebelp = @ct_item-ebelp.

  LOOP AT ct_item ASSIGNING <gs_item>.
    LOOP AT lt_fls INTO DATA(ls_fls) WHERE ebeln = <gs_item>-ebeln
                                       AND ebelp = <gs_item>-ebelp
                                       AND matnr = <gs_item>-matnr
                                       AND charg = <gs_item>-charg.
      <gs_item>-menge2 = <gs_item>-menge2 + ls_fls-menge." 已发料数
    ENDLOOP.
    <gs_item>-menge3 = <gs_item>-menge1 - <gs_item>-menge2." 未清数
  ENDLOOP.

  " 获取库存
  PERFORM frm_get_kc TABLES ct_item.

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.


  LOOP AT ct_item.
    lt_matnr-matnr = ct_item-matnr.
    lt_matnr-meins = ct_item-meins.
    COLLECT lt_matnr.
  ENDLOOP.

  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.

  LOOP AT ct_item.

    IF ct_item-zcolor IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ZCOL1_OUTPUT'
        EXPORTING
          input  = ct_item-zcolor
        IMPORTING
          output = ct_item-zcolor_text.
    ENDIF.

    READ TABLE lt_matnr WITH KEY matnr = ct_item-matnr.
    IF sy-subrc EQ 0 .
      ct_item-maktx = lt_matnr-maktx.
      ct_item-idnlf = lt_matnr-idnlf.
      ct_item-zcate1 = lt_matnr-zcate1.
      ct_item-zcate2 = lt_matnr-zcate2.
      ct_item-zcate3 = lt_matnr-zcate3.
      ct_item-bprme = lt_matnr-bprme.
      ct_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    ct_item-menge2_bj = ct_item-menge2 / ct_item-zmm_tran_rate.
    ct_item-menge3_bj = ct_item-menge3 / ct_item-zmm_tran_rate.
    ct_item-menge4_bj = ct_item-menge4 / ct_item-zmm_tran_rate.

    IF ct_item-text = 'X'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-009."'未审批'.
    ELSE.
      IF ct_item-menge3 <= 0.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'S' CHANGING ct_item-icon ct_item-text .
      ELSE.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
      ENDIF.
    ENDIF.

    MODIFY ct_item.

  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE icon = icon_complete .
  ENDIF.

ENDFORM.


FORM frm_ref_u TABLES ct_item STRUCTURE zafo_sitem
                      ct_item_po STRUCTURE zafo_sitem_po. "
  DATA:lt_ztpp0089 TYPE TABLE OF ztpp0089 WITH HEADER LINE.

  DATA:ls_menge TYPE menge_d.
*查询工艺单工ZSFWW字段为X的
  DATA:lt_data LIKE TABLE OF ct_item.

  SELECT
    a~zppdhd,
    a~zzpino,
    a~zkunnr_mat,
    a~werks,
    a~vbeln AS vbeln_va,
    a~posnr AS posnr_va,
    a~kunnr,
    a~zzscgdh AS aufnr,
    a~zdhdsl AS menge,
    c~itemno AS zzbom_item,
    c~xgxname AS maktx
    INTO CORRESPONDING FIELDS OF TABLE @lt_data
    FROM ztpp0089 AS a
    INNER JOIN ztpp0100 AS b ON a~zppdhd = b~zppdhd
    INNER JOIN ztpp0101 AS c ON b~gylxno = c~gylxno AND b~version = c~version
    WHERE zzpino IN @s_zzpino
    AND a~zppdhd IN @s_zppdhd
    AND a~werks = @p_werks
    AND a~zdhdzt NOT IN ( 'D','Y' )
    AND a~zsfmbdhd = ''
    AND a~loevm = ''
    AND b~loekz = ''
    AND c~zsfww = 'X'.

  CHECK sy-subrc EQ 0.

  SELECT * INTO TABLE @DATA(lt_ztpp0093) FROM ztpp0093.

  SELECT * INTO TABLE @lt_ztpp0089
    FROM ztpp0089 FOR ALL ENTRIES IN @lt_data
    WHERE zppdhd = @lt_data-zppdhd.


  SELECT zafo_item_po~afono,
         zafo_item_po~afonr,
         zafo_item_po~zppdhd,
         zafo_item_po~zzbom_item,
         zafo_item_po~menge
    FROM zafo_item_po
    INNER JOIN zafo_head ON zafo_head~afono = zafo_item_po~afono
    FOR ALL ENTRIES IN @lt_data
    WHERE zafo_item_po~zppdhd = @lt_data-zppdhd
      AND zafo_item_po~zzbom_item = @lt_data-zzbom_item
      AND zafo_head~bustyp = @g_bustyp
      AND zafo_item_po~loekz = ''
      AND zafo_head~status <> 'D'
    INTO TABLE @DATA(lt_po).

  LOOP AT lt_data INTO DATA(ls_data).

    CLEAR ls_menge.

    " 已采购数
    LOOP AT lt_po INTO DATA(ls_po) WHERE zppdhd  = ls_data-zppdhd
                                     AND zzbom_item = ls_data-zzbom_item.
      ls_menge = ls_menge + ls_po-menge.
    ENDLOOP.

    IF ls_data-menge <= 0.
      CONTINUE.
    ELSE.
      ls_data-icon = icon_led_inactive.
      ls_data-text = TEXT-010."'初始'.

      MOVE-CORRESPONDING ls_data TO ct_item_po.
      ct_item_po-maktx_zh = ls_data-maktx.
      ct_item_po-ktmng = ls_data-menge.
      ct_item_po-menge = ct_item_po-menge - ls_menge.
      ct_item_po-meins = 'PCS'.

      APPEND ct_item_po.
      CLEAR ct_item_po.
    ENDIF.

    ct_item-zzpino     = ls_data-zzpino.
    ct_item-maktx      = ls_data-maktx.
    ct_item-meins      = 'PCS'.
    ct_item-bprme      = ct_item-meins.


    ct_item-menge1     = ls_data-menge.
    ct_item-menge2     = ls_menge." 已采购数

    ct_item-menge      = ls_data-menge - ls_menge.

    PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND CHANGING ct_item.
    PERFORM frm_po_item_calculation IN PROGRAM saplzafo IF FOUND CHANGING ct_item.

    ct_item-werks      = ls_data-werks.
    ct_item-bukrs      = ls_data-werks.
    ct_item-ekorg      = ls_data-werks.

    ct_item-zppdhd     = ls_data-zppdhd.
    ct_item-zkunnr_mat = ls_data-zkunnr_mat.
    ct_item-zzbom_item = ls_data-zzbom_item.
    ct_item-vbeln_va   = ls_data-vbeln_va.
    ct_item-posnr_va   = ls_data-posnr_va.

    CLEAR ct_item-icon.

    SORT lt_ztpp0089 BY zppdhd.
    READ TABLE lt_ztpp0089 WITH KEY zppdhd = ct_item-zppdhd BINARY SEARCH.
    IF sy-subrc = 0.
      ct_item-bukrs = lt_ztpp0089-werks.
      ct_item-aufnr = lt_ztpp0089-zzscgdh.
    ELSE.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-017."'大货单不存在'.
    ENDIF.

    IF ct_item-icon IS INITIAL AND ( lt_ztpp0089-zdhdsl <= 0
      OR lt_ztpp0089-loevm = 'X'
      OR lt_ztpp0089-zdhdzt = 'D' ).
      ct_item-icon = icon_led_red.
      ct_item-text = '大货单已删除'."'大货单已删除.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'A'.
      ct_item-icon = icon_led_red.
      ct_item-text = '大货单未发料'."'大货单未发料
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zzscgdh IS INITIAL.
      ct_item-icon = icon_led_red.
      ct_item-text = '工单不存在'."'生产工单不存在.
    ENDIF.

    IF ct_item-icon IS INITIAL AND lt_ztpp0089-zdhdzt = 'Y'.
      ct_item-icon = icon_led_red.
      ct_item-text = '大货单已关闭'."'大货单已关闭'.
      DATA:lv_objnr LIKE  jest-objnr.
      DATA:t_status TYPE TABLE OF jstat WITH HEADER LINE.
      CONCATENATE 'OR' lt_ztpp0089-zzscgdh INTO lv_objnr.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          client           = sy-mandt
          objnr            = lv_objnr
        TABLES
          status           = t_status
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      LOOP AT t_status WHERE stat = 'I0056' AND inact IS INITIAL.
        IF t_status-stat = 'I0056'.
          ct_item-icon = icon_led_red.
          ct_item-text = '工单已结算'.
        ELSEIF t_status-stat = 'I0045'.
          ct_item-icon = icon_led_red.
          ct_item-text = '工单已关闭'."'生产工单已关闭'.
        ELSEIF t_status-stat = 'I0001'.
          ct_item-icon = icon_led_red.
          ct_item-text = '工单未下达'."'生产工单未下达.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF ct_item-icon IS INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    COLLECT ct_item.
    CLEAR ct_item.

  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0 .
    DELETE ct_item_po WHERE menge <= 0 .
  ENDIF.

ENDFORM.


FORM frm_ref_o TABLES ct_item STRUCTURE zafo_sitem.

ENDFORM.


FORM frm_ref_s TABLES ct_item STRUCTURE zafo_sitem.
  DATA:ls_menge_fh TYPE menge_d,
       ls_menge_th TYPE menge_d.


  SELECT
    t~vbeln AS afono,
    t~posnr AS afonr,
    p~werks,
    k~vbeln AS vbeln_va,
    k~kunnr," 客户
    k~stwae AS waers,
    f~remark1,
    p~posnr AS posnr_va,
    p~matnr,
    p~arktx  AS maktx,
    p~meins,
    p~vrkme AS bprme,
    p~lgort,
    CAST( CAST( p~kpein AS CHAR ) AS NUMC( 5 ) ) AS peinh,
    p~charg,
    c~zcolor,
    c~zsize,
    c~znorms,
    c~zshelves,
    c~zppflag,
    p~kzwi1 AS amount,
    CAST( p~umziz AS CHAR ) AS zmm_tran_rate ,
    a~zname1 AS kunnr_name,
    p~kwmeng AS menge1
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    FROM vbak AS k
    INNER JOIN zafo_head AS f ON k~vbeln = f~afono
    INNER JOIN vbap AS p ON k~vbeln = p~vbeln
    INNER JOIN lips AS t ON p~vbeln = t~vgbel AND p~posnr = t~vgpos
    INNER JOIN zscmt0010 AS a ON k~kunnr = a~partner AND p~werks = a~bukrs
    LEFT JOIN zmch1 AS c ON p~charg = c~charg
    WHERE p~werks IN @s_werks
      AND p~matnr IN @s_matnr
      AND p~arktx IN @s_mtart
      AND k~vbeln IN @s_vbeln
      AND k~auart IN @s_auart
      AND k~vkgrp IN @s_vkgrp
      AND k~kunnr IN @s_kunnr
      AND k~ernam IN @s_pernam.

  CHECK ct_item[] IS NOT INITIAL.


  IF p_typ = 'S3001'.
    " 已发货数量
    SELECT i~* FROM zafo_item AS i
       INNER JOIN zafo_head AS h ON h~afono = i~afono
       FOR ALL ENTRIES IN @ct_item
       WHERE i~vbeln_va = @ct_item-vbeln_va
         AND i~posnr_va = @ct_item-posnr_va
         AND  bustyp = 'S3001'
         AND ( h~status <> 'D' AND h~status <> 'F'  )
         AND i~item_status <> 'F'
       INTO TABLE @DATA(lt_afo_item).
  ENDIF.

  SELECT vbeln_va,posnr_va,afonr,price_long,price,peinh,amount
    INTO TABLE @DATA(lt_price)
    FROM zafo_item AS a
    FOR ALL ENTRIES IN @ct_item
    WHERE vbeln_va = @ct_item-vbeln_va.

  SELECT afono,bustyp
    INTO TABLE @DATA(lt_po_bustyp)
    FROM zafo_head
    FOR ALL ENTRIES IN @ct_item
    WHERE vbeln_va = @ct_item-vbeln_va.

  DATA: lt_matnr TYPE TABLE OF zmms0008 WITH HEADER LINE.

  LOOP AT ct_item.
    lt_matnr-matnr = ct_item-matnr.
    lt_matnr-meins = ct_item-meins.
    COLLECT lt_matnr.
  ENDLOOP.

  CALL FUNCTION 'ZMM_EX_CONV_RATES'
    TABLES
      ct_matnr = lt_matnr.


  LOOP AT ct_item.

    IF ct_item-zcolor IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ZCOL1_OUTPUT'
        EXPORTING
          input  = ct_item-zcolor
        IMPORTING
          output = ct_item-zcolor_text.
    ENDIF.


    IF lt_afo_item[] IS NOT INITIAL.
      LOOP AT lt_afo_item INTO DATA(ls_afo_item) WHERE vbeln_va = ct_item-vbeln_va
                                                    AND posnr_va = ct_item-posnr_va.
        ct_item-menge2 = ct_item-menge2 + ls_afo_item-menge. " 已发货数量
      ENDLOOP.

    ENDIF.

*    ct_item-menge4. "库存数量
    ct_item-menge3 = ct_item-menge1 -  ct_item-menge2 ." 未清数量

    READ TABLE lt_matnr WITH KEY matnr = ct_item-matnr.
    IF sy-subrc EQ 0 .
      ct_item-zcate1 = lt_matnr-zcate1.
      ct_item-zcate2 = lt_matnr-zcate2.
      ct_item-zcate3 = lt_matnr-zcate3.
      ct_item-bprme = lt_matnr-bprme.
      ct_item-zmm_tran_rate = lt_matnr-zmm_tran_rate.
    ENDIF.

    ct_item-menge3_bj = ct_item-menge3 / ct_item-zmm_tran_rate.

    ct_item-menge = ct_item-menge3 .
    ct_item-menge_cg = ct_item-menge3_bj .

    READ TABLE lt_price INTO DATA(ls_price) WITH KEY vbeln_va = ct_item-vbeln_va
                                                     posnr_va = ct_item-posnr_va.
    IF sy-subrc EQ 0.
      ct_item-price_long = ls_price-price_long.
      ct_item-price = ls_price-price.
      ct_item-peinh = ls_price-peinh.
    ELSE.
      ct_item-price_long = ct_item-amount / ct_item-menge1 * ct_item-zmm_tran_rate.
      PERFORM frm_set_price IN PROGRAM saplzafo IF FOUND CHANGING ct_item.
    ENDIF.

    IF ct_item-text = 'X'.
      ct_item-icon = icon_led_red.
      ct_item-text = TEXT-009."'未审批'.
    ELSE.
      IF ct_item-menge3 <= 0.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING 'S' CHANGING ct_item-icon ct_item-text .
      ELSE.
        PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND USING '' CHANGING ct_item-icon ct_item-text .
      ENDIF.
    ENDIF.

    MODIFY ct_item.

  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE icon = icon_complete .
  ENDIF.
ENDFORM.
