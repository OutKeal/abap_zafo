
FORM frm_ref CHANGING ct_item TYPE zafo_tt_sitem."仅做索引 不调用
  PERFORM frm_ref_a CHANGING ct_item."采购订单收货参考
  PERFORM frm_ref_b CHANGING ct_item."一般库存参考
  PERFORM frm_ref_c CHANGING ct_item."一般库存+E库存参考
  PERFORM frm_ref_d CHANGING ct_item."获取RESB数据生成采购
  PERFORM frm_ref_e CHANGING ct_item."E库存参考
  PERFORM frm_ref_f CHANGING ct_item."生产报工
  PERFORM frm_ref_g CHANGING ct_item."采购订单原单退货
  PERFORM frm_ref_h CHANGING ct_item."生产发料
  PERFORM frm_ref_i CHANGING ct_item."生产退料
  PERFORM frm_ref_j CHANGING ct_item."生产收货
  PERFORM frm_ref_k CHANGING ct_item."参考工序外协加工
  PERFORM frm_ref_l CHANGING ct_item."生产订单抬头信息
  PERFORM frm_ref_o CHANGING ct_item."O库存参考
  PERFORM frm_ref_z CHANGING ct_item."前序参考
ENDFORM.

FORM frm_ref_a CHANGING ct_item TYPE zafo_tt_sitem. "采购订单收货参考

  SELECT
    k~bukrs,
    k~ebeln,
    k~lifnr,
    k~waers,
    k~frgrl,
    k~ekorg,
    k~ekgrp,
    k~bsart,
    p~ebelp,
    p~matnr,
    p~matkl,
    p~idnlf,
    p~txz01  AS maktx,
    p~meins,
    p~bprme,
    p~werks,
    p~lgort,
    p~elikz,
    p~peinh AS peinh,
    p~brtwr AS amount,
    t~charg,
    t~menge AS menge_ref,
    t~wemng AS menge_done,
    ( t~menge - t~wemng ) AS menge_plan,
    ( t~menge - t~wemng ) AS menge,
    a~name1 AS lifnr_name,
    b~name1 AS werks_name,
    p~bednr AS satnr,
    p~externalreferenceid AS ihrez,
    p~labnr AS aufnr,
    c~lgobe   AS lgort_name
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    FROM ekko AS k
    INNER JOIN ekpo AS p
    ON k~ebeln = p~ebeln
    INNER JOIN eket AS t
    ON p~ebeln = t~ebeln AND p~ebelp = t~ebelp
    LEFT JOIN lfa1 AS a ON k~lifnr = a~lifnr
    LEFT JOIN t001w AS b ON p~werks = b~werks
    LEFT JOIN t001l AS c ON p~werks = c~werks AND p~lgort = c~lgort
    WHERE  p~werks IN @s_werks
    AND p~lgort   IN @s_lgort
    AND p~matnr IN @s_matnr
    AND p~mtart IN @s_mtart
    AND p~matkl IN @s_matkl
    AND p~txz01 IN @s_maktx
    AND k~ekorg IN @s_ekorg
    AND k~ebeln IN @s_ebeln
    AND k~bsart IN @s_bsart
    AND k~ekgrp IN @s_ekgrp
    AND k~lifnr IN @s_lifnr
    AND k~aedat IN @s_erdat
    AND k~ernam IN @s_ernam
    AND k~bsart IN @s_bsart
    AND p~labnr IN @s_aufnr
    AND p~bednr IN @s_satnr
    AND t~menge > 0
    AND p~loekz = ''.

  CHECK ct_item IS NOT INITIAL.

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).

    IF <item>-peinh IS NOT INITIAL.
      <item>-netpr = <item>-netpr / <item>-peinh.
      <item>-netwr = <item>-netwr .
    ENDIF.

    <item>-price = <item>-amount / <item>-menge_ref * <item>-peinh.
    <item>-price_long = <item>-amount / <item>-menge_ref.

    IF <item>-menge_plan <= 0.
      zafo_basic=>set_icon( EXPORTING status = 'S'
         IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'S'.
    ELSE.
      <item>-item_status = 'C'.
      zafo_basic=>set_icon( EXPORTING status = 'C'
         IMPORTING icon = <item>-icon text = <item>-text ).
    ENDIF.
    IF <item>-frgrl = 'X'.
      zafo_basic=>set_icon( EXPORTING status = 'B'
        IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'B'.
    ENDIF.

  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE elikz = 'X'.
  ENDIF.
ENDFORM.

FORM frm_ref_b CHANGING ct_item TYPE zafo_tt_sitem. "一般库存
  SELECT
  d~matnr,
  d~werks,
  d~lgort,
  d~labst AS menge_stock,
  t~maktx,
  a~meins,
  w~name1 AS werks_name,
  l~lgobe AS lgort_name
  FROM mard AS d
  INNER JOIN marc AS c
  ON d~matnr = c~matnr AND d~werks = c~werks
  INNER JOIN mara AS a
  ON c~matnr =  a~matnr
  LEFT JOIN makt AS t ON c~matnr = t~matnr AND t~spras = @sy-langu
  INNER JOIN t001w AS w ON d~werks = w~werks
  INNER JOIN t001l AS l ON d~werks = l~werks AND d~lgort = l~lgort
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  WHERE a~matnr IN @s_matnr
  AND a~mtart IN @s_mtart
  AND d~werks IN @s_werks
  AND d~lgort IN @s_lgort
  AND a~matkl IN @s_matkl
  AND t~maktx IN @s_maktx
  AND labst <> 0
   AND c~xchar = ''.


  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).
    <item>-umwrk = <item>-werks.
    <item>-umwrk_name = <item>-werks_name.
    IF <item>-menge_stock IS NOT INITIAL.
      zafo_basic=>set_icon( EXPORTING status = 'A'
         IMPORTING icon = <item>-icon text = <item>-text ).
    ENDIF.
    <item>-item_status = 'C'.
  ENDLOOP.
ENDFORM.

FORM frm_ref_c CHANGING ct_item TYPE zafo_tt_sitem. "E库存参考+普通库存
  SELECT
  k~vbeln AS kdauf,
  k~posnr AS kdpos,
  k~satnr,
  vbak~ihrez ,
  p~aufnr,
  k~sobkz,
  k~matnr,
  k~werks,
  k~lgort,
  k~labst AS menge_stock,
  t~maktx,
  a~meins,
  w~name1 AS werks_name,
  l~lgobe AS lgort_name,
  CASE WHEN k~sobkz = 'E' THEN 'M'
    ELSE ' ' END AS knttp
  FROM zafo_wh_stock AS k
  LEFT JOIN vbak ON k~vbeln = vbak~vbeln
  LEFT JOIN vbap AS p ON k~vbeln = p~vbeln AND k~posnr = p~posnr
  INNER JOIN marc AS c
  ON k~matnr = c~matnr AND k~werks = c~werks
  INNER JOIN mara AS a
  ON c~matnr =  a~matnr
  LEFT JOIN makt AS t ON c~matnr = t~matnr AND t~spras = @sy-langu
  INNER JOIN t001w AS w ON k~werks = w~werks
  INNER JOIN t001l AS l ON k~werks = l~werks AND k~lgort = l~lgort
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  WHERE a~matnr IN @s_matnr
  AND a~mtart IN @s_mtart
  AND k~satnr IN @s_satnr
  AND k~werks IN @s_werks
  AND k~lgort IN @s_lgort
  AND a~matkl IN @s_matkl
  AND t~maktx IN @s_maktx
  AND p~pmatn IN @s_satnr
  AND k~vbeln IN @s_vbeln
  AND t~maktx IN @s_maktx
  AND k~labst <> 0
  AND c~xchar = ''.


  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>)..
    <item>-umwrk = <item>-werks.
    <item>-umwrk_name = <item>-werks_name.
    IF <item>-menge_stock IS NOT INITIAL.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = <item>-icon text = <item>-text ).
    ENDIF.
    <item>-item_status = 'C'.
  ENDLOOP.
ENDFORM.


FORM frm_ref_d CHANGING ct_item TYPE zafo_tt_sitem. "获取RESB数据采购


ENDFORM.

FORM frm_ref_e CHANGING ct_item TYPE zafo_tt_sitem. "E库存参考
  SELECT
  k~vbeln AS kdauf,
  k~posnr AS kdpos,
  p~pmatn AS satnr,
  vbak~ihrez,
  k~sobkz,
  k~matnr,
  k~werks,
  k~lgort,
  k~kalab AS menge_stock,
  t~maktx,
  a~meins,
  w~name1 AS werks_name,
  l~lgobe AS lgort_name
  FROM mska AS k
  LEFT JOIN vbak ON k~vbeln = vbak~ihrez
  LEFT JOIN vbap AS p ON k~vbeln = p~vbeln AND k~posnr = p~posnr
*  INNER JOIN marc AS c
*  ON k~matnr = c~matnr AND k~werks = c~werks
  INNER JOIN mara AS a
  ON k~matnr =  a~matnr
  LEFT JOIN makt AS t ON k~matnr = t~matnr AND t~spras = @sy-langu
  INNER JOIN t001w AS w ON k~werks = w~werks
  INNER JOIN t001l AS l   ON k~werks = l~werks AND k~lgort = l~lgort
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  WHERE a~matnr IN @s_matnr
  AND p~pmatn IN @s_satnr
  AND a~mtart IN @s_mtart
  AND k~werks IN @s_werks
  AND k~lgort IN @s_lgort
  AND a~matkl IN @s_matkl
  AND t~maktx IN @s_maktx
  AND k~vbeln IN @s_vbeln
  AND k~kalab <> 0.


  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).


    <item>-umwrk = <item>-werks.
    <item>-umwrk_name = <item>-werks_name.
    IF <item>-menge_stock IS NOT INITIAL.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = <item>-icon text = <item>-text ).
    ENDIF.
    <item>-item_status = 'C'.
  ENDLOOP.

ENDFORM.

FORM frm_ref_g CHANGING ct_item TYPE zafo_tt_sitem."采购订单原单退货
  SELECT
  k~bukrs,
  k~ebeln,
  k~lifnr,
  k~waers,
  k~frgrl,
  p~ebelp,
  p~matnr,
  p~matkl,
  p~idnlf,
  p~txz01  AS maktx,
  p~meins,
  p~bprme,
  p~werks,
  p~lgort,
  p~peinh AS peinh,
  p~brtwr AS amount,
  t~charg,
  t~menge AS menge_ref,
  t~wemng AS menge_plan,
  t~wemng AS menge,
  a~name1 AS lifnr_name,
  b~name1 AS werks_name,
  p~bednr AS satnr,
  p~externalreferenceid AS ihrez,
  p~labnr AS aufnr,
  c~lgobe   AS lgort_name
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  FROM ekko AS k
  INNER JOIN ekpo AS p
  ON k~ebeln = p~ebeln
  INNER JOIN eket AS t
  ON p~ebeln = t~ebeln AND p~ebelp = t~ebelp
  LEFT JOIN lfa1 AS a ON k~lifnr = a~lifnr
  LEFT JOIN t001w AS b ON p~werks = b~werks
  LEFT JOIN t001l AS c ON p~werks = c~werks AND p~lgort = c~lgort
  WHERE  p~werks IN @s_werks
  AND p~lgort   IN @s_lgort
  AND p~matnr IN @s_matnr
  AND p~mtart IN @s_mtart
  AND p~matkl IN @s_matkl
  AND k~ebeln IN @s_ebeln
  AND k~bsart IN @s_bsart
  AND k~ekgrp IN @s_ekgrp
  AND k~lifnr IN @s_lifnr
  AND k~aedat IN @s_erdat
  AND k~bsart IN @s_bsart
  AND p~labnr IN @s_aufnr
  AND p~bednr IN @s_satnr
  AND p~retpo = ''
  AND t~wemng > 0
  AND p~loekz = '  '.

  CHECK ct_item IS NOT INITIAL.

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>)..

    IF <item>-peinh IS NOT INITIAL.
      <item>-netpr = <item>-netpr / <item>-peinh.
      <item>-netwr = <item>-netwr .
    ENDIF.

    <item>-price = <item>-amount / <item>-menge_ref * <item>-peinh.
    <item>-price_long = <item>-amount / <item>-menge_ref.

    IF <item>-menge_plan <= 0.
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'S'.
    ELSE.
      <item>-item_status = 'C'.
      zafo_basic=>set_icon( EXPORTING status = 'C'
      IMPORTING icon = <item>-icon text = <item>-text ).
    ENDIF.
    IF <item>-frgrl = 'X'.
      zafo_basic=>set_icon( EXPORTING status = 'B'
      IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'B'.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM frm_ref_f CHANGING ct_item TYPE zafo_tt_sitem."生产报工
  SELECT
  a~aufnr,
  a~ihrez,
  a~vornr,
  a~werks,
  a~arbpl,
  a~meins,
  a~matnr,
  a~rueck,
  a~ltxa1 AS maktx,
  a~mgvrg AS menge_ref,
  a~lmnga AS menge_done,
  ( a~mgvrg - a~lmnga ) AS menge_plan,
  w~name1 AS werks_name
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  FROM zafo_mo_conf AS a
  INNER JOIN t001w AS w ON a~werks = w~werks
  WHERE a~werks IN @s_werks
  AND aufnr IN @s_aufnr
  AND matnr IN @s_matnr
  AND satnr IN @s_satnr
  AND arbpl <> ''
  ORDER BY aufnr ,vornr.

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>)..
    IF <item>-menge_plan > 0.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'C'.
    ELSE.
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'S'.
    ENDIF.
    <item>-menge = COND #( WHEN <item>-menge_plan >= 0 THEN <item>-menge_plan ELSE 0 ).

  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0.
  ENDIF.
ENDFORM.


FORM frm_ref_h CHANGING ct_item TYPE zafo_tt_sitem. "生产发料
  SELECT
  a~rsnum,
  a~rspos,
  a~matnr,
  a~werks,
  a~sobkz,
  a~bdmng AS menge_ref,
  a~enmng AS menge_done,
  a~meins,
  h~satnr,
  h~ihrez,
  a~kdauf,
  a~kdpos,
  a~aufnr,
  a~knttp,
  a~matkl,
  a~bdter AS eeind,
  t1~maktx,
  w~name1 AS werks_name,
  k~labst AS menge_stock,
  k~lgort,
  l~lgobe AS lgort_name,
  h~status AS item_status,
  h~status_text AS text
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  FROM resb AS a
  INNER JOIN zafo_mo_head AS h ON a~aufnr = h~aufnr
  INNER JOIN t001w AS w ON a~werks = w~werks
  LEFT JOIN makt AS t1 ON a~matnr = t1~matnr AND t1~spras = @sy-langu
  LEFT JOIN zafo_wh_stock AS k ON a~matnr = k~matnr
                                                 AND a~werks = k~werks
                                                 AND a~kdauf = k~vbeln
                                                 AND a~kdpos = k~posnr
   LEFT JOIN t001l AS l ON k~werks = l~werks AND k~lgort = l~lgort
                                                  AND a~sobkz = k~sobkz
  WHERE a~werks IN @s_werks
  AND a~aufnr IN @s_aufnr
  AND a~matnr IN @s_matnr
  AND h~satnr IN @s_satnr
  AND k~lgort IN @s_lgort
  AND a~bwart = '261'
  AND a~bdart = 'AR'
  AND a~xloek = ''.

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).


    <item>-move_satnr = <item>-satnr.
    <item>-menge_plan = <item>-menge_ref - <item>-menge_done.
    <item>-menge = COND #( WHEN <item>-menge_plan > <item>-menge_stock THEN <item>-menge_stock
                                              ELSE <item>-menge_plan ).
    zafo_basic=>set_icon( EXPORTING status = <item>-item_status
    IMPORTING icon = <item>-icon  ).
    IF <item>-item_status = 'S'.
      <item>-item_status = 'A'.
    ENDIF.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0.
  ENDIF.

ENDFORM.

FORM frm_ref_i CHANGING ct_item TYPE zafo_tt_sitem. "生产退料,RESB取值无计划外发料
  SELECT
  a~aufnr,
  a~rsnum,
  a~rspos,
  s~matnr AS matnr_mo,
  s~satnr,
  vbak~ihrez,
  a~matnr,
  a~werks,
  a~sobkz,
  a~meins,
  a~kdauf,
  a~kdpos,
  m~matkl,
  a~sobkz,
  m~maktx,
  w~name1 AS werks_name,
  SUM( CASE a~shkzg WHEN 'H' THEN a~menge WHEN 'S' THEN - a~menge END )  AS menge_plan
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  FROM aufm AS a
  INNER JOIN aufk AS u ON a~aufnr = u~aufnr
  INNER JOIN afko AS k ON a~aufnr = k~aufnr
  INNER JOIN mara AS s ON k~plnbez = s~matnr
  INNER JOIN t001w AS w ON a~werks = w~werks
  INNER JOIN zafo_v_mara AS m ON a~matnr = m~matnr
  LEFT JOIN vbak  ON vbak~vbeln = a~kdauf
*                                                  AND a~sobkz = k~sobkz
  WHERE a~werks IN @s_werks
  AND a~aufnr IN @s_aufnr
  AND s~satnr IN @s_satnr
  AND a~matnr IN @s_matnr
  AND m~matkl IN @s_matkl
  AND m~mtart IN @s_mtart
  AND m~maktx IN @s_maktx
  AND bwart IN ( '261' , '262' )
  GROUP BY a~aufnr,
  a~rsnum,
  a~rspos,
  s~satnr,
  vbak~ihrez,
  a~matnr,
  a~werks,
  a~sobkz,
  a~meins,
  a~kdauf,
  a~kdpos,
  m~matkl,
  a~sobkz,
  m~maktx,
  w~name1,
  s~matnr.

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).
    <item>-retpo = 'X'.
    <item>-menge = COND #( WHEN <item>-menge_plan > 0 THEN <item>-menge_plan
    ELSE 0 ).
    IF <item>-menge > 0.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'C'.
    ELSE .
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'S'.
    ENDIF.
  ENDLOOP.
  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0.
  ENDIF.
ENDFORM.

FORM frm_ref_j CHANGING ct_item TYPE zafo_tt_sitem.
  SELECT * FROM zafo_mo_gr
    INTO CORRESPONDING FIELDS OF TABLE ct_item
    WHERE aufnr IN s_aufnr
    AND kdauf IN s_vbeln
    AND satnr IN s_satnr
    AND werks IN s_werks.

  DELETE ADJACENT DUPLICATES FROM ct_item COMPARING aufnr posnr_mo.

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).
    <item>-menge_plan = <item>-menge_ref - <item>-menge_done.
*    <item>-menge = COND #( WHEN <item>-menge_plan > 0 THEN <item>-menge_plan
*                                              ELSE 0 ).
    zafo_basic=>set_icon( EXPORTING status = <item>-item_status
    IMPORTING icon = <item>-icon  ).
    IF <item>-item_status = 'S'.
      <item>-item_status = 'A'.
    ENDIF.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge_plan <= 0 .
  ENDIF.
ENDFORM.

FORM frm_ref_k CHANGING ct_item TYPE zafo_tt_sitem."参考工序外协加工
  SELECT
  a~aufnr,
  a~vornr,
  a~werks,
  a~meins,
  a~matnr AS satnr,
  a~ihrez ,
  a~rueck,
  a~ltxa1 AS maktx,
  a~mgvrg AS menge_ref,
  d~menge AS menge_done,
*  ( a~mgvrg - d~menge ) AS menge_plan,
  a~price,
  a~peinh,
  a~waers,
  a~ekorg,
  a~ekgrp,
  a~lifnr,
  a~matkl,
  a~aufpl,
  a~aplzl,
  'F' AS knttp,
   v~name1 AS lifnr_name,
  w~name1 AS werks_name
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  FROM zafo_mo_conf AS a
  LEFT JOIN zafo_done_sum AS d ON d~bustyp = @p_typ AND a~aufnr =  d~aufnr AND a~vornr = d~vornr
  INNER JOIN t001w AS w ON a~werks = w~werks
  LEFT JOIN lfa1 AS v ON a~lifnr = v~lifnr
  WHERE a~werks IN @s_werks
  AND a~aufnr IN @s_aufnr
  AND a~matnr IN @s_matnr
  AND a~satnr IN @s_satnr
  AND steus = 'PP02'
  ORDER BY a~aufnr ,a~vornr.


  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).
    <item>-menge_plan = <item>-menge_ref - <item>-menge_done.
    <item>-menge = COND #( WHEN <item>-menge_plan > 0 THEN <item>-menge_plan ELSE 0 ).
    <item>-price_long = <item>-price / <item>-peinh.
    <item>-amount = <item>-menge * <item>-price / <item>-peinh.
    IF <item>-menge_plan > 0.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'C'.
    ELSE.
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = <item>-icon text = <item>-text ).
      <item>-item_status = 'S'.
    ENDIF.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0.
  ENDIF.


ENDFORM.

FORM frm_ref_l CHANGING ct_item TYPE zafo_tt_sitem."生产订单抬头信息

  SELECT
    h~aufnr,
    h~werks,
    h~satnr,
    h~ihrez,
    h~maktx AS satnr_name,
    menge AS menge_ref,
*    meins,
    kdauf AS vbeln_va,
    kdpos AS posnr_va,
    status AS item_status,
    status_text AS text
    FROM zafo_mo_head AS h
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    WHERE aufnr IN @s_aufnr
    AND satnr IN @s_satnr
    AND werks IN @s_werks
    AND kdauf IN @s_vbeln
    AND mtart IN @s_mtart
    AND matkl IN @s_matkl.

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).
    zafo_basic=>set_icon( EXPORTING status = <item>-item_status
    IMPORTING icon = <item>-icon   ).
    IF <item>-item_status = 'S'.
      <item>-item_status = 'A'.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM frm_ref_o CHANGING ct_item TYPE zafo_tt_sitem. "O库存参考

  SELECT
  c~matnr,
  c~werks,
  c~werks AS umwrk,
  c~lifnr,
  c~charg,
  lblab AS menge_stock,
  t~maktx,
  a~meins,
  l~name1 AS lifnr_name,
  w~name1 AS werks_name
  FROM mslb AS c
     INNER JOIN mara AS a
       ON c~matnr =  a~matnr
    LEFT JOIN t001w AS w ON c~werks = w~werks
     LEFT JOIN makt AS t ON c~matnr = t~matnr AND t~spras = @sy-langu
     LEFT JOIN lfa1 AS l ON c~lifnr = l~lifnr
     INTO CORRESPONDING FIELDS OF TABLE @ct_item
       WHERE a~matnr IN @s_matnr
         AND a~mtart IN @s_mtart
         AND c~werks IN @s_werks
         AND c~lifnr IN @s_lifnr
*            AND c~charg IN @s_charg
         AND lblab <> 0.


  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).
    zafo_basic=>set_icon( EXPORTING status = 'A'
    IMPORTING icon = <item>-icon text = <item>-text ).
    <item>-item_status = 'C'.

  ENDLOOP.

ENDFORM.


FORM frm_ref_y CHANGING ct_item TYPE zafo_tt_sitem. "过账单据引用

  PERFORM frm_ref_z CHANGING ct_item.

ENDFORM.

FORM frm_ref_z CHANGING ct_item TYPE zafo_tt_sitem. "申请单引用

  DATA(r_bustyp) = zafo_basic=>get_bustyp_ref( p_typ ).

  SELECT
  i~*
  APPENDING CORRESPONDING FIELDS OF TABLE @ct_item
  FROM zafo_head AS h INNER JOIN zafo_item AS i
  ON h~afono =  i~afono
    WHERE bustyp IN @r_bustyp
  AND h~afono IN @s_afono
  AND h~ernam IN @s_ernam
*       AND h~werks = @p_werks
*     AND ( h~werks IN @s_werks OR h~umwrk IN @s_werks OR i~werks IN @s_werks )
  AND h~erdat IN @s_erdat
  AND h~budat IN @s_budat
  AND h~status IN @s_status
  AND h~ekgrp IN @s_ekgrp
  AND i~matnr IN @s_matnr
  AND i~maktx IN @s_maktx
  AND h~exord IN @s_exord
  AND i~del_flag = ''
.


  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<item>).
    IF <item>-item_status IS NOT INITIAL.
      <item>-icon = <item>-item_status.
    ENDIF.
    zafo_basic=>set_icon( EXPORTING status = <item>-item_status
                                        IMPORTING icon = <item>-icon text = <item>-text ).
    <item>-menge_ref = <item>-menge.
  ENDLOOP.


  IF p_fin IS INITIAL.
    DELETE ct_item WHERE icon = icon_allow.
  ENDIF.

ENDFORM.
