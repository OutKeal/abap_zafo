
FORM frm_ref TABLES ct_item STRUCTURE zafo_sitem.
  PERFORM frm_ref_a TABLES ct_item."采购订单收货参考
  PERFORM frm_ref_b TABLES ct_item."一般库存参考
  PERFORM frm_ref_c TABLES ct_item."一般库存+E库存参考
  PERFORM frm_ref_d TABLES ct_item."获取RESB数据生成采购
  PERFORM frm_ref_e TABLES ct_item."E库存参考
  PERFORM frm_ref_f TABLES ct_item."生产报工
  PERFORM frm_ref_g TABLES ct_item."采购订单原单退货
  PERFORM frm_ref_h TABLES ct_item."生产发料
  PERFORM frm_ref_i TABLES ct_item."生产退料
  PERFORM frm_ref_j TABLES ct_item."生产收货
  PERFORM frm_ref_k TABLES ct_item."参考工序外协加工
  PERFORM frm_ref_z TABLES ct_item."前序参考
ENDFORM.

FORM frm_ref_a TABLES ct_item STRUCTURE zafo_sitem. "采购订单收货参考

  SELECT
    k~bukrs,
    k~ebeln,
    k~lifnr,
    k~waers,
    p~ebelp,
    p~matnr,
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
    t~wemng AS menge_done,
    ( t~menge - t~wemng ) AS menge_plan,
    ( t~menge - t~wemng ) AS menge,
    a~name1 AS lifnr_name,
    b~name1 AS werks_name,
    p~bednr AS satnr,
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
    AND t~menge > 0
    AND p~loekz = '  '.

  CHECK ct_item[] IS NOT INITIAL.


  LOOP AT ct_item.

    IF ct_item-peinh IS NOT INITIAL.
      ct_item-netpr = ct_item-netpr / ct_item-peinh.
      ct_item-netwr = ct_item-netwr .
    ENDIF.

    ct_item-price = ct_item-amount / ct_item-menge_ref * ct_item-peinh.
    ct_item-price_long = ct_item-amount / ct_item-menge_ref.

    IF ct_item-menge_plan <= 0.
      zafo_basic=>set_icon( EXPORTING status = 'S'
         IMPORTING icon = ct_item-icon text = ct_item-text ).
    ELSE.
      ct_item-item_status = 'C'.
      zafo_basic=>set_icon( EXPORTING status = 'C'
         IMPORTING icon = ct_item-icon text = ct_item-text ).
    ENDIF.

    MODIFY ct_item.

  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge_plan <= 0 .
  ENDIF.
ENDFORM.

FORM frm_ref_b TABLES ct_item STRUCTURE zafo_sitem. "一般库存
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
  INNER JOIN t001l AS l   ON d~werks = l~werks AND d~lgort = l~lgort
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  WHERE a~matnr IN @s_matnr
  AND a~mtart IN @s_mtart
  AND d~werks IN @s_werks
  AND d~lgort IN @s_lgort
  AND a~matkl IN @s_matkl
  AND labst <> 0
   AND c~xchar = ''.


  LOOP AT ct_item.
    ct_item-umwrk = ct_item-werks.
    ct_item-umwrk_name = ct_item-werks_name.
    IF ct_item-menge_stock IS NOT INITIAL.
      zafo_basic=>set_icon( EXPORTING status = 'A'
         IMPORTING icon = ct_item-icon text = ct_item-text ).
    ENDIF.
    ct_item-item_status = 'C'.
    MODIFY ct_item.
  ENDLOOP.
ENDFORM.

FORM frm_ref_c TABLES ct_item STRUCTURE zafo_sitem. "E库存参考+普通库存
  SELECT
  k~vbeln AS kdauf,
  k~posnr AS kdpos,
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
  INNER JOIN marc AS c
  ON k~matnr = c~matnr AND k~werks = c~werks
  INNER JOIN mara AS a
  ON c~matnr =  a~matnr
  LEFT JOIN makt AS t ON c~matnr = t~matnr AND t~spras = @sy-langu
  INNER JOIN t001w AS w ON k~werks = w~werks
  INNER JOIN t001l AS l   ON k~werks = l~werks AND k~lgort = l~lgort
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  WHERE a~matnr IN @s_matnr
  AND a~mtart IN @s_mtart
  AND k~werks IN @s_werks
  AND k~lgort IN @s_lgort
  AND a~matkl IN @s_matkl
  AND k~kalab <> 0
  AND c~xchar = ''.

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
  INNER JOIN t001l AS l   ON d~werks = l~werks AND d~lgort = l~lgort
  APPENDING CORRESPONDING FIELDS OF TABLE @ct_item
  WHERE a~matnr IN @s_matnr
  AND a~mtart IN @s_mtart
  AND d~werks IN @s_werks
  AND d~lgort IN @s_lgort
  AND a~matkl IN @s_matkl
  AND labst <> 0
  AND c~xchar = ''.


  LOOP AT ct_item.
    ct_item-umwrk = ct_item-werks.
    ct_item-umwrk_name = ct_item-werks_name.
    IF ct_item-menge_stock IS NOT INITIAL.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
    ENDIF.
    ct_item-item_status = 'C'.
    MODIFY ct_item.
  ENDLOOP.
ENDFORM.


FORM frm_ref_d TABLES ct_item STRUCTURE zafo_sitem. "获取RESB数据采购
  SELECT
  a~rsnum,
  a~rspos,
  a~matnr,
  a~werks,
  a~aufnr,
  a~lgort,
  a~sobkz,
  a~bdmng AS menge_ref,
  a~meins,
  m~satnr,
  a~kdauf,
  a~kdpos,
  a~knttp,
  t1~matkl,
  t1~mtart,
  a~bdter AS eeind,
  w~name1 AS werks_name,
  b~menge_done,
  t1~maktx
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  FROM resb AS a
  LEFT JOIN zafo_v_mara AS m ON  a~baugr = m~matnr AND m~spras = @sy-langu
  LEFT JOIN zafo_po_sum AS b ON a~rsnum = b~rsnum AND a~rspos = b~rspos AND b~bustyp = @p_typ
  LEFT JOIN t001w AS w ON a~werks = w~werks
  LEFT JOIN zafo_v_mara AS t1 ON a~matnr = t1~matnr AND t1~spras = @sy-langu
  WHERE a~werks IN @s_werks
  AND a~aufnr IN @s_aufnr
  AND a~matnr IN @s_matnr
  AND t1~mtart IN @s_mtart
  AND t1~matkl IN @s_matkl
  AND m~satnr IN @s_satnr
  AND a~kdauf IN @s_vbeln
  AND t1~maktx IN @s_maktx
  AND a~xwaok = 'X'
  AND a~bdart = 'AR'.

  LOOP AT ct_item.
    ct_item-menge_plan = ct_item-menge_ref - ct_item-menge_done.
    ct_item-menge = COND #( WHEN ct_item-menge_plan > 0 THEN ct_item-menge_plan ELSE 0 ).
    IF ct_item-menge > 0.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'C'.
    ELSE.
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'S'.
    ENDIF.
    ct_item-menge = COND #( WHEN ct_item-menge_plan >= 0 THEN ct_item-menge_plan ELSE 0 ).
    MODIFY ct_item.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0.
  ENDIF.

ENDFORM.

FORM frm_ref_e TABLES ct_item STRUCTURE zafo_sitem. "E库存参考
  SELECT
  k~vbeln AS kdauf,
  k~posnr AS kdpos,
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
  INNER JOIN marc AS c
  ON k~matnr = c~matnr AND k~werks = c~werks
  INNER JOIN mara AS a
  ON c~matnr =  a~matnr
  LEFT JOIN makt AS t ON c~matnr = t~matnr AND t~spras = @sy-langu
  INNER JOIN t001w AS w ON k~werks = w~werks
  INNER JOIN t001l AS l   ON k~werks = l~werks AND k~lgort = l~lgort
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  WHERE a~matnr IN @s_matnr
  AND a~mtart IN @s_mtart
  AND k~werks IN @s_werks
  AND k~lgort IN @s_lgort
  AND a~matkl IN @s_matkl
  AND k~kalab <> 0
  AND c~xchar = ''.


  LOOP AT ct_item.
    ct_item-umwrk = ct_item-werks.
    ct_item-umwrk_name = ct_item-werks_name.
    IF ct_item-menge_stock IS NOT INITIAL.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
    ENDIF.
    ct_item-item_status = 'C'.
    MODIFY ct_item.
  ENDLOOP.

ENDFORM.

FORM frm_ref_g TABLES ct_item STRUCTURE zafo_sitem."采购订单原单退货

ENDFORM.
FORM frm_ref_f TABLES ct_item STRUCTURE zafo_sitem."生产报工
  SELECT
  a~aufnr,
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
  AND arbpl <> ''
  ORDER BY aufnr ,vornr.

  LOOP AT ct_item.
    IF ct_item-menge_plan > 0.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'C'.
    ELSE.
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'S'.
    ENDIF.
    ct_item-menge = COND #( WHEN ct_item-menge_plan >= 0 THEN ct_item-menge_plan ELSE 0 ).
    MODIFY ct_item.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0.
  ENDIF.
ENDFORM.


FORM frm_ref_h TABLES ct_item STRUCTURE zafo_sitem. "生产发料
  SELECT
  a~rsnum,
  a~rspos,
  a~matnr,
  a~werks,
  a~sobkz,
  a~bdmng AS menge_ref,
  a~enmng AS menge_done,
  a~meins,
  m~satnr,
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
  l~lgobe AS lgort_name
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  FROM resb AS a
  INNER JOIN t001w AS w ON a~werks = w~werks
  LEFT JOIN makt AS t1 ON a~matnr = t1~matnr AND t1~spras = @sy-langu
  LEFT JOIN mara AS m ON a~baugr = m~matnr
  LEFT JOIN zafo_wh_stock AS k ON a~matnr = k~matnr
                                                 AND a~werks = k~werks
                                                 AND a~kdauf = k~vbeln
                                                 AND a~kdpos = k~posnr
   LEFT JOIN t001l AS l ON k~werks = l~werks AND k~lgort = l~lgort
*                                                  AND a~sobkz = k~sobkz
  WHERE a~werks IN @s_werks
  AND a~aufnr IN @s_aufnr
  AND a~matnr IN @s_matnr
  AND a~baugr IN @s_satnr
  AND a~xwaok = 'X'
  AND a~bdart = 'AR'.


  LOOP AT ct_item.
    ct_item-menge_plan = ct_item-menge_ref - ct_item-menge_done.
    ct_item-menge = COND #( WHEN ct_item-menge_plan > ct_item-menge_stock THEN ct_item-menge_stock
                                              ELSE ct_item-menge_plan ).
    IF ct_item-menge > 0.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'C'.
    ELSEIF ct_item-menge_plan = 0.
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'S'.
    ELSE.
      zafo_basic=>set_icon( EXPORTING status = ''
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = ''.
    ENDIF.
    MODIFY ct_item.
  ENDLOOP.
  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0.
  ENDIF.

ENDFORM.

FORM frm_ref_i TABLES ct_item STRUCTURE zafo_sitem. "生产退料

ENDFORM.

FORM frm_ref_j TABLES ct_item STRUCTURE zafo_sitem.
  SELECT * FROM zafo_mo_gr
    INTO CORRESPONDING FIELDS OF TABLE ct_item
    WHERE aufnr IN s_aufnr
    AND kdauf IN s_vbeln
    AND satnr IN s_satnr
    AND werks IN s_werks.

  LOOP AT ct_item.
    ct_item-menge_plan = ct_item-menge_ref - ct_item-menge_done.
    ct_item-menge = COND #( WHEN ct_item-menge_plan > 0 THEN ct_item-menge_plan
                                              ELSE 0 ).

    IF ct_item-menge_plan > 0.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'C'.
    ELSE.
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'S'.
    ENDIF.

    MODIFY ct_item.
  ENDLOOP.

  IF p_fin = ''.
    DELETE ct_item WHERE menge_plan <= 0 .
  ENDIF.
ENDFORM.

FORM frm_ref_k TABLES ct_item STRUCTURE zafo_sitem."参考工序外协加工
  SELECT
  a~aufnr,
  a~vornr,
  a~werks,
  a~meins,
  a~matnr AS satnr,
  a~rueck,
  a~ltxa1 AS maktx,
  a~mgvrg AS menge_plan,
  a~price,
  a~peinh,
  a~waers,
  a~ekorg,
  a~ekgrp,
  a~lifnr,
  a~matkl,
  'F' AS knttp,
  w~name1 AS werks_name
  INTO CORRESPONDING FIELDS OF TABLE @ct_item
  FROM zafo_mo_conf AS a
  INNER JOIN t001w AS w ON a~werks = w~werks
  WHERE a~werks IN @s_werks
  AND aufnr IN @s_aufnr
  AND matnr IN @s_matnr
    AND steus = 'PP02'
  ORDER BY aufnr ,vornr.

  LOOP AT ct_item.
    ct_item-menge = COND #( WHEN ct_item-menge_plan > 0 THEN ct_item-menge_plan ELSE 0 ).
    ct_item-price_long = ct_item-price / ct_item-peinh.
    ct_item-amount = ct_item-menge * ct_item-price / ct_item-peinh.
    IF ct_item-menge_plan > 0.
      zafo_basic=>set_icon( EXPORTING status = 'A'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'C'.
    ELSE.
      zafo_basic=>set_icon( EXPORTING status = 'S'
      IMPORTING icon = ct_item-icon text = ct_item-text ).
      ct_item-item_status = 'S'.
    ENDIF.
    MODIFY ct_item.
  ENDLOOP.


  IF p_fin = ''.
    DELETE ct_item WHERE menge <= 0.
  ENDIF.


ENDFORM.

FORM frm_ref_o TABLES ct_item STRUCTURE zafo_sitem. "O库存参考

  SELECT
  c~matnr,
  c~werks,
  c~werks AS umwrk,
  c~lifnr,
  c~charg,
  lblab AS menge_stock,
  t~maktx,
  a~meins,
  l~name1 AS lifnr_name
  FROM mslb AS c
     INNER JOIN mara AS a
       ON c~matnr =  a~matnr
     LEFT JOIN makt AS t ON c~matnr = t~matnr AND t~spras = @sy-langu
     LEFT JOIN lfa1 AS l ON c~lifnr = l~lifnr
     INTO CORRESPONDING FIELDS OF TABLE @ct_item
       WHERE a~matnr IN @s_matnr
         AND a~mtart IN @s_mtart
         AND c~werks IN @s_werks
         AND c~lifnr IN @s_lifnr
*            AND c~charg IN @s_charg
         AND lblab <> 0.


  LOOP AT ct_item.

    IF ct_item-menge_stock IS NOT INITIAL.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
          USING '' CHANGING ct_item-icon ct_item-text .
    ELSE.
      PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
       USING 'E' CHANGING ct_item-icon ct_item-text .
    ENDIF.

    MODIFY ct_item.
  ENDLOOP.

ENDFORM.


FORM frm_ref_y TABLES ct_item STRUCTURE zafo_sitem. "过账单据引用



  CHECK gs_bustyp-bustyp_ref IS NOT INITIAL.

  DATA:ls_item TYPE zafo_sitem.

  SELECT
    h~status AS icon,
    i~*
    FROM zafo_head AS h INNER JOIN zafo_item AS i
    ON h~afono =  i~afono
    INTO CORRESPONDING FIELDS OF TABLE @ct_item
    WHERE bustyp = @gs_bustyp-bustyp_ref
    AND h~afono IN @s_afono
    AND h~ernam IN @s_ernam
    AND i~umcha IN @s_charg"on 20220322.
    AND h~erdat IN @s_erdat
    AND h~budat IN @s_budat
    AND h~status IN @s_status
    AND i~del_flag = ''
    .

  LOOP AT ct_item.

    ls_item = ct_item.
    IF ct_item-umwrk IS NOT INITIAL.
      ct_item-bukrs = ct_item-umwrk.
    ENDIF.

    CLEAR ct_item-umcha .
    ct_item-umcha = ls_item-charg.

    ct_item-menge_ref = ct_item-menge.

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

FORM frm_ref_z TABLES ct_item STRUCTURE zafo_sitem. "申请单引用

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


  LOOP AT ct_item.
    IF ct_item-item_status IS NOT INITIAL.
      ct_item-icon = ct_item-item_status.
    ENDIF.
    zafo_basic=>set_icon( EXPORTING status = ct_item-item_status
                                        IMPORTING icon = ct_item-icon text = ct_item-text ).
    ct_item-menge_ref = ct_item-menge.
    MODIFY ct_item.
  ENDLOOP.


  IF p_fin IS INITIAL.
    DELETE ct_item WHERE icon = icon_allow.
  ENDIF.

ENDFORM.
