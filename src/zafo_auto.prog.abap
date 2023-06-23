*&---------------------------------------------------------------------*
*& Report ZAFO_AUTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo_auto.

TYPES:BEGIN OF ty_afo ,
        bustyp        LIKE zafo_head-bustyp,
        zzpino        LIKE zafo_item-zzpino,
        zppdhd        LIKE zafo_item-zppdhd,
        vbeln_va      LIKE zafo_head-vbeln_va,
        posnr_va      LIKE zafo_head-posnr_va,
        matnr         LIKE zafo_item-matnr,
        maktx         LIKE zafo_item-maktx,
        idnlf         LIKE zafo_item-idnlf,
        zcolor        LIKE zafo_item-zcolor,
        zsize         LIKE zafo_item-zsize,
        znorms        LIKE zafo_item-znorms,
        zppflag       LIKE zafo_item-zppflag,
        zvat_nub      LIKE zafo_item-zvat_nub,
        werks         LIKE zafo_item-werks,
        bukrs         LIKE zafo_item-bukrs,
        aufnr         LIKE zafo_item-aufnr,
        meins         LIKE zafo_item-meins,
        bprme         LIKE zafo_item-bprme,
        zmm_tran_rate LIKE zafo_item-zmm_tran_rate,
        menge         LIKE zafo_item-menge,
      END OF ty_afo.

DATA: gs_bustyp TYPE zafo_bustype   .
DATA:gs_head TYPE zafo_head.
DATA:gt_item TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
DATA:gt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

IF sy-calld = 'X'.
  PERFORM frm_scfl.
ENDIF.

FORM frm_scfl.
  DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  SELECT SINGLE * INTO gs_bustyp FROM zafo_bustype WHERE bustyp = 'R2001'.


  CLEAR gs_head.
  gs_head-werks = '1000'.
  gs_head-kostl = '0010000210'.
  gs_head-kostl_name = '德清安泰-生产部'.
  REFRESH: gt_item,gt_return.
  PERFORM frm_ref_f TABLES gt_item USING gs_head-werks.

  IF gt_item[] IS NOT INITIAL.
    CALL FUNCTION 'ZAFO_CREATE_SAVE'
      EXPORTING
        i_bustyp  = 'R2001'
        i_post    = 'X'
      TABLES
        et_return = gt_return[]
        ct_item   = gt_item[]
      CHANGING
        cs_head   = gs_head
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
  ENDIF.


  CLEAR gs_head.
  gs_head-werks = '1010'.
  gs_head-kostl = '0010100060'.
  gs_head-kostl_name = '山东安泰-生产部'.

  REFRESH: gt_item,gt_return.
  PERFORM frm_ref_f TABLES gt_item USING gs_head-werks.

  IF gt_item[] IS NOT INITIAL.
    CALL FUNCTION 'ZAFO_CREATE_SAVE'
      EXPORTING
        i_bustyp  = 'R2001'
        i_post    = 'X'
      TABLES
        et_return = gt_return[]
        ct_item   = gt_item[]
      CHANGING
        cs_head   = gs_head
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
  ENDIF.

ENDFORM.


FORM frm_ref_f TABLES ct_item STRUCTURE zafo_sitem USING pv_werks . "生产发料
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
    WHERE m~werks = @pv_werks
    AND m~lgort = '1009'
    AND m~kalab > 0
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
         AND i~matnr = @lt_stock-matnr
         AND i~idnlf = @lt_stock-idnlf
         AND i~zcolor = @lt_stock-zcolor
         AND i~zsize = @lt_stock-zsize
         AND i~znorms = @lt_stock-znorms
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
                                 matnr  = ls_stock-matnr
                                 idnlf  = ls_stock-idnlf
                                 zcolor = ls_stock-zcolor
                                 zsize  = ls_stock-zsize
                                 znorms = ls_stock-znorms.
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

    IF ct_item-icon IS INITIAL AND  ct_item-werks <> lt_ztpp0089-werks AND gs_bustyp-execute_type = 'MB'.
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
      ct_item-icon = icon_led_inactive.
      ct_item-text = '初始'."'初始'
    ENDIF.

    MODIFY ct_item.
  ENDLOOP.

  DELETE ct_item WHERE icon <> icon_led_inactive.

  DATA:lv_afonr TYPE zafonr.

  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<fs_item>).
    ADD 1 TO lv_afonr.
    <fs_item>-afonr = lv_afonr.
    <fs_item>-icon = icon_led_yellow.
    <fs_item>-text = '已保存'.
  ENDLOOP.

ENDFORM.
