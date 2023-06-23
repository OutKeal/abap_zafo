*&---------------------------------------------------------------------*
*& 业务相关FRM               ZAFO_I02_F03
*&---------------------------------------------------------------------*

FORM frm_set_zzpino_hide TABLES pr_zzpino.
  REFRESH pr_zzpino.

  SELECT DISTINCT 'I' AS sign,'EQ' AS option, zzpino AS low
    FROM ztpp0089
    WHERE zhide <> ''
    INTO TABLE @pr_zzpino.
ENDFORM.


FORM f_set_po_so_department USING pv_ywy CHANGING cv_department.

  CHECK pv_ywy IS NOT INITIAL.

  SELECT SINGLE department FROM zapp_addr
  INTO cv_department
  WHERE person = pv_ywy.


ENDFORM .


FORM f_set_po_ywy USING pv_zzpino.

  CHECK pv_zzpino IS NOT INITIAL.

  SELECT SINGLE ernam FROM zsdsch
  INTO gs_head-afnam
  WHERE zzpino = pv_zzpino.
  IF sy-subrc = 0.
    PERFORM f_set_po_so_department USING gs_head-afnam CHANGING gs_head-so_department.
  ENDIF.

ENDFORM .


FORM f_set_kunnr CHANGING cs_item TYPE zafo_sitem.

  SELECT SINGLE * INTO @DATA(ls_t0010) FROM zscmt0010
    WHERE partner = @cs_item-kunnr.
  IF sy-subrc EQ 0.
    cs_item-kunnr_name = ls_t0010-zname1.
  ENDIF.

ENDFORM .


FORM f_zppdhd_f4 USING pv_text CHANGING pv_zppdhd TYPE zppdhd.
  DATA: ct_text TYPE lvc_value.
  DATA: lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  DATA: BEGIN OF lt_hitlist OCCURS 0,
          werks      LIKE ztpp0089-werks,
          zwerks     LIKE ztpp0089-zwerks,
          text       LIKE zafo_sitem-text,
          zppdhd     LIKE ztpp0089-zppdhd,
          zzpino     LIKE ztpp0089-zzpino,
          zkunnr_mat LIKE ztpp0089-zkunnr_mat,
          zzkhxtno   LIKE ztpp0089-zzkhxtno,
          matnr      LIKE ztpp0089-matnr,
          maktx      LIKE ztpp0089-maktx,
        END OF lt_hitlist.

  CLEAR ct_text." 这个顺序不要改，以防传入指针

  ct_text = pv_text.

  CLEAR pv_zppdhd.

  IF ct_text IS NOT INITIAL .
    SELECT SINGLE  zppdhd INTO  @pv_zppdhd
      FROM ztpp0089 WHERE zppdhd = @ct_text.
    IF sy-subrc EQ 0 .
      RETURN.
    ENDIF.
  ENDIF.

  REPLACE ALL OCCURRENCES OF '*' IN ct_text WITH '%'.
  IF strlen( ct_text ) < 29.
    CONCATENATE '%' ct_text '%' INTO ct_text.
  ELSEIF strlen( ct_text ) = 39.
    CONCATENATE ct_text '%' INTO ct_text.
  ENDIF.

  SELECT
    CASE WHEN c~zdhdsl <= 0 THEN '已删除' ELSE d~ddtext END AS text,
    c~zppdhd,
    c~zzpino,
    c~zkunnr_mat,
    c~zzkhxtno,
    c~zwerks,
    c~matnr,
    c~maktx,
    c~werks
    INTO CORRESPONDING FIELDS OF TABLE @lt_hitlist
    FROM ztpp0089 AS c
    LEFT JOIN dd07t AS d ON d~domname = 'ZDHDZT' AND  d~domvalue_l = c~zdhdzt
    WHERE c~zppdhd LIKE @ct_text.

  DATA: lr_zzpino TYPE RANGE OF zzpino.
  PERFORM frm_set_zzpino_hide TABLES lr_zzpino.
  IF lr_zzpino[] IS NOT INITIAL.
    DELETE lt_hitlist WHERE zzpino IN lr_zzpino.
  ENDIF.


  DATA(lv_lines) = lines( lt_hitlist ).

  IF lv_lines EQ 1 .
    READ TABLE lt_hitlist INTO DATA(ls_hitlist) INDEX 1.
    pv_zppdhd = ls_hitlist-zppdhd.
  ELSE.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'ZPPDHD'
        value_org       = 'S'
      TABLES
        value_tab       = lt_hitlist
        return_tab      = lt_ret_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc EQ 0 .
      READ TABLE lt_ret_tab INDEX 1.
      IF sy-subrc = 0.
        pv_zppdhd = lt_ret_tab-fieldval.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM .


FORM f_set_ebeln CHANGING cs_item TYPE zafo_sitem.
  SELECT SINGLE ebeln INTO @DATA(lv_ebeln)
    FROM zafo_head
    WHERE ebeln EQ @cs_item-ebeln.
  IF sy-subrc = 0.
    cs_item-ebeln = lv_ebeln.
  ELSE.
    MESSAGE s094 DISPLAY LIKE 'E' WITH cs_item-ebeln." 采购订单号&1不存在
    CLEAR cs_item-ebeln.
  ENDIF.
ENDFORM.

FORM f_set_ebeln_duc01 CHANGING cs_item TYPE zafo_sitem.

  CHECK cs_item-zzpino IS NOT INITIAL.
  CHECK cs_item-matnr IS NOT INITIAL.

  SELECT SINGLE mtart INTO @DATA(lv_mtart)
     FROM mara
    WHERE matnr = @cs_item-matnr.

  CHECK lv_mtart IS NOT INITIAL.

  IF lv_mtart = 'Z003' OR lv_mtart = 'Z004'.
    PERFORM f_set_ebeln_duc01_cy CHANGING cs_item.
  ELSE.
    PERFORM f_set_ebeln_duc01_cl CHANGING cs_item.

  ENDIF.

ENDFORM.


FORM f_set_ebeln_duc01_cy CHANGING cs_item TYPE zafo_sitem.
  IF cs_item-ebeln IS INITIAL.
    SELECT SINGLE zhtbh,neptr
      INTO @DATA(ls_ebeln)
      FROM ztpp_wwht_item
      WHERE zzpino EQ @cs_item-zzpino
        AND matnr EQ @cs_item-matnr.
    IF sy-subrc = 0.
      cs_item-ebeln = ls_ebeln-zhtbh.
      cs_item-price_long = ls_ebeln-neptr.
      PERFORM frm_set_price CHANGING cs_item.
      PERFORM f_set_amount CHANGING cs_item.
    ELSEIF cs_item-menge <> 0 OR cs_item-menge_cg <> 0.
      MESSAGE s093 DISPLAY LIKE 'E'." 采购订单信息不存在
    ENDIF.

  ELSE.

    SELECT SINGLE zhtbh,neptr
      INTO @ls_ebeln
      FROM ztpp_wwht_item
      WHERE zhtbh EQ @cs_item-ebeln
        AND matnr EQ @cs_item-matnr.
    IF sy-subrc = 0.
      cs_item-price_long = ls_ebeln-neptr.
      PERFORM frm_set_price CHANGING cs_item.
      PERFORM f_set_amount CHANGING cs_item.
    ELSEIF cs_item-menge <> 0 OR cs_item-menge_cg <> 0.
      MESSAGE s093 DISPLAY LIKE 'E'." 采购订单信息不存在
    ENDIF.

  ENDIF.

ENDFORM.


FORM f_set_ebeln_duc01_cl CHANGING cs_item TYPE zafo_sitem.

  IF cs_item-ebeln IS INITIAL.
    SELECT SINGLE ebeln,price_long
      INTO @DATA(ls_ebeln)
      FROM zafo_item
      WHERE zzpino EQ @cs_item-zzpino
        AND matnr EQ @cs_item-matnr
        AND del_flag <> 'X'.
    IF sy-subrc = 0.
      cs_item-ebeln = ls_ebeln-ebeln.
      cs_item-price_long = ls_ebeln-price_long.
      PERFORM frm_set_price CHANGING cs_item.
      PERFORM f_set_amount CHANGING cs_item.
    ELSEIF cs_item-menge <> 0 OR cs_item-menge_cg <> 0.
      MESSAGE s093 DISPLAY LIKE 'E'." 采购订单信息不存在
    ENDIF.

  ELSE.

    SELECT SINGLE ebeln,price_long
      INTO @ls_ebeln
      FROM zafo_item
      WHERE ebeln EQ @cs_item-ebeln
        AND matnr EQ @cs_item-matnr
        AND del_flag <> 'X'.
    IF sy-subrc = 0.
      cs_item-price_long = ls_ebeln-price_long.
      PERFORM frm_set_price CHANGING cs_item.
      PERFORM f_set_amount CHANGING cs_item.
    ELSEIF cs_item-menge <> 0 OR cs_item-menge_cg <> 0.
      MESSAGE s093 DISPLAY LIKE 'E'." 采购订单信息不存在
    ENDIF.

  ENDIF.

ENDFORM.


FORM f_set_zzpino CHANGING cs_item TYPE zafo_sitem.
  SELECT SINGLE * INTO @DATA(ls_ztpp0089)
    FROM ztpp0089
    WHERE zzpino EQ @cs_item-zzpino.
  IF sy-subrc = 0.
    cs_item-zzpino = ls_ztpp0089-zzpino.
  ELSE.
    CLEAR cs_item-zzpino.
    MESSAGE s013 DISPLAY LIKE 'E'." 选择的合同不存在,请重新选择
  ENDIF.
ENDFORM.


FORM f_set_zppdhd CHANGING cs_item TYPE zafo_sitem.
  SELECT SINGLE * INTO @DATA(ls_ztpp0089)
    FROM ztpp0089
    WHERE zppdhd EQ @cs_item-zppdhd.
  IF sy-subrc = 0.
    cs_item-zzpino = ls_ztpp0089-zzpino.
    cs_item-zkunnr_mat = ls_ztpp0089-zkunnr_mat.
    cs_item-vbeln_va = ls_ztpp0089-vbeln.
    cs_item-posnr_va = ls_ztpp0089-posnr.

    PERFORM f_set_zshd CHANGING cs_item.

  ELSE.
    CLEAR cs_item-zppdhd.
    MESSAGE s104 DISPLAY LIKE 'E'." 此大货通知单无面辅料单记录！
  ENDIF.
ENDFORM.


FORM f_set_zshd CHANGING cs_item TYPE zafo_sitem.

  CHECK cs_item-zppdhd IS NOT INITIAL.

  SELECT SINGLE ztpp0093~name1 INTO @DATA(lv_zshd)
    FROM ztpp0089
    LEFT JOIN ztpp0093 ON  ztpp0089~zwerks = ztpp0093~zwerks
    WHERE ztpp0089~zppdhd =  @cs_item-zppdhd.

  cs_item-zshd = lv_zshd.

ENDFORM.

FORM f_set_zkunnr_mat CHANGING cs_item TYPE zafo_sitem.
  SELECT SINGLE * INTO @DATA(ls_ztpp0089)
    FROM ztpp0089
    WHERE zkunnr_mat EQ @cs_item-zkunnr_mat.
  IF sy-subrc = 0.
    cs_item-zzpino = ls_ztpp0089-zzpino.
    cs_item-zppdhd = ls_ztpp0089-zppdhd.
    cs_item-zkunnr_mat = ls_ztpp0089-zkunnr_mat.
    cs_item-vbeln_va = ls_ztpp0089-vbeln.
    cs_item-posnr_va = ls_ztpp0089-posnr.
    PERFORM f_set_zshd CHANGING cs_item.
  ELSE.
    CLEAR cs_item-zkunnr_mat.
    MESSAGE s124 DISPLAY LIKE 'E'." 客户款号不存在！
  ENDIF.
ENDFORM.


FORM f_set_matkl CHANGING cs_item TYPE zafo_sitem." 物料组
  SELECT SINGLE * INTO @DATA(ls_t023t)
    FROM t023t
    WHERE matkl = @cs_item-matkl.
  IF sy-subrc EQ 0.
    cs_item-maktx = ls_t023t-wgbez.
  ENDIF.
ENDFORM.


FORM f_set_anln1 CHANGING cs_item TYPE zafo_sitem."主要资产编号
  SELECT SINGLE * INTO @DATA(ls_anla) FROM anla
    WHERE anln1 = @cs_item-anln1.
  IF sy-subrc EQ 0.
    cs_item-maktx = ls_anla-txt50.
    cs_item-meins = ls_anla-meins.
  ENDIF.
ENDFORM .


FORM f_matnr_f4 TABLES pt_ret_tab STRUCTURE ddshretval
                  USING pv_maktx TYPE maktx." 物料描述

  DATA: lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.
  DATA: BEGIN OF lt_hitlist OCCURS 0,
          matnr          LIKE makt-matnr,
          maktx          LIKE makt-maktx,
          idnlf          LIKE zmmt0010-idnlf,
          zmm_cf         LIKE zmmt0010-zmm_cf,
          zmm_gg         LIKE zmmt0010-zmm_gg,
          zcolor_text_zh LIKE zmmt0010-zcolor_text_zh,
          zcolor_text_en LIKE zmmt0010-zcolor_text_en,
          meins          LIKE mara-meins,
          bprme          LIKE zmmt0010-bprme,
          zmm_tran_rate  LIKE zmmt0010-zmm_tran_rate,
        END OF lt_hitlist.

  RANGES :r_mtart FOR mara-mtart .

  REFRESH pt_ret_tab.

  REPLACE ALL OCCURRENCES OF '*' IN pv_maktx WITH '%'.
  IF strlen( pv_maktx ) < 39.
    CONCATENATE '%' pv_maktx '%' INTO pv_maktx.
  ELSEIF strlen( pv_maktx ) = 39.
    CONCATENATE pv_maktx '%' INTO pv_maktx.
  ENDIF.

  REFRESH r_mtart.

  CASE gs_head-bsart.
    WHEN 'PO01'." 面辅料
      CLEAR r_mtart.
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z001' .
      APPEND r_mtart .
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z002' .
      APPEND r_mtart .
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z006' .
      APPEND r_mtart .
    WHEN 'PO02' OR 'PO05'." 备品备件/固定资产
      CLEAR r_mtart.
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z007' .
      APPEND r_mtart .
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z008' .
      APPEND r_mtart .
    WHEN 'PO03'." 贸易商品
      CLEAR r_mtart.
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z004' .
      APPEND r_mtart .
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z005' .
      APPEND r_mtart .
  ENDCASE.

*Z001	面料
*Z002	辅料
*Z003	半成品
*Z004	成衣
*Z005	贸易货物
*Z006	包装材料
*Z007	办公用品
*Z008	设备备件

  CASE gs_head-bustyp+0(1).
    WHEN 'E'.
      CLEAR r_mtart.
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z007' .
      APPEND r_mtart .
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z008' .
      APPEND r_mtart .
    WHEN 'R'.
      CLEAR r_mtart.
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z001' .
      APPEND r_mtart .
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z002' .
      APPEND r_mtart .
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z006' .
      APPEND r_mtart .
    WHEN 'S'.
      CLEAR r_mtart.
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z004' .
      APPEND r_mtart .
      r_mtart-low    =  'Z005' .
      APPEND r_mtart .
    WHEN 'D'.
      CLEAR r_mtart.
      r_mtart-sign   =  'I' .
      r_mtart-option =  'EQ' .
      r_mtart-low    =  'Z001' .
      APPEND r_mtart .
      r_mtart-low    =  'Z002' .
      APPEND r_mtart .
      r_mtart-low    =  'Z003' .
      APPEND r_mtart .
      r_mtart-low    =  'Z004' .
      APPEND r_mtart .
      r_mtart-low    =  'Z005' .
      APPEND r_mtart .
      r_mtart-low    =  'Z006' .
      APPEND r_mtart .
      r_mtart-low    =  'Z007' .
      APPEND r_mtart .
      r_mtart-low    =  'Z008' .
      APPEND r_mtart .
  ENDCASE.

  IF r_mtart[] IS INITIAL.
    MESSAGE s000 WITH '请先输入抬头信息！'.
    RETURN.
  ENDIF.

  SELECT t~matnr,
    t~maktx,
    m~meins,
    z~idnlf,
    z~zmm_cf,
    z~zmm_gg,
    z~zcolor_text_zh,
    z~zcolor_text_en,
    z~bprme,
    z~zmm_tran_rate
     INTO CORRESPONDING FIELDS OF TABLE @lt_hitlist
    FROM makt AS t
    LEFT JOIN mara AS m ON t~matnr = m~matnr
    LEFT JOIN zmmt0010 AS z ON t~matnr = z~matnr
    WHERE t~spras = @sy-langu
      AND m~mtart IN @r_mtart
      AND ( t~matnr LIKE @pv_maktx OR t~maktg LIKE @pv_maktx ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MATNR'
      value_org       = 'S'
    TABLES
      value_tab       = lt_hitlist
      return_tab      = lt_ret_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0 .
    pt_ret_tab[] = lt_ret_tab[].
  ENDIF.

ENDFORM.


FORM f_set_maktx CHANGING cs_item TYPE zafo_sitem." 物料描述
  DATA:ls_maktx TYPE maktx.
  ls_maktx = cs_item-matnr.

  DATA: lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  PERFORM f_matnr_f4 TABLES lt_ret_tab USING ls_maktx.
  IF lt_ret_tab[] IS NOT INITIAL.
    READ TABLE lt_ret_tab INDEX 1.
    IF sy-subrc = 0.
      cs_item-matnr = lt_ret_tab-fieldval.
    ELSE.
      CLEAR cs_item-matnr.
      CLEAR cs_item-maktx.
      CLEAR cs_item-maktx_kp.
      CLEAR cs_item-maktx_en.
      RETURN.
    ENDIF.
  ENDIF.

*  CALL FUNCTION 'MM_MAKTX_CONVERT'
*    EXPORTING
*      i_spras       = sy-langu
*      i_direction   = 'X'
*    CHANGING
*      c_matnr       = cs_item-matnr
*      c_maktx       = ls_maktx
*    EXCEPTIONS
*      error_message = 1.
*  IF sy-subrc NE 0.
*    CLEAR cs_item-matnr.
*    CLEAR cs_item-maktx.
*    RETURN.
*  ENDIF.

  CHECK cs_item-matnr IS NOT INITIAL.

  PERFORM f_set_matnr CHANGING cs_item.

ENDFORM.


FORM f_set_matnr CHANGING cs_item TYPE zafo_sitem.
  IF cs_item-matkl <> 'Z100' AND cs_item-matkl <> 'Z200'." 物料自带的类型不能修改费用类型
    SELECT SINGLE meins,matkl
      INTO (@cs_item-meins,@cs_item-matkl)
      FROM mara
      WHERE matnr = @cs_item-matnr.
  ENDIF.

  PERFORM f_set_offer CHANGING cs_item.

  IF gs_head-knttp = 'K'." 费用类型
    CLEAR cs_item-matnr.
  ENDIF.

ENDFORM.


FORM f_set_offer CHANGING cs_item TYPE zafo_sitem.

  SELECT SINGLE * INTO @DATA(ls_0051)
    FROM zmmt0051 WHERE matnr = @cs_item-matnr.
  IF sy-subrc EQ 0." 有报价单物料
    cs_item-ponam = ls_0051-zcguser.
    cs_item-matnr = ls_0051-matnr.
    cs_item-maktx = ls_0051-maktx.
    cs_item-maktx_kp = ls_0051-maktx_kp.
    cs_item-maktx_en = ls_0051-maktx_en.
    cs_item-idnlf = ls_0051-idnlf.
    cs_item-meins = ls_0051-meins.
    cs_item-bprme = ls_0051-bprme.
    cs_item-price_long = ls_0051-price_long .
    PERFORM frm_set_price CHANGING cs_item.
    cs_item-zcolor_text_zh = ls_0051-zcolor_text_zh.
    cs_item-zcolor_text_en = ls_0051-zcolor_text_en.
    cs_item-zsize = ls_0051-zmm_gg.
    cs_item-zmm_mf1 = ls_0051-zmm_mf1.
    IF gs_bustyp-bustyp = 'QC001'.
      cs_item-lifnr = ls_0051-lifnr.
      cs_item-lifnr_name = ls_0051-name1.
    ENDIF.

  ELSE." 无报价单物料
    SELECT SINGLE
      t~matnr,
      z~idnlf,
      t~maktx,
      z~zcolor_text_zh,
      z~zcolor_text_en,
      z~zmm_gg AS zsize,
      z~zmm_cf  AS znorms ,
      m~meins,
      z~bprme,
      z~zmm_tran_rate
   INTO CORRESPONDING FIELDS OF  @cs_item
      FROM makt AS t
      LEFT JOIN mara AS m ON t~matnr = m~matnr
      LEFT JOIN zmmt0010 AS z ON t~matnr = z~matnr
      WHERE t~spras = @sy-langu
      AND t~matnr = @cs_item-matnr.
    TRY .
        DATA(ll_menge) = 1 / cs_item-zmm_tran_rate.
      CATCH cx_sy_zerodivide.
        cs_item-zmm_tran_rate = 1.
    ENDTRY.
    IF cs_item-bprme IS INITIAL.
      cs_item-bprme = cs_item-meins.
    ENDIF.
  ENDIF.


  SELECT SINGLE a~zcolor,a~zcolor_text AS zcolor_text_zh,b~zcolor_text AS zcolor_text_en
  FROM zvcolor AS a
  INNER JOIN zvcolor AS b ON b~zcolor = a~zcolor
  WHERE a~spras = '1' AND  b~spras = 'E'
    AND a~zcolor_text = @cs_item-zcolor_text_zh
    AND b~zcolor_text = @cs_item-zcolor_text_en
    INTO @DATA(ls_zcolor).
  IF sy-subrc = 0.
    cs_item-zcolor = ls_zcolor-zcolor.
    cs_item-zcolor_text = ls_zcolor-zcolor_text_zh.
  ELSE.
    cs_item-zcolor = cs_item-zcolor_text_zh.
    cs_item-zcolor_text = cs_item-zcolor_text_zh.
  ENDIF.

  IF gs_head-knttp = 'K'." 费用类型
    cs_item-zmm_tran_rate = 1.
    IF cs_item-bprme IS NOT INITIAL.
      cs_item-meins = cs_item-bprme.
    ENDIF.

  ELSE.

    DATA: ls_matnr TYPE zmms0008.

    ls_matnr-matnr = cs_item-matnr.
    ls_matnr-meins = cs_item-meins.
    CALL FUNCTION 'ZMM_EX_CONV_RATE'
      CHANGING
        cs_matnr = ls_matnr.
    cs_item-zmm_tran_rate = ls_matnr-zmm_tran_rate.
    cs_item-bprme = ls_matnr-bprme.
  ENDIF.

ENDFORM.


FORM f_set_idnlf CHANGING cs_item TYPE zafo_sitem.
  DATA:ls_text TYPE char100.
  DATA:key TYPE char20.
  DATA: ls_0051 TYPE zmmt0051,
        lt_0051 TYPE TABLE OF zmmt0051.
  DATA:   lt_return      TYPE TABLE OF ddshretval WITH HEADER LINE.
  IF cs_item-matnr IS NOT INITIAL .
    CHECK cs_item-matnr+0(3) <> 'RKC'.
  ENDIF.

  REFRESH lt_0051.

  SELECT  * INTO TABLE @lt_0051
    FROM zmmt0051
    WHERE idnlf = @cs_item-idnlf
    AND del_flag <> 'X'.

  DATA(lv_line) = lines( lt_0051 ).

  IF lv_line EQ 1.
    READ TABLE lt_0051 INTO ls_0051 INDEX 1.
    cs_item-matnr = ls_0051-matnr.
    PERFORM f_set_matnr CHANGING cs_item.
  ELSE.
    ls_text = '%' && cs_item-idnlf && '%'.
    SELECT idnlf, matnr, maktx, name1, bprme, meins,
       zcolor_text_zh, zcolor_text_en, zmm_gg, zmm_mf1,
       lifnr, price_long, zmm_tran_rate,
       zremark, zofno, line_id
       FROM zvoffer
       WHERE idnlf LIKE @ls_text OR maktx LIKE @ls_text
       INTO TABLE @DATA(lt_offer).

    lv_line = lines( lt_offer ).

    IF lv_line = 0.
      MESSAGE s034 DISPLAY LIKE 'E'." 工厂款号不存在
    ELSEIF lv_line = 1.
      READ TABLE lt_offer INDEX 1 INTO DATA(ls_offer).
      IF sy-subrc EQ 0.
        cs_item-matnr = ls_offer-matnr.
        PERFORM f_set_matnr CHANGING cs_item.
      ENDIF.

    ELSEIF lv_line > 1.

      LOOP AT lt_offer INTO ls_offer.
        ls_offer-zremark = ls_offer-zofno && ls_offer-line_id.
        MODIFY lt_offer FROM ls_offer.
      ENDLOOP.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield         = 'ZREMARK'
          dynprofield      = 'ZREMARK'
          value_org        = 'S'
          callback_program = sy-repid
*         callback_form    = 'CB_FORM_301_IDNLF'
        TABLES
          value_tab        = lt_offer
          return_tab       = lt_return
        EXCEPTIONS
          parameter_error  = 1
          no_values_found  = 2
          OTHERS           = 3.

      READ TABLE lt_return INDEX 1.
      IF sy-subrc EQ 0.
        READ TABLE lt_offer INTO ls_offer WITH KEY zremark = lt_return-fieldval.
        IF sy-subrc EQ 0.
          cs_item-matnr = ls_offer-matnr.
          PERFORM f_set_matnr CHANGING cs_item.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.



FORM f_zcolor_f4 TABLES pt_ret_tab STRUCTURE ddshretval
                  USING pv_zcolor_text TYPE zcolor_text." 物料描述

  DATA: lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.
  DATA: BEGIN OF lt_hitlist OCCURS 0,
          zcolor         TYPE char40,
          zcolor_text    LIKE zafo_item-zcolor_text,
          zcolor_text_zh LIKE zafo_item-zcolor_text_zh,
          zcolor_text_en LIKE zafo_item-zcolor_text_en,
        END OF lt_hitlist.


  REFRESH pt_ret_tab.

  REPLACE ALL OCCURRENCES OF '*' IN pv_zcolor_text WITH '%'.
  IF strlen( pv_zcolor_text ) < 39.
    CONCATENATE '%' pv_zcolor_text '%' INTO pv_zcolor_text.
  ELSEIF strlen( pv_zcolor_text ) = 39.
    CONCATENATE pv_zcolor_text '%' INTO pv_zcolor_text.
  ENDIF.


  SELECT a~zcolor,a~zcolor_text,a~zcolor_text AS zcolor_text_zh,
    b~zcolor_text AS zcolor_text_en
    INTO CORRESPONDING FIELDS OF TABLE @lt_hitlist
    FROM zvcolor AS a
    LEFT JOIN zvcolor AS b ON a~zcolor = b~zcolor
    WHERE a~spras = '1'
      AND b~spras = 'E'
      AND a~zcolor_text LIKE @pv_zcolor_text.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZCOLOR'
      value_org       = 'S'
    TABLES
      value_tab       = lt_hitlist
      return_tab      = lt_ret_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0 .
    pt_ret_tab[] = lt_ret_tab[].
  ENDIF.

ENDFORM.



FORM f_set_zcolor_text CHANGING cs_item TYPE zafo_sitem.

  DATA:ls_zcolor_text TYPE zcolor_text.
  ls_zcolor_text = cs_item-zcolor_text.

  DATA:lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  PERFORM f_zcolor_f4 TABLES lt_ret_tab USING ls_zcolor_text.

  IF lt_ret_tab[] IS NOT INITIAL.
    READ TABLE lt_ret_tab INDEX 1.
    IF sy-subrc = 0.
      cs_item-zcolor = lt_ret_tab-fieldval.
    ELSE.
      CLEAR:cs_item-zcolor,
            cs_item-zcolor_text,
            cs_item-zcolor_text_zh,
            cs_item-zcolor_text_en.
    ENDIF.
    CHECK cs_item-zcolor IS NOT INITIAL.

    PERFORM f_set_zcolor CHANGING cs_item.

  ELSE.

    CLEAR: cs_item-zcolor_text_en.
    cs_item-zcolor = cs_item-zcolor_text.
    cs_item-zcolor_text_zh = cs_item-zcolor_text.
  ENDIF.


ENDFORM.


FORM f_set_zcolor CHANGING cs_item TYPE zafo_sitem.

  IF cs_item-zcolor IS INITIAL.
    CLEAR: cs_item-zcolor_text,
          cs_item-zcolor_text_zh,
          cs_item-zcolor_text_en.
    RETURN.
  ENDIF.

  SELECT SINGLE zcolor_text INTO @cs_item-zcolor_text_zh
    FROM zvcolor
    WHERE zcolor = @cs_item-zcolor
      AND spras = '1'.

  SELECT SINGLE zcolor_text INTO @cs_item-zcolor_text_en
    FROM zvcolor
    WHERE zcolor = @cs_item-zcolor
      AND spras = 'E'.

  cs_item-zcolor_text = cs_item-zcolor_text_zh.

ENDFORM.


FORM f_bom_f4 TABLES pt_ret_tab STRUCTURE ddshretval
               USING pv_zppdhd TYPE zppdhd
                     pv_multiple_choice TYPE char1." 物料描述

  DATA: lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  SELECT  a~zppdhd,
          a~zzbom_item ,
          a~idnlf,
          a~matnr,
          a~maktx,
          a~zcolor1,
          a~zsize,
          a~zcolor1 AS zcolor,
          a~zzjsssgg,
          a~zcqy,
          a~zzdh,
          e~meins
    FROM ztpp0091 AS a
    LEFT JOIN mara AS e ON a~matnr = e~matnr
    WHERE a~zppdhd = @pv_zppdhd
    AND a~zcqy = ''
    INTO TABLE @DATA(lt_data).

  IF sy-subrc NE 0.
    MESSAGE s038."'该大货单BOM中无此物料'
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZZBOM_ITEM'
      pvalkey         = 'ZZBOM_ITEM'
      dynprofield     = 'ZZBOM_ITEM'
      value_org       = 'S'
      multiple_choice = pv_multiple_choice
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
*     callback_program = sy-repid
*     callback_form   = 'FRM_RETURN_VALUE'
    TABLES
      value_tab       = lt_data
      return_tab      = lt_ret_tab[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0 .
    pt_ret_tab[] = lt_ret_tab[].
  ENDIF.

ENDFORM.


FORM f_set_umbom CHANGING cs_item TYPE zafo_sitem.
  IF cs_item-to_zppdhd IS INITIAL.
    MESSAGE s114."请先输入大货单号!
    CLEAR  cs_item-to_zzbom_item.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    INTO @DATA(ls_0091) FROM ztpp0091
    WHERE zppdhd = @cs_item-to_zppdhd
    AND zzbom_item = @cs_item-to_zzbom_item.

  IF sy-subrc NE 0.
    MESSAGE s115."该BOM行不存在，请重新选择!
    CLEAR  cs_item-to_zzbom_item.
    RETURN.
  ENDIF.

  IF cs_item-idnlf <> ls_0091-idnlf.
    MESSAGE s116."选择的工厂款号与本单不一致，请重新选择!
    CLEAR  cs_item-to_zzbom_item.
    RETURN.
  ENDIF.

  IF cs_item-zppflag <> ls_0091-zcqy.
    MESSAGE s117."选择的产前样标记与本单不一致，请重新选择!
    CLEAR  cs_item-to_zzbom_item.
    RETURN.
  ENDIF.

  cs_item-zcolor_um = ls_0091-zcolor1.
  cs_item-zsize_um = ls_0091-zsize.
  cs_item-znorms_um = ls_0091-zzjsssgg.
  cs_item-zppflag_um = ls_0091-zcqy.

  cs_item-zvat_nub_um = cs_item-zvat_nub.
  cs_item-zshelves_um = cs_item-zshelves.

ENDFORM.


FORM f_set_bom CHANGING cs_item TYPE zafo_sitem.

  IF cs_item-zppdhd IS INITIAL.
    MESSAGE s114."请先输入大货单号!
    CLEAR  cs_item-to_zzbom_item.
    RETURN.
  ENDIF.

  SELECT SINGLE * INTO @DATA(ls_0091)
    FROM ztpp0091
    WHERE zppdhd = @cs_item-zppdhd
      AND zzbom_item = @cs_item-zzbom_item.
  IF sy-subrc NE 0.
    MESSAGE s115."该BOM行不存在，请重新选择!
    CLEAR cs_item-zzbom_item.
    RETURN.
  ENDIF.

  cs_item-matnr = ls_0091-matnr.
  IF cs_item-matnr IS NOT INITIAL.
    PERFORM f_set_matnr CHANGING cs_item.
  ENDIF.

  cs_item-zcolor = ls_0091-zcolor1.

  PERFORM f_set_zcolor CHANGING cs_item.

  cs_item-zsize  = ls_0091-zsize.
  cs_item-znorms = ls_0091-zzjsssgg.

ENDFORM.


FORM frm_set_price_long USING price peinh CHANGING price_long." 设置可见单价
  IF peinh IS INITIAL.
    peinh = 1.
  ENDIF.

  price_long = price / peinh.
ENDFORM.


FORM frm_set_price CHANGING ls_item TYPE zafo_sitem." 设置内部单价
  CALL FUNCTION 'ZAFO_CONVERT_PRICE'
    EXPORTING
      price_long = ls_item-price_long
    IMPORTING
      price      = ls_item-price
      peinh      = ls_item-peinh.
ENDFORM.


FORM frm_set_amount1 CHANGING ls_item TYPE zafo_sitem." 根据金额设置单价

  IF ls_item-amount > 0 AND ls_item-menge_cg > 0." 有金额按金额算
    ls_item-price_long = ls_item-amount / ls_item-menge_cg.

    CALL FUNCTION 'ZAFO_CONVERT_PRICE'
      EXPORTING
        price_long = ls_item-price_long
      IMPORTING
        price      = ls_item-price
        peinh      = ls_item-peinh.
  ELSEIF ls_item-price_long > 0 AND ls_item-menge_cg > 0." 有单价按单价算
    ls_item-amount = ls_item-price_long * ls_item-menge_cg.

    CALL FUNCTION 'ZAFO_CONVERT_PRICE'
      EXPORTING
        price_long = ls_item-price_long
      IMPORTING
        price      = ls_item-price
        peinh      = ls_item-peinh.
  ENDIF.

ENDFORM.


FORM f_set_menge CHANGING cs_item TYPE zafo_sitem." 设置基本单位数量
  DATA:ls_tran_rate TYPE zmm_tran_rate.
  DATA:ls_menge_cg TYPE menge_d.
  CLEAR ls_menge_cg.

  ls_menge_cg = cs_item-menge_cg.

  ls_tran_rate = cs_item-zmm_tran_rate.
  TRY .
      DATA(ll_menge) = 1 / ls_tran_rate.
    CATCH cx_sy_zerodivide.
      ls_tran_rate = 1.
      cs_item-zmm_tran_rate = ls_tran_rate.
  ENDTRY.

  cs_item-menge = cs_item-menge_cg * ls_tran_rate.
  PERFORM frm_set_round USING <gs_item>-meins CHANGING cs_item-menge.
ENDFORM.


FORM f_set_menge_cg CHANGING cs_item TYPE zafo_sitem." 设置采购单位数量
  DATA:ls_tran_rate TYPE zmm_tran_rate.
  DATA:l_menge_cg TYPE p DECIMALS 9.

  PERFORM frm_set_round USING cs_item-meins CHANGING cs_item-menge.
  ls_tran_rate = cs_item-zmm_tran_rate.

  TRY .
      DATA(ll_menge) = 1 / ls_tran_rate.
    CATCH cx_sy_zerodivide.
      ls_tran_rate = 1.
      cs_item-zmm_tran_rate = ls_tran_rate.
  ENDTRY.

  l_menge_cg = cs_item-menge / ls_tran_rate.

  CALL FUNCTION 'ROUND'
    EXPORTING
      decimals = 3
      input    = l_menge_cg
      sign     = '+'
    IMPORTING
      output   = cs_item-menge_cg.

ENDFORM.


FORM f_set_menge_cg_dis CHANGING cs_item TYPE zafo_sitem." 设置采购单位数量
  DATA:ls_tran_rate TYPE zmm_tran_rate.
  DATA:l_menge_cg TYPE p DECIMALS 9.

  ls_tran_rate = cs_item-zmm_tran_rate.

  TRY .
      DATA(ll_menge) = 1 / ls_tran_rate.
    CATCH cx_sy_zerodivide.
      ls_tran_rate = 1.
      cs_item-zmm_tran_rate = ls_tran_rate.
  ENDTRY.
  cs_item-menge_cg = cs_item-menge / ls_tran_rate.
ENDFORM.


FORM frm_set_po_menge CHANGING ls_item TYPE zafo_sitem." gt_item_po 数量分摊
  DATA:ls_menge_sum TYPE menge_d.
  DATA:ls_menge_sumkey TYPE menge_d.
  DATA:ls_menge_last TYPE menge_d.

  IF gs_bustyp-execute_type = 'PO' OR gs_bustyp-execute_type = 'PRO'.
    PERFORM frm_po_item_calculation CHANGING ls_item.
  ELSEIF gs_bustyp-execute_type = 'POC'.
    PERFORM frm_poc_item_calculation CHANGING ls_item.
  ELSEIF gs_bustyp-execute_type = 'POP'.
    PERFORM frm_pop_item_calculation CHANGING ls_item.
  ENDIF.

  CLEAR ls_menge_sum.
  CLEAR ls_menge_sumkey.
  ls_menge_sum = ls_item-menge.
  LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = ls_item-afonr.
    ls_menge_sum = ls_menge_sum + <gs_item_po>-menge.
    ls_menge_sumkey = ls_menge_sumkey + <gs_item_po>-ktmng.
  ENDLOOP.

  IF ls_menge_sum <> ls_item-menge.
    ls_menge_last = ls_item-menge.
    SORT gt_item_po BY ktmng." 先分数量小的，不然取整之后最后一行可能为负数
    LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = ls_item-afonr.
      <gs_item_po>-menge = <gs_item_po>-ktmng * ls_item-menge / ls_menge_sumkey.
      IF <gs_item_po>-meins IS INITIAL .
        PERFORM frm_set_round USING ls_item-meins CHANGING <gs_item_po>-menge.
      ELSE.
        PERFORM frm_set_round USING <gs_item_po>-meins CHANGING <gs_item_po>-menge.
      ENDIF.
      ls_menge_last = ls_menge_last - <gs_item_po>-menge.
    ENDLOOP.
    IF <gs_item_po>-menge IS NOT INITIAL.
      <gs_item_po>-menge = <gs_item_po>-menge + ls_menge_last.
    ENDIF.
  ENDIF.
  SORT gt_item_po BY ebelp.
ENDFORM.


FORM f_set_amount CHANGING cs_item TYPE zafo_sitem.

  DATA:ls_tran_rate TYPE zmm_tran_rate.

  IF cs_item-peinh IS INITIAL .
    cs_item-peinh = 1.
  ENDIF.

  IF cs_item-bprme IS INITIAL.
    cs_item-bprme = cs_item-meins.
  ENDIF.

  ls_tran_rate = cs_item-zmm_tran_rate.

  IF ls_tran_rate IS INITIAL .
    ls_tran_rate = 1.
    cs_item-zmm_tran_rate = ls_tran_rate.
    CONDENSE cs_item-zmm_tran_rate NO-GAPS.
  ENDIF.


  IF cs_item-price IS NOT INITIAL AND cs_item-menge IS NOT INITIAL.
    CASE gs_bustyp-execute_type .
      WHEN 'PO' OR 'PRO' OR 'POC' OR 'POP'.
        IF ls_tran_rate <> 1 AND cs_item-menge_cg IS NOT INITIAL.
          cs_item-amount = cs_item-price * ( cs_item-menge_cg + cs_item-menge_zj ) / cs_item-peinh.
        ELSE.
          cs_item-amount = cs_item-price * cs_item-menge / cs_item-peinh / ls_tran_rate.
        ENDIF.
      WHEN OTHERS.
        IF ls_tran_rate <> 1 AND cs_item-menge_cg IS NOT INITIAL.
          cs_item-amount = cs_item-price * cs_item-menge_cg / cs_item-peinh.
        ELSE.
          cs_item-amount = cs_item-price * cs_item-menge / cs_item-peinh / ls_tran_rate.
        ENDIF.
    ENDCASE.

  ELSE.
    CASE gs_bustyp-object.
      WHEN 'COST' OR 'COST01' OR 'COST02' OR 'DUC' OR 'POF01'.

      WHEN OTHERS.
        cs_item-amount = 0.
    ENDCASE.
  ENDIF.

ENDFORM.


FORM frm_set_po_eeind CHANGING ls_item TYPE zafo_sitem.

  CHECK gt_item_po[] IS NOT INITIAL.

  LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = ls_item-afonr.
    <gs_item_po>-eeind = ls_item-eeind.
  ENDLOOP.
ENDFORM.



FORM frm_set_po_amount CHANGING ls_item TYPE zafo_sitem." gt_item_po 金额分摊
  DATA:lv_amount TYPE zafo_amount.

  CHECK gt_item_po[] IS NOT INITIAL.

  lv_amount = ls_item-amount.
  LOOP AT gt_item_po ASSIGNING <gs_item_po> WHERE afonr = ls_item-afonr.
    <gs_item_po>-zmm_tran_rate = ls_item-zmm_tran_rate.
    <gs_item_po>-peinh = ls_item-peinh.
    <gs_item_po>-price = ls_item-price.
    IF ls_item-menge IS NOT INITIAL.
      <gs_item_po>-amount = ls_item-amount / ls_item-menge * <gs_item_po>-menge .
      lv_amount = lv_amount - <gs_item_po>-amount.
    ENDIF.
  ENDLOOP.
  IF sy-subrc EQ 0 AND <gs_item_po>-amount IS NOT INITIAL.
    <gs_item_po>-amount = <gs_item_po>-amount + lv_amount.
  ENDIF.

ENDFORM.


FORM frm_po_item_calculation CHANGING cs_item TYPE zafo_sitem.

  DATA:l_menge_cg TYPE p DECIMALS 9.
  DATA:l_menge TYPE p DECIMALS 9.
  DATA:lv_decimals TYPE i VALUE 3.

  CONDENSE cs_item-zmm_tran_rate NO-GAPS.

  IF cs_item-zmm_tran_rate IS INITIAL
    OR cs_item-zmm_tran_rate = '0.000'
    OR cs_item-zmm_tran_rate = '1.000' .
    cs_item-zmm_tran_rate = 1.
  ENDIF.

  IF cs_item-peinh  IS INITIAL.
    cs_item-zmm_tran_rate = 1.
  ENDIF.
  " 未清数  =  制单数 - 已采购数  - 使用库存数
  cs_item-menge3 = cs_item-menge1 - cs_item-menge2 - cs_item-menge4.

  IF cs_item-menge3 < 0 AND gs_bustyp-busref <> 'L'.
    cs_item-menge3 = 0.
  ENDIF.
  " 采购数 = 未清数  / 转换率
  l_menge_cg = cs_item-menge3 / cs_item-zmm_tran_rate.

  IF cs_item-maktx = '胶条' .
    lv_decimals = 2.
  ENDIF.


  CALL FUNCTION 'ROUND'
    EXPORTING
      decimals = lv_decimals
      input    = l_menge_cg
      sign     = '+'
    IMPORTING
      output   = cs_item-menge_cg.

  l_menge = cs_item-menge_cg + cs_item-menge_zj .
  cs_item-menge = l_menge * cs_item-zmm_tran_rate.
  cs_item-amount = cs_item-price * cs_item-menge / cs_item-peinh / cs_item-zmm_tran_rate.

ENDFORM.


FORM frm_pot_item_calculation CHANGING ct_item TYPE zafo_sitem.
  IF ct_item-zmm_tran_rate IS INITIAL.
    ct_item-zmm_tran_rate = 1.
  ENDIF.

  IF ct_item-peinh  IS INITIAL.
    ct_item-zmm_tran_rate = 1.
  ENDIF.

  ct_item-menge =  ct_item-menge_cg  * ct_item-zmm_tran_rate.
  ct_item-amount = ct_item-price * ct_item-menge / ct_item-peinh / ct_item-zmm_tran_rate.
ENDFORM.


FORM frm_poc_item_calculation CHANGING cs_item TYPE zafo_sitem.
  IF cs_item-zmm_tran_rate IS INITIAL.
    cs_item-zmm_tran_rate = 1.
  ENDIF.

  IF cs_item-peinh  IS INITIAL.
    cs_item-zmm_tran_rate = 1.
  ENDIF.
  cs_item-menge = ( cs_item-menge_cg + cs_item-menge_zj ) * cs_item-zmm_tran_rate.
  cs_item-amount = cs_item-price * cs_item-menge / cs_item-peinh / cs_item-zmm_tran_rate.
ENDFORM.


FORM frm_pop_item_calculation CHANGING cs_item TYPE zafo_sitem.
  IF cs_item-zmm_tran_rate IS INITIAL.
    cs_item-zmm_tran_rate = 1.
  ENDIF.

  IF cs_item-peinh  IS INITIAL.
    cs_item-zmm_tran_rate = 1.
  ENDIF.

  cs_item-amount = cs_item-price_long  * cs_item-menge_cg.
ENDFORM.
