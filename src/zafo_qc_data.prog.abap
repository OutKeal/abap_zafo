*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_DATA
*&---------------------------------------------------------------------*

FORM frm_create_sh.

  SELECT h~afono,
      i~afonr,
      i~zzpino,
      i~zppdhd,
      i~werks,
      i~ebeln,
      budat AS datum_gr,
      i~lifnr ,
      i~lifnr_name ,
      i~matnr ,
      i~idnlf ,
      i~maktx ,
      i~zcolor  ,
      i~zcolor_text  ,
      i~zsize,
      i~znorms,
      i~zppflag,
      i~meins ,
      i~bprme ,
      i~zmm_tran_rate ,
      i~zvat_nub,
      i~qcno,
      i~qc_status,
      i~menge AS menge,
      i~menge_cg AS menge_gr
    INTO CORRESPONDING FIELDS OF  TABLE @gt_head
    FROM zafo_head AS h
    INNER JOIN zafo_item AS i ON h~afono = i~afono
    WHERE h~bustyp = 'R1001'
    AND i~werks IN @s_werks
    AND i~lifnr IN @s_lifnr
    AND h~afono IN @s_shafo
    AND h~bldat IN @s_shdate
    AND i~zzpino IN @s_zzpino
    AND i~idnlf IN @s_idnlf
    AND i~ebeln IN @s_ebeln
    AND i~matnr IN @s_matnr
    AND i~maktx IN @s_maktx
    AND i~del_flag = ''
    AND i~lgort IN @s_lgort
    AND i~menge <> 0
    AND h~status <> 'D'
    AND ( i~qc_status = 'A' OR i~qc_status = '').

  LOOP AT gt_head ASSIGNING <gs_head>.
    <gs_head>-qcmode = 'A'.
    <gs_head>-qc_status = 'A'.
    PERFORM frm_set_icon USING <gs_head>-qc_status CHANGING <gs_head>-icon <gs_head>-text .
  ENDLOOP.
ENDFORM.


FORM frm_create_rk.

  SELECT h~afono,
      i~afonr,
      i~zzpino,
      i~zppdhd,
      i~werks,
      i~ebeln,
      budat AS datum_gr,
      i~lifnr ,
      i~lifnr_name ,
      i~matnr ,
      i~idnlf ,
      i~maktx ,
      i~zcolor  ,
      i~zcolor_text  ,
      i~zsize,
      i~znorms,
      i~zppflag,
      i~meins ,
      i~bprme ,
      i~zmm_tran_rate ,
      i~zvat_nub,
      i~qcno,
      i~qc_status,
      i~menge AS menge,
      i~menge_cg AS menge_gr
    INTO CORRESPONDING FIELDS OF  TABLE @gt_head
    FROM zafo_head AS h
    INNER JOIN zafo_item AS i ON h~afono = i~afono
    WHERE h~bustyp IN ( 'R1006' , 'R1011' )
    AND i~werks IN @s_werks
    AND i~lifnr IN @s_lifnr
    AND h~afono IN @s_rkafo
    AND h~bldat IN @s_rkdate
    AND h~status IN ('S','T')
    AND i~zzpino IN @s_zzpino
    AND i~idnlf IN @s_idnlf
    AND i~ebeln IN @s_ebeln
    AND i~matnr IN @s_matnr
    AND i~maktx IN @s_maktx
    AND i~lgort IN @s_lgort
    AND i~menge <> 0
    AND i~del_flag = ''
    AND ( i~qc_status = 'A' OR i~qc_status = '').

  LOOP AT gt_head ASSIGNING <gs_head>.
    <gs_head>-qcmode = 'B'.
    <gs_head>-qc_status = 'A'.
    PERFORM frm_set_icon USING <gs_head>-qc_status CHANGING <gs_head>-icon <gs_head>-text .
  ENDLOOP.
ENDFORM.



FORM frm_create_cg.

  SELECT h~afono,
        i~afonr,
        i~zzpino,
        i~zppdhd,
        i~werks,
        i~ebeln,
        h~budat AS datum_gr,
        i~lifnr ,
        i~lifnr_name ,
        i~matnr,
        i~idnlf,
        i~maktx,
        i~zcolor,
        i~zcolor_text,
        i~zsize,
        i~znorms,
        i~zppflag,
        i~meins,
        i~bprme,
        i~zmm_tran_rate,
        i~zvat_nub,
        i~menge AS menge,
        i~menge_cg AS menge_gr
    INTO CORRESPONDING FIELDS OF TABLE @gt_head
    FROM zafo_head AS h
    INNER JOIN zafo_item AS i ON h~afono = i~afono
    WHERE h~bustyp IN ( 'PO001','PO004' )
    AND h~status = 'C'
    AND h~bldat IN @s_podate
    AND i~werks IN @s_werks
    AND i~lifnr IN @s_lifnr
    AND h~afono IN @s_poafo
    AND i~zzpino IN @s_zzpino
    AND i~idnlf IN @s_idnlf
    AND i~ebeln IN @s_ebeln
    AND i~matnr IN @s_matnr
    AND i~maktx IN @s_maktx
    AND i~del_flag = ''
    AND i~menge <> 0
    AND ( i~qc_status = 'A' OR i~qc_status = '').

  LOOP AT gt_head ASSIGNING <gs_head>.
    <gs_head>-qcmode = 'C'.
    <gs_head>-qc_status = 'A'.
    PERFORM frm_set_icon USING <gs_head>-qc_status CHANGING <gs_head>-icon <gs_head>-text .
  ENDLOOP.
ENDFORM.


FORM frm_get_data.
  DATA: lt_head LIKE TABLE OF gt_head.

  SELECT DISTINCT q~qcno
    FROM zafo_qc_head AS q
    LEFT JOIN zafo_item AS i ON i~afono = q~afono AND i~afonr = q~afonr
    INTO CORRESPONDING FIELDS OF TABLE @lt_head
    WHERE q~qcmode = @gv_qcmode
    AND q~werks IN @s_werks
    AND q~lifnr IN @s_lifnr
    AND i~lgort IN @s_lgort
    AND q~afono IN @s_shafo
    AND q~datum_gr IN @s_shdate
    AND q~afono IN @s_rkafo
    AND q~datum_gr IN @s_rkdate
    AND q~afono IN @s_poafo
    AND q~datum_gr IN @s_podate
    AND q~zzpino IN @s_zzpino
    AND q~zppdhd IN @s_zppdhd
    AND q~idnlf IN @s_idnlf
    AND q~ebeln IN @s_ebeln
    AND q~matnr IN @s_matnr
    AND q~maktx IN @s_maktx
    AND q~qcno IN @s_qcno
    AND q~qc_status IN @s_status
    AND q~ernam IN @s_ernam
    AND q~erdat IN @s_erdat.

  CHECK sy-subrc EQ 0.

  SELECT * FROM zafo_qc_head
    INTO CORRESPONDING FIELDS OF TABLE @gt_head
    FOR ALL ENTRIES IN @lt_head
    WHERE qcno = @lt_head-qcno.

  LOOP AT gt_head ASSIGNING <gs_head>.
    PERFORM frm_set_icon USING <gs_head>-qc_status CHANGING <gs_head>-icon <gs_head>-text .
  ENDLOOP.

ENDFORM.


FORM frm_get_data_cx.
  DATA: lt_head LIKE TABLE OF gt_head.

  SELECT DISTINCT qcno
    FROM zafo_qc_head
    INTO CORRESPONDING FIELDS OF TABLE @lt_head
    WHERE ( qcmode = 'A' OR qcmode = 'B ')
      AND zzpino IN @s_zzpino
      AND zppdhd IN @s_zppdhd
      AND matnr IN @s_matnr
      AND maktx IN @s_maktx.

  SELECT DISTINCT qcno
    FROM zafo_qc_head
    APPENDING CORRESPONDING FIELDS OF TABLE @lt_head
   WHERE qcmode = 'C'
    AND zzpino IN @s_zzpino
*      AND zppdhd IN @s_zppdhd " 外发的不按没有大货通知单号
      AND matnr IN @s_matnr
      AND maktx IN @s_maktx.

  CHECK lt_head[] IS NOT INITIAL.

  SELECT * FROM zafo_qc_item
    INTO CORRESPONDING FIELDS OF TABLE @gt_item
    FOR ALL ENTRIES IN @lt_head
    WHERE qcno = @lt_head-qcno.

ENDFORM.



FORM frm_get_kc." 获取质检单对应库存数
  DATA: lt_item TYPE TABLE OF zafo_item WITH HEADER LINE.
  FIELD-SYMBOLS <fs_item> TYPE zafo_item.

  CHECK gv_qcmode = 'B'.

  SELECT afono,afonr,werks,lgort,matnr,charg,vbeln_va,posnr_va,
          menge,
          zmm_tran_rate,
          meins
    INTO CORRESPONDING FIELDS OF TABLE @lt_item
    FROM zafo_item AS i
    FOR ALL ENTRIES IN @gt_head
    WHERE afono = @gt_head-afono
      AND afonr = @gt_head-afonr.


  SELECT werks,lgort,matnr,charg,
          vbeln AS vbeln_va,
          posnr AS posnr_va,
          kalab AS menge4
    INTO TABLE @DATA(lt_kc)
    FROM mska
    FOR ALL ENTRIES IN @lt_item
    WHERE lgort =  @lt_item-lgort
      AND werks =  @lt_item-werks
      AND matnr =  @lt_item-matnr
      AND charg =  @lt_item-charg
      AND vbeln =  @lt_item-vbeln_va
      AND posnr =  @lt_item-posnr_va
      AND kalab > 0.


  DATA:ls_menge_sum TYPE menge_d.
  DATA:ls_menge_sumkey TYPE menge_d.
  DATA:ls_menge_last TYPE menge_d.

  LOOP AT lt_kc INTO DATA(ls_kc).
    CLEAR ls_menge_sum." 分摊总数
    CLEAR ls_menge_sumkey." 分摊基准总数
    ls_menge_sum = ls_kc-menge4.
    LOOP AT lt_item  WHERE werks = ls_kc-werks
                       AND lgort = ls_kc-lgort
                       AND matnr = ls_kc-matnr
                       AND charg = ls_kc-charg
                       AND vbeln_va = ls_kc-vbeln_va
                       AND posnr_va = ls_kc-posnr_va.
      ls_menge_sumkey = ls_menge_sumkey + lt_item-menge." 入库数
    ENDLOOP.

    ls_menge_last = ls_kc-menge4.
    SORT lt_item BY menge." 先分数量小的，不然取整之后最后一行可能为负数
    LOOP AT lt_item ASSIGNING <fs_item> WHERE  werks = ls_kc-werks
                                          AND lgort = ls_kc-lgort
                                          AND matnr = lt_item-matnr
                                          AND charg = lt_item-charg
                                          AND vbeln_va = lt_item-vbeln_va
                                          AND posnr_va = lt_item-posnr_va.
      <fs_item>-menge4 = <fs_item>-menge * ls_kc-menge4 / ls_menge_sumkey.
      PERFORM frm_set_round USING <fs_item>-meins CHANGING <fs_item>-menge4.
      ls_menge_last = ls_menge_last - <fs_item>-menge4.
    ENDLOOP.
    IF <fs_item>-menge4 IS NOT INITIAL.
      <fs_item>-menge4 = <fs_item>-menge4 + ls_menge_last.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_head ASSIGNING <gs_head>.
    READ TABLE lt_item WITH KEY afono = <gs_head>-afono afonr = <gs_head>-afonr.
    IF sy-subrc = 0.
      TRY .
          DATA(ll_menge) = 1 / lt_item-zmm_tran_rate.
        CATCH cx_sy_zerodivide.
          lt_item-zmm_tran_rate = 1.
      ENDTRY.
      <gs_head>-menge_kc = lt_item-menge4 / lt_item-zmm_tran_rate.

    ENDIF.
    IF <gs_head>-menge_kc > <gs_head>-menge_gr." 库存大于当前入库单入库数的 取入库数
      <gs_head>-menge_kc = <gs_head>-menge_gr.
    ENDIF.

  ENDLOOP.

ENDFORM.
