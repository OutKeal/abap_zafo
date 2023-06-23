*&---------------------------------------------------------------------*
*& 包含               ZAFO_SEL
*&---------------------------------------------------------------------*


SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.   .
  PARAMETERS: p_typ   LIKE zafo_object-object AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND typ,
              p_afono LIKE zafo_head-afono MODIF ID mod MEMORY ID zafono.
  SELECT-OPTIONS: s_werks FOR t001w-werks NO INTERVALS.
  PARAMETERS: p_werks   TYPE t001w-werks MEMORY ID wrk .
SELECTION-SCREEN:END OF  BLOCK b1.


SELECTION-SCREEN:BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_lgort   FOR t001l-lgort MODIF ID mb.
*                 s_charg   FOR mchb-charg MODIF ID mb.
SELECTION-SCREEN:END OF  BLOCK b2.


SELECTION-SCREEN:BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS:  s_zzpino  FOR zsdsch-zzpino MODIF ID sc,
                   s_zppdhd  FOR ztpp0089-zppdhd MODIF ID sc,
                   s_kunnr FOR ztpp0089-kunnr MODIF ID ku,
                   s_vgbel FOR zafo_head-remark1 MODIF ID vg,
                   s_zzwlly  FOR ztpp0091-zzwlly MODIF ID ly,
                   s_auart  FOR vbak-auart MODIF ID so,
                   s_vkgrp  FOR vbak-vkgrp MODIF ID so,
                   s_vbeln  FOR vbak-vbeln MODIF ID so.
SELECTION-SCREEN:END OF  BLOCK b3.


SELECTION-SCREEN:BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  SELECT-OPTIONS: s_bsart   FOR ekko-bsart MODIF ID po,
                  s_ekgrp   FOR ekko-ekgrp MODIF ID po,
                  s_ebeln   FOR ekko-ebeln MODIF ID po,
                  s_lifnr   FOR ekko-lifnr MODIF ID po,
                  s_pernam   FOR zafo_head-ernam MODIF ID po,
                  s_potext   FOR zafo_head-remark1 MODIF ID po,
                  s_zname1   FOR zscmt0010-zname1 MODIF ID po.
SELECTION-SCREEN:END OF  BLOCK b4.


SELECTION-SCREEN:BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS: s_matnr   FOR mara-matnr MODIF ID mat,
                  s_maktx   FOR zmmt0010-maktx MODIF ID mat,
                  s_mtart   FOR mara-mtart MODIF ID mat,
                  s_cate1   FOR zmmt0010-zcate1 MODIF ID mat,
                  s_cate2   FOR zmmt0010-zcate2 MODIF ID mat,
                  s_idnlf   FOR zmmt0010-idnlf MODIF ID mat.
SELECTION-SCREEN:END OF  BLOCK b5.


SELECTION-SCREEN:BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
  SELECT-OPTIONS: s_qcno   FOR zafo_item-qcno MODIF ID qc.
SELECTION-SCREEN:END OF  BLOCK b6.


SELECTION-SCREEN:BEGIN OF BLOCK b7 WITH FRAME TITLE TEXT-007.
  SELECT-OPTIONS: s_afono   FOR zafo_head-afono MODIF ID dis,
                  s_status  FOR zafo_head-status MODIF ID dis,
                  s_ernam   FOR zafo_head-ernam MODIF ID dis,
                  s_erdat   FOR zafo_head-erdat MODIF ID dis,
                  s_budat   FOR zafo_head-budat MODIF ID dis,
                  s_nenam   FOR zafo_head-nenam MODIF ID ne.
SELECTION-SCREEN:END OF  BLOCK b7.


SELECTION-SCREEN:BEGIN OF BLOCK b20 WITH FRAME TITLE TEXT-020.   .
  PARAMETERS: p_cre  TYPE c RADIOBUTTON GROUP g2 USER-COMMAND con, "创建
              p_dis  TYPE c RADIOBUTTON GROUP g2, "查询
              p_load TYPE c RADIOBUTTON GROUP g2, "上载
              p_mod  TYPE c RADIOBUTTON GROUP g2. "修改
  PARAMETERS: p_fin  TYPE c AS CHECKBOX.
  PARAMETERS: p_scan TYPE c AS CHECKBOX.
  PARAMETERS: p_self TYPE c AS CHECKBOX."add by at-yuxs 20220104只查看自己 费用登记用

SELECTION-SCREEN:END OF  BLOCK b20.

SELECTION-SCREEN BEGIN OF BLOCK b30 WITH FRAME TITLE TEXT-030.
  SELECTION-SCREEN PUSHBUTTON 2(12)  b_down  USER-COMMAND down MODIF ID lod .
SELECTION-SCREEN END OF BLOCK b30.

SELECTION-SCREEN BEGIN OF BLOCK b40 WITH FRAME TITLE TEXT-040.
  PARAMETERS:p_file LIKE rlgrap-filename OBLIGATORY DEFAULT 'D:\' MODIF ID lod MEMORY ID zfile_path.  "导入文件的路径
  PARAMETERS:p_row TYPE i OBLIGATORY DEFAULT '1' MODIF ID lod.
SELECTION-SCREEN END OF BLOCK b40.

DATA:l_ok TYPE char1 VALUE 'X'.

FORM frm_vendor_search_s.
  DATA: lv_lifnr TYPE lifnr.
  DATA: lv_lifnr_name TYPE name1.

  LOOP AT s_lifnr WHERE low IS NOT INITIAL.
    CALL FUNCTION 'ZAFO_VENDOR_SEARCH'
      CHANGING
        lifnr   = s_lifnr-low
        o_lifnr = lv_lifnr.
    s_lifnr-low = lv_lifnr.
    MODIFY s_lifnr.
  ENDLOOP.

  LOOP AT s_lifnr WHERE high IS NOT INITIAL.
    CALL FUNCTION 'ZAFO_VENDOR_SEARCH'
      CHANGING
        lifnr   = s_lifnr-high
        o_lifnr = lv_lifnr.
    s_lifnr-high = lv_lifnr.
    MODIFY s_lifnr.
  ENDLOOP.

ENDFORM.


FORM frm_vendor_search_sname.
  DATA: lv_lifnr TYPE lifnr.
  DATA: lv_lifnr_name TYPE name1.

  LOOP AT s_zname1 WHERE low IS NOT INITIAL.
    CALL FUNCTION 'ZAFO_VENDOR_SEARCH'
      CHANGING
        lifnr        = s_zname1-low
        o_lifnr_name = lv_lifnr_name.

    s_zname1-low = lv_lifnr_name.
    MODIFY s_zname1.
  ENDLOOP.

  LOOP AT s_zname1 WHERE high IS NOT INITIAL.
    CALL FUNCTION 'ZAFO_VENDOR_SEARCH'
      CHANGING
        lifnr        = s_zname1-high
        o_lifnr_name = lv_lifnr_name.
    s_zname1-high = lv_lifnr_name.
    MODIFY s_zname1.
  ENDLOOP.

ENDFORM.
