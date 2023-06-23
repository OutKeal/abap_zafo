*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.   .
  SELECT-OPTIONS: s_werks    FOR zafo_qc_item-werks ,
                  s_lgort    FOR zafo_item-lgort MEMORY ID lag MODIF ID ab,
                  s_lifnr    FOR zafo_qc_item-lifnr ,
                  s_zzpino   FOR zafo_qc_item-zzpino ,
                  s_zppdhd   FOR zafo_qc_item-zppdhd ,
                  s_idnlf    FOR zafo_qc_item-idnlf ,
                  s_matnr    FOR zafo_qc_item-matnr ,
                  s_maktx    FOR zafo_qc_item-matnr ,
                  s_shafo    FOR zafo_qc_item-afono MODIF ID a,
                  s_ebeln    FOR zafo_qc_item-ebeln MODIF ID ab,
                  s_shdate   FOR zafo_qc_item-datum_gr MODIF ID a," 收货日期
                  s_rkafo    FOR zafo_qc_item-afono  MODIF ID b,  " 入库单
                  s_rkdate   FOR zafo_qc_item-datum_gr MODIF ID b," 入库日期
                  s_poafo    FOR zafo_qc_item-ebeln MODIF ID c,
                  s_podate   FOR zafo_qc_item-datum_gr MODIF ID c." 合同日期
SELECTION-SCREEN:END OF  BLOCK b1.


SELECTION-SCREEN:BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_qcno    FOR zafo_qc_item-qcno MODIF ID dis,
                  s_ernam   FOR zafo_qc_item-ernam MODIF ID dis DEFAULT sy-uname,
                  s_erdat   FOR zafo_qc_item-erdat MODIF ID dis,
                  s_status  FOR zafo_qc_item-qc_status MODIF ID dis.
SELECTION-SCREEN:END OF  BLOCK b2.


SELECTION-SCREEN:BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.   .
  PARAMETERS: p_cre TYPE c RADIOBUTTON GROUP g2 USER-COMMAND singleclick, "创建
              p_dis TYPE c RADIOBUTTON GROUP g2.
SELECTION-SCREEN:END OF BLOCK b3.
