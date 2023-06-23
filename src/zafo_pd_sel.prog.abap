*&---------------------------------------------------------------------*
*& 包含               ZAFO_PD_SEL
*&---------------------------------------------------------------------*


SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.   .
  PARAMETERS:
    p_werks TYPE t001l-werks MEMORY ID wrk OBLIGATORY,
    p_lgort TYPE t001l-lgort MODIF ID cre MEMORY ID lgo OBLIGATORY.
  SELECT-OPTIONS:
              s_zcate1 FOR zafo_pd_item-zcate1 MODIF ID cre,
              s_zcate2 FOR zafo_pd_item-zcate2 MODIF ID cre,
              s_matnr  FOR zafo_pd_item-matnr MODIF ID cre,
              s_idnlf  FOR zafo_pd_item-idnlf MODIF ID cre.
SELECTION-SCREEN:END OF  BLOCK b1.


SELECTION-SCREEN:BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.   .
  SELECT-OPTIONS:
              s_zzpino FOR zafo_pd_item-zzpino MODIF ID cre,
              s_zppdhd FOR zafo_pd_item-zppdhd MODIF ID cre,
              s_kunnr FOR zafo_pd_item-kunnr MODIF ID cre,
              s_name1 FOR zafo_pd_item-name1 MODIF ID cre.
SELECTION-SCREEN:END OF  BLOCK b2.


SELECTION-SCREEN:BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS:
    p_afopd TYPE zafo_pd_head-afopd  MODIF ID dis MEMORY ID zafopd.
  SELECT-OPTIONS:
              s_afopd FOR zafo_pd_head-afopd MODIF ID dis,
*              s_werks FOR zafo_pd_head-werks MODIF ID dis,
              s_lgort FOR zafo_pd_head-lgort MODIF ID dis,
              s_remark FOR zafo_pd_head-remark MODIF ID dis,
              s_status FOR zafo_pd_head-pdstatus MODIF ID dis,
              s_erdat FOR zafo_pd_head-erdat MODIF ID dis.
SELECTION-SCREEN:END OF  BLOCK b3.
SELECTION-SCREEN:BEGIN OF BLOCK b20 WITH FRAME TITLE TEXT-020.   .
  PARAMETERS: p_cre TYPE c RADIOBUTTON GROUP g2 USER-COMMAND singleclick, "创建

              p_dis TYPE c RADIOBUTTON GROUP g2. "查询
*              p_load TYPE c RADIOBUTTON GROUP g2. "上载
SELECTION-SCREEN:END OF  BLOCK b20.
