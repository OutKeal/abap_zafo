*&---------------------------------------------------------------------*
*& 包含               ZAFO_SEL
*&---------------------------------------------------------------------*


SELECTION-SCREEN:BEGIN OF BLOCK b20 WITH FRAME TITLE TEXT-010.   .
  PARAMETERS: p_cre  TYPE c RADIOBUTTON GROUP g2 USER-COMMAND con, "创建
              p_ref  TYPE c RADIOBUTTON GROUP g2, "参考创建
              p_dis  TYPE c RADIOBUTTON GROUP g2, "查询
              p_load TYPE c RADIOBUTTON GROUP g2, "上载
              p_mod  TYPE c RADIOBUTTON GROUP g2. "修改
  PARAMETERS: p_fin TYPE c AS CHECKBOX.
SELECTION-SCREEN:END OF  BLOCK b20.

SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.   .
  PARAMETERS: p_typ   LIKE zafo_bustype-bustyp AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND con,
              p_afono LIKE zafo_head-afono MODIF ID mod MEMORY ID zafono. "
  SELECT-OPTIONS: s_werks FOR t001l-werks NO INTERVALS MEMORY ID wrk.
  PARAMETERS:
              p_werks   TYPE t001l-werks MEMORY ID wrk .
SELECTION-SCREEN:END OF  BLOCK b1.


SELECTION-SCREEN:BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS:
              s_lgort   FOR t001l-lgort MODIF ID mb,
              s_charg   FOR mchb-charg MODIF ID mb.
SELECTION-SCREEN:END OF  BLOCK b2.

SELECTION-SCREEN:BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS:
            s_vbeln FOR zafo_head-kdauf MODIF ID so,
            s_auart FOR zafo_head-auart MODIF ID so,
            s_vkorg FOR zafo_head-vkorg MODIF ID so,
            s_kunnr FOR zafo_head-kunnr MODIF ID so,
            s_exord FOR zafo_head-exord MODIF ID so.
SELECTION-SCREEN:END OF  BLOCK b3.


SELECTION-SCREEN:BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  SELECT-OPTIONS:
            s_ebeln   FOR ekko-ebeln MODIF ID po,
            s_bsart   FOR ekko-bsart MODIF ID po,
            s_ekorg   FOR ekko-ekorg MODIF ID po," MEMORY ID ekg,
            s_ekgrp   FOR ekko-ekgrp MODIF ID po," MEMORY ID ekg,
            s_lifnr   FOR ekko-lifnr MODIF ID po,
            s_eeind FOR zafo_head-eeind MODIF ID po.
SELECTION-SCREEN:END OF  BLOCK b4.


SELECTION-SCREEN:BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS:
            s_matnr   FOR mara-matnr MODIF ID mat,
            s_maktx   FOR makt-maktx MODIF ID mat,
            s_matkl   FOR mara-matkl MODIF ID mat,
            s_satnr   FOR mara-satnr MODIF ID mat,
            s_mtart   FOR mara-mtart MODIF ID mat.
SELECTION-SCREEN:END OF  BLOCK b5.


SELECTION-SCREEN:BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
  SELECT-OPTIONS:
            s_aufnr   FOR zafo_item-aufnr MODIF ID auf.
SELECTION-SCREEN:END OF  BLOCK b6.


SELECTION-SCREEN:BEGIN OF BLOCK b7 WITH FRAME TITLE TEXT-007.
  SELECT-OPTIONS:
            s_afono   FOR zafo_head-afono MODIF ID dis,
            s_status   FOR zafo_head-status MODIF ID dis,
            s_ernam   FOR zafo_head-ernam MODIF ID dis,
            s_erdat   FOR zafo_head-erdat MODIF ID dis,
            s_budat   FOR zafo_head-budat MODIF ID dis.
SELECTION-SCREEN:END OF  BLOCK b7.

SELECTION-SCREEN BEGIN OF BLOCK b30 WITH FRAME TITLE TEXT-008.
  SELECTION-SCREEN PUSHBUTTON 2(12)  b_down  USER-COMMAND down MODIF ID lod .
SELECTION-SCREEN END OF BLOCK b30.

SELECTION-SCREEN BEGIN OF BLOCK b40 WITH FRAME TITLE TEXT-009.
  PARAMETERS:p_file LIKE rlgrap-filename OBLIGATORY DEFAULT 'D:\' MODIF ID lod MEMORY ID zfile_path.  "导入文件的路径
  PARAMETERS:p_row TYPE i OBLIGATORY DEFAULT '1' MODIF ID lod.
SELECTION-SCREEN END OF BLOCK b40.


FORM frm_vendor_search.
  READ TABLE s_lifnr ASSIGNING FIELD-SYMBOL(<f_lifnr>) INDEX 1.
  CHECK sy-subrc EQ 0.
  zwft_common=>search_vendor( CHANGING lifnr = <f_lifnr>-low ).
  DELETE s_lifnr WHERE low = '' AND high = ''.
ENDFORM.

FORM frm_customer_search.
  READ TABLE s_kunnr ASSIGNING FIELD-SYMBOL(<f_kunnr>) INDEX 1.
  CHECK sy-subrc EQ 0.
  zwft_common=>search_customer( CHANGING kunnr = <f_kunnr>-low ).
  DELETE s_kunnr WHERE low = '' AND high = ''.
ENDFORM.

FORM frm_material_search.
  READ TABLE s_matnr ASSIGNING FIELD-SYMBOL(<f_matnr>) INDEX 1.
  CHECK sy-subrc EQ 0.
  zwft_common=>search_material( EXPORTING werks = p_werks CHANGING matnr = <f_matnr>-low ).
  DELETE s_matnr WHERE low = '' AND high = ''.
ENDFORM.

FORM frm_clear_select.
  CLEAR: s_lgort[],
         s_charg[],
         s_auart[],
         s_kunnr[],
         s_eeind[],
         s_bsart[],
         s_ekgrp[],
         s_ebeln[],
         s_lifnr[],
         s_matnr[],
         s_maktx[],
         s_mtart[],
         s_aufnr[],
         s_afono[],
         s_status[],
         s_ernam[],
         s_erdat[],
         s_vbeln[],
         s_budat[].

ENDFORM.
