*&---------------------------------------------------------------------*
*& 包含               ZAFO_SEL
*&---------------------------------------------------------------------*



    SELECTION-SCREEN:BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-010.
      PARAMETERS: p_cre  TYPE c RADIOBUTTON GROUP g2 USER-COMMAND con, "创建
                  p_ref  TYPE c RADIOBUTTON GROUP g2, "参考创建
                  p_dis  TYPE c RADIOBUTTON GROUP g2, "查询
                  p_load TYPE c RADIOBUTTON GROUP g2, "上载
                  p_mod  TYPE c RADIOBUTTON GROUP g2. "修改
      PARAMETERS: p_fin TYPE c AS CHECKBOX.
    SELECTION-SCREEN:END OF  BLOCK b0.


    SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.   .
      PARAMETERS: p_typ   LIKE zafo_bustype-bustyp AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND con,
                  p_afono LIKE zafo_head-afono MODIF ID mod MEMORY ID zafono. "
      SELECT-OPTIONS: s_werks FOR t001l-werks NO INTERVALS MEMORY ID wrk.
      PARAMETERS:
                  p_werks   TYPE t001l-werks MEMORY ID wrk .
    SELECTION-SCREEN:END OF  BLOCK b1.


    SELECTION-SCREEN:BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
      SELECT-OPTIONS:
                  s_lgort   FOR t001l-lgort MODIF ID mb MEMORY ID lag,
                  s_charg   FOR mchb-charg MODIF ID mb.
    SELECTION-SCREEN:END OF  BLOCK b2.

    SELECTION-SCREEN:BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
      SELECT-OPTIONS:
                s_vbeln FOR zafo_head-kdauf MODIF ID so,
                s_auart FOR zafo_head-auart MODIF ID so,
                s_vkorg FOR zafo_head-vkorg MODIF ID so,
                s_kunnr FOR zafo_head-kunnr MODIF ID so,
                s_kdmat FOR zafo_item-kdmat MODIF ID so,
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
                s_satnr   FOR mara-satnr MODIF ID mat,
                s_matnr   FOR mara-matnr MODIF ID mat,
                s_maktx   FOR makt-maktx MODIF ID mat,
                s_matkl   FOR mara-matkl MODIF ID mat,
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



    SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
      SELECTION-SCREEN PUSHBUTTON 2(10) risubmit USER-COMMAND &submit.
      SELECTION-SCREEN INCLUDE BLOCKS: b1,b2,b3,b4,b5,b6.
    SELECTION-SCREEN END OF SCREEN 1100.



    AT SELECTION-SCREEN ON s_lifnr.
      PERFORM frm_vendor_search USING ''.

    AT SELECTION-SCREEN ON s_kunnr.
      PERFORM frm_customer_search USING ''.

    AT SELECTION-SCREEN ON s_matnr.
      PERFORM frm_material_search USING ''.

    AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_kunnr-low.
      PERFORM frm_customer_search USING 'F4'.

    AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lifnr-low.
      PERFORM frm_vendor_search USING 'F4'.

    AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_matnr-low.
      PERFORM frm_material_search USING 'F4'.



    FORM frm_vendor_search USING act.
      DATA l_werks TYPE werks_d.
      IF act = 'F4'.
        CLEAR s_lifnr[].
        INSERT INITIAL LINE INTO s_lifnr INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-sign = 'I'.
        <line>-option = 'EQ'.
        <line>-low = '%'.
      ELSE.
        READ TABLE s_lifnr ASSIGNING <line> INDEX 1.
        CHECK sy-subrc EQ 0.
        FIND '*' IN <line>-low.
        CHECK sy-subrc NE 0.
      ENDIF.
      PERFORM frm_get_werks CHANGING l_werks.
      zwft_common=>search_vendor( EXPORTING werks = l_werks CHANGING lifnr = <line>-low ).
      PERFORM frm_set_dynp_value USING 'S_LIFNR-LOW' <line>-low.
      DELETE s_lifnr WHERE low = '' AND high = ''.
    ENDFORM.

    FORM frm_customer_search USING act.
      DATA l_werks TYPE werks_d.
      IF act = 'F4'.
        CLEAR s_kunnr[].
        INSERT INITIAL LINE INTO s_kunnr INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-sign = 'I'.
        <line>-option = 'EQ'.
        <line>-low = '%'.
      ELSE.
        READ TABLE s_kunnr ASSIGNING <line> INDEX 1.
        CHECK sy-subrc EQ 0.
        FIND '*' IN <line>-low.
        CHECK sy-subrc NE 0.
      ENDIF.
      PERFORM frm_get_werks CHANGING l_werks.
      zwft_common=>search_customer( EXPORTING werks = l_werks CHANGING kunnr = <line>-low ).
      PERFORM frm_set_dynp_value USING 'S_KUNNR-LOW' <line>-low.
      DELETE s_kunnr WHERE low = '' AND high = ''.
    ENDFORM.

    FORM frm_material_search USING act.
      DATA l_werks TYPE werks_d.
      IF act = 'F4'.
        CLEAR s_matnr.
        INSERT INITIAL LINE INTO s_matnr INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
        <line>-sign = 'I'.
        <line>-option = 'EQ'.
        <line>-low = '%'.
      ELSE.
        READ TABLE s_matnr ASSIGNING <line> INDEX 1.
        CHECK sy-subrc EQ 0.
        FIND '*' IN <line>-low.
        CHECK sy-subrc NE 0.
      ENDIF.
      PERFORM frm_get_werks CHANGING l_werks.
      zwft_common=>search_material( EXPORTING werks = l_werks CHANGING matnr = <line>-low ).
      PERFORM frm_set_dynp_value USING 'S_MATNR-LOW' <line>-low.
      DELETE s_matnr WHERE low = '' AND high = ''.
    ENDFORM.

    FORM frm_get_werks CHANGING werks.
      IF p_werks IS INITIAL AND s_werks[] IS NOT INITIAL.
        werks = s_werks[ 1 ]-low.
      ELSEIF p_werks IS NOT INITIAL.
        werks = p_werks.
      ELSE.
        CALL FUNCTION 'GET_DYNP_VALUE'
          EXPORTING
            i_field = 'S_WERKS-LOW'
            i_repid = sy-repid
            i_dynnr = sy-dynnr
          CHANGING
            o_value = werks.
      ENDIF.
    ENDFORM.

    FORM frm_clear_select.
      zwft_common=>get_select_dynnr_field( EXPORTING program = sy-repid
                                                                                          dynnr = '1000'
                                                                    IMPORTING  global_sscr = DATA(lt_rsscr) ).
      LOOP AT lt_rsscr INTO DATA(ls_rsscr) WHERE kind = 'S'.
        CHECK ls_rsscr-name <> 'S_WERKS'.
        DATA(tabname) = ls_rsscr-name && '[]'.
        ASSIGN (tabname) TO FIELD-SYMBOL(<table>).
        CHECK sy-subrc EQ 0.
        CLEAR <table>.
      ENDLOOP.
    ENDFORM.

    FORM frm_set_dynp_value USING i_field
          i_value .
      DATA field TYPE dynpread-fieldname.
      DATA value TYPE dynpread-fieldvalue.
      field = i_field.
      value = |{ i_value ALPHA = OUT }|.
      CALL FUNCTION 'SET_DYNP_VALUE'
        EXPORTING
          i_field = field
          i_repid = sy-repid
          i_dynnr = sy-dynnr
          i_value = value.
    ENDFORM.
