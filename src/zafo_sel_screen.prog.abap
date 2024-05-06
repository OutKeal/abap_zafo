*&---------------------------------------------------------------------*
*& 包含               ZAFO_SEL
*&---------------------------------------------------------------------*
TABLES: mkpf,ekko,mara,ekpo,makt,eket,zafo_head,zafo_item,
        rmmg1,marc,mchb,afko,aufk,vbak,mska,vbap,t001w,t001l.
TABLES:  sscrfields.

DATA:gt_list  TYPE  vrm_values.
DATA:g_action TYPE zafo_action.
DATA:g_werks_flag TYPE char1.
DATA:gs_object TYPE zafo_object.
DATA:gs_bustyp TYPE zafo_bustype.
DATA:gv_create_model TYPE char1.
DATA:g_error TYPE char1.
DATA:gt_head TYPE TABLE OF zafo_shead  WITH HEADER LINE.
DATA:gt_item TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
DATA:g_bustyp TYPE zafo_bustyp.
DATA ref_in TYPE abap_bool.

CONSTANTS c_fin_flag TYPE char20 VALUE 'ADFHJKZ'.
SELECTION-SCREEN BEGIN OF SCREEN 1100.

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

SELECTION-SCREEN END OF SCREEN 1100.

SELECTION-SCREEN BEGIN OF SCREEN 1200 AS SUBSCREEN.
  SELECTION-SCREEN PUSHBUTTON 2(10) risubmit USER-COMMAND &submit.
  SELECTION-SCREEN INCLUDE BLOCKS: b1,b2,b3,b4,b5,b6.
SELECTION-SCREEN END OF SCREEN 1200.



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
    dynnr = '1100'
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



FORM frm_at_screen CHANGING p_ucomm.
  CASE p_ucomm.
    WHEN 'CRET'.
      PERFORM frm_run.
    WHEN 'DOWN'.
      PERFORM frm_download_temp .  "下载模板
    WHEN 'CON'.
      PERFORM frm_set_default_value .
    WHEN '&SUBMIT'.
      DATA(ls_bustyp) = zafo_basic=>get_bustyp( p_typ ).
      DATA lt_item TYPE zafo_tt_sitem.
      PERFORM frm_get_ref USING ls_bustyp-busref_in  CHANGING lt_item[] .
      PERFORM frm_ref_in_sure  USING lt_item[].

  ENDCASE.
ENDFORM.



FORM frm_set_sel_screen .

  CHECK p_typ IS NOT INITIAL .
  DATA:l_field TYPE char1.

  gs_bustyp = zafo_basic=>get_bustyp( p_typ ).

  CHECK gs_bustyp IS NOT INITIAL.

  gv_create_model = gs_bustyp-busref_control.

  IF gv_create_model = 1 AND p_ref = 'X'..
    p_cre = 'X'.
    p_ref = ''.
  ELSEIF gv_create_model = 2 AND p_cre = 'X'..
    p_cre = ''.
    p_ref = 'X'.
  ENDIF.
  DATA(lt_sel_screen) = zafo_basic=>get_bustyp_sel_screen( p_typ ).

  PERFORM frm_set_werks_flag.
  PERFORM frm_set_action.

  CHECK p_typ IS NOT INITIAL.

  LOOP AT SCREEN.
    LOOP AT lt_sel_screen INTO DATA(ls_sel_screen) WHERE fieldname IS NOT INITIAL AND action = ''  .
      IF screen-name CP '*' && ls_sel_screen-fieldname && '*'.
        IF ls_sel_screen-fieldname = 'P_CRE'.
          p_cre = ''.
          IF p_mod = ''.
            p_dis = 'X'.
          ENDIF.
        ENDIF.
        screen-active = '0'.
        l_field = 'X'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDLOOP.

  LOOP AT SCREEN.

    IF screen-group1 = 'MOD' AND p_mod IS INITIAL."
      screen-active = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF ref_in EQ abap_true.
      IF screen-name CP '*P_DIS*'
      OR screen-name CP '*P_TYP*'
      OR screen-name CP '*P_CRE*'
      OR screen-name CP '*P_REF*'
      OR screen-name CP '*S_WERKS*'.
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      IF screen-name CP '*P_WERKS*'.
        screen-active = 1.
        screen-invisible = 1.
        screen-input = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF screen-name CP '*P_CRE*'.

      IF gv_create_model = 1 OR gv_create_model = 3.
        screen-active = '1'.
        MODIFY SCREEN.
        CONTINUE.
      ELSE.
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDIF.

    IF screen-name CP '*P_REF*'.

      IF gv_create_model = 2 OR gv_create_model = 3.
        screen-active = '1'.
        MODIFY SCREEN.
        CONTINUE.
      ELSE.
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF screen-name CP '*P_WERKS*'.


      IF g_werks_flag = 'P' .
        screen-active = '1'.
        MODIFY SCREEN.
        CONTINUE.
      ELSE.
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      CONTINUE.
    ENDIF.

    IF screen-name CP '*S_WERKS*'.


      IF g_werks_flag = 'S' .
        screen-active = '1'.
        MODIFY SCREEN.
        CONTINUE.
      ELSE.
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      CONTINUE.
    ENDIF.

    IF screen-name CP '*P_MOD*' AND p_mod = ''.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
    CLEAR l_field.

    IF screen-name CP '*P_OBJ*' AND g_action NE 'DIS'  .
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    IF screen-name CP '*P_FIN*'   .
      IF NOT ( p_ref = 'X'  AND gs_bustyp-busref CA c_fin_flag ).
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      CONTINUE.
    ENDIF.

    IF screen-name CP '*P_LOAD*'   .
      READ TABLE lt_sel_screen INTO ls_sel_screen WITH KEY fieldname = 'P_LOAD'.
      IF sy-subrc EQ 0.
        IF screen-name = 'P_LOAD'.
          screen-input = 1.
        ENDIF.
        screen-active = '1'.
        screen-invisible = '0'.
        screen-output = '1'.
      ELSE.
        screen-active = '0'.
        screen-invisible = '1'.
        screen-output = '0'.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.


    LOOP AT lt_sel_screen INTO ls_sel_screen WHERE fieldname IS NOT INITIAL AND action = g_action  .
      IF screen-name CP '*' && ls_sel_screen-fieldname && '*'.
        screen-active = '1'.
        l_field = 'X'.
        IF ls_sel_screen-read_only IS NOT INITIAL.
          screen-input = ''.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    CHECK l_field IS INITIAL.

    READ TABLE lt_sel_screen INTO ls_sel_screen
    WITH KEY fieldgroup = screen-group1
    fieldname = ''
    action = g_action.
    IF sy-subrc EQ 0.
      screen-active = '1'.
    ELSE.
      screen-active = '0'.
    ENDIF.

    IF g_action = 'LOD' AND screen-group1 = 'CRE'.
      screen-active = '1'.
    ENDIF.

    IF screen-group1 = g_action OR screen-group1 = ''.
      screen-active = '1'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.


FORM frm_download_temp .
  CHECK p_typ IS NOT INITIAL.
  CALL FUNCTION 'ZAFO_DOWNLOAD_TEMPLATE'
    EXPORTING
      i_bustyp = p_typ
*         TABLES
*     ET_RETURN       =
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
  ENDIF.

ENDFORM.

FORM frm_set_default_value .
  DATA css_class TYPE i.
  DATA: tabname TYPE char30.
  DATA: ls_day(3) TYPE n.
  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any,
                 <txt_field> TYPE any.

  PERFORM frm_clear_select.

  IF p_typ IS INITIAL.
    READ TABLE gt_list INDEX 1 INTO DATA(l_list).
    p_typ = l_list-key.
  ENDIF.

  DATA(lt_value) = zafo_basic=>get_bustyp_sel_value( p_typ ) .
  CHECK lt_value IS NOT INITIAL.

  LOOP AT lt_value INTO DATA(it_value)
        GROUP BY ( fieldname = it_value-fieldname ).

    IF it_value-fieldname+0(1) = 'P'.
*       txt_field = it_value-fieldname.
      ASSIGN (it_value-fieldname) TO <txt_field>.
      IF sy-subrc EQ 0.
        <txt_field> = it_value-fieldvalue.
      ENDIF.
    ENDIF.

    IF it_value-fieldname+0(1) = 'S'.
      CONCATENATE it_value-fieldname '[]' INTO tabname.
      ASSIGN (tabname) TO <dyn_table>.
      CHECK sy-subrc EQ 0.
      CLEAR <dyn_table>.

      LOOP AT GROUP it_value INTO DATA(ls_value).
        APPEND INITIAL LINE TO <dyn_table> ASSIGNING <dyn_wa>.

        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <dyn_wa> TO <txt_field>.
        CHECK sy-subrc EQ 0.
        <txt_field> = 'I'.

        ASSIGN COMPONENT 'OPTION' OF STRUCTURE <dyn_wa> TO <txt_field>.
        CHECK sy-subrc EQ 0.
        <txt_field> = ls_value-fieldoption.

        ASSIGN COMPONENT 'LOW' OF STRUCTURE <dyn_wa> TO <txt_field>.
        CHECK sy-subrc EQ 0.
        IF ls_value-fieldvalue = 'UNAME'.
          <txt_field> = sy-uname.
        ELSEIF ls_value-fieldvalue+0(3) = 'DAY'.
          ls_day = ls_value-fieldvalue+3(3).
          <txt_field> = sy-datum - ls_day.
          ASSIGN COMPONENT 'HIGH' OF STRUCTURE <dyn_wa> TO <txt_field>.
          CHECK sy-subrc EQ 0.
          <txt_field> = sy-datum.
        ELSE.
          <txt_field> = ls_value-fieldvalue.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.


FORM frm_set_action.
  CASE 'X'.
    WHEN ref_in.
      g_action = 'REF_IN'.
    WHEN p_ref.
      g_action = 'REF'.
    WHEN p_cre.
      g_action = 'CRE'.
    WHEN p_mod.
      g_action = 'MOD'.
    WHEN p_dis.
      g_action = 'DIS'.
    WHEN p_load.
      g_action = 'LOD'.
  ENDCASE.

ENDFORM.



FORM frm_set_werks_flag.
  CASE 'X'.
    WHEN p_cre OR p_mod OR p_load.
      g_werks_flag = 'P'.
    WHEN p_dis OR p_ref.
      g_werks_flag = 'S'.
  ENDCASE.
ENDFORM.



FORM frm_init.
  CHECK b_down IS INITIAL.
  SELECT SINGLE ttext INTO sy-title FROM tstct WHERE sprsl = sy-langu AND tcode = sy-tcode.
  b_down = TEXT-013.
  risubmit = TEXT-012.
  IF sy-calld = 'X' OR sy-oncom = 'S'.
*     p_mod = 'X'.
    GET PARAMETER ID 'ZBUSTYP' FIELD p_typ.
  ENDIF.
  zwft_common=>set_init_sdate( EXPORTING days = '90' CHANGING sdate = s_erdat[] ).
  APPEND VALUE #( sign = 'I'
  option = 'NE'
  low = 'D'  ) TO s_status.
  PERFORM frm_set_list.
  PERFORM frm_set_default_value.
ENDFORM.

FORM frm_set_list.

  DATA(lt_bustyp) = zafo_basic=>get_bustyp_by_tcode( sy-tcode ).
  SORT lt_bustyp BY bustyp.
  READ TABLE lt_bustyp INTO DATA(ls_bustyp) INDEX 1.
  IF sy-subrc EQ 0 AND p_typ IS INITIAL.
    p_typ = ls_bustyp-bustyp.
    gv_create_model = ls_bustyp-busref_control.
    IF gv_create_model = 2 AND p_mod = ''..
      p_cre = ''.
      p_ref = 'X'.
    ELSE.
      p_cre = 'X'.
      p_ref = ''.
    ENDIF.
  ENDIF.


  gt_list = CORRESPONDING #( lt_bustyp  MAPPING key = bustyp
  text =  bustyp_name1 ).
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_TYP'
      values = gt_list.
  IF sy-subrc EQ 0.
    MODIFY SCREEN.
  ENDIF.
ENDFORM.

FORM frm_set_ref_in.
  IF sy-dynnr = '1200'.
    ref_in = abap_true.
    PERFORM frm_set_default_value .
  ELSE.
    ref_in = abap_false.
  ENDIF.

ENDFORM.
