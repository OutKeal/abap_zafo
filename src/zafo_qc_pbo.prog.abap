*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_PBO
*&---------------------------------------------------------------------*

FORM frm_sel_ini.

  CASE sy-tcode.
    WHEN 'ZAFO_QC'.
      gv_qcmode = 'A'.

      IF s_shdate[] IS INITIAL.
        s_shdate-sign = 'I'.
        s_shdate-option = 'BT'.
        s_shdate-low = sy-datum - 90.
        s_shdate-high = sy-datum.
        APPEND s_shdate.
      ENDIF.

    WHEN 'ZAFO_QC_R'.
      gv_qcmode = 'B'.

      IF s_rkdate[] IS INITIAL.
        s_rkdate-sign = 'I'.
        s_rkdate-option = 'BT'.
        s_rkdate-low = sy-datum - 90.
        s_rkdate-high = sy-datum.
        APPEND s_rkdate.
      ENDIF.

    WHEN 'ZAFO_QC_W'.
      gv_qcmode = 'C'.

      IF s_podate[] IS INITIAL.
        s_podate-sign = 'I'.
        s_podate-option = 'BT'.
        s_podate-low = sy-datum - 90.
        s_podate-high = sy-datum.
        APPEND s_podate.
      ENDIF.


    WHEN OTHERS.
      gv_qcmode = 'Q'.
  ENDCASE.


  IF s_status[] IS INITIAL.
    s_status-sign = 'I'.
    s_status-option = 'BT'.
    s_status-low = 'A'.
    s_status-high = 'C'.
    APPEND s_status.
  ENDIF.

  IF s_erdat[] IS INITIAL.
    s_erdat-sign = 'I'.
    s_erdat-option = 'BT'.
    s_erdat-low = sy-datum - 30.
    s_erdat-high = sy-datum.
    APPEND s_erdat.
  ENDIF.

ENDFORM.


FORM frm_set_sel_screen.
  LOOP AT SCREEN.

    IF screen-group1 = 'DIS'.
      IF p_dis = 'X' .
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.


    CASE gv_qcmode.
      WHEN 'A'.
        IF screen-group1 = 'B' OR screen-group1 = 'C'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'B'.
        IF screen-group1 = 'A' OR screen-group1 = 'C'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'C'.
        IF screen-group1 = 'A' OR screen-group1 = 'B' OR screen-group1 = 'AB'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.


  ENDLOOP.

ENDFORM.


MODULE status_100 OUTPUT.
  DATA:lt_exec TYPE TABLE OF sy-ucomm.

  REFRESH lt_exec.

  CASE gv_qcmode.
    WHEN 'A'.
      APPEND '&OKAY' TO lt_exec.
    WHEN 'B'.
      APPEND '&OKAY' TO lt_exec.
    WHEN 'C'.
      APPEND '&SURE' TO lt_exec.
      APPEND '&ALLSURE' TO lt_exec.
      APPEND '&ALLUNSURE' TO lt_exec.
  ENDCASE.

  SET PF-STATUS 'S100' EXCLUDING lt_exec.
ENDMODULE.


MODULE status_0103 OUTPUT.
  SET PF-STATUS 'S103'.

  SET TITLEBAR 'TITEL'  WITH <gs_head>-qcno.
ENDMODULE.


MODULE status_0101 OUTPUT.
  SET PF-STATUS 'S101'.
ENDMODULE.


MODULE set_list_0101 OUTPUT.
  DATA: lt_list  TYPE  vrm_values,
        ls_value TYPE vrm_value.
  DATA: lt_list_obj  TYPE  vrm_values,
        ls_value_obj TYPE vrm_value.


  CLEAR lt_list.
  CLEAR ls_value.

  SELECT * FROM zafo_qc_model_h
    INTO TABLE @DATA(lt_model_h).

  SORT lt_model_h BY qcmodel.

  LOOP AT lt_model_h INTO DATA(ls_model_h).

    ls_value-key =  ls_model_h-qcmodel.     "这个就是变量P_LIST的值
    ls_value-text = ls_model_h-name1.    "这个是TEXT
    APPEND ls_value TO lt_list .
  ENDLOOP.

*---〉调用函数显示LISTBOX里面的值
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_HEAD-QCMODEL'
      values = lt_list.
  IF sy-subrc EQ 0.
    MODIFY SCREEN.
  ENDIF.

ENDMODULE.


MODULE status_0300 OUTPUT.
  DATA lt_excu TYPE TABLE OF sy-ucomm.
  CLEAR lt_excu[].

  CASE <gs_item>-qc_status.
    WHEN 'A'.
      APPEND 'PRINT' TO lt_excu.
      APPEND 'SAVE_AS' TO lt_excu.
      APPEND 'RELEASE' TO lt_excu.
      APPEND 'ALLOW' TO lt_excu.
    WHEN 'B'.
    WHEN 'C'.
      APPEND 'SAVE' TO lt_excu.
    WHEN 'D'.
      APPEND 'SAVE' TO lt_excu.
      APPEND 'PRINT' TO lt_excu.
      APPEND 'SAVE_AS' TO lt_excu.
      APPEND 'ALLOW' TO lt_excu.
    WHEN 'E'.
      APPEND 'SAVE' TO lt_excu.
    WHEN 'F'.
      APPEND 'SAVE' TO lt_excu.
      APPEND 'PRINT' TO lt_excu.
      APPEND 'SAVE_AS' TO lt_excu.
      APPEND 'RELEASE' TO lt_excu.
      APPEND 'ALLOW' TO lt_excu.

  ENDCASE.
  SET PF-STATUS 'S300' EXCLUDING lt_excu.
ENDMODULE.


MODULE status_0400 OUTPUT.
  CLEAR lt_excu[].

  IF g_change = 'M'.
    APPEND 'EDIT' TO lt_excu.
    APPEND 'PRINT' TO lt_excu.
    APPEND '&APPR' TO lt_excu.
  ENDIF.

  IF g_change = 'D'.
    APPEND 'SAVE' TO lt_excu.
  ENDIF.
  SET PF-STATUS 'S400' EXCLUDING lt_excu.
ENDMODULE.


MODULE status_0500 OUTPUT.
  CLEAR lt_excu[].

  IF g_change = 'M'.
    APPEND 'EDIT' TO lt_excu.
    APPEND 'PRINT' TO lt_excu.
    APPEND '&APPR' TO lt_excu.
  ENDIF.

  IF g_change = 'D'.
    APPEND 'SAVE' TO lt_excu.
  ENDIF.
  SET PF-STATUS 'S400' EXCLUDING lt_excu.

ENDMODULE.


MODULE g_menge_qc_f INPUT.
  IF g_menge_qc_f1 <> g_menge_qc_f.
    g_menge_qc_f1 = g_menge_qc_f.
    g_menge_qc = gs_head-menge_gr - gs_head-menge_qc - gs_head-menge_qc_f - g_menge_qc_f.
  ENDIF.

  IF g_menge_qc < 0 .
    g_menge_qc = 0.
    g_menge_qc_f = 0.
  ENDIF.
ENDMODULE.


MODULE set_screen_0400 OUTPUT.

  PERFORM frm_set_text.

  LOOP AT SCREEN.
    CASE gv_qcmode.
      WHEN 'A' OR 'B'.
        IF screen-name EQ '<GS_ITEM>-MENGE_DS'.
          screen-input = 0.
          screen-required = '0'.
        ENDIF.
      WHEN 'C'.
        IF screen-name EQ '<GS_ITEM>-MENGE_DS'.
          screen-input = 1.
          screen-required = '2'.
        ENDIF.
    ENDCASE.

    IF g_change = 'D'.
      screen-input = 0.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.


MODULE set_screen_0500 OUTPUT.

  PERFORM frm_set_text.

  LOOP AT SCREEN.
    CASE gv_qcmode.
      WHEN 'A' OR 'B'.
        IF screen-name EQ '<GS_ITEM>-MENGE_DS'.
          screen-input = 0.
          screen-required = '0'.
        ENDIF.
      WHEN 'C'.
        IF screen-name EQ '<GS_ITEM>-MENGE_DS'.
          screen-input = 1.
          screen-required = '2'.
        ENDIF.
    ENDCASE.

    IF g_change = 'D'.
      screen-input = 0.
    ENDIF.

    IF screen-name = ''.
      screen-input = 1.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.


FORM frm_set_text.

  DATA: lt_comp TYPE abap_component_tab,
        ls_comp LIKE LINE OF lt_comp,
        lr_type TYPE REF TO  cl_abap_typedescr.

  DATA: lcr_ref_line TYPE REF TO cl_abap_structdescr.
  DATA: lr_ref_line TYPE REF TO data.


  FIELD-SYMBOLS:<fs_value> TYPE any.

  ls_comp-name = 'AFONO'.
  ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'CHAR40' ).
  APPEND ls_comp TO lt_comp.
  CLEAR ls_comp.

  ls_comp-name = 'MENGE_GR'.
  ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'CHAR40' ).
  APPEND ls_comp TO lt_comp.
  CLEAR ls_comp.

  ls_comp-name = 'DATUM_GR'.
  ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'CHAR40' ).
  APPEND ls_comp TO lt_comp.
  CLEAR ls_comp.

  lcr_ref_line ?= cl_abap_structdescr=>create( p_components = lt_comp ).

  CREATE DATA lr_ref_line TYPE HANDLE lcr_ref_line.

  ASSIGN lr_ref_line->* TO <gs_item_text>.

  ASSIGN COMPONENT 'AFONO' OF STRUCTURE <gs_item_text> TO <fs_value>.
  IF sy-subrc EQ 0.
    CASE gv_qcmode.
      WHEN 'A'.
        <fs_value> = '收货单号'.
      WHEN 'B'.
        <fs_value> = '入库单号'.
      WHEN 'C'.
        <fs_value> = '采购单号'.
    ENDCASE.
  ENDIF.

  ASSIGN COMPONENT 'MENGE_GR' OF STRUCTURE <gs_item_text> TO <fs_value>.
  IF sy-subrc EQ 0.
    CASE gv_qcmode.
      WHEN 'A'.
        <fs_value> = '收货数量'.
      WHEN 'B'.
        <fs_value> = '入库数量'.
      WHEN 'C'.
        <fs_value> = '采购数量'.
    ENDCASE.
  ENDIF.

  ASSIGN COMPONENT 'DATUM_GR' OF STRUCTURE <gs_item_text> TO <fs_value>.
  IF sy-subrc EQ 0.
    CASE gv_qcmode.
      WHEN 'A'.
        <fs_value> = '收货日期'.
      WHEN 'B'.
        <fs_value> = '入库日期'.
      WHEN 'C'.
        <fs_value> = '采购日期'.
    ENDCASE.
  ENDIF.

ENDFORM.


MODULE create_gos_service OUTPUT.
  DATA:obj TYPE borident.
  DATA:manager TYPE REF TO cl_gos_manager.

  CHECK <gs_item>-qcno IS NOT INITIAL .

  obj-objtype = 'ZAFO_QC'.

  obj-objkey = <gs_item>-qcno && <gs_item>-qcnr.

  IF manager IS INITIAL .

    CREATE OBJECT manager
      EXPORTING
        is_object    = obj
        ip_no_commit = 'R'
      EXCEPTIONS
        OTHERS       = 1.
  ENDIF.
  IF g_change = 'D'.
    CALL METHOD manager->set_rw_mode
      EXPORTING
        ip_mode = 'D'.
  ELSEIF g_change = 'M'.
    CALL METHOD manager->set_rw_mode
      EXPORTING
        ip_mode = 'E'.
  ENDIF.

ENDMODULE.


MODULE pull_force_dorp_list INPUT.

  DATA:BEGIN OF lt_pull_force OCCURS 0,
         pull_force TYPE char40,
       END OF lt_pull_force.
  IF lt_pull_force[] IS INITIAL.

    lt_pull_force-pull_force = '合格'.
    APPEND lt_pull_force.

    lt_pull_force-pull_force = '不合格'.
    APPEND lt_pull_force.

  ENDIF.

  CHECK lt_pull_force[] IS NOT  INITIAL.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = '<GS_ITEM>-PULL_FORCE'
    TABLES
      value_tab = lt_pull_force[].

ENDMODULE.


MODULE fastness_dorp_list INPUT.

  DATA:BEGIN OF lt_fastness OCCURS 0,
         fastness TYPE char40,
       END OF lt_fastness.
  IF lt_fastness[] IS INITIAL.

    lt_fastness-fastness = '合格'.
    APPEND lt_fastness.

    lt_fastness-fastness = '不合格'.
    APPEND lt_fastness.

  ENDIF.

  CHECK lt_fastness[] IS NOT  INITIAL.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = '<GS_ITEM>-PULL_FORCE'
    TABLES
      value_tab = lt_fastness[].

ENDMODULE.


MODULE shrinkage_dorp_list INPUT.

  DATA:BEGIN OF lt_shrinkage OCCURS 0,
         shrinkage TYPE char40,
       END OF lt_shrinkage.
  IF lt_shrinkage[] IS INITIAL.

    lt_shrinkage-shrinkage = '合格'.
    APPEND lt_shrinkage.

    lt_shrinkage-shrinkage = '不合格'.
    APPEND lt_shrinkage.

  ENDIF.

  CHECK lt_shrinkage[] IS NOT  INITIAL.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = '<GS_ITEM>-PULL_FORCE'
    TABLES
      value_tab = lt_shrinkage[].

ENDMODULE.


MODULE twist_dorp_list INPUT.

  DATA:BEGIN OF lt_twist OCCURS 0,
         twist TYPE char40,
       END OF lt_twist.
  IF lt_twist[] IS INITIAL.

    lt_twist-twist = '合格'.
    APPEND lt_twist.

    lt_twist-twist = '不合格'.
    APPEND lt_twist.

  ENDIF.

  CHECK lt_twist[] IS NOT  INITIAL.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = '<GS_ITEM>-PULL_FORCE'
    TABLES
      value_tab = lt_twist[].

ENDMODULE.
