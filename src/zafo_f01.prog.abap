*&---------------------------------------------------------------------*
*& 包含               ZAFO_F01
*&---------------------------------------------------------------------*

FORM frm_set_sel_screen .

  CHECK p_typ IS NOT INITIAL .
  DATA:l_field TYPE char1.

  DATA:lv_level TYPE char1.

  DATA:lt_sel_screen TYPE TABLE OF zafo_sel_screen WITH HEADER LINE.
  DATA:lt_sel_scr_bty TYPE TABLE OF zafo_sel_scr_bty WITH HEADER LINE.
  DATA:lt_sel_value TYPE TABLE OF zafo_sel_value WITH HEADER LINE.

  READ TABLE gt_bustyp INTO gs_bustyp WITH KEY bustyp = p_typ.
  READ TABLE gt_object INTO gs_object WITH KEY object = gs_bustyp-object.

  lt_sel_screen[] = gt_sel_screen[].
  lt_sel_scr_bty[] = gt_sel_scr_bty[].

  DELETE lt_sel_screen WHERE object <> gs_object-object.
  DELETE lt_sel_scr_bty WHERE bustyp <> gs_bustyp-bustyp.

  IF lt_sel_scr_bty[] IS INITIAL.
    LOOP AT lt_sel_screen.
      MOVE-CORRESPONDING lt_sel_screen TO lt_sel_scr_bty.
      lt_sel_scr_bty-bustyp = gs_bustyp-bustyp.
      APPEND lt_sel_scr_bty.
    ENDLOOP.
  ENDIF.

  IF p_mod = 'X' .
    g_werks_flag = 'S'.
  ELSEIF p_load = 'X'.
    g_werks_flag = 'P'.
  ELSEIF p_dis = 'X'.
    g_werks_flag = 'S'.
  ELSE.
    IF gs_bustyp-busref IS INITIAL.
      g_werks_flag = 'P'.
    ELSE.
      FIND gs_bustyp-busref IN 'ADEFGHIJLNPYZS'.
      IF sy-subrc EQ 0.
        g_werks_flag = 'S'.
      ELSE.
        g_werks_flag = 'P'.
      ENDIF.
    ENDIF.

  ENDIF.

  PERFORM frm_set_action.

  LOOP AT SCREEN.
    LOOP AT lt_sel_scr_bty WHERE fieldname IS NOT INITIAL AND action = ''  .
      IF screen-name CP '*' && lt_sel_scr_bty-fieldname && '*'.
        IF lt_sel_scr_bty-fieldname = 'P_CRE'.
          p_cre = ''.
          IF p_mod = ''.
            p_dis = 'X'.
          ENDIF.
          PERFORM frm_set_action.
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

    IF screen-name CP '*P_WERKS*'.
      IF g_werks_flag = 'P' .
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name CP '*S_WERKS*'.
      IF g_werks_flag = 'S' .
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
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
      IF p_cre = 'X' AND gs_bustyp-busref IS NOT INITIAL.
      ELSE.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      CONTINUE.
    ENDIF.

    IF screen-name CP '*P_LOAD*'   .
      READ TABLE lt_sel_scr_bty WITH KEY fieldname = 'P_LOAD'.
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

*20220104 at-yuxs 只看自己选择按钮处理 begin
    IF screen-name CS 'P_SELF'.
      IF  p_dis = 'X' AND ( gs_object-object = 'COST' OR gs_object-object = 'COST02').
        AUTHORITY-CHECK OBJECT 'ZFI_001' ID 'ACTVT' DUMMY.
        IF sy-subrc = 0.
          p_self = ''.
          screen-input = '1' .
        ELSE.
          p_self = 'X'.
          screen-input = '0' .
        ENDIF.
      ELSE.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
*20220104 at-yuxs 只看自己选择按钮处理 end

    IF screen-name CS 'P_SCAN'." 扫码创建
      IF  p_cre = 'X' AND (
          gs_object-object = 'R1006' OR
          gs_object-object = 'R6005' OR
          gs_object-object = 'R4011' ).
        screen-active = '1'.
      ELSE.
        p_scan = ''.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
      CONTINUE.
    ENDIF.


    LOOP AT lt_sel_scr_bty WHERE fieldname IS NOT INITIAL AND action = g_action  .
      IF screen-name CP '*' && lt_sel_scr_bty-fieldname && '*'.
        screen-active = '1'.
        l_field = 'X'.
        IF lt_sel_scr_bty-read_only IS NOT INITIAL.
          screen-input = ''.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    CHECK l_field IS INITIAL.

    READ TABLE lt_sel_scr_bty WITH KEY fieldgroup = screen-group1
                                       fieldname = ''
                                       action = g_action.
    IF sy-subrc EQ 0.
      screen-active = '1'.
      CONTINUE.
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


FORM frm_clear_sel_value USING fieldname .

  DATA: tabname TYPE char20.
  FIELD-SYMBOLS: <dyn_table> TYPE table.
  tabname = fieldname && '[]'.
  ASSIGN (tabname) TO <dyn_table>.
  CHECK sy-subrc EQ 0.
  CLEAR <dyn_table>.

ENDFORM.


FORM frm_set_default_value .
  DATA: tabname TYPE char20.
  DATA: strname TYPE char20.
  DATA: fieldname TYPE fieldname.
  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any,
                 <txt_field> TYPE any,
                 <val_field> TYPE any.
*  DATA: DYN_TABLE TYPE CHAR30.
  DATA: dyn_table TYPE REF TO data.
  DATA: dyn_wa TYPE REF TO data.
  DATA: val_field TYPE char40.
  DATA: txt_field TYPE char40.

  DATA:lt_value TYPE TABLE OF zafo_sel_value WITH HEADER LINE.
  DATA:it_value TYPE TABLE OF zafo_sel_value WITH HEADER LINE.
  DATA:lt_sel_screen TYPE TABLE OF zafo_sel_screen WITH HEADER LINE.

  lt_value[] = gt_sel_value[].
  DELETE lt_value WHERE bustyp <> p_typ.

  CHECK lt_value[] IS NOT INITIAL.

  it_value[] = lt_value[].
  DELETE ADJACENT DUPLICATES FROM it_value COMPARING fieldname.

  CASE 'X'.
    WHEN p_cre.
      REFRESH s_ernam.
    WHEN p_mod.

    WHEN p_dis.
  ENDCASE.

  LOOP AT it_value .
    IF it_value-fieldname+0(1) = 'P'.
      txt_field = it_value-fieldname.
      ASSIGN (txt_field) TO <txt_field>.
      IF <txt_field> IS ASSIGNED.
        <txt_field> = it_value-fieldvalue.
      ENDIF.
    ENDIF.

    IF it_value-fieldname+0(1) = 'S'.
      strname = it_value-fieldname.
      CONCATENATE strname '[]' INTO tabname.
      IF <dyn_wa> IS ASSIGNED.
        UNASSIGN <dyn_wa>.
      ENDIF.
      IF <dyn_table> IS ASSIGNED.
        UNASSIGN <dyn_table>.
      ENDIF.

      ASSIGN (tabname) TO <dyn_table>.
      ASSIGN (strname) TO <dyn_wa>.

      CHECK <dyn_table> IS ASSIGNED.
      CLEAR <dyn_table>.

      IF <dyn_table> IS ASSIGNED AND <dyn_wa> IS ASSIGNED.
        CLEAR <dyn_table>.
        LOOP AT lt_value WHERE fieldname = it_value-fieldname.

          CONCATENATE strname '-SIGN' INTO val_field.
          ASSIGN (val_field) TO <txt_field>.
          IF <txt_field> IS ASSIGNED.
            <txt_field> = 'I'.
            UNASSIGN <txt_field>.
          ENDIF.

          CONCATENATE strname '-OPTION' INTO val_field.
          ASSIGN (val_field) TO <txt_field>.
          IF <txt_field> IS ASSIGNED.
            <txt_field> = lt_value-fieldoption.
            UNASSIGN <txt_field>.
          ENDIF.

          CONCATENATE strname '-LOW' INTO val_field.
          ASSIGN (val_field) TO <txt_field>.
          IF <txt_field> IS ASSIGNED.
            IF lt_value-fieldvalue = 'UNAME'.
              <txt_field> = sy-uname.
            ELSE.
              <txt_field> = lt_value-fieldvalue.
            ENDIF.

            UNASSIGN <txt_field>.
          ENDIF.

          APPEND <dyn_wa> TO <dyn_table>.
        ENDLOOP.

      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.


FORM frm_set_action.
  CASE 'X'.
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

FORM frm_init.
  GET PARAMETER ID 'ZAPP_JUMP' FIELD gv_jump.
  SET PARAMETER ID 'ZAPP_JUMP' FIELD abap_false.
  IF sy-calld = 'X' OR sy-oncom = 'S' OR gv_jump = abap_true.
    p_mod = 'X'.
    GET PARAMETER ID 'ZBUSTYP' FIELD p_typ.
  ELSE.
    p_cre = 'X'.
  ENDIF.

  b_down = TEXT-030.
  IF s_erdat[] IS INITIAL.
    s_erdat-sign = 'I'.
    s_erdat-option = 'BT'.
    s_erdat-low = sy-datum - 90.
    s_erdat-high = sy-datum.
    APPEND s_erdat.
  ENDIF.

  IF s_status[] IS INITIAL.
    s_status-sign = 'I'.
    s_status-option = 'NE'.
    s_status-low = 'D'.
*     s_status-high = sy-datum.
    APPEND s_status.
  ENDIF.
ENDFORM.


FORM frm_set_list.
  DATA: lt_list  TYPE  vrm_values,
        ls_value TYPE vrm_value.
  DATA: lt_list_obj  TYPE  vrm_values,
        ls_value_obj TYPE vrm_value.

  CLEAR lt_list.
  CLEAR ls_value.

  IF gt_bustyp[] IS INITIAL.
    SELECT * INTO TABLE gt_bustyp
      FROM zafo_bustype
      WHERE tcode = sy-tcode .
    IF sy-subrc EQ 0.
      SELECT * INTO TABLE gt_object
        FROM zafo_object
        FOR ALL ENTRIES IN gt_bustyp
        WHERE object = gt_bustyp-object.
    ENDIF.
    IF gt_object[] IS NOT INITIAL.
      SELECT * INTO TABLE gt_sel_screen
        FROM zafo_sel_screen
        FOR ALL ENTRIES IN gt_object
        WHERE object = gt_object-object.
      SELECT * INTO TABLE gt_sel_scr_bty
        FROM zafo_sel_scr_bty
        FOR ALL ENTRIES IN gt_bustyp
        WHERE bustyp = gt_bustyp-bustyp.
    ENDIF.

    IF gt_bustyp[] IS NOT INITIAL.
      SELECT * INTO TABLE gt_sel_value
        FROM zafo_sel_value
        FOR ALL ENTRIES IN gt_bustyp
        WHERE bustyp = gt_bustyp-bustyp.
    ENDIF.

  ENDIF.

  SORT gt_bustyp BY bustyp.

  LOOP AT gt_bustyp INTO gs_bustyp.
    IF sy-tabix EQ 1 AND p_typ IS INITIAL.
      p_typ = gs_bustyp-bustyp.
    ENDIF.
    ls_value-key =  gs_bustyp-bustyp.     "这个就是变量P_LIST的值
    ls_value-text = gs_bustyp-bustyp_name1.    "这个是TEXT
    APPEND ls_value TO lt_list .
  ENDLOOP.

*   LOOP AT gt_object INTO gs_object.
*
*     ls_value_obj-key =  gs_object-object.     "这个就是变量P_LIST的值
*     ls_value_obj-text = gs_object-object_name1.    "这个是TEXT
*     APPEND ls_value_obj TO lt_list_obj .
*
*   ENDLOOP.

*---〉调用函数显示LISTBOX里面的值
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_TYP'
      values = lt_list.
  IF sy-subrc EQ 0.
    MODIFY SCREEN.
  ENDIF.

*   CALL FUNCTION 'VRM_SET_VALUES'
*     EXPORTING
*       id     = 'P_OBJ'
*       values = lt_list_obj.
  IF sy-subrc EQ 0.
    MODIFY SCREEN.
  ENDIF.

ENDFORM.


FORM frm_run.

  IF g_werks_flag = 'P'.

    IF p_werks IS INITIAL.
      MESSAGE s005 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ENDIF.

  READ TABLE gt_bustyp INTO gs_bustyp WITH KEY bustyp = p_typ.
  READ TABLE gt_object INTO gs_object WITH KEY object = gs_bustyp-object.
  g_bustyp = gs_bustyp-bustyp.

*   PERFORM frm_tran_selection.

  CASE 'X'.
    WHEN p_cre.

      IF g_werks_flag = 'P'.

        " 创建的时候工厂必须正确
        SELECT SINGLE werks INTO @p_werks FROM t001w WHERE werks = @p_werks.
        IF sy-subrc <> 0.
          MESSAGE s045 DISPLAY LIKE 'E'.
          STOP.
        ENDIF.

        SELECT SINGLE zdefault INTO @DATA(lv_zdefault)
          FROM zafo_screen
          WHERE object = @gs_bustyp-object
          AND fieldname = 'KNTTP'.
        IF sy-subrc EQ 0  AND lv_zdefault = 'K'.
          CASE p_werks .
            WHEN '1000' OR '2000'.
            WHEN OTHERS.
              MESSAGE s118 DISPLAY LIKE 'E'."'此公司不能做费用采购，请使用库存采购或者费用报销！'
              STOP.
          ENDCASE.
        ENDIF.

        PERFORM frm_auth_bustyp_check IN PROGRAM saplzafo IF FOUND
          USING '01' p_typ p_werks
                CHANGING g_error .

      ELSEIF g_werks_flag = 'S'.
        PERFORM frm_auth_swerks_check  IN PROGRAM saplzafo IF FOUND
           TABLES s_werks USING '01'
                 CHANGING g_error.

      ENDIF.

      IF p_scan EQ 'X'.
        PERFORM frm_get_barcode_dis TABLES gt_barcode_dis USING p_typ.

        CALL FUNCTION 'ZMM_BARCODE_CHOISE'
          EXPORTING
            i_bustyp    = p_typ
            i_werks     = p_werks
          TABLES
            ct_sbarcode = gt_barcode_dis.

        CHECK gt_barcode_dis[] IS NOT INITIAL.

        CALL FUNCTION 'ZMM_BARCODE_DJ_CRE'
          EXPORTING
            i_bustyp    = p_typ
            i_werks     = p_werks
          TABLES
*           ET_RETURN   =
            ct_sbarcode = gt_barcode_dis
            ct_item     = gt_item.

        CHECK gt_item[] IS NOT INITIAL.

        CALL FUNCTION 'ZAFO_CREATE_SCAN'
          EXPORTING
            i_werks     = p_werks
            i_bustyp    = p_typ
          TABLES
*           ET_RETURN   =
            ct_item     = gt_item[]
            ct_sbarcode = gt_barcode_dis[]
          CHANGING
            cs_head     = gs_head
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
      ELSE.

        IF gs_bustyp-busref IS NOT INITIAL.
          PERFORM frm_get_ref TABLES gt_item gt_item_po USING gs_bustyp-busref.
        ENDIF.

        CHECK g_error IS INITIAL.

        CALL FUNCTION 'ZAFO_CREATE'
          EXPORTING
            i_werks      = p_werks
            i_bustyp     = p_typ
          TABLES
*           ET_RETURN    =
            ct_item      = gt_item[]
            ct_item_po   = gt_item_po[] "PO业务相关
            ct_item_cost = gt_item_cost[] "PO业务相关
          CHANGING
            cs_head      = gs_head
          EXCEPTIONS
            error        = 1
            OTHERS       = 2.

      ENDIF.

    WHEN p_mod.

      IF g_werks_flag = 'P'.

        PERFORM frm_auth_bustyp_check
          IN PROGRAM saplzafo IF FOUND
          USING '03' p_typ p_werks
                CHANGING g_error .

      ELSEIF g_werks_flag = 'S'.

        PERFORM frm_auth_swerks_check
           IN PROGRAM saplzafo IF FOUND
           TABLES s_werks USING '03'
                 CHANGING g_error.
      ENDIF.

      CHECK g_error IS INITIAL.

      CALL FUNCTION 'ZAFO_MAINTAIN'
        EXPORTING
*         i_werks  = p_werks
          i_bustyp = p_typ
          i_afono  = p_afono
*           IMPORTING
*         ES_HEAD  =
*           TABLES
*         ET_RETURN       =
*         ET_ITEM  =
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

    WHEN p_dis.

      IF g_werks_flag = 'P'.

        PERFORM frm_auth_bustyp_check IN PROGRAM saplzafo IF FOUND
          USING '03' p_typ p_werks  CHANGING g_error .

      ELSEIF g_werks_flag = 'S'.
        PERFORM frm_auth_swerks_check IN PROGRAM saplzafo IF FOUND
           TABLES s_werks USING '03' CHANGING g_error.

      ENDIF.

      CHECK g_error IS INITIAL.

      PERFORM frm_get_display TABLES gt_head gt_item.

      CHECK gt_head[] IS NOT INITIAL.
      CALL FUNCTION 'ZAFO_DISPLAY'
        EXPORTING
          i_werks  = p_werks
          i_bustyp = p_typ
        TABLES
          ct_head  = gt_head
          ct_item  = gt_item
*         ET_RETURN       =
        .
    WHEN p_load.

      IF g_werks_flag = 'P'.

        PERFORM frm_auth_bustyp_check
          IN PROGRAM saplzafo IF FOUND
          USING '01' p_typ p_werks
                CHANGING g_error .

      ELSEIF g_werks_flag = 'S'.

        PERFORM frm_auth_swerks_check
           IN PROGRAM saplzafo IF FOUND
           TABLES s_werks USING '01'
                 CHANGING g_error.

      ENDIF.

      CALL FUNCTION 'ZAFO_CREATE_UPLOAD'
        EXPORTING
          i_werks     = p_werks
          i_bustyp    = p_typ
          i_filename  = p_file
          i_begin_row = p_row
*           TABLES
*         ET_RETURN   =
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_group_user
*& 获取同小组成员
*&---------------------------------------------------------------------*
FORM get_group_user  TABLES lt_pernr.

  RANGES: lr_group FOR ztfi_group-name,
          lr_objid_user FOR ztfi_group-objid,
          lr_pernr FOR ztfi_group-pernr.
  DATA:lt_object TYPE STANDARD TABLE OF objec.
  DATA:lv_objid  TYPE  ztfi_group-objid,lv_objid1 TYPE hrobjid.
  DATA:lr_pernr_temp TYPE TABLE OF zhrs_employee.
  DEFINE add_range.
    &1-sign = 'I'.
    &1-option = 'EQ'.
    &1-low = &2.
    APPEND &1.
  end-of-DEFINITION.

  add_range lr_pernr sy-uname.
*同一个采购小组的人员可以相互查看付款申请单
*系统登录账号=PA0105-USRID，限制PA0105-SUBTY=0001找到PA0105-PERNR，通过PA0105-PERNR=PA0001-PERNR，找到PA0001-OBJD2（部门编号）
  SELECT pernr, subty, objps,  sprps,endda,begda, seqnr
   INTO TABLE @DATA(lt_pa)  FROM pa0105
        WHERE usrid = @sy-uname AND subty = '0001' .
  CHECK lt_pa[] IS NOT INITIAL.
  SORT lt_pa BY begda DESCENDING .
  READ TABLE lt_pa INTO DATA(wa_pa) INDEX 1.

  SELECT orgeh,begda  FROM pa0001   WHERE pernr = @wa_pa-pernr AND orgeh IS NOT INITIAL INTO  TABLE @DATA(lt_001) ."获取组织单位
  IF sy-subrc = 0.
    SORT lt_001 BY begda DESCENDING .
    READ TABLE lt_001 INTO DATA(wa_001) INDEX 1.
    IF sy-subrc = 0.
*获取组织单位的所有上级组织
      lv_objid =  wa_001-orgeh.
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype       = 'O'
          act_objid       = lv_objid
          act_wegid       = 'ORGA-UP'
*         ACT_INT_FLAG    =
*         act_plvar       = ''
          act_begda       = sy-datum
          act_endda       = sy-datum
          act_tdepth      = 0
          act_tflag       = 'X'
          act_vflag       = 'X'
          authority_check = ''
*         TEXT_BUFFER_FILL       =
*         BUFFER_MODE     =
*     IMPORTING
*         ACT_PLVAR       =
        TABLES
*         result_tab      = lt_result
          result_objec    = lt_object
*         RESULT_STRUC    =
        EXCEPTIONS
          no_plvar_found  = 1
          no_entry_found  = 2
          OTHERS          = 3.

*组织单位查询条件
      LOOP AT lt_object INTO DATA(lwa_result).
        add_range lr_objid_user lwa_result-objid.
      ENDLOOP.

    ENDIF.
  ENDIF.

*根据人员或组织查询采购组
  SELECT * FROM ztfi_group INTO TABLE @DATA(lt_002) WHERE pernr = @wa_pa-pernr OR objid IN @lr_objid_user.
  LOOP AT lt_002 INTO DATA(lwa_002).
    IF lwa_002-name IS NOT INITIAL.
      add_range lr_group lwa_002-name.
    ENDIF.
    CLEAR lwa_002.
  ENDLOOP.
*根据采购组查询所有的组织单位和个人
  IF lr_group[] IS NOT INITIAL.
    SORT lr_group BY low.
    DELETE ADJACENT DUPLICATES FROM lr_group COMPARING low.
    SELECT * FROM ztfi_group INTO TABLE @DATA(lt_003) WHERE pernr = @wa_pa-pernr OR objid IN @lr_objid_user OR name IN @lr_group.
    LOOP AT lt_003 INTO DATA(lwa_003).
      IF lwa_003-objid IS NOT INITIAL."如果是组织单位，查询该组织单位下的所有人员
        CLEAR:lr_pernr_temp.
        lv_objid1 = lwa_003-objid.
        CALL FUNCTION 'ZHR_GET_EMPOYEE_BY_DEPT'
          EXPORTING
            act_objid       = lv_objid1
            act_begda       = sy-datum
            act_endda       = sy-datum
            authority_check = '' "无权限检查
          TABLES
*           employees       = lt_pernr_temp
            rng_employees   = lr_pernr_temp.
        APPEND LINES OF lr_pernr_temp TO lr_pernr.
      ENDIF.
      IF lwa_003-pernr IS NOT INITIAL."如果是员工工号，添加到查询条件中
        add_range lr_pernr lwa_003-pernr.
      ENDIF.
      CLEAR lwa_003.
    ENDLOOP.
  ENDIF.
* 所有人员去重
  SORT   lr_pernr BY low.
  DELETE ADJACENT DUPLICATES FROM lr_pernr COMPARING low.
  APPEND LINES OF lr_pernr TO lt_pernr.
ENDFORM.

FORM frm_auth_check.
  IF gs_bustyp-execute_type = 'PO' OR gs_bustyp-execute_type = 'PRO'.
    SELECT * FROM t024
      INTO TABLE @DATA(gt_t024)
      WHERE ekgrp IN @s_ekgrp.

    CHECK sy-subrc EQ 0.

    CLEAR s_ekgrp[].

    LOOP AT gt_t024 INTO DATA(gs_t024).
      AUTHORITY-CHECK OBJECT 'ZAFO_EKGRP'
                 ID 'EKGRP' FIELD gs_t024-ekgrp
                 ID 'ACTVT' FIELD '01'.
      CHECK sy-subrc EQ 0.

      s_ekgrp-sign = 'I'.
      s_ekgrp-option = 'EQ'.
      s_ekgrp-low = gs_t024-ekgrp.
      APPEND s_ekgrp.
      CLEAR s_ekgrp.
    ENDLOOP.

  ENDIF.

ENDFORM.


FORM frm_download_temp CHANGING p_ucomm.

  CASE p_ucomm.
    WHEN 'DOWN'.
      CHECK p_typ IS NOT INITIAL.
      CALL FUNCTION 'ZAFO_DOWNLOAD_TEMPLATE'
        EXPORTING
          i_bustyp = p_typ
*         TABLES
*         ET_RETURN       =
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
      ENDIF.

  ENDCASE.
ENDFORM.

FORM frm_get_excel_f4  CHANGING p_p_up.


  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = 'D:\'
      mask             = '*,XLSX,*.XLSX,*.XLS,*.XLS'
      title            = TEXT-001
    IMPORTING
      filename         = p_p_up
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc <> 0 AND sy-subrc <> 3.
    MESSAGE i004(zafo) DISPLAY LIKE 'E'. "选择文件出错
    STOP.
  ENDIF.

ENDFORM.                    " FRM_GET_EXCEL_F4


FORM frm_tran_selection.
  DATA:lt_line TYPE TABLE OF zafo_sline WITH HEADER LINE.

  PERFORM frm_insert_line TABLES lt_line USING 'P_TYP' .
  PERFORM frm_insert_line TABLES lt_line USING 'P_AFONO' .
  PERFORM frm_insert_line TABLES lt_line USING 'P_WERKS' .
  PERFORM frm_insert_line TABLES lt_line USING 'S_LGORT'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_ZZPINO'   .
  PERFORM frm_insert_line TABLES lt_line USING 'S_ZPPDHD'   .
  PERFORM frm_insert_line TABLES lt_line USING 'S_KUNNR'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_ZZWLLY'   .
  PERFORM frm_insert_line TABLES lt_line USING 'S_BSART'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_EKGRP'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_EBELN'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_LIFNR'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_POTEXT'   .
  PERFORM frm_insert_line TABLES lt_line USING 'S_ZNAME1'   .
  PERFORM frm_insert_line TABLES lt_line USING 'S_MATNR'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_MAKTX'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_MTART'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_CATE1'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_CATE2'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_IDNLF'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_QCNO'     .
  PERFORM frm_insert_line TABLES lt_line USING 'S_AFONO'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_STATUS'   .
  PERFORM frm_insert_line TABLES lt_line USING 'S_ERNAM'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_ERDAT'    .
  PERFORM frm_insert_line TABLES lt_line USING 'S_BUDAT'    .

  CALL FUNCTION 'ZAFO_TRAN_SELECTION'
    TABLES
      ut_line = lt_line.

ENDFORM.


FORM frm_insert_line TABLES ct_line STRUCTURE zafo_sline USING name .
  DATA: dyn_table TYPE REF TO data.
  DATA: dyn_wa TYPE REF TO data.
  DATA: val_field TYPE char40.
  DATA: txt_field TYPE char40.

  DATA: tabname TYPE char20.
  DATA: strname TYPE char20.
  DATA: fieldname TYPE fieldname.
  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any,
                 <txt_field> TYPE any,
                 <val_field> TYPE any.

  IF name+0(1) = 'P'.
    ASSIGN (name) TO <txt_field>.
    CHECK sy-subrc EQ 0.

    ct_line-name = name.
    ct_line-line = <txt_field>.
    APPEND ct_line.
    CLEAR ct_line.
  ELSEIF name+(1) = 'S'.

    strname = name.
    CONCATENATE name '[]' INTO tabname.

    ASSIGN (tabname) TO <dyn_table>.
    CHECK sy-subrc EQ 0.

    ASSIGN (strname) TO <dyn_wa>.
    CHECK sy-subrc EQ 0.

    LOOP AT <dyn_table> INTO <dyn_wa>.
      ct_line-name = name.
      ct_line-line = <dyn_wa>.
      APPEND ct_line.
      CLEAR ct_line.
    ENDLOOP.
  ENDIF.
ENDFORM.


FORM frm_get_display TABLES ct_head STRUCTURE zafo_shead
                             ct_item STRUCTURE zafo_sitem.

  DATA: BEGIN OF gt_zzpino OCCURS 0,
          afono  TYPE zafono,
          zzpino TYPE zzpino,
        END OF gt_zzpino.
  DATA: ls_zzpino TYPE zzpino.
  DATA: lr_pernr TYPE TABLE OF zhrs_employee.
  IF p_self = 'X' AND ( gs_object-object = 'COST' OR gs_object-object = 'COST02') .
    PERFORM get_group_user TABLES  lr_pernr.
  ENDIF.


  SELECT h~* FROM zafo_head AS h
    LEFT JOIN zafo_item AS i ON h~afono = i~afono
    INTO CORRESPONDING FIELDS OF TABLE @ct_head
    WHERE bustyp = @p_typ
      AND ( h~werks IN @s_werks OR h~umwrk IN @s_werks )
      AND h~afono IN @s_afono
      AND h~lifnr IN @s_lifnr
      AND h~lifnr_name IN @s_zname1
      AND h~ernam IN @s_ernam
      AND h~ernam IN @lr_pernr
      AND h~erdat IN @s_erdat
      AND h~budat IN @s_budat
      AND h~status IN @s_status
      AND ( h~remark1 IN @s_potext OR h~remark2 IN @s_potext )
      AND i~ebeln  IN @s_ebeln
      AND i~zzpino IN @s_zzpino
      AND i~zppdhd IN @s_zppdhd
      AND i~idnlf IN @s_idnlf
      AND i~lgort IN @s_lgort.

  IF sy-subrc NE 0.
    MESSAGE s006 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SORT ct_head BY afono.
  DELETE ADJACENT DUPLICATES FROM ct_head COMPARING afono.

  SELECT * FROM zafo_item
    INTO CORRESPONDING FIELDS OF TABLE  ct_item
    FOR ALL ENTRIES IN ct_head
    WHERE afono = ct_head-afono  AND del_flag = ''.


  LOOP AT ct_head ASSIGNING  FIELD-SYMBOL(<fs_head>).
    PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
        USING <fs_head>-status CHANGING <fs_head>-icon <fs_head>-text .
  ENDLOOP.

  SORT ct_item BY afono zzpino  .
  CLEAR gt_zzpino[].
  LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<fs_item>).
    READ TABLE ct_head ASSIGNING <fs_head> WITH KEY afono = <fs_item>-afono.
    PERFORM frm_set_icon IN PROGRAM saplzafo IF FOUND
        USING <fs_head>-status CHANGING <fs_item>-icon <fs_item>-text .

    IF <fs_item>-zzpino IS NOT INITIAL.
      gt_zzpino-afono = <fs_item>-afono.
      gt_zzpino-zzpino = <fs_item>-zzpino.
      APPEND gt_zzpino.
      CLEAR gt_zzpino.
    ENDIF.
  ENDLOOP.

  SORT gt_zzpino.
  DELETE ADJACENT DUPLICATES FROM gt_zzpino.

  LOOP AT gt_zzpino.
    ls_zzpino = gt_zzpino-zzpino.
    AT NEW afono.
      READ TABLE ct_head ASSIGNING <fs_head> WITH KEY afono = gt_zzpino-afono.
      IF sy-subrc EQ 0.
        <fs_head>-remark5 = ls_zzpino.
        CONTINUE.
      ENDIF.
    ENDAT.
    <fs_head>-remark5 = <fs_head>-remark5 && ';' && ls_zzpino.
  ENDLOOP.

ENDFORM.
