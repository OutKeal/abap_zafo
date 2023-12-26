class ZAFO_BASIC definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_fcat,
        dzaehk    TYPE col_pos,
        fieldname TYPE fieldname,
        edit      TYPE lvc_edit,
        emphasize TYPE lvc_emphsz,
        hotspot   TYPE lvc_hotspt,
        coltext   TYPE lvc_txtcol,
        tech      TYPE lvc_tech,
        hidde     TYPE lvc_tech,
      END OF ty_fcat .

  class-methods GET_BUSTYP
    importing
      value(BUSTYP) type ZAFO_BUSTYP
    returning
      value(R_BUSTYP) type ZAFO_BUSTYPE .
  class-methods GET_BUSTYP_BY_TCODE
    importing
      value(TCODE) type SY-TCODE
    returning
      value(R_BUSTYP) type ZAFO_TT_BUSTYPE .
  class-methods GET_BUSTYP_SEL_SCREEN
    importing
      value(BUSTYP) type ZAFO_BUSTYP
    returning
      value(R_SEL_SCREEN) type ZAFO_TT_SEL_SCREEN .
  class-methods GET_BUSTYP_SEL_VALUE
    importing
      value(BUSTYP) type ZAFO_BUSTYP
    returning
      value(R_SEL_VALUE) type ZAFO_TT_SEL_VALUE .
  class-methods GET_BUSTYP_REF
    importing
      !BUSTYP type ZAFO_BUSTYP
    returning
      value(RT_BUSTYP) type ZAFO_RANGE_BUSTYP .
  class-methods GET_BUSTYP_SCREEN
    importing
      !BUSTYP type ZAFO_BUSTYP
      !FIELDALV type ZAFO_FIELDALV default ''
    returning
      value(RT_SCREEN) type ZAFO_TT_SCREEN .
  class-methods GET_BUSTYP_ACT
    importing
      !BUSTYP type ZAFO_BUSTYP
    returning
      value(RT_ACT) type ZAFO_TT_OBJECT_ACT .
  class-methods GET_BUSTYP_PRINT
    importing
      !BUSTYP type ZAFO_BUSTYP
    returning
      value(RT_PRINT) type ZAFO_TT_PRINT_RULE .
  class-methods GET_OBJECT
    importing
      !OBJECT type ZAFO_EOBJECT
    returning
      value(R_OBJECT) type ZAFO_OBJECT .
  class-methods GET_BUSTYP_DICT
    importing
      !BUSTYP type ZAFO_BUSTYP
      !WERKS type WERKS_D
    returning
      value(RT_DICT) type ZAFO_TT_DICT .
  class-methods SET_ICON
    importing
      value(STATUS) type ZAFO_STATUS
    exporting
      value(ICON) type ICON_D
      value(TEXT) type ZAFO_TEXT .
  class-methods AUTH_CHECK_LINE
    importing
      value(ACTVT) type ACTIV_AUTH
      value(BUSTYP) type ZAFO_BUSTYP
      value(WERKS) type WERKS_D
    returning
      value(BOOL) type ABAP_BOOL .
  class-methods AUTH_CHECK_TAB
    importing
      value(ACTVT) type ACTIV_AUTH
      value(BUSTYP) type ZAFO_BUSTYP
    changing
      value(SWERKS) type RANGE_T_WERKS
    returning
      value(BOOL) type ABAP_BOOL .
  class-methods CALL_TRANSATION
    importing
      value(TYPE) type CHAR30
      value(KEY1) type ANY optional
      value(KEY2) type ANY optional
      value(KEY3) type ANY optional .
  class-methods CALL_TRANSATION_BY_LINE
    importing
      value(LINE) type ANY
      value(FIELDNAME) type FIELDNAME
    returning
      value(BOOL) type ABAP_BOOL .
  class-methods INIT_GUI_STATUS
    returning
      value(GUI_STATUS) type TT_FCODES .
protected section.

  class-data GT_BUSTYP type ZAFO_TT_BUSTYPE .
  class-data GT_OBJECT type ZAFO_TT_BUSTYPE .
  class-data GT_SEL_SCREEN type ZAFO_TT_SEL_SCREEN .
  class-data GT_SEL_VALUE type ZAFO_TT_SEL_VALUE .
  class-data GT_SCREEN type ZAFO_TT_SCREEN .
  class-data GT_DICT type ZAFO_TT_DICT .
  class-data GT_ACT type ZAFO_TT_OBJECT_ACT .
  class-data GT_PRINT type ZAFO_TT_PRINT_RULE .
private section.
ENDCLASS.



CLASS ZAFO_BASIC IMPLEMENTATION.


  METHOD auth_check_line.
    bool = abap_false.


    AUTHORITY-CHECK OBJECT 'ZAFO_BUSTY'
    ID 'ZAFO_BUSTY' FIELD bustyp
    ID 'ACTVT' FIELD actvt.
    IF sy-subrc NE 0 .
      MESSAGE s030 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'ZAFO_WERKS'
    ID 'WERKS' FIELD werks
    ID 'ACTVT' FIELD actvt.
    IF sy-subrc NE 0 .
      MESSAGE s031 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    bool = abap_true.
  ENDMETHOD.


  METHOD auth_check_tab.

    DATA: s_werks TYPE ranges_werks_tt.
    bool = abap_false.

    AUTHORITY-CHECK OBJECT 'ZAFO_BUSTY'
    ID 'ZAFO_BUSTY' FIELD bustyp
    ID 'ACTVT' FIELD actvt.
    IF sy-subrc NE 0 .
      MESSAGE s030 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT werks FROM t001w INTO TABLE @DATA(lt_t001w)
       WHERE werks IN @swerks[].
    LOOP AT lt_t001w INTO DATA(ls_t001w).

      AUTHORITY-CHECK OBJECT 'ZAFO_WERKS'
                                                  ID 'WERKS' FIELD ls_t001w-werks
                                                  ID 'ACTVT' FIELD actvt.
      IF sy-subrc = 0..
        APPEND VALUE #( sign = 'I'
                                      option = 'EQ'
                                       low = ls_t001w-werks
                                      ) TO s_werks.
      ENDIF.
    ENDLOOP.

    IF s_werks IS NOT INITIAL.
      swerks = s_werks.
      bool = abap_true.
    ELSE.
      MESSAGE s031 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.


METHOD CALL_TRANSATION.
  CASE TYPE.
  WHEN 'EBAN' OR 'PR' OR 'BUS2015'."采购申请
    SET PARAMETER ID 'BAN' FIELD key1.
    CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

  WHEN 'EKKO' OR 'PO' OR 'STO' OR 'BUS2012'.  "采购订单
    SET PARAMETER ID 'BES' FIELD key1.
    CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.

  WHEN 'ASN' . "内向交货单"
    SET PARAMETER ID 'VL' FIELD key1.
    CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.

  WHEN 'VBAK' OR 'SO' OR 'BUS2032'. "销售订单"
    SET PARAMETER ID 'AUN' FIELD key1.
    CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

  WHEN 'LIKP' OR 'DN' OR 'VL'. "交货单"
    SET PARAMETER ID 'VL' FIELD key1.
    CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

  WHEN 'VBRK' OR 'VF' OR 'RV' OR 'BUS2037'. "发票"
    SET PARAMETER ID 'VF' FIELD key1.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

  WHEN 'RESB' OR 'RS' OR 'BUS2093'. "预留"
    SET PARAMETER ID 'RES' FIELD key1.
    CALL TRANSACTION  'MB23' AND SKIP FIRST SCREEN.

  WHEN 'MKPF' OR 'MB' OR 'BUS2017'. "商品凭证"
    CALL FUNCTION 'MIGO_DIALOG'
    EXPORTING
      i_action = 'A04'
      i_refdoc = 'R02'
      i_mblnr  = key1
      i_mjahr  = key2.

  WHEN 'RBKP' OR 'MR' OR 'BUS2081'.            "发票校验"
    SET PARAMETER ID 'RBN' FIELD key1.
    SET PARAMETER ID 'GJR' FIELD key2.
    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

  WHEN 'BKPF' OR 'FB' .  "会计凭证
    SET PARAMETER ID 'BLN' FIELD key1.
    SET PARAMETER ID 'GJR' FIELD key2.
    SET PARAMETER ID 'BUK' FIELD key3.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  WHEN 'BUS1001' . "商品"
    SET PARAMETER ID 'MAT' FIELD key1.
    CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

  WHEN 'MARA' OR 'MATNR' OR 'BUS1001001' . "商品"
    SET PARAMETER ID 'MAT' FIELD key1.
    CALL TRANSACTION 'MM43' AND SKIP FIRST SCREEN.

  WHEN 'KNA1' OR 'LFA1' OR 'KUNNR' OR 'LIFNR' OR 'BUS1006'. "客户/供应商"
    SET PARAMETER ID 'BPA' FIELD key1.
    SUBMIT r_ftr_display_bp WITH p_bp = key1 AND RETURN.

  WHEN 'AUFK' OR 'BUS2075'.  "内部订单"
    SET PARAMETER ID 'ANR' FIELD key1.
    CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.

  WHEN 'SKA1' OR 'HKONT' OR ' BUS3006'. "科目"
    SET PARAMETER ID 'SAK' FIELD key1.
    SET PARAMETER ID 'BUK' FIELD key2.
    CALL TRANSACTION 'FS00' AND SKIP FIRST SCREEN.

  WHEN 'MAST' OR 'BOM' OR 'BUS3006'. "BOM"
    SET PARAMETER ID 'MAT' FIELD key1.
    SET PARAMETER ID 'WRK' FIELD key2.
    SET PARAMETER ID 'CSV' FIELD key3.
    CALL TRANSACTION 'CS03' AND SKIP FIRST SCREEN.

  WHEN 'ANLA' OR 'ANLN1' OR 'BUS1022'.   "固定资产
    SET PARAMETER ID 'AN1' FIELD key1.
    SET PARAMETER ID 'BUK' FIELD key2.
    CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.

  WHEN 'T001W' OR 'WERKS' OR 'BUS1069'. "地点
    SET PARAMETER ID 'WRK' FIELD key1.
    CALL TRANSACTION 'WB03' AND SKIP FIRST SCREEN.

  WHEN 'AFKO' OR 'AUFNR' OR 'BUS2005'."生产订单
    SET PARAMETER ID 'ANR' FIELD key1.
    CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN.

  WHEN 'EDIDC' OR 'IDOC'."IDOC
    SUBMIT idoc_tree_control WITH docnum = key1 AND RETURN.

  WHEN 'XML' OR 'PROXY'.
    SUBMIT rsxmb_display_msg_vers_new WITH msgguid = key1
    AND RETURN.
  ENDCASE.
ENDMETHOD.


METHOD call_transation_by_line.
  bool = abap_false.
  ASSIGN COMPONENT fieldname OF STRUCTURE line TO FIELD-SYMBOL(<value>).
  CHECK sy-subrc EQ 0.
  CHECK <value> IS NOT INITIAL.

  CASE fieldname.
    WHEN 'BANFN' ."采购申请
      SET PARAMETER ID 'BAN' FIELD <value>.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
      bool = abap_true.

    WHEN 'EBELN' .  "采购订单
      SET PARAMETER ID 'BES' FIELD <value>.
      CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'VBELN_ASN' . "内向交货单"
      SET PARAMETER ID 'VL' FIELD <value>.
      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'VBELN_VA'. "销售订单"
      SET PARAMETER ID 'AUN' FIELD <value>.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'VBELN_VL'. "交货单"
      SET PARAMETER ID 'VL' FIELD <value>.
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'VBELN_VF'. "发票"
      SET PARAMETER ID 'VF' FIELD <value>.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'RSNUM'. "预留"
      SET PARAMETER ID 'RES' FIELD <value>.
      CALL TRANSACTION  'MB23' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'MBLNR'. "商品凭证"
      ASSIGN COMPONENT 'MJAHR' OF STRUCTURE line TO FIELD-SYMBOL(<mjahr>).
      CHECK sy-subrc EQ 0.
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_action = 'A04'
          i_refdoc = 'R02'
          i_mblnr  = <value>
          i_mjahr  = <mjahr>.
      bool = abap_true.
    WHEN 'BELNR_R' .            "发票校验"
      ASSIGN COMPONENT 'GJAHR_R' OF STRUCTURE line TO FIELD-SYMBOL(<gjahr_r>).
      CHECK sy-subrc EQ 0.
      SET PARAMETER ID 'RBN' FIELD <value>.
      SET PARAMETER ID 'GJR' FIELD <gjahr_r>.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'BELNR' .  "会计凭证
      ASSIGN COMPONENT 'GJAHR' OF STRUCTURE line TO FIELD-SYMBOL(<gjahr>).
      CHECK sy-subrc EQ 0.
      ASSIGN COMPONENT 'BUKRS' OF STRUCTURE line TO FIELD-SYMBOL(<bukrs>).
      CHECK sy-subrc EQ 0.
      SET PARAMETER ID 'BLN' FIELD <value>.
      SET PARAMETER ID 'GJR' FIELD <gjahr>.
      SET PARAMETER ID 'BUK' FIELD <bukrs>.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'MATNR' OR 'SATNR'. "商品"
      SET PARAMETER ID 'MAT' FIELD <value>.
      CALL TRANSACTION 'MM43' AND SKIP FIRST SCREEN.
      bool = abap_true.

    WHEN 'LIFNR' OR 'KUNNR' . "客户/供应商"
      SET PARAMETER ID 'BPA' FIELD <value>.
      SUBMIT r_ftr_display_bp WITH p_bp = <value> AND RETURN.
      bool = abap_true.
    WHEN 'AUFNR_U' .  "内部订单"
      SET PARAMETER ID 'ANR' FIELD <value>.
      CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'SAKNR' OR 'HKONT' . "科目"
      ASSIGN COMPONENT 'BUKRS' OF STRUCTURE line TO FIELD-SYMBOL(<bukrs_skb1>).
      CHECK sy-subrc EQ 0.
      SET PARAMETER ID 'SAK' FIELD <value>.
      SET PARAMETER ID 'BUK' FIELD <bukrs_skb1>.
      CALL TRANSACTION 'FS00' AND SKIP FIRST SCREEN.
      bool = abap_true.

    WHEN 'ANLA' OR 'ANLN1' OR 'BUS1022'.   "固定资产
      ASSIGN COMPONENT 'BUKRS' OF STRUCTURE line TO FIELD-SYMBOL(<bukrs_as03>).
      CHECK sy-subrc EQ 0.
      SET PARAMETER ID 'AN1' FIELD <value>.
      SET PARAMETER ID 'BUK' FIELD <bukrs_as03>.
      CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'WERKS'  OR 'UMWRK'. "地点
      SET PARAMETER ID 'WRK' FIELD <value>.
      CALL TRANSACTION 'WB03' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'AUFNR' ."生产订单
      SET PARAMETER ID 'ANR' FIELD <value>.
      CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN.
      bool = abap_true.
    WHEN 'DOCNUM' ."IDOC
      SUBMIT idoc_tree_control WITH docnum = <value> AND RETURN.
      bool = abap_true.
    WHEN 'XML' OR 'PROXY'.
      SUBMIT rsxmb_display_msg_vers_new WITH msgguid = <value>
      AND RETURN.
      bool = abap_true.
    WHEN 'RUECK' .
      ASSIGN COMPONENT 'RMZHL' OF STRUCTURE line TO FIELD-SYMBOL(<rmzhl>).
      CHECK sy-subrc EQ 0.
      CHECK <rmzhl> IS NOT INITIAL.
      SET PARAMETER ID 'RCK' FIELD <value>.
      SET PARAMETER ID 'RZL' FIELD <rmzhl>.
      CALL TRANSACTION 'CO14' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDMETHOD.


  METHOD get_bustyp.

    READ TABLE gt_bustyp INTO r_bustyp WITH KEY bustyp = bustyp.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM zafo_bustype
        WHERE bustyp = @bustyp
        INTO @r_bustyp.
      IF sy-subrc EQ 0.
        APPEND r_bustyp TO gt_bustyp.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_bustyp_act.
    DATA(ls_bustyp) = get_bustyp( bustyp ).
    CHECK ls_bustyp-object IS NOT INITIAL.

    rt_act = VALUE #( FOR wa IN gt_act
                                             WHERE ( object = ls_bustyp-object ) ( wa ) ) .

    IF rt_act IS INITIAL.
      SELECT * FROM zafo_object_act
      WHERE object = @ls_bustyp-object
      APPENDING TABLE @gt_act.
      IF sy-subrc EQ 0.
        rt_act = VALUE #( FOR wa IN gt_act
                                         WHERE ( object = ls_bustyp-object ) ( wa ) ) .
      ELSE.
        APPEND VALUE #( object = ls_bustyp-object
                        ) TO gt_act.
      ENDIF.
    ENDIF.



  ENDMETHOD.


  METHOD get_bustyp_by_tcode.

    r_bustyp[] = VALUE #( FOR wa IN gt_bustyp
                                      WHERE ( tcode = tcode ) ( wa ) ).

    IF r_bustyp[] IS INITIAL.
      SELECT * FROM zafo_bustype
        WHERE tcode = @tcode
        APPENDING TABLE @gt_bustyp.

      IF sy-subrc EQ 0.
          r_bustyp[] = VALUE #( FOR wa IN gt_bustyp
                                  WHERE ( tcode = tcode ) ( wa ) ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_bustyp_dict.
    DATA(ls_bustyp) = get_bustyp( bustyp ).
    CHECK ls_bustyp-bustyp IS NOT INITIAL.

    rt_dict = VALUE #( FOR wa IN gt_dict
    WHERE ( bustyp = ls_bustyp  and ( werks = werks or werks = '' ) ) ( wa ) ) .

    IF rt_dict IS INITIAL.
      SELECT * FROM zafo_dict
      WHERE bustyp = @ls_bustyp-bustyp
       AND ( werks = @werks OR werks = '' )
      APPENDING TABLE @gt_dict.
      IF sy-subrc EQ 0.
        rt_dict = VALUE #( FOR wa IN gt_dict
        WHERE ( bustyp = ls_bustyp-bustyp and ( werks = werks or werks = '' ) ) ( wa ) ) .
      ELSE.
        APPEND VALUE #( bustyp = ls_bustyp-bustyp
        ) TO gt_dict.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD get_bustyp_ref.
    DATA(l_bustyp) = get_bustyp( bustyp ).

    IF l_bustyp-bustyp_bak IS NOT INITIAL.
      SPLIT l_bustyp-bustyp_bak AT ',' INTO TABLE DATA(lt_bustyp).
      LOOP AT lt_bustyp INTO DATA(ls_bustyp).
        APPEND VALUE #( sign = 'I'
                                     option = 'EQ'
                                     low = ls_bustyp
                                    ) TO rt_bustyp.
      ENDLOOP.
    ENDIF.

    IF l_bustyp-bustyp_ref IS NOT INITIAL.
      APPEND VALUE #( sign = 'I'
                                    option = 'EQ'
                                    low = l_bustyp-bustyp_ref
                                          ) TO rt_bustyp.
    ENDIF.

    SORT rt_bustyp BY low..
    DELETE ADJACENT DUPLICATES FROM rt_bustyp .

  ENDMETHOD.


  METHOD GET_BUSTYP_SCREEN.
    DATA(ls_bustyp) = get_bustyp( bustyp ).
    CHECK ls_bustyp-object IS NOT INITIAL.

    rt_screen = VALUE #( FOR wa IN gt_screen
                                             WHERE ( object = ls_bustyp-object ) ( wa ) ) .

    IF rt_screen IS INITIAL.
      SELECT * FROM zafo_screen
      WHERE object = @ls_bustyp-object
      APPENDING TABLE @gt_screen.
      IF sy-subrc EQ 0.
        rt_screen = VALUE #( FOR wa IN gt_screen
                                         WHERE ( object = ls_bustyp-object ) ( wa ) ) .
      ELSE.
        APPEND VALUE #( object = ls_bustyp-object
                        ) TO gt_screen.
      ENDIF.
    ENDIF.

    IF fieldalv IS NOT INITIAL.
      DELETE rt_screen WHERE fieldalv <> fieldalv.
    ENDIF.


  ENDMETHOD.


  METHOD GET_BUSTYP_SEL_SCREEN.

    r_sel_screen = VALUE #( FOR wa IN gt_sel_screen
                                             WHERE ( bustyp = bustyp ) ( wa ) ) .

    IF r_sel_screen IS INITIAL.
      SELECT * FROM zafo_sel_screen
        WHERE bustyp = @bustyp
        APPENDING TABLE @gt_sel_screen.
      IF sy-subrc EQ 0.
        r_sel_screen = VALUE #( FOR wa IN gt_sel_screen
                                       WHERE ( bustyp = bustyp ) ( wa ) ) .
      ELSE.
        APPEND VALUE #( bustyp = bustyp
                                      ) TO gt_sel_screen.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_bustyp_sel_value.

    r_sel_value = VALUE #( FOR wa IN gt_sel_value
                                             WHERE ( bustyp = bustyp ) ( wa ) ) .

    IF r_sel_value IS INITIAL.
      SELECT * FROM zafo_sel_value
        WHERE bustyp = @bustyp
        APPENDING TABLE @gt_sel_value.
      IF sy-subrc EQ 0.
        r_sel_value = VALUE #( FOR wa IN gt_sel_value
                                       WHERE ( bustyp = bustyp ) ( wa ) ) .
      ELSE.
        APPEND VALUE #( bustyp = bustyp
                                      ) TO r_sel_value.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_object.

    READ TABLE gt_object INTO r_object WITH KEY object = object.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM zafo_object
      WHERE object = @object
      INTO @r_object.
      IF sy-subrc EQ 0.
        APPEND r_object TO gt_object.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD init_gui_status.
    zwft_common=>get_status_functions( EXPORTING program = 'SAPLZAFO' status = '200'
    IMPORTING  functions = DATA(functions) ).
    DELETE functions WHERE fcode+0(1) <> '&'.
    LOOP AT functions INTO DATA(fun).
      APPEND fun-fcode TO gui_status.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_icon.

    CASE status.
      WHEN ''.
        icon = icon_led_inactive.
        text = TEXT-002."'未保存'.

      WHEN 'A'.
        icon = icon_led_yellow.
        text = TEXT-003."'已保存'.

      WHEN 'B'.
        icon = icon_structure.
        text = TEXT-004."'审核中'.

      WHEN 'C'.
        icon = icon_led_green.
        text = TEXT-005."'已审核'.

      WHEN 'S'.
        icon = icon_complete.
        text = TEXT-006."'已完成'.

      WHEN 'T'.
        icon = icon_allow.
        text = TEXT-007."'后续已完成'.

      WHEN 'L'.
        icon = icon_locked.
        text = TEXT-008."'已锁定'.

      WHEN 'D'.
        icon = icon_delete.
        text = TEXT-009."'已作废'.

      WHEN 'E'.
        icon = icon_led_yellow.
        text = TEXT-010."'暂存'.

      WHEN 'F'.
        icon = icon_status.
        text = TEXT-047."'暂存'.


    ENDCASE.
  ENDMETHOD.


  METHOD get_bustyp_print.


    rt_print = VALUE #( FOR wa IN gt_print
                                             WHERE ( bustyp = bustyp ) ( wa ) ) .

    IF rt_print IS INITIAL.
      SELECT * FROM zafo_v_print
      WHERE bustyp = @bustyp
      APPENDING TABLE @gt_print.
      IF sy-subrc EQ 0.
        rt_print = VALUE #( FOR wa IN gt_print
                                         WHERE ( bustyp = bustyp ) ( wa ) ) .
      ELSE.

      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
