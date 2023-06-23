*&---------------------------------------------------------------------*
*& 包含               ZAFO_I21_PBO
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'S0100'.
  SET TITLEBAR 'T0100' WITH gs_bustyp-bustyp '-' gs_bustyp-bustyp_name1.
ENDMODULE.


MODULE status_0200 OUTPUT.
  DATA fcode TYPE TABLE OF sy-ucomm.

  CLEAR: fcode.

  PERFORM frm_set_fcode TABLES fcode.

  SET PF-STATUS 'S0200' EXCLUDING fcode.

  IF gs_head-ernam IS INITIAL.
    gs_head-ernam  = sy-uname.
  ENDIF.
  SET TITLEBAR 'T0200' WITH gs_bustyp-bustyp '-' gs_bustyp-bustyp_name1 ' 申请人 ' gs_head-ernam.

ENDMODULE.


MODULE status_0300 OUTPUT.
  DATA:lt_exec TYPE TABLE OF sy-ucomm.
  CLEAR lt_exec.

  IF gs_bustyp-print_type IS INITIAL.
    APPEND '&PRINT' TO lt_exec.
    APPEND '&SUM_PRINT' TO lt_exec.
  ELSE.

    CASE gs_bustyp-print_type+(1).
      WHEN 'G' OR 'S'.
      WHEN OTHERS.
        APPEND '&SUM_PRINT' TO lt_exec.
    ENDCASE.

  ENDIF.

  IF gs_bustyp-execute_type <> 'PO' AND gs_bustyp-execute_type <> 'PRO'.
    APPEND '&GOTOGR' TO lt_exec.
  ENDIF.


  IF gs_bustyp-bustyp NE 'R1002' AND gs_bustyp-bustyp NE 'R1006'." 抽检报告录入
    APPEND '&ZSIRE' TO lt_exec.
  ENDIF.

  IF sy-uname NE '10015894' AND sy-uname NE 'AT-WTH'.
    APPEND '&TEST' TO lt_exec.
  ENDIF.


  SET PF-STATUS 'S0300' EXCLUDING lt_exec.
  SET TITLEBAR 'T0300' WITH gs_bustyp-bustyp '-' gs_bustyp-bustyp_name1.

ENDMODULE.


MODULE status_0500 OUTPUT.

  IF gs_head-zstaff IS INITIAL.
    gs_head-zstaff = sy-uname.
  ENDIF.
  CLEAR: fcode.

  PERFORM frm_set_fcode TABLES fcode.

  SET PF-STATUS 'S0500' EXCLUDING fcode.

  IF gs_head-ernam IS INITIAL.
    gs_head-ernam  = sy-uname.
  ENDIF.
  SET TITLEBAR 'T0500' WITH gs_bustyp-bustyp '-' gs_bustyp-bustyp_name1 ' 制单人 ' gs_head-ernam.
ENDMODULE.


MODULE status_0510 OUTPUT.
  DATA:ls_text TYPE char100.

  CLEAR: fcode.
  SET PF-STATUS 'S0510' EXCLUDING fcode.

  ls_text = <gs_item>-idnlf && <gs_item>-maktx && <gs_item>-zcolor_text && '拆分缸号,总数量' && <gs_item>-menge.
  SET TITLEBAR 'T0510' WITH  ls_text  .

ENDMODULE.


MODULE status_0600 OUTPUT.

  CLEAR: fcode.

  PERFORM frm_set_fcode TABLES fcode.

  SET PF-STATUS 'S0600' EXCLUDING fcode.

  IF gs_head-ernam IS INITIAL.
    gs_head-ernam  = sy-uname.
  ENDIF.
  SET TITLEBAR 'T0600' WITH gs_bustyp-bustyp '-' gs_bustyp-bustyp_name1 ' 制单人 ' gs_head-ernam.
ENDMODULE.


MODULE status_0700 OUTPUT.
  CLEAR: fcode.

  PERFORM frm_set_fcode TABLES fcode.

  IF gs_head-remark2 IS INITIAL.
    APPEND '&PACK_D' TO fcode.
  ENDIF.

  SET PF-STATUS 'S0700' EXCLUDING fcode.

  IF gs_head-ernam IS INITIAL.
    gs_head-ernam  = sy-uname.
  ENDIF.
  SET TITLEBAR 'T0700' WITH gs_bustyp-bustyp '-' gs_bustyp-bustyp_name1 ' 制单人 ' gs_head-ernam.

ENDMODULE.


MODULE status_0800 OUTPUT.

  CLEAR: fcode.

  PERFORM frm_set_fcode TABLES fcode.

  SET PF-STATUS 'S0800' EXCLUDING fcode.

  IF gs_head-ernam IS INITIAL.
    gs_head-ernam  = sy-uname.
  ENDIF.
  SET TITLEBAR 'T0800' WITH gs_bustyp-bustyp '-' gs_bustyp-bustyp_name1 ' 申请人 ' gs_head-ernam.

ENDMODULE.


MODULE status_0900 OUTPUT.

  CLEAR: fcode.

  PERFORM frm_set_fcode TABLES fcode.

  SET PF-STATUS 'S0900' EXCLUDING fcode.

  IF gs_head-ernam IS INITIAL.
    gs_head-ernam  = sy-uname.
  ENDIF.
  SET TITLEBAR 'T0900' WITH gs_bustyp-bustyp '-' gs_bustyp-bustyp_name1 ' 制单人 ' gs_head-ernam.
ENDMODULE.


FORM frm_set_fcode TABLES fcode .
  CLEAR fcode[].
  PERFORM frm_init_fcode.

  LOOP AT gt_fcode.
    IF gt_fcode-status = gs_head-status.
      APPEND gt_fcode-fcode TO fcode.
    ENDIF.
  ENDLOOP.

  IF g_bustyp+0(2) NE 'PO'.
    IF g_readonly = 'M'.
      APPEND '&COMMIT' TO fcode.
      APPEND '&EDIT' TO fcode.
      APPEND '&POST' TO fcode.
      APPEND '&DELETE' TO fcode.
    ELSEIF g_readonly = 'D'.
      APPEND '&SAVE' TO fcode.
      APPEND '&LSAVE' TO fcode.
    ENDIF.
  ENDIF.

  CASE gs_bustyp-execute_type.
    WHEN 'SO'.
      APPEND '&LSAVE' TO fcode.
      APPEND '&LIFNR_M' TO fcode.
      APPEND '&COMMIT' TO fcode.
  ENDCASE.

  IF gs_bustyp-execute_type IS INITIAL.
    APPEND '&POST' TO fcode.
  ENDIF.

  IF gs_bustyp-print_type IS INITIAL.
    APPEND '&PRINT' TO fcode.
  ENDIF.

  IF gs_head-app_status IS NOT INITIAL.
    APPEND '&UNCOMMIT' TO fcode.
  ELSE.
    APPEND '&GOTOAPP' TO fcode.
  ENDIF.

  IF gs_bustyp-bustyp = 'CJD01'.

    SELECT SINGLE department INTO @DATA(lv_department)
      FROM zapp_addr
      WHERE person = @sy-uname.
    IF lv_department <> '仓库' AND lv_department <> '信息部'.
      APPEND '&DELETE' TO fcode.
    ENDIF.

    APPEND '&COMMIT' TO fcode.

    IF gs_head-app_status = 'A'.
      APPEND '&GOTOAPP' TO fcode.
    ENDIF.
  ELSE.
    APPEND '&CJQR' TO fcode.
  ENDIF.


  CASE gs_bustyp-bustyp.
    WHEN 'ASN02' OR 'ASN03'
      OR 'R1001' OR 'R1002' OR 'R1003' OR 'R1006'
      OR 'R1013' OR 'R1014' OR 'R1015' OR 'R1019'.
    WHEN OTHERS.
      APPEND '&CJTS' TO fcode.
  ENDCASE.

  IF sy-uname <> '10015894' AND sy-uname <> 'AT-WTH'.
    APPEND '&TEST' TO fcode.
  ENDIF.

ENDFORM.


MODULE init_po OUTPUT.

  CHECK gs_bustyp-execute_type = 'PO' OR gs_bustyp-execute_type = 'PRO'.

  IF gs_head-bsart IS INITIAL.
    gs_head-bsart = 'PO01'.
  ENDIF.

  IF gs_head-mwskz IS  INITIAL.
    CALL FUNCTION 'ZCCT_GET_TAX'
      EXPORTING
*       I_TAX_CODE =
        i_tax_type = 'J'
*       I_TXT_RATE =
      IMPORTING
        e_tax_code = gs_head-mwskz
*       E_TAX_RATE =
*     EXCEPTIONS
*       ERROR      = 1
*       OTHERS     = 2
      .
  ENDIF.

ENDMODULE.


MODULE init_so OUTPUT.

  CHECK gs_bustyp-execute_type = 'SO'.

  IF gs_head-bsart IS INITIAL.
    gs_head-bsart = 'SO01'.
  ENDIF.

  IF gs_head-mwskz IS  INITIAL.
    CALL FUNCTION 'ZCCT_GET_TAX'
      EXPORTING
*       I_TAX_CODE =
        i_tax_type = 'X'
*       I_TXT_RATE =
      IMPORTING
        e_tax_code = gs_head-mwskz
*       E_TAX_RATE =
*     EXCEPTIONS
*       ERROR      = 1
*       OTHERS     = 2
      .
  ENDIF.

ENDMODULE.


FORM frm_init_fcode." 排除按钮
  CHECK gt_fcode[] IS INITIAL.
  DEFINE append_code.    "INCREMENT为自定义宏的名称。
    gt_fcode-status = &1.
    gt_fcode-fcode = &2.
    APPEND gt_fcode.
  END-OF-DEFINITION.

*&SAVE
*&EDIT
*&COMMIT
*&UNCOMMIT
*&DELETE
*&GOTOAPP

  append_code ''  '&EDIT'.
  append_code ''  '&LIFNR_M'.
  append_code ''  '&COMMIT'.
  append_code ''  '&UNCOMMIT'.
  append_code ''  '&DELETE'.
  append_code ''  '&GOTOAPP'.
  append_code ''  '&POST'.
  append_code ''  '&CANCEL'.
  append_code ''  '&PRINT'.
  append_code ''  '&ZPRINT'.

  append_code 'E'  '&COMMIT'.
  append_code 'E'  '&UNCOMMIT'.
  append_code 'E'  '&GOTOAPP'.
  append_code 'E'  '&CANCEL'.
  append_code 'E'  '&ZPRINT'.

  append_code 'A'  '&LSAVE'.
  append_code 'A'  '&UNCOMMIT'.
  append_code 'A'  '&GOTOAPP'.
  append_code 'A'  '&CANCEL'.

  append_code 'B'  '&LSAVE'.
  append_code 'B'  '&SAVE'.
  append_code 'B'  '&EDIT'.
  append_code 'B'  '&LIFNR_M'.
  append_code 'B'  '&COMMIT'.
  append_code 'B'  '&UNCOMMIT'.
  append_code 'B'  '&DELETE'.
  append_code 'B'  '&POST'.
  append_code 'B'  '&CANCEL'.
  append_code 'B'  '&CJQR'.

  append_code 'C'  '&LSAVE'.
  append_code 'C'  '&SAVE'.
  append_code 'C'  '&EDIT'.
  append_code 'C'  '&LIFNR_M'.
  append_code 'C'  '&COMMIT'.
  append_code 'C'  '&DELETE'.
  append_code 'C'  '&POST'.
  append_code 'C'  '&CANCEL'.
  append_code 'C'  '&CJQR'.

  append_code 'D'  '&LSAVE'.
  append_code 'D'  '&SAVE'.
  append_code 'D'  '&EDIT'.
  append_code 'D'  '&LIFNR_M'.
  append_code 'D'  '&COMMIT'.

  append_code 'D'  '&UNCOMMIT'.
  append_code 'D'  '&DELETE'.
  append_code 'D'  '&GOTOAPP'.
  append_code 'D'  '&POST'.
  append_code 'D'  '&CANCEL'.
  append_code 'D'  '&CJQR'.

  append_code 'F'  '&LSAVE'.
  append_code 'F'  '&SAVE'.
  append_code 'F'  '&EDIT'.
  append_code 'S'  '&LIFNR_M'.
  append_code 'F'  '&COMMIT'.
  append_code 'F'  '&UNCOMMIT'.
  append_code 'F'  '&DELETE'.
  append_code 'F'  '&GOTOAPP'.
  append_code 'F'  '&POST'.

  append_code 'S'  '&LSAVE'.
  append_code 'S'  '&SAVE'.
  append_code 'S'  '&EDIT'.
  append_code 'S'  '&COMMIT'.
  append_code 'S'  '&LIFNR_M'.
  append_code 'S'  '&UNCOMMIT'.
  append_code 'S'  '&DELETE'.
  append_code 'S'  '&GOTOAPP'.
  append_code 'S'  '&POST'.

  append_code 'T'  '&LSAVE'.
  append_code 'T'  '&SAVE'.
  append_code 'T'  '&EDIT'.
  append_code 'T'  '&COMMIT'.
  append_code 'T'  '&UNCOMMIT'.
  append_code 'T'  '&DELETE'.
  append_code 'T'  '&POST'.
  append_code 'T'  '&CANCEL'.
  append_code 'T'  '&CJQR'.
  append_code 'T'  '&LIFNR_M'.

ENDFORM.


MODULE init_sum_item OUTPUT.

  CHECK gs_bustyp-sum_screen = 'X'.
  PERFORM frm_set_screen_sum .

ENDMODULE.


MODULE screen_mod_head OUTPUT.

  DATA:ls_fname TYPE char30.

  DATA:lt_screen TYPE TABLE OF zafo_screen WITH HEADER LINE.

  zafo_shead = gs_head.
  CLEAR lt_screen[].
  LOOP AT gt_screen WHERE object = g_object AND fieldalv = 'HEAD'.
    MOVE-CORRESPONDING gt_screen TO lt_screen.
    lt_screen-fieldname = 'GS_HEAD-' && gt_screen-fieldname.
    APPEND lt_screen .

    lt_screen-fieldname = '<GS_TEXT>-' && gt_screen-fieldname.
    APPEND lt_screen .
    CLEAR lt_screen.
  ENDLOOP.

  LOOP AT gt_screen INTO lt_screen WHERE object = g_object AND fieldalv = ''.
    lt_screen-fieldname = lt_screen-fieldname.
    APPEND lt_screen .
    CLEAR lt_screen.
  ENDLOOP.

  LOOP AT SCREEN.
    READ TABLE lt_screen WITH KEY fieldname = screen-name.
    IF sy-subrc EQ 0.
      IF lt_screen-fedit = 'X'.
        screen-input = '1'.
      ELSEIF lt_screen-fedit = 'C'.
        IF g_action = 'CRE'.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
      ELSE.
        screen-input = '0'.
      ENDIF.

      IF lt_screen-requi = 'X'.
        screen-required = '2'.
      ELSE.
        screen-required = '0'.
      ENDIF.

      IF lt_screen-hidde = 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.

    ELSE.
*        IF sy-DYNNR <>  '0400'.
*          screen-active = 0.
*        ENDIF.
    ENDIF.

    IF g_readonly = 'D'.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.


MODULE create_gos_service OUTPUT.
  DATA:obj TYPE borident.
  DATA:manager TYPE REF TO cl_gos_manager.
  DATA:it_gos_sels TYPE tgos_sels.
  DATA:is_gos_sels TYPE sgos_sels.

  CHECK gs_head-afono IS NOT INITIAL .

*    PERFORM frm_set_gos_sels TABLES it_gos_sels.

  obj-objtype = 'ZAFO'.

  obj-objkey = gs_head-afono .

  IF manager IS INITIAL .
    CREATE OBJECT manager
      EXPORTING
*       IP_START_DIRECT      = 'X'
        it_service_selection = it_gos_sels
*       ip_no_instance       = 'X'
        is_object            = obj
*       ip_no_commit         = 'X'
      EXCEPTIONS
        OTHERS               = 1.

  ENDIF.

  cl_gui_cfw=>flush( ).

ENDMODULE.                    "CREATE_GOS_SERVICE


FORM frm_set_gos_sels TABLES it_gos_sels STRUCTURE sgos_sels.

  DEFINE append_gos_sels  .
    it_gos_sels-sign = 'I'.
    it_gos_sels-option = 'EQ'..
    it_gos_sels-low = &1.
    APPEND it_gos_sels.
    CLEAR it_gos_sels.
  End-of-definition.

  CHECK it_gos_sels[] IS  INITIAL.
*    append_gos_sels 'ESJI_UNFOLLO'   . "取消关注
*    append_gos_sels 'FBKR_V_FIDOC'   . "View FI document
*    append_gos_sels 'FBKR_V_MO   '   . "View Memo Order
*    append_gos_sels 'FSSC_LINKAIC'   . "链接到 AIC
*    append_gos_sels 'INFO_SERVICE'   . "对象服务的帮助
*    append_gos_sels 'J3RF_IMCD   '   . "导入报关
*    append_gos_sels 'MYOBJECTS   '   . "我的对象
*    append_gos_sels 'MYO_ADD     '   . "添加我的对象
*    append_gos_sels 'NOTE_CREA   '   . "创建注释
  append_gos_sels 'ARL_LINK   '   . "创建注释
  append_gos_sels 'PCATTA_CREA '   . "创建附件
*    append_gos_sels 'PERS_NOTE   '   . "私人注释
*    append_gos_sels 'POC_MONITOR '   . "流程监控器
*    append_gos_sels 'PPFACTION   '   . "中的动作
*    append_gos_sels 'RESUBMISSION'   . "创建重新提交
*    append_gos_sels 'SOFTPHONE   '   . "电话
  append_gos_sels 'SO_SENDHIST '   . "对象发件箱
  append_gos_sels 'SO_SENDOBJ  '   . "为对象发送注释
  append_gos_sels 'SO_SENDSERV '   . "发送
  append_gos_sels 'SRELATIONS  '   . "关系
*    append_gos_sels 'SUBSCRIBE   '   . "预订/取消对象
*    append_gos_sels 'URL_CREA    '   . "创建外部凭证 (URL)
  append_gos_sels 'VIEW_ATTA   '   . "附件清单
  append_gos_sels 'ZMSG   '   . "附件清单
  append_gos_sels 'ZAPP_CREATE   '   . "附件清单
*    append_gos_sels 'WF_ARCHIVE  '   . "归档的工作流
*    append_gos_sels 'WF_OVERVIEW '   . "工作流概览
*    append_gos_sels 'WF_SERVICES '   . "工作流
*    append_gos_sels 'WF_START    '   . "启动工作流
*    append_gos_sels 'WF_TOOLBOX  '   . "当前工作流任务

ENDFORM.


MODULE set_cursor OUTPUT.
  CLEAR:g_ean.
  SET CURSOR FIELD 'G_EAN'.
ENDMODULE.                 " SET_CURSOR


MODULE init_barcode OUTPUT.

  CHECK gs_head-afono IS NOT INITIAL.

  REFRESH gt_barcode.
  REFRESH gt_barcode_dis.

  SELECT * FROM zmm_barcode
    WHERE afono = @gs_head-afono
    INTO TABLE @gt_barcode.

  LOOP AT gt_barcode.
    MOVE-CORRESPONDING gt_barcode TO gs_barcode_dis.
    gs_barcode_dis-icon = icon_led_green.
    gs_barcode_dis-text = TEXT-035." '已成功'.
    APPEND gs_barcode_dis TO gt_barcode_dis.
  ENDLOOP.

ENDMODULE.


MODULE init_zscmt0011 OUTPUT.
  CHECK zscmt0011 IS INITIAL.

  SELECT SINGLE * FROM zscmt0011
    WHERE lifnr = gs_head-lifnr
    AND uname = sy-uname.

  IF sy-subrc NE 0.
    SELECT SINGLE * FROM zscmt0011
       WHERE lifnr = gs_head-lifnr.
  ENDIF.

ENDMODULE.
