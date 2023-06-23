*&---------------------------------------------------------------------*
*& 包含               ZAFO_I22_PAI
*&---------------------------------------------------------------------*
MODULE get_cursor INPUT.
  CLEAR:g_field.
  GET CURSOR FIELD g_field.

  SET CURSOR FIELD g_ean.
ENDMODULE.


MODULE bsart_dorp_list INPUT."采购凭证类型
  DATA:BEGIN OF lt_bsart OCCURS 0 ,
         batxt LIKE t161t-batxt,
         bsart LIKE t161-bsart,
       END OF lt_bsart.

  SELECT t161~bsart, batxt
    INTO CORRESPONDING FIELDS OF TABLE @lt_bsart
    FROM t161
    LEFT JOIN t161t ON t161~bstyp = t161t~bstyp AND t161~bsart = t161t~bsart
    WHERE t161~bstyp = 'B'
    AND t161t~spras = @sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = 'GS_HEAD-BSART'
    TABLES
      value_tab = lt_bsart.

ENDMODULE.


MODULE ekgrp_dorp_list OUTPUT."采购组

  TYPE-POOLS vrm.
  DATA: vid    TYPE vrm_id VALUE 'GS_HEAD-EKGRP',
        vlist  TYPE vrm_values,
        values LIKE LINE OF vlist.

  CLEAR vlist.
  CLEAR values.

  SELECT * INTO TABLE @DATA(lt_t024)
    FROM t024.

  LOOP AT lt_t024 INTO DATA(ls_t024).
    MOVE ls_t024-ekgrp TO values-key.
    MOVE ls_t024-eknam TO values-text.
    APPEND values TO vlist.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = vid
      values          = vlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
  ENDIF.
ENDMODULE.


MODULE vkgrp_dorp_list OUTPUT."销售组

  TYPE-POOLS vrm.
  DATA: va_vid    TYPE vrm_id VALUE 'GS_HEAD-VKGRP',
        va_vlist  TYPE vrm_values,
        va_values LIKE LINE OF vlist.

  CLEAR va_vlist.
  CLEAR va_values.

  SELECT * INTO TABLE @DATA(lt_tvkgr)
    FROM tvgrt
    WHERE spras = @sy-langu.

  LOOP AT lt_tvkgr INTO DATA(ls_tvkgr).
    MOVE ls_tvkgr-vkgrp TO va_values-key.
    MOVE ls_tvkgr-bezei TO va_values-text.
    APPEND va_values TO va_vlist.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = va_vid
      values          = va_vlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
  ENDIF.
ENDMODULE.


MODULE reason_dorp_list INPUT." 原因说明
  DATA:BEGIN OF lt_reason OCCURS 0 ,
         reason LIKE zafo_reason-reason,
       END OF lt_reason.

  CLEAR lt_reason[].
  LOOP AT gt_reason WHERE bustyp = g_bustyp AND fieldname = ''.
    lt_reason-reason = gt_reason-reason.
    APPEND lt_reason.
  ENDLOOP.

  CHECK lt_reason IS NOT  INITIAL.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = 'GS_HEAD-REASON'
    TABLES
      value_tab = lt_reason.
ENDMODULE.


MODULE mwskz_dorp_list INPUT."销售/购买税代码

  CALL FUNCTION 'FI_F4_MWSKZ'
    EXPORTING
      i_kalsm = 'TAXCN'
    IMPORTING
      e_mwskz = gs_head-mwskz.

ENDMODULE.



MODULE gs_head-ekorg INPUT.
  IF gs_head-ekorg IS INITIAL.
    CLEAR gs_head-bukrs.
  ELSE.
    gs_head-bukrs = gs_head-ekorg.
    gs_head-werks = gs_head-ekorg.
  ENDIF.
ENDMODULE.

MODULE gs_head-vkorg INPUT.
  IF gs_head-vkorg IS INITIAL.
    CLEAR gs_head-bukrs.
  ELSE.
    gs_head-bukrs = gs_head-vkorg.
    gs_head-werks = gs_head-vkorg.
  ENDIF.
ENDMODULE.


MODULE gs_head-werks INPUT." 发货工厂 和库存地点

  IF gs_head-werks IS NOT INITIAL.
    SELECT SINGLE name1
      INTO gs_head-werks_name
      FROM t001w
      WHERE werks EQ gs_head-werks.
    IF sy-subrc NE 0.
      gs_head-werks = ''.
      gs_head-werks_name = ''.
      MESSAGE s045."  '工厂不存在' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF gs_head-lgort IS NOT INITIAL.
    SELECT SINGLE lgobe INTO gs_head-lgort_name
      FROM t001l
      WHERE werks EQ gs_head-werks AND lgort = gs_head-lgort .
    IF sy-subrc NE 0.
      gs_head-lgort = ''.
      gs_head-lgort_name = ''.
      MESSAGE s046."'库存地点不存在' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDMODULE.


MODULE gs_head-umwrk INPUT." 收货工厂 和库存地点
  IF gs_head-umwrk IS NOT INITIAL.
    SELECT SINGLE name1 INTO gs_head-umwrk_name
      FROM t001w
      WHERE werks EQ gs_head-umwrk.
    IF sy-subrc NE 0.
      CLEAR gs_head-umwrk.
      CLEAR gs_head-umwrk_name.
      MESSAGE s045."'工厂不存在' TYPE 'S'.
    ELSE.
      CASE gs_bustyp-execute_type .
        WHEN 'CC'.
          gs_head-bukrs = gs_head-umwrk.
          gs_head-bukrs_name = gs_head-umwrk_name.
          LOOP AT gt_item ASSIGNING <gs_item>.
            <gs_item>-bukrs = gs_head-umwrk.
            <gs_item>-bukrs_name = gs_head-umwrk_name.
            <gs_item>-umwrk = gs_head-umwrk.
            <gs_item>-umwrk_name = gs_head-umwrk_name.
          ENDLOOP.
        WHEN OTHERS.
          IF gs_bustyp-bustyp EQ 'H4007' .
            LOOP AT gt_item ASSIGNING <gs_item>.
              <gs_item>-umwrk = gs_head-umwrk.
              <gs_item>-umwrk_name = gs_head-umwrk_name.
            ENDLOOP.
          ELSE.
            gs_head-umwrk = gs_head-werks.
            gs_head-umwrk_name = gs_head-werks_name.
          ENDIF.
      ENDCASE.
    ENDIF.
  ELSE.
    CASE gs_bustyp-execute_type .
      WHEN 'CC'.
      WHEN OTHERS.
        IF gs_bustyp-bustyp EQ 'H4007' .
        ELSE.
          gs_head-umwrk = gs_head-werks.
          gs_head-umwrk_name = gs_head-werks_name.
        ENDIF.
    ENDCASE.
  ENDIF.

  IF gs_head-umlgo IS NOT INITIAL.
    IF gs_head-umwrk IS INITIAL.
      CLEAR gs_head-umlgo.
      CLEAR gs_head-umlgo_name.
      MESSAGE s047."'收货库存地点不存在' TYPE 'S'.
    ELSE.
      SELECT SINGLE lgobe INTO gs_head-umlgo_name
        FROM t001l
        WHERE werks EQ gs_head-umwrk AND lgort =  gs_head-umlgo .
      IF sy-subrc NE 0.
        CLEAR gs_head-umlgo.
        CLEAR gs_head-umlgo_name.
        MESSAGE s047." '收货库存地点不存在' TYPE 'S'.
      ELSE.
        gs_head-umlgo = gs_head-umlgo.
        gs_head-umlgo_name = gs_head-umlgo_name.

        LOOP AT gt_item ASSIGNING <gs_item>.
          <gs_item>-umlgo = gs_head-umlgo.
          <gs_item>-umlgo_name = gs_head-umlgo_name.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.


MODULE gs_head-nenam INPUT.
  CHECK gs_head-nenam IS NOT INITIAL.

  SELECT SINGLE person
    INTO gs_head-nenam
    FROM zapp_addr
    WHERE person EQ gs_head-nenam
    OR name EQ gs_head-nenam.
  IF sy-subrc NE 0.
    CLEAR gs_head-nenam.
    MESSAGE i106."您输入的工号名称不存在!
  ENDIF.
ENDMODULE.


MODULE gs_head-afnam INPUT.
  CHECK gs_head-afnam IS NOT INITIAL.

  SELECT SINGLE person
    INTO gs_head-afnam
    FROM zapp_addr
    WHERE person EQ gs_head-afnam OR name EQ gs_head-afnam.
  IF sy-subrc NE 0.
    CLEAR gs_head-afnam.
    MESSAGE i106."您输入的工号名称不存在!
  ENDIF.
ENDMODULE.


MODULE gs_head-lifnr INPUT.
  DATA: in_lifnr_text  LIKE gs_head-lifnr,
        out_lifnr_text LIKE gs_head-werks.

  IF gs_head-lifnr IS INITIAL.
    CLEAR gs_head-lifnr.
    CLEAR gs_head-lifnr_name.
    CLEAR gs_head-waers.
    RETURN.
  ENDIF.

  CALL FUNCTION 'ZAFO_VENDOR_SEARCH'
    CHANGING
      lifnr = gs_head-lifnr.

  in_lifnr_text = gs_head-lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = in_lifnr_text
    IMPORTING
      output = out_lifnr_text.

  IF out_lifnr_text = gs_head-werks.
    MESSAGE e109."供应商不能为当前公司！.
    CLEAR gs_head-lifnr.
    RETURN.
  ENDIF.

  IF gs_head-lifnr IS NOT INITIAL.
    SELECT SINGLE name1
      INTO gs_head-lifnr_name
      FROM lfa1
      WHERE lifnr EQ gs_head-lifnr.
    IF sy-subrc NE 0.
      gs_head-lifnr = ''.
      gs_head-lifnr_name = ''.
      gs_head-waers = ''.
      MESSAGE s048."'供应商不存在' TYPE 'S'.
    ELSE.
      SELECT SINGLE  waers ,zname1,country
        FROM zscmt0010
        INTO ( @gs_head-waers , @gs_head-lifnr_name , @gs_head-land1 )
        WHERE partner = @gs_head-lifnr .
      LOOP AT gt_item ASSIGNING <gs_item>.
        <gs_item>-waers = gs_head-waers.
        <gs_item>-lifnr = gs_head-lifnr.
        <gs_item>-lifnr_name = gs_head-lifnr_name.
      ENDLOOP.
      LOOP AT gt_item_po ASSIGNING <gs_item_po>.
        <gs_item_po>-waers = gs_head-waers.
        <gs_item_po>-lifnr = gs_head-lifnr.
        <gs_item_po>-lifnr_name = gs_head-lifnr_name.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMODULE.


MODULE gs_head-kunnr INPUT.
  DATA: in_kunnr_text  LIKE gs_head-kunnr,
        out_kunnr_text LIKE gs_head-werks.

  CALL FUNCTION 'ZAFO_CUSTOMER_SEARCH'
    CHANGING
      kunnr = gs_head-kunnr.

  in_kunnr_text = gs_head-kunnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = in_kunnr_text
    IMPORTING
      output = out_kunnr_text.

  IF out_kunnr_text = gs_head-werks.
    MESSAGE e133."客户不能为当前公司！.
    CLEAR gs_head-kunnr.
    RETURN.
  ENDIF.

  IF gs_head-kunnr IS NOT INITIAL.
    SELECT SINGLE name1
      INTO gs_head-kunnr_name
      FROM kna1
      WHERE kunnr EQ gs_head-kunnr.
    IF sy-subrc NE 0.
      gs_head-kunnr = ''.
      gs_head-kunnr_name = ''.
      gs_head-waers = ''.
      MESSAGE s048."'供应商不存在' TYPE 'S'.
    ELSE.
      SELECT SINGLE waers ,zname1,country FROM zscmt0010
        INTO ( @gs_head-waers , @gs_head-kunnr_name, @gs_head-land1 )
        WHERE partner = @gs_head-kunnr .

    ENDIF.

  ENDIF.

ENDMODULE.


MODULE gs_head-remark3 INPUT." 出口装箱 货柜编号
  DATA: BEGIN OF lt_help OCCURS 0.
  DATA: zzhkbh LIKE zsdhgwh-zzhkbh,
        zzzgrq LIKE zsdhgwh-zzzgrq,
        werks  LIKE zsdhgwh-werks,
        zzivno LIKE zsdhgwh-zzivno,
        zzbeiz LIKE zsdhgwh-zzbeiz,
        END OF lt_help.
  DATA: lt_ddshretval LIKE ddshretval OCCURS 0 WITH HEADER LINE.
  DATA: ls_ddshretval LIKE LINE OF lt_ddshretval.
  DATA: zzksrq TYPE zzzgrq.
  DATA: lv_zzhkbh TYPE zzhkbh.

  CLEAR: lt_help.

  IF gs_head-remark3 IS NOT INITIAL.
    SELECT SINGLE zzhkbh INTO gs_head-remark3
      FROM zsdhgwh
      WHERE zzhkbh = gs_head-remark3.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
  ENDIF.

  zzksrq = sy-datum - 60.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_help
    FROM zsdhgwh
    WHERE zzzgrq >= zzksrq.

  SORT lt_help BY werks zzzgrq zzhkbh.
  CHECK lt_help[] IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'ZZHKBH'
      dynpprog   = sy-cprog
      dynpnr     = sy-dynnr
*     WINDOW_TITLE = TEXT-S05
      value_org  = 'S'
    TABLES
      value_tab  = lt_help
      return_tab = lt_ddshretval.

  READ TABLE lt_ddshretval INDEX 1.
  IF sy-subrc = 0.
    lv_zzhkbh = lt_ddshretval-fieldval.
    READ TABLE lt_help WITH KEY zzhkbh = lv_zzhkbh.
    IF sy-subrc EQ 0.
      gs_head-remark3 = lt_help-zzhkbh.
    ELSE.
      CLEAR gs_head-remark3.
    ENDIF.
  ELSE.
    CLEAR gs_head-remark3.
  ENDIF.

ENDMODULE.


MODULE gs_head-eeind.
  IF gs_head-eeind IS NOT INITIAL.
    LOOP AT gt_item ASSIGNING <gs_item> WHERE eeind <> gs_head-eeind .
      IF <gs_item>-eeind IS INITIAL OR <gs_item>-eeind = '00000000'.
        <gs_item>-eeind = gs_head-eeind.
        IF gt_item_po[] IS NOT INITIAL .
          PERFORM frm_set_po_eeind CHANGING <gs_item>.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF g_grid_600_top IS BOUND.
    PERFORM f_refresh_grid_alv USING g_grid_600_top.
    IF gt_item_po[] IS NOT INITIAL .
      PERFORM f_refresh_grid_alv USING g_grid_600_right.
    ENDIF.
  ENDIF.

  IF g_grid_200 IS BOUND.
    PERFORM f_refresh_grid_alv USING g_grid_200.
  ENDIF.

ENDMODULE.


MODULE gs_head-brand.
  IF gs_head-brand IS NOT INITIAL.
    LOOP AT gt_item ASSIGNING <gs_item> .
      IF <gs_item>-brand IS NOT INITIAL .
        IF <gs_item>-brand = gs_head-brand.
          CONTINUE.
        ELSE.
          <gs_item>-brand = gs_head-brand.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF g_grid_600_top IS BOUND.
    PERFORM f_refresh_grid_alv USING g_grid_600_top.
  ENDIF.

  IF g_grid_200 IS BOUND.
    PERFORM f_refresh_grid_alv USING g_grid_200.
  ENDIF.
ENDMODULE.


MODULE gs_head-bukrs INPUT.

  IF gs_head-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt
      INTO gs_head-bukrs_name
      FROM t001 WHERE bukrs EQ gs_head-bukrs  .
    IF sy-subrc NE 0.
      CLEAR gs_head-bukrs.
      CLEAR gs_head-bukrs_name.
      MESSAGE s049."'公司代码填写错误' TYPE 'S'.
    ENDIF.
    IF gs_head-knttp EQ 'K'.
      CASE gs_head-bukrs .
        WHEN '1000' OR '2000'.
        WHEN OTHERS.
          CLEAR gs_head-bukrs.
          CLEAR gs_head-bukrs_name.
          MESSAGE s118 DISPLAY LIKE 'E'."'此公司不能做费用采购，请使用库存采购或者费用报销！' TYPE 'E'.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDMODULE.


MODULE gs_head-kostl INPUT.
  DATA : lv_bukrs TYPE bukrs.
  IF gs_head-kostl IS NOT INITIAL.
    IF gs_head-bustyp EQ 'C4001'.
      IF gs_head-umwrk IS INITIAL .
        MESSAGE s120 DISPLAY LIKE 'E'."请输入收货工厂！
      ENDIF.
      lv_bukrs = gs_head-umwrk.
    ELSE.
      lv_bukrs = gs_head-bukrs.
    ENDIF.
    SELECT SINGLE ktext
      INTO gs_head-kostl_name
      FROM vfco_csks_shv
      WHERE kostl EQ gs_head-kostl AND bukrs EQ lv_bukrs.
    IF sy-subrc NE 0.
      CLEAR gs_head-kostl_name .
      CLEAR gs_head-kostl .
      MESSAGE s050."'请选择对应公司的成本中心' TYPE 'S'.
    ENDIF.

    IF gs_head-knttp EQ 'K' .
      CASE gs_head-kostl .
        WHEN '0010000050' OR '0010000060' OR '0010000080' OR '0010000220' OR '0010000230'
              OR '0020000010' OR '0020000020' OR '0020000030'.
        WHEN OTHERS.
          CLEAR gs_head-kostl_name .
          CLEAR gs_head-kostl .
          MESSAGE s119 DISPLAY LIKE 'E'."此成本中心不能做费用采购，请使用库存采购或者费用报销！' TYPE 'E'.
      ENDCASE.
    ENDIF.

  ENDIF.
ENDMODULE.


MODULE gs_head-zstaff INPUT.
  IF gs_head-zstaff IS NOT INITIAL.
    SELECT SINGLE name_textc
      INTO gs_head-zstaff_name
      FROM user_addr
      WHERE bname EQ gs_head-zstaff .
    IF sy-subrc NE 0.
      CLEAR gs_head-zstaff .
      CLEAR gs_head-zstaff_name .
      MESSAGE s051."'仓管员填写错误' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDMODULE.


MODULE exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.


MODULE exit_command INPUT.
  PERFORM frm_unlock.

  PERFORM free_object.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      CLEAR sy-ucomm.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'EXIT' OR 'QUIT'.
      CLEAR sy-ucomm.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.


MODULE user_command_0300 INPUT.
  DATA:it_head TYPE TABLE OF zafo_shead WITH HEADER LINE.
  DATA:it_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA: seltab    TYPE TABLE OF rsparams,
        seltab_wa LIKE LINE OF seltab.

  PERFORM frm_before_command.

  CASE sy-ucomm.
    WHEN '&REFRESH'.
      PERFORM frm_refresh_300.
      PERFORM f_refresh_hold_grid_alv CHANGING g_grid_300_up.
      PERFORM f_refresh_hold_grid_alv CHANGING g_grid_300_down.

    WHEN '&DETIAL'.
      CLEAR sy-ucomm.

      CALL METHOD g_grid_300_up->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      IF lt_index_rows[] IS INITIAL.
        MESSAGE s055 DISPLAY LIKE 'E'."'请至少选择一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR gt_item_front_dis[].

      LOOP AT lt_index_rows INTO ls_index_rows.

        PERFORM frm_get_item_front_dis USING ls_index_rows-index.

      ENDLOOP.

      PERFORM f_refresh_grid_alv USING g_grid_300_down.

    WHEN '&REPORT'.
      gs_bustyp-busref = 'Y'.
      gt_item_100[] = gt_item_dis[].
      CALL SCREEN 100.

    WHEN '&PRINT'.
      CLEAR sy-ucomm.

      CALL METHOD g_grid_300_up->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      DATA(lvx_line) = lines( lt_index_rows ).

      IF lvx_line < 1.
        MESSAGE s056."'请一行抬头数据' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF lvx_line > 1 AND gs_bustyp-print_type+0(1) = 'P'.
        MESSAGE s102."'采购合同只能选择一行打印
        RETURN.
      ENDIF.

      CLEAR it_head[].
      CLEAR it_item[].

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_head_dis INDEX ls_index_rows-index.

        IF gs_bustyp-print_per IS NOT INITIAL AND gt_head_dis-print_no IS INITIAL.
          CALL FUNCTION 'ZAFO_PRINT_GET_MB_NO'
            EXPORTING
              is_bustyp = gs_bustyp
            CHANGING
              cs_head   = gt_head_dis.
        ENDIF.

        APPEND gt_head_dis TO it_head.
        LOOP AT gt_item_dis WHERE afono = gt_head_dis-afono.
          APPEND gt_item_dis TO it_item.
        ENDLOOP.
      ENDLOOP.

      PERFORM frm_print TABLES it_head[] it_item[].

    WHEN '&SUM_PRINT'.
      CLEAR sy-ucomm.

      DATA: ls_head_top TYPE zafo_shead.
      DATA:new_key TYPE char200.
      DATA:last_key TYPE char200.
      FIELD-SYMBOLS: <ls_head_dis> TYPE zafo_shead.

      CALL METHOD g_grid_300_up->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.
      DATA(lvsum_line) = lines( lt_index_rows ).

      IF lvsum_line < 2.
        MESSAGE s107."'请至少选择两行抬头数据！
        RETURN.
      ENDIF.

      CLEAR new_key.
      CLEAR last_key.
      LOOP AT lt_index_rows INTO ls_index_rows .
        READ TABLE gt_head_dis ASSIGNING <ls_head_dis> INDEX ls_index_rows-index.
        new_key = <ls_head_dis>-werks && <ls_head_dis>-lifnr &&  <ls_head_dis>-lgort.
        IF last_key IS INITIAL.
          last_key = new_key.
        ELSE.
          IF new_key <> last_key.
            MESSAGE '关键字不同，无法合并:同公司，供应商，库存地点' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDIF.

      ENDLOOP.

      CLEAR it_head[].
      CLEAR it_item[].

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_head_dis INDEX ls_index_rows-index.

        IF ls_head_top IS INITIAL .
          ls_head_top = gt_head_dis.

          CLEAR ls_head_top-menge.
          CLEAR ls_head_top-amount.
          IF gs_bustyp-print_per IS NOT INITIAL AND ls_head_top-print_no IS INITIAL.
            CALL FUNCTION 'ZAFO_PRINT_GET_MB_NO'
              EXPORTING
                is_bustyp = gs_bustyp
              CHANGING
                cs_head   = ls_head_top.
          ENDIF.
        ENDIF.

        gt_head_dis-print_no = ls_head_top-print_no.
        ls_head_top-menge = ls_head_top-menge + gt_head_dis-menge.
        ls_head_top-amount = ls_head_top-amount + gt_head_dis-amount.

        UPDATE zafo_head SET print_no = ls_head_top-print_no
                 WHERE afono = gt_head_dis-afono AND afono NE ls_head_top-afono.

        LOOP AT gt_item_dis WHERE afono = gt_head_dis-afono.
          gt_item_dis-afono = ls_head_top-afono.
          APPEND gt_item_dis TO it_item.
        ENDLOOP.

      ENDLOOP.

      COMMIT WORK.

      APPEND ls_head_top TO it_head.
      CLEAR ls_head_top.
      PERFORM frm_print TABLES it_head[] it_item[].

    WHEN '&GOTOGR'." 采购入库报表
      CALL METHOD g_grid_300_up->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.

      IF lt_index_rows[] IS INITIAL.
        MESSAGE s055 DISPLAY LIKE 'E'."'请至少选择一行抬头数据'
        RETURN.
      ENDIF.
      RANGES:r_ebeln FOR ekko-ebeln.
      CLEAR r_ebeln[].
      r_ebeln-sign = 'I'.
      r_ebeln-option = 'EQ'.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_head_dis INDEX ls_index_rows-index.
        IF sy-subrc EQ 0.
          r_ebeln-low = gt_head_dis-afono.
          APPEND r_ebeln.
        ENDIF.
      ENDLOOP.

      SUBMIT zmmr0160 WITH s_ebeln IN r_ebeln AND RETURN.

    WHEN '&ZSIRE'." 短码抽检报告
      CLEAR sy-ucomm.

      CALL METHOD g_grid_300_up->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.
      DATA(lvsire_line) = lines( lt_index_rows ).

      IF lvsire_line <> 1.
        MESSAGE s043."'请选择一行数据！ TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      LOOP AT lt_index_rows INTO ls_index_rows.
        READ TABLE gt_head_dis INDEX ls_index_rows-index.
        CLEAR seltab.
        CLEAR seltab_wa.
        seltab_wa-selname = 'S_AFONO'.
        seltab_wa-sign = 'I'.
        seltab_wa-option = 'EQ'.
        seltab_wa-low = gt_head_dis-afono.
        APPEND seltab_wa TO seltab.
        SUBMIT zmm_zsire WITH SELECTION-TABLE seltab AND RETURN.
      ENDLOOP.

    WHEN '&TEST'.
      CLEAR sy-ucomm.
      CALL METHOD g_grid_300_up->get_selected_rows
        IMPORTING
          et_index_rows = lt_index_rows
          et_row_no     = lt_row_no.
      DELETE lt_index_rows WHERE rowtype IS NOT INITIAL.

*      LOOP AT lt_index_rows INTO ls_index_rows.
*        CLEAR gs_head.
*        REFRESH gt_item.
*        READ TABLE gt_head_dis INDEX ls_index_rows-index.
*        IF sy-subrc EQ 0 .
*          SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_head
*            FROM zafo_head WHERE afono = gt_head_dis-afono AND bustyp = gt_head_dis-bustyp.
*
*          SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item
*            FROM zafo_item
*            WHERE afono = gt_head_dis-afono
*            AND del_flag = ''.
*          PERFORM frm_delete_data.
*        ENDIF.
*
*      ENDLOOP.

  ENDCASE.

  PERFORM frm_after_command.
ENDMODULE.


MODULE user_command_0200 INPUT.

  CALL METHOD g_grid_200->check_changed_data.

  PERFORM frm_before_command.

  CASE sy-ucomm.
    WHEN '&ENTR'.
      PERFORM frm_call_transaction USING g_field.

    WHEN '&EDIT'.
      CLEAR sy-ucomm.

      PERFORM frm_set_edit_status_02 CHANGING g_refresh_catalog.

      IF g_refresh_catalog = 'X'.
        PERFORM f_set_catalog_alv_200 .
      ENDIF.

    WHEN '&PRINT'.
      CLEAR it_head[].
      APPEND gs_head TO it_head.
      LOOP AT gt_item WHERE menge <> 0.
        APPEND gt_item TO it_item.
        CLEAR gt_item.
      ENDLOOP.
      PERFORM frm_print TABLES it_head it_item.

    WHEN '&LSAVE'.
      CLEAR sy-ucomm.

      PERFORM frm_before_save_data.

      PERFORM frm_check_save.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_data.

      g_readonly = 'D'.

      PERFORM f_set_catalog_alv_200 .

      PERFORM frm_pop_msg.

    WHEN '&SAVE'.
      CLEAR sy-ucomm.

      PERFORM frm_before_save_data.

      PERFORM frm_check_save.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      IF gs_bustyp-execute_type = 'PR' OR gs_bustyp-execute_type = 'PRO'.
        PERFORM frm_post.
        IF g_error = 'X'.
          PERFORM frm_pop_msg.
          RETURN.
        ENDIF.

      ENDIF.

      PERFORM frm_save_data.

      g_readonly = 'D'.

      PERFORM f_set_catalog_alv_200 .

      PERFORM frm_pop_msg.
    WHEN '&CJQR'.
      CLEAR sy-ucomm.

      IF g_readonly NE 'D'.
        MESSAGE s122."请先保存数据!
        RETURN.
      ENDIF.

      READ TABLE gt_item INDEX 1.
      PERFORM frm_cjqr USING gt_item-ebeln.

      PERFORM f_set_catalog_alv_200 .
      CALL METHOD cl_gui_cfw=>flush.

    WHEN '&COMMIT'.
      CLEAR sy-ucomm.

      IF g_readonly NE 'D'.
        MESSAGE s122."请先保存数据!
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-036."'是否确认提交?'.

      CHECK g_error <> 'X'.

      PERFORM frm_commit.

    WHEN '&UNCOMMIT'.

      CLEAR sy-ucomm.

      PERFORM frm_pop_confirm USING TEXT-037."'是否确认取消审核?'.

      CHECK g_error <> 'X'.

      PERFORM frm_uncommit.

    WHEN '&TEST'.
      PERFORM frm_cjsx USING gs_head-ebeln.

    WHEN '&DELETE'.
      CLEAR sy-ucomm.

      PERFORM frm_check_delete.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-038."'是否确认删除单据?'.

      CHECK g_error <> 'X'.

      PERFORM frm_delete_po.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.
      PERFORM frm_delete_data.

    WHEN '&GOTOAPP'.
      CLEAR sy-ucomm.

      PERFORM frm_goto_app.

  ENDCASE.

  PERFORM frm_after_command.
ENDMODULE.


MODULE user_command_0500 INPUT.
  CALL METHOD g_grid_500_left->check_changed_data.

  IF gs_bustyp-sum_screen = 'X'.
    CALL METHOD g_grid_500_right->check_changed_data.
  ENDIF.

  PERFORM frm_before_command.

  CASE sy-ucomm.
    WHEN '&ENTR'.
      CLEAR sy-ucomm.
      CASE g_field.
        WHEN 'G_EAN'.
          PERFORM frm_scan.
        WHEN 'GS_HEAD-MBLNR'.
          PERFORM frm_call_transaction USING g_field.
      ENDCASE.

    WHEN '&EDIT'.
      CLEAR sy-ucomm.

      PERFORM frm_set_edit_status CHANGING g_refresh_catalog.

      IF g_refresh_catalog = 'X'.
        PERFORM f_set_catalog_alv_500 .
      ENDIF.

    WHEN '&PRINT'.
      CLEAR sy-ucomm.

      CHECK gs_bustyp-print_type IS NOT INITIAL.

      PERFORM frm_auth_bustyp_check USING '04' gs_head-bustyp g_werks
                                 CHANGING g_error.

      CHECK g_error IS INITIAL.

      READ TABLE gt_item INDEX 1.
      IF sy-subrc EQ 0 AND gt_item-werks IS NOT INITIAL..
        gs_head-werks = gt_item-werks.
      ENDIF.

      CLEAR it_head[].
      CLEAR it_item[].

      IF gs_bustyp-print_per IS NOT INITIAL AND gs_head-print_no IS INITIAL.
        CALL FUNCTION 'ZAFO_PRINT_GET_MB_NO'
          EXPORTING
            is_bustyp = gs_bustyp
          CHANGING
            cs_head   = gs_head.
      ENDIF.

      APPEND gs_head TO it_head.
      LOOP AT gt_item WHERE menge <> 0.
        APPEND gt_item TO it_item.
        CLEAR gt_item.
      ENDLOOP.

      PERFORM frm_print TABLES it_head it_item.

    WHEN '&SAVE'.
      CLEAR sy-ucomm.

      PERFORM frm_auth_bustyp_check USING '01' gs_head-bustyp g_werks CHANGING g_error .

      CHECK g_error IS INITIAL.

      PERFORM frm_before_save_data.

      PERFORM frm_check_head_data.

      PERFORM frm_check_item_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_zz_rkcj USING 'SAVE' 'X'.
      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_data.

      PERFORM frm_save_bc." 保存 条码相关信息

      PERFORM frm_save_batch_data.

      g_readonly = 'D'.

      PERFORM f_set_catalog_alv_500 .

      PERFORM frm_pop_msg.
    WHEN '&CJTS'.
      CLEAR sy-ucomm.
      IF gs_head-status = 'S' OR gs_head-status = 'T'.
        PERFORM frm_zz_rkcj USING 'DIS' ''.
      ELSE.
        PERFORM frm_zz_rkcj USING 'DIS' 'X'.
      ENDIF.

    WHEN '&TEST'.
*      PERFORM frm_zz_pack_del.
*      PERFORM frm_zz_pack_post." 装箱单
      PERFORM frm_zz_cjd_post.



    WHEN '&POST'.
      CLEAR sy-ucomm.

      PERFORM frm_auth_bustyp_check
        USING '10' gs_head-bustyp g_werks
        CHANGING g_error .
      CHECK g_error IS INITIAL.

      PERFORM frm_check_post.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_pop_get_info USING TEXT-039"'请确认过账日期跟抬头备注'.
                                CHANGING gs_head-budat gs_head-remark1.
      CHECK g_error <> 'X'.

      PERFORM frm_post.

      PERFORM frm_pop_msg.

    WHEN '&CANCEL'.
      CLEAR sy-ucomm.

      PERFORM frm_auth_bustyp_check USING '02' gs_head-bustyp g_werks CHANGING g_error .

      CHECK g_error IS INITIAL.

      PERFORM frm_check_cancel_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      gs_head-budat_re = sy-datum.

      PERFORM frm_pop_get_info USING TEXT-040 "'请确认冲销日期跟备注'.
                              CHANGING gs_head-budat_re gs_head-remark_re .

      CHECK g_error <> 'X'.

      PERFORM frm_cancel.

      PERFORM frm_delete_bc." 删除 条码相关信息

      PERFORM frm_pop_msg.

    WHEN '&DELETE'.
      CLEAR sy-ucomm.

      PERFORM frm_check_delete.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-038."'是否确认删除单据?'.

      CHECK g_error <> 'X'.

      PERFORM frm_delete_bc." 删除 条码相关信息

      PERFORM frm_delete_data.

  ENDCASE.
  PERFORM frm_after_command.
ENDMODULE.


MODULE user_command_0510 INPUT.
  CHECK sy-ucomm = '&OK'.
  PERFORM frm_check_ok_510.
ENDMODULE.


MODULE user_command_0600 INPUT.

  DATA: lv_msg TYPE char200.

  CALL METHOD g_grid_600_top->check_changed_data.
  CALL METHOD g_grid_600_down->check_changed_data.
  CALL METHOD g_grid_600_right->check_changed_data.

  PERFORM frm_before_command.

  CASE sy-ucomm.

    WHEN '&ENTR'.
      CLEAR sy-ucomm.

      PERFORM frm_call_transaction USING g_field.

    WHEN '&EDIT'.

      CLEAR sy-ucomm.

      PERFORM frm_set_edit_status_01 CHANGING g_refresh_catalog.

      IF g_refresh_catalog = 'X'.
        PERFORM f_set_catalog_alv_600 .
      ENDIF.

    WHEN '&LSAVE'.
      CLEAR sy-ucomm.

      PERFORM frm_auth_bustyp_check USING '01' gs_head-bustyp g_werks CHANGING g_error.

      CHECK g_error IS INITIAL.

      PERFORM frm_before_save_data.

      PERFORM frm_lsave_po_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_text.

      g_readonly = 'M'.
      PERFORM frm_pop_msg.

    WHEN '&SAVE'.
      CLEAR sy-ucomm.

      PERFORM frm_auth_bustyp_check  USING '01' gs_head-bustyp g_werks CHANGING g_error.

      CHECK g_error IS INITIAL.

      PERFORM frm_before_save_data.

      PERFORM frm_check_head_data.

      PERFORM frm_check_item_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_post.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_po_data.

      PERFORM frm_save_text.
      g_action = 'MOD'.
      g_readonly = 'M'.

      PERFORM frm_pop_msg.

    WHEN '&LIFNR_M'.
      CLEAR sy-ucomm.
      IF gs_head-status <> 'A'.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 000 '只有保存状态能变更供应商' '' '' ''.
        RETURN.
      ENDIF.

      DATA: lv_lifnr LIKE gs_head-lifnr.
      DATA: lv_afono LIKE gs_head-afono.

      DATA:lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

      PERFORM frm_pop_get_lifnr CHANGING lv_lifnr.

      CHECK lv_lifnr IS NOT INITIAL.

      lv_afono = gs_head-afono.

      CALL FUNCTION 'ZAFO_DELETE'
        EXPORTING
          i_afono   = lv_afono
*       IMPORTING
*         ES_HEAD   =
        TABLES
          et_return = lt_return
        EXCEPTIONS
          error     = 1
          OTHERS    = 2.
      LOOP AT lt_return WHERE type = 'E'.
        APPEND lt_return TO  ot_return.
        g_error = 'X'.
      ENDLOOP.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      CLEAR gs_head-afono.
      CLEAR gs_head-ebeln.

      PERFORM frm_move_head_to_item.

      PERFORM frm_check_head_data.

      PERFORM frm_check_item_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_post.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_po_data.

      PERFORM frm_save_text.
      g_action = 'DIS'.
      g_readonly = 'D'.

      PERFORM frm_pop_msg.

    WHEN '&COMMIT'.
      CLEAR sy-ucomm.
      CLEAR lv_msg.

      lv_msg = '是否确认提交审批?'.

      PERFORM frm_auth_bustyp_check USING '02' gs_head-bustyp g_werks CHANGING g_error .
      CHECK g_error IS INITIAL.

      PERFORM frm_before_save_data.

      PERFORM frm_check_head_data.

      PERFORM frm_check_item_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_post.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_po_data.

      PERFORM frm_save_text.

      PERFORM frm_check_commit CHANGING lv_msg.

      PERFORM frm_pop_confirm USING lv_msg.

      CHECK g_error <> 'X'.

      PERFORM frm_commit.
      g_action = 'DIS'.

    WHEN '&DELETE'.
      CLEAR sy-ucomm.

      PERFORM frm_check_delete.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-038."'是否确认删除单据?'.

      CHECK g_error <> 'X'.

      PERFORM frm_delete_po.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_delete_data.

      g_action = 'DIS'.

    WHEN '&GOTOAPP'.
      CLEAR sy-ucomm.

      PERFORM frm_goto_app.

    WHEN '&ZPRINT'.
      CLEAR sy-ucomm.

      CLEAR it_head[].
      CLEAR it_item[].
      APPEND gs_head TO it_head.
      it_item[] = gt_item[].

      PERFORM frm_print TABLES it_head it_item.

    WHEN '&TEST'.

      CLEAR gs_head-afono.
      CLEAR gs_head-ebeln.

      PERFORM frm_move_head_to_item.

      PERFORM frm_check_head_data.

      PERFORM frm_check_item_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_post.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_po_data.

      PERFORM frm_save_text.
      g_action = 'DIS'.
      g_readonly = 'D'.

      PERFORM frm_pop_msg.

  ENDCASE.
  PERFORM frm_after_command.
ENDMODULE.

MODULE user_command_0700 INPUT.
  CALL METHOD g_grid_700_top->check_changed_data.
  CALL METHOD g_grid_700_down->check_changed_data.

  PERFORM frm_before_command.
  CASE sy-ucomm.
    WHEN '&ENTR'.
      CLEAR sy-ucomm.
      PERFORM frm_call_transaction USING 'GS_HEAD-MBLNR'.

    WHEN '&EDIT'.
      CLEAR sy-ucomm.

      PERFORM frm_set_edit_status CHANGING g_refresh_catalog.

      IF g_refresh_catalog = 'X'.
        PERFORM f_set_catalog_alv_700 .
      ENDIF.

    WHEN '&SAVE'.
      CLEAR sy-ucomm.

      PERFORM frm_check_save.
      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_before_save_data.
      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_data.

      g_readonly = 'D'.

      PERFORM f_set_catalog_alv_700 .

      PERFORM frm_pop_msg.

    WHEN '&CJTS'.
      CLEAR sy-ucomm.
      IF gs_head-status = 'S' OR gs_head-status = 'T'.
        PERFORM frm_zz_rkcj USING 'DIS' ''.
      ELSE.
        PERFORM frm_zz_rkcj USING 'DIS' 'X'.
      ENDIF.

    WHEN '&POST'.
      CLEAR sy-ucomm.

      PERFORM frm_auth_bustyp_check
        USING '10' gs_head-bustyp g_werks
       CHANGING g_error .
      CHECK g_error IS INITIAL.

      PERFORM frm_check_post.
      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_zz_rkcj USING 'SAVE' 'X'.
      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_pop_get_info USING '请确认过账日期跟抬头备注'
                                CHANGING gs_head-budat gs_head-remark1 .

      CHECK g_error <> 'X'.
      PERFORM frm_post.

      PERFORM frm_pop_msg.

    WHEN '&CANCEL'.
      CLEAR sy-ucomm.

      PERFORM frm_auth_bustyp_check  USING '02' gs_head-bustyp g_werks  CHANGING g_error .

      CHECK g_error IS INITIAL.

      PERFORM frm_check_cancel_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      gs_head-budat_re = sy-datum.

      PERFORM frm_pop_get_info USING TEXT-040 " '请确认冲销日期跟备注'.
                              CHANGING gs_head-budat_re gs_head-remark_re.

      CHECK g_error <> 'X'.

      PERFORM frm_cancel.

      PERFORM frm_delete_bc." 删除 条码相关信息

      PERFORM frm_pop_msg.

    WHEN '&DELETE'.
      CLEAR sy-ucomm.

      PERFORM frm_check_delete.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-038."'是否确认删除单据?'.

      CHECK g_error <> 'X'.

      PERFORM frm_delete_bc." 删除 条码相关信息

      PERFORM frm_delete_data.

    WHEN '&PRINT'.
      CLEAR sy-ucomm.

      PERFORM frm_auth_bustyp_check USING '04' gs_head-bustyp g_werks CHANGING g_error .

      CHECK g_error IS INITIAL.
      CHECK gs_bustyp-print_type IS NOT INITIAL.

      READ TABLE gt_item INDEX 1.
      IF sy-subrc EQ 0 AND gt_item-werks IS NOT INITIAL..
        gs_head-werks = gt_item-werks.
      ENDIF.

      CLEAR it_head[].
      CLEAR it_item[].

      IF gs_bustyp-print_per IS NOT INITIAL AND gs_head-print_no IS INITIAL.
        CALL FUNCTION 'ZAFO_PRINT_GET_MB_NO'
          EXPORTING
            is_bustyp = gs_bustyp
          CHANGING
            cs_head   = gs_head.
      ENDIF.

      APPEND gs_head TO it_head.
      LOOP AT gt_item .
        CHECK gt_item-menge <> 0.
        APPEND gt_item TO it_item.
        CLEAR gt_item.
      ENDLOOP.

      PERFORM frm_print TABLES it_head it_item.


    WHEN '&TEST'.
      CLEAR sy-ucomm.
*     PERFORM frm_zz_pack_del.
      PERFORM frm_zz_pack_post.
  ENDCASE.

  PERFORM frm_after_command.

ENDMODULE.


MODULE user_command_0800 INPUT.
  CALL METHOD g_grid_800_left->check_changed_data.
  CALL METHOD g_grid_800_right->check_changed_data.

  PERFORM frm_before_command.

  CASE sy-ucomm.
    WHEN '&ENTR'.
      PERFORM frm_call_transaction USING g_field.

    WHEN '&EDIT'.
      CLEAR sy-ucomm.

      PERFORM frm_set_edit_status CHANGING g_refresh_catalog.

      IF g_refresh_catalog = 'X'.
        PERFORM f_set_catalog_alv_800 .
      ENDIF.

    WHEN '&SAVE'.
      CLEAR sy-ucomm.

      PERFORM frm_before_save_data.

      PERFORM frm_check_head_data.

      PERFORM frm_check_item_data.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.

        RETURN.
      ENDIF.

      PERFORM frm_move_head_to_item.

      IF gs_bustyp-execute_type = 'PR' OR gs_bustyp-execute_type = 'PRO'.
        PERFORM frm_post.

        IF g_error = 'X'.
          PERFORM frm_pop_msg.
          RETURN.
        ENDIF.
      ENDIF.

      PERFORM frm_save_data.
      PERFORM frm_save_data_serve.

      g_readonly = 'D'.

      PERFORM f_set_catalog_alv_800 .

      PERFORM frm_pop_msg.

    WHEN '&COMMIT'.
      CLEAR sy-ucomm.

      PERFORM frm_pop_confirm USING TEXT-036."'是否确认提交?'.

      CHECK g_error <> 'X'.

      PERFORM frm_commit.

    WHEN '&UNCOMMIT'.
      CLEAR sy-ucomm.

      PERFORM frm_pop_confirm USING TEXT-037." '是否确认取消审核?'.

      CHECK g_error <> 'X'.

      PERFORM frm_uncommit.

    WHEN '&DELETE'.
      CLEAR sy-ucomm.

      PERFORM frm_check_delete.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-038."'是否确认删除单据?'.

      CHECK g_error <> 'X'.

      PERFORM frm_delete_data.

    WHEN '&GOTOAPP'.
      CLEAR sy-ucomm.
      PERFORM frm_goto_app.
  ENDCASE.

  PERFORM frm_after_command.

ENDMODULE.


MODULE user_command_0900 INPUT.

  CALL METHOD g_grid_900->check_changed_data.

  PERFORM frm_before_command.

  CASE sy-ucomm.
    WHEN '&ENTR'.
      PERFORM frm_call_transaction USING g_field.

    WHEN '&EDIT'.
      CLEAR sy-ucomm.

      PERFORM frm_set_edit_status_02 CHANGING g_refresh_catalog.

      IF g_refresh_catalog = 'X'.
        PERFORM f_set_catalog_alv_900 .
      ENDIF.

    WHEN '&PRINT'.
      CLEAR it_head[].
      APPEND gs_head TO it_head.
      LOOP AT gt_item WHERE menge <> 0.
        APPEND gt_item TO it_item.
        CLEAR gt_item.
      ENDLOOP.
      PERFORM frm_print TABLES it_head it_item.

    WHEN '&SAVE'.
      CLEAR sy-ucomm.

      PERFORM frm_before_save_data.

      PERFORM frm_check_save.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_post.
      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_save_data.

      g_readonly = 'D'.

      PERFORM f_set_catalog_alv_900 .

      PERFORM frm_pop_msg.

    WHEN '&COMMIT'.
      CLEAR sy-ucomm.

      IF g_readonly NE 'D'.
        MESSAGE s122."请先保存数据!
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-036."'是否确认提交?'.

      CHECK g_error <> 'X'.

      PERFORM frm_commit.

    WHEN '&UNCOMMIT'.

      CLEAR sy-ucomm.

      PERFORM frm_pop_confirm USING TEXT-037."'是否确认取消审核?'.

      CHECK g_error <> 'X'.

      PERFORM frm_uncommit.

    WHEN '&TEST'.

    WHEN '&DELETE'.
      CLEAR sy-ucomm.

      PERFORM frm_check_delete.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_pop_confirm USING TEXT-038."'是否确认删除单据?'.

      CHECK g_error <> 'X'.

      PERFORM frm_delete_sd.

      IF g_error = 'X'.
        PERFORM frm_pop_msg.
        RETURN.
      ENDIF.

      PERFORM frm_delete_data.

    WHEN '&GOTOAPP'.
      CLEAR sy-ucomm.

      PERFORM frm_goto_app.

  ENDCASE.

  PERFORM frm_after_command.
ENDMODULE.
