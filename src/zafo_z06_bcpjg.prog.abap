*&---------------------------------------------------------------------*
*& 包含               ZAFO_Z06_BCPJG
*&---------------------------------------------------------------------*
" 半成品加工

" 半成品委外组件原材料计划数量反算
FORM frm_get_cpt_jhsfp.
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:lv_menge_fl TYPE menge_d.
  DATA:lv_menge_sh TYPE menge_d.

  CHECK g_error NE 'X'.
  CHECK g_bustyp = 'R1011' OR g_bustyp = 'R1020'.

  lt_item[] = gt_item[].

  DELETE lt_item[] WHERE menge <= 0.

  CHECK lt_item[] IS NOT INITIAL.

  " 系统组件
  SELECT * INTO TABLE @DATA(lt_resb)
  FROM resb
  FOR ALL ENTRIES IN @lt_item
  WHERE ebeln = @lt_item-ebeln
    AND ebelp = @lt_item-ebelp .

  " 原材料计划发料
  SELECT * INTO TABLE @DATA(lt_afo_item_po_cpt)
    FROM zafo_item_po_cpt
    FOR ALL ENTRIES IN @lt_item
    WHERE ebeln = @lt_item-ebeln
      AND ebelp = @lt_item-ebelp.

  " 原材料实际发料
  SELECT i~afono,i~afonr,i~ebeln,i~ebelp,i~matnr,i~charg,i~menge
    INTO TABLE @DATA(lt_fls)
    FROM zafo_item AS i
    INNER JOIN zafo_head AS h ON h~afono = i~afono
   FOR ALL ENTRIES IN @lt_item
    WHERE h~status IN ( 'S','T' )
      AND h~bustyp = 'R1012'
      AND i~del_flag <> 'X'
      AND i~ebeln = @lt_item-ebeln
      AND i~ebelp = @lt_item-ebelp.

  " 半成品计划收货数
  SELECT * INTO TABLE @DATA(lt_zafo_item_po)
    FROM zafo_item_po
    FOR ALL ENTRIES IN @lt_item
    WHERE ebeln = @lt_item-ebeln
      AND ebelp = @lt_item-ebelp.

  " 半成品实际收货数
  SELECT i~afono,i~afonr,i~ebeln,i~ebelp,i~menge
    INTO TABLE @DATA(lt_shs)
    FROM zafo_item AS i
    INNER JOIN zafo_head AS h ON h~afono = i~afono
   FOR ALL ENTRIES IN @lt_item
    WHERE h~status IN ( 'S','T' )
      AND h~bustyp in ( 'R1011' , 'R1020' )
      AND i~del_flag <> 'X'
      AND i~ebeln = @lt_item-ebeln
      AND i~ebelp = @lt_item-ebelp.

  LOOP AT lt_item.
    READ TABLE lt_zafo_item_po INTO DATA(ls_zafo_item_po)
                                WITH KEY ebeln = lt_item-ebeln
                                         ebelp = lt_item-ebelp.
    IF sy-subrc <> 0.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 000 '半成品明细不存在！' '' '' ''.
      RETURN.
    ENDIF.

    CLEAR: lv_menge_sh.
    lv_menge_sh = lt_item-menge.
    LOOP AT lt_shs INTO DATA(ls_shs) WHERE ebeln = lt_item-ebeln
                                       AND ebelp = lt_item-ebelp.
      lv_menge_sh = lv_menge_sh + ls_shs-menge.
    ENDLOOP.

    IF lv_menge_sh <= 0 .
      PERFORM frm_add_msg USING 'E' 'ZAFO' 000 '半成品收货数量不存在！' '' '' ''.
      RETURN.
    ENDIF.


    LOOP AT lt_afo_item_po_cpt ASSIGNING FIELD-SYMBOL(<ls_po_cpt>) " 循环组件
                                                WHERE ebeln = lt_item-ebeln
                                                  AND ebelp = lt_item-ebelp.
      CLEAR: lv_menge_fl.
      LOOP AT lt_fls INTO DATA(ls_fls) WHERE ebeln = <ls_po_cpt>-ebeln
                                         AND ebelp = <ls_po_cpt>-ebelp
                                         AND matnr = <ls_po_cpt>-matnr
                                         AND charg = <ls_po_cpt>-charg.
        lv_menge_fl = lv_menge_fl + ls_fls-menge.
      ENDLOOP.
      IF lv_menge_fl <= 0 .
        PERFORM frm_add_msg USING 'E' 'ZAFO' 000 '原材料发料数量不存在！' '' '' ''.
        RETURN.
      ENDIF.

      READ TABLE lt_resb ASSIGNING FIELD-SYMBOL(<fs_resb>)
                                        WITH KEY ebeln = <ls_po_cpt>-ebeln
                                                 ebelp = <ls_po_cpt>-ebelp
                                                 matnr = <ls_po_cpt>-matnr
                                                 charg = <ls_po_cpt>-charg.
      IF sy-subrc <> 0.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 000 '原材料系统组件不存在！' '' '' ''.
        RETURN.
      ENDIF.

      IF lv_menge_sh >= ls_zafo_item_po-menge." 实际收货数大于等于计划收货数数时，重算计划发料数
        " 计划原材料发料数 = 实际原材料发料数 * 计划半成品收货数 / 实际半成品收货数
        <ls_po_cpt>-menge = lv_menge_fl * ls_zafo_item_po-menge / lv_menge_sh.
      ENDIF.

      <fs_resb>-erfmg = <ls_po_cpt>-menge.
      <fs_resb>-bdmng = <ls_po_cpt>-menge.
    ENDLOOP.
    IF sy-subrc <> 0.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 000 '原材料组件不存在！' '' '' ''.
      RETURN.
    ENDIF.

  ENDLOOP.

  MODIFY zafo_item_po_cpt FROM TABLE lt_afo_item_po_cpt.
  MODIFY resb FROM TABLE lt_resb.
  COMMIT WORK AND WAIT.

ENDFORM.


* 批量添加原材料
FORM frm_get_cpt_ycl.
  DATA: lt_ret_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  SELECT zppdhd,
         zzbom_item,
         idnlf,
         matnr,
         maktx AS maktx_zh,
         zcolor1 AS zcolor,
         zsize,
         zzjsssgg AS znorms,
         zcqy AS zppflag,
         zzdh AS menge,
         meins
    FROM ztpp0091
    FOR ALL ENTRIES IN @gt_item
    WHERE zppdhd = @gt_item-zppdhd
      AND matnr <> @gt_item-matnr
      AND zcate1 IN ( 'FA', 'AC' )
      AND zzwlly = ''
      AND zzdh > 0
    INTO TABLE @DATA(lt_data).

  SORT lt_data BY maktx_zh zcolor zsize znorms zppdhd zppflag .

  DATA:lv_zzbom_item TYPE rspos.
  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    READ TABLE gt_item_po_cpt TRANSPORTING NO FIELDS
                                    WITH KEY zppdhd = <fs_data>-zppdhd
                                              matnr = <fs_data>-matnr
                                             zcolor = <fs_data>-zcolor
                                              zsize = <fs_data>-zsize
                                             znorms = <fs_data>-znorms
                                            zppflag = <fs_data>-zppflag.
    IF sy-subrc = 0.
      <fs_data>-zzbom_item = 0.
    ELSE.
      ADD 1 TO lv_zzbom_item.
      <fs_data>-zzbom_item = lv_zzbom_item.
    ENDIF.
  ENDLOOP.

  DELETE lt_data WHERE zzbom_item = 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZZBOM_ITEM'
      pvalkey         = 'ZZBOM_ITEM'
      dynprofield     = 'ZZBOM_ITEM'
      value_org       = 'S'
      multiple_choice = 'X'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
*     callback_program = sy-repid
*     callback_form   = 'FRM_RETURN_VALUE'
    TABLES
      value_tab       = lt_data
      return_tab      = lt_ret_tab[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0 .

    DATA:lt_ycl LIKE TABLE OF gt_item_po_cpt WITH HEADER LINE.
    REFRESH lt_ycl.

    LOOP AT lt_ret_tab.
      READ TABLE lt_data INTO DATA(ls_data) WITH KEY zzbom_item = lt_ret_tab-fieldval.
      IF sy-subrc EQ 0 .
        MOVE-CORRESPONDING ls_data TO lt_ycl.
        APPEND lt_ycl.
      ENDIF.
    ENDLOOP.


*      PERFORM frm_get_cpt_ycl_same TABLES lt_ycl.

    PERFORM frm_get_cpt_ycl_diff TABLES lt_ycl.

  ENDIF.
ENDFORM.

* 批量添加原材料（半成品和材料不同颜色）
FORM frm_get_cpt_ycl_diff TABLES lt_ycl STRUCTURE gt_item_po_cpt.

  DATA:lt_sum_item_po LIKE TABLE OF gt_item_po WITH HEADER LINE.

  LOOP AT gt_item_po." 多行半成品使用同一原材料情况（比如半成品分尺码，材料不分尺码）
    lt_sum_item_po-zppdhd = gt_item_po-zppdhd.
    lt_sum_item_po-matnr  = gt_item_po-matnr.
    lt_sum_item_po-zcolor = gt_item_po-zcolor.
    lt_sum_item_po-ktmng  = gt_item_po-ktmng.
    lt_sum_item_po-menge  = gt_item_po-menge.
    COLLECT lt_sum_item_po.
  ENDLOOP.

  DATA:lv_index TYPE rspos.

  LOOP AT gt_item_po_cpt.
    IF lv_index < gt_item_po_cpt-rspos.
      lv_index = gt_item_po_cpt-rspos.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_item_po.

    LOOP AT lt_ycl WHERE zppdhd = gt_item_po-zppdhd
                     AND zppflag = gt_item_po-zppflag.

      MOVE-CORRESPONDING gt_item_po TO gt_item_po_cpt.

      ADD 1 TO lv_index.
      gt_item_po_cpt-rspos = lv_index.
      gt_item_po_cpt-matnr = lt_ycl-matnr.
      gt_item_po_cpt-idnlf = lt_ycl-idnlf.
      gt_item_po_cpt-maktx_zh = lt_ycl-maktx_zh.
      gt_item_po_cpt-zcolor = lt_ycl-zcolor.
      gt_item_po_cpt-zsize = lt_ycl-zsize.
      gt_item_po_cpt-znorms = lt_ycl-znorms.
      gt_item_po_cpt-zppflag = lt_ycl-zppflag.

      READ TABLE lt_sum_item_po WITH KEY zppdhd  = gt_item_po-zppdhd
                                         matnr   = gt_item_po-matnr
                                         zcolor  = gt_item_po-zcolor
                                         zppflag = gt_item_po-zppflag.
      IF sy-subrc = 0.
        gt_item_po_cpt-ktmng = lt_ycl-menge * gt_item_po-ktmng / lt_sum_item_po-ktmng.
        gt_item_po_cpt-menge = lt_ycl-menge * gt_item_po-menge / lt_sum_item_po-menge.
      ENDIF.

      gt_item_po_cpt-meins = lt_ycl-meins.

      SELECT SINGLE zcolor_text
        INTO gt_item_po_cpt-zcolor_text
        FROM zvcolor
        WHERE zcolor EQ gt_item_po_cpt-zcolor
          AND spras = '1'.
      PERFORM frm_get_cpt_charg USING gt_item_po_cpt.
      APPEND gt_item_po_cpt.

    ENDLOOP.

  ENDLOOP.

ENDFORM.

* 批量添加原材料（半成品和材料同颜色）
FORM frm_get_cpt_ycl_same TABLES lt_ycl STRUCTURE gt_item_po_cpt.

  DATA:lt_sum_item_po LIKE TABLE OF gt_item_po WITH HEADER LINE.

  LOOP AT gt_item_po." 多行半成品使用同一原材料情况（比如半成品分尺码，材料不分尺码）
    lt_sum_item_po-zppdhd = gt_item_po-zppdhd.
    lt_sum_item_po-zcolor = gt_item_po-zcolor.
    lt_sum_item_po-zppflag = gt_item_po-zppflag.
    lt_sum_item_po-ktmng = gt_item_po-ktmng.
    lt_sum_item_po-menge = gt_item_po-menge.
    COLLECT lt_sum_item_po.
  ENDLOOP.

  DATA:lv_index TYPE rspos.

  LOOP AT gt_item_po_cpt.
    IF lv_index < gt_item_po_cpt-rspos.
      lv_index = gt_item_po_cpt-rspos.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_item_po.
    LOOP AT lt_ycl WHERE zppdhd = gt_item_po-zppdhd
                      AND zcolor = gt_item_po-zcolor
                      AND zppflag = gt_item_po-zppflag.

      MOVE-CORRESPONDING gt_item_po TO gt_item_po_cpt.

      ADD 1 TO lv_index.
      gt_item_po_cpt-rspos = lv_index.
      gt_item_po_cpt-matnr = lt_ycl-matnr.
      gt_item_po_cpt-idnlf = lt_ycl-idnlf.
      gt_item_po_cpt-maktx_zh = lt_ycl-maktx_zh.
      gt_item_po_cpt-zcolor = lt_ycl-zcolor.
      gt_item_po_cpt-zsize = lt_ycl-zsize.
      gt_item_po_cpt-znorms = lt_ycl-znorms.
      gt_item_po_cpt-zppflag = lt_ycl-zppflag.

      READ TABLE lt_sum_item_po WITH KEY zppdhd = gt_item_po-zppdhd
                                         zcolor = gt_item_po-zcolor
                                         zppflag = gt_item_po-zppflag.
      IF sy-subrc = 0.
        gt_item_po_cpt-ktmng = lt_ycl-menge * gt_item_po-ktmng / lt_sum_item_po-ktmng.
        gt_item_po_cpt-menge = lt_ycl-menge * gt_item_po-menge / lt_sum_item_po-menge.
      ENDIF.

      gt_item_po_cpt-meins = lt_ycl-meins.

      SELECT SINGLE zcolor_text
        INTO gt_item_po_cpt-zcolor_text
        FROM zvcolor
        WHERE zcolor EQ gt_item_po_cpt-zcolor
          AND spras = '1'.

      PERFORM frm_get_cpt_charg USING gt_item_po_cpt.
      APPEND gt_item_po_cpt.
    ENDLOOP.
  ENDLOOP.

ENDFORM.


FORM frm_get_component_cpt TABLES pt_pocomponents STRUCTURE bapimepocomponent
                                  pt_pocomponentsx STRUCTURE bapimepocomponentx
                            USING ps_item_po   STRUCTURE zafo_sitem_po.
  DATA:lt_resb TYPE TABLE OF resb WITH HEADER LINE.

  IF ps_item_po-ebeln IS NOT INITIAL AND ps_item_po-ebelp IS NOT INITIAL.
    DELETE FROM resb WHERE ebeln = ps_item_po-ebeln AND ebelp = ps_item_po-ebelp.
    COMMIT WORK AND WAIT.
  ENDIF.

  LOOP AT gt_item_po_cpt WHERE afono = ps_item_po-afono AND afonr = ps_item_po-afonr.

    pt_pocomponents-po_item        = gt_item_po_cpt-ebelp.
    pt_pocomponents-sched_line     = 1.
    pt_pocomponents-item_no        = gt_item_po_cpt-rspos.
    pt_pocomponents-item_cat       = 'L'.
    pt_pocomponents-material       = gt_item_po_cpt-matnr.
    pt_pocomponents-entry_uom      = gt_item_po_cpt-meins.
    pt_pocomponents-entry_quantity = gt_item_po_cpt-menge.
    pt_pocomponents-plant          = gt_item_po_cpt-werks.
    pt_pocomponents-batch          = gt_item_po_cpt-charg.
    pt_pocomponents-change_id      = 'I'.

    APPEND pt_pocomponents.
    CLEAR pt_pocomponents.

    pt_pocomponentsx-po_item        = gt_item_po_cpt-ebelp.
    pt_pocomponentsx-sched_line     = 1.
    pt_pocomponentsx-item_no        = gt_item_po_cpt-rspos.
    pt_pocomponentsx-item_nox       = abap_true.
    pt_pocomponentsx-po_itemx       = abap_true.
    pt_pocomponentsx-item_cat       = abap_true.
    pt_pocomponentsx-sched_linex    = abap_true.
    pt_pocomponentsx-material       = abap_true.
    pt_pocomponentsx-entry_quantity = abap_true.
    pt_pocomponentsx-entry_uom      = abap_true.
    pt_pocomponentsx-batch          = abap_true.
    pt_pocomponentsx-plant          = abap_true.
    pt_pocomponentsx-change_id      = abap_true.
    APPEND pt_pocomponentsx.
    CLEAR pt_pocomponentsx.
  ENDLOOP.

ENDFORM.


FORM frm_get_po_charg USING ps_item_po STRUCTURE zafo_sitem_po CHANGING cv_charg.
  DATA: i_commit TYPE char1,
        cs_zmch1 TYPE zmch1.

  DATA:lv_subrca  TYPE sy-subrc.
  DATA:lv_subrc1  TYPE sy-subrc.
  DATA:ls_charg TYPE  bapibatchkey.

  i_commit = 'X'.

  cs_zmch1-zcolor   = ps_item_po-zcolor.
  cs_zmch1-zsize    = ps_item_po-zsize.
  cs_zmch1-znorms   = ps_item_po-znorms.
  CALL FUNCTION 'ZAFO_GET_CHARG'
    EXPORTING
      i_commit = i_commit
    CHANGING
      cs_zmch1 = cs_zmch1.

  DATA:lt_charg_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
  CALL FUNCTION 'VB_BATCH_READ_BUFFER_DB'
    EXPORTING
      matnr      = ps_item_po-matnr
      charg      = cs_zmch1-charg
      werks      = ps_item_po-werks
    IMPORTING
      rc_mcha_db = lv_subrca
      rc_mch1_db = lv_subrc1.
  IF sy-subrc <> 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 131 '' '' '' ''."材料批次生成失败！
    RETURN.
  ENDIF.

  IF lv_subrca = 4  OR lv_subrc1 = 4.
    ls_charg-material = ps_item_po-matnr.
    ls_charg-batch = cs_zmch1-charg.
    ls_charg-plant = ps_item_po-werks.

    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING
        material = ls_charg-material
        batch    = ls_charg-batch
        plant    = ls_charg-plant
      TABLES
        return   = lt_charg_return.
    IF sy-subrc <> 0 .
      PERFORM frm_add_msg USING 'E' 'ZAFO' 131 '' '' '' ''."材料批次生成失败！
      RETURN.
    ENDIF.

    DELETE lt_charg_return WHERE id  ='M7' AND number = 838.

    READ TABLE lt_charg_return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 131 lt_charg_return-message '' '' ''."材料批次生成失败！
      RETURN.
    ENDIF.
  ENDIF.

  cv_charg = cs_zmch1-charg.
ENDFORM.


FORM frm_get_cpt_charg USING ps_item_po_cpt STRUCTURE zafo_sitem_po_cpt.
  DATA: i_commit TYPE char1,
        cs_zmch1 TYPE zmch1.
  i_commit = 'X'.
  cs_zmch1-zcolor   = ps_item_po_cpt-zcolor.
  cs_zmch1-zsize    = ps_item_po_cpt-zsize.
  cs_zmch1-znorms   = ps_item_po_cpt-znorms.
  CALL FUNCTION 'ZAFO_GET_CHARG'
    EXPORTING
      i_commit = i_commit
    CHANGING
      cs_zmch1 = cs_zmch1.

  DATA:lv_subrca  TYPE sy-subrc.
  DATA:lv_subrc1  TYPE sy-subrc.
  DATA:ls_charg TYPE  bapibatchkey.

  DATA:lt_charg_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'VB_BATCH_READ_BUFFER_DB'
    EXPORTING
      matnr      = ps_item_po_cpt-matnr
      charg      = cs_zmch1-charg
      werks      = ps_item_po_cpt-werks
    IMPORTING
      rc_mcha_db = lv_subrca
      rc_mch1_db = lv_subrc1.
  IF sy-subrc <> 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 125 '' '' '' ''."材料批次生成失败！
    RETURN.
  ENDIF.

  IF lv_subrca = 4  OR lv_subrc1 = 4.
    ls_charg-material = ps_item_po_cpt-matnr.
    ls_charg-batch = cs_zmch1-charg.
    ls_charg-plant = ps_item_po_cpt-werks.

    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING
        material = ls_charg-material
        batch    = ls_charg-batch
        plant    = ls_charg-plant
      TABLES
        return   = lt_charg_return.
    IF sy-subrc <> 0 .
      PERFORM frm_add_msg USING 'E' 'ZAFO' 125 '' '' '' ''."材料批次生成失败！
      RETURN.
    ENDIF.

    DELETE lt_charg_return WHERE id  ='M7' AND number = 838.

    READ TABLE lt_charg_return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      PERFORM frm_add_msg USING 'E' 'ZAFO' 125 lt_charg_return-message '' '' ''."材料批次生成失败！
      RETURN.
    ENDIF.
  ENDIF.

  ps_item_po_cpt-charg = cs_zmch1-charg.

ENDFORM.


FORM frm_get_component TABLES pt_pocomponents STRUCTURE bapimepocomponent
                                pt_pocomponentsx STRUCTURE bapimepocomponentx
                          USING ps_item_po STRUCTURE zafo_sitem_po.

  DATA:lv_tabix TYPE i.

  DATA: et_output1 TYPE STANDARD TABLE OF zspp_bom111 WITH HEADER LINE,
        ls_parent  TYPE zspp_bom111,
        i_zppdhd   TYPE zppdhd,
        i_module   TYPE char01,
        i_commit   TYPE char1,
        cs_zmch1   TYPE zmch1.

  CHECK ps_item_po-zppdhd IS NOT INITIAL.
  i_zppdhd = ps_item_po-zppdhd.
  i_module = 'A'.
  REFRESH et_output1.
  CALL FUNCTION 'ZPP_BOM_SUM_NOSUM'
    EXPORTING
      i_zppdhd   = i_zppdhd
      i_module   = i_module
    TABLES
      et_output1 = et_output1.

  "获取销售订单库存，如果需求数超出库存数在范围在5%以内，以库存数量代替需求数量
  SELECT * INTO TABLE @DATA(lt_mska) FROM mska WHERE matnr = @ps_item_po-matnr AND
                                                     werks = @ps_item_po-werks AND
                                                     lgort = @ps_item_po-lgort AND
                                                     vbeln = @ps_item_po-vbeln_va AND
                                                     posnr = @ps_item_po-posnr_va.

  READ TABLE et_output1 INTO ls_parent WITH KEY  matnr     = ps_item_po-matnr
                                                 zcolor1   = ps_item_po-zcolor
                                                 zsize     = ps_item_po-zsize
                                                 zzjsssgg  = ps_item_po-znorms
                                                 zcqy      = ps_item_po-zppflag
                                                 zzwlly    = 'J'.
  CHECK sy-subrc = 0 AND ls_parent-zzdh > 0.


  LOOP AT et_output1 INTO DATA(ls_out) WHERE zcolor1 = ls_parent-zcolor1 AND
                                             zbgbs   = ls_parent-zbgbs   AND
                                             zcqy    = ls_parent-zcqy    AND
                                             zzwlly  = ''.

    lv_tabix = lv_tabix + 1.

    pt_pocomponents-po_item        = ps_item_po-ebelp.
    pt_pocomponents-sched_line     = 1.
    pt_pocomponents-item_no        = lv_tabix.
    pt_pocomponents-item_cat       = 'L'.
    pt_pocomponents-material       = ls_out-matnr.
    pt_pocomponents-entry_quantity = ls_out-zzdh * ps_item_po-menge / ls_parent-zzdh."按比例
    pt_pocomponents-plant          = ps_item_po-werks.

    SELECT SINGLE meins INTO pt_pocomponents-entry_uom FROM mara WHERE matnr = ls_out-matnr.
    SELECT COUNT(*) FROM marc WHERE matnr = ls_out-matnr AND werks = ps_item_po-werks AND xchpf = 'X'.
    IF sy-subrc IS INITIAL.

      i_commit = 'X'.
      cs_zmch1-zcolor   = ls_out-zcolor1.
      cs_zmch1-zsize    = ls_out-zsize.
      cs_zmch1-znorms   = ls_out-zzjsssgg.
*      cs_zmch1-zppflag  = ls_out-zcqy
      .


      CALL FUNCTION 'ZAFO_GET_CHARG'
        EXPORTING
          i_commit = i_commit
        CHANGING
          cs_zmch1 = cs_zmch1.

      DATA:lv_subrca  TYPE sy-subrc.
      DATA:lv_subrc1  TYPE sy-subrc.
      DATA:ls_charg TYPE  bapibatchkey.

      DATA:lt_charg_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
      CALL FUNCTION 'VB_BATCH_READ_BUFFER_DB'
        EXPORTING
          matnr      = ls_out-matnr
          charg      = cs_zmch1-charg
          werks      = pt_pocomponents-plant
        IMPORTING
          rc_mcha_db = lv_subrca
          rc_mch1_db = lv_subrc1.
      IF sy-subrc <> 0.
        REFRESH pt_pocomponents.
        PERFORM frm_add_msg USING 'E' 'ZAFO' 125 '' '' '' ''."材料批次生成失败！
        RETURN.
      ENDIF.

      IF lv_subrca = 4  OR lv_subrc1 = 4.
        ls_charg-material = ls_out-matnr.
        ls_charg-batch = cs_zmch1-charg.
        ls_charg-plant = pt_pocomponents-plant.

        CALL FUNCTION 'BAPI_BATCH_CREATE'
          EXPORTING
            material = ls_charg-material
            batch    = ls_charg-batch
            plant    = ls_charg-plant
          TABLES
            return   = lt_charg_return.
        IF sy-subrc <> 0 .
          REFRESH pt_pocomponents.
          PERFORM frm_add_msg USING 'E' 'ZAFO' 125 '' '' '' ''."材料批次生成失败！
          RETURN.
        ENDIF.

        DELETE lt_charg_return WHERE id  ='M7' AND number = 838.

        READ TABLE lt_charg_return WITH KEY type = 'E'.
        IF sy-subrc EQ 0.
          REFRESH pt_pocomponents.
          PERFORM frm_add_msg USING 'E' 'ZAFO' 125 lt_charg_return-message '' '' ''."材料批次生成失败！
          RETURN.
        ENDIF.
      ENDIF.

      pt_pocomponents-batch          = cs_zmch1-charg.
      pt_pocomponentsx-batch         = abap_true.


      READ TABLE lt_mska INTO DATA(ls_mska) WITH KEY charg = cs_zmch1-charg.
      IF sy-subrc = 0 AND pt_pocomponents-entry_quantity NE 0 AND pt_pocomponents-entry_quantity > ls_mska-kalab.
        "如果需求数超出库存数在范围在5%以内，以库存数量代替需求数量
        IF ( pt_pocomponents-entry_quantity - ls_mska-kalab ) / pt_pocomponents-entry_quantity * 100 LE 5.
          pt_pocomponents-entry_quantity = ls_mska-kalab.
        ENDIF.
      ENDIF.
    ENDIF.

    pt_pocomponents-change_id      = 'U'.
    APPEND pt_pocomponents.
    CLEAR pt_pocomponents.

    pt_pocomponentsx-po_item        = ps_item_po-ebelp.
    pt_pocomponentsx-sched_line     = 1.
    pt_pocomponentsx-item_no        = lv_tabix.
    pt_pocomponentsx-item_nox       = abap_true.
    pt_pocomponentsx-po_itemx       = abap_true.
    pt_pocomponentsx-item_cat       = abap_true.
    pt_pocomponentsx-sched_linex    = abap_true.
    pt_pocomponentsx-material       = abap_true.
    pt_pocomponentsx-entry_quantity = abap_true.
    pt_pocomponentsx-entry_uom      = abap_true.
    pt_pocomponentsx-plant          = abap_true.
    pt_pocomponentsx-change_id      = abap_true.

    APPEND pt_pocomponentsx.
    CLEAR pt_pocomponentsx.

  ENDLOOP.

ENDFORM.


FORM frm_mb_post_ww .
  DATA: ls_mb_head LIKE bapi2017_gm_head_01,
        ls_mb_item TYPE bapi2017_gm_item_create,
        lt_mb_item TYPE TABLE OF bapi2017_gm_item_create,
        mb_gm_code LIKE bapi2017_gm_code,
        lt_return  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA:ls_afonr TYPE zafonr.

  DATA : l_mat_doc   LIKE bapi2017_gm_head_ret-mat_doc.
  DATA : l_doc_year LIKE bapi2017_gm_head_ret-doc_year.

  CHECK g_error NE 'X'.
  CHECK g_bustyp = 'R1012'.


  CLEAR ls_mb_head.
  ls_mb_head-pstng_date = gs_head-budat. "凭证中的过帐日期
  ls_mb_head-doc_date =  gs_head-bldat. " 凭证中的凭证日期
  ls_mb_head-pr_uname = sy-uname."用户名
  ls_mb_head-ref_doc_no = gs_head-afono."参考凭证编号
  ls_mb_head-header_txt = gs_head-remark1."参考描述
  mb_gm_code = '04'.

  CLEAR lt_mb_item.
  CLEAR ls_afonr.

  LOOP AT gt_item INTO gs_item WHERE menge <> ''.
    ls_mb_item-material          = gs_item-matnr .         "article物料号
    ls_mb_item-material_external = gs_item-matnr.         "物料号
    ls_mb_item-move_mat_external = gs_item-matnr.         "物料号
    ls_mb_item-move_type         =  '411'.           "移动类型（销售订单库存到非限制库存）
    ls_mb_item-spec_stock        =  'E' .            "特殊库存标识：现有订单
    ls_mb_item-move_reas         = '4005'.          "移动原因
    ls_mb_item-material_long     = gs_item-matnr.         "物料号
    ls_mb_item-plant             = gs_item-werks .         "工厂
    ls_mb_item-stge_loc          = gs_item-lgort .         "库存地点
    ls_mb_item-batch             = gs_item-charg .       "批次
    ls_mb_item-move_mat          = gs_item-matnr.        "接收/发出物料
    ls_mb_item-move_mat_long     = gs_item-matnr.        "接收/发出物料
    ls_mb_item-move_plant        = gs_item-werks.        "收货/发货工厂
    ls_mb_item-move_stloc        = gs_item-lgort .        "收货/发货库存地点
    ls_mb_item-move_batch        = gs_item-charg .        "批次
    ls_mb_item-entry_qnt         = gs_item-menge.      "调拨数量
    ls_mb_item-entry_uom         = gs_item-meins.       "条目单位
    ls_mb_item-val_sales_ord     = gs_item-vbeln_va.  "评估销售订单库存的销售订单号----从
    ls_mb_item-val_s_ord_item    = gs_item-posnr_va.  "评估销售订单库存的销售订单项目

    APPEND ls_mb_item TO lt_mb_item.
    CLEAR ls_mb_item.

  ENDLOOP.

  IF lt_mb_item[] IS INITIAL.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 078 '' '' '' ''."无有效数据行，无法过账
    RETURN.
  ENDIF.



  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_mb_head
      goodsmvt_code    = mb_gm_code
    IMPORTING
      materialdocument = l_mat_doc
      matdocumentyear  = l_doc_year
    TABLES
      goodsmvt_item    = lt_mb_item
      return           = lt_return.

  IF sy-subrc = 0 AND l_mat_doc IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    gs_head-belnr = l_mat_doc."用来存储转非限制库存生成的凭证号
    gs_head-mjahr = l_doc_year.
    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    PERFORM frm_db_modify.
    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.

  ENDIF.
ENDFORM.


FORM frm_mb_post_ww_rec .

  DATA: ls_mb_head LIKE bapi2017_gm_head_01,
        ls_mb_item TYPE bapi2017_gm_item_create,
        lt_mb_item TYPE TABLE OF bapi2017_gm_item_create,
        mb_gm_code LIKE bapi2017_gm_code,
        lt_return  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA:ls_afonr TYPE zafonr.

  DATA : l_mat_doc   LIKE bapi2017_gm_head_ret-mat_doc.
  DATA : l_doc_year LIKE bapi2017_gm_head_ret-doc_year.

  CHECK g_error NE 'X'.
  CHECK g_bustyp = 'R1011' OR g_bustyp = 'R1020'..
  CLEAR ls_mb_head.
  ls_mb_head-pstng_date = gs_head-budat. "凭证中的过帐日期
  ls_mb_head-doc_date =  gs_head-bldat. " 凭证中的凭证日期
  ls_mb_head-pr_uname = sy-uname."用户名
  ls_mb_head-ref_doc_no = gs_head-afono."参考凭证编号
  ls_mb_head-header_txt = gs_head-remark1."参考描述
  mb_gm_code = '04'.

  CLEAR lt_mb_item.
  CLEAR ls_afonr.

  LOOP AT gt_item INTO gs_item WHERE menge <> ''.
    ls_mb_item-material          = gs_item-matnr .         "article物料号
    ls_mb_item-material_external = gs_item-matnr.         "物料号
    ls_mb_item-move_mat_external = gs_item-matnr.         "物料号
    ls_mb_item-move_type         =  '413'.           "移动类型（销售订单库存到非限制库存）
*    ls_mb_item-spec_stock        =  'E' .            "特殊库存标识
    ls_mb_item-move_reas         = '4006'.          "移动原因
    ls_mb_item-material_long     = gs_item-matnr.         "物料号
    ls_mb_item-plant             = gs_item-werks .         "工厂
    ls_mb_item-stge_loc          = gs_item-lgort .         "库存地点
    ls_mb_item-batch             = gs_item-charg .       "批次
    ls_mb_item-move_mat          = gs_item-matnr.        "接收/发出物料
    ls_mb_item-move_mat_long     = gs_item-matnr.        "接收/发出物料
    ls_mb_item-move_plant        = gs_item-werks.        "收货/发货工厂
    ls_mb_item-move_stloc        = gs_item-lgort .        "收货/发货库存地点
    ls_mb_item-move_batch        = gs_item-charg .        "批次
    ls_mb_item-entry_qnt         = gs_item-menge.      "调拨数量
    ls_mb_item-entry_uom         = gs_item-meins.       "条目单位
    ls_mb_item-sales_ord         = gs_item-vbeln_va.  "评估销售订单库存的销售订单号----到
    ls_mb_item-s_ord_item        = gs_item-posnr_va.  "评估销售订单库存的销售订单项目---到
*    ls_mb_item-val_sales_ord     = gs_item-vbeln_va.  "评估销售订单库存的销售订单号----从
*    ls_mb_item-val_s_ord_item    = gs_item-posnr_va.  "评估销售订单库存的销售订单项目---从

    APPEND ls_mb_item TO lt_mb_item.
    CLEAR ls_mb_item.

  ENDLOOP.

  IF lt_mb_item[] IS INITIAL.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 078 '' '' '' ''."无有效数据行，无法过账
    RETURN.
  ENDIF.


  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_mb_head
      goodsmvt_code    = mb_gm_code
    IMPORTING
      materialdocument = l_mat_doc
      matdocumentyear  = l_doc_year
    TABLES
      goodsmvt_item    = lt_mb_item
      return           = lt_return.

  IF sy-subrc = 0 AND l_mat_doc IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    gs_head-belnr = l_mat_doc."用来存储非限制库存转销售订单库存生成的凭证号
    gs_head-mjahr = l_doc_year.
    MOVE-CORRESPONDING gs_head TO gs_head_modify.

    PERFORM frm_db_modify.
    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT lt_return.
      PERFORM frm_add_msg USING lt_return-type
                            lt_return-id
                            lt_return-number
                            lt_return-message_v1
                            lt_return-message_v2
                            lt_return-message_v3
                            lt_return-message_v4.
    ENDLOOP.

  ENDIF.

ENDFORM.
