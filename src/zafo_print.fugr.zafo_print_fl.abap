FUNCTION zafo_print_fl.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_MODEL) TYPE  ZAFO_PRINT_MODEL OPTIONAL
*"     VALUE(I_PRINT_TYPE) TYPE  ZAFO_PRINT_TYPE
*"  TABLES
*"      IT_HEAD STRUCTURE  ZAFO_SHEAD OPTIONAL
*"      IT_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"----------------------------------------------------------------------

  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.


  SELECT SINGLE * INTO @gs_print
    FROM zafo_print
    WHERE model = @i_model
    AND print_type = @i_print_type.

  IF i_model IS INITIAL.
    PERFORM frm_pop_model USING i_print_type.
    IF gs_print IS INITIAL.
      RETURN .
    ENDIF.
  ENDIF.

  CLEAR lt_item[].
  CLEAR gt_print_h[].
  CLEAR gt_print_i[].

  SELECT * FROM zconf_cate_cate1 WHERE zconf_id = 'ZCONF_CATE_A'
  INTO TABLE @DATA(lt_conf_cate1).
  SELECT * FROM zconf_cate_cate2 WHERE zconf_id = 'ZCONF_CATE_A'
  INTO TABLE @DATA(lt_conf_cate2).
  SELECT * FROM zconf_cate_cate3 WHERE zconf_id = 'ZCONF_CATE_A'
  INTO TABLE @DATA(lt_conf_cate3).
  SELECT * FROM zconf_cate_matnr WHERE zconf_id = 'ZCONF_CATE_A'
  INTO TABLE @DATA(lt_conf_matnr).

  LOOP AT it_item .

    SELECT SINGLE zcate1,zcate2,zcate3
      INTO CORRESPONDING FIELDS OF @it_item
      FROM zmmt0010 WHERE matnr = @it_item-matnr.

    IF it_item-zcate1 = 'FA' OR it_item-zcate1 = 'GL' OR it_item-zcate1 = 'LA' .
      lt_item-afonr = 1.
      lt_item-zcate1 = '主料'.
    ELSE.
      lt_item-afonr = 2.
      lt_item-zcate1 = '辅料'.
    ENDIF.

    "修改物料大类分页
    READ TABLE lt_conf_cate1 INTO DATA(ls_conf_cate1) WITH KEY zcate1 = it_item-zcate1.
    IF sy-subrc = 0.
      CASE ls_conf_cate1-zcate1_fp.
        WHEN  'FA'.
          lt_item-afonr = 1.
          lt_item-zcate1 = '主料'.
        WHEN 'AC'.
          lt_item-afonr = 2.
          lt_item-zcate1 = '辅料'.
      ENDCASE.
    ENDIF.

    READ TABLE lt_conf_cate2 INTO DATA(ls_conf_cate2)
        WITH KEY zcate1 = it_item-zcate1
                  zcate2 = it_item-zcate2.
    IF sy-subrc = 0.
      CASE ls_conf_cate2-zcate1_fp.
        WHEN  'FA'.
          lt_item-afonr = 1.
          lt_item-zcate1 = '主料'.
        WHEN 'AC'.
          lt_item-afonr = 2.
          lt_item-zcate1 = '辅料'.
      ENDCASE.
    ENDIF.

    READ TABLE lt_conf_cate3 INTO DATA(ls_conf_cate3)
        WITH KEY zcate1 = it_item-zcate1
                  zcate2 = it_item-zcate2
                  zcate3 = it_item-zcate3.
    IF sy-subrc = 0.
      CASE ls_conf_cate3-zcate1_fp.
        WHEN  'FA'.
          lt_item-afonr = 1.
          lt_item-zcate1 = '主料'.
        WHEN 'AC'.
          lt_item-afonr = 2.
          lt_item-zcate1 = '辅料'.
      ENDCASE.
    ENDIF.

    READ TABLE lt_conf_matnr INTO DATA(ls_conf_matnr)
        WITH KEY matnr = it_item-matnr.
    IF sy-subrc = 0.
      CASE ls_conf_matnr-zcate1_fp.
        WHEN  'FA'.
          lt_item-afonr = 1.
          lt_item-zcate1 = '主料'.
        WHEN 'AC'.
          lt_item-afonr = 2.
          lt_item-zcate1 = '辅料'.
      ENDCASE.
    ENDIF.

    lt_item-afono = it_item-afono.
*    lt_item-zzpino = it_item-zzpino.
*    lt_item-zcate1 = it_item-zcate1.
    lt_item-matnr = it_item-matnr.
    lt_item-maktx = it_item-maktx.
    lt_item-idnlf = it_item-idnlf.

    IF it_item-zcolor IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ZCOL_OUTPUT'
        EXPORTING
          input  = it_item-zcolor
        IMPORTING
          output = lt_item-zcolor_text.
    ENDIF.
    lt_item-zsize = it_item-zsize.
    lt_item-znorms = it_item-znorms.
    lt_item-meins = it_item-meins.
    lt_item-bprme = it_item-bprme.
    lt_item-menge1 = it_item-menge1.
    lt_item-menge2 = it_item-menge2.
    lt_item-menge3 = it_item-menge3.
    lt_item-menge = it_item-menge.
    lt_item-menge_cg = it_item-menge_cg.
    COLLECT lt_item.
    CLEAR lt_item.
  ENDLOOP.

  SORT lt_item BY afonr maktx zcolor znorms zsize .

  CASE gs_print-model.
    WHEN 'LL01'.
      PERFORM frm_set_all_print_data TABLES it_head lt_item.
    WHEN 'LL02'.
      PERFORM frm_set_fl_zl_print_data TABLES it_head lt_item.
      PERFORM frm_set_fl_fl_print_data TABLES it_head lt_item.
  ENDCASE.

  PERFORM frm_initial_smartforms USING gs_print-smart_form.

  LOOP AT gt_print_h INTO gs_print_h.

    AT FIRST.
      control_parameters-no_close = 'X'.
    ENDAT.
    AT LAST.

      control_parameters-no_close = space.

    ENDAT.

    PERFORM frm_print_report.

    control_parameters-no_open = 'X'.

  ENDLOOP.
ENDFUNCTION.


FORM frm_set_all_print_data TABLES  it_head STRUCTURE zafo_shead
                                  it_item STRUCTURE zafo_sitem.
  DATA:ls_line(5) TYPE i.
  DATA:lv_text1 TYPE char20.
  DATA:lv_text2 TYPE char20.
  DATA:lv_unit1 TYPE char20.
  DATA:lv_unit2 TYPE char20.

  LOOP AT it_head.

    gt_print_h-afono = it_head-afono+2(8).
*    gt_print_h-afono = it_head-afono.

    SELECT SINGLE butxt FROM t001
      WHERE bukrs = @it_head-werks
      INTO @gt_print_h-title_txt1.

    gt_print_h-title_txt2 = '生产领料单'.

*    PERFORM frm_get_afopd_name USING it_head-afopd '单据号' CHANGING gt_print_h-head_txt1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_head-afono
      IMPORTING
        output = gt_print_h-head_txt1.
    gt_print_h-head_txt1 = '领料单号:' && gt_print_h-head_txt1 .

    SELECT SINGLE name INTO gt_print_h-head_txt2 FROM zapp_addr WHERE person = it_head-ernam.
    gt_print_h-head_txt2 = '制单人:' && gt_print_h-head_txt2.
    PERFORM frm_get_date_name USING it_head-erdat  CHANGING gt_print_h-head_txt4.
    gt_print_h-head_txt4 = '创建日期:' && gt_print_h-head_txt4.
    gt_print_h-head_txt5 = '备注:' && it_head-remark1.

    gt_print_h-head_txt3 = '销售合同号:' && it_head-group1.

    gt_print_h-col_name1 = '序号'.
    gt_print_h-col_name2 = '分类'.
    gt_print_h-col_name3 = '物品名称'.
    gt_print_h-col_name4 = '工厂款号'.
    gt_print_h-col_name5 = '颜色'.
    gt_print_h-col_name6 = '尺码'.
    gt_print_h-col_name7 = '说明'.
    gt_print_h-col_name8 = '计划数'.
    gt_print_h-col_name9 = '领用数'.
    gt_print_h-col_name10 = '单位'.
    gt_print_h-col_name11 = '领用数(报价)'.

    CLEAR ls_line.

    LOOP AT it_item WHERE afono = it_head-afono.

      gt_print_i-afono = it_head-afono+2(8) .
*      gt_print_i-afono = it_head-afono.
      ADD 1 TO ls_line.
      gt_print_i-col01 = ls_line.
      CONDENSE gt_print_i-col01 NO-GAPS.

      gt_print_i-col02 = it_item-zcate1.
      CONDENSE gt_print_i-col02 NO-GAPS.

      gt_print_i-col03 = it_item-maktx.
      CONDENSE gt_print_i-col03 NO-GAPS.

      gt_print_i-col04 = it_item-idnlf.
      CONDENSE gt_print_i-col04 NO-GAPS.

      gt_print_i-col05 =  it_item-zcolor_text.

      gt_print_i-col06 = it_item-zsize.
      CONDENSE gt_print_i-col06 NO-GAPS.

      gt_print_i-col07 = it_item-znorms.
      CONDENSE gt_print_i-col07 NO-GAPS.

      PERFORM frm_convert_zero USING it_item-menge1 CHANGING lv_text1.
      gt_print_i-col08 = lv_text1.
      CONDENSE gt_print_i-col08 NO-GAPS.

      CLEAR: lv_text1,lv_text2.
      PERFORM frm_convert_zero USING it_item-menge CHANGING lv_text1.
      gt_print_i-col09 = lv_text1.
      CONDENSE gt_print_i-col09 NO-GAPS.

      PERFORM frm_convert_meins USING it_item CHANGING lv_unit1.
      gt_print_i-col10 = lv_unit1.
      CONDENSE gt_print_i-col10 NO-GAPS.

      PERFORM frm_convert_zero USING it_item-menge_cg CHANGING lv_text2.

      PERFORM frm_convert_bprme USING it_item CHANGING lv_unit2.
      gt_print_i-col11 = lv_text2 && '(' && lv_unit2 && ')'.
      CONDENSE gt_print_i-col11 NO-GAPS.

      APPEND gt_print_i.
      CLEAR gt_print_i.

    ENDLOOP.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    APPEND gt_print_h.
    CLEAR gt_print_h.

  ENDLOOP.
ENDFORM.


FORM frm_set_fl_zl_print_data TABLES it_head STRUCTURE zafo_shead
                                  it_item STRUCTURE zafo_sitem.

  DATA:ls_line(5) TYPE i.
  DATA:lv_text1 TYPE char20.
  DATA:lv_text2 TYPE char20.
  DATA:lv_unit1 TYPE char20.
  DATA:lv_unit2 TYPE char20.


  LOOP AT it_head.

    gt_print_h-afono ='ZL' && it_head-afono+2(8).
*    gt_print_h-afono = it_head-afono.

    SELECT SINGLE butxt FROM t001
      WHERE bukrs = @it_head-werks
      INTO @gt_print_h-title_txt1.

    gt_print_h-title_txt2 = '生产领料单'.

*    PERFORM frm_get_afopd_name USING it_head-afopd '单据号' CHANGING gt_print_h-head_txt1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_head-afono
      IMPORTING
        output = gt_print_h-head_txt1.
    gt_print_h-head_txt1 = '领料单号:' && gt_print_h-head_txt1 .

    SELECT SINGLE name INTO gt_print_h-head_txt2 FROM zapp_addr WHERE person = it_head-ernam.
    gt_print_h-head_txt2 = '制单人:' && gt_print_h-head_txt2.
    PERFORM frm_get_date_name USING it_head-erdat  CHANGING gt_print_h-head_txt4.
    gt_print_h-head_txt4 = '创建日期:' && gt_print_h-head_txt4.
    gt_print_h-head_txt5 = '备注:' && it_head-remark1.

    gt_print_h-head_txt3 = '销售合同号:' && it_head-group1.

    gt_print_h-col_name1 = '序号'.
    gt_print_h-col_name2 = '分类'.
    gt_print_h-col_name3 = '物品名称'.
    gt_print_h-col_name4 = '工厂款号'.
    gt_print_h-col_name5 = '颜色'.
    gt_print_h-col_name6 = '尺码'.
    gt_print_h-col_name7 = '说明'.
    gt_print_h-col_name8 = '计划数'.
    gt_print_h-col_name9 = '领用数'.
    gt_print_h-col_name10 = '单位'.
    gt_print_h-col_name11 = '领用数(报价)'.

    CLEAR ls_line.

    LOOP AT it_item WHERE afono = it_head-afono AND zcate1 = '主料'.

      gt_print_i-afono = 'ZL' && it_head-afono+2(8) .
*      gt_print_i-afono = it_head-afono.
      ADD 1 TO ls_line.
      gt_print_i-col01 = ls_line.
      CONDENSE gt_print_i-col01 NO-GAPS.

      gt_print_i-col02 = it_item-zcate1.
      CONDENSE gt_print_i-col02 NO-GAPS.


      gt_print_i-col03 = it_item-maktx.
      CONDENSE gt_print_i-col03 NO-GAPS.

      gt_print_i-col04 = it_item-idnlf.
      CONDENSE gt_print_i-col04 NO-GAPS.

      gt_print_i-col05 =  it_item-zcolor_text.

      gt_print_i-col06 = it_item-zsize.
      CONDENSE gt_print_i-col06 NO-GAPS.

      gt_print_i-col07 = it_item-znorms.
      CONDENSE gt_print_i-col07 NO-GAPS.

      PERFORM frm_convert_zero USING it_item-menge1 CHANGING lv_text1.
      gt_print_i-col08 = lv_text1.
      CONDENSE gt_print_i-col08 NO-GAPS.

      CLEAR: lv_text1,lv_text2.
      PERFORM frm_convert_zero USING it_item-menge CHANGING lv_text1.
      gt_print_i-col09 = lv_text1.
      CONDENSE gt_print_i-col09 NO-GAPS.

      PERFORM frm_convert_meins USING it_item CHANGING lv_unit1.
      gt_print_i-col10 = lv_unit1.
      CONDENSE gt_print_i-col10 NO-GAPS.

      PERFORM frm_convert_zero USING it_item-menge_cg CHANGING lv_text2.

      PERFORM frm_convert_bprme USING it_item CHANGING lv_unit2.
      gt_print_i-col11 = lv_text2 && '(' && lv_unit2 && ')'.
      CONDENSE gt_print_i-col11 NO-GAPS.

      APPEND gt_print_i.
      CLEAR gt_print_i.
    ENDLOOP.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    APPEND gt_print_h.
    CLEAR gt_print_h.

  ENDLOOP.
ENDFORM.


FORM frm_set_fl_fl_print_data TABLES
                                  it_head STRUCTURE zafo_shead
                                  it_item STRUCTURE zafo_sitem.
  DATA:ls_line(5) TYPE i.
  DATA:lv_text1 TYPE char20.
  DATA:lv_text2 TYPE char20.
  DATA:lv_unit1 TYPE char20.
  DATA:lv_unit2 TYPE char20.


  LOOP AT it_head.
    gt_print_h-afono ='FL' && it_head-afono+2(8).

    SELECT SINGLE butxt FROM t001
      WHERE bukrs = @it_head-werks
      INTO @gt_print_h-title_txt1.

    gt_print_h-title_txt2 = '生产领料单'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_head-afono
      IMPORTING
        output = gt_print_h-head_txt1.
    gt_print_h-head_txt1 = '领料单号:' && gt_print_h-head_txt1 .
    SELECT SINGLE name INTO gt_print_h-head_txt2 FROM zapp_addr WHERE person = it_head-ernam.
    gt_print_h-head_txt2 = '制单人:' && gt_print_h-head_txt2.
    PERFORM frm_get_lgort_name USING it_head-werks it_head-lgort CHANGING gt_print_h-head_txt3.
    PERFORM frm_get_date_name USING it_head-erdat  CHANGING gt_print_h-head_txt4.
    gt_print_h-head_txt4 = '创建日期:' && gt_print_h-head_txt4.
    gt_print_h-head_txt5 = '备注:' && it_head-remark1.
    gt_print_h-head_txt3 = '销售合同号:' && it_head-group1.

    gt_print_h-col_name1 = '序号'.
    gt_print_h-col_name2 = '分类'.
    gt_print_h-col_name3 = '物品名称'.
    gt_print_h-col_name4 = '工厂款号'.
    gt_print_h-col_name5 = '颜色'.
    gt_print_h-col_name6 = '尺码'.
    gt_print_h-col_name7 = '说明'.
    gt_print_h-col_name8 = '计划数'.
    gt_print_h-col_name9 = '领用数'.
    gt_print_h-col_name10 = '单位'.
    gt_print_h-col_name11 = '领用数(报价)'.

    CLEAR ls_line.

    LOOP AT it_item WHERE afono = it_head-afono AND zcate1 = '辅料'.

      gt_print_i-afono = 'FL' && it_head-afono+2(8) .

      ADD 1 TO ls_line.
      gt_print_i-col01 = ls_line.
      CONDENSE gt_print_i-col01 NO-GAPS.


      gt_print_i-col02 = '辅料'.
      CONDENSE gt_print_i-col02 NO-GAPS.


      gt_print_i-col03 = it_item-maktx.
      CONDENSE gt_print_i-col03 NO-GAPS.

      gt_print_i-col04 = it_item-idnlf.
      CONDENSE gt_print_i-col04 NO-GAPS.

      gt_print_i-col05 =  it_item-zcolor_text.

      gt_print_i-col06 = it_item-zsize.
      CONDENSE gt_print_i-col06 NO-GAPS.

      gt_print_i-col07 = it_item-znorms.
      CONDENSE gt_print_i-col07 NO-GAPS.

      PERFORM frm_convert_zero USING it_item-menge1 CHANGING lv_text1.
      gt_print_i-col08 = lv_text1.
      CONDENSE gt_print_i-col08 NO-GAPS.

      CLEAR: lv_text1,lv_text2.
      PERFORM frm_convert_zero USING it_item-menge CHANGING lv_text1.
      gt_print_i-col09 = lv_text1.
      CONDENSE gt_print_i-col09 NO-GAPS.

      PERFORM frm_convert_meins USING it_item CHANGING lv_unit1.
      gt_print_i-col10 = lv_unit1.
      CONDENSE gt_print_i-col10 NO-GAPS.

      PERFORM frm_convert_zero USING it_item-menge_cg CHANGING lv_text2.

      PERFORM frm_convert_bprme USING it_item CHANGING lv_unit2.
      gt_print_i-col11 = lv_text2 && '(' && lv_unit2 && ')'.
      CONDENSE gt_print_i-col11 NO-GAPS.


      APPEND gt_print_i.
      CLEAR gt_print_i.

    ENDLOOP.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    APPEND gt_print_h.
    CLEAR gt_print_h.

  ENDLOOP.


ENDFORM.
