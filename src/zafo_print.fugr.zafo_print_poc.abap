FUNCTION zafo_print_poc.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_MODEL) TYPE  ZAFO_PRINT_MODEL OPTIONAL
*"     VALUE(I_PRINT_TYPE) TYPE  ZAFO_PRINT_TYPE
*"  TABLES
*"      IT_HEAD STRUCTURE  ZAFO_SHEAD OPTIONAL
*"      IT_ITEM STRUCTURE  ZAFO_SITEM OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.

  CLEAR lt_item[].
  CLEAR gt_print_h[].
  CLEAR gt_print_i[].

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

  PERFORM frm_set_poc_print_data TABLES it_head it_item.

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


FORM frm_set_poc_print_data TABLES
                                  it_head STRUCTURE zafo_shead
                                  it_item STRUCTURE zafo_sitem.
  DATA:l_text TYPE char20.
  DATA:ls_line(5) TYPE i.
  DATA ct_item LIKE TABLE OF it_item WITH HEADER LINE.
  FIELD-SYMBOLS: <it_item> LIKE it_item.

  LOOP AT it_item ASSIGNING <it_item>.
    IF <it_item>-zcolor IS NOT INITIAL.
      CLEAR l_text.
      CALL FUNCTION 'CONVERSION_EXIT_ZCOL_OUTPUT'
        EXPORTING
          input  = <it_item>-zcolor
        IMPORTING
          output = <it_item>-zcolor_text.
    ENDIF.
  ENDLOOP.


  CASE gs_print-model.
    WHEN 'POC01'.
      DATA: in_text  TYPE char10,
            out_text TYPE char10.

      LOOP AT it_head.

        gt_print_h-afono = it_head-afono.

        gt_print_h-title_txt1 = it_head-bukrs_name.

        gt_print_h-title_txt2 = '采购追加单'.

        CLEAR out_text.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = it_head-afono
          IMPORTING
            output = out_text.
        gt_print_h-head_txt1 = '单据号:' && out_text.
        gt_print_h-head_txt2 = '单据日期:' && it_head-erdat.

        SELECT SINGLE remark1 INTO gt_print_h-head_txt3 FROM zafo_head
          WHERE afono = it_head-ebeln.
        gt_print_h-head_txt3 = '原采购编号:' && gt_print_h-head_txt3.

        CLEAR out_text.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = it_head-ebeln
          IMPORTING
            output = out_text.
        gt_print_h-head_txt4 = '原采购合同:' && out_text.

        gt_print_h-foot_txt1 = '变更说明:' && it_head-remark1.

        SELECT SINGLE name INTO gt_print_h-foot_txt2 FROM zapp_addr WHERE person = it_head-ernam.
        gt_print_h-foot_txt2 = '采购员:' && gt_print_h-foot_txt2.

        gt_print_h-foot_txt3 = '供应商:' && it_head-lifnr_name.

        gt_print_h-foot_txt4 = '数量:' && it_head-menge.

        gt_print_h-foot_txt5 = '金额:' && it_head-amount.

        gt_print_h-col_name1 = '物品名称'.
*        gt_print_h-col_name2 = '工厂款号'.
*        gt_print_h-col_name3 = '颜色'.
*        gt_print_h-col_name4 = '规格'.
*        gt_print_h-col_name5 = '说明'.
        gt_print_h-col_name2 = '物料编码'.
        gt_print_h-col_name3 = '工厂款号'.
        gt_print_h-col_name4 = '颜色'.
        gt_print_h-col_name5 = '规格/说明'.
        gt_print_h-col_name6 = '单位'.
        gt_print_h-col_name7 = '数量'.
        gt_print_h-col_name8 = '单价'.
        gt_print_h-col_name9 = '金额'.
        gt_print_h-col_name10 = '产前样'.
        gt_print_h-col_name11 = '备注'.

        CLEAR ls_line.

        CLEAR ct_item.
        CLEAR ct_item[].

        LOOP AT it_item  WHERE afono = it_head-afono.
          ct_item-maktx = it_item-maktx.
          ct_item-matnr = it_item-matnr.
          ct_item-idnlf = it_item-idnlf.
          ct_item-zcolor_text = it_item-zcolor_text.
          ct_item-zsize = it_item-zsize.
          ct_item-znorms = it_item-znorms.
          ct_item-meins = it_item-meins.
          ct_item-menge = it_item-menge.
          ct_item-menge_cg = it_item-menge_cg.
          ct_item-menge_zj = it_item-menge_zj.
          ct_item-price_long = it_item-price_long.
          ct_item-amount = it_item-amount.
          ct_item-zppflag = it_item-zppflag.
          ct_item-remark2 = it_item-remark2.
          COLLECT ct_item INTO ct_item.
          CLEAR ct_item.
        ENDLOOP.
        SORT ct_item BY afono maktx zsize zcolor_text znorms zppflag.

        LOOP AT ct_item.

          READ TABLE it_item WITH KEY
            afono = it_head-afono
            matnr = ct_item-matnr
            maktx = ct_item-maktx
            idnlf = ct_item-idnlf
            zcolor_text = ct_item-zcolor_text
            zsize = ct_item-zsize
            znorms = ct_item-znorms
            meins = ct_item-meins.

          gt_print_i-afono = it_head-afono.

          gt_print_i-col01 = ct_item-maktx.
          CONDENSE gt_print_i-col01 NO-GAPS.

          gt_print_i-col02 = ct_item-matnr.
          CONDENSE gt_print_i-col02 NO-GAPS.

          gt_print_i-col03 = ct_item-idnlf.
          CONDENSE gt_print_i-col03 NO-GAPS.

          gt_print_i-col04 = ct_item-zcolor_text.
          gt_print_i-col05 = ct_item-zsize && '/' && ct_item-znorms.
          CONDENSE gt_print_i-col05 NO-GAPS.

          PERFORM frm_convert_bprme USING ct_item CHANGING gt_print_i-col06.

          CONDENSE gt_print_i-col06 NO-GAPS.

          ct_item-menge_cg = ct_item-menge_cg + ct_item-menge_zj.
          PERFORM frm_convert_menge USING ct_item CHANGING gt_print_i-col07.
          PERFORM frm_convert_zero USING gt_print_i-col07 CHANGING gt_print_i-col07.

          CONDENSE gt_print_i-col07 NO-GAPS.

          ct_item-price_long = it_item-price_long.
          PERFORM frm_convert_price USING ct_item-price_long CHANGING gt_print_i-col08.
          CONDENSE gt_print_i-col08 NO-GAPS.

          PERFORM frm_convert_price USING ct_item-amount CHANGING gt_print_i-col09.
          CONDENSE gt_print_i-col09 NO-GAPS.

          IF ct_item-zppflag IS INITIAL.
          ELSE.
            gt_print_i-col10 = '是'.
          ENDIF.

          gt_print_i-col11 = ct_item-remark2.
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

    WHEN 'POC02'.
      LOOP AT it_head.

        gt_print_h-afono = it_head-afono.

        gt_print_h-title_txt1 = it_head-bukrs_name.

        gt_print_h-title_txt2 = '采购追加单'.

        CLEAR out_text.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = it_head-afono
          IMPORTING
            output = out_text.

        gt_print_h-head_txt1 = '单据号:' && out_text.
        gt_print_h-head_txt2 = '单据日期:' && it_head-erdat.

        SELECT SINGLE remark1 INTO gt_print_h-head_txt3 FROM zafo_head
          WHERE afono = it_head-ebeln.
        gt_print_h-head_txt3 = '原采购编号:' && gt_print_h-head_txt3.

        CLEAR out_text.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = it_head-ebeln
          IMPORTING
            output = out_text.
        gt_print_h-head_txt4 = '原采购合同:' && out_text.

        gt_print_h-foot_txt1 = '变更说明:' && it_head-remark1.

        SELECT SINGLE name INTO gt_print_h-foot_txt2 FROM zapp_addr WHERE person = it_head-ernam.
        gt_print_h-foot_txt2 = '采购员:' && gt_print_h-foot_txt2.

        gt_print_h-foot_txt3 = '供应商:' && it_head-lifnr_name.

        gt_print_h-foot_txt4 = '数量:' && it_head-menge.

        gt_print_h-foot_txt5 = '金额:' && it_head-amount.

        gt_print_h-col_name1 = '合同号'.
        gt_print_h-col_name2 = '物品名称'.
*        gt_print_h-col_name3 = '工厂款号'.
*        gt_print_h-col_name4 = '颜色'.
*        gt_print_h-col_name5 = '规格'.
*        gt_print_h-col_name6 = '说明'.

        gt_print_h-col_name3 = '物料编码'.
        gt_print_h-col_name4 = '工厂款号'.
        gt_print_h-col_name5 = '颜色'.
        gt_print_h-col_name6 = '规格/说明'.

        gt_print_h-col_name7 = '单位'.
        gt_print_h-col_name8 = '数量'.
        gt_print_h-col_name9 = '单价'.
        gt_print_h-col_name10 = '金额'.
        gt_print_h-col_name11 = '产前样'.
        gt_print_h-col_name12 = '备注'.

        CLEAR ls_line.

        CLEAR ct_item.
        CLEAR ct_item[].

        LOOP AT it_item  WHERE afono = it_head-afono.
          ct_item-zzpino = it_item-zzpino.
          ct_item-maktx = it_item-maktx.
          ct_item-matnr = it_item-matnr.
          ct_item-idnlf = it_item-idnlf.
          ct_item-zcolor_text = it_item-zcolor_text.
          ct_item-zsize = it_item-zsize.
          ct_item-znorms = it_item-znorms.
          ct_item-meins = it_item-meins.
          ct_item-menge = it_item-menge.
          ct_item-menge_cg = it_item-menge_cg.
          ct_item-menge_zj = it_item-menge_zj.
          ct_item-price_long = it_item-price_long.
          ct_item-amount = it_item-amount.
          ct_item-zppflag = it_item-zppflag.
          ct_item-remark2 = it_item-remark2.
          COLLECT ct_item INTO ct_item.
          CLEAR ct_item.
        ENDLOOP.
        SORT ct_item BY afono zzpino maktx zsize zcolor_text znorms zppflag.

        LOOP AT ct_item.

          READ TABLE it_item WITH KEY
            afono = it_head-afono
            matnr = ct_item-matnr
            maktx = ct_item-maktx
            idnlf = ct_item-idnlf
            zcolor_text = ct_item-zcolor_text
            zsize = ct_item-zsize
            znorms = ct_item-znorms
            meins = ct_item-meins.

          gt_print_i-afono = it_head-afono.

          gt_print_i-col01 = ct_item-zzpino.

          gt_print_i-col02 = ct_item-maktx.
          CONDENSE gt_print_i-col02 NO-GAPS.

          gt_print_i-col03 = ct_item-matnr.
          CONDENSE gt_print_i-col03 NO-GAPS.

          gt_print_i-col04 = ct_item-idnlf.
          CONDENSE gt_print_i-col04 NO-GAPS.

          gt_print_i-col05 = ct_item-zcolor_text.
          CONDENSE gt_print_i-col05 NO-GAPS.

          gt_print_i-col06 = ct_item-zsize && '/' && ct_item-znorms.
          CONDENSE gt_print_i-col06 NO-GAPS.

          PERFORM frm_convert_bprme USING ct_item CHANGING gt_print_i-col07.

          CONDENSE gt_print_i-col07 NO-GAPS.

          ct_item-menge_cg = ct_item-menge_cg + ct_item-menge_zj.
          PERFORM frm_convert_menge USING ct_item CHANGING gt_print_i-col08.
          PERFORM frm_convert_zero USING gt_print_i-col08 CHANGING gt_print_i-col08.

          CONDENSE gt_print_i-col08 NO-GAPS.

          ct_item-price_long = it_item-price_long.
          PERFORM frm_convert_price USING ct_item-price_long CHANGING gt_print_i-col09.
          CONDENSE gt_print_i-col09 NO-GAPS.

          PERFORM frm_convert_price USING ct_item-amount CHANGING gt_print_i-col10.
          CONDENSE gt_print_i-col10 NO-GAPS.

          IF ct_item-zppflag IS INITIAL.
          ELSE.
            gt_print_i-col11 = '是'.
          ENDIF.

          gt_print_i-col12 = ct_item-remark2.
          CONDENSE gt_print_i-col12 NO-GAPS.

          APPEND gt_print_i.
          CLEAR gt_print_i.

        ENDLOOP.

        IF sy-subrc NE 0.
          RETURN.
        ENDIF.

        APPEND gt_print_h.
        CLEAR gt_print_h.

      ENDLOOP.
  ENDCASE.
ENDFORM.
