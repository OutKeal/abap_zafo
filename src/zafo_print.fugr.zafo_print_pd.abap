FUNCTION zafo_print_pd.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      IT_HEAD STRUCTURE  ZAFO_PD_HEAD OPTIONAL
*"      IT_ITEM STRUCTURE  ZAFO_PD_ITEM OPTIONAL
*"----------------------------------------------------------------------

  PERFORM frm_set_pd_print_data TABLES it_head it_item.

  PERFORM frm_initial_smartforms USING 'ZAFO_PRINT_PD01'.


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

FORM frm_set_pd_print_data TABLES it_head STRUCTURE zafo_pd_head
                                  it_item STRUCTURE zafo_pd_item.

  CLEAR gt_print_h[].
  CLEAR gt_print_i[].

  LOOP AT it_head.

    gt_print_h-afono = it_head-afopd.

    SELECT SINGLE butxt FROM t001
      WHERE bukrs = @it_head-werks
      INTO @gt_print_h-title_txt1.

    gt_print_h-title_txt2 = '仓库盘点录入单'.

*    PERFORM frm_get_afopd_name USING it_head-afopd '单据号' CHANGING gt_print_h-head_txt1.
    gt_print_h-head_txt1 = '盘点单:' && it_head-afopd.


    PERFORM frm_get_lgort_name USING it_head-werks it_head-lgort CHANGING gt_print_h-head_txt3.

    PERFORM frm_get_date_name USING it_head-erdat  CHANGING gt_print_h-head_txt4.

    gt_print_h-col_name1 = '合同号'.
    gt_print_h-col_name2 = '物品名称'.
    gt_print_h-col_name3 = '工厂款号'.
    gt_print_h-col_name4 = '颜色'.
    gt_print_h-col_name5 = '规格'.
    gt_print_h-col_name6 = '说明'.
    gt_print_h-col_name7 = '单位'.
    gt_print_h-col_name8 = '原库位'.
    gt_print_h-col_name9 = '原库存'.
    gt_print_h-col_name10 = '现库存'.
    gt_print_h-FOOT_TXT1 = '负责人:'.
    gt_print_h-FOOT_TXT2 = '仓库主管:'.
    gt_print_h-FOOT_TXT3 = '盘点:'.
    gt_print_h-FOOT_TXT4 = '制单:'.
*    gt_print_h-FOOT_TXT5 = '负责人:'.

    append gt_print_h.
    clear gt_print_h.

    LOOP AT it_item WHERE afopd = it_head-afopd.

      gt_print_i-afono = it_head-afopd.

      gt_print_i-col01 = it_item-zzpino.
      CONDENSE gt_print_i-col01 NO-GAPS.


      gt_print_i-col02 = it_item-maktx.
      CONDENSE gt_print_i-col02 NO-GAPS.


      gt_print_i-col03 = it_item-idnlf.
      CONDENSE gt_print_i-col03 NO-GAPS.

      IF it_item-zcolor IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ZCOL_OUTPUT'
          EXPORTING
            input  = it_item-zcolor
          IMPORTING
            output = gt_print_i-col04.
      ENDIF.

      gt_print_i-col05 = it_item-zsize.
      CONDENSE gt_print_i-col05 NO-GAPS.

      gt_print_i-col06 = it_item-znorms.
      CONDENSE gt_print_i-col06 NO-GAPS.

      gt_print_i-col07 = it_item-meins.
      CONDENSE gt_print_i-col07 NO-GAPS.

      gt_print_i-col08 = it_item-zshelves.
      CONDENSE gt_print_i-col08 NO-GAPS.

      PERFORM frm_convert_zero USING it_item-menge CHANGING gt_print_i-col09.
      CONDENSE gt_print_i-col09 NO-GAPS.

      PERFORM frm_convert_zero USING it_item-menge_sp CHANGING gt_print_i-col10.
      CONDENSE gt_print_i-col10 NO-GAPS.

      APPEND gt_print_i.
      CLEAR gt_print_i.

    ENDLOOP.

  ENDLOOP.


ENDFORM.
