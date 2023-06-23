*&---------------------------------------------------------------------*
*& 包含               ZAFO_F02
*&---------------------------------------------------------------------*

FORM frm_get_kc TABLES ct_item STRUCTURE zafo_sitem.." 获取对应库存数
  FIELD-SYMBOLS <fs_item> TYPE zafo_sitem.

  SELECT werks,lgort,matnr,charg,
          vbeln AS vbeln_va,
          posnr AS posnr_va,
          kalab AS menge4
    INTO TABLE @DATA(lt_kc)
    FROM mska
    FOR ALL ENTRIES IN @ct_item
    WHERE lgort IN ('1001','1002','1003')
      AND werks =  @ct_item-werks
      AND matnr =  @ct_item-matnr
      AND charg =  @ct_item-charg
      AND vbeln =  @ct_item-vbeln_va
      AND posnr =  @ct_item-posnr_va
      AND kalab > 0.


  DATA:ls_menge_sum TYPE menge_d.
  DATA:ls_menge_sumkey TYPE menge_d.
  DATA:ls_menge_last TYPE menge_d.

  LOOP AT lt_kc INTO DATA(ls_kc).
    CLEAR ls_menge_sum." 分摊总数
    CLEAR ls_menge_sumkey." 分摊基准总数
    ls_menge_sum = ls_kc-menge4.
    LOOP AT ct_item WHERE werks = ls_kc-werks
                      AND matnr = ls_kc-matnr
                      AND charg = ls_kc-charg
                      AND vbeln_va = ls_kc-vbeln_va
                      AND posnr_va = ls_kc-posnr_va.
      ls_menge_sumkey = ls_menge_sumkey + ct_item-menge1." 计划数
    ENDLOOP.

    ls_menge_last = ls_kc-menge4.

    SORT ct_item BY menge." 先分数量小的，不然取整之后最后一行可能为负数
    LOOP AT ct_item ASSIGNING <fs_item> WHERE werks = ls_kc-werks
                                          AND matnr = ls_kc-matnr
                                          AND charg = ls_kc-charg
                                          AND vbeln_va = ls_kc-vbeln_va
                                          AND posnr_va = ls_kc-posnr_va.
      <fs_item>-lgort = ls_kc-lgort.
      <fs_item>-menge4 = <fs_item>-menge1 * ls_kc-menge4 / ls_menge_sumkey.
      PERFORM frm_set_round USING <fs_item>-meins CHANGING <fs_item>-menge4.
      ls_menge_last = ls_menge_last - <fs_item>-menge4.
    ENDLOOP.
    IF ls_menge_last <> 0.
      <fs_item>-menge4 = <fs_item>-menge4 + ls_menge_last.
    ENDIF.

  ENDLOOP.

ENDFORM.


FORM frm_set_round USING meins CHANGING value.
  DATA: lv_round TYPE zafo_round.

  CHECK meins IS NOT INITIAL.

  SELECT SINGLE andec INTO @DATA(ls_andec) FROM t006 WHERE msehi = @meins.
  IF sy-subrc = 0.
    lv_round = ls_andec.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals = lv_round
        input    = value
        sign     = '+'
      IMPORTING
        output   = value.
  ENDIF.
ENDFORM.
