*&---------------------------------------------------------------------*
*& Report ZAFO_INIT_SCREEN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo_init_screen.

SELECT * FROM zafo_screen
  INTO TABLE @DATA(lt_screen)
  ORDER BY object, fieldalv,dzaehk.

DEFINE set_info.
  IF <screen>-fieldname = &1 .
    IF &2 IS NOT INITIAL.
      <screen>-coltext = &2.
    ENDIF.
    IF &3 IS NOT INITIAL.
      <screen>-emphasize = &3.
    ENDIF.
    IF &4 IS NOT INITIAL.
        <screen>-outputlen = &4.
    ENDIF.

  ENDIF.
END-OF-DEFINITION.

DATA line_id TYPE zafonr.

LOOP AT lt_screen INTO DATA(l_screen)
  GROUP BY ( object = l_screen-object )
  INTO DATA(group_screen).
  CLEAR line_id.
  LOOP AT GROUP group_screen ASSIGNING FIELD-SYMBOL(<screen>).
    ADD 10 TO line_id.
    IF line_id < 500 AND <screen>-fieldalv = 'ITEM'.
      line_id = 510.
    ENDIF.
    <screen>-dzaehk = line_id.
    <screen>-dzaehk = |{ <screen>-dzaehk ALPHA = IN }|.
    CASE <screen>-fieldname.
        set_info 'SATNR' '' 'C600' '10' .
        set_info 'MATNR' '' 'C500' '12'.
        set_info 'MAKTX' '' '' '20'.
        set_info 'PRICE_LONG' '' '' '8'.
        set_info 'PRICE' '' '' '8'.
        set_info 'AMOUNT' '' '' '8'.
        set_info 'MENGE' '' '' '8'.
        set_info 'MENGE_PLAN' '未清数量'(001) 'C300' '8'.
        set_info 'MENGE_REF' '计划数量'(002) 'C300' '8'.
        set_info 'MENGE_DONE' '已完成数量'(003) 'C300' '8'.
        set_info 'MENGE_STOCK' '库存数量'(004) 'C310' '8'.
        set_info 'AUFNR' '生产订单'(005) 'C700' '10'.
        set_info 'REMARK1' '备注'(006) '' '20'.
        set_info 'LIFNR_NAME' '供应商名称'(007) '' '20'.
        set_info 'KOSTL_NAME' '成本中心名称'(008) '' '20'.

    ENDCASE.
  ENDLOOP.

ENDLOOP.

DELETE FROM zafo_screen.
COMMIT WORK AND WAIT.
MODIFY zafo_screen FROM TABLE lt_screen.
COMMIT WORK AND WAIT.

*loop at lt_data.
*
*endloop.
