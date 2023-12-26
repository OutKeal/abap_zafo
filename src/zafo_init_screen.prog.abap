*&---------------------------------------------------------------------*
*& Report ZAFO_INIT_SCREEN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo_init_screen.

SELECT * FROM zafo_screen
  INTO TABLE @DATA(lt_screen).

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


LOOP AT lt_screen ASSIGNING FIELD-SYMBOL(<screen>).

  CASE <screen>-fieldname.
      set_info 'SATNR' '款号' 'C600' '10'.
      set_info 'MATNR' '款号' 'C500' '12'.
      set_info 'MENGE_PLAN' '未清数量' 'C300' '8'.
      set_info 'MENGE_REF' '计划数量' 'C300' '8'.
      set_info 'MENGE_DONE' '已完成数量' 'C300' '8'.
      set_info 'MENGE_STOCK' '库存数量' 'C310' '8'.
      set_info 'AUFNR' '生产订单' 'C700' '10'.
  ENDCASE.


ENDLOOP.

MODIFY zafo_screen FROM TABLE lt_screen.
COMMIT WORK.

*loop at lt_data.
*
*endloop.
