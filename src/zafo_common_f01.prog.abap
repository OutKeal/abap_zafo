FORM frm_set_dict .
  DATA fieldname TYPE char30.
  DATA linename TYPE char30.
  DATA: BEGIN OF t_dict_value OCCURS 0,
          dict_name  TYPE name1,
          dict_value TYPE zafo_dict_value,
        END OF t_dict_value.

  DATA(call_stack) = zwft_common=>system_callstack( ).
  READ TABLE call_stack INTO DATA(l_stack) WITH KEY eventtype = 'MODULE(PAI)'.
  CHECK sy-subrc EQ 0.

  SPLIT l_stack-eventname AT '-' INTO linename fieldname.
  DATA lt_dict TYPE zafo_tt_dict.
  lt_dict = VALUE #( FOR wa IN <io_class>->dict
                                WHERE ( fieldname = fieldname
                                               AND ( werks = <io_class>->werks OR werks = '' ) ) ( wa ) ).
  CHECK lt_dict[] IS NOT INITIAL.
  t_dict_value[] = CORRESPONDING #( lt_dict ).
  fieldname = l_stack-eventname.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = fieldname
    TABLES
      value_tab = t_dict_value.
ENDFORM.

FORM frm_mwskz_list.
  SELECT mwskz, text1 AS name
  FROM t007s
  WHERE spras = @sy-langu
  AND kalsm = 'TAXCN'
    AND MWSKZ LIKE 'J%'
  INTO TABLE @DATA(lt_t007s).



  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
      dynprofield = '<HEAD>-MWSKZ'
      retfield    = 'MWSKZ'
    TABLES
      value_tab   = lt_t007s.
*  CALL FUNCTION 'FI_F4_MWSKZ'
*    EXPORTING
*      i_kalsm = 'TAXCN'
*      i_xshow = <io_class>->readonly
*    IMPORTING
*      e_mwskz = <HEAD>-mwskz.
ENDFORM.

FORM frm_vtweg_list.
  SELECT vtweg, vtext AS name FROM
    tvtwt
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvtwt).
  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'VTWEG' CHANGING table = lt_tvtwt ).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
      dynprofield = '<HEAD>-VTWEG'
    TABLES
      value_tab   = lt_tvtwt.
ENDFORM.

FORM frm_head_werks.
  IF <head>-werks IS INITIAL.
    CLEAR <head>-werks_name.
    RETURN.
  ELSE.
    DATA(l_t001w) = zwft_single_read=>t001w( <head>-werks ).
    IF sy-subrc EQ 0.
      <head>-werks_name = l_t001w-name1.
    ELSE.
      MESSAGE i045 .
      CLEAR: <head>-werks,<head>-werks_name.
    ENDIF.
  ENDIF.
ENDFORM.

FORM frm_head_lgort.
  IF <head>-werks IS INITIAL.
    CLEAR: <head>-lgort,<head>-lgort_name.
    RETURN.
  ENDIF.
  IF <head>-lgort IS INITIAL.
    CLEAR <head>-lgort_name.
    RETURN.
  ENDIF.

  DATA(l_t001l) = zwft_single_read=>t001l( werks = <head>-werks
                                                                          lgort = <head>-lgort  ).
  IF sy-subrc EQ 0.
    <head>-lgort_name = l_t001l-lgobe.
  ELSE.
    MESSAGE i115 ."仓库不存在
    CLEAR: <head>-lgort,<head>-lgort_name.
  ENDIF.
ENDFORM.

FORM frm_head_umwrk.
  IF <head>-umwrk IS INITIAL.
    CLEAR <head>-umwrk_name.
    RETURN.
  ELSE.
    DATA(l_t001w) = zwft_single_read=>t001w( <head>-umwrk ).
    IF sy-subrc EQ 0.
      <head>-umwrk_name = l_t001w-name1.
    ELSE.
      MESSAGE i045 .
      CLEAR: <head>-umwrk,<head>-umwrk_name.
    ENDIF.
  ENDIF.
ENDFORM.

FORM frm_head_umlgo.
  IF <head>-umwrk IS INITIAL.
*    MESSAGE i005."请输入工厂
    CLEAR: <head>-umlgo,<head>-umlgo_name.
    RETURN.
  ENDIF.

  IF <head>-umlgo IS INITIAL.
    CLEAR <head>-umlgo_name.
    RETURN.
  ENDIF.
  DATA(l_t001l) = zwft_single_read=>t001l( werks = <head>-umwrk
                                                                     lgort = <head>-umlgo  ).
  IF sy-subrc EQ 0.
    <head>-umlgo_name = l_t001l-lgobe.
  ELSE.
    MESSAGE i115 ."仓库不存在
    CLEAR: <head>-umlgo,<head>-umlgo_name.
  ENDIF.
ENDFORM.

FORM frm_head_bukrs.
  IF <head>-bukrs IS INITIAL.
    CLEAR <head>-bukrs_name.
    RETURN.
  ENDIF.
  DATA(ls_t001) = zwft_single_read=>t001( <head>-bukrs ).
  IF sy-subrc EQ 0.
    <head>-bukrs_name = ls_t001-butxt.
  ELSE.
    CLEAR: <head>-bukrs,<head>-bukrs_name.
  ENDIF.
ENDFORM.

FORM frm_head_kostl.
  SELECT SINGLE ktext INTO <head>-kostl_name
        FROM cskt
        WHERE spras = sy-langu
        AND kostl = <head>-kostl.
ENDFORM.


FORM frm_lgort_list.
  DATA: return_tab TYPE TABLE OF ddshretval.
  SELECT lgort,lgobe AS name
    FROM t001l WHERE werks = @<head>-werks
    INTO TABLE @DATA(lt_t001l).
  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'LGORT' CHANGING table = lt_t001l ).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
      dynprofield = '<HEAD>-LGORT'
      retfield    = 'LGORT'
    TABLES
      value_tab   = lt_t001l
      return_tab  = return_tab.
  READ TABLE return_tab INTO DATA(l_return) INDEX 1.
  CHECK sy-subrc EQ 0.
  <head>-lgort = l_return-fieldval.
  READ TABLE lt_t001l INTO DATA(ls_t001l) WITH KEY lgort = <head>-lgort.
  CHECK sy-subrc EQ 0.
  <head>-lgort_name = ls_t001l-name.
  PERFORM frm_set_dynp_value USING '<HEAD>-LGORT_NAME'
                                                               <head>-lgort_name.
ENDFORM.

FORM frm_umlgo_list.
  DATA: return_tab TYPE TABLE OF ddshretval.

  SELECT lgort,lgobe AS name
  FROM t001l WHERE werks = @<head>-werks
  INTO TABLE @DATA(lt_t001l).

  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'LGORT' CHANGING table = lt_t001l ).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
      dynprofield = '<HEAD>-UMLGO'
      retfield    = 'LGORT'
    TABLES
      value_tab   = lt_t001l
      return_tab  = return_tab.
  READ TABLE return_tab INTO DATA(l_return) INDEX 1.
  CHECK sy-subrc EQ 0.
  <head>-umlgo = l_return-fieldval.
  READ TABLE lt_t001l INTO DATA(ls_t001l) WITH KEY lgort = <head>-umlgo.
  CHECK sy-subrc EQ 0.
  <head>-umlgo_name = ls_t001l-name.
  PERFORM frm_set_dynp_value USING '<HEAD>-UMLGO_NAME'
        <head>-umlgo_name.
ENDFORM.

FORM frm_bukrs_list.
  SELECT bukrs,butxt FROM t001 INTO TABLE @DATA(lt_t001).
  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'BUKRS' CHANGING table = lt_t001 ).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = 'BUKRS'
    TABLES
      value_tab = lt_t001.
ENDFORM.

FORM frm_kostl_list.
  SELECT kostl,mctxt AS name FROM m_kosts INTO TABLE @DATA(lt_csks)
    WHERE bukrs = @<head>-bukrs.
  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'KOSTL' CHANGING table = lt_csks ).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = 'KOSTL'
    TABLES
      value_tab = lt_csks.
ENDFORM.


FORM frm_po_ekgrp_list.
  SELECT
  ekgrp,
  eknam AS name
  INTO TABLE @DATA(lt_ekgrp)
  FROM t024.

  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'EKGRP' CHANGING table = lt_ekgrp ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = 'EKGRP'
    TABLES
      value_tab = lt_ekgrp.
ENDFORM.

FORM frm_po_bsart_list.
  SELECT
  t161~bsart,
  batxt AS name
  INTO TABLE @DATA(lt_bsart)
  FROM t161
  LEFT JOIN t161t ON t161~bstyp = t161t~bstyp AND t161~bsart = t161t~bsart
  WHERE t161~bstyp = 'F'
  AND t161t~spras = @sy-langu.

  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'BSART' CHANGING table = lt_bsart ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = 'BSART'
    TABLES
      value_tab = lt_bsart.

ENDFORM.

FORM frm_pr_bsart_list.
  SELECT
  t161~bsart,
  batxt
  INTO TABLE @DATA(lt_bsart)
  FROM t161
  LEFT JOIN t161t ON t161~bstyp = t161t~bstyp AND t161~bsart = t161t~bsart
  WHERE t161~bstyp = 'B'
  AND t161t~spras = @sy-langu.

  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'BSART' CHANGING table = lt_bsart ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = 'BSART'
    TABLES
      value_tab = lt_bsart.
ENDFORM.




FORM frm_head_kunnr.
  zwft_common=>search_customer(
  EXPORTING werks = <io_class>->werks
  CHANGING kunnr = <head>-kunnr name = <head>-kunnr_name ).

  SELECT SINGLE"绝大多数情况下供应商的币种唯一
  waers INTO <head>-waers
  FROM knvv
  WHERE kunnr = <head>-kunnr.
ENDFORM.

FORM frm_head_kunnr_f4.
  DATA:kunnr TYPE kunnr.
  kunnr = '%'.
  zwft_common=>search_customer(
  EXPORTING werks = <io_class>->werks
  CHANGING kunnr = kunnr name = <head>-kunnr_name ).
  <head>-kunnr = kunnr.
  PERFORM frm_set_dynp_value USING '<HEAD>-KUNNR_NAME'
                                                      <head>-kunnr_name.
  SELECT SINGLE"绝大多数情况下供应商的币种唯一
  waers INTO <head>-waers
  FROM knvv
  WHERE kunnr = <head>-kunnr.
ENDFORM.

FORM frm_head_lifnr.
  zwft_common=>search_vendor(
  EXPORTING werks = <io_class>->werks
  CHANGING lifnr = <head>-lifnr name = <head>-lifnr_name ).
  IF <head>-lifnr IS NOT INITIAL.
    <head>-waers = zwft_single_read=>lfm1( lifnr = <head>-lifnr ekorg = <head>-ekorg  )-waers.

  ENDIF.
ENDFORM.

FORM frm_head_lifnr_f4.
  DATA:lifnr TYPE lifnr.
  lifnr = '%'.
  zwft_common=>search_vendor(
  EXPORTING werks = <io_class>->werks
  CHANGING lifnr = lifnr name = <head>-lifnr_name ).
  <head>-lifnr = lifnr.
  IF <head>-lifnr IS NOT INITIAL.
    <head>-waers = zwft_single_read=>lfm1( lifnr = <head>-lifnr ekorg = <head>-ekorg  )-waers.

  ENDIF.
  DATA dynpread TYPE TABLE OF dynpread.
  dynpread = VALUE #( ( fieldname = '<HEAD>-LIFNR_NAME' fieldvalue = <head>-lifnr_name )
                                       ( fieldname = '<HEAD>-MWSKZ' fieldvalue = <head>-mwskz )
                                       ( fieldname = '<HEAD>-WAERS' fieldvalue = <head>-waers )
                                       ) .

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynpread.
ENDFORM.

FORM frm_head_lifnr_bukrs.
  RANGES range_ktokk FOR lfa1-ktokk.

  range_ktokk[] = VALUE #( FOR wa IN <io_class>->dict
  WHERE ( fieldname = 'KTOKK' ) ( sign = 'I' option = 'EQ' low = wa-dict_value ) ).

  zwft_common=>search_vendor_bukrs(
  EXPORTING bukrs = <io_class>->head-bukrs
                       range_ktokk = range_ktokk[]
  CHANGING lifnr = <head>-lifnr name = <head>-lifnr_name ).
  IF <head>-lifnr IS NOT INITIAL.
    <head>-waers = zwft_single_read=>lfm1( lifnr = <head>-lifnr ekorg = <head>-ekorg  )-waers.
  ENDIF.
ENDFORM.

FORM frm_head_lifnr_f4_bukrs.
  RANGES range_ktokk FOR lfa1-ktokk.

  range_ktokk[] = VALUE #( FOR wa IN <io_class>->dict
  WHERE ( fieldname = 'KTOKK' ) ( sign = 'I' option = 'EQ' low = wa-dict_value ) ).

  DATA:lifnr TYPE lifnr.
  lifnr = '%'.
  zwft_common=>search_vendor_bukrs(
  EXPORTING bukrs = <io_class>->head-bukrs
                      range_ktokk = range_ktokk[]
  CHANGING lifnr = lifnr name = <head>-lifnr_name ).
  <head>-lifnr = lifnr.
  IF <head>-lifnr IS NOT INITIAL.
    <head>-waers = zwft_single_read=>lfm1( lifnr = <head>-lifnr ekorg = <head>-ekorg  )-waers.
  ENDIF.
  PERFORM frm_set_dynp_value USING '<HEAD>-LIFNR_NAME'
        <head>-lifnr_name.
ENDFORM.

FORM frm_page_trun USING i_ucomm.
  DATA(tabix) = sy-tabix.
  DATA to_afono TYPE zafono.
  IF zafo_basic=>lists IS NOT INITIAL.
    READ TABLE zafo_basic=>lists TRANSPORTING NO FIELDS WITH KEY afono = <head>-afono.
    CHECK sy-subrc EQ 0.
    CASE i_ucomm.
      WHEN '%PREV'.
        tabix = sy-tabix - 1.
      WHEN '%NEXT' .
        tabix = sy-tabix + 1.
      WHEN '%FIRST'.
        tabix = 1.
      WHEN '%LAST'.
        tabix = lines( zafo_basic=>lists ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.
    READ TABLE zafo_basic=>lists INTO DATA(list) INDEX tabix.
    CHECK sy-subrc EQ 0.
    to_afono = list-afono.
  ELSE.
    CASE i_ucomm.
      WHEN '%PREV'.
        SELECT MAX( afono ) INTO to_afono FROM zafo_head
          WHERE bustyp = <head>-bustyp
          AND afono < <head>-afono.
      WHEN '%NEXT' .
        SELECT MIN( afono ) INTO to_afono FROM zafo_head
          WHERE bustyp = <head>-bustyp
          AND afono > <head>-afono.
      WHEN '%FIRST'.
        SELECT MIN( afono ) INTO to_afono FROM zafo_head
          WHERE bustyp = <head>-bustyp.
      WHEN '%LAST'.
        SELECT MAX( afono ) INTO to_afono FROM zafo_head
          WHERE bustyp = <head>-bustyp.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDIF.

  CHECK to_afono IS NOT INITIAL.
  CHECK to_afono <> <head>-afono.
  PERFORM frm_free_object.
  zafo_class=>maintain( to_afono ).
ENDFORM.

FORM frm_free_object.
  CASE sy-dynnr .
    WHEN '0200'.
      IF g_container_200 IS BOUND.
        g_container_200->free( ).
        FREE g_container_200.
      ENDIF.
      IF <io_class>->falv_item IS BOUND.
        FREE <io_class>->falv_item.
      ENDIF.
      IF text_container IS BOUND.
        text_container->free( ).
        FREE text_container.
      ENDIF.

    WHEN '0110'.
      IF g_container_112 IS BOUND.
        g_container_112->free( ).
        FREE g_container_112.
      ENDIF.
      IF <io_class>->falv_ref IS BOUND.
        FREE <io_class>->falv_ref.
      ENDIF.

      ref_in_switch = icon_data_area_expand.
  ENDCASE.
ENDFORM.
