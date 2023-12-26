FORM frm_set_dict USING dict_index.
  DATA fieldname TYPE char30.
  DATA: BEGIN OF t_dict_value OCCURS 0,
          dict_name  TYPE name1,
          dict_value TYPE zafo_dict_value,
        END OF t_dict_value.

  fieldname = |DICT{ dict_index }|.
  DATA lt_dict TYPE zafo_tt_dict.
  lt_dict = VALUE #( FOR wa IN <io_class>->dict
                                WHERE ( fieldname = fieldname
                                               AND ( werks = <io_class>->werks OR werks = '' ) ) ( wa ) ).
  CHECK lt_dict[] IS NOT INITIAL.
  t_dict_value[] = CORRESPONDING #( lt_dict ).
  fieldname = |<IO_CLASS>->HEAD-DICT{ dict_index }|.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      value_org = 'S'
      retfield  = fieldname
    TABLES
      value_tab = t_dict_value.
ENDFORM.

FORM frm_mwskz_list.
  CALL FUNCTION 'FI_F4_MWSKZ'
    EXPORTING
      i_kalsm = 'TAXCN'
      i_xshow = <io_class>->readonly
    IMPORTING
      e_mwskz = <io_class>->head-mwskz.
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
      dynprofield = '<IO_CLASS>->HEAD-VTWEG'
    TABLES
      value_tab   = lt_tvtwt.
ENDFORM.

FORM frm_head_werks.
  IF <io_class>->head-werks IS INITIAL.
    CLEAR <io_class>->head-werks_name.
    RETURN.
  ELSE.
    SELECT SINGLE name1 FROM t001w
    WHERE werks = @<io_class>->head-werks
    INTO @<io_class>->head-werks_name.
    IF sy-subrc NE 0.
      MESSAGE i045 .
      CLEAR: <io_class>->head-werks,<io_class>->head-werks_name.
    ENDIF.
  ENDIF.
ENDFORM.

FORM frm_head_lgort.
  IF <io_class>->head-werks IS INITIAL.
    MESSAGE i005."请输入工厂
    CLEAR: <io_class>->head-lgort,<io_class>->head-lgort_name.
    RETURN.
  ENDIF.

  IF <io_class>->head-lgort IS INITIAL.
    CLEAR <io_class>->head-lgort_name.
    RETURN.
  ENDIF.

  SELECT SINGLE lgobe
  INTO <io_class>->head-lgort_name
  FROM t001l
  WHERE werks = <io_class>->head-werks
  AND lgort = <io_class>->head-lgort.
  IF sy-subrc NE 0.
    MESSAGE i115 ."仓库不存在
    CLEAR: <io_class>->head-lgort,<io_class>->head-lgort_name.
  ENDIF.
ENDFORM.

FORM frm_head_umwrk.
  IF <io_class>->head-umwrk IS INITIAL.
    CLEAR <io_class>->head-umwrk_name.
    RETURN.
  ELSE.
    SELECT SINGLE name1 FROM t001w
    WHERE werks = @<io_class>->head-umwrk
    INTO @<io_class>->head-umwrk_name.
    IF sy-subrc NE 0.
      MESSAGE i045 .
      CLEAR: <io_class>->head-umwrk,<io_class>->head-umwrk_name.
    ENDIF.
  ENDIF.
ENDFORM.

FORM frm_head_umlgo.
  IF <io_class>->head-umwrk IS INITIAL.
    MESSAGE i005."请输入工厂
    CLEAR: <io_class>->head-umlgo,<io_class>->head-umlgo_name.
    RETURN.
  ENDIF.

  IF <io_class>->head-umlgo IS INITIAL.
    CLEAR <io_class>->head-umlgo_name.
    RETURN.
  ENDIF.

  SELECT SINGLE lgobe
  INTO <io_class>->head-umlgo_name
  FROM t001l
  WHERE werks = <io_class>->head-umwrk
  AND lgort = <io_class>->head-umlgo.
  IF sy-subrc NE 0.
    MESSAGE i115 ."仓库不存在
    CLEAR: <io_class>->head-umlgo,<io_class>->head-umlgo_name.
  ENDIF.
ENDFORM.

FORM frm_head_bukrs.
  IF <io_class>->head-bukrs IS INITIAL.
    CLEAR <io_class>->head-bukrs_name.
  ENDIF.

  SELECT SINGLE butxt FROM t001
  WHERE bukrs = @<io_class>->head-bukrs
  INTO @<io_class>->head-bukrs_name.
  IF sy-subrc NE 0.
    CLEAR: <io_class>->head-bukrs,<io_class>->head-bukrs_name.
  ENDIF.
ENDFORM.

FORM frm_head_kostl.
  SELECT SINGLE ktext INTO <io_class>->head-kostl_name
  FROM cskt
  WHERE spras = sy-langu
  AND kostl = <io_class>->head-kostl.
ENDFORM.


FORM frm_lgort_list.
  DATA: return_tab TYPE TABLE OF ddshretval.
  SELECT lgort,lgobe AS name
  FROM t001l WHERE werks = @<io_class>->head-werks
  INTO TABLE @DATA(lt_t001l).
  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'LGORT' CHANGING table = lt_t001l ).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
      dynprofield = '<IO_CLASS>->HEAD-LGORT'
      retfield    = 'LGORT'
    TABLES
      value_tab   = lt_t001l
      return_tab  = return_tab.
  READ TABLE return_tab INTO DATA(l_return) INDEX 1.
  CHECK sy-subrc EQ 0.
  <io_class>->head-lgort = l_return-fieldval.
  READ TABLE lt_t001l INTO DATA(ls_t001l) WITH KEY lgort = <io_class>->head-lgort.
  CHECK sy-subrc EQ 0.
  <io_class>->head-lgort_name = ls_t001l-name.
  PERFORM frm_set_dynp_value USING '<IO_CLASS>->HEAD-LGORT_NAME'
                                                               <io_class>->head-lgort_name.
ENDFORM.

FORM frm_umlgo_list.
  DATA: return_tab TYPE TABLE OF ddshretval.
  SELECT lgort,lgobe AS name
  FROM t001l WHERE werks = @<io_class>->head-werks
  INTO TABLE @DATA(lt_t001l).

  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'LGORT' CHANGING table = lt_t001l ).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
      dynprofield = '<IO_CLASS>->HEAD-UMLGO'
      retfield    = 'LGORT'
    TABLES
      value_tab   = lt_t001l
      return_tab  = return_tab.
  READ TABLE return_tab INTO DATA(l_return) INDEX 1.
  CHECK sy-subrc EQ 0.
  <io_class>->head-umlgo = l_return-fieldval.
  READ TABLE lt_t001l INTO DATA(ls_t001l) WITH KEY lgort = <io_class>->head-umlgo.
  CHECK sy-subrc EQ 0.
  <io_class>->head-umlgo_name = ls_t001l-name.
  PERFORM frm_set_dynp_value USING '<IO_CLASS>->HEAD-UMLGO_NAME'
        <io_class>->head-umlgo_name.
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
    WHERE bukrs = @<io_class>->head-bukrs.
  <io_class>->value_filter_by_dict( EXPORTING fieldname = 'BUKRS' CHANGING table = lt_csks ).
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

FORM frm_tmodel_list.
  DATA: return_tab TYPE TABLE OF ddshretval.

  SELECT
    tmodel
    FROM  zafo_notes_allo
WHERE bustyp = @<io_class>->bustype-bustyp
    INTO TABLE @DATA(lt_model).
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      value_org   = 'S'
      dynprofield = '<IO_CLASS>->HEAD-TMODEL'
      retfield    = 'TMODEL'
    TABLES
      value_tab   = lt_model
      return_tab  = return_tab.

  CHECK <io_class>->readonly IS INITIAL.
  READ TABLE return_tab INTO DATA(l_return) INDEX 1.
  CHECK sy-subrc EQ 0.
  <io_class>->head-tmodel = l_return-fieldval.

  CHECK <io_class>->head-tmodel IS NOT INITIAL.

  SELECT line FROM zafo_notes
     WHERE tmodel = @<io_class>->head-tmodel
     ORDER BY line_id
     INTO TABLE @<io_class>->text_lines.

  IF sy-subrc EQ 0.
    <io_class>->editor->set_text_as_stream(  text = <io_class>->text_lines ) .
  ENDIF.
ENDFORM.

FORM frm_set_dynp_value USING i_field
                                                i_value .
  DATA field TYPE dynpread-fieldname.
  DATA value TYPE dynpread-fieldvalue.
  field = i_field.
  value = i_value.
  CALL FUNCTION 'SET_DYNP_VALUE'
    EXPORTING
      i_field = field
      i_repid = sy-repid
      i_dynnr = sy-dynnr
      i_value = value.
ENDFORM.

FORM frm_head_kunnr.
  zwft_common=>search_customer(
  EXPORTING werks = <io_class>->werks
  CHANGING kunnr = <io_class>->head-kunnr name = <io_class>->head-kunnr_name ).

  SELECT SINGLE"绝大多数情况下供应商的币种唯一
  waers INTO <io_class>->head-waers
  FROM knvv
  WHERE kunnr = <io_class>->head-kunnr.
ENDFORM.

FORM frm_head_kunnr_f4.
  DATA:kunnr TYPE kunnr.
  kunnr = '%'.
  zwft_common=>search_customer(
  EXPORTING werks = <io_class>->werks
  CHANGING kunnr = kunnr name = <io_class>->head-kunnr_name ).
  <io_class>->head-kunnr = kunnr.
  PERFORM frm_set_dynp_value USING '<IO_CLASS>->HEAD-KUNNR_NAME'
                                                      <io_class>->head-kunnr_name.
  SELECT SINGLE"绝大多数情况下供应商的币种唯一
  waers INTO <io_class>->head-waers
  FROM knvv
  WHERE kunnr = <io_class>->head-kunnr.
ENDFORM.

FORM frm_head_lifnr.
  zwft_common=>search_vendor(
  EXPORTING werks = <io_class>->werks
  CHANGING lifnr = <io_class>->head-lifnr name = <io_class>->head-lifnr_name ).
  IF <io_class>->head-lifnr IS NOT INITIAL.
    SELECT SINGLE"绝大多数情况下供应商的币种唯一
    waers INTO <io_class>->head-waers
    FROM lfm1
    WHERE lifnr = <io_class>->head-lifnr.
  ENDIF.
ENDFORM.

FORM frm_head_lifnr_f4.
  DATA:lifnr TYPE lifnr.
  lifnr = '%'.
  zwft_common=>search_vendor(
  EXPORTING werks = <io_class>->werks
  CHANGING lifnr = lifnr name = <io_class>->head-lifnr_name ).
  <io_class>->head-lifnr = lifnr.
  IF <io_class>->head-lifnr IS NOT INITIAL.
    SELECT SINGLE"绝大多数情况下供应商的币种唯一
    waers INTO <io_class>->head-waers
    FROM lfm1
    WHERE lifnr = <io_class>->head-lifnr.
  ENDIF.
  PERFORM frm_set_dynp_value USING '<IO_CLASS>->HEAD-LIFNR_NAME'
                                                                <io_class>->head-lifnr_name.

ENDFORM.
