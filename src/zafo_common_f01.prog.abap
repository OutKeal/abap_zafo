FORM frm_set_dict USING dict_index.
  DATA fieldname TYPE char30.
  DATA: BEGIN OF t_dict_value OCCURS 0,
          dict_name  TYPE name1,
          dict_value TYPE zafo_dict_value,
        END OF t_dict_value.

  fieldname = |DICT{ dict_index }|.
  DATA lt_dict TYPE zafo_tt_dict.
  lt_dict = VALUE #( FOR wa IN <io_class>->dict
                                WHERE ( fieldname = fieldname ) ( wa ) ).
  t_dict_value[] = CORRESPONDING #( lt_dict ).
  CHECK t_dict_value[] IS NOT INITIAL.
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

FORM frm_lgort_list.
  DATA: return_tab TYPE TABLE OF ddshretval.
  SELECT lgort,lgobe
  FROM t001l WHERE werks = @<io_class>->head-werks
  INTO TABLE @DATA(lt_t001l).
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
  <io_class>->head-lgort_name = ls_t001l-lgobe.
  PERFORM frm_set_dynp_value USING '<IO_CLASS>->HEAD-LGORT_NAME'
                                                               <io_class>->head-lgort_name.
ENDFORM.

FORM frm_umlgo_list.
  DATA: return_tab TYPE TABLE OF ddshretval.
  SELECT lgort,lgobe
  FROM t001l WHERE werks = @<io_class>->head-werks
  INTO TABLE @DATA(lt_t001l).
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
  <io_class>->head-umlgo_name = ls_t001l-lgobe.
  PERFORM frm_set_dynp_value USING '<IO_CLASS>->HEAD-UMLGO_NAME'
        <io_class>->head-umlgo_name.
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
