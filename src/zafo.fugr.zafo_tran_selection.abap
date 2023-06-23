FUNCTION zafo_tran_selection.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      UT_LINE STRUCTURE  ZAFO_SLINE OPTIONAL
*"----------------------------------------------------------------------

  CHECK ut_line[] IS NOT INITIAL.

  PERFORM frm_set_selection TABLES ut_line USING 'P_TYP' .
  PERFORM frm_set_selection TABLES ut_line USING 'P_AFONO' .
  PERFORM frm_set_selection TABLES ut_line USING 'P_WERKS' .

  PERFORM frm_set_selection TABLES ut_line USING 'S_LGORT'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_ZZPINO'   .
  PERFORM frm_set_selection TABLES ut_line USING 'S_ZPPDHD'   .
  PERFORM frm_set_selection TABLES ut_line USING 'S_KUNNR'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_ZZWLLY'   .
  PERFORM frm_set_selection TABLES ut_line USING 'S_BSART'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_EKGRP'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_EBELN'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_LIFNR'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_POTEXT'   .
  PERFORM frm_set_selection TABLES ut_line USING 'S_ZNAME1'   .
  PERFORM frm_set_selection TABLES ut_line USING 'S_MATNR'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_MAKTX'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_MTART'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_CATE1'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_CATE2'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_IDNLF'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_QCNO'     .
  PERFORM frm_set_selection TABLES ut_line USING 'S_AFONO'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_STATUS'   .
  PERFORM frm_set_selection TABLES ut_line USING 'S_ERNAM'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_ERDAT'    .
  PERFORM frm_set_selection TABLES ut_line USING 'S_BUDAT'    .

ENDFUNCTION.


FORM frm_set_selection TABLES ut_line STRUCTURE zafo_sline USING name.

  DATA: dyn_table TYPE REF TO data.
  DATA: dyn_wa TYPE REF TO data.
  DATA: val_field TYPE char40.
  DATA: txt_field TYPE char40.

  DATA: tabname TYPE char20.
  DATA: strname TYPE char20.
  DATA: fieldname TYPE fieldname.
  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any,
                 <txt_field> TYPE any,
                 <val_field> TYPE any.

  IF name+0(1) = 'P'.
    ASSIGN (name) TO <txt_field>.
    CHECK sy-subrc EQ 0.

    CLEAR <txt_field>.

    READ TABLE ut_line WITH KEY name = name.
    IF sy-subrc EQ 0.
      <txt_field> = ut_line-line.
    ENDIF.

  ELSEIF name+(1) = 'S'.

    strname = name.
    CONCATENATE name '[]' INTO tabname.

    ASSIGN (tabname) TO <dyn_table>.
    CHECK sy-subrc EQ 0.

    ASSIGN (strname) TO <dyn_wa>.
    CHECK sy-subrc EQ 0.

    CLEAR <dyn_table>.
    CLEAR <dyn_wa>.

    LOOP AT ut_line WHERE name = name.
      <dyn_wa> = ut_line-line.
      APPEND <dyn_wa> TO <dyn_table>.
      CLEAR <dyn_wa>.
    ENDLOOP.


  ENDIF.


ENDFORM.
