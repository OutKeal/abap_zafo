*&---------------------------------------------------------------------*
*& 包含               ZMMR0110_EXCEL
*&---------------------------------------------------------------------*

TYPE-POOLS: soi.
TYPE-POOLS: ole2.
DATA:excel TYPE ole2_object.
DATA lv_date TYPE char10.
* SAP Desktop Office Integration Interfaces
DATA: container    TYPE REF TO cl_gui_custom_container,
      control      TYPE REF TO i_oi_container_control,
      bds_instance TYPE REF TO cl_bds_document_set,
      document     TYPE REF TO i_oi_document_proxy,
      spreadsheet  TYPE REF TO i_oi_spreadsheet,
      error        TYPE REF TO i_oi_error.

DATA: g_docking_container_300 TYPE REF TO cl_gui_docking_container,
      g_splitter_300          TYPE REF TO cl_gui_splitter_container,
      g_container_300         TYPE REF TO cl_gui_container.
DATA:
  g_template_url(256) TYPE c.

DATA: gr_bds_documents TYPE REF TO cl_bds_document_set,
      g_classname      TYPE sbdst_classname,
      g_classtype      TYPE sbdst_classtype,
      g_objectkey      TYPE sbdst_object_key,
      g_doc_components TYPE sbdst_components,
      g_doc_signature  TYPE sbdst_signature.
* template url
DATA: gt_bds_uris TYPE sbdst_uri,
      gs_bds_url  LIKE LINE OF gt_bds_uris.

* spreadsheet interface structures for Excel data input
DATA: cellitem TYPE soi_generic_item.
DATA: rangeitem TYPE soi_range_item.
DATA: ranges TYPE soi_range_list.
DATA: ranges_output TYPE soi_range_list.
DATA: excel_output TYPE soi_generic_table.
DATA: excel_output_wa TYPE soi_generic_item.
DATA: excel_input TYPE soi_generic_table.
DATA: excel_input_wa TYPE soi_generic_item.
DATA: initialized(1), retcode TYPE soi_ret_string.




MODULE create_basic_objects OUTPUT.

  CHECK document IS  INITIAL.

  PERFORM initial_excel.

*  PERFORM set_excel_protect.

ENDMODULE.                             " CREATE_BASIC_OBJECTS  INPUT

FORM get_model_url.

  CLEAR: g_doc_components ,g_doc_signature ,gt_bds_uris.

  g_classname = 'ZAFO'.
  g_classtype = 'OT'.

  SELECT SINGLE * INTO gs_model_h FROM zafo_qc_model_h
    WHERE qcmodel = <gs_item>-qcmodel.

  IF sy-subrc NE 0 OR  gs_model_h-qctype IS INITIAL.
    MESSAGE '模板确认错误,请检查配置' TYPE 'E'.
  ENDIF.

  CASE gs_model_h-qctype.
    WHEN 'A'.
      g_objectkey = 'ZAFO_QC_ML'.
      CLEAR gt_model_t[].
      SELECT * FROM zafo_qc_model_t INTO TABLE gt_model_t
        WHERE qcmodel = <gs_item>-qcmodel.
    WHEN 'B'.
      g_objectkey = 'ZAFO_QC_FL'.
      CLEAR gt_model_i[].
      SELECT * FROM zafo_qc_model_i INTO TABLE gt_model_i
        WHERE qcmodel = <gs_item>-qcmodel.
  ENDCASE.



  CALL METHOD cl_bds_document_set=>get_info
    EXPORTING
      classname  = g_classname
      classtype  = g_classtype
      object_key = g_objectkey
    CHANGING
      components = g_doc_components
      signature  = g_doc_signature.

  CALL METHOD cl_bds_document_set=>get_with_url
    EXPORTING
      classname  = g_classname
      classtype  = g_classtype
      object_key = g_objectkey
    CHANGING
      uris       = gt_bds_uris
      signature  = g_doc_signature.
  FREE gr_bds_documents.
  READ TABLE gt_bds_uris INTO gs_bds_url INDEX 1.
  g_template_url = gs_bds_url-uri.

ENDFORM.


FORM create_excel_objects.
* get the sap doi interface references.

* first get the SAP DOI i_oi_container_control interface

  CALL METHOD c_oi_container_control_creator=>get_container_control
    IMPORTING
      control = control
      error   = error
      retcode = retcode.

* check no errors occured
  PERFORM handle_error USING 'X'.
* create a control container as defined in dynpro 100
*  CREATE OBJECT container
*    EXPORTING
*      container_name = 'CONTAINER'.


  IF g_docking_container_300 IS INITIAL.

    CREATE OBJECT g_docking_container_300
      EXPORTING
        style     = cl_gui_control=>ws_child
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = g_docking_container_300->dock_at_bottom
        lifetime  = cl_gui_control=>lifetime_imode
        extension = '3000'
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT g_splitter_300
      EXPORTING
        parent  = g_docking_container_300
        rows    = 1
        columns = 1.
    g_container_300 = g_splitter_300->get_container( row = 1 column = 1 ).


  ENDIF.

  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

* specified above and tell it to run Excel in-place
  CALL METHOD control->init_control
    EXPORTING
      r3_application_name      = 'SAP'     "#EC NOTEXT
      inplace_show_toolbars    = ''
      inplace_enabled          = 'X'
      inplace_scroll_documents = 'X'
      parent                   = g_container_300
      register_on_close_event  = 'X'
      register_on_custom_event = 'X'
*     no_flush                 = 'X'
    IMPORTING
      error                    = error
      retcode                  = retcode.
  PERFORM handle_error USING 'X'.
* save error object in collection
* ask the SAP DOI container for a i_oi_document_proxy for Excel
  CALL METHOD control->get_document_proxy
    EXPORTING
      document_type      = 'Excel.Sheet.8'
*     no_flush           = 'X'
      register_container = 'X'
      document_format    = 'OLE'
    IMPORTING
      document_proxy     = document
      error              = error
      retcode            = retcode.

  CALL METHOD document->open_document
    EXPORTING
      open_inplace = 'X'
      document_url = g_template_url
    IMPORTING
      error        = error
      retcode      = retcode.

  PERFORM handle_error USING 'X'.


* and then create a new Excel sheet
*  CALL METHOD document->create_document
*    EXPORTING
*      open_inplace   = 'X'
*      document_title =
*                       'R/3 table contents in Excel' "#EC NOTEXT
*      no_flush       = 'X'
*    IMPORTING
*      error          = ERROR.
*  PERFORM HANDLE_ERROR USING 'X'.

* check if our document proxy can serve a spreadsheet interface  data:
  DATA: has TYPE i.

  CALL METHOD document->has_spreadsheet_interface
*    EXPORTING
*      no_flush     = 'X'
    IMPORTING
      is_available = has
      error        = error
      retcode      = retcode.

  PERFORM handle_error USING 'X'.

  CALL METHOD document->get_spreadsheet_interface
    EXPORTING
      no_flush        = ' '
    IMPORTING
      sheet_interface = spreadsheet
      error           = error
      retcode         = retcode.

  PERFORM handle_error USING 'X'.
* now loop through error collection because
* Get_spreadsheet_interface flushed and synchronized
* the automation queue !
  DATA:ls_name TYPE char40.

  ls_name = <gs_item>-qcno && '-' && <gs_item>-qcnr && '质检报告' .
  CALL METHOD spreadsheet->set_sheet_name
    EXPORTING
      newname = ls_name
      oldname = 'Sheet1'
    IMPORTING
      error   = error
      retcode = retcode.
  PERFORM handle_error USING 'X'.

  CALL METHOD spreadsheet->select_sheet
    EXPORTING
      name    = ls_name
*     no_flush = 'X'
    IMPORTING
      error   = error
      retcode = retcode.
  PERFORM handle_error USING 'X'.
*  CALL METHOD spreadsheet->print
*    EXPORTING
**     no_flush = ' '
*      name  = 'Sheet1'
*    IMPORTING
*      error = errors
**     retcode  =
*    .

ENDFORM.


FORM initial_data.
  IF <gs_item>-datum_qc IS INITIAL.
    <gs_item>-datum_qc = sy-datum.
  ENDIF.
  IF <gs_item>-iqc IS INITIAL.
    SELECT SINGLE name_last
      INTO <gs_item>-iqc
      FROM user_addr WHERE bname = sy-uname.
  ENDIF.

  CLEAR ranges.
  CLEAR excel_input.

  DATA:ls_text TYPE char40.

  PERFORM insert_ranges USING 'TITEL'  gs_model_h-name1.

  ls_text = <gs_item>-qcno && '-' && <gs_item>-qcnr.
  PERFORM insert_ranges USING 'QCNO'    ls_text.

  PERFORM insert_ranges USING 'AFONO'    <gs_item>-afono.

  PERFORM insert_ranges USING 'LIFNR_NAME'    <gs_item>-lifnr_name.

  PERFORM insert_ranges USING 'MAKTX' <gs_item>-maktx.

  PERFORM insert_ranges USING 'ZCOLOR_TEXT'  <gs_item>-zcolor_text.

  PERFORM insert_ranges USING 'MENGE_GR'    <gs_item>-menge_gr.

  PERFORM insert_ranges USING 'EBELN'    <gs_item>-ebeln.

  PERFORM sap_to_excel_date USING <gs_item>-datum_gr CHANGING lv_date.
  PERFORM insert_ranges USING 'DATUM_GR'    lv_date.

  PERFORM sap_to_excel_date USING <gs_item>-datum_qc CHANGING lv_date.
  PERFORM insert_ranges USING 'DATUM_QC'    lv_date.
  PERFORM insert_ranges USING 'DATUM_QC1'    lv_date.

  PERFORM insert_ranges USING 'MENGE_QC'    <gs_item>-menge_qc.

  PERFORM insert_ranges USING 'MATNR'    <gs_item>-matnr.

  PERFORM insert_ranges USING 'FASTNESS'    <gs_item>-fastness.

  PERFORM insert_ranges USING 'PULL_FORCE'    <gs_item>-pull_force.

  PERFORM insert_ranges USING 'SHRINKAGE'    <gs_item>-shrinkage.

  PERFORM insert_ranges USING 'SCORE'    <gs_item>-score.

  PERFORM insert_ranges USING 'KMS'    <gs_item>-kms.

  PERFORM insert_ranges USING 'QC_RESULT'    <gs_item>-qc_result.

  IF <gs_item>-iqc IS INITIAL.
    SELECT SINGLE name
      INTO <gs_item>-iqc
      FROM zapp_addr
      WHERE person = sy-uname.
  ENDIF.

  PERFORM insert_ranges USING 'IQC'    <gs_item>-iqc.

  PERFORM insert_ranges USING 'QC_LEAD'    <gs_item>-qc_lead.

  PERFORM insert_ranges USING 'APPROVER'    <gs_item>-approver.

  PERFORM insert_ranges USING 'APP_DATE'    <gs_item>-app_date.

  PERFORM insert_ranges USING 'PU_RESULT'    <gs_item>-pu_result.

  PERFORM insert_ranges USING 'IPU'    <gs_item>-ipu.

  PERFORM insert_ranges USING 'PU_DATE'    <gs_item>-pu_date.

  IF gs_model_h-qctype = 'A'.

    PERFORM insert_ranges USING 'KMS'  <gs_item>-dec_per_100m.

    PERFORM insert_ranges USING 'SCORE'  <gs_item>-dec_per_100m.

    PERFORM insert_ranges USING 'IDNLF'  <gs_item>-idnlf.

    PERFORM insert_ranges USING 'JCOUNT'    <gs_item>-jcount.

    PERFORM insert_ranges USING 'WIDTH'    <gs_item>-width.

    PERFORM insert_ranges USING 'WEIGHT'    <gs_item>-weight.

    PERFORM insert_ranges USING 'ZVAT_NUB'    <gs_item>-zvat_nub.

    PERFORM insert_ranges USING 'TWIST'    <gs_item>-twist.

  ENDIF.

  CALL METHOD spreadsheet->set_ranges_data
    EXPORTING
      ranges   = ranges[]
      contents = excel_input[]
    IMPORTING
      error    = error
      retcode  = retcode.
  .
  PERFORM handle_error USING 'X'.

  IF gs_model_h-qctype = 'A'.
    PERFORM frm_set_ml_table.
  ENDIF.

  IF gs_model_h-qctype = 'B'.
    PERFORM frm_set_fl_table.
  ENDIF.

ENDFORM.


FORM frm_change_cell .

  CLEAR : excel_input_wa, excel_input[] .
  CLEAR: rangeitem,ranges[].

  rangeitem-name    = 'cell'.
  rangeitem-rows    = 1.
  rangeitem-columns = 1.
  APPEND rangeitem TO ranges.

  excel_input_wa-row = 1.
  excel_input_wa-column = 1.
  excel_input_wa-value = '《面料进场检验规程》'.
  APPEND excel_input_wa TO excel_input.

  CALL METHOD spreadsheet->insert_range_dim
    EXPORTING
      name     = 'cell'
      top      = 5
      left     = 8
      rows     = 1
      no_flush = 'X'
      columns  = 1.

  CALL METHOD spreadsheet->set_ranges_data
    EXPORTING
      ranges   = ranges[]
      contents = excel_input[]
      no_flush = 'X'.

ENDFORM.


FORM sap_to_excel_date USING i_date CHANGING c_date.

  c_date = i_date+0(4) && '/'
        && i_date+4(2) && '/'
        && i_date+6(2).

  REPLACE ALL OCCURRENCES OF '-' IN c_date WITH '0'.

ENDFORM.


FORM handle_error USING ex.

  IF retcode NE 'OK'.
    CALL METHOD error->get_message
      IMPORTING
        message_id     = sy-msgid
        message_number = sy-msgno
        param1         = sy-msgv1
        param2         = sy-msgv2
        param3         = sy-msgv3
        param4         = sy-msgv4.
    CLEAR retcode.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1
                                                      sy-msgv2
                                                      sy-msgv3
                                                      sy-msgv4.
    IF ex = 'X'.

      PERFORM release_objects.
      LEAVE TO SCREEN 0.

    ENDIF.
  ENDIF.
ENDFORM.


FORM frm_set_ml_table.

  DATA:ls_menge TYPE menge_d.
  CLEAR gt_qc_scol[].
  SELECT * INTO TABLE gt_qc_line FROM zafo_qc_line
    WHERE qcno = <gs_item>-qcno AND qcnr = <gs_item>-qcnr.

  IF sy-subrc EQ 0.
    LOOP AT gt_qc_line.
      gt_qc_scol-col01 = gt_qc_line-qcjnr.
      gt_qc_scol-col02 = gt_qc_line-zmm_ms.
      gt_qc_scol-col03 = gt_qc_line-zmm_mf1.
      gt_qc_scol-col04 = gt_qc_line-zmm_jb.
      gt_qc_scol-col05 = gt_qc_line-zmm_xw.
      gt_qc_scol-col06 = gt_qc_line-zmm_kz.
      gt_qc_scol-col07 = gt_qc_line-zmm_sg.
      gt_qc_scol-col08 = gt_qc_line-zmm_syh.
      gt_qc_scol-col09 = gt_qc_line-zmm_bz.

      IF gt_qc_line-numc1 IS NOT INITIAL.
        gt_qc_scol-col10 = gt_qc_line-numc1.
      ENDIF.
      IF gt_qc_line-numc2 IS NOT INITIAL.
        gt_qc_scol-col11 = gt_qc_line-numc2.
      ENDIF.
      IF gt_qc_line-numc3 IS NOT INITIAL.
        gt_qc_scol-col12 = gt_qc_line-numc3.
      ENDIF.
      IF gt_qc_line-numc4 IS NOT INITIAL.
        gt_qc_scol-col13 = gt_qc_line-numc4.
      ENDIF.
      IF gt_qc_line-numc_sum IS NOT INITIAL.
        gt_qc_scol-col14 = gt_qc_line-numc_sum.
      ENDIF.
      ls_menge = gt_qc_line-numc_sum / gt_qc_line-zmm_ms * 100.
      IF ls_menge IS NOT INITIAL.
        gt_qc_scol-col15 = ls_menge.
      ENDIF.
      gt_qc_scol-col16  = gt_qc_line-remark.

      APPEND gt_qc_scol.
      CLEAR gt_qc_scol.
    ENDLOOP.

  ENDIF.

  CALL METHOD spreadsheet->insert_one_table
    EXPORTING
      data_table = gt_qc_scol[]
      ddic_name  = 'ZAFO_QC_SCOL'
      rangename  = 'TAB'
*     no_flush   = 'X'
      wholetable = 'X'
    IMPORTING
      error      = error
      retcode    = retcode.
  PERFORM handle_error USING 'X'.

ENDFORM.


FORM frm_set_fl_table.
  CLEAR gt_qc_scol[].
  CLEAR gt_item_i.
  SELECT * INTO TABLE gt_item_i FROM zafo_qc_item_i
    WHERE qcno = <gs_item>-qcno AND qcnr = <gs_item>-qcnr
    ORDER BY qcnr.
  IF sy-subrc EQ 0.
    LOOP AT gt_item_i.
    ENDLOOP.
  ELSE.
    SELECT * INTO TABLE gt_model_i FROM zafo_qc_model_i WHERE qcmodel = <gs_item>-qcmodel
      ORDER BY afonr  .
    IF sy-subrc EQ 0.
      LOOP AT gt_model_i.
        MOVE-CORRESPONDING gt_model_i TO gt_item_i.
        APPEND gt_item_i.
        CLEAR gt_item_i.
      ENDLOOP.
    ENDIF.
  ENDIF.

  DATA(lv_line) = lines( gt_item_i[] ).
  DATA(line) = lv_line / 2.
  DATA:lv_tabix TYPE sy-tabix.

  LOOP AT gt_item_i.
    IF sy-tabix <= line.
      gt_qc_scol-col01 = gt_item_i-afonr   .
      gt_qc_scol-col02 = gt_item_i-qctxt   .
      gt_qc_scol-col03 = gt_item_i-menge   .
      gt_qc_scol-col04 = gt_item_i-percent .
      gt_qc_scol-col05 = gt_item_i-remark  .

      IF gt_item_i-menge IS INITIAL.
        CLEAR gt_qc_scol-col03.
      ENDIF.
      APPEND gt_qc_scol.
      CLEAR gt_qc_scol.
    ELSE.
      lv_tabix = sy-tabix - line.
      READ TABLE gt_qc_scol ASSIGNING FIELD-SYMBOL(<gs_scol>)  INDEX lv_tabix.
      IF sy-subrc EQ 0.
        <gs_scol>-col06 = gt_item_i-afonr   .
        <gs_scol>-col07 = gt_item_i-qctxt   .
        <gs_scol>-col08 = gt_item_i-menge   .
        <gs_scol>-col09 = gt_item_i-percent .
        <gs_scol>-col10 = gt_item_i-remark  .
        IF gt_item_i-menge IS INITIAL.
          CLEAR <gs_scol>-col08.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.
  CALL METHOD spreadsheet->insert_one_table
    EXPORTING
      data_table = gt_qc_scol[]
      ddic_name  = 'ZAFO_QC_SCOL'
      rangename  = 'TAB'
*     no_flush   = 'X'
      wholetable = 'X'
    IMPORTING
      error      = error
      retcode    = retcode.
  PERFORM handle_error USING 'X'.
ENDFORM.


FORM insert_ranges USING name value.
  rangeitem-name    = name.
  rangeitem-rows    = 1.
  rangeitem-columns = 1.
*  rangeitem-code    = 4.
  APPEND rangeitem TO ranges.
  CLEAR rangeitem.

  excel_input_wa-row = 1.
  excel_input_wa-column = 1.
  excel_input_wa-value = value.
  APPEND excel_input_wa TO excel_input.
  CLEAR excel_input_wa.
ENDFORM.

FORM set_excel_protect.
  CALL METHOD spreadsheet->protect
    EXPORTING
      protect = 'X'
*     no_flush = ' '
    IMPORTING
      error   = error
      retcode = retcode.
  PERFORM handle_error USING 'X'.
ENDFORM.




FORM release_objects.

  IF NOT document IS INITIAL.
    CALL METHOD document->close_document
      IMPORTING
        error   = error
        retcode = retcode.

    FREE document.
    IF retcode NE 'OK'.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.


  IF NOT control IS INITIAL.
    CALL METHOD control->destroy_control
      IMPORTING
        error   = error
        retcode = retcode.
    FREE control.
    IF retcode NE 'OK'.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.


  IF container IS NOT INITIAL.
    CALL METHOD container->free.
  ENDIF.

  IF spreadsheet IS NOT INITIAL.
    FREE spreadsheet.
  ENDIF.

*  PERFORM frm_unlock USING <fs_s0080>.
ENDFORM.

MODULE user_command_0300 INPUT.


  DATA:ok_code TYPE sy-ucomm.

  ok_code = sy-ucomm.

  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'ADD_ROW'.
      CALL METHOD document->execute_macro
        EXPORTING
          macro_string = '模块1.InsertRows'
          param_count  = 2
          param1       = 9
          param2       = 15
        IMPORTING
          error        = error
          retcode      = retcode.
      PERFORM handle_error USING ''.
      IF retcode <> 'OK'.
        RETURN.
      ENDIF.


    WHEN 'SAVE'.

      IF <gs_item>-qc_status = 'A' OR <gs_item>-qc_status = 'B'.
      ELSE.
        MESSAGE s000(afo) WITH  '当前状态无法重新保存'.
        RETURN.
      ENDIF.

      PERFORM get_ranges_data.
      IF retcode <> 'OK'.
        RETURN.
      ENDIF.

      PERFORM set_qc_state USING 'B'.

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
      APPEND gt_item_modify.
      CLEAR gt_item_modify.

      PERFORM frm_save_db.

      MESSAGE s000(zafo) WITH '保存成功'.

      PERFORM f_refresh_grid_alv USING g_grid_100_item.

*      PERFORM release_objects.
*
*      g_ucomm = '&OK'.
*      LEAVE TO SCREEN 0.
    WHEN 'PRINT'.

      PERFORM print_data.
*      PERFORM release_objects.
*      LEAVE TO SCREEN 0.

    WHEN 'SAVE_AS'.

      PERFORM save_as_data.
*      PERFORM release_objects.
*      LEAVE TO SCREEN 0.

    WHEN 'RELEASE'.


      IF <gs_item>-qc_status = 'A' OR <gs_item>-qc_status = 'B'
        OR <gs_item>-qc_status = 'C' OR <gs_item>-qc_status = 'E'.
      ELSE.
        MESSAGE s000(afo) WITH  '当前状态无法调整质检意见'.
        RETURN.
      ENDIF.

      PERFORM frm_release.

      IF <gs_item>-qc_status = 'C' OR <gs_item>-qc_status = 'E' OR <gs_item>-qc_status = 'B'.
      ELSE.
        MESSAGE s000(afo) WITH  '不支持调整为此状态'.
        RETURN.
      ENDIF.
      PERFORM set_qc_state USING <gs_item>-qc_status.


      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
      APPEND gt_item_modify.
      CLEAR gt_item_modify.

      PERFORM frm_save_db.

      PERFORM release_objects.

      PERFORM f_refresh_grid_alv USING g_grid_100_item.

      LEAVE TO SCREEN 0.

    WHEN 'ALLOW'.

      IF <gs_item>-qc_status = 'C' OR <gs_item>-qc_status = 'E'.
      ELSE.
        MESSAGE s000(afo) WITH  '只有质检盘点合格/不合格后才可以录入采购意见'.
        RETURN.
      ENDIF.

      DATA:lv_status TYPE char1.

      IF <gs_item>-qc_status = 'C'.
        lv_status = 'F'.
      ELSE.
        <gs_item>-qc_status = 'E'.
        lv_status = ''.
      ENDIF.

      PERFORM frm_allow USING lv_status .

      MOVE-CORRESPONDING <gs_item> TO gt_item_modify.
      APPEND gt_item_modify.
      CLEAR gt_item_modify.

      PERFORM frm_save_db.

      PERFORM release_objects.

      PERFORM f_refresh_grid_alv USING g_grid_100_item.

      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.


FORM frm_release.
  DATA:lv_title TYPE char40.
  DATA:ls_fieldname TYPE fieldname.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.
  FIELD-SYMBOLS <fs_value> TYPE any.
  CLEAR lt_flds.


  CLEAR ls_flds.
  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '质检判定结果'.
  ls_flds-fieldname = 'QC_RESULT'.
  ls_flds-comp_field = 'QC_RESULT'.
  ls_flds-value     = <gs_item>-qc_result.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds.

  CLEAR ls_flds.
  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '质检判定结果'.
  ls_flds-fieldname = 'QC_RESULT_T'.
  ls_flds-comp_field = 'QC_RESULT_T'.
  ls_flds-value     = '合格'.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds.
*  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
*  ls_flds-fieldtext = '质检员'.
*  ls_flds-comp_field = 'IQC'.
*  ls_flds-fieldname = 'IQC'.
*  ls_flds-field_obl = 'X'.
*  ls_flds-value     = <gs_item>-iqc.
*  APPEND ls_flds TO lt_flds.

  CLEAR ls_flds.
  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '扣码数量'.
  ls_flds-fieldname = 'KMS'.
  ls_flds-value     = <gs_item>-kms.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds.

  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '审签人'.
  ls_flds-comp_field = 'APPROVER'.
  ls_flds-fieldname = 'APPROVER'.
  IF <gs_item>-qc_lead IS INITIAL.
    SELECT SINGLE name_last
      INTO <gs_item>-qc_lead
      FROM user_addr
      WHERE bname = sy-uname.
  ENDIF.
  ls_flds-value     = <gs_item>-qc_lead.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds.


  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '审签日期'.
  ls_flds-fieldname = 'APP_DATE'.
  IF <gs_item>-app_date IS INITIAL OR <gs_item>-app_date = '0'.
    <gs_item>-app_date = sy-datum.
  ENDIF.
  ls_flds-value     = <gs_item>-app_date.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds.


  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '单据状态'.
  ls_flds-fieldname = 'QC_STATUS'.
  ls_flds-value     = 'C'.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds .

  lv_title = '质检单号:' && <gs_item>-qcno && <gs_item>-qcnr && '质检部审批'.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = lv_title
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  CHECK p_gv_ret_code <> 'A'.

  LOOP AT lt_flds INTO ls_flds.
    ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE <gs_item> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = ls_flds-value.
    ENDIF.
  ENDLOOP.


  PERFORM frm_set_icon
           USING <gs_item>-qc_status
        CHANGING <gs_item>-icon
                 <gs_item>-text.

  <gs_item>-modat = sy-datum.
  <gs_item>-mozet = sy-uzeit.
  <gs_item>-monam = sy-uname.


*  IF gs_s0080-mlzjjg = '1' AND gs_s0080-kmsl IS NOT INITIAL.
*    MESSAGE e000(zmm01) WITH '质检结果入库，不能有扣码数量'.
*  ENDIF.
*  IF gs_s0080-mlzjjg = '3' AND gs_s0080-kmsl IS NOT INITIAL.
*    MESSAGE e000(zmm01) WITH '质检结果退货，不能有扣码数量'.
*  ENDIF.
*  IF gs_s0080-kmsl IS INITIAL.
*    gs_s0080-kmsl = 0.
*  ENDIF.

ENDFORM.


FORM frm_allow USING status.
  DATA:lv_title TYPE char40.
  DATA:ls_fieldname TYPE fieldname.
  DATA:lt_flds TYPE TABLE OF sval.
  DATA:ls_flds TYPE sval.
  DATA: p_gv_ret_code TYPE c.
  FIELD-SYMBOLS <fs_value> TYPE any.
  CLEAR lt_flds.


  CLEAR ls_flds.
  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '采购判定意见'.
  ls_flds-fieldname = 'PU_RESULT'.
  ls_flds-comp_field = 'PU_RESULT'.
  ls_flds-value     = <gs_item>-pu_result.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds.


  IF <gs_item>-ipu IS INITIAL.
    SELECT SINGLE name_last
      INTO <gs_item>-ipu
      FROM user_addr
      WHERE bname = sy-uname.
  ENDIF.

  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '采购审批人'.
  ls_flds-comp_field = 'IPU'.
  ls_flds-fieldname = 'IPU'.
  ls_flds-field_obl = 'X'.
  ls_flds-value     = <gs_item>-ipu.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds.

  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '审签日期'.
  ls_flds-fieldname = 'PU_DATE'.
  IF <gs_item>-app_date IS INITIAL OR <gs_item>-app_date = '0'.
    <gs_item>-pu_date = sy-datum.
  ENDIF.
  ls_flds-value     = <gs_item>-app_date.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds.

  ls_flds-tabname   = 'ZAFO_QC_ITEM'.
  ls_flds-fieldtext = '单据状态'.
  ls_flds-fieldname = 'QC_STATUS'.

  ls_flds-value     = status.
  ls_flds-field_attr     = '02'.
  APPEND ls_flds TO lt_flds.
  CLEAR ls_flds .


  lv_title = '质检单号:' && <gs_item>-qcno && <gs_item>-qcnr && '采购部审批'.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = lv_title
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  CHECK p_gv_ret_code <> 'A'.

  LOOP AT lt_flds INTO ls_flds.
    ASSIGN COMPONENT ls_flds-fieldname OF STRUCTURE <gs_item> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = ls_flds-value.
    ENDIF.
  ENDLOOP.

  <gs_item>-modat = sy-datum.
  <gs_item>-mozet = sy-uzeit.
  <gs_item>-monam = sy-uname.

ENDFORM.


MODULE excel_exit INPUT.

  CLEAR gt_qc_line.
  CLEAR gt_qc_line_4fz.
  CLEAR gt_qc_line_4fz_dis.
  REFRESH gt_qc_line.
  REFRESH gt_qc_line_4fz.
  REFRESH gt_qc_line_4fz_dis.

  PERFORM release_objects.

  LEAVE TO SCREEN 0.
ENDMODULE.


FORM get_ranges_data.
  FIELD-SYMBOLS:<fs_value> TYPE any .

  CLEAR ranges_output.
  ranges_output = ranges.



  CALL METHOD spreadsheet->get_ranges_data
    EXPORTING
      no_flush = ''
    IMPORTING
      contents = excel_output
      error    = error
      retcode  = retcode
    CHANGING
      ranges   = ranges_output.

  PERFORM handle_error USING ''.
  IF retcode <> 'OK'.
    RETURN.
  ENDIF.

  LOOP AT ranges_output INTO rangeitem.
    READ TABLE excel_output INTO excel_output_wa INDEX sy-tabix.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT rangeitem-name OF STRUCTURE <gs_item> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF rangeitem-name+0(3) = 'DAT'.
          PERFORM excel_to_sap_date CHANGING excel_output_wa-value.
        ENDIF.
        <fs_value> = excel_output_wa-value.
      ENDIF.
    ENDIF.
  ENDLOOP.
*


  CLEAR ranges_output.
  rangeitem-name    = 'TAB'.
  rangeitem-rows    = 1.
  rangeitem-columns = 1.
*  rangeitem-code    = 4.
  APPEND rangeitem TO ranges_output.
  CLEAR rangeitem.
  CALL METHOD spreadsheet->get_ranges_data
    EXPORTING
      no_flush = ''
    IMPORTING
      contents = excel_output
      error    = error
      retcode  = retcode
    CHANGING
      ranges   = ranges_output.
  IF retcode <> 'OK'.
    RETURN.
  ENDIF.
  PERFORM handle_error USING ''.

  IF gs_model_h-qctype = 'A'.
    PERFORM frm_get_ml_table.
  ELSEIF gs_model_h-qctype = 'B'.
    PERFORM frm_get_fl_table.
  ENDIF.

ENDFORM.

FORM excel_to_sap_date CHANGING c_date.
  DATA lv_year  TYPE char10.
  DATA lv_month TYPE char10.
  DATA lv_day   TYPE char10.

  FIND '/' IN c_date.
  IF sy-subrc EQ 0.
    SPLIT c_date AT '/' INTO lv_year lv_month lv_day.
  ENDIF.
  FIND '-' IN c_date.
  IF sy-subrc EQ 0.
    SPLIT c_date AT '-' INTO lv_year lv_month lv_day.
  ENDIF.
  FIND '.' IN c_date.
  IF sy-subrc EQ 0.
    SPLIT c_date AT '.' INTO lv_year lv_month lv_day.
  ENDIF.
  IF strlen( lv_month ) = 1.
    lv_month = '0' && lv_month.
  ENDIF.

  IF strlen( lv_day ) = 1.
    lv_day = '0' && lv_day.
  ENDIF.

  c_date = lv_year && lv_month && lv_day.

ENDFORM.

FORM frm_get_ml_table.
*
  DATA:lt_dd03l TYPE TABLE OF dd03l WITH HEADER LINE  .
  FIELD-SYMBOLS:<fs_value> TYPE any.
  CLEAR gt_qc_scol[].

  SELECT * INTO TABLE lt_dd03l FROM dd03l WHERE tabname = 'ZAFO_QC_SCOL'.

  SORT  lt_dd03l BY position.
  CLEAR gt_qc_scol[].
  CLEAR gt_qc_scol.
  LOOP AT excel_output INTO excel_output_wa.
    READ TABLE lt_dd03l WITH KEY position = excel_output_wa-column.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT lt_dd03l-fieldname OF STRUCTURE gt_qc_scol TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = excel_output_wa-value.
      ENDIF.
    ENDIF.
    AT END OF row.
      APPEND gt_qc_scol.
      CLEAR gt_qc_scol.
    ENDAT.
  ENDLOOP.

  CLEAR gt_item_t[].
  LOOP AT gt_qc_scol.
    MOVE-CORRESPONDING gt_qc_scol TO gt_item_t .
    gt_item_t-qcno = <gs_item>-qcno.
    gt_item_t-qcnr = <gs_item>-qcnr.
    gt_item_t-afonr = sy-tabix.
    APPEND gt_item_t.
    CLEAR gt_item_t.
  ENDLOOP.
ENDFORM.

FORM frm_get_fl_table.

  DATA:lt_dd03l TYPE TABLE OF dd03l WITH HEADER LINE  .
  FIELD-SYMBOLS:<fs_value> TYPE any.
  CLEAR gt_qc_scol[].

  SELECT * INTO TABLE lt_dd03l FROM dd03l WHERE tabname = 'ZAFO_QC_SCOL'.

  SORT  lt_dd03l BY position.
  CLEAR gt_qc_scol[].
  CLEAR gt_qc_scol.
  LOOP AT excel_output INTO excel_output_wa.
    READ TABLE lt_dd03l WITH KEY position = excel_output_wa-column.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT lt_dd03l-fieldname OF STRUCTURE gt_qc_scol TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = excel_output_wa-value.
      ENDIF.
    ENDIF.
    AT END OF row.
      APPEND gt_qc_scol.
      CLEAR gt_qc_scol.
    ENDAT.
  ENDLOOP.

  CLEAR gt_item_i[].
  LOOP AT gt_qc_scol.
    IF gt_qc_scol-col01 IS NOT INITIAL.
      gt_item_i-qcno = <gs_item>-qcno.
      gt_item_i-qcnr = <gs_item>-qcnr.

      gt_item_i-afonr   = gt_qc_scol-col01 .
      gt_item_i-qctxt   = gt_qc_scol-col02 .
      gt_item_i-menge   = gt_qc_scol-col03 .
      gt_item_i-percent = gt_qc_scol-col04 .
      gt_item_i-remark  = gt_qc_scol-col05 .

      APPEND gt_item_i.
      CLEAR gt_item_i.
    ENDIF.

    IF gt_qc_scol-col06 IS NOT INITIAL.
      gt_item_i-qcno = <gs_item>-qcno.
      gt_item_i-qcnr = <gs_item>-qcnr.
      gt_item_i-afonr   = gt_qc_scol-col06 .
      gt_item_i-qctxt   = gt_qc_scol-col07 .
      gt_item_i-menge   = gt_qc_scol-col08 .
      gt_item_i-percent = gt_qc_scol-col09 .
      gt_item_i-remark  = gt_qc_scol-col10 .

      APPEND gt_item_i.
      CLEAR gt_item_i.
    ENDIF.

  ENDLOOP.


ENDFORM.

FORM set_qc_state USING i_qc_state.

  CASE i_qc_state.
    WHEN 'B'.
      <gs_item>-qc_status = 'B'.
    WHEN 'E'.
      <gs_item>-qc_status = 'E'.
    WHEN 'C'.
      <gs_item>-qc_status = 'C'.
    WHEN OTHERS.
      MESSAGE e000(zmm01) WITH '不能修改为其它状态'.
  ENDCASE.

ENDFORM.



FORM save_as_data.
  DATA:
    default_filename TYPE string,
    l_filename       TYPE string,
    i_filename(100)  TYPE c.


  default_filename =  <gs_item>-qcno && '_' && <gs_item>-qcnr && '.XLSX'.
*
*  PERFORM save_dialog USING default_filename CHANGING l_filename.

  i_filename = default_filename.

  CALL METHOD document->save_as
    EXPORTING
      file_name   = i_filename
*     no_flush    = 'X'
      prompt_user = 'X'
    IMPORTING
      error       = error
      retcode     = retcode.
  PERFORM handle_error USING ''.
ENDFORM.


FORM save_dialog USING    default_filename TYPE string
                 CHANGING filename         TYPE string.

  DATA:
    l_path     TYPE string,
    l_fullpath TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
*      WINDOW_TITLE
*      DEFAULT_EXTENSION
      default_file_name = default_filename
*      WITH_ENCODING
*      FILE_FILTER
*      INITIAL_DIRECTORY
*      PROMPT_ON_OVERWRITE
    CHANGING
      path              = l_path
      filename          = filename
      fullpath          = l_fullpath ).
*      USER_ACTION
*      FILE_ENCODING).
  filename = l_path && filename.

ENDFORM.                    " save_dialog

FORM print_data.
  CALL METHOD document->print_document
    EXPORTING
*     no_flush    = ' '
      prompt_user = 'X'
    IMPORTING
      error       = error
      retcode     = retcode.
  PERFORM handle_error USING ''.
ENDFORM.

FORM initial_excel.

  SKIP 1.

  PERFORM get_model_url.

  PERFORM create_excel_objects.

  PERFORM initial_data.

ENDFORM.

FORM mass_save_excel USING i_sitem STRUCTURE zafo_qc_sitem
                      lv_complete_path.
  CHECK document IS  INITIAL.
  PERFORM initial_excel.

  CALL METHOD document->save_as
    EXPORTING
      file_name = lv_complete_path
*     no_flush  = 'X'
*     prompt_user = sy-uname
    IMPORTING
      error     = error
      retcode   = retcode.


  PERFORM handle_error USING ''.

  PERFORM release_objects.

ENDFORM.

FORM mass_print_excel USING i_sitem STRUCTURE zafo_qc_sitem.
  CHECK document IS  INITIAL.
  PERFORM initial_excel.
  CALL METHOD document->print_document
    EXPORTING
*     no_flush    = ' '
      prompt_user = sy-uname
    IMPORTING
      error       = error
      retcode     = retcode.

  PERFORM handle_error USING ''.

  PERFORM release_objects.

ENDFORM.
