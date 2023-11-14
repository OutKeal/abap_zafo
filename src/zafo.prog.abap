*&---------------------------------------------------------------------*
*& Report ZAFO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo MESSAGE-ID zafo.

INCLUDE zafo_top.

INCLUDE zafo_sel.

INCLUDE zafo_f01.

INCLUDE zafo_f02.

INITIALIZATION.
  "页面初始化
  PERFORM frm_init.

AT SELECTION-SCREEN ON s_lifnr.

  PERFORM frm_vendor_search.

AT SELECTION-SCREEN ON s_kunnr.

  PERFORM frm_customer_search.

  AT SELECTION-SCREEN ON s_matnr.

  PERFORM frm_material_search.

AT SELECTION-SCREEN.

  PERFORM frm_at_screen CHANGING sscrfields-ucomm.

AT SELECTION-SCREEN OUTPUT.

  PERFORM frm_set_sel_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM frm_get_excel_f4 CHANGING p_file.

START-OF-SELECTION.

  PERFORM frm_run.

END-OF-SELECTION.
