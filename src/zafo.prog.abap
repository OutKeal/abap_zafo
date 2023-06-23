*&---------------------------------------------------------------------*
*& Report ZAFO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo MESSAGE-ID zafo.

INCLUDE zafo_i01_top.

INCLUDE zafo_sel.

INCLUDE zafo_f01.

INCLUDE zafo_f02.

INCLUDE zafo_f03.


INITIALIZATION.

  PERFORM frm_init.

  PERFORM frm_set_list.

  PERFORM frm_set_default_value.

AT SELECTION-SCREEN ON s_lifnr.
  PERFORM frm_vendor_search_s.

AT SELECTION-SCREEN ON s_zname1.
  PERFORM frm_vendor_search_sname.

AT SELECTION-SCREEN.

  PERFORM frm_download_temp CHANGING sscrfields-ucomm.  "下载模板

  IF sscrfields-ucomm = 'CON'.
    PERFORM frm_set_default_value .
  ELSEIF sscrfields-ucomm = 'TYP'.
    IF p_dis EQ 'X' .
      CLEAR p_dis.
      p_cre = 'X'.
    ENDIF.
    PERFORM frm_set_default_value .
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  PERFORM frm_set_sel_screen.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM frm_get_excel_f4 CHANGING p_file.

START-OF-SELECTION.

  PERFORM frm_run.
