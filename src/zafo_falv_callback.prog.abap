*&---------------------------------------------------------------------*
*& 包含               ZAFO_FALV_CALLBACK
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 包含               myfalv
*&---------------------------------------------------------------------*



"内部调用
FORM frm_user_command USING c_alv TYPE REF TO zwft_falv
      e_ucomm TYPE sy-ucomm.
  CASE c_alv .
    WHEN <io_class>->falv_item.
      <io_class>->user_command_item( EXPORTING i_ucomm = e_ucomm ).
  ENDCASE.
ENDFORM.

"内部调用
FORM frm_toolbar USING c_falv TYPE REF TO zwft_falv
      e_object TYPE REF TO cl_alv_event_toolbar_set
      e_interactive TYPE char01.
  <io_class>->toolbar( EXPORTING c_falv = c_falv i_object = e_object ).
ENDFORM.


FORM frm_data_changed_finished USING c_falv TYPE REF TO zwft_falv
      e_modified TYPE char01
      et_good_cells TYPE lvc_t_modi.

  <io_class>->data_changed_finished( EXPORTING c_falv = c_falv it_good_cells = et_good_cells ).
ENDFORM.

FORM frm_data_changed USING c_falv TYPE REF TO zwft_falv
      er_data_changed TYPE REF TO cl_alv_changed_data_protocol
      e_onf4 TYPE char01
      e_onf4_before TYPE char01
      e_onf4_after TYPE char01
      e_ucomm TYPE sy-ucomm.
  <io_class>->data_changed( EXPORTING c_falv = c_falv
   cl_data_changed = er_data_changed ) .
ENDFORM.



FORM frm_double_click  USING c_falv TYPE REF TO zwft_falv
      e_row  TYPE lvc_s_row
      e_column TYPE lvc_s_col
      es_row_no TYPE lvc_s_roid.

  <io_class>->double_click( EXPORTING c_falv = c_falv i_row = e_row i_column = e_column ).
ENDFORM.

FORM frm_hotspot_click USING c_falv TYPE REF TO zwft_falv
      e_row TYPE lvc_s_row
      e_column TYPE lvc_s_col
      es_row_no TYPE lvc_s_roid .
  <io_class>->hotspot_click( EXPORTING c_falv = c_falv i_row = e_row i_column = e_column ).
ENDFORM.

FORM frm_onf4  USING c_falv TYPE REF TO zwft_falv
      e_fieldname TYPE lvc_fname
      e_fieldvalue TYPE lvc_value
      es_row_no TYPE lvc_s_roid
      er_event_data TYPE REF TO cl_alv_event_data
      et_bad_cells TYPE lvc_t_modi
      e_display TYPE char01.
  <io_class>->onf4( EXPORTING c_falv = c_falv
                                                    c_event_data = er_event_data
                                                    i_fieldname = e_fieldname
                                                    i_index = es_row_no-row_id
                                                    i_display = e_display ).

ENDFORM.
