*&---------------------------------------------------------------------*
*& 包含               ZAFS_ALV_0000
*&---------------------------------------------------------------------*



DATA:index TYPE sy-tabix.


FORM free_object_alv USING u_object TYPE  REF TO cl_gui_alv_grid.
  IF u_object IS BOUND.
    CALL METHOD u_object->free.
    FREE u_object.
  ENDIF.
ENDFORM.


FORM f_refresh_grid_alv USING u_grid TYPE REF TO cl_gui_alv_grid.
  DATA: ls_scroll TYPE lvc_s_stbl.

  CLEAR: ls_scroll.
  ls_scroll-row = 'X'.
  ls_scroll-col = 'X'.

  CALL METHOD u_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_scroll
      i_soft_refresh = 'X'.

ENDFORM.


FORM f_refresh_hold_grid_alv USING u_grid TYPE REF TO cl_gui_alv_grid.
  DATA: ls_scroll TYPE lvc_s_stbl.
  DATA:ls_disvariant TYPE disvariant.

  CALL METHOD u_grid->get_variant
    IMPORTING
      es_variant = ls_disvariant.

  CLEAR: ls_scroll.
  ls_scroll-row = 'X'.
  ls_scroll-col = 'X'.
  CALL METHOD u_grid->refresh_table_display
    EXPORTING
      is_stable = ls_scroll.
  " i_soft_refresh = 'X'.

  CALL METHOD u_grid->set_variant
    EXPORTING
      is_variant = ls_disvariant
*     i_save     =
    .
ENDFORM.


FORM f_transfer_slis_to_lvc CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv
                                     ct_fcat     TYPE lvc_t_fcat.
  DATA:lt_dd36q TYPE TABLE OF dd36q WITH HEADER LINE.
  DATA:lt_dd03l TYPE TABLE OF dd03l WITH HEADER LINE.
  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
    EXPORTING
      it_fieldcat = ct_fieldcat
    IMPORTING
      et_fieldcat = lt_fieldcat.

  CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
    EXPORTING
      it_fieldcat_kkblo = lt_fieldcat
    IMPORTING
      et_fieldcat_lvc   = ct_fcat.

  IF lt_dd36q[] IS INITIAL.
    SELECT * INTO TABLE lt_dd36q
      FROM dd36q
      WHERE tabname = 'ZAFO_SITEM'.
  ENDIF.

  IF lt_dd03l[] IS INITIAL.
    SELECT * INTO TABLE lt_dd03l
      FROM dd03l
      WHERE tabname = 'ZAFO_SITEM'.
  ENDIF.

  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<cs_fcat>).
    IF <cs_fcat>-fieldname+0(5) = 'MENGE'.
      <cs_fcat>-no_zero = 'X'.
    ENDIF.

    READ TABLE lt_dd36q WITH KEY fieldname = <cs_fcat>-fieldname.
    IF sy-subrc EQ 0.
*      IF lt_dd36q-checktable IS NOT INITIAL .
      <cs_fcat>-f4availabl = 'X'.
      CONTINUE.
*      ENDIF.
    ENDIF.

    READ TABLE lt_dd03l WITH KEY fieldname = <cs_fcat>-fieldname.
    IF sy-subrc EQ 0.
      IF lt_dd03l-checktable IS NOT INITIAL .
        <cs_fcat>-f4availabl = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM f_set_drop_down_table USING u_grid TYPE REF TO cl_gui_alv_grid
                            CHANGING ct_fcat TYPE lvc_t_fcat.
  DATA: lt_drdn TYPE lvc_t_drop,                            "#EC NEEDED
        ls_drdn TYPE lvc_s_drop,                            "#EC NEEDED
        lt_dral TYPE lvc_t_dral,                            "#EC NEEDED
        ls_dral TYPE lvc_s_dral.                            "#EC NEEDED

  DATA: l_field  TYPE lvc_fname,
        l_handle TYPE i VALUE 0.

  DEFINE mac_get_drdn.
    ls_drdn-handle  = &1.
    ls_drdn-value   = &2.

      APPEND ls_drdn TO lt_drdn.

      ls_dral-handle    = &1.
      ls_dral-value     = &2.
      ADD 1 TO ls_dral-int_value.
      APPEND ls_dral TO lt_dral.

  end-of-definition.


  CLEAR lt_dral[].
  SORT gt_reason BY fieldname.
*  IF gt_reason[] IS INITIAL.
  LOOP AT gt_reason WHERE fieldname <> ''.
    AT NEW  fieldname.
      ADD 1 TO l_handle.
      READ TABLE ct_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>) WITH KEY fieldname = gt_reason-fieldname.
      IF sy-subrc EQ 0.
        <fs_fcat>-drdn_hndl = l_handle.
      ENDIF.
    ENDAT.
    mac_get_drdn l_handle gt_reason-reason.

  ENDLOOP.


  CHECK lt_dral[] IS NOT INITIAL.
  IF u_grid IS BOUND.
    CALL METHOD u_grid->set_drop_down_table
      EXPORTING
        it_drop_down_alias = lt_dral.
  ENDIF.

ENDFORM.
