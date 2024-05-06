*&---------------------------------------------------------------------*
*& 包含               ZAFO_START
*&---------------------------------------------------------------------*
  INCLUDE zafo_sel_screen.
  INCLUDE zafo_get_data.


  AT SELECTION-SCREEN OUTPUT.
    PERFORM frm_init.
    PERFORM frm_set_ref_in.
    PERFORM frm_set_sel_screen.

  AT SELECTION-SCREEN .
    PERFORM frm_at_screen CHANGING sscrfields-ucomm.

  FORM frm_run.
    IF p_werks IS INITIAL AND g_werks_flag = 'P'.
      MESSAGE s005 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    DATA(ls_bustyp) = zafo_basic=>get_bustyp( p_typ ).
    CASE 'X'.
      WHEN p_cre.
        IF g_werks_flag = 'P'.
          CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '01' bustyp = p_typ werks = p_werks )  .
        ELSEIF g_werks_flag = 'S'.
          CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '01' bustyp = p_typ CHANGING swerks =  s_werks[] ) .
        ENDIF.

        zafo_class=>create( EXPORTING werks = p_werks bustyp = p_typ item = gt_item[] ).

      WHEN p_ref.
        IF g_werks_flag = 'P'.
          CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '01' bustyp = p_typ werks = p_werks ) .
        ELSEIF g_werks_flag = 'S'.
          CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '01' bustyp = p_typ CHANGING swerks =  s_werks[] ) .
        ENDIF.

        PERFORM frm_get_ref USING ls_bustyp-busref CHANGING gt_item[] .
        IF gt_item[] IS INITIAL.
          MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E'."无参考数据
          RETURN.
        ENDIF.
        zafo_class=>create_by_ref( EXPORTING bustyp = p_typ item = gt_item[] ).

      WHEN p_mod.

        IF g_werks_flag = 'P'.
          CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '02' bustyp = p_typ werks = p_werks ) .
        ELSEIF g_werks_flag = 'S'.
          CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '02' bustyp = p_typ CHANGING swerks =  s_werks[] ) .
        ENDIF.

        zafo_class=>maintain( EXPORTING afono = p_afono   ).

      WHEN p_dis.

        IF g_werks_flag = 'P'.
          CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '03' bustyp = p_typ werks = p_werks ) .
        ELSEIF g_werks_flag = 'S'.
          CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '03' bustyp = p_typ CHANGING swerks =  s_werks[] ) .
        ENDIF.

        PERFORM frm_get_display TABLES gt_head CHANGING gt_item[].
        CHECK gt_head[] IS NOT INITIAL.
        zafo_class=>report( bustyp = p_typ head = gt_head[] item = gt_item[] ).

      WHEN p_load.
        IF g_werks_flag = 'P'.
          CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '01' bustyp = p_typ werks = p_werks ).
        ELSEIF g_werks_flag = 'S'.
          CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '01' bustyp = p_typ CHANGING swerks =  s_werks[] ).
        ENDIF.
*
*       CALL FUNCTION 'ZAFO_UPLOAD_CREATE'
*         EXPORTING
*           i_werks     = p_werks
*           i_bustyp    = p_typ
*           i_filename  = p_file
*           i_begin_row = p_row
**           TABLES
**          ET_RETURN   =
*         EXCEPTIONS
*           error       = 1
*           OTHERS      = 2.
*       IF sy-subrc <> 0.
** IMPLEMENT SUITABLE ERROR HANDLING HERE
*       ENDIF.
    ENDCASE.

  ENDFORM.



  FORM frm_get_display TABLES ct_head STRUCTURE zafo_shead
  CHANGING ct_item TYPE zafo_tt_sitem.
    SELECT h~* FROM zafo_head AS h
    LEFT JOIN zafo_item AS i
    ON h~afono = i~afono
    INTO CORRESPONDING FIELDS OF TABLE @ct_head
    WHERE bustyp = @p_typ
    AND h~werks IN @s_werks
    AND h~afono IN @s_afono
    AND h~ernam IN @s_ernam
    AND h~erdat IN @s_erdat
    AND h~budat IN @s_budat
    AND h~status IN @s_status
    AND h~lifnr IN @s_lifnr
    AND h~lgort IN @s_lgort
    AND h~kunnr IN @s_kunnr
    AND i~eeind IN @s_eeind
    AND i~lifnr IN @s_lifnr
    AND i~bsart IN @s_bsart
    AND i~ekorg IN @s_ekorg
    AND i~ekgrp IN @s_ekgrp
    AND i~kdauf IN @s_vbeln
    AND i~aufnr IN @s_aufnr
    AND i~maktx IN @s_maktx
    AND i~matkl IN @s_matkl
    AND i~mtart IN @s_mtart
    AND i~ebeln IN @s_ebeln
    AND i~matnr IN @s_matnr
    AND i~satnr IN @s_satnr
    AND i~charg IN @s_charg.
    IF sy-subrc NE 0.
      MESSAGE s024 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    SORT ct_head BY afono.
    DELETE ADJACENT DUPLICATES FROM ct_head COMPARING afono.

    SELECT * FROM zafo_item
    INTO CORRESPONDING FIELDS OF TABLE
    ct_item
    FOR ALL ENTRIES IN ct_head
    WHERE afono = ct_head-afono.

    SORT ct_head BY afono DESCENDING .
    SORT ct_item BY afono DESCENDING afonr .


    LOOP AT ct_head ASSIGNING  FIELD-SYMBOL(<fs_head>).
      zafo_basic=>set_icon( EXPORTING status = <fs_head>-status
      IMPORTING icon = <fs_head>-icon text = <fs_head>-text ).
    ENDLOOP.

    SORT ct_item BY afono   .

    LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<fs_item>).
      <fs_item>-history = icon_system_extended_help.
      zafo_basic=>set_icon( EXPORTING status = <fs_item>-item_status
      IMPORTING icon = <fs_item>-icon text = <fs_item>-text ).
    ENDLOOP.

  ENDFORM.
