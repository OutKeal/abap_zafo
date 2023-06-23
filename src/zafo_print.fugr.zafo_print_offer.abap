FUNCTION zafo_print_offer.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      IT_HEAD STRUCTURE  ZMMT0050 OPTIONAL
*"      IT_ITEM STRUCTURE  ZMMT0051 OPTIONAL
*"----------------------------------------------------------------------

  DATA: l_ssfcrespd TYPE ssfcrespd.

  DATA:ls_phead TYPE zmmp0050.
  DATA:ls_pitem TYPE zmmp0051.



  PERFORM frm_initial_smartforms USING 'ZAFO_PRINT_OF01'.


  LOOP AT it_item .

    READ TABLE it_head WITH KEY zofno = it_item-zofno.

    MOVE-CORRESPONDING it_head TO ls_phead.
    MOVE-CORRESPONDING it_item TO ls_pitem.

    PERFORM frm_set_of_print_data CHANGING ls_phead ls_pitem.

    AT FIRST.

      control_parameters-no_close = 'X'.

    ENDAT.
    AT LAST.

      control_parameters-no_close = space.

    ENDAT.


    CALL FUNCTION func_module_name
      EXPORTING
        control_parameters   = control_parameters
        gs_t0050             = ls_phead
        gs_t0051             = ls_pitem
      IMPORTING
        document_output_info = l_ssfcrespd
*      TABLES
*       gt_print_i           = lt_print_i
      EXCEPTIONS
        user_cancled         = 4.

    IF sy-subrc = '4'.
    ELSEIF sy-subrc <> 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    control_parameters-no_open = 'X'.

  ENDLOOP.



ENDFUNCTION.


FORM frm_set_of_print_data CHANGING cs_head TYPE zmmp0050
                                     cs_item TYPE zmmp0051.

  PERFORM frm_get_menge_name USING  cs_item-zmm_mf1 CHANGING cs_item-zmm_mf1.

  PERFORM frm_get_menge_name USING  cs_item-zmm_qdl CHANGING cs_item-zmm_qdl.

  PERFORM frm_get_menge_name USING  cs_item-zmm_qrl CHANGING cs_item-zmm_qrl.

  CONCATENATE cs_item-zmm_qdl cs_item-bprme INTO cs_item-zmm_qdl SEPARATED BY space.
  CONCATENATE cs_item-zmm_qrl cs_item-bprme INTO cs_item-zmm_qrl SEPARATED BY space.


  PERFORM frm_get_menge_name USING cs_item-netpr CHANGING cs_item-netpr.

  PERFORM frm_get_menge_name USING cs_item-zmm_tran_rate CHANGING cs_item-zmm_tran_rate.

  PERFORM frm_get_date_name USING cs_head-zof_date CHANGING cs_head-zof_date.

  PERFORM frm_get_date_name USING cs_item-zend_date CHANGING cs_item-zend_date.

  PERFORM frm_get_x_name  CHANGING cs_item-zmm_yf.

  PERFORM frm_get_x_name  CHANGING cs_item-zmm_csf.

  PERFORM frm_get_x_name  CHANGING cs_item-zmm_oeko.

  PERFORM frm_get_x_name  CHANGING cs_item-zmm_oekox.






ENDFORM.
