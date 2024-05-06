FUNCTION zafo_idoc_input.
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     VALUE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD
*"     VALUE(MASS_PROCESSING) LIKE  BDWFAP_PAR-MASS_PROC
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) LIKE  BDWF_PARAM-RESULT
*"     VALUE(APPLICATION_VARIABLE) LIKE  BDWF_PARAM-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) LIKE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) LIKE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"--------------------------------------------------------------------
*----------------------------------------------------------------------*
*  this function module is generated                                   *
*          never change it manually, please!        2024.03.25         *
*----------------------------------------------------------------------*

  DATA:
      z1zafo_create LIKE z1zafo_create,
      z1zafo_bapi_head LIKE z1zafo_bapi_head,
      z1zafo_bapi_item LIKE z1zafo_bapi_item,

      e_afono LIKE
        zafo_shead-afono,
      es_head LIKE
        zafo_shead,
      et_item TYPE
        zafo_tt_sitem,
      is_head LIKE
        zafo_bapi_head,
      i_post LIKE
        bapiflag-bapiflag,
      no_commit LIKE
        bapiflag-bapiflag,

      it_item LIKE zafo_bapi_item
                  OCCURS 0 WITH HEADER LINE,
      et_return LIKE bapiret2
                  OCCURS 0 WITH HEADER LINE,

      t_edidd  LIKE edidd OCCURS 0 WITH HEADER LINE,
      bapi_retn_info  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: error_flag,
        bapi_idoc_status LIKE bdidocstat-status.

  in_update_task = 'X'.
  CLEAR call_transaction_done.
* check if the function is called correctly                            *
  READ TABLE idoc_contrl INDEX 1.
  IF sy-subrc <> 0.
    EXIT.
  ELSEIF idoc_contrl-mestyp <> 'ZAFO_CREATE'.
    RAISE wrong_function_called.
  ENDIF.

* go through all IDocs                                                 *
  LOOP AT idoc_contrl.
*   select segments belonging to one IDoc                              *
    REFRESH t_edidd.
    LOOP AT idoc_data WHERE docnum = idoc_contrl-docnum.
      APPEND idoc_data TO t_edidd.
    ENDLOOP.

*   through all segments of this IDoc                                  *
    CLEAR error_flag.
    REFRESH bapi_retn_info.
    CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
      LOOP AT t_edidd INTO idoc_data.

        CASE idoc_data-segnam.

          WHEN 'Z1ZAFO_CREATE'.

            z1zafo_create = idoc_data-sdata.
            MOVE z1zafo_create-i_post
              TO i_post.
            MOVE z1zafo_create-no_commit
              TO no_commit.


          WHEN 'Z1ZAFO_BAPI_HEAD'.

            z1zafo_bapi_head = idoc_data-sdata.
            MOVE-CORRESPONDING z1zafo_bapi_head
               TO is_head.                                  "#EC ENHOK

            IF z1zafo_bapi_head-bldat
               IS INITIAL.
              CLEAR is_head-bldat.
            ENDIF.
            IF z1zafo_bapi_head-budat
               IS INITIAL.
              CLEAR is_head-budat.
            ENDIF.
            IF z1zafo_bapi_head-eeind
               IS INITIAL.
              CLEAR is_head-eeind.
            ENDIF.

          WHEN 'Z1ZAFO_BAPI_ITEM'.

            z1zafo_bapi_item = idoc_data-sdata.
            MOVE-CORRESPONDING z1zafo_bapi_item
               TO it_item.                                  "#EC ENHOK

            IF z1zafo_bapi_item-eeind
               IS INITIAL.
              CLEAR it_item-eeind.
            ENDIF.
            APPEND it_item.

        ENDCASE.

      ENDLOOP.
    ENDCATCH.
    IF sy-subrc = 1.
*     write IDoc status-record as error and continue                   *
      CLEAR bapi_retn_info.
      bapi_retn_info-type   = 'E'.
      bapi_retn_info-id     = 'B1'.
      bapi_retn_info-number = '527'.
      bapi_retn_info-message_v1 = idoc_data-segnam.
      bapi_idoc_status      = '51'.
      PERFORM zafo_idoc_status
              TABLES t_edidd
                     idoc_status
                     return_variables
               USING idoc_contrl
                     bapi_retn_info
                     bapi_idoc_status
                     workflow_result.
      CONTINUE.
    ENDIF.
*   call BAPI-function in this system                                  *
    CALL FUNCTION 'ZAFO_CREATE'
       EXPORTING
        is_head = is_head
        i_post = i_post
        no_commit = no_commit
       IMPORTING
        e_afono = e_afono
        es_head = es_head
        et_item = et_item
       TABLES
        it_item = it_item
        et_return = et_return
       EXCEPTIONS
        OTHERS =  1
         .
    IF sy-subrc <> 0.
*     write IDoc status-record as error                                *
      CLEAR bapi_retn_info.
      bapi_retn_info-type       = 'E'.
      bapi_retn_info-id         = sy-msgid.
      bapi_retn_info-number     = sy-msgno.
      bapi_retn_info-message_v1 = sy-msgv1.
      bapi_retn_info-message_v2 = sy-msgv2.
      bapi_retn_info-message_v3 = sy-msgv3.
      bapi_retn_info-message_v4 = sy-msgv4.
      bapi_idoc_status          = '51'.
      PERFORM zafo_idoc_status
              TABLES t_edidd
                     idoc_status
                     return_variables
               USING idoc_contrl
                     bapi_retn_info
                     bapi_idoc_status
                     workflow_result.
    ELSE.
      LOOP AT et_return.
        IF NOT et_return IS INITIAL.
          CLEAR bapi_retn_info.
          MOVE-CORRESPONDING et_return
               TO bapi_retn_info.                           "#EC ENHOK
          IF et_return-type = 'A' OR
             et_return-type = 'E'.
            error_flag = 'X'.
          ENDIF.
          APPEND bapi_retn_info.
        ENDIF.
      ENDLOOP.
      LOOP AT bapi_retn_info.
*       write IDoc status-record                                       *
        IF error_flag IS INITIAL.
          bapi_idoc_status = '53'.
        ELSE.
          bapi_idoc_status = '51'.
          IF bapi_retn_info-type = 'S'.
            CONTINUE.
          ENDIF.
        ENDIF.
        PERFORM zafo_idoc_status
                TABLES t_edidd
                       idoc_status
                       return_variables
                 USING idoc_contrl
                       bapi_retn_info
                       bapi_idoc_status
                       workflow_result.
      ENDLOOP.
      IF sy-subrc <> 0.
*      'ET_RETURN'                                                     *
*       is empty write idoc status-record as successful                *
        CLEAR bapi_retn_info.
        bapi_retn_info-type       = 'S'.
        bapi_retn_info-id         = 'B1'.
        bapi_retn_info-number     = '501'.
        bapi_retn_info-message_v1 = 'CREATE'.
        bapi_idoc_status          = '53'.
        PERFORM zafo_idoc_status
                TABLES t_edidd
                       idoc_status
                       return_variables
                 USING idoc_contrl
                       bapi_retn_info
                       bapi_idoc_status
                       workflow_result.
      ENDIF.
      IF error_flag IS INITIAL.
*       write linked object keys                                       *
        CLEAR return_variables.
        return_variables-wf_param = 'Appl_Objects'.
      ENDIF.
    ENDIF.

  ENDLOOP.                             " idoc_contrl






ENDFUNCTION.


* subroutine writing IDoc status-record                                *
FORM zafo_idoc_status
     TABLES idoc_data    STRUCTURE  edidd
            idoc_status  STRUCTURE  bdidocstat
            r_variables  STRUCTURE  bdwfretvar
      USING idoc_contrl  LIKE  edidc
            VALUE(retn_info) LIKE   bapiret2
            status       LIKE  bdidocstat-status
            wf_result    LIKE  bdwf_param-result.

  CLEAR idoc_status.
  idoc_status-docnum   = idoc_contrl-docnum.
  idoc_status-msgty    = retn_info-type.
  idoc_status-msgid    = retn_info-id.
  idoc_status-msgno    = retn_info-number.
  idoc_status-appl_log = retn_info-log_no.
  idoc_status-msgv1    = retn_info-message_v1.
  idoc_status-msgv2    = retn_info-message_v2.
  idoc_status-msgv3    = retn_info-message_v3.
  idoc_status-msgv4    = retn_info-message_v4.
  idoc_status-repid    = sy-repid.
  idoc_status-status   = status.

  CASE retn_info-parameter.
    WHEN 'ISHEAD'
      OR 'IS_HEAD'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1ZAFO_BAPI_HEAD'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'ITITEM'
      OR 'IT_ITEM'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1ZAFO_BAPI_ITEM'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'IPOST'
      OR 'I_POST'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1ZAFO_CREATE'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'NOCOMMIT'
      OR 'NO_COMMIT'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1ZAFO_CREATE'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.

  ENDCASE.

  INSERT idoc_status INDEX 1.

  IF idoc_status-status = '51'.
    wf_result = '99999'.
    r_variables-wf_param   = 'Error_IDOCs'.
    r_variables-doc_number = idoc_contrl-docnum.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
  ELSEIF idoc_status-status = '53'.
    CLEAR wf_result.
    r_variables-wf_param = 'Processed_IDOCs'.
    r_variables-doc_number = idoc_contrl-docnum.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
    r_variables-wf_param = 'Appl_Object_Type'.
    r_variables-doc_number = 'ZAFO'.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
  ENDIF.

ENDFORM.                               " ZAFO_IDOC_STATUS
