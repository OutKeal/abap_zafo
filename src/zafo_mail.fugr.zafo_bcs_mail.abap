FUNCTION zafo_bcs_mail.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_SENDER) TYPE  SY-UNAME DEFAULT SY-UNAME
*"     VALUE(I_HEADER) TYPE  SO_OBJ_DES OPTIONAL
*"     VALUE(I_REPLYER) TYPE  ADR6-SMTP_ADDR OPTIONAL
*"  TABLES
*"      IT_RECIPIENT STRUCTURE  ZAFO_RECEIVERS OPTIONAL
*"      IT_TEXT STRUCTURE  LINE OPTIONAL
*"      IT_ATTCHMENT STRUCTURE  ZAFO_ATTCHMENT OPTIONAL
*"  EXCEPTIONS
*"      FILE_UPLOAD_ERROR
*"----------------------------------------------------------------------




*Prepare Mail Object
  DATA:  lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL.

  DATA: lo_document TYPE REF TO cl_document_bcs VALUE IS INITIAL. "document object

  CLASS cl_bcs DEFINITION LOAD.
  DATA:l_string TYPE string.
  DATA: l_email TYPE adr6-smtp_addr.
  DATA:lt_data TYPE  solix_tab .

  lo_send_request = cl_bcs=>create_persistent( ).


*Create Email document
  lo_document = cl_document_bcs=>create_document( "create document
                                  i_language  = sy-langu
                                  i_importance = '1'
                                  i_sensitivity = 'O'
                                  i_type = 'HTM' "Type of document HTM, TXT etc
                                  i_text =  it_text[] "email body internal table
                                  i_subject = i_header ). "email subject here p_sub input parameter




  LOOP AT it_attchment.

    l_string = it_attchment-fileaddr.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = l_string
        filetype                = 'BIN'
      TABLES
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      RAISE file_upload_error.
    ENDIF.

    DATA:ls_objty TYPE soodk-objtp.
    ls_objty = it_attchment-filetype.

    CALL METHOD lo_document->add_attachment
      EXPORTING
        i_attachment_type     = ls_objty
        i_attachment_subject  = it_attchment-filename
        i_attachment_language = space
        i_att_content_hex     = lt_data.
* Pass the document to send request

  ENDLOOP.


  lo_send_request->set_document( lo_document ).

*Set Sender
  DATA: lo_sender TYPE REF TO if_sender_bcs VALUE IS INITIAL.
  TRY.
      lo_sender = cl_sapuser_bcs=>create( i_sender ). "sender is the logged in user
* Set sender to send request
      lo_send_request->set_sender(
      EXPORTING
      i_sender = lo_sender ).
*    CATCH CX_ADDRESS_BCS.
****Catch exception here
  ENDTRY.



**Set recipient
  DATA: lo_recipient TYPE REF TO if_recipient_bcs VALUE IS INITIAL.

  LOOP AT it_recipient. "处理收件人
    l_email = it_recipient-mail_addr.
    CHECK l_email IS NOT INITIAL.

    lo_recipient = cl_cam_address_bcs=>create_internet_address( l_email ). "Here Recipient is email input p_email
    TRY.
        lo_send_request->add_recipient(
            EXPORTING
            i_recipient = lo_recipient
            i_copy       = it_recipient-mail_copy_to
            i_blind_copy = it_recipient-mail_copy_to2

            i_express = 'X' ).
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
    ENDTRY.

  ENDLOOP.


  IF i_replyer IS NOT INITIAL. "处理回复者
    l_email = i_replyer.
    lo_recipient = cl_cam_address_bcs=>create_internet_address( l_email ). "Here Recipient is email input p_email
*TRY.
    CALL METHOD lo_send_request->set_reply_to
      EXPORTING
        i_reply_to = lo_recipient.
*  CATCH cx_send_req_bcs.
*ENDTRY.
  ENDIF.
*TRY.

*Set immediate sending
  TRY.
      CALL METHOD lo_send_request->set_send_immediately
        EXPORTING
          i_send_immediately = 'X'.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
  ENDTRY.
*  CALL METHOD: lo_document->if_document_display_bcs~display_outplace( i_send_request = lo_send_request ).


  TRY.
** Send email
      lo_send_request->send(
      EXPORTING
      i_with_error_screen = 'X' ).
      COMMIT WORK.
      IF sy-subrc = 0.
        MESSAGE : '邮件发送成功' TYPE 'S'.
      ENDIF.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
*catch exception here
  ENDTRY.



*
*  CALL METHOD lo_send_request->edit
*    EXPORTING
**     i_starting_at_x   =
**     i_starting_at_y   =
**     i_ending_at_x  =
**     i_ending_at_y  =
*      i_hide_note    = 'X'
*      i_edit_subject = 'X'.
**     i_process_by_badi = lt_bcsy_fcode
**      i_badi_fltval  = 'RSRA'.
**  CATCH cx_send_req_bcs.
**  CATCH cx_bcs.
**ENDTRY.

ENDFUNCTION.
