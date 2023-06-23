FUNCTION zafo_bcs_mail_inter.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(UNAME) TYPE  UNAME
*"     VALUE(TITLE) TYPE  SO_OBJ_DES
*"  TABLES
*"      EMAIL_TEXT TYPE  BCSY_TEXT OPTIONAL
*"----------------------------------------------------------------------


  DATA: sendrequest TYPE REF TO cl_bcs.
  DATA: mailrecipient TYPE REF TO cl_sapuser_bcs.
  DATA: document TYPE REF TO cl_document_bcs.


  TRY.
      sendrequest = cl_bcs=>create_persistent( ).
      mailrecipient = cl_sapuser_bcs=>create( uname ).

      CALL METHOD sendrequest->add_recipient
        EXPORTING
          i_recipient = mailrecipient
          i_express   = 'X'.
      document = cl_document_bcs=>create_document(
               i_type = 'RAW'
               i_text = email_text[]
               i_subject = title ).



      CALL METHOD sendrequest->set_document( document ).
      sendrequest->set_send_immediately( 'X' ).
      sendrequest->send( ).
      COMMIT WORK.
    CATCH cx_bcs.
  ENDTRY.




ENDFUNCTION.
