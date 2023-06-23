FUNCTION zafo_mail.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  TABLES
*"      IT_RECEIVERS STRUCTURE  SOOS1
*"----------------------------------------------------------------------

* Datendeklaration
  DATA: message TYPE swc_object.
  DATA: recipient TYPE swc_object.
  DATA: content LIKE soli-line OCCURS 0 WITH HEADER LINE.
  DATA: content_header(50)  TYPE c.
  DATA receiver LIKE soli OCCURS 1 WITH HEADER LINE.
  DATA rec_type LIKE soli OCCURS 1 WITH HEADER LINE.
  DATA: matnr     LIKE rm61r-matnr,
        l_matnr   LIKE rm61r-matnr,
        ext_matnr LIKE eina-idnlf,
        werks     LIKE rm61r-werks,
        lief      LIKE ekko-lifnr,
        delkz     LIKE mdps-delkz,
        delnr     LIKE mdps-delnr.
  DATA: string1(35),
        string2(10) TYPE c.
  DATA text(80) TYPE c.
  DATA:files(128) TYPE c.

  DATA: object_header TYPE TABLE OF solisti1 WITH HEADER LINE.
  DATA: object_content TYPE TABLE OF solisti1 WITH HEADER LINE.

  DATA:  attachments    LIKE swotobjid OCCURS 0 WITH HEADER LINE.


  text = '标题456'.

  content = '<head>亲爱的余小三<\head>'.
  APPEND content.

  content = '    测试123456'.
  APPEND content.


* Deklaration eines Containers
  swc_container container.

*--> Objektreferenz auf ein MESSAGE-Objekt erzeugen
  swc_create_object message 'MESSAGE' space.
* Importparameter für MESSAGE.Compose in Container stellen
* Titel des Dokuments
  swc_set_element container 'DOCUMENTTITLE' text.
* Name des Dokuments
  swc_set_element container 'DOCUMENTNAME' 'Nachricht'.
* Dokumenttyp
  swc_set_element container 'DOCUMENTTYPE' 'RAW'.
* Kennzeichen: kein Dialog
  swc_set_element container 'NO_DIALOG' 'X'.
* Dokumentinhalt

  swc_set_table container 'DocumentContent' content.

* Importparameter: Adresse für Empfänger

  it_receivers-recextnam = '83396131@QQ.COM'.
  APPEND it_receivers.
  it_receivers-recextnam = 'FOXJDYWJ@QQ.COM'.
  APPEND it_receivers.
  LOOP AT it_receivers.
    receiver-line = it_receivers-recextnam.
    APPEND receiver .
    rec_type = 'U'.
    APPEND rec_type.

  ENDLOOP.
  swc_set_table container 'ReceiverType' rec_type.
  swc_set_table container 'Receiver' receiver.
  swc_set_element container 'ReplyReceiver' '4123@qq.com'.
  swc_set_element container 'ReplyReceiverType' 'U'.



  swc_set_table container 'Receiver' receiver.

*--> Aufruf der Methode Compose
*  files = 'c:\附件文件.xlsx'.
*  swc_set_element container 'FILES' files.
*  swc_call_method message 'ImportFile' container.

*  DATA result_object TYPE swc_object.
*  swc_get_element container '_RESULT' result_object.

*  swc_call_method message 'Create' container.


  PERFORM att TABLES object_content.

  swc_get_table container 'DocumentContent' object_content.

  swc_call_method message 'Compose' container.

*
*  PERFORM so_user_read.
*  PERFORM document_repository.
*  PERFORM creat_attachments.
*
*  attachments-objtype    = 'SOFM'.
*  attachments-objkey     = gs_document-foltp && gs_document-folyr && gs_document-folno
*                        && gs_document-objtp && gs_document-objyr && gs_document-objno
*                        && gs_document-fornam.
*  APPEND attachments.
*  swc_set_table container 'Attachments' attachments.


*  swc_call_method message 'compose' container.


  IF sy-subrc NE 0.
* Ausgabe der zur Ausnahme gehörigen Fehlermeldung
    MESSAGE ID sy-msgid TYPE 'S'  NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


*--> Freigeben der Referenzen
* MESSAGE-Objekt
  swc_free_object message.
* RECIPIENT-Objekt
  swc_free_object recipient.



ENDFUNCTION.

FORM att TABLES gt_binary STRUCTURE solisti1.
  DATA: l_attlin TYPE i,
        l_mailin TYPE i,
        l_binlen TYPE i.

  DATA: BEGIN OF gt_attachment OCCURS 0,
          id       TYPE c,
          name(10),
          price    TYPE p,
        END OF gt_attachment.

  DATA:l_attachment TYPE string.
  gt_attachment-id = 1.
  gt_attachment-name = 'NAME1'.
  gt_attachment-price = 11.
  APPEND gt_attachment.
  CLEAR gt_attachment.
  gt_attachment-id = 2.
  gt_attachment-name = 'NAME2'.
  gt_attachment-price = 12.
  APPEND gt_attachment.
  DESCRIBE TABLE gt_attachment LINES l_attlin.


  PERFORM itabtostr   TABLES gt_attachment USING l_attachment.

  "将储存附件内容的STRING转换为BIN
  PERFORM strtobin TABLES gt_binary USING l_attachment l_binlen.




ENDFORM.

FORM so_user_read .

  gs_user-sapname = sy-uname.
  CALL FUNCTION 'SO_USER_READ_API1'
    EXPORTING
      user            = gs_user
    IMPORTING
      user_data       = gs_user_data
    EXCEPTIONS
      user_not_exist  = 1
      parameter_error = 2
      x_error         = 3
      OTHERS          = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  gv_fold_type   = gs_user_data-outboxfol+0(3).
  gv_fold_yr     = gs_user_data-outboxfol+3(2).
  gv_fold_number = gs_user_data-outboxfol+5(12).


  CLEAR: gt_files[], gt_files.
  REFRESH : gt_objcnt,
  gt_objhead,
  gt_objpara,
  gt_objparb,
  gt_receipients,
  gt_attachments,
  gt_references,
  gt_files.
  CLEAR  :    gs_document,
  gs_header2,
  gt_objcnt,
  gt_objhead,
  gt_objpara,
  gt_objparb,
  gt_receipients,
  gt_attachments,
  gt_references,
  gt_files.


ENDFORM.                    " SO_USER_READ


FORM document_repository .
  gv_method1 = 'SAVE'.
  gs_document-foltp   = gv_fold_type.
*  gs_document-foltp   = 'RAW'.
  gs_document-folyr   = gv_fold_yr.
  gs_document-folno   = gv_fold_number.
  gs_document-objtp   = gs_user_data-object_typ.
*  gs_document-objtp   = 'EXT'.
*g_document-OBJYR   = '27'.
*g_document-OBJNO   = '000000002365'.
*g_document-OBJNAM = 'MESSAGE'.
  gs_document-objdes   = 'NAME123'.
*  gs_document-objtype   = 'PDF'.
*  gs_document-folrg   = 'O'.
*g_document-okcode   = 'CHNG'.
*  gs_document-objlen = '11K'.
  gs_document-file_ext = 'PDF'.
*  gs_document-EXTCT = 'K'.
  gs_header2-objdes = gv_mail_title.

  CALL FUNCTION 'SO_DOCUMENT_REPOSITORY_MANAGER'
    EXPORTING
      method       = gv_method1
      office_user  = sy-uname
      ref_document = gs_ref_document
      new_parent   = gs_new_parent
    IMPORTING
      authority    = gv_authority
    TABLES
      objcont      = gt_objcnt
      objhead      = gt_objhead
      objpara      = gt_objpara
      objparb      = gt_objparb
      recipients   = gt_receipients
      attachments  = gt_attachments
      references   = gt_references
      files        = gt_files
    CHANGING
      document     = gs_document
      header_data  = gs_header2.

ENDFORM.                    " DOCUMENT_REPOSITORY

FORM creat_attachments .

  DATA: gr_bds_documents TYPE REF TO cl_bds_document_set,
        g_classname      TYPE sbdst_classname,
        g_classtype      TYPE sbdst_classtype,
        g_objectkey      TYPE sbdst_object_key,
        g_doc_components TYPE sbdst_components,
        g_doc_signature  TYPE sbdst_signature.
  DATA: gt_bds_uris TYPE sbdst_uri,
        gs_bds_url  LIKE LINE OF gt_bds_uris.
  DATA:
  g_template_url(256) TYPE c.

  gv_method1 = 'ATTCREATEFROMPC'.
*  gv_method1 = 'IMPORTFROMPC'.

*  GV_SAVE_PATHT = P_FILE.
  gt_files-text = 'c:\HTML000001.pdf'.

  APPEND gt_files.

  CALL FUNCTION 'SO_DOCUMENT_REPOSITORY_MANAGER'
    EXPORTING
      method       = gv_method1
      office_user  = gv_owner
      ref_document = gs_ref_document
      new_parent   = gs_new_parent
    IMPORTING
      authority    = gv_authority
    TABLES
      objcont      = gt_objcnt
      objhead      = gt_objhead
      objpara      = gt_objpara
      objparb      = gt_objparb
      recipients   = gt_receipients
      attachments  = gt_attachments
      references   = gt_references
      files        = gt_files
    CHANGING
      document     = gs_document
      header_data  = gs_header2.
  COMMIT WORK AND WAIT.

ENDFORM.                    " CREAT_ATTACHMENTS



FORM itabtostr TABLES intab
               USING  outstr TYPE string.
  DATA: tab      TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
        enter(2) TYPE c VALUE cl_abap_char_utilities=>cr_lf,
        n        TYPE i.
  DATA: BEGIN OF headtab OCCURS 0 ,
          length    TYPE i,
          decimals  TYPE i,
          type_kind TYPE c,
          name(30)  TYPE c,
        END OF headtab.
  DATA descr_ref TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS: <comp_wa> TYPE abap_compdescr,
                 <f_field> ,
                 <f_intab> TYPE any.
  DATA:str   TYPE string,
       str2  TYPE string,
       text1 TYPE c.
  descr_ref ?= cl_abap_typedescr=>describe_by_data( intab ).
  LOOP AT descr_ref->components ASSIGNING <comp_wa>.
    MOVE-CORRESPONDING <comp_wa> TO headtab.
    APPEND headtab.
  ENDLOOP.
  DESCRIBE TABLE headtab LINES n.
  LOOP AT intab ASSIGNING <f_intab>.
    DO n TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE <f_intab> TO <f_field>.
      str = <f_field>.
      READ TABLE headtab INDEX sy-index.
      IF headtab-type_kind = 'I' OR headtab-type_kind = 'P'
                                 OR headtab-type_kind = 'F'.
        SEARCH str FOR '-'.
        IF sy-subrc = 0 AND sy-fdpos <> 0.
          SPLIT str AT '-' INTO str text1.
          CONDENSE str.
          CONCATENATE '-' str INTO str.
        ELSE.
          CONDENSE str.
        ENDIF.
      ELSE.
*        SHIFT str LEFT DELETING LEADING '0' .
      ENDIF.
      CONCATENATE str2 tab str INTO str2.
    ENDDO.
    SHIFT str2.
    CONCATENATE outstr str2 enter INTO outstr.
    CLEAR str2.
  ENDLOOP.
ENDFORM.

FORM strtobin TABLES record
              USING  str
                     len.
  DATA:tmpbuffer TYPE xstring.
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text     = str
      mimetype = '"text/html; charset=Unicode; charset=big5"'
*     encoding = '8400'
    IMPORTING
      buffer   = tmpbuffer
    EXCEPTIONS
      failed   = 1
      OTHERS   = 2.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer          = tmpbuffer
      append_to_table = ''
    IMPORTING
      output_length   = len
    TABLES
      binary_tab      = record.
ENDFORM.
