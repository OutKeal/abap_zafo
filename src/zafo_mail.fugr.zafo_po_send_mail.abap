FUNCTION zafo_po_send_mail.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_SENDER) TYPE  SY-UNAME DEFAULT SY-UNAME
*"     VALUE(I_CONTACTS) TYPE  ZCONTACTS OPTIONAL
*"     VALUE(I_RECIPIENT) TYPE  ADR6-SMTP_ADDR
*"     VALUE(I_REPLYER) TYPE  ADR6-SMTP_ADDR OPTIONAL
*"     VALUE(I_HEAD) TYPE  ZAFO_HEAD OPTIONAL
*"     VALUE(I_FILENAME) TYPE  SO_OBJ_DES OPTIONAL
*"     VALUE(I_FILEADDR) OPTIONAL
*"  TABLES
*"      IT_ITEM STRUCTURE  ZAFO_ITEM OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA:i_header TYPE  so_obj_des.
  DATA:it_recipient LIKE TABLE OF  zafo_receivers WITH HEADER LINE.
  DATA: it_text LIKE TABLE OF  line WITH HEADER LINE.
  DATA: it_attchment  LIKE TABLE OF  zafo_attchment WITH HEADER LINE.

  DATA:l_afono TYPE zafono.
  DATA:l_text TYPE sylisel.

  CLEAR g_error.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = i_head-afono
    IMPORTING
      output = l_afono.


  PERFORM frm_check_mail_addr USING i_recipient."检查收件人邮箱格式
  PERFORM frm_check_mail_addr USING i_replyer."检查回复人邮件格式


  IF g_error = 'X'.
    et_return[] = ot_return[].
    RAISE error.
  ENDIF.





  IF i_fileaddr IS NOT INITIAL."附件列表
    PERFORM frm_check_fileaddr USING i_fileaddr.
    IF g_error = 'X'.
      et_return[] = ot_return[].
      RAISE error.
    ENDIF.
    it_attchment-filename = i_filename.
    it_attchment-fileaddr = i_fileaddr.
    it_attchment-filetype = 'PDF'.
    APPEND it_attchment.
    CLEAR it_attchment.
  ENDIF.


  IF i_head-remark1 IS INITIAL.
    i_header = i_head-remark1.
  ELSE.
    i_header = i_head-bukrs_name &&  '采购合同' && l_afono && '系统发送' ."邮件标题
  ENDIF.

  l_text = '你好' && i_contacts && ':'.
  PERFORM frm_add_text TABLES it_text USING l_text.

  l_text =  '&nbsp;&nbsp;请查收附件合同，并核对是否有误，并确认交期。' .
  PERFORM frm_add_text TABLES it_text USING l_text.

  l_text =  '&nbsp;&nbsp;注意：产前样需要分开包装，并发送德清。' .
  PERFORM frm_add_text TABLES it_text USING l_text.

  PERFORM frm_add_tab_text TABLES it_text it_item USING 'ZAFO_S_PO_MAIL'.

  PERFORM frm_add_foot_text TABLES it_text USING sy-uname.

  it_recipient-mail_addr = i_recipient.
  APPEND it_recipient.

  IF i_replyer IS INITIAL.
    SELECT SINGLE name_last2 INTO i_replyer
      FROM zapp_addr WHERE person = i_sender.
  ENDIF.


  IF i_replyer IS NOT INITIAL.
    it_recipient-mail_addr = i_recipient.
*    it_recipient-mail_copy_to = 'X'.
    it_recipient-mail_copy_to2 = 'X'.
    APPEND it_recipient.
  ENDIF.




  CALL FUNCTION 'ZAFO_BCS_MAIL'
    EXPORTING
      i_sender          = i_sender
      i_header          = i_header
      i_replyer         = i_replyer
    TABLES
      it_recipient      = it_recipient
      it_text           = it_text
      it_attchment      = it_attchment
    EXCEPTIONS
      file_upload_error = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    PERFORM frm_add_msg USING 'S' 'ZAFO' '000' '发送成功:' '' '' ''.
* Implement suitable error handling here
  ENDIF.

  IF g_error = 'X'.
    RAISE error.
  ENDIF.







ENDFUNCTION.

FORM frm_add_tab_text TABLES ct_text STRUCTURE line
                              ut_item STRUCTURE zafo_item
                              USING strname.
  DATA: dyn_table TYPE REF TO data.
  DATA: dyn_wa TYPE REF TO data.
  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any.

  DATA:lt_html TYPE TABLE OF w3html WITH HEADER LINE.
  DATA:lt_fields TYPE TABLE OF w3fields WITH HEADER LINE.
  DATA:lt_row_header TYPE TABLE OF w3head WITH HEADER LINE.

  DATA: lt_text TYPE TABLE OF line WITH HEADER LINE.
*创建动态表结构
  CREATE DATA dyn_table TYPE TABLE OF (strname).
*创建动态内表
  ASSIGN dyn_table->* TO <dyn_table>.
*创建动态工作区结构
  CREATE DATA dyn_wa LIKE LINE OF <dyn_table>.
*创建动态工作区
  ASSIGN dyn_wa->* TO <dyn_wa>.


  LOOP AT ut_item.
    MOVE-CORRESPONDING ut_item TO <dyn_wa>.
    APPEND <dyn_wa> TO <dyn_table>.
    CLEAR <dyn_wa>.
  ENDLOOP.



  CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS' "根据结构得到列抬头
    EXPORTING
      table_name = strname
    TABLES
      header     = lt_row_header.

  DATA:table_attributes LIKE  w3html.


  LOOP AT lt_row_header.
    CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT' "F每一列的格式
      EXPORTING
        field_nr  = sy-tabix
*       line_nr   = 3
        justified = 'center'
*       ICON      =
*       SYMBOL    =
*       SIZE      =
*       LINK      =
*       fgcolor   = 'BULE'
*       bgcolor   = 'GREEN'
*       FONT      =
      TABLES
        fields    = lt_fields.
  ENDLOOP.



  table_attributes = '" border="1" cellspacing="0" cellpadding="0"  align="left"'. "bgcolor="#cccccc" width="100%

  CALL FUNCTION 'WWW_ITAB_TO_HTML'
    EXPORTING
      table_attributes = table_attributes
    TABLES
      html             = lt_text
      fields           = lt_fields
      row_header       = lt_row_header
      itable           = <dyn_table>.

  lt_text-line = '<br/>'.
  APPEND lt_text TO ct_text.
  lt_text-line = '合同明细' && '<br/>'.
  APPEND lt_text TO ct_text.

  LOOP AT lt_text.
    APPEND lt_text TO ct_text.
  ENDLOOP.



ENDFORM.

FORM frm_add_text TABLES ct_text STRUCTURE line USING i_text.

  i_text = i_text && '<br/>'.
  ct_text = i_text.
  APPEND ct_text TO ct_text.

ENDFORM.

FORM frm_check_mail_addr USING mail.
  CHECK mail IS NOT INITIAL.
  IF cl_abap_matcher=>matches( pattern = '\w[-\w.+]*@([A-Za-z0-9][-A-Za-z0-9]+\.)+[A-Za-z]{2,14}' text = mail ) = abap_true.
  ELSE.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '邮件格式错误:' mail '' ''.
  ENDIF.

ENDFORM.

FORM frm_check_fileaddr USING addr.

  DATA:rcode TYPE sy-subrc.
  CALL FUNCTION 'WS_QUERY'
    EXPORTING
      filename  = addr
      query     = 'FE'
    IMPORTING
      return    = rcode
    EXCEPTIONS
      inv_query = 01
      no_batch  = 02.

  IF rcode = '0'.  "record =0 表示文件不存在
    PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '文件不存在,请先保存附件:' '' '' ''.
    RETURN.
  ENDIF.
ENDFORM.


FORM frm_add_msg USING msgty
                        msgid
                        msgno
                        msgv1
                        msgv2
                        msgv3
                        msgv4.

  CLEAR ot_return.
  IF msgty = 'E' OR msgty =  'A'.
    g_error = 'X'.
  ENDIF.
  ot_return-type = msgty.
  ot_return-id = msgid.
  ot_return-number = msgno.
  ot_return-message_v1 = msgv1.
  ot_return-message_v2 = msgv2.
  ot_return-message_v3 = msgv3.
  ot_return-message_v4 = msgv4.
  MESSAGE ID ot_return-id TYPE ot_return-type NUMBER ot_return-number
     INTO ot_return-message WITH ot_return-message_v1
                                 ot_return-message_v2
                                 ot_return-message_v3
                                 ot_return-message_v4.
  APPEND ot_return.

ENDFORM.

FORM frm_add_foot_text TABLES ct_text STRUCTURE line
                                      USING uname.
  DATA:ls_line TYPE  w3html .
  ls_line = '<h5>我是标题5 h5</h5>'.


ENDFORM.
