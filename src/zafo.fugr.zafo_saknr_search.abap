FUNCTION zafo_saknr_search.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"  CHANGING
*"     REFERENCE(SAKNR)
*"----------------------------------------------------------------------


  DATA lt_customer_found LIKE customer_found OCCURS 0 WITH HEADER LINE.
  DATA l_customer_found LIKE sy-tabix.
  DATA l_customer LIKE customer_found.
  DATA:  l_num(10) TYPE i.

  CHECK saknr IS NOT INITIAL .

  SELECT COUNT(*)
    FROM skb1
    WHERE bukrs EQ bukrs
    AND saknr EQ saknr.
  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.


  TRY.
      l_num = saknr.
    CATCH cx_root.
      saknr = '*' && saknr && '*'.
  ENDTRY.

  DATA lr_txt20 TYPE RANGE OF txt20_skat .
  lr_txt20 = VALUE #( ( sign = 'I' option = 'CP' low = saknr ) ).

  DATA: BEGIN OF lt_sak OCCURS 0,
          saknr TYPE skb1-saknr,
          txt20 TYPE skat-txt20,
        END OF lt_sak.


  SELECT a~saknr,b~txt20
    FROM skb1 AS a
    JOIN skat AS b ON b~saknr EQ a~saknr
    WHERE a~bukrs EQ @bukrs
    AND b~txt20 IN @lr_txt20
    AND b~spras EQ @sy-langu
    AND b~ktopl EQ '1000'
    INTO TABLE @lt_sak.


  IF sy-subrc NE 0.
    CLEAR saknr.
  ELSE.
*------------add by wxh AT 12.08.2021 10:09:39 ------begin--*
    DATA: tab_fields LIKE dfies  OCCURS 0 WITH HEADER LINE,
          tab_retval LIKE ddshretval OCCURS 0 WITH HEADER LINE.


    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = 'SAKNR'
        value_org        = 'S'
*       multiple_choice  = 'X'
        callback_program = 'SAPLZAFO'
        callback_form    = 'F4CALLBACK'
      TABLES
        value_tab        = lt_sak
        return_tab       = tab_retval.


    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      READ TABLE tab_retval WITH KEY fieldname = 'SAKNR'.
      saknr = tab_retval-fieldval.
    ENDIF.

*------------add by wxh at 12.08.2021 10:09:39 ------end--*


  ENDIF.



ENDFUNCTION.
