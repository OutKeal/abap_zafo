FUNCTION zafo_customer_search.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  CHANGING
*"     REFERENCE(KUNNR)
*"----------------------------------------------------------------------


  DATA lt_customer_found LIKE customer_found OCCURS 0 WITH HEADER LINE.
  DATA l_customer_found LIKE sy-tabix.
  DATA l_customer LIKE customer_found.
  DATA:  l_num(10) TYPE i.

  CHECK kunnr IS NOT INITIAL .


  TRY.
      l_num = kunnr.
    CATCH cx_root.
      kunnr = '*' && kunnr && '*'.
  ENDTRY.

  CALL FUNCTION 'MM_CUSTOMER_SEARCH'
    EXPORTING
      i_string          = kunnr
*     I_BUKRS           =
    IMPORTING
      e_customers_found = l_customer_found
    TABLES
      t_customer_found  = lt_customer_found.

  IF l_customer_found EQ 0.
    CLEAR kunnr.
  ELSEIF l_customer_found EQ 1.
    READ TABLE lt_customer_found INDEX 1.
    kunnr = lt_customer_found-kunnr.
  ELSE.
    CALL FUNCTION 'MM_CUSTOMER_SHOW_HITS'
      IMPORTING
        e_customer_return = l_customer
      TABLES
        t_customer_value  = lt_customer_found.

    kunnr = l_customer-kunnr.

  ENDIF.

  IF kunnr IS NOT INITIAL .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = kunnr
      IMPORTING
        output = kunnr.
  ENDIF.

ENDFUNCTION.
