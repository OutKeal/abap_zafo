FUNCTION zafo_vendor_search.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  CHANGING
*"     VALUE(LIFNR)
*"     VALUE(O_LIFNR) OPTIONAL
*"     VALUE(O_LIFNR_NAME) OPTIONAL
*"----------------------------------------------------------------------

  DATA:  lt_vendors      LIKE vendor_found OCCURS 0 WITH HEADER LINE.
  DATA:  l_vendors_found LIKE sy-tabix.
  DATA:  l_vendor        LIKE vendor_found.
  DATA:  lv_is_name TYPE char1.
  DATA: out_lifnr      TYPE lifnr,
        out_lifnr_name TYPE ad_name1.
  DATA: ll_lifnr      TYPE lifnr,
        in_lifnr      TYPE lifnr,
        in_lifnr_text TYPE char30,
        ll_text       TYPE char30.

  CHECK lifnr IS NOT INITIAL .

  SHIFT lifnr RIGHT DELETING TRAILING space.
  SHIFT lifnr LEFT DELETING LEADING space.

  ll_text = '0123456789'.
  in_lifnr_text = lifnr.

  IF in_lifnr_text CO ll_text.

    CONDENSE lifnr NO-GAPS.
    lv_is_name = abap_false.

    in_lifnr = lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = in_lifnr
      IMPORTING
        output = ll_lifnr.
    SELECT SINGLE partner,zname1
      INTO (@out_lifnr ,@out_lifnr_name)
      FROM zscmt0010
      WHERE partner EQ @ll_lifnr.
    IF sy-subrc EQ 0 .
      lifnr = out_lifnr.
      o_lifnr = out_lifnr.
      o_lifnr_name = out_lifnr_name.
      RETURN.
    ENDIF.

    IF strlen( lifnr ) > 8 OR lifnr CA '*' .
      lifnr =  lifnr.
    ELSE.
      lifnr = '*' && lifnr && '*'.
    ENDIF.

  ELSE.
    lv_is_name = abap_true.

    CALL FUNCTION 'MM_VENDOR_SEARCH'
      EXPORTING
        i_string        = lifnr
      IMPORTING
        e_vendors_found = l_vendors_found
      TABLES
        t_vendor_found  = lt_vendors.
    IF l_vendors_found EQ 0 .
      lifnr = '*' && lifnr && '*'.
    ENDIF.

  ENDIF.

  IF l_vendors_found EQ 0 .
    CALL FUNCTION 'MM_VENDOR_SEARCH'
      EXPORTING
        i_string        = lifnr
      IMPORTING
        e_vendors_found = l_vendors_found
      TABLES
        t_vendor_found  = lt_vendors.

  ENDIF.


  IF l_vendors_found EQ 0.

    CLEAR: o_lifnr,o_lifnr_name.
  ELSEIF l_vendors_found EQ 1.
    READ TABLE lt_vendors INDEX 1.
    out_lifnr = lt_vendors-lifnr.
    out_lifnr_name = lt_vendors-name.
  ELSE.
    CALL FUNCTION 'MM_VENDOR_SHOW_HITS'
      IMPORTING
        e_vendor_return = l_vendor
      TABLES
        t_vendor_value  = lt_vendors.
    out_lifnr = l_vendor-lifnr.
    out_lifnr_name = l_vendor-name.
  ENDIF.

  lifnr = out_lifnr.
  o_lifnr = out_lifnr.
  o_lifnr_name = out_lifnr_name.


ENDFUNCTION.
