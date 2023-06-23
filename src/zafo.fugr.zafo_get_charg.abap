FUNCTION zafo_get_charg.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_COMMIT) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      CT_ZMCH1 STRUCTURE  ZMCH1 OPTIONAL
*"  CHANGING
*"     VALUE(CS_ZMCH1) TYPE  ZMCH1 OPTIONAL
*"----------------------------------------------------------------------

  DATA:new_zmchb1 TYPE TABLE OF zmch1 WITH HEADER LINE..

  IF cs_zmch1 IS NOT INITIAL.

    SELECT SINGLE charg INTO cs_zmch1-charg
      FROM zmch1
      WHERE sgt_scat     = cs_zmch1-sgt_scat
        AND zcolor       = cs_zmch1-zcolor
*        AND zcolor_text  = cs_zmch1-zcolor_text
        AND zsize        = cs_zmch1-zsize
        AND znorms       = cs_zmch1-znorms
        AND zshelves     = cs_zmch1-zshelves
        AND zvat_nub     = cs_zmch1-zvat_nub
        AND zppflag      = cs_zmch1-zppflag.

    IF sy-subrc NE 0.
      PERFORM frm_get_next_batch CHANGING cs_zmch1-charg.
      new_zmchb1 = cs_zmch1.
      APPEND new_zmchb1.
    ENDIF.
  ENDIF.



  IF ct_zmch1[] IS NOT INITIAL.
    SELECT * FROM zmch1
      INTO TABLE @DATA(lt_zmch1)
      FOR ALL ENTRIES IN @ct_zmch1
      WHERE sgt_scat     = @ct_zmch1-sgt_scat
        AND zcolor       = @ct_zmch1-zcolor
*        AND zcolor_text  = @ct_zmch1-zcolor_text
        AND zsize        = @ct_zmch1-zsize
        AND znorms       = @ct_zmch1-znorms
        AND zshelves     = @ct_zmch1-zshelves
        AND zvat_nub     = @ct_zmch1-zvat_nub
        AND zppflag      = @ct_zmch1-zppflag.

    LOOP AT ct_zmch1 ASSIGNING FIELD-SYMBOL(<cs_zmch1>) .
      READ TABLE lt_zmch1 INTO DATA(ls_zmch1)
                          WITH KEY sgt_scat     = <cs_zmch1>-sgt_scat
                                   zcolor       = <cs_zmch1>-zcolor
*                                   zcolor_text  = cs_zmch1-zcolor_text
                                   zsize        = <cs_zmch1>-zsize
                                   znorms       = <cs_zmch1>-znorms
                                   zshelves     = <cs_zmch1>-zshelves
                                   zvat_nub     = <cs_zmch1>-zvat_nub
                                   zppflag     = <cs_zmch1>-zppflag.
      IF sy-subrc EQ 0.
        <cs_zmch1>-charg = ls_zmch1-charg.
      ELSE.
        PERFORM frm_get_next_batch CHANGING <cs_zmch1>-charg.
        new_zmchb1 = <cs_zmch1>.

        APPEND new_zmchb1 TO lt_zmch1.

        APPEND new_zmchb1.
        CLEAR new_zmchb1.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF new_zmchb1[] IS NOT INITIAL.
    MODIFY zmch1 FROM TABLE new_zmchb1.
  ENDIF.

  IF new_zmchb1[] IS NOT INITIAL AND i_commit = 'X'.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.


FORM frm_get_next_batch  CHANGING ls_charg.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'       " 锁定编码对象
    EXPORTING
      object           = 'ZMM_CHARG'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'            " 获取流水号
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZMM_CHARG'
    IMPORTING
      number                  = ls_charg
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      internal_overflow       = 6
      OTHERS                  = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'       " 释放编码对象锁
    EXPORTING
      object = 'ZMM_CHARG'.
  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
