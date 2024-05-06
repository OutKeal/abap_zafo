FUNCTION-POOL zafo_ex.                      "MESSAGE-ID ..

* INCLUDE LZAFO_EXD...                       " Local class definition


DATA: gt_t023t TYPE TABLE OF t023t.
DATA: gt_t006a TYPE TABLE OF t006a.
DATA:BEGIN OF gt_user OCCURS 0,
       bname      TYPE bname,
       name_textc TYPE name_text,
     END OF gt_user.


FORM frm_convert_zero USING input CHANGING output.
  DATA : l_str  TYPE  string .
  DATA : l_neg TYPE abap_bool.
  DATA: l_int(15) TYPE c.
  DATA: l_dec(6) TYPE c.
  DATA: elemdescr TYPE REF TO cl_abap_elemdescr.
  elemdescr ?= cl_abap_elemdescr=>describe_by_data( input ).
  l_str = input.
  FIND '-' IN l_str.
  IF sy-subrc EQ 0.
    l_neg = 'X'.
    REPLACE '-' IN l_str  WITH ''.
  ENDIF.
  SPLIT l_str AT '.' INTO l_int l_dec.

  IF  l_dec  IS  NOT  INITIAL .
    CONDENSE  l_str .
    SHIFT  l_dec RIGHT DELETING TRAILING  '0' . "去掉没用的小数位的0
  ENDIF.

  IF l_dec IS INITIAL.
    output  =  l_int.
  ELSE.
    output  =  l_int && '.' && l_dec.
  ENDIF .

  IF l_neg = 'X'.
    output = '-' && output.
  ENDIF.
ENDFORM.

FORM frm_convert_zero1 USING input CHANGING output.
  DATA : l_str TYPE string.
  DATA : l_neg TYPE abap_bool.
  DATA : l_int TYPE string.
  DATA : l_dec TYPE string.
  DATA : curnt_off TYPE i.
  l_str = input.
  CLEAR output.

  FIND '-' IN l_str.
  IF sy-subrc EQ 0.
    l_neg = 'X'.
    REPLACE '-' IN l_str  WITH ''.
  ENDIF.
  SPLIT l_str AT '.' INTO l_int l_dec.

  curnt_off = strlen( l_int ) - 1.

  WHILE curnt_off >= 0.
    output = l_int+curnt_off(1) && output.
    IF sy-index MOD 3 = 0   .
      output = ',' && output.
    ENDIF.
    curnt_off -= 1.
  ENDWHILE.
  IF output+0(1) = ','.
    output+0(1) = ''.
    CONDENSE output NO-GAPS.
  ENDIF.

  IF  l_dec  IS  NOT  INITIAL .
    CONDENSE l_dec NO-GAPS.
    SHIFT l_dec RIGHT DELETING TRAILING '0'.
    CONDENSE l_dec NO-GAPS.
  ENDIF.

  IF l_dec IS NOT INITIAL.
    output  =  output && '.' &&  l_dec.
  ENDIF .

  IF l_neg = 'X'.
    output = '-' && output.
  ENDIF.
ENDFORM.
