*&---------------------------------------------------------------------*
*& 包含               ZAFO_EX_F01
*&---------------------------------------------------------------------*

FORM frm_convert_zero USING value CHANGING name1.
  DATA : l_str  TYPE  string .
  l_str = value.
  FIND '.' IN l_str.
  CHECK sy-subrc EQ 0.

  IF  value  IS  NOT  INITIAL .
    l_str  =  value .
    CONDENSE  l_str .  " 去掉没用的小数位的0
    SHIFT  l_str RIGHT DELETING TRAILING  '0' . "去掉没用的小数位的0
    SHIFT  l_str RIGHT DELETING TRAILING  '.' . "去掉没用的小数位的0
  ELSE .
    l_str  =  '' . "为空就给个空值
  ENDIF .
  CONDENSE  l_str .
  name1  =  l_str .

ENDFORM.
