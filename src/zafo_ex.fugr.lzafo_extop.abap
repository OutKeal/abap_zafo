FUNCTION-POOL zafo_ex.                      "MESSAGE-ID ..

* INCLUDE LZAFO_EXD...                       " Local class definition



DATA gt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
DATA batch_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
DATA:gt_fcat TYPE TABLE OF lvc_s_fcat WITH HEADER LINE  .
DATA:g_model TYPE char1.

DATA:BEGIN OF gt_model OCCURS 0,
       model TYPE tpl_id,
       name  TYPE name1,
     END OF gt_model.


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
