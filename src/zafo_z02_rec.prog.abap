*&---------------------------------------------------------------------*
*& 包含               ZAFO_ZZ_REC
*&---------------------------------------------------------------------*

***************申领单特殊逻辑
FORM frm_zz_rec_check_head.

  SELECT SINGLE kostl INTO @DATA(l_kostl)
    FROM csks
    WHERE kostl = @gs_head-kostl
    AND bukrs = @gs_head-bukrs.
  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'KI' '222' gs_head-bukrs gs_head-kostl '' ''.
  ENDIF.

  IF gs_head-menge <= 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 099 '' '' '' ''."申请数量必须大于0
  ENDIF.

ENDFORM.


FORM frm_zz_gr_check_head.
  DATA:lt_usr TYPE TABLE OF zapp_replace_usr WITH HEADER LINE.

  CHECK gs_head-afnam <> sy-uname.

  SELECT SINGLE * INTO lt_usr
    FROM zapp_replace_usr
    WHERE person = gs_head-afnam
    AND person_re = sy-uname
    AND datab <= sy-datum AND datbi >= sy-datum.
  CHECK sy-subrc NE 0.

  SELECT SINGLE * INTO lt_usr
    FROM zapp_replace_usr
    WHERE person = '*'
    AND person_re = sy-uname
    AND datab <= sy-datum AND datbi >= sy-datum.
  CHECK sy-subrc NE 0.

  READ TABLE gt_item INTO DATA(ls_item) INDEX 1.

  SELECT SINGLE ernam INTO @DATA(ls_ernam)
    FROM zafo_head
    WHERE ebeln = @ls_item-ebeln.

  CHECK sy-uname <> ls_ernam.

  PERFORM frm_add_msg USING 'E' 'ZAFO' 100  '' '' '' ''."您不能签收其它人的费用
ENDFORM.
