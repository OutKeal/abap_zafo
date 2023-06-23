*&---------------------------------------------------------------------*
*& 包含               ZAFO_ZZ_DUC
*&---------------------------------------------------------------------*

***********扣款单特殊逻辑

FORM frm_zz_duc_check_head.

  SELECT SINGLE lifnr INTO @DATA(l_lifrn) FROM lfb1
    WHERE lifnr = @gs_head-lifnr
    AND bukrs = @gs_head-bukrs.
  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'F5' '104' gs_head-lifnr gs_head-bukrs '' ''.
  ENDIF.

  IF gs_head-amount <= 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' 101 '' '' '' ''."扣款单金额必须大于0
  ENDIF.

ENDFORM.
