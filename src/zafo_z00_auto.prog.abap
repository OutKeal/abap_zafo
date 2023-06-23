*&---------------------------------------------------------------------*
*& 包含               ZAFO_Z00_AUTO
*&---------------------------------------------------------------------*



FORM frm_zz_auto_ransfer_post." 绑定订单
  DATA:ct_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA lv_continue TYPE char1.

  CHECK g_error NE 'X'.
  CHECK g_bustyp = 'R1032' OR g_bustyp = 'R1033'.

  LOOP AT gt_item.
    MOVE-CORRESPONDING gt_item TO ct_item.
    IF ct_item-werks = '2000'.
      ct_item-bukrs = '1000'.
      ct_item-werks = '1000'.
    ENDIF.
    ct_item-menge4 = ct_item-menge.
    ct_item-menge4 = ct_item-menge.
    ct_item-menge4_bj = ct_item-menge_cg.

    ct_item-to_zppdhd = ct_item-zppdhd.
    ct_item-afono_ref = ct_item-afono.
    ct_item-afonr_ref = ct_item-afonr.
    APPEND ct_item.
  ENDLOOP.

  CALL FUNCTION 'ZAFO_CREATE_SAVE'
    EXPORTING
      i_bustyp  = 'R4006'
      i_post    = 'X'
    TABLES
      et_return = ot_return[]
      ct_item   = ct_item[]
*     CT_ITEM_PO         =
*     CT_ITEM_COST       =
*   CHANGING
*     CS_HEAD   =
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.
  IF sy-subrc <> 0 .
    PERFORM frm_pop_msg.
  ENDIF.
ENDFORM.


FORM frm_zz_auto_sctl_post." 生产退料
  DATA:lt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:ct_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
  DATA:cs_head TYPE zafo_head .
  DATA lv_continue TYPE char1.

  CHECK g_error NE 'X'.

  CHECK g_bustyp = 'R4011' OR g_bustyp = 'R1015' OR g_bustyp = 'ASN03'.


  CHECK gs_head-erdat <> sy-datum.

  cs_head-budat = gs_head-budat_re.

  IF gs_head-werks = '1000' .
    cs_head-kostl = '0010000210'.
    cs_head-kostl_name = '德清安泰-生产部'.
  ELSEIF gs_head-werks = '1010'.
    cs_head-kostl = '0010100060'.
    cs_head-kostl_name = '山东安泰-生产部'.
  ELSE.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '000' '公司错误！' '' '' ''.
    RETURN.
  ENDIF.

  SELECT DISTINCT i~zppdhd  INTO CORRESPONDING FIELDS OF TABLE @lt_item
    FROM zafo_head AS h
    LEFT JOIN zafo_item AS i ON i~afono = h~afono
    FOR ALL ENTRIES IN @gt_item
    WHERE i~zppdhd = @gt_item-zppdhd
    AND h~budat >= @gs_head-erdat
    AND h~bustyp = 'R2001'
    AND h~status = 'S'
    AND h~del_flag <> 'X'
    AND i~del_flag <> 'X'.

  CHECK lt_item[] IS NOT INITIAL.

  LOOP AT gt_item.
    READ TABLE lt_item WITH KEY zppdhd = gt_item-zppdhd.
    IF sy-subrc EQ 0 .
      MOVE-CORRESPONDING gt_item TO ct_item.
      ct_item-menge1 = ct_item-menge.
      ct_item-menge1_bj = ct_item-menge_cg.
      CLEAR ct_item-menge2.
      CLEAR ct_item-menge3.
      CLEAR ct_item-menge4.
      CLEAR ct_item-menge5.
      ct_item-kostl = cs_head-kostl.
      ct_item-lgort = '1009'.
      APPEND ct_item.
    ENDIF.
  ENDLOOP.

  CHECK ct_item[] IS NOT INITIAL.

  CALL FUNCTION 'ZAFO_CREATE_SAVE'
    EXPORTING
      i_bustyp  = 'R2002'
      i_post    = 'X'
    TABLES
      et_return = ot_return[]
      ct_item   = ct_item[]
*     CT_ITEM_PO         =
*     CT_ITEM_COST       =
    CHANGING
      cs_head   = cs_head
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.

  READ TABLE ot_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0 .
    g_error = 'X'.
  ENDIF.
ENDFORM.
