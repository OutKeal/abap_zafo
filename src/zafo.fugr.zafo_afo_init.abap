FUNCTION ZAFO_AFO_INIT.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_BUSTYP) TYPE  ZAFO_BUSTYP OPTIONAL
*"     VALUE(I_AFONO) TYPE  ZAFONO
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_head
    FROM zafo_head WHERE afono = i_afono.
  IF sy-subrc NE 0.
    PERFORM frm_add_msg USING 'E' 'ZAFO' '010' '' '' '' ''."该单号不存在
    RETURN.
  ENDIF.

  g_werks = gs_head-werks.
  g_bustyp = gs_head-bustyp.
  g_object = gs_head-object.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item
    FROM zafo_item
    WHERE afono = i_afono
    AND del_flag = ''.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_item_batch
    FROM zafo_item_batch WHERE afono = i_afono.

  PERFORM frm_get_config.
  IF g_error = 'X'.
    RETURN.
  ENDIF.

  DATA: lr_zzpino TYPE RANGE OF zzpino.
  PERFORM frm_set_zzpino_hide TABLES lr_zzpino.
  IF lr_zzpino[] IS NOT INITIAL.
    DELETE gt_item WHERE zzpino IN lr_zzpino.
  ENDIF.

ENDFUNCTION.
