FUNCTION zafo_conversion.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_EXEC_ENV) TYPE  CHAR5
*"----------------------------------------------------------------------
  IF i_exec_env EQ 'START'.
    src_werks = g_werks.
    src_werks_flag = g_werks_flag.
    src_no_save = g_no_save.
    src_no_commit = g_no_commit.
    src_action =  g_action.
    src_readonly = g_readonly .
    src_locked = g_locked.
    src_refresh_catalog = g_refresh_catalog .
    src_no_authcheck = g_no_authcheck.
    src_f4_contract = g_f4_contract.
    src_ok_code = g_ok_code.
    src_field = g_field.
    src_ean = g_ean.
    src_scan_icon = g_scan_icon.
    src_scan_msg = g_scan_msg.
    src_kzfae = g_kzfae.

    src_error = g_error.
    src_exec_flag = g_exec_flag.

    srcv_jump = gv_jump .

    srct_fcode[] = gt_fcode[].

    src_bustyp = g_bustyp.
    src_bustyp_ref = g_bustyp_ref.
    src_object = g_object.
    src_object_ref = g_object_ref.
    srcs_object = gs_object.
    srcs_object_ref = gs_object_ref.

    srcs_bustyp = gs_bustyp.
    srcs_bustyp_ref = gs_bustyp_ref.

    srct_object[] = gt_object[].
    srct_bustyp[] = gt_bustyp[].

    srct_screen[] = gt_screen[].
    srct_screen_text[] = gt_screen_text[].
    srct_reason[] = gt_reason[].

    srct_sel_value[] = gt_sel_value[].
    srct_sel_screen[] = gt_sel_screen[].
    srct_sel_scr_bty[] = gt_sel_scr_bty[].

    srcs_head_modify = gs_head_modify.
    srcs_head_modify_ref = gs_head_modify_ref.
    srct_item_modify[] = gt_item_modify[].
    srct_item_po_modify[] = gt_item_po_modify[].
    srct_item_cost_modify[] = gt_item_cost_modify[].

    srcs_head = gs_head.
    srct_head[] = gt_head[].

    srct_head_dis[] = gt_head_dis[].

    srct_item[] = gt_item[].
    srct_item_100[] = gt_item_100[].
    srct_item_dis[] = gt_item_dis[].
    srct_item_front_dis[] = gt_item_front_dis[].
    srct_item_sum[] = gt_item_sum[].

    srcs_item  = gs_item.
    srcs_item_dis = gs_item_dis .

    srcs_item_serve = gs_item_serve.
    srct_item_serve[] = gt_item_serve[].

    srct_item_po[] = gt_item_po[].
    srct_item_po_100[] = gt_item_po_100[].

    srct_cost[] = gt_cost[].
    srct_item_cost[] = gt_item_cost[].

    srct_item_po_cpt[] = gt_item_po_cpt[].

    srct_item_batch[] = gt_item_batch[].
    srct_item_batch_dis[] = gt_item_batch_dis[].

    srct_barcode[] = gt_barcode[].

    srcs_0010 = gs_0010.
    srct_0010[] = gt_0010[].

    srct_line[] = gt_line[].
    srct_tline[] = gt_tline[].
  ENDIF.


  IF i_exec_env EQ 'END'.
    g_werks = src_werks.
    g_werks_flag = src_werks_flag.

    g_no_save = src_no_save.
    g_no_commit = src_no_commit.
    g_action =  src_action.
    g_readonly = src_readonly .
    g_locked = src_locked.
    g_refresh_catalog = src_refresh_catalog .
    g_no_authcheck = src_no_authcheck.
    g_f4_contract = src_f4_contract.
    g_ok_code = src_ok_code.
    g_field = src_field.
    g_ean = src_ean.
    g_scan_icon = src_scan_icon.
    g_scan_msg = src_scan_msg.
    g_kzfae = src_kzfae.

    g_error = src_error.
    g_exec_flag = src_exec_flag.

    gv_jump = srcv_jump .

    gt_fcode[] = srct_fcode[].

    g_bustyp = src_bustyp.
    g_bustyp_ref = src_bustyp_ref.
    g_object = src_object.
    g_object_ref = src_object_ref.
    gs_object = srcs_object.
    gs_object_ref = srcs_object_ref.

    gs_bustyp = srcs_bustyp.
    gs_bustyp_ref = srcs_bustyp_ref.

    gt_object[] = srct_object[].
    gt_bustyp[] = srct_bustyp[].

    gt_screen[] = srct_screen[].
    gt_screen_text[] = srct_screen_text[].
    gt_reason[] = srct_reason[].

    gt_sel_value[] = srct_sel_value[].
    gt_sel_screen[] = srct_sel_screen[].
    gt_sel_scr_bty[] = srct_sel_scr_bty[].

    gs_head_modify = srcs_head_modify.
    gs_head_modify_ref = srcs_head_modify_ref.
    gt_item_modify[] = srct_item_modify[].
    gt_item_po_modify[] = srct_item_po_modify[].
    gt_item_cost_modify[] = srct_item_cost_modify[].

    gs_head = srcs_head.
    gt_head[] = srct_head[].

    gt_head_dis[] = srct_head_dis[].

    gt_item[] = srct_item[].
    gt_item_100[] = srct_item_100[].
    gt_item_dis[] = srct_item_dis[].
    gt_item_front_dis[] = srct_item_front_dis[].
    gt_item_sum[] = srct_item_sum[].

    gs_item  = srcs_item.
    gs_item_dis = srcs_item_dis .

    gs_item_serve = srcs_item_serve.
    gt_item_serve[] = srct_item_serve[].

    gt_item_po[] = srct_item_po[].
    gt_item_po_100[] = srct_item_po_100[].

    gt_cost[] = srct_cost[].
    gt_item_cost[] = srct_item_cost[].

    gt_item_po_cpt[] = srct_item_po_cpt[].

    gt_item_batch[] = srct_item_batch[].
    gt_item_batch_dis[] = srct_item_batch_dis[].

    gt_barcode[] = srct_barcode[].

    gs_0010 = srcs_0010.
    gt_0010[] = srct_0010[].

    gt_line[] = srct_line[].
    gt_tline[] = srct_tline[].

  ENDIF.

ENDFUNCTION.
