*&---------------------------------------------------------------------*
*& 包含               ZAFO_I01_SRC_TOP
*&---------------------------------------------------------------------*

  DATA:src_werks TYPE werks_d.
  DATA:src_werks_flag TYPE char1.

  DATA:src_no_save TYPE char1.
  DATA:src_no_commit TYPE char1.
  DATA:src_action TYPE zafo_action.
  DATA:src_readonly TYPE char1.
  DATA:src_locked TYPE char1.
  DATA:src_refresh_catalog TYPE char1.
  DATA:src_no_authcheck TYPE char1.
  DATA:src_f4_contract TYPE char1.
  DATA:src_ok_code TYPE sy-ucomm.
  DATA:src_field TYPE char30.
  DATA:src_ean TYPE char40.
  DATA:src_scan_icon TYPE icon_d.
  DATA:src_scan_msg TYPE char40.
  DATA:src_kzfae TYPE kzfae.

  DATA:src_error TYPE char1.
  DATA:src_exec_flag TYPE char1.

  DATA:srcv_jump TYPE char1.

  DATA: BEGIN OF srct_fcode OCCURS 0,
          status     TYPE zafo_status,
          app_status TYPE zapp_status,
          fcode      TYPE sy-ucomm,
        END OF srct_fcode.

  DATA:src_bustyp TYPE zafo_bustyp.
  DATA:src_bustyp_ref TYPE zafo_bustyp.
  DATA:src_object TYPE zafo_eobject.
  DATA:src_object_ref TYPE zafo_eobject.
  DATA:srcs_object TYPE zafo_object.
  DATA:srcs_object_ref TYPE zafo_object.

  DATA:srcs_bustyp TYPE zafo_bustype.
  DATA:srcs_bustyp_ref TYPE zafo_bustype.

  DATA:srct_object TYPE TABLE OF zafo_object.
  DATA:srct_bustyp TYPE TABLE OF zafo_bustype.

  DATA:srct_screen TYPE TABLE OF zafo_screen WITH HEADER LINE.
  DATA:srct_screen_text TYPE TABLE OF zafo_screen_text WITH HEADER LINE.
  DATA:srct_reason TYPE TABLE OF zafo_reason WITH HEADER LINE.


  DATA:srct_sel_screen TYPE TABLE OF zafo_sel_screen WITH HEADER LINE.
  DATA:srct_sel_scr_bty TYPE TABLE OF zafo_sel_scr_bty WITH HEADER LINE.
  DATA:srct_sel_value TYPE TABLE OF zafo_sel_value WITH HEADER LINE.


  DATA:srcs_head_modify TYPE zafo_head  .
  DATA:srcs_head_modify_ref TYPE zafo_head  .
  DATA:srct_item_modify TYPE TABLE OF zafo_item  WITH HEADER LINE.
  DATA:srct_item_po_modify TYPE TABLE OF zafo_item_po  WITH HEADER LINE.
  DATA:srct_item_cost_modify TYPE TABLE OF zafo_item_cost  WITH HEADER LINE.

  DATA:srcs_head TYPE  zafo_shead  .
  DATA:srct_head TYPE TABLE OF zafo_shead  WITH HEADER LINE.

  DATA:srct_head_dis TYPE TABLE OF zafo_shead  WITH HEADER LINE.

  DATA:srct_item TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
  DATA:srct_item_100 TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
  DATA:srct_item_dis TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
  DATA:srct_item_front_dis TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
  DATA:srct_item_sum TYPE TABLE OF zafo_sitem  WITH HEADER LINE.

  DATA:srcs_item TYPE  zafo_sitem.
  DATA:srcs_item_dis TYPE  zafo_sitem.

  DATA:srct_item_serve TYPE TABLE OF zafo_item_serve WITH HEADER LINE.
  DATA:srcs_item_serve TYPE zafo_item_serve.

  DATA:srct_item_po TYPE TABLE OF zafo_sitem_po WITH HEADER LINE.
  DATA:srct_item_po_100 TYPE TABLE OF zafo_sitem_po WITH HEADER LINE.

  DATA:srct_cost TYPE TABLE OF zafo_cost WITH HEADER LINE..
  DATA:srct_item_cost TYPE TABLE OF zafo_item_cost WITH HEADER LINE.

  DATA:srct_item_po_cpt TYPE TABLE OF zafo_sitem_po_cpt WITH HEADER LINE..

  DATA:srct_item_batch TYPE TABLE OF zafo_item_batch WITH HEADER LINE.
  DATA:srct_item_batch_dis TYPE TABLE OF zafo_item_batch WITH HEADER LINE.

  DATA:srct_barcode TYPE TABLE OF zmm_barcode WITH HEADER LINE.
  DATA:srcs_barcode_dis TYPE zmm_sbarcode.
  DATA:srct_barcode_dis TYPE TABLE OF zmm_sbarcode.

  DATA:srcs_0010 TYPE zmmt0010,
       srct_0010 TYPE TABLE OF zmmt0010.

  DATA:srct_line TYPE TABLE OF line.
  DATA:srct_tline TYPE TABLE OF tline.
