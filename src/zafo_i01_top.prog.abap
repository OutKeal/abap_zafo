*&---------------------------------------------------------------------*
*& 包含               ZAFO_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS:vrm,slis,esp1.
TABLES: zafo_head,zafo_item,
        vbak,vbap,zsdsch,ztpp0089,ztpp0091,afko,aufk,
        ekko,ekpo,eket,zscmt0011,
        mska, mara,makt,mkpf,rmmg1,marc,mchb,
        zmmt0010,zscmt0010,t001w,t001l.
TABLES zmmv0010.

TABLES:sscrfields.
TABLES:zafo_shead.


TYPES:BEGIN OF ty_afo ,
        bustyp        LIKE zafo_head-bustyp,
        zzpino        LIKE zafo_item-zzpino,
        zppdhd        LIKE zafo_item-zppdhd,
        vbeln_va      LIKE zafo_head-vbeln_va,
        posnr_va      LIKE zafo_head-posnr_va,
        matnr         LIKE zafo_item-matnr,
        maktx         LIKE zafo_item-maktx,
        idnlf         LIKE zafo_item-idnlf,
        zcolor        LIKE zafo_item-zcolor,
        zsize         LIKE zafo_item-zsize,
        znorms        LIKE zafo_item-znorms,
        zppflag       LIKE zafo_item-zppflag,
        zvat_nub      LIKE zafo_item-zvat_nub,
        werks         LIKE zafo_item-werks,
        bukrs         LIKE zafo_item-bukrs,
        aufnr         LIKE zafo_item-aufnr,
        meins         LIKE zafo_item-meins,
        bprme         LIKE zafo_item-bprme,
        zmm_tran_rate LIKE zafo_item-zmm_tran_rate,
        menge         LIKE zafo_item-menge,
      END OF ty_afo.

DATA:subscreen(4) TYPE c VALUE '1001'.
DATA:alvsubscreen(4) TYPE c VALUE '0100'.

CONSTANTS:common_head TYPE char4 VALUE '0400'.


DATA:g_werks TYPE werks_d.
DATA:g_werks_flag TYPE char1.

DATA:g_no_save TYPE char1.
DATA:g_no_commit TYPE char1.
DATA:g_action TYPE zafo_action.
DATA:g_readonly TYPE char1.

DATA:g_locked TYPE char1.
DATA:g_refresh_catalog TYPE char1.
DATA:g_no_authcheck TYPE char1.
DATA:g_f4_contract TYPE char1.
DATA:g_ok_code TYPE sy-ucomm.
DATA:g_field TYPE char30.
DATA:g_ean TYPE char40.
DATA:g_scan_icon TYPE icon_d.
DATA:g_scan_msg TYPE char40.
DATA:g_kzfae TYPE kzfae.

DATA:g_error TYPE char1.
DATA:g_exec_flag TYPE char1.

DATA:gv_jump TYPE char1.
DATA:ot_return TYPE TABLE OF bapiret2 WITH HEADER LINE.
DATA:gt_message TYPE TABLE OF esp1_message_wa_type WITH HEADER LINE.

DATA: BEGIN OF gt_fcode OCCURS 0,
        status     TYPE zafo_status,
        app_status TYPE zapp_status,
        fcode      TYPE sy-ucomm,
      END OF gt_fcode.


DATA:g_bustyp TYPE zafo_bustyp.
DATA:g_bustyp_ref TYPE zafo_bustyp.
DATA:g_object TYPE zafo_eobject.
DATA:g_object_ref TYPE zafo_eobject.
DATA:gs_object TYPE zafo_object.
DATA:gs_object_ref TYPE zafo_object.

DATA:gs_bustyp TYPE zafo_bustype.
DATA:gs_bustyp_ref TYPE zafo_bustype.

DATA:gt_object TYPE TABLE OF zafo_object.
DATA:gt_bustyp TYPE TABLE OF zafo_bustype.

DATA:gt_screen TYPE TABLE OF zafo_screen WITH HEADER LINE.
DATA:gt_screen_text TYPE TABLE OF zafo_screen_text WITH HEADER LINE.
DATA:gt_reason TYPE TABLE OF zafo_reason WITH HEADER LINE.

DATA:gt_sel_screen TYPE TABLE OF zafo_sel_screen WITH HEADER LINE.
DATA:gt_sel_scr_bty TYPE TABLE OF zafo_sel_scr_bty WITH HEADER LINE.
DATA:gt_sel_value TYPE TABLE OF zafo_sel_value WITH HEADER LINE.

DATA:gs_head_modify TYPE zafo_head.
DATA:gs_head_modify_ref TYPE zafo_head.
DATA:gt_item_modify TYPE TABLE OF zafo_item WITH HEADER LINE.
DATA:gt_item_po_modify TYPE TABLE OF zafo_item_po WITH HEADER LINE.
DATA:gt_item_cost_modify TYPE TABLE OF zafo_item_cost WITH HEADER LINE.
DATA:gt_item_po_cpt_modify TYPE TABLE OF zafo_item_po_cpt WITH HEADER LINE.

DATA:gs_head TYPE  zafo_shead  .
DATA:gt_head TYPE TABLE OF zafo_shead  WITH HEADER LINE.

DATA:gt_head_dis TYPE TABLE OF zafo_shead  WITH HEADER LINE.

DATA:gt_item TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
DATA:gt_item_100 TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
DATA:gt_item_dis TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
DATA:gt_item_front_dis TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
DATA:gt_item_sum TYPE TABLE OF zafo_sitem  WITH HEADER LINE.

DATA:gs_item TYPE  zafo_sitem.
DATA:gs_item_dis TYPE  zafo_sitem.

DATA:gt_item_serve TYPE TABLE OF zafo_item_serve WITH HEADER LINE.
DATA:gs_item_serve TYPE zafo_item_serve.

DATA:gt_item_po TYPE TABLE OF zafo_sitem_po WITH HEADER LINE.
DATA:gt_item_po_100 TYPE TABLE OF zafo_sitem_po WITH HEADER LINE.

DATA:gt_cost TYPE TABLE OF zafo_cost WITH HEADER LINE..
DATA:gt_item_cost TYPE TABLE OF zafo_item_cost WITH HEADER LINE.

DATA:gt_item_po_cpt TYPE TABLE OF zafo_sitem_po_cpt WITH HEADER LINE.

DATA:gt_item_batch TYPE TABLE OF zafo_item_batch WITH HEADER LINE.
DATA:gt_item_batch_dis TYPE TABLE OF zafo_item_batch WITH HEADER LINE.

DATA:gt_barcode TYPE TABLE OF zmm_barcode WITH HEADER LINE.
DATA:gs_barcode_dis TYPE zmm_sbarcode.
DATA:gt_barcode_dis TYPE TABLE OF zmm_sbarcode.


DATA:gs_0010 TYPE zmmt0010,
     gt_0010 TYPE TABLE OF zmmt0010.

DATA:gt_line TYPE TABLE OF line.
DATA:gt_tline TYPE TABLE OF tline.


FIELD-SYMBOLS <gt_tab>  TYPE STANDARD TABLE.
FIELD-SYMBOLS <gs_tab>  TYPE any.
FIELD-SYMBOLS <gs_text> TYPE any.
FIELD-SYMBOLS <gs_head> TYPE zafo_shead.
FIELD-SYMBOLS <gs_head_dis> TYPE zafo_shead.
FIELD-SYMBOLS <gs_item> TYPE zafo_sitem.
FIELD-SYMBOLS <gs_item_sum> TYPE zafo_sitem.
FIELD-SYMBOLS <gs_item_po> TYPE zafo_sitem_po.
FIELD-SYMBOLS <gs_item_cost> TYPE zafo_item_cost.
FIELD-SYMBOLS <gs_item_po_cpt> TYPE zafo_sitem_po_cpt.
FIELD-SYMBOLS <gs_item_serve> TYPE zafo_item_serve.
