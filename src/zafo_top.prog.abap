*&---------------------------------------------------------------------*
*& 包含               ZAFO_TOP
*&---------------------------------------------------------------------*


*TYPE-POOLS:vrm,slis,esp1.
TABLES: mkpf,ekko,mara,ekpo,makt,eket,zafo_head,zafo_item,
        rmmg1,marc,mchb,afko,aufk,vbak,mska,vbap,t001w,t001l.
TABLES:  sscrfields.
TABLES:  zafo_shead.

DATA:gt_list  TYPE  vrm_values.
DATA:g_action TYPE zafo_action.
DATA:g_werks_flag TYPE char1.
DATA:gs_object TYPE zafo_object.
DATA:gs_bustyp TYPE zafo_bustype.
DATA:gv_create_model TYPE char1.
DATA:g_error TYPE char1.
DATA:gt_head TYPE TABLE OF zafo_shead  WITH HEADER LINE.
DATA:gt_item TYPE TABLE OF zafo_sitem  WITH HEADER LINE.
DATA:g_bustyp TYPE zafo_bustyp.
DATA ref_in TYPE abap_bool.

CONSTANTS c_fin_flag TYPE char20 VALUE 'ADFHJKZ'.
