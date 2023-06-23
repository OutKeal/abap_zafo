*&---------------------------------------------------------------------*
*& 包含               ZAFO_PD_TOP
*&---------------------------------------------------------------------*
TABLES:zafo_pd_head ,zafo_pd_item,ztpp0089,zmch1,zmmv0010.


DATA:gt_pd_head TYPE TABLE OF zafo_pd_head WITH HEADER LINE.
DATA:gt_pd_item TYPE TABLE OF zafo_pd_item WITH HEADER LINE.
FIELD-SYMBOLS:<gs_pd_head> TYPE  zafo_pd_head .
FIELD-SYMBOLS:<gs_pd_item> TYPE  zafo_pd_item .

DATA:txt_werks TYPE name1.
DATA:txt_lgort TYPE name1.
DATA:txt_ernam TYPE name1.

DATA:g_change TYPE char1.

DATA:ot_return TYPE TABLE OF bapiret2 WITH HEADER LINE.
DATA gt_message TYPE TABLE OF esp1_message_wa_type WITH HEADER LINE.
DATA:g_error TYPE char1.
DATA:gv_jump TYPE char1.
