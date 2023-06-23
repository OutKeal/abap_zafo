*&---------------------------------------------------------------------*
*& 包含               ZAFO_QC_TOP
*&---------------------------------------------------------------------*
TABLES:zafo_item.
TABLES:zafo_qc_head.
TABLES:zafo_qc_item.


DATA:gt_head TYPE TABLE OF zafo_qc_shead WITH HEADER LINE." 质检数据单

DATA:gs_item TYPE zafo_qc_sitem.
DATA:gt_item TYPE TABLE OF zafo_qc_sitem WITH HEADER LINE." 质检单
DATA:gt_item_i TYPE TABLE OF zafo_qc_item_i WITH HEADER LINE." 辅料明细
DATA:gt_qc_line LIKE TABLE OF zafo_qc_line WITH HEADER LINE." 面料卷明细
DATA:gt_qc_line_4fz LIKE TABLE OF zafo_qc_line_4fz WITH HEADER LINE ." 质检卷明细评分

DATA:gt_qc_line_4fz_dis LIKE TABLE OF zafo_qc_line_4fz WITH HEADER LINE .

DATA:gt_head_modify TYPE TABLE OF zafo_qc_head WITH HEADER LINE.
DATA:gt_item_modify TYPE TABLE OF zafo_qc_item WITH HEADER LINE.

DATA: gs_head TYPE zafo_qc_shead.
FIELD-SYMBOLS: <gs_head> TYPE zafo_qc_shead.
FIELD-SYMBOLS: <gs_item> TYPE zafo_qc_sitem.

FIELD-SYMBOLS: <gs_item_text> TYPE any.

DATA:gs_model_h TYPE  zafo_qc_model_h ." 质检模板配置表
DATA:gt_model_i TYPE TABLE OF zafo_qc_model_i WITH HEADER LINE." 质检模板配置表 明细

DATA:gt_item_t TYPE TABLE OF zafo_qc_item_t WITH HEADER LINE."
DATA:gt_model_t TYPE TABLE OF zafo_qc_model_t WITH HEADER LINE."


DATA:gt_qc_scol TYPE TABLE OF zafo_qc_scol WITH HEADER LINE.

FIELD-SYMBOLS: <gs_qc_line> TYPE zafo_qc_line.
FIELD-SYMBOLS: <gs_qc_line_4fz> TYPE zafo_qc_line_4fz.


DATA:g_ucomm TYPE sy-ucomm.
DATA:g_error TYPE char1.
DATA:g_change TYPE char1.
DATA:gv_qcmode TYPE char1.


DATA gt_message TYPE TABLE OF esp1_message_wa_type WITH HEADER LINE.
DATA:ot_return TYPE TABLE OF bapiret2 WITH HEADER LINE.


DATA:gt_head_sure TYPE TABLE OF zafo_qc_shead WITH HEADER LINE." 质检数确认
DATA:g_menge_qc   TYPE zafo_qc_shead-menge_qc,
     g_menge_qc_f TYPE zafo_qc_shead-menge_qc_f.
DATA:g_menge_qc_f1 TYPE zafo_qc_shead-menge_qc_f,
     g_menge_qc1   TYPE zafo_qc_shead-menge_qc.
