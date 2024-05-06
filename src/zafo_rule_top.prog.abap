*&---------------------------------------------------------------------*
*& 包含               ZAFO_RULE_TOP
*&---------------------------------------------------------------------*

TABLES:zafo_rule.

TYPES:BEGIN OF ty_tree ,
        position  TYPE position,
        fieldtext TYPE fieldtext,
        icon      TYPE icon_d,
        rollname  TYPE rollname,
        fieldname TYPE fieldname,
        inttype   TYPE inttype,
        leng      TYPE leng,
        group     TYPE char40,
        parent    TYPE char40,
        tabname   TYPE tabname,
        node_key  TYPE lvc_nkey,
      END OF ty_tree.
TYPES tt_tree TYPE TABLE OF ty_tree.



DATA:gt_detail TYPE TABLE OF zafo_rule_detail   .
DATA:gt_code TYPE TABLE OF zafo_rule_code  .
DATA:gs_rule TYPE zafo_rule.
DATA:gt_rule TYPE TABLE OF zafo_rule.
DATA:gs_detail TYPE zafo_rule_detail.
DATA:g_splitter          TYPE REF TO cl_gui_splitter_container.
DATA:g_splitter_up          TYPE REF TO cl_gui_splitter_container.

DATA: g_tree_left  TYPE REF TO cl_gui_alv_tree,
      g_tree_right TYPE REF TO cl_gui_alv_tree.
DATA:g_falv_list TYPE REF TO zwft_falv.
DATA:g_falv TYPE REF TO zwft_falv.
DATA g_treev_l TYPE treev_hhdr.
DATA g_treev_r TYPE treev_hhdr.

DATA:g_at_node TYPE lvc_nkey.
DATA:gs_at TYPE  ty_tree .
DATA:g_at_has_child TYPE abap_bool .
DATA:gt_at TYPE TABLE OF ty_tree .
DATA:gt_tree_at TYPE TABLE OF ty_tree .

DATA:g_func_node TYPE lvc_nkey.
DATA:gs_func TYPE  ty_tree .
DATA:gs_func_parent TYPE  ty_tree .
DATA:g_func_has_child TYPE  abap_bool .
DATA:gt_func TYPE TABLE OF ty_tree .
DATA:gt_tree_func TYPE TABLE OF ty_tree .

DATA:g_action LIKE zafo_rule_code-action.

DATA ob_html TYPE REF TO zwft_html.
SELECT-OPTIONS: s_rule FOR zafo_rule-rule_name .
