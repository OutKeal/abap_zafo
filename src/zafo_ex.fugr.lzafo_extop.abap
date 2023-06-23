FUNCTION-POOL zafo_ex.                      "MESSAGE-ID ..

* INCLUDE LZAFO_EXD...                       " Local class definition


DATA:gt_fcat TYPE TABLE OF lvc_s_fcat WITH HEADER LINE  .
DATA gt_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
DATA batch_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.
DATA selected_item TYPE TABLE OF zafo_sitem WITH HEADER LINE.


DATA:g_model TYPE char1.
DATA:g_exec_env TYPE char5.
DATA:g_confirm TYPE char1.


DATA:BEGIN OF gt_model OCCURS 0,
       model TYPE tpl_id,
       name  TYPE name1,
     END OF gt_model.
