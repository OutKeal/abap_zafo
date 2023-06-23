FUNCTION-POOL ZAFO_PRINT.                   "MESSAGE-ID ..

* INCLUDE LZAFO_PRINTD...                    " Local class definition


*&---------------------------------------------------------------------*
*& 包含               ZAFO_TOP_PRINT
*&---------------------------------------------------------------------*
DATA: func_module_name   TYPE rs38l_fnam,
      control_parameters TYPE ssfctrlop.

DATA:gs_print TYPE zafo_print.

DATA:gs_print_h TYPE zafo_print_h .
DATA:gt_print_h LIKE TABLE OF zafo_print_h WITH HEADER LINE.
DATA:gt_print_i LIKE TABLE OF zafo_print_i WITH HEADER LINE.
