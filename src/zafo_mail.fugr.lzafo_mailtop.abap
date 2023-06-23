FUNCTION-POOL zafo_mail.                    "MESSAGE-ID ..

* INCLUDE LZAFO_MAILD...                     " Local class definition
INCLUDE <cntn01>.

DATA gv_method1      LIKE sy-ucomm.
DATA gs_user         LIKE soudnamei1.
DATA gs_user_data    LIKE soudatai1.
DATA gv_owner        LIKE soud-usrnam.
DATA gt_receipients  LIKE soos1 OCCURS 0 WITH HEADER LINE.
DATA gs_document     LIKE sood4 .
DATA gs_header2      LIKE sood2.
DATA gs_folmam       LIKE sofm2.
DATA gt_objcnt       LIKE soli OCCURS 0 WITH HEADER LINE.
DATA gt_objhead      LIKE soli OCCURS 0 WITH HEADER LINE.
DATA gt_objpara      LIKE selc OCCURS 0 WITH HEADER LINE.
DATA gt_objparb      LIKE soop1 OCCURS 0 WITH HEADER LINE.
DATA gt_attachments  LIKE sood5 OCCURS 0 WITH HEADER LINE.
DATA gt_references   LIKE soxrl OCCURS 0 WITH HEADER LINE.
DATA gs_reciver      LIKE soos6 .
DATA gv_authority    LIKE sofa-usracc.
DATA gs_ref_document LIKE sood4.
DATA gs_new_parent   LIKE soodk.
DATA: BEGIN OF gt_files OCCURS 10 ,
        text(4096) TYPE c,
      END OF gt_files.

DATA : gv_fold_number(12) TYPE c,
       gv_fold_yr(2)      TYPE c,
       gv_fold_type(3)    TYPE c.

DATA: gs_folder_id LIKE soodk,
      gs_order_id  LIKE soodk.
DATA: gv_mail_title(50).

DATA:g_error TYPE char1.
DATA:ot_return LIKE TABLE OF bapiret2 WITH HEADER LINE..
