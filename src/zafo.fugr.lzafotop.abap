FUNCTION-POOL zafo MESSAGE-ID zafo.                  "MESSAGE-ID ..

* INCLUDE LZAFO_COMMOND...                   " Local class definition
DATA:g_splitter_100          TYPE REF TO cl_gui_splitter_container.
DATA:g_splitter_200          TYPE REF TO cl_gui_splitter_container.
DATA:g_splitter_300          TYPE REF TO cl_gui_splitter_container.
DATA:g_container_200 TYPE REF TO cl_gui_custom_container.
DATA text_container TYPE REF TO cl_gui_custom_container.

FIELD-SYMBOLS <io_class> TYPE REF TO  zafo_class .

TYPES: t_class TYPE REF TO zafo_class.
TYPES: tt_class TYPE SORTED TABLE OF t_class WITH UNIQUE KEY table_line.
DATA: tab_class TYPE tt_class.

FIELD-SYMBOLS <gs_text> TYPE any.

CONTROLS: tag0200 TYPE TABSTRIP.
CONSTANTS: BEGIN OF c_tag0200,
             tab1 LIKE sy-ucomm VALUE 'TAG0200_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAG0200_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAG0200_FC3',
           END OF c_tag0200.
DATA: BEGIN OF g_tag0200,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'SAPLZAFO',
        pressed_tab LIKE sy-ucomm VALUE c_tag0200-tab1,
      END OF g_tag0200.

*DATA:g_ean TYPE char40.

DATA:gt_t024 TYPE TABLE OF t024.
DATA:gt_tvtwt TYPE TABLE OF tvtwt.
DATA zafo_shead TYPE zafo_shead."f4参考

DATA cursor TYPE fieldname.
DATA cursor_400 TYPE fieldname.
