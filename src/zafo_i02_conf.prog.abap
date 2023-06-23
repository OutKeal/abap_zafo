*&---------------------------------------------------------------------*
*& 包含               ZAFO_I02_CONF
*&---------------------------------------------------------------------*

FORM frm_get_config.
  DATA:lt_screen_bty TYPE TABLE OF zafo_screen_bty WITH HEADER LINE.


  READ TABLE gt_bustyp INTO gs_bustyp WITH KEY bustyp = g_bustyp.
  IF sy-subrc NE 0.
    SELECT SINGLE * INTO gs_bustyp
      FROM zafo_bustype WHERE bustyp = g_bustyp.
    IF sy-subrc EQ 0.
      APPEND gs_bustyp TO gt_bustyp.
      g_object = gs_bustyp-object.
      g_bustyp_ref = gs_bustyp-bustyp_ref.

      REFRESH gt_reason.
      SELECT * INTO TABLE gt_reason
        FROM zafo_reason WHERE bustyp = g_bustyp.
    ELSE.
      PERFORM frm_add_msg USING 'E' 'ZAFO' '015' '' '' '' ''."获取业务类型配置失败
      RETURN.
    ENDIF.
  ENDIF.

  IF g_bustyp_ref IS NOT INITIAL.
    READ TABLE gt_bustyp INTO gs_bustyp_ref WITH KEY bustyp = g_bustyp_ref.
    IF sy-subrc NE 0.
      SELECT SINGLE * INTO gs_bustyp_ref
        FROM zafo_bustype WHERE bustyp = g_bustyp_ref.
      IF sy-subrc EQ 0.
        APPEND gs_bustyp_ref TO gt_bustyp.
        g_object_ref = gs_bustyp_ref-object.

        SELECT * APPENDING TABLE gt_reason
          FROM zafo_reason WHERE bustyp = g_bustyp_ref.
      ELSE.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '016' '' '' '' ''."获取业务类型配置失败
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  IF g_object IS NOT INITIAL.
    READ TABLE gt_object INTO gs_object WITH KEY object = g_object.
    IF sy-subrc NE 0.
      SELECT SINGLE * INTO gs_object FROM zafo_object WHERE object = g_object.
      IF sy-subrc EQ 0.
        APPEND gs_object TO gt_object.

        SELECT * FROM zafo_screen APPENDING TABLE gt_screen
          WHERE object = g_object.

        " 业务屏幕字段覆盖对象屏幕字段
        SELECT * FROM zafo_screen_bty INTO TABLE lt_screen_bty
          WHERE bustyp = g_bustyp.

        LOOP AT lt_screen_bty.
          READ TABLE gt_screen ASSIGNING FIELD-SYMBOL(<fs_screen>)
                                          WITH KEY  fieldalv = lt_screen_bty-fieldalv
                                                    fieldname = lt_screen_bty-fieldname.
          IF sy-subrc EQ 0 AND <fs_screen> IS ASSIGNED.
            <fs_screen>-coltext       =   lt_screen_bty-coltext      .
            <fs_screen>-fedit         =   lt_screen_bty-fedit        .
            <fs_screen>-requi         =   lt_screen_bty-requi        .
            <fs_screen>-hidde         =   lt_screen_bty-hidde        .
            <fs_screen>-emphasize     =   lt_screen_bty-emphasize    .
            <fs_screen>-split_col     =   lt_screen_bty-split_col    .
            <fs_screen>-import_index  =   lt_screen_bty-import_index .
            <fs_screen>-rollname      =   lt_screen_bty-rollname     .
            <fs_screen>-zdefault      =   lt_screen_bty-zdefault     .
          ELSE.
            SORT gt_screen BY dzaehk DESCENDING.
            READ TABLE gt_screen WITH KEY fieldalv = lt_screen_bty-fieldalv.
            gt_screen-dzaehk = gt_screen-dzaehk + 10.
            gt_screen-fieldname     =   lt_screen_bty-fieldname    .
            gt_screen-coltext       =   lt_screen_bty-coltext      .
            gt_screen-fedit         =   lt_screen_bty-fedit        .
            gt_screen-requi         =   lt_screen_bty-requi        .
            gt_screen-hidde         =   lt_screen_bty-hidde        .
            gt_screen-emphasize     =   lt_screen_bty-emphasize    .
            gt_screen-split_col     =   lt_screen_bty-split_col    .
            gt_screen-import_index  =   lt_screen_bty-import_index .
            gt_screen-rollname      =   lt_screen_bty-rollname     .
            gt_screen-zdefault      =   lt_screen_bty-zdefault     .
            APPEND gt_screen.
          ENDIF.
        ENDLOOP.

        SORT gt_screen.

      ELSE.

        PERFORM frm_add_msg USING 'E' 'ZAFO' '017' '' '' '' ''."获取对象配置失败
      ENDIF.
    ENDIF.
  ENDIF.

  IF g_object_ref IS NOT INITIAL.
    READ TABLE gt_object INTO gs_object_ref WITH KEY object = g_object_ref.

    IF sy-subrc NE 0.
      SELECT SINGLE * INTO gs_object_ref
        FROM zafo_object WHERE object = g_object_ref.
      IF sy-subrc EQ 0.
        APPEND gs_object_ref TO gt_object.
        SELECT * FROM zafo_screen APPENDING TABLE gt_screen
          WHERE object = g_object_ref.

        " 业务屏幕字段覆盖对象屏幕字段
        SELECT * FROM zafo_screen_bty INTO TABLE lt_screen_bty
          WHERE bustyp = g_bustyp_ref.

        LOOP AT lt_screen_bty.
          READ TABLE gt_screen ASSIGNING <fs_screen>
                                          WITH KEY  fieldalv = lt_screen_bty-fieldalv
                                                    fieldname = lt_screen_bty-fieldname.
          IF sy-subrc EQ 0 AND <fs_screen> IS ASSIGNED.
            <fs_screen>-coltext       =   lt_screen_bty-coltext      .
            <fs_screen>-fedit         =   lt_screen_bty-fedit        .
            <fs_screen>-requi         =   lt_screen_bty-requi        .
            <fs_screen>-hidde         =   lt_screen_bty-hidde        .
            <fs_screen>-emphasize     =   lt_screen_bty-emphasize    .
            <fs_screen>-split_col     =   lt_screen_bty-split_col    .
            <fs_screen>-import_index  =   lt_screen_bty-import_index .
            <fs_screen>-rollname      =   lt_screen_bty-rollname     .
            <fs_screen>-zdefault      =   lt_screen_bty-zdefault     .
          ELSE.
            SORT gt_screen BY dzaehk DESCENDING.
            READ TABLE gt_screen WITH KEY fieldalv = lt_screen_bty-fieldalv.
            gt_screen-dzaehk = gt_screen-dzaehk + 10.
            gt_screen-fieldname     =   lt_screen_bty-fieldname    .
            gt_screen-coltext       =   lt_screen_bty-coltext      .
            gt_screen-fedit         =   lt_screen_bty-fedit        .
            gt_screen-requi         =   lt_screen_bty-requi        .
            gt_screen-hidde         =   lt_screen_bty-hidde        .
            gt_screen-emphasize     =   lt_screen_bty-emphasize    .
            gt_screen-split_col     =   lt_screen_bty-split_col    .
            gt_screen-import_index  =   lt_screen_bty-import_index .
            gt_screen-rollname      =   lt_screen_bty-rollname     .
            gt_screen-zdefault      =   lt_screen_bty-zdefault     .
            APPEND gt_screen.
          ENDIF.
        ENDLOOP.

        SORT gt_screen.
      ELSE.
        PERFORM frm_add_msg USING 'E' 'ZAFO' '018' '' '' '' ''."获取参考对象配置失败
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


FORM frm_set_text.
  TYPES: BEGIN OF ty_abap_componentdescr,   "用于生成动态内表
           name       TYPE string,
           type       TYPE REF TO cl_abap_datadescr,
           as_include TYPE abap_bool,
           suffix     TYPE string,
         END OF ty_abap_componentdescr.
  DATA: lt_abap_componentdescr TYPE STANDARD TABLE OF ty_abap_componentdescr WITH KEY name,
        ls_abap_componentdescr TYPE ty_abap_componentdescr.

  DATA: lcr_ref_line TYPE REF TO cl_abap_structdescr.
  DATA: lr_ref_line TYPE REF TO data.

  DATA: ls_fieldname TYPE fieldname.

  FIELD-SYMBOLS:<fs_value> TYPE any.
  FIELD-SYMBOLS:<fs_screen> TYPE zafo_screen.

  SELECT * INTO TABLE gt_screen_text
    FROM zafo_screen_text
    WHERE langu = sy-langu
    AND fieldalv <> ''
    AND fieldname <> ''
    AND ( object = g_object
    OR object = g_object_ref
    OR object = '' ).

  SORT gt_screen_text BY fieldname object DESCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_screen_text COMPARING fieldname.

  SORT gt_screen[] BY dzaehk.



  LOOP AT gt_screen ASSIGNING <fs_screen> WHERE object = g_object AND fieldalv = 'HEAD' .
    READ TABLE lt_abap_componentdescr TRANSPORTING NO FIELDS
                                  WITH KEY name = gt_screen_text-fieldname.
    IF sy-subrc NE 0.
      ls_abap_componentdescr-name = <fs_screen>-fieldname.  "用于生成动态内表
      ls_abap_componentdescr-type ?= cl_abap_typedescr=>describe_by_name( 'CHAR40' ).
      APPEND ls_abap_componentdescr TO lt_abap_componentdescr.
      CLEAR ls_abap_componentdescr.
    ENDIF.
  ENDLOOP.


  LOOP AT gt_screen_text.
    READ TABLE gt_screen ASSIGNING <fs_screen>
                         WITH KEY object = gt_screen_text-object
                                  fieldalv = gt_screen_text-fieldalv
                                  fieldname = gt_screen_text-fieldname.
    IF sy-subrc EQ 0.
      <fs_screen>-coltext = gt_screen_text-coltext.
    ENDIF.

    IF gt_screen_text-object = '' OR gt_screen_text-object = g_object.
      READ TABLE lt_abap_componentdescr TRANSPORTING NO FIELDS
                                        WITH KEY name = gt_screen_text-fieldname.
      IF sy-subrc NE 0.
        ls_abap_componentdescr-name = gt_screen_text-fieldname.  "用于生成动态内表
        ls_abap_componentdescr-type ?= cl_abap_typedescr=>describe_by_name( 'CHAR40' ).
        APPEND ls_abap_componentdescr TO lt_abap_componentdescr.
        CLEAR ls_abap_componentdescr.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  sort lt_abap_componentdescr by name.
*
*  delete ADJACENT DUPLICATES FROM lt_abap_componentdescr COMPARING name.

  lcr_ref_line ?= cl_abap_structdescr=>create( p_components = lt_abap_componentdescr ).
  CREATE DATA lr_ref_line TYPE HANDLE lcr_ref_line.
  ASSIGN lr_ref_line->* TO <gs_text>.

  LOOP AT gt_screen WHERE object = g_object AND fieldalv = 'HEAD' .
    ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_text> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = gt_screen-coltext.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_screen_text WHERE fieldalv = 'HEAD' AND ( object = '' OR object = g_object ).
    ASSIGN COMPONENT gt_screen_text-fieldname OF STRUCTURE <gs_text> TO <fs_value>.
    IF sy-subrc EQ 0.
      IF <fs_value> IS INITIAL.
        <fs_value> = gt_screen_text-coltext.
      ENDIF.
    ENDIF.
  ENDLOOP.

*OBJECT
*DZAEHK
*FIELDALV
*FIELDNAME
*COLTEXT
*FEDIT
*REQUI
*HIDDE
*EMPHASIZE
*SPLIT_COL
*IMPORT_INDEX
*ROLLNAME
*ZDEFAULT

ENDFORM.


FORM init_head.
  gs_head-object = g_object.
  gs_head-bustyp = g_bustyp.
  gs_head-execute_type = gs_bustyp-execute_type.

  IF gs_head-bldat IS INITIAL .
    gs_head-bldat = sy-datum.
  ENDIF.

  IF gs_head-budat IS INITIAL .
    gs_head-budat = sy-datum.
  ENDIF.

  IF gs_head-bukrs IS INITIAL.
    gs_head-bukrs = g_werks.
    SELECT SINGLE butxt FROM t001
      INTO gs_head-bukrs_name
      WHERE bukrs = gs_head-bukrs.
  ENDIF.

  IF gs_head-ekorg IS INITIAL.
    gs_head-ekorg = g_werks.
  ENDIF.

  IF gs_head-werks IS INITIAL.
    gs_head-werks = g_werks.
  ENDIF.

  LOOP AT gt_screen WHERE object = g_object AND fieldalv = 'HEAD' AND zdefault <> ''.
    ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gs_head TO FIELD-SYMBOL(<fs_value>).
    IF sy-subrc EQ 0.
      IF <fs_value> IS INITIAL.
        CASE gt_screen-zdefault.
          WHEN 'UNAME'.
            <fs_value> = sy-uname.
          WHEN OTHERS.
            <fs_value> = gt_screen-zdefault.
        ENDCASE.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF gs_object-app_object IS NOT INITIAL." 设置审批初始状态
    gs_head-app_status = 'A'.
  ENDIF.

  IF gs_bustyp-qc_claim IS NOT INITIAL." 设置质检初始状态
    gs_head-qc_status = gs_bustyp-qc_claim.
  ENDIF.

  IF gs_head-status IS INITIAL.
    PERFORM frm_set_status USING ''.
  ENDIF.

ENDFORM.


FORM init_head_text.

  IF gs_head-werks IS NOT INITIAL.
    SELECT SINGLE name1
      INTO gs_head-werks_name
      FROM t001w
      WHERE werks EQ gs_head-werks.
    IF sy-subrc NE 0.
      gs_head-werks = ''.
      gs_head-werks_name = ''.
      MESSAGE s042."'工厂不存在' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF gs_head-lgort IS NOT INITIAL.
    SELECT SINGLE lgobe
      INTO gs_head-lgort_name
      FROM t001l
      WHERE werks EQ gs_head-werks
      AND lgort = gs_head-lgort .
    IF sy-subrc NE 0.
      gs_head-lgort = ''.
      gs_head-lgort_name = ''.
      MESSAGE s046."'库存地点不存在' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF gs_head-umwrk IS NOT INITIAL.
    SELECT SINGLE name1
      INTO gs_head-umwrk_name
      FROM t001w
      WHERE werks EQ gs_head-umwrk.
    IF sy-subrc NE 0.
      gs_head-umwrk = ''.
      gs_head-umwrk_name = ''.
      MESSAGE s045."'工厂不存在' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF gs_head-umlgo IS NOT INITIAL.
    IF gs_head-umwrk IS INITIAL.
      SELECT SINGLE lgobe
        INTO gs_head-umlgo_name
        FROM t001l
        WHERE werks EQ gs_head-werks
        AND lgort =  gs_head-umlgo .
      IF sy-subrc NE 0.
        gs_head-umlgo = ''.
        gs_head-umlgo_name = ''.
        MESSAGE s047." '收货库存地点不存在' TYPE 'S'.
      ENDIF.
    ELSE.

      SELECT SINGLE lgobe
        INTO gs_head-umlgo_name
        FROM t001l
        WHERE werks EQ gs_head-umwrk
        AND lgort =  gs_head-umlgo .
      IF sy-subrc NE 0.
        gs_head-umlgo = ''.
        gs_head-umlgo_name = ''.
        MESSAGE s047."'收货库存地点不存在' TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gs_head-kunnr IS NOT INITIAL AND gs_head-kunnr_name IS INITIAL.
    SELECT SINGLE zname1
      INTO gs_head-kunnr_name
      FROM zscmt0010
      WHERE partner EQ gs_head-kunnr.
    IF sy-subrc NE 0.
      gs_head-kunnr = ''.
      gs_head-kunnr_name = ''.
      MESSAGE s135."'客户不存在' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF gs_head-kunnr IS NOT INITIAL.
    READ TABLE gt_item INTO gs_item INDEX 1.
    IF sy-subrc EQ 0 AND gs_item-waers IS NOT INITIAL.
      gs_head-waers = gs_item-waers.
    ELSE.
      gs_head-waers = 'CNY'.
    ENDIF.
    SELECT SINGLE land1 FROM kna1 INTO gs_head-land1
      WHERE kunnr = gs_head-kunnr.
  ENDIF.

  IF gs_head-lifnr IS NOT INITIAL AND gs_head-lifnr_name IS INITIAL.
    SELECT SINGLE zname1
      INTO gs_head-lifnr_name
      FROM zscmt0010
      WHERE partner EQ gs_head-lifnr.
    IF sy-subrc NE 0.
      gs_head-lifnr = ''.
      gs_head-lifnr_name = ''.
      MESSAGE s048."'供应商不存在' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF gs_head-lifnr IS NOT INITIAL.
    READ TABLE gt_item INTO gs_item INDEX 1.
    IF sy-subrc EQ 0 AND gs_item-waers IS NOT INITIAL.
      gs_head-waers = gs_item-waers.
    ELSE.
      SELECT SINGLE waers FROM lfm1
        INTO gs_head-waers
        WHERE lifnr = gs_head-lifnr
        AND ekorg = gs_head-ekorg.
    ENDIF.

    SELECT SINGLE land1 FROM lfa1
      INTO gs_head-land1
      WHERE lifnr = gs_head-lifnr.
  ENDIF.

  IF gs_head-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt
      INTO gs_head-bukrs_name
      FROM t001 WHERE bukrs EQ gs_head-bukrs  .
    IF sy-subrc NE 0.
      CLEAR gs_head-bukrs.
      CLEAR gs_head-bukrs_name.
      MESSAGE s049." '公司代码填写错误' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF gs_head-kostl IS NOT INITIAL.
    SELECT SINGLE ktext
      INTO gs_head-kostl_name
      FROM vfco_csks_shv
      WHERE kostl EQ gs_head-kostl
      AND bukrs EQ gs_head-bukrs.
    IF sy-subrc NE 0.
      CLEAR gs_head-kostl_name .
      CLEAR gs_head-kostl .
      MESSAGE s050."'请选择对应公司的成本中心' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF gs_head-zstaff IS NOT INITIAL.
    SELECT SINGLE name_textc
      INTO gs_head-zstaff_name
      FROM user_addr
      WHERE bname EQ gs_head-zstaff .
    IF sy-subrc NE 0.
      CLEAR gs_head-zstaff .
      CLEAR gs_head-zstaff_name .
      MESSAGE s051."'仓管员填写错误' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.


FORM init_item.

  CHECK gt_item[] IS NOT INITIAL.

  LOOP AT gt_item ASSIGNING <gs_item>.
    IF gs_head-qc_status IS NOT INITIAL.
      <gs_item>-qc_status = gs_head-qc_status.
    ENDIF.
  ENDLOOP.

  PERFORM frm_zz_rebuild_item.

ENDFORM.


FORM init_item_zdefault.
  CHECK gt_item[] IS NOT INITIAL.

  LOOP AT gt_screen WHERE object = g_object AND fieldalv = 'ITEM' AND zdefault <> ''.
    LOOP AT gt_item ASSIGNING <gs_item>.
      ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE <gs_item> TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc EQ 0.
        IF <fs_value> IS INITIAL.
          <fs_value> = gt_screen-zdefault.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.


FORM init_initial_item.
  CHECK gt_item[] IS INITIAL.
  CLEAR gs_item.

  APPEND gs_item TO gt_item_dis.

  DO 10 TIMES.

    PERFORM init_item_line USING sy-index CHANGING gs_item.

    APPEND gs_item TO gt_item.
  ENDDO.

ENDFORM.


FORM init_item_line USING afonr CHANGING cs_item STRUCTURE zafo_sitem.
  gs_item-afonr = afonr.
  gs_item-werks = g_werks.
  gs_item-eeind = gs_head-eeind.

  LOOP AT gt_screen WHERE object = g_object AND fieldalv = 'ITEM' AND zdefault <> ''.
    ASSIGN COMPONENT gt_screen-fieldname OF STRUCTURE gs_item TO FIELD-SYMBOL(<fs_value>).
    IF sy-subrc EQ 0.
      IF <fs_value> IS INITIAL.
        <fs_value> = gt_screen-zdefault.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM frm_set_icon USING gs_head-status CHANGING cs_item-icon cs_item-text.

ENDFORM.
