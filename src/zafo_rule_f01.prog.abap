*&---------------------------------------------------------------------*
*& 包含               ZAFO_RULE_F01
*&---------------------------------------------------------------------*

FORM frm_get_data.
  SELECT *
  INTO TABLE @gt_rule
  FROM zafo_rule
  WHERE rule_name IN @s_rule.
  IF sy-subrc NE 0.
    MESSAGE '无规则' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.

FORM frm_double_click  USING c_falv TYPE REF TO zwft_falv
      e_row  TYPE lvc_s_row
      e_column TYPE lvc_s_col
      es_row_no TYPE lvc_s_roid.
  CHECK c_falv = g_falv_list.
  READ TABLE gt_rule INTO gs_rule INDEX e_row-index.
  CHECK sy-subrc EQ 0.
  PERFORM frm_get_single_data.
  CALL SCREEN 100.
ENDFORM.

FORM frm_get_single_data.
  IF gs_rule-rule_name IS INITIAL.
    MESSAGE '请输入要维护的规则' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT  * FROM zafo_rule_detail
    INTO TABLE gt_detail
    WHERE rule_name = gs_rule-rule_name.

  SELECT  * FROM zafo_rule_code
    INTO TABLE gt_code
    WHERE rule_name = gs_rule-rule_name.

  PERFORM frm_get_at_tree.
  PERFORM frm_get_gt_func.
  PERFORM frm_set_icon.
ENDFORM.

FORM frm_get_gt_func.
  DATA:parent TYPE char40.
  DATA(lt_func) = zafo_run=>get_function_interface( gs_rule-bapi_name ).
  IF lt_func IS INITIAL .
    RETURN.
  ENDIF.
  LOOP AT lt_func INTO DATA(group_funcx)
        GROUP BY ( paramclass = group_funcx-paramclass )
        INTO DATA(group_func)  .
    CASE group_func-paramclass.
      WHEN 'I'.
        parent = 'INPUT'.
        APPEND VALUE #( parent = ''
        group = 'INPUT'
        fieldtext = '传入值'
        fieldname = 'INPUT'
        inttype = 'a'
        ) TO gt_func.
      WHEN 'E'.
        parent = 'OUTPUT'.
        APPEND VALUE #( parent = ''
        group = 'OUTPUT'
        fieldtext = '传出值'
        fieldname = 'OUTPUT'
        inttype = 'a'
        ) TO gt_func.
      WHEN 'C'.
        parent = 'CHANGE'.
        APPEND VALUE #( parent = ''
        group = 'CHANGE'
        fieldtext = '修改值'
        fieldname = 'CHANGE'
        inttype = 'a'
        ) TO gt_func.
      WHEN 'T'.
        parent = 'TABLE'.
        APPEND VALUE #( parent = ''
        group = 'TABLE'
        fieldtext = '修改值'
        fieldname = 'TABLE'
        inttype = 'a'
        ) TO gt_func.
    ENDCASE.

    LOOP AT GROUP group_func INTO DATA(ls_func).
      IF ls_func-paramclass CA 'IEC' AND ls_func-exid = cl_abap_typedescr=>typekind_struct1.
        PERFORM frm_get_fields_dfies USING ls_func-tabname
              parent
              ls_func-parameter
              ls_func-paramtext
              cl_abap_typedescr=>typekind_struct1
        CHANGING gt_func.
      ELSEIF ls_func-paramclass = 'T' AND ls_func-exid = cl_abap_typedescr=>typekind_struct1.
        PERFORM frm_get_fields_dfies USING ls_func-tabname
              parent
              ls_func-parameter
              ls_func-paramtext
              cl_abap_typedescr=>typekind_table
        CHANGING gt_func.
      ELSEIF ls_func-paramclass CA 'IEC'  AND ls_func-exid = cl_abap_typedescr=>typekind_table.
        SELECT SINGLE rowtype INTO @DATA(ls_rowtype) FROM dd40l WHERE typename = @ls_func-tabname.
        IF sy-subrc EQ 0.
          PERFORM frm_get_fields_dfies USING ls_rowtype
                parent
                ls_func-parameter
                ls_func-paramtext
                cl_abap_typedescr=>typekind_table
          CHANGING gt_func.
        ENDIF.
      ELSE.
        APPEND VALUE #( parent = parent
        group = ls_func-parameter
        fieldtext =  ls_func-paramtext
        fieldname = ls_func-parameter
        leng = ls_func-intlength
        inttype = ls_func-exid
        tabname = ls_func-tabname
        ) TO gt_func.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
FORM frm_get_at_tree.
  PERFORM frm_get_fields_dfies USING 'zafo_HEAD' '' 'HEAD' '抬头' cl_abap_typedescr=>typekind_struct1 CHANGING gt_at.
  PERFORM frm_get_fields_dfies USING 'zafo_ITEM' '' 'ITEM' '项目' cl_abap_typedescr=>typekind_table CHANGING gt_at.
  PERFORM frm_get_fields_dfies_ret  CHANGING gt_at.
ENDFORM.

FORM frm_get_fields_dfies_ret  CHANGING  ct_tree TYPE tt_tree.
  TYPES:
    BEGIN OF msty_bapi_ret ,
      docnr   TYPE docnr,
      cjahr   TYPE mjahr,
      objtype TYPE swo_objtyp,
      msgty   TYPE msgty,
      msgno   TYPE msgno,
      return  TYPE bapiret2_t,
    END OF msty_bapi_ret .

  APPEND VALUE #( parent = ''
  group = 'RET'
  fieldtext = '返回结构'
  fieldname = 'RET'
  inttype = cl_abap_typedescr=>typekind_struct1"u
  ) TO gt_at.

  APPEND VALUE #( parent = 'RET'
  group = 'DOCNR'
  fieldtext = '返回单号'
  fieldname = 'DOCNR'
  inttype = cl_abap_typedescr=>typekind_char"C
  leng = '10'
  ) TO gt_at.

  APPEND VALUE #( parent = 'RET'
  group = 'CJAHR'
  fieldtext = '返回年份'
  fieldname = 'CJAHR'
  inttype = cl_abap_typedescr=>typekind_char"C
  leng = '4'
  ) TO ct_tree.

  APPEND VALUE #( parent = 'RET'
  group = 'RETURN_TAB'
  fieldtext = '返回内表'
  fieldname = 'RETURN_TAB'
  inttype = cl_abap_typedescr=>typekind_char"C
  leng = '10'
  ) TO gt_at.

  PERFORM frm_get_fields_dfies USING 'BAPIRET2' 'RET' 'RETURN' 'BAPI返回值' cl_abap_typedescr=>typekind_table CHANGING ct_tree.
ENDFORM.

FORM frm_refresh_icon.
  PERFORM frm_set_icon.
  LOOP AT gt_func ASSIGNING FIELD-SYMBOL(<fs_func>) .
    g_tree_right->change_item( i_node_key = <fs_func>-node_key
    i_fieldname = 'ICON'
    i_data = <fs_func>-icon
    i_u_data = 'X').
  ENDLOOP.

  LOOP AT gt_at ASSIGNING FIELD-SYMBOL(<fs_at>) .
    g_tree_left->change_item( i_node_key = <fs_at>-node_key
    i_fieldname = 'ICON'
    i_data = <fs_at>-icon
    i_u_data = 'X').
  ENDLOOP.

  g_tree_left->frontend_update( ).
  g_tree_right->frontend_update( ).

ENDFORM.
FORM frm_set_icon.
  LOOP AT gt_func ASSIGNING FIELD-SYMBOL(<fs_func>) .
    <fs_func>-icon = icon_set_sum_undo.
    READ TABLE gt_detail INTO DATA(ls_detail) WITH KEY to_tabname = <fs_func>-parent to_fieldname = <fs_func>-fieldname.
    IF sy-subrc EQ 0.
      IF ls_detail-from_fieldalv IS INITIAL.
        <fs_func>-icon = icon_ben_offer_default.
      ELSE.
        <fs_func>-icon = icon_connect.
      ENDIF.
      READ TABLE gt_func ASSIGNING FIELD-SYMBOL(<fs_func_parent>) WITH KEY fieldname = <fs_func>-parent.
      IF sy-subrc EQ 0.
        <fs_func_parent>-icon = icon_led_green.
      ENDIF.
    ELSEIF <fs_func>-parent = 'INPUT'
      OR <fs_func>-parent = 'OUTPUT'
      OR <fs_func>-parent = 'CHANGE'.
      READ TABLE gt_detail INTO ls_detail WITH KEY to_tabname = <fs_func>-fieldname to_fieldname = ''.
      IF sy-subrc EQ 0.
        IF ls_detail-from_fieldalv IS INITIAL.
          <fs_func>-icon = icon_ben_offer_default.
        ELSE.
          <fs_func>-icon = icon_connect.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_at ASSIGNING FIELD-SYMBOL(<fs_at>).
    READ TABLE gt_detail TRANSPORTING NO FIELDS WITH KEY from_fieldalv = <fs_at>-parent from_fname = <fs_at>-fieldname.
    IF sy-subrc EQ 0.
      <fs_at>-icon = icon_connect.
    ELSE.
      <fs_at>-icon = icon_set_sum_undo.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_code INTO DATA(group_code)
        GROUP BY ( action = group_code-action
        to_tabname = group_code-to_tabname
        to_fieldname = group_code-to_fieldname )
  INTO DATA(ls_code).
    CASE ls_code-action.
      WHEN 'MOVE_VALUE'.
        READ TABLE gt_func ASSIGNING <fs_func> WITH KEY
        parent = ls_code-to_tabname
        fieldname = ls_code-to_fieldname.
        IF sy-subrc EQ 0.
          <fs_func>-icon = icon_businav_objects.
        ENDIF.
      WHEN 'BEFORE_POST'.
        READ TABLE gt_at ASSIGNING <fs_at> WITH KEY
        fieldname = 'HEAD'.
        IF sy-subrc EQ 0.
          <fs_at>-icon = icon_businav_objects.
        ENDIF.

        READ TABLE gt_at ASSIGNING <fs_at> WITH KEY
        fieldname = 'ITEM'.
        IF sy-subrc EQ 0.
          <fs_at>-icon = icon_businav_objects.
        ENDIF.
      WHEN 'AFTER_POST'.
        READ TABLE gt_at ASSIGNING <fs_at> WITH KEY
        fieldname = 'RET'.
        IF sy-subrc EQ 0.
          <fs_at>-icon = icon_businav_objects.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.


FORM frm_get_fields_dfies USING tabname parent group name typekind CHANGING  ct_tree TYPE tt_tree.
  DATA lv_tabname TYPE tabname.
  DATA:l_tree TYPE ty_tree.
  lv_tabname = |{ tabname CASE = UPPER }|.
  CONDENSE lv_tabname NO-GAPS.

  DATA:rt_dfies TYPE dfies_tab.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = lv_tabname
      langu     = sy-langu
    TABLES
      dfies_tab = rt_dfies.

  APPEND VALUE #( parent = parent
  group = group
  fieldname = group
  fieldtext = name
  inttype = typekind
  tabname = tabname ) TO ct_tree.

  LOOP AT rt_dfies INTO DATA(l_dfies).
    MOVE-CORRESPONDING l_dfies TO l_tree.
    l_tree-parent = group.
    l_tree-group = l_tree-fieldname.
    APPEND l_tree TO ct_tree.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_init_container
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_init_container .
  IF g_splitter IS  INITIAL.
    g_splitter = NEW cl_gui_splitter_container( parent = NEW cl_gui_docking_container( extension = '3000' )
    rows = 2
    columns = 1 ).
    g_splitter_up = NEW cl_gui_splitter_container( parent = g_splitter->get_container( row = 1 column = 1 )
    rows = 1
    columns = 2 ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_inti_treel
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_init_treel .
  IF g_tree_left IS INITIAL.
    g_tree_left = NEW cl_gui_alv_tree( parent = g_splitter_up->get_container( row = 1 column = 1 )
    node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
    item_selection              = 'X'
    no_html_header              = 'X'
    no_toolbar                  = '' ) .

    g_treev_l-heading = 'AT结构/AT字段'.
    g_treev_l-tooltip = 'AT信息'.
    g_treev_l-width = '15'.
    g_treev_l-width_pix = ''.
    DATA: p_relat_key TYPE lvc_nkey,
          p_node_key  TYPE lvc_nkey.

    DATA(it_fcat) =  zwft_common=>fcat_from_data( gt_tree_at ).
    READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>) WITH KEY fieldname = 'ICON'.
    IF sy-subrc EQ 0.
      <fs_fcat>-icon = 'X'.
    ENDIF.

    CALL METHOD g_tree_left->set_table_for_first_display
      EXPORTING
        i_structure_name    = 'TY_TREE'
        is_hierarchy_header = g_treev_l
      CHANGING
        it_fieldcatalog     = it_fcat
        it_outtab           = gt_tree_at. "table must be empty !

    LOOP  AT gt_at ASSIGNING FIELD-SYMBOL(<fs_at>).
      CLEAR p_relat_key.
      IF <fs_at>-parent IS NOT INITIAL.
        READ TABLE gt_at INTO DATA(ls_at) WITH KEY group = <fs_at>-parent .
        IF sy-subrc EQ 0.
          p_relat_key = ls_at-node_key.
        ENDIF.
      ENDIF.
      g_tree_left->add_node( EXPORTING i_relat_node_key = p_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = |{ <fs_at>-fieldname }|
        is_outtab_line   = <fs_at>
      IMPORTING e_new_node_key   = <fs_at>-node_key ).
    ENDLOOP.
  ENDIF.
  CALL METHOD g_tree_left->frontend_update.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.

FORM frm_init_treer .
  IF  g_tree_right IS INITIAL.
    g_tree_right = NEW cl_gui_alv_tree( parent = g_splitter_up->get_container( row = 1 column = 2 )
    node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
    item_selection              = 'X'
    no_html_header              = 'X'
    no_toolbar                  = '' ) .

    g_treev_r-heading = 'BAPI结构,BAPI字段'.
    g_treev_r-tooltip = 'BAPI信息'.
    g_treev_r-width = '30'.
    g_treev_r-width_pix = ''.
    DATA: p_relat_key TYPE lvc_nkey,
          p_node_key  TYPE lvc_nkey.

    DATA(it_fcat) =  zwft_common=>fcat_from_data( gt_tree_func ).
    READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>) WITH KEY fieldname = 'ICON'.
    IF sy-subrc EQ 0.
      <fs_fcat>-icon = 'X'.
    ENDIF.

    CALL METHOD g_tree_right->set_table_for_first_display
      EXPORTING
        i_structure_name    = 'TY_TREE'
        is_hierarchy_header = g_treev_r
      CHANGING
        it_fieldcatalog     = it_fcat
        it_outtab           = gt_tree_func. "table must be empty !

    LOOP  AT gt_func ASSIGNING FIELD-SYMBOL(<fs_func>).
      CLEAR p_relat_key.
      IF <fs_func>-parent IS NOT INITIAL.
        READ TABLE gt_func INTO DATA(ls_func) WITH KEY group = <fs_func>-parent .
        IF sy-subrc EQ 0.
          p_relat_key = ls_func-node_key.
        ENDIF.
      ENDIF.
      g_tree_right->add_node( EXPORTING i_relat_node_key = p_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = |{ <fs_func>-fieldname }|
        is_outtab_line   = <fs_func>
      IMPORTING e_new_node_key   = <fs_func>-node_key ).

    ENDLOOP.
    CALL METHOD g_tree_right->frontend_update.
  ENDIF.
ENDFORM.

FORM frm_init_falv.
  IF g_falv IS INITIAL.
    g_falv = zwft_falv=>create( EXPORTING i_parent = g_splitter->get_container( row = 2 column = 1 )
    CHANGING ct_table = gt_detail ).
    g_falv->display( ).
  ELSE.
    g_falv->soft_refresh( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_registered_events
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_TREE_left
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form frm_handle_double_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> NODE_KEY
*&---------------------------------------------------------------------*
FORM frm_get_tree_select.
  CLEAR: g_at_node,g_func_node,gs_at,gs_func,gs_func_parent,g_at_has_child,g_func_has_child.

  g_tree_left->get_selected_item( IMPORTING e_selected_node = g_at_node e_fieldname = DATA(l_fieldname_left) ).
  g_tree_right->get_selected_item( IMPORTING e_selected_node = g_func_node e_fieldname = DATA(l_fieldname_right) ).
  IF l_fieldname_left = '&Hierarchy' AND g_at_node IS NOT INITIAL.
    READ TABLE gt_at INTO gs_at WITH KEY node_key = g_at_node.
    g_tree_left->get_children( EXPORTING i_node_key = g_at_node IMPORTING et_children = DATA(children_left) ).
    IF children_left IS NOT INITIAL.
      g_at_has_child = abap_true.
    ENDIF.
  ENDIF.
  IF l_fieldname_right = '&Hierarchy' AND g_func_node IS NOT INITIAL.
    READ TABLE gt_func INTO gs_func WITH KEY node_key = g_func_node.
    g_tree_right->get_parent( EXPORTING i_node_key = g_func_node IMPORTING e_parent_node_key = DATA(node_parent)  ).
    READ TABLE gt_func INTO gs_func_parent WITH KEY node_key = node_parent.
    g_tree_right->get_children( EXPORTING i_node_key = g_func_node IMPORTING et_children = DATA(children_right) ).
    IF children_right IS NOT INITIAL.
      g_func_has_child = abap_true.
    ENDIF.
  ENDIF.
ENDFORM.

FORM frm_relate.
  PERFORM frm_get_tree_select.
  IF g_at_has_child = abap_true AND g_func_has_child = abap_true.
    MESSAGE '请只选择最底层' TYPE 'I' .
    RETURN.
  ENDIF.
  IF gs_at IS INITIAL OR gs_func IS INITIAL.
    MESSAGE '请勾选两边字段' TYPE 'I' .
    RETURN.
  ENDIF.

  IF gs_func_parent-fieldname = 'INPUT'
  OR gs_func_parent-fieldname = 'OUTPUT'
  OR gs_func_parent-fieldname = 'CHANGE'.
    READ TABLE gt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>) WITH KEY to_tabname = gs_func-fieldname to_fieldname = ''.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_detail ASSIGNING <fs_detail>.
      <fs_detail>-to_tabname = gs_func-fieldname.
    ENDIF.
  ELSE.
    READ TABLE gt_detail ASSIGNING <fs_detail> WITH KEY to_tabname = gs_func-parent to_fieldname = gs_func-fieldname.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_detail ASSIGNING <fs_detail>.
    ENDIF.
    <fs_detail>-to_tabname = gs_func-parent.
    <fs_detail>-to_fieldname = gs_func-fieldname.
  ENDIF.


  <fs_detail>-rule_name = gs_rule-rule_name.
  IF gs_func_parent-inttype = cl_abap_typedescr=>typekind_table.
    <fs_detail>-rule_type = 'I'.
  ELSEIF gs_func_parent-inttype = cl_abap_typedescr=>typekind_struct1.
    <fs_detail>-rule_type = 'H'.
  ELSE.
    <fs_detail>-rule_type = 'G'.
  ENDIF.

  IF gs_at-parent = 'RET'.
    <fs_detail>-rule_type = 'R'.
  ENDIF.

  gs_at-icon = icon_connect.
  gs_func-icon = icon_connect.
  <fs_detail>-from_fieldalv = gs_at-parent.
  <fs_detail>-from_fname = gs_at-fieldname.

  g_tree_left->change_item( i_node_key = g_at_node
  i_fieldname = 'ICON'
  i_data = gs_at-icon
  i_u_data = 'X').

  g_tree_right->change_item( i_node_key = g_func_node
  i_fieldname = 'ICON'
  i_data = gs_func-icon
  i_u_data = 'X').

  g_tree_left->frontend_update( ).
  g_tree_right->frontend_update( ).
  g_tree_left->unselect_all( ).
  g_tree_right->unselect_all( ).
  g_falv->soft_refresh( ).

ENDFORM.

FORM frm_default.
  DATA:l_default TYPE char30.

  PERFORM frm_get_tree_select.
  IF g_func_has_child = abap_true.
    MESSAGE '请只选择最底层' TYPE 'I' .
    RETURN.
  ENDIF.
  IF  gs_func IS INITIAL.
    MESSAGE '请勾选右侧字段' TYPE 'I' .
    RETURN.
  ENDIF.

  PERFORM frm_get_input USING  '请填入默认值' CHANGING l_default .


  IF gs_func_parent-fieldname = 'INPUT'
  OR gs_func_parent-fieldname = 'OUTPUT'
  OR gs_func_parent-fieldname = 'CHANGE'.
    READ TABLE gt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>) WITH KEY to_tabname = gs_func-fieldname to_fieldname = ''.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_detail ASSIGNING <fs_detail>.
    ENDIF.
    <fs_detail>-to_tabname = gs_func-fieldname.
  ELSE.
    READ TABLE gt_detail ASSIGNING <fs_detail> WITH KEY to_tabname = gs_func-parent to_fieldname = gs_func-fieldname.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_detail ASSIGNING <fs_detail>.
    ENDIF.
    <fs_detail>-to_tabname = gs_func-parent.
    <fs_detail>-to_fieldname = gs_func-fieldname.
  ENDIF.
  <fs_detail>-rule_name = gs_rule-rule_name.
  IF gs_func_parent-inttype = cl_abap_typedescr=>typekind_table.
    <fs_detail>-rule_type = 'I'.
  ELSEIF gs_func_parent-inttype = cl_abap_typedescr=>typekind_struct1.
    <fs_detail>-rule_type = 'H'.
  ELSE.
    <fs_detail>-rule_type = 'G'.
  ENDIF.

  <fs_detail>-default_value = l_default.

  gs_func-icon = icon_ben_offer_default.


  g_tree_right->change_item( i_node_key = g_func_node
  i_fieldname = 'ICON'
  i_data = gs_func-icon
  i_u_data = 'X').

  g_tree_right->frontend_update( ).
  g_tree_right->unselect_all( ).
  g_falv->soft_refresh( ).


ENDFORM.

FORM frm_last.
  DATA:l_last TYPE char30.

  PERFORM frm_get_tree_select.
  IF g_func_has_child = abap_true.
    MESSAGE '请只选择最底层' TYPE 'I' .
    RETURN.
  ENDIF.
  IF  gs_func IS INITIAL.
    MESSAGE '请勾选右侧字段' TYPE 'I' .
    RETURN.
  ENDIF.

  PERFORM frm_get_input USING  '请填入参考步骤值,默认为上一步' CHANGING l_last .

  IF gs_func_parent-fieldname = 'INPUT'
  OR gs_func_parent-fieldname = 'OUTPUT'
  OR gs_func_parent-fieldname = 'CHANGE'.
    READ TABLE gt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>) WITH KEY to_tabname = gs_func-fieldname to_fieldname = ''.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_detail ASSIGNING <fs_detail>.
    ENDIF.
    <fs_detail>-to_tabname = gs_func-fieldname.
  ELSE.
    READ TABLE gt_detail ASSIGNING <fs_detail> WITH KEY to_tabname = gs_func-parent to_fieldname = gs_func-fieldname.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO gt_detail ASSIGNING <fs_detail>.
    ENDIF.
    <fs_detail>-to_tabname = gs_func-parent.
    <fs_detail>-to_fieldname = gs_func-fieldname.
  ENDIF.

  <fs_detail>-rule_name = gs_rule-rule_name.
  IF gs_func_parent-inttype = cl_abap_typedescr=>typekind_table.
    <fs_detail>-rule_type = 'I'.
  ELSEIF gs_func_parent-inttype = cl_abap_typedescr=>typekind_struct1.
    <fs_detail>-rule_type = 'H'.
  ELSE.
    <fs_detail>-rule_type = 'G'.
  ENDIF.

  <fs_detail>-from_fname = l_last.
  <fs_detail>-from_fieldalv = 'LAST'.

  gs_func-icon = icon_ben_offer_default.

  g_tree_right->change_item( i_node_key = g_func_node
  i_fieldname = 'ICON'
  i_data = gs_func-icon
  i_u_data = 'X').

  g_tree_right->frontend_update( ).
  g_tree_right->unselect_all( ).
  g_falv->soft_refresh( ).


ENDFORM.

FORM frm_get_input USING text CHANGING l_default.

  DATA:lt_flds TYPE TABLE OF sval.
  DATA:p_gv_ret_code TYPE char1.

  APPEND VALUE #( tabname = 'MKPF'
  fieldname = 'BKTXT'
  value = ''
  fieldtext = '默认值'
  ) TO lt_flds.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = text
    IMPORTING
      returncode      = p_gv_ret_code
    TABLES
      fields          = lt_flds
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF p_gv_ret_code = 'A'.
    MESSAGE '操作已取消' TYPE 'S'.
    RETURN.
  ENDIF.

  l_default = lt_flds[ fieldname = 'BKTXT' ]-value.

ENDFORM.

FORM frm_routine.
  PERFORM frm_get_tree_select.
  CLEAR g_code.
  IF gs_at-fieldname = 'HEAD' OR gs_at-fieldname = 'ITEM'.
    g_action = 'BEFORE_POST'.
    PERFORM frm_fill_code USING g_action '' ''.
  ELSEIF gs_at-fieldname = 'RET'.
    g_action = 'AFTER_POST'.
    PERFORM frm_fill_code USING g_action '' ''.
  ELSEIF gs_at IS NOT INITIAL
    AND gs_func IS NOT INITIAL
    AND g_at_has_child <> abap_true
    AND g_func_has_child <> abap_true .
    g_action = 'MOVE_VALUE'.
    PERFORM frm_fill_code USING g_action gs_func-parent gs_func-fieldname.
    IF g_code IS INITIAL.
      APPEND |C_VALUE = I_VALUE.| TO g_code.
    ENDIF.
  ELSE.
    MESSAGE '请选择值两侧值明细' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  PERFORM frm_init_abap_edit.
  CALL SCREEN 200.
  PERFORM frm_refresh_icon.
ENDFORM.
FORM frm_fill_code  USING action tabname fieldname .
  LOOP AT gt_code INTO DATA(ls_code) WHERE action = action AND to_tabname = tabname AND to_fieldname = fieldname.
    APPEND INITIAL LINE TO g_code ASSIGNING FIELD-SYMBOL(<f_code>).
    <f_code> = ls_code-code.
  ENDLOOP.
ENDFORM.

FORM frm_delete.
  g_falv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).
  IF lt_rows IS INITIAL.
    MESSAGE '请选择配置行明细' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SORT lt_rows BY index DESCENDING.
  LOOP AT lt_rows INTO DATA(ls_row).
    DELETE gt_detail INDEX ls_row-index.
  ENDLOOP.
  IF sy-subrc EQ 0.
    g_falv->soft_refresh( ).
  ENDIF.

ENDFORM.

FORM frm_save.
  PERFORM frm_set_return.
  SORT gt_detail BY rule_type to_tabname.
  LOOP AT gt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>).
    <fs_detail>-dzaehk = sy-tabix.
  ENDLOOP.

  DELETE FROM zafo_rule_detail WHERE rule_name = gs_rule-rule_name.
*  DELETE FROM zafo_rule_code WHERE rule_name = gs_rule-rule_name.
  COMMIT WORK AND WAIT.
  MODIFY zafo_rule_detail FROM TABLE gt_detail.
*  MODIFY zafo_rule_code FROM TABLE gt_code.
  g_falv->soft_refresh( ).
ENDFORM.

FORM frm_set_return.
  LOOP AT gt_func INTO DATA(ls_func) WHERE ( tabname = 'BAPIRET1'
        OR tabname = 'BAPIRET2'
        OR tabname = 'BAPI_CORU_RETURN'
        OR tabname = 'BAPIRETURN'
        OR tabname = 'BAPIVBRKERRORS' )
        AND ( inttype = 'h' OR inttype = 'u' ).

    READ TABLE gt_detail TRANSPORTING NO FIELDS WITH KEY to_tabname = ls_func-fieldname.
    IF sy-subrc NE 0.
      APPEND VALUE #( mandt = sy-mandt
      rule_name = gs_rule-rule_name
      to_tabname = ls_func-fieldname
      from_fieldalv = 'RET'
      rule_type = 'R'
      ) TO gt_detail.
    ENDIF.

  ENDLOOP.

ENDFORM.
