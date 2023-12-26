*&---------------------------------------------------------------------*
*& 包含               ZAFO_F01
*&---------------------------------------------------------------------*

 FORM frm_set_sel_screen .

   CHECK p_typ IS NOT INITIAL .
   DATA:l_field TYPE char1.

   gs_bustyp = zafo_basic=>get_bustyp( p_typ ).

   CHECK gs_bustyp IS NOT INITIAL.

   gv_create_model = gs_bustyp-busref_control.

   IF gv_create_model = 1 AND p_ref = 'X'..
     p_cre = 'X'.
     p_ref = ''.
   ELSEIF gv_create_model = 2 AND p_cre = 'X'..
     p_cre = ''.
     p_ref = 'X'.
   ENDIF.
   DATA(lt_sel_screen) = zafo_basic=>get_bustyp_sel_screen( p_typ ).

   PERFORM frm_set_werks_flag.
   PERFORM frm_set_action.

   CHECK p_typ IS NOT INITIAL.

   LOOP AT SCREEN.
     LOOP AT lt_sel_screen INTO DATA(ls_sel_screen) WHERE fieldname IS NOT INITIAL AND action = ''  .
       IF screen-name CP '*' && ls_sel_screen-fieldname && '*'.
         IF ls_sel_screen-fieldname = 'P_CRE'.
           p_cre = ''.
           IF p_mod = ''.
             p_dis = 'X'.
           ENDIF.
         ENDIF.
         screen-active = '0'.
         l_field = 'X'.
         MODIFY SCREEN.
       ENDIF.
     ENDLOOP.
     IF sy-subrc NE 0.
       EXIT.
     ENDIF.
   ENDLOOP.

   LOOP AT SCREEN.

     IF screen-group1 = 'MOD' AND p_mod IS INITIAL."ON 20220325.
       screen-active = '0'.
       MODIFY SCREEN.
     ENDIF.

     IF screen-name CP '*P_CRE*'.
       IF gv_create_model = 1 OR gv_create_model = 3.
         screen-active = '1'.
         MODIFY SCREEN.
         CONTINUE.
       ELSE.
         screen-active = '0'.
         MODIFY SCREEN.
         CONTINUE.
       ENDIF.
     ENDIF.

     IF screen-name CP '*P_REF*'.
       IF gv_create_model = 2 OR gv_create_model = 3.
         screen-active = '1'.
         MODIFY SCREEN.
         CONTINUE.
       ELSE.
         screen-active = '0'.
         MODIFY SCREEN.
         CONTINUE.
       ENDIF.
     ENDIF.

     IF screen-name CP '*P_WERKS*'.
       IF g_werks_flag = 'P' .
         screen-active = '1'.
         MODIFY SCREEN.
         CONTINUE.
       ELSE.
         screen-active = '0'.
         MODIFY SCREEN.
         CONTINUE.
       ENDIF.
       CONTINUE.
     ENDIF.

     IF screen-name CP '*S_WERKS*'.
       IF g_werks_flag = 'S' .
         screen-active = '1'.
         MODIFY SCREEN.
         CONTINUE.
       ELSE.
         screen-active = '0'.
         MODIFY SCREEN.
         CONTINUE.
       ENDIF.
       CONTINUE.
     ENDIF.

     IF screen-name CP '*P_MOD*' AND p_mod = ''.
       screen-active = '0'.
       MODIFY SCREEN.
       CONTINUE.
     ENDIF.
     CLEAR l_field.

     IF screen-name CP '*P_OBJ*' AND g_action NE 'DIS'  .
       screen-active = '0'.
       MODIFY SCREEN.
       CONTINUE.
     ENDIF.

     IF screen-name CP '*P_FIN*'   .
       IF NOT ( p_ref = 'X'  AND gs_bustyp-busref CA c_fin_flag ).
         screen-active = '0'.
         MODIFY SCREEN.
       ENDIF.
       CONTINUE.
     ENDIF.

     IF screen-name CP '*P_LOAD*'   .
       READ TABLE lt_sel_screen INTO ls_sel_screen WITH KEY fieldname = 'P_LOAD'.
       IF sy-subrc EQ 0.
         IF screen-name = 'P_LOAD'.
           screen-input = 1.
         ENDIF.
         screen-active = '1'.
         screen-invisible = '0'.
         screen-output = '1'.
       ELSE.
         screen-active = '0'.
         screen-invisible = '1'.
         screen-output = '0'.
       ENDIF.
       MODIFY SCREEN.
       CONTINUE.
     ENDIF.


     LOOP AT lt_sel_screen INTO ls_sel_screen WHERE fieldname IS NOT INITIAL AND action = g_action  .
       IF screen-name CP '*' && ls_sel_screen-fieldname && '*'.
         screen-active = '1'.
         l_field = 'X'.
         IF ls_sel_screen-read_only IS NOT INITIAL.
           screen-input = ''.
         ENDIF.
         MODIFY SCREEN.
       ENDIF.
     ENDLOOP.

     CHECK l_field IS INITIAL.

     READ TABLE lt_sel_screen INTO ls_sel_screen
            WITH KEY fieldgroup = screen-group1
                     fieldname = ''
                     action = g_action.
     IF sy-subrc EQ 0.
       screen-active = '1'.
       CONTINUE.
     ELSE.
       screen-active = '0'.
     ENDIF.

     IF g_action = 'LOD' AND screen-group1 = 'CRE'.
       screen-active = '1'.
     ENDIF.

     IF screen-group1 = g_action OR screen-group1 = ''.
       screen-active = '1'.
     ENDIF.
     MODIFY SCREEN.
   ENDLOOP.
 ENDFORM.

 FORM frm_clear_sel_value USING fieldname .

   DATA: tabname TYPE char20.
   FIELD-SYMBOLS: <dyn_table> TYPE table.
   tabname = fieldname && '[]'.
   ASSIGN (tabname) TO <dyn_table>.
   CHECK sy-subrc EQ 0.
   CLEAR <dyn_table>.

 ENDFORM.


 FORM frm_set_default_value .
   DATA: tabname TYPE char30.
   DATA: ls_day(3) TYPE n.
   FIELD-SYMBOLS: <dyn_table> TYPE table,
                  <dyn_wa>    TYPE any,
                  <txt_field> TYPE any.

   PERFORM frm_clear_select.

   IF p_typ IS INITIAL.
     READ TABLE gt_list INDEX 1 INTO DATA(l_list).
     p_typ = l_list-key.
   ENDIF.

   DATA(lt_value) = zafo_basic=>get_bustyp_sel_value( p_typ ) .
   CHECK lt_value IS NOT INITIAL.

   LOOP AT lt_value INTO DATA(it_value)
                                         GROUP BY ( fieldname = it_value-fieldname ).

     IF it_value-fieldname+0(1) = 'P'.
*       txt_field = it_value-fieldname.
       ASSIGN (it_value-fieldname) TO <txt_field>.
       IF sy-subrc EQ 0.
         <txt_field> = it_value-fieldvalue.
       ENDIF.
     ENDIF.

     IF it_value-fieldname+0(1) = 'S'.
       CONCATENATE it_value-fieldname '[]' INTO tabname.
       ASSIGN (tabname) TO <dyn_table>.
       CHECK sy-subrc EQ 0.
       CLEAR <dyn_table>.

       LOOP AT GROUP it_value INTO DATA(ls_value).
         APPEND INITIAL LINE TO <dyn_table> ASSIGNING <dyn_wa>.

         ASSIGN COMPONENT 'SIGN' OF STRUCTURE <dyn_wa> TO <txt_field>.
         CHECK sy-subrc EQ 0.
         <txt_field> = 'I'.

         ASSIGN COMPONENT 'OPTION' OF STRUCTURE <dyn_wa> TO <txt_field>.
         CHECK sy-subrc EQ 0.
         <txt_field> = ls_value-fieldoption.

         ASSIGN COMPONENT 'LOW' OF STRUCTURE <dyn_wa> TO <txt_field>.
         CHECK sy-subrc EQ 0.
         IF ls_value-fieldvalue = 'UNAME'.
           <txt_field> = sy-uname.
         ELSEIF ls_value-fieldvalue+0(3) = 'DAY'.
           ls_day = ls_value-fieldvalue+3(3).
           <txt_field> = sy-datum - ls_day.
           ASSIGN COMPONENT 'HIGH' OF STRUCTURE <dyn_wa> TO <txt_field>.
           CHECK sy-subrc EQ 0.
           <txt_field> = sy-datum.
         ELSE.
           <txt_field> = ls_value-fieldvalue.
         ENDIF.
       ENDLOOP.
     ENDIF.
   ENDLOOP.
 ENDFORM.

 FORM frm_set_action.
   CASE 'X'.
     WHEN p_ref.
       g_action = 'REF'.
     WHEN p_cre.
       g_action = 'CRE'.
     WHEN p_mod.
       g_action = 'MOD'.
     WHEN p_dis.
       g_action = 'DIS'.
     WHEN p_load.
       g_action = 'LOD'.
   ENDCASE.

 ENDFORM.


 FORM frm_set_werks_flag.
   CASE 'X'.
     WHEN p_cre OR p_mod OR p_load.
       g_werks_flag = 'P'.
     WHEN p_dis OR p_ref.
       g_werks_flag = 'S'.
   ENDCASE.
 ENDFORM.

 FORM frm_init.
   IF sy-calld = 'X' OR sy-oncom = 'S'.
     p_mod = 'X'.
     GET PARAMETER ID 'ZBUSTYP' FIELD p_typ.
   ENDIF.
   b_down = TEXT-030.
   zwft_common=>set_init_sdate( EXPORTING days = '90' CHANGING sdate = s_erdat[] ).
   APPEND VALUE #( sign = 'I'
                                 option = 'NE'
                                 low = 'D'  ) TO s_status.

   PERFORM frm_set_list.

   PERFORM frm_set_default_value.

 ENDFORM.

 FORM frm_set_list.

   DATA(lt_bustyp) = zafo_basic=>get_bustyp_by_tcode( sy-tcode ).
   SORT lt_bustyp BY bustyp.
   READ TABLE lt_bustyp INTO DATA(ls_bustyp) INDEX 1.
   IF sy-subrc EQ 0 AND p_typ IS INITIAL.
     p_typ = ls_bustyp-bustyp.
     gv_create_model = ls_bustyp-busref_control.
     IF gv_create_model = 2 AND p_mod = ''..
       p_cre = ''.
       p_ref = 'X'.
     ELSE.
       p_cre = 'X'.
       p_ref = ''.
     ENDIF.
   ENDIF.

   gt_list = CORRESPONDING #( lt_bustyp  MAPPING key = bustyp
                                                                                     text =  bustyp_name1 ).
   CALL FUNCTION 'VRM_SET_VALUES'
     EXPORTING
       id     = 'P_TYP'
       values = gt_list.
   IF sy-subrc EQ 0.
     MODIFY SCREEN.
   ENDIF.
 ENDFORM.

 FORM frm_run.
   IF p_werks IS INITIAL AND g_werks_flag = 'P'.
     MESSAGE s005 DISPLAY LIKE 'E'.
     STOP.
   ENDIF.

   DATA(ls_bustyp) = zafo_basic=>get_bustyp( p_typ ).

   CASE 'X'.
     WHEN p_cre.
       IF g_werks_flag = 'P'.
         CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '01' bustyp = p_typ werks = p_werks ).
       ELSEIF g_werks_flag = 'S'.
         CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '01' bustyp = p_typ CHANGING swerks =  s_werks[] ).
       ENDIF.

       zafo_class=>create( EXPORTING werks = p_werks bustyp = p_typ item = gt_item[] ).

     WHEN p_ref.
       IF g_werks_flag = 'P'.
         CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '01' bustyp = p_typ werks = p_werks ).
       ELSEIF g_werks_flag = 'S'.
         CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '01' bustyp = p_typ CHANGING swerks =  s_werks[] ).
       ENDIF.

       PERFORM frm_get_ref TABLES gt_item USING ls_bustyp-busref.
       IF gt_item[] IS INITIAL.
         MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E'."无参考数据
         RETURN.
       ENDIF.
       zafo_class=>create_by_ref( EXPORTING bustyp = p_typ item = gt_item[] ).

     WHEN p_mod.

       IF g_werks_flag = 'P'.
         CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '02' bustyp = p_typ werks = p_werks ).
       ELSEIF g_werks_flag = 'S'.
         CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '02' bustyp = p_typ CHANGING swerks =  s_werks[] ).
       ENDIF.

       zafo_class=>maintain( EXPORTING afono = p_afono bustyp = p_typ  ).

     WHEN p_dis.

       IF g_werks_flag = 'P'.
         CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '03' bustyp = p_typ werks = p_werks ).
       ELSEIF g_werks_flag = 'S'.
         CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '03' bustyp = p_typ CHANGING swerks =  s_werks[] ).
       ENDIF.

       PERFORM frm_get_display TABLES gt_head gt_item.
       CHECK gt_head[] IS NOT INITIAL.
       zafo_class=>report( bustyp = p_typ head = gt_head[] item = gt_item[] ).

     WHEN p_load.
       IF g_werks_flag = 'P'.
         CHECK zafo_basic=>auth_check_line( EXPORTING actvt = '01' bustyp = p_typ werks = p_werks ).
       ELSEIF g_werks_flag = 'S'.
         CHECK zafo_basic=>auth_check_tab( EXPORTING actvt = '01' bustyp = p_typ CHANGING swerks =  s_werks[] ).
       ENDIF.
*
*       CALL FUNCTION 'ZAFO_UPLOAD_CREATE'
*         EXPORTING
*           i_werks     = p_werks
*           i_bustyp    = p_typ
*           i_filename  = p_file
*           i_begin_row = p_row
**           TABLES
**          ET_RETURN   =
*         EXCEPTIONS
*           error       = 1
*           OTHERS      = 2.
*       IF sy-subrc <> 0.
** IMPLEMENT SUITABLE ERROR HANDLING HERE
*       ENDIF.
   ENDCASE.

 ENDFORM.


 FORM frm_get_display TABLES ct_head STRUCTURE zafo_shead
                               ct_item STRUCTURE zafo_sitem.
   SELECT h~* FROM zafo_head AS h
     LEFT JOIN zafo_item AS i
     ON h~afono = i~afono
     INTO CORRESPONDING FIELDS OF TABLE @ct_head
     WHERE bustyp = @p_typ
     AND h~afono IN @s_afono
     AND h~ernam IN @s_ernam
     AND h~erdat IN @s_erdat
     AND h~budat IN @s_budat
     AND h~status IN @s_status
     AND h~lifnr IN @s_lifnr
     AND h~lgort IN @s_lgort
     AND h~kunnr IN @s_kunnr
     AND h~eeind IN @s_eeind
     AND i~ebeln IN @s_ebeln
     AND i~matnr IN @s_matnr
     AND i~satnr IN @s_satnr
     AND i~charg IN @s_charg
.
   IF sy-subrc NE 0.
     MESSAGE s006 DISPLAY LIKE 'E'.
     RETURN.
   ENDIF.
   SORT ct_head BY afono.
   DELETE ADJACENT DUPLICATES FROM ct_head COMPARING afono.

   SELECT * FROM zafo_item
     INTO CORRESPONDING FIELDS OF TABLE
     ct_item
     FOR ALL ENTRIES IN ct_head
     WHERE afono = ct_head-afono.


   LOOP AT ct_head ASSIGNING  FIELD-SYMBOL(<fs_head>).
     zafo_basic=>set_icon( EXPORTING status = <fs_head>-status
                                         IMPORTING icon = <fs_head>-icon text = <fs_head>-text ).
   ENDLOOP.

   SORT ct_item BY afono   .

   LOOP AT ct_item ASSIGNING FIELD-SYMBOL(<fs_item>).
     zafo_basic=>set_icon( EXPORTING status = <fs_item>-item_status
                                         IMPORTING icon = <fs_item>-icon text = <fs_item>-text ).
   ENDLOOP.

 ENDFORM.

 FORM  frm_get_ref   TABLES ct_item STRUCTURE zafo_sitem
                      USING busref.
   CHECK busref IS NOT INITIAL.
   PERFORM frm_auth_check.
   DATA(perform_name) = |FRM_REF_{ busref }|.
   PERFORM (perform_name) IN PROGRAM zafo TABLES ct_item IF FOUND.
 ENDFORM.

 FORM frm_auth_check.
   IF gs_bustyp-category = 'PO' .
     SELECT * FROM t024
       INTO TABLE @DATA(gt_t024)
       WHERE ekgrp IN @s_ekgrp.

     CHECK sy-subrc EQ 0.
     CLEAR s_ekgrp[].

     LOOP AT gt_t024 INTO DATA(gs_t024).
       AUTHORITY-CHECK OBJECT 'ZAFO_EKGRP'
                  ID 'EKGRP' FIELD gs_t024-ekgrp
                  ID 'ACTVT' FIELD '01'.
       CHECK sy-subrc EQ 0.

       s_ekgrp-sign = 'I'.
       s_ekgrp-option = 'EQ'.
       s_ekgrp-low = gs_t024-ekgrp.
       APPEND s_ekgrp.
       CLEAR s_ekgrp.
     ENDLOOP.

   ENDIF.

 ENDFORM.

 FORM frm_at_screen CHANGING p_ucomm.
   CASE p_ucomm.
     WHEN 'DOWN'.
       PERFORM frm_download_temp .  "下载模板
     WHEN 'CON'.
       PERFORM frm_set_default_value .
   ENDCASE.
 ENDFORM.

 FORM frm_download_temp .
   CHECK p_typ IS NOT INITIAL.
   CALL FUNCTION 'ZAFO_DOWNLOAD_TEMPLATE'
     EXPORTING
       i_bustyp = p_typ
*         TABLES
*      ET_RETURN       =
     EXCEPTIONS
       error    = 1
       OTHERS   = 2.
   IF sy-subrc <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
   ENDIF.

 ENDFORM.

 FORM frm_get_excel_f4  CHANGING p_p_up.


   CALL FUNCTION 'WS_FILENAME_GET'
     EXPORTING
       def_path         = 'D:\'
       mask             = '*,XLSX,*.XLSX,*.XLS,*.XLS'
       title            = TEXT-001
     IMPORTING
       filename         = p_p_up
     EXCEPTIONS
       inv_winsys       = 1
       no_batch         = 2
       selection_cancel = 3
       selection_error  = 4
       OTHERS           = 5.

   IF sy-subrc <> 0 AND sy-subrc <> 3.
     MESSAGE i004(zafo) DISPLAY LIKE 'E'. "选择文件出错
     STOP.
   ENDIF.
*  ENDIF.
 ENDFORM.                    " FRM_GET_EXCEL_F4
