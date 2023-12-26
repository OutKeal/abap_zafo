FUNCTION zafo_salesdocument_create.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(CUSTOMER) TYPE  VBAK-KUNNR
*"     VALUE(SALESDOCUMENT) LIKE  BAPIVBELN-VBELN OPTIONAL
*"     VALUE(SALES_HEADER_IN) LIKE  BAPISDHD1 STRUCTURE  BAPISDHD1
*"     VALUE(SALES_HEADER_INX) LIKE  BAPISDHD1X STRUCTURE  BAPISDHD1X
*"       OPTIONAL
*"     VALUE(SENDER) LIKE  BDI_LOGSYS STRUCTURE  BDI_LOGSYS OPTIONAL
*"     VALUE(BINARY_RELATIONSHIPTYPE) LIKE  BRELTYP-RELTYPE DEFAULT
*"       SPACE
*"     VALUE(INT_NUMBER_ASSIGNMENT) LIKE  BAPIFLAG-BAPIFLAG DEFAULT
*"       SPACE
*"     VALUE(BEHAVE_WHEN_ERROR) LIKE  BAPIFLAG-BAPIFLAG DEFAULT SPACE
*"     VALUE(LOGIC_SWITCH) LIKE  BAPISDLS STRUCTURE  BAPISDLS DEFAULT
*"       SPACE
*"     VALUE(BUSINESS_OBJECT) LIKE  BAPIUSW01-OBJTYPE DEFAULT SPACE
*"     VALUE(TESTRUN) LIKE  BAPIFLAG-BAPIFLAG OPTIONAL
*"     VALUE(CONVERT_PARVW_AUART) LIKE  BAPIFLAG-BAPIFLAG DEFAULT SPACE
*"     VALUE(STATUS_BUFFER_REFRESH) LIKE  BAPIFLAG-BAPIFLAG DEFAULT 'X'
*"     VALUE(CALL_ACTIVE) TYPE  CHAR4 DEFAULT SPACE
*"     VALUE(I_WITHOUT_INIT) LIKE  BAPIFLAG-BAPIFLAG DEFAULT SPACE
*"     VALUE(I_REFRESH_V45I) TYPE  BAPIFLAG-BAPIFLAG DEFAULT 'X'
*"     VALUE(I_TESTRUN_EXTENDED) TYPE  BAPIFLAG-BAPIFLAG DEFAULT SPACE
*"  EXPORTING
*"     VALUE(SALESDOCUMENT_EX) LIKE  BAPIVBELN-VBELN
*"     VALUE(SALES_HEADER_OUT) LIKE  BAPISDHD STRUCTURE  BAPISDHD
*"     VALUE(SALES_HEADER_STATUS) LIKE  BAPISDHDST STRUCTURE
*"        BAPISDHDST
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      SALES_ITEMS_IN STRUCTURE  BAPISDITM OPTIONAL
*"      SALES_ITEMS_INX STRUCTURE  BAPISDITMX OPTIONAL
*"      SALES_PARTNERS STRUCTURE  BAPIPARNR OPTIONAL
*"      SALES_SCHEDULES_IN STRUCTURE  BAPISCHDL OPTIONAL
*"      SALES_SCHEDULES_INX STRUCTURE  BAPISCHDLX OPTIONAL
*"      SALES_CONDITIONS_IN STRUCTURE  BAPICOND OPTIONAL
*"      SALES_CONDITIONS_INX STRUCTURE  BAPICONDX OPTIONAL
*"      SALES_CFGS_REF STRUCTURE  BAPICUCFG OPTIONAL
*"      SALES_CFGS_INST STRUCTURE  BAPICUINS OPTIONAL
*"      SALES_CFGS_PART_OF STRUCTURE  BAPICUPRT OPTIONAL
*"      SALES_CFGS_VALUE STRUCTURE  BAPICUVAL OPTIONAL
*"      SALES_CFGS_BLOB STRUCTURE  BAPICUBLB OPTIONAL
*"      SALES_CFGS_VK STRUCTURE  BAPICUVK OPTIONAL
*"      SALES_CFGS_REFINST STRUCTURE  BAPICUREF OPTIONAL
*"      SALES_CCARD STRUCTURE  BAPICCARD OPTIONAL
*"      SALES_TEXT STRUCTURE  BAPISDTEXT OPTIONAL
*"      SALES_KEYS STRUCTURE  BAPISDKEY OPTIONAL
*"      SALES_CONTRACT_IN STRUCTURE  BAPICTR OPTIONAL
*"      SALES_CONTRACT_INX STRUCTURE  BAPICTRX OPTIONAL
*"      EXTENSIONIN STRUCTURE  BAPIPAREX OPTIONAL
*"      PARTNERADDRESSES STRUCTURE  BAPIADDR1 OPTIONAL
*"      SALES_SCHED_CONF_IN STRUCTURE  BAPISCHDL2 OPTIONAL
*"      ITEMS_EX STRUCTURE  BAPISDIT OPTIONAL
*"      SCHEDULE_EX STRUCTURE  BAPISDHEDU OPTIONAL
*"      BUSINESS_EX STRUCTURE  BAPISDBUSI OPTIONAL
*"      INCOMPLETE_LOG STRUCTURE  BAPIINCOMP OPTIONAL
*"      EXTENSIONEX STRUCTURE  BAPIPAREX OPTIONAL
*"      CONDITIONS_EX STRUCTURE  BAPICOND OPTIONAL
*"      PARTNERS_EX STRUCTURE  BAPISDPART OPTIONAL
*"      TEXTHEADERS_EX STRUCTURE  BAPISDTEHD OPTIONAL
*"      TEXTLINES_EX STRUCTURE  BAPITEXTLI OPTIONAL
*"      BATCH_CHARC STRUCTURE  BAPIBTSEL OPTIONAL
*"      CAMPAIGN_ASGN STRUCTURE  BAPISDCA OPTIONAL
*"----------------------------------------------------------------------
  DATA:knvp TYPE TABLE OF knvp .
  IF sales_header_in-sales_org IS NOT INITIAL
    AND sales_header_in-distr_chan IS NOT INITIAL
    AND sales_header_in-division IS NOT INITIAL.
    CALL FUNCTION 'SD_KNVP_READ'
      EXPORTING
        fif_vkorg            = sales_header_in-sales_org
        fif_vtweg            = sales_header_in-distr_chan
        fif_spart            = sales_header_in-division
        fif_kunnr            = customer
      TABLES
        fet_knvp             = knvp
      EXCEPTIONS
        parameter_incomplete = 1
        no_record_found      = 2
        OTHERS               = 3.
    sales_partners[] = CORRESPONDING #( knvp MAPPING partn_role = parvw partn_numb = kunn2  ).
  ENDIF.

  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      salesdocument           = salesdocument
      sales_header_in         = sales_header_in
      sales_header_inx        = sales_header_inx
      sender                  = sender
      binary_relationshiptype = binary_relationshiptype
      int_number_assignment   = int_number_assignment
      behave_when_error       = behave_when_error
      logic_switch            = logic_switch
      business_object         = business_object
      testrun                 = testrun
      convert_parvw_auart     = convert_parvw_auart
      status_buffer_refresh   = status_buffer_refresh
      call_active             = call_active
      i_without_init          = i_without_init
      i_refresh_v45i          = i_refresh_v45i
      i_testrun_extended      = i_testrun_extended
    IMPORTING
      salesdocument_ex        = salesdocument_ex
      sales_header_out        = sales_header_out
      sales_header_status     = sales_header_status
    TABLES
      return                  = return
      sales_items_in          = sales_items_in
      sales_items_inx         = sales_items_inx
      sales_partners          = sales_partners
      sales_schedules_in      = sales_schedules_in
      sales_schedules_inx     = sales_schedules_inx
      sales_conditions_in     = sales_conditions_in
      sales_conditions_inx    = sales_conditions_inx
      sales_cfgs_ref          = sales_cfgs_ref
      sales_cfgs_inst         = sales_cfgs_inst
      sales_cfgs_part_of      = sales_cfgs_part_of
      sales_cfgs_value        = sales_cfgs_value
      sales_cfgs_blob         = sales_cfgs_blob
      sales_cfgs_vk           = sales_cfgs_vk
      sales_cfgs_refinst      = sales_cfgs_refinst
      sales_ccard             = sales_ccard
      sales_text              = sales_text
      sales_keys              = sales_keys
      sales_contract_in       = sales_contract_in
      sales_contract_inx      = sales_contract_inx
      extensionin             = extensionin
      partneraddresses        = partneraddresses
      sales_sched_conf_in     = sales_sched_conf_in
      items_ex                = items_ex
      schedule_ex             = schedule_ex
      business_ex             = business_ex
      incomplete_log          = incomplete_log
      extensionex             = extensionex
      conditions_ex           = conditions_ex
      partners_ex             = partners_ex
      textheaders_ex          = textheaders_ex
      textlines_ex            = textlines_ex
      batch_charc             = batch_charc
      campaign_asgn           = campaign_asgn.


  LOOP AT return ASSIGNING FIELD-SYMBOL(<l_return>)
  WHERE id = 'V1' AND number = '555'.
    <l_return>-type = 'E'.
    EXIT.
  ENDLOOP.

ENDFUNCTION.
