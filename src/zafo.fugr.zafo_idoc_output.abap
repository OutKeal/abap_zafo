FUNCTION zafo_idoc_output.
*"--------------------------------------------------------------------
*"*"局部接口：
*"  IMPORTING
*"     VALUE(ISHEAD) TYPE  ZAFO_BAPI_HEAD
*"     VALUE(IPOST) TYPE  BAPIFLAG-BAPIFLAG OPTIONAL
*"     VALUE(NOCOMMIT) TYPE  BAPIFLAG-BAPIFLAG OPTIONAL
*"     VALUE(OBJ_TYPE) LIKE  SERIAL-OBJ_TYPE DEFAULT 'ZAFO'
*"     VALUE(SERIAL_ID) LIKE  SERIAL-CHNUM DEFAULT '0'
*"  TABLES
*"      ITITEM STRUCTURE  ZAFO_BAPI_ITEM
*"      RECEIVERS STRUCTURE  BDI_LOGSYS
*"      COMMUNICATION_DOCUMENTS STRUCTURE  SWOTOBJID OPTIONAL
*"      APPLICATION_OBJECTS STRUCTURE  SWOTOBJID OPTIONAL
*"  EXCEPTIONS
*"      ERROR_CREATING_IDOCS
*"--------------------------------------------------------------------
*----------------------------------------------------------------------*
*  this function module is generated                                   *
*          never change it manually, please!        2023.10.25         *
*----------------------------------------------------------------------*

  DATA: idoc_control  LIKE bdicontrol,
        idoc_data     LIKE edidd      OCCURS 0 WITH HEADER LINE,
        idoc_receiver LIKE bdi_logsys OCCURS 0 WITH HEADER LINE,
        idoc_comm     LIKE edidc      OCCURS 0 WITH HEADER LINE,
        syst_info     LIKE syst.


* create IDoc control-record                                           *
  idoc_control-mestyp = 'ZAFO_CREATE'.
  idoc_control-idoctp = 'ZAFO_CREATE01'.
  idoc_control-serial = sy-datum.
  idoc_control-serial+8 = sy-uzeit.

  idoc_receiver[] = receivers[].

*   call subroutine to create IDoc data-record                         *
  CLEAR: syst_info, idoc_data.
  REFRESH idoc_data.
  PERFORM zafo_idoc_output
          TABLES
              ititem
              idoc_data
          USING
              ishead
              ipost
              nocommit
              syst_info
              .
  IF NOT syst_info IS INITIAL.
    MESSAGE ID syst_info-msgid
          TYPE syst_info-msgty
        NUMBER syst_info-msgno
          WITH syst_info-msgv1 syst_info-msgv2
               syst_info-msgv3 syst_info-msgv4
    RAISING error_creating_idocs.
  ENDIF.

*   distribute idocs                                                   *
  CALL FUNCTION 'ALE_IDOCS_CREATE'
       EXPORTING
            idoc_control                = idoc_control
            obj_type                    = obj_type
            chnum                       = serial_id
       TABLES
            idoc_data                   = idoc_data
            receivers                   = idoc_receiver
*             CREATED_IDOCS               =                            *
            created_idocs_additional    = idoc_comm
            application_objects         = application_objects
       EXCEPTIONS
            idoc_input_was_inconsistent = 1
            OTHERS                      = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING error_creating_idocs.
  ENDIF.

  IF communication_documents IS REQUESTED.
    LOOP AT idoc_comm.
      CLEAR communication_documents.
      communication_documents-objtype  = 'IDOC'.
      communication_documents-objkey   = idoc_comm-docnum.
      communication_documents-logsys   = idoc_comm-rcvprn.
      communication_documents-describe = space.
      APPEND communication_documents.
    ENDLOOP.
  ENDIF.

* applications do commit work to trigger communications                *





ENDFUNCTION.


* subroutine creating IDoc data-record                                 *
FORM zafo_idoc_output
     TABLES
         ititem STRUCTURE
           zafo_bapi_item
         idoc_data STRUCTURE edidd
     USING
         ishead LIKE
           zafo_bapi_head
         ipost LIKE
           bapiflag-bapiflag
         nocommit LIKE
           bapiflag-bapiflag
         syst_info LIKE syst
         .                                                  "#EC *

  DATA:  z1zafo_create LIKE z1zafo_create.
  DATA:  z1zafo_bapi_head LIKE z1zafo_bapi_head.
  DATA:  z1zafo_bapi_item LIKE z1zafo_bapi_item.

* go through all IDoc-segments                                         *

* for segment 'Z1ZAFO_CREATE'                                          *
  CLEAR: z1zafo_create,
         idoc_data.
  MOVE ipost
    TO z1zafo_create-i_post.
  MOVE nocommit
    TO z1zafo_create-no_commit.
  idoc_data-sdata  = z1zafo_create.
  idoc_data-segnam = 'Z1ZAFO_CREATE'.
  APPEND idoc_data.


*   for segment 'Z1ZAFO_BAPI_HEAD'                                     *
  CLEAR: z1zafo_bapi_head,
         idoc_data.
  MOVE-CORRESPONDING ishead
      TO z1zafo_bapi_head.                                  "#EC ENHOK
  IF NOT z1zafo_bapi_head IS INITIAL.
    idoc_data-sdata = z1zafo_bapi_head.
    idoc_data-segnam = 'Z1ZAFO_BAPI_HEAD'.
    APPEND idoc_data.
  ENDIF.

*   for segment 'Z1ZAFO_BAPI_ITEM'                                     *
  LOOP AT ititem
               .
    CLEAR: z1zafo_bapi_item,
           idoc_data.
    MOVE-CORRESPONDING ititem
        TO z1zafo_bapi_item.                                "#EC ENHOK
    CONDENSE z1zafo_bapi_item-menge.
    CONDENSE z1zafo_bapi_item-price_long.
    CONDENSE z1zafo_bapi_item-peinh.
    CONDENSE z1zafo_bapi_item-menge_ref.
    CONDENSE z1zafo_bapi_item-menge_done.
    CONDENSE z1zafo_bapi_item-menge_plan.
    CONDENSE z1zafo_bapi_item-menge_stock.
    CONDENSE z1zafo_bapi_item-menge_flow.
    idoc_data-sdata = z1zafo_bapi_item.
    idoc_data-segnam = 'Z1ZAFO_BAPI_ITEM'.
    APPEND idoc_data.
  ENDLOOP.


* end of through all IDoc-segments                                     *

ENDFORM.                               " ZAFO_IDOC_OUTPUT
