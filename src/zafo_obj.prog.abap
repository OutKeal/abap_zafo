*****           Implementation of object type ZAFO                 *****
INCLUDE <object>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      AFONO LIKE ZAFO_HEAD-AFONO,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated

begin_method gosaddobjects changing container.
DATA:
      service(255),
      busidentifs LIKE borident OCCURS 0.
DATA: ls_borident TYPE borident.

CLEAR ls_borident.

ls_borident-logsys = space.
ls_borident-objtype = 'ZAFO'.
ls_borident-objkey = object-key.
APPEND ls_borident TO BusIdentifs.

swc_get_element container 'Service' service.
swc_set_table container 'BusIdentifs' busidentifs.
end_method.

begin_method display changing container.

ZAFO_CLASS=>maintain( object-key-afono ).

*SELECT SINGLE
*  bustyp
*  INTO @DATA(ls_bustyp)
*  FROM zafo_head WHERE afono = @object-key-afono.
*
*CHECK sy-subrc EQ 0.
*
*SELECT SINGLE tcode
*  INTO @DATA(ls_tcode)
*  FROM zafo_bustype
*  WHERE bustyp = @ls_bustyp.
*
*SET PARAMETER ID 'ZAFONO' FIELD object-key-afono.
*SET PARAMETER ID 'ZBUSTYP' FIELD ls_bustyp.

*CALL TRANSACTION ls_tcode AND SKIP FIRST SCREEN.


end_method.

begin_method existencecheck changing container.

DATA:ls_afono TYPE zafono.
SELECT SINGLE afono  INTO ls_afono FROM zafo_head WHERE afono = object-key-afono.
IF sy-subrc NE 0.
  exit_object_not_found.
ENDIF.


end_method.

BEGIN_METHOD CREATE CHANGING CONTAINER.
DATA:
      ISHEAD LIKE ZAFO_BAPI_HEAD,
      IPOST TYPE BAPIFLAG-BAPIFLAG,
      NOCOMMIT TYPE BAPIFLAG-BAPIFLAG,
      EAFONO TYPE ZAFO_SHEAD-AFONO,
      ESHEAD LIKE ZAFO_SHEAD,
      ITITEM LIKE ZAFO_BAPI_ITEM OCCURS 0,
      ETITEM LIKE ZAFO_SITEM OCCURS 0,
      ETRETURN LIKE BAPIRET2 OCCURS 0.
  SWC_GET_ELEMENT CONTAINER 'IsHead' ISHEAD.
  SWC_GET_ELEMENT CONTAINER 'IPost' IPOST.
  SWC_GET_ELEMENT CONTAINER 'NoCommit' NOCOMMIT.
  SWC_GET_TABLE CONTAINER 'ItItem' ITITEM.
  SWC_GET_TABLE CONTAINER 'EtItem' ETITEM.
  SWC_GET_TABLE CONTAINER 'EtReturn' ETRETURN.
  CALL FUNCTION 'ZAFO_CREATE'
    EXPORTING
      IS_HEAD = ISHEAD
      I_POST = IPOST
      NO_COMMIT = NOCOMMIT
    IMPORTING
      E_AFONO = EAFONO
      ES_HEAD = ESHEAD
    TABLES
      IT_ITEM = ITITEM
      ET_ITEM = ETITEM
      ET_RETURN = ETRETURN
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.
  SWC_SET_ELEMENT CONTAINER 'EAfono' EAFONO.
  SWC_SET_ELEMENT CONTAINER 'EsHead' ESHEAD.
  SWC_SET_TABLE CONTAINER 'ItItem' ITITEM.
  SWC_SET_TABLE CONTAINER 'EtItem' ETITEM.
  SWC_SET_TABLE CONTAINER 'EtReturn' ETRETURN.
END_METHOD.
