*****           Implementation of object type ZAFO                 *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
  " begin of private,
  "   to declare private attributes remove comments and
  "   insert private attributes here ...
  " end of private,
  BEGIN OF key,
    afono LIKE zafo_head-afono,
  END OF key.
end_data object. " Do not change.. DATA is generated

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


CALL FUNCTION 'ZAFO_CALL_TRANSACTION'
 EXPORTING
   AFONO         = object-key-afono
          .

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
