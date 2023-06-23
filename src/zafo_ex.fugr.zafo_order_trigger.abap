FUNCTION zafo_order_trigger.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(ZCODE) TYPE  CHAR10
*"     REFERENCE(ZZPINO) TYPE  ZZPINO OPTIONAL
*"     REFERENCE(ZPPDHD) TYPE  ZPPDHD
*"     REFERENCE(REMARK) TYPE  ZAFO_REMARK OPTIONAL
*"     REFERENCE(ERNAM) TYPE  ERNAM
*"     REFERENCE(ERDAT) TYPE  ERDAT OPTIONAL
*"     REFERENCE(ERZET) TYPE  ERZET OPTIONAL
*"----------------------------------------------------------------------

  DATA g_error TYPE char1.
  DATA : ls_zafo_trigger_log TYPE zafo_trigger_log,
         lt_zafo_trigger_log TYPE TABLE OF zafo_trigger_log.
  DATA :e_lognr TYPE lognr.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZAFONO_LOG'
    IMPORTING
      number                  = e_lognr
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  CLEAR ls_zafo_trigger_log.
  CASE zcode.
    WHEN 'CLOSE'." 大货通知单关闭
      " 各单总结表
      PERFORM frm_order_trigger IN PROGRAM zmmr0180 IF FOUND USING zzpino zppdhd ernam erdat erzet.

      ls_zafo_trigger_log-lognr = e_lognr.
      ls_zafo_trigger_log-object = 'ORDER_TRI'.
      ls_zafo_trigger_log-zzpino = zzpino.
      ls_zafo_trigger_log-zppdhd = zppdhd.
      ls_zafo_trigger_log-erdat = erdat.
      ls_zafo_trigger_log-erzet = erzet.
      ls_zafo_trigger_log-ernam = ernam.
      ls_zafo_trigger_log-zoperation = 'CLOSE'.
      INSERT zafo_trigger_log FROM ls_zafo_trigger_log.

    WHEN 'OPEN'." 大货通知单打开
      ls_zafo_trigger_log-lognr = e_lognr.
      ls_zafo_trigger_log-object = 'ORDER_TRI'.
      ls_zafo_trigger_log-zzpino = zzpino.
      ls_zafo_trigger_log-zppdhd = zppdhd.
      ls_zafo_trigger_log-erdat = erdat.
      ls_zafo_trigger_log-erzet = erzet.
      ls_zafo_trigger_log-ernam = ernam.
      ls_zafo_trigger_log-zoperation = 'OPEN'.
      INSERT zafo_trigger_log FROM ls_zafo_trigger_log.

    WHEN OTHERS.
  ENDCASE.

ENDFUNCTION.
