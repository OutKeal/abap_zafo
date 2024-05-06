*&---------------------------------------------------------------------*
*& Report ZAFO_ACTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zafo_action.
TABLES:zafo_head.
DATA:gt_list  TYPE  vrm_values.

SELECT-OPTIONS:s_afono FOR zafo_head-afono,
                              s_status FOR zafo_head-status,
                              s_bustyp FOR zafo_head-bustyp.
PARAMETERS:p_action     TYPE zafo_action AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND con OBLIGATORY.

INITIALIZATION.
  gt_list = VALUE #( ( key = '&DELETE' text = '作废' )
                                ( key = '&COMMIT' text = '提交' )
                                ( key = '&UNCOMMIT' text = '取消提交' )
                                ( key = '&APPROVE' text = '审批' )
                                ( key = '&UNAPPROVE' text = '取消审批' )
                                ( key = '&POST' text = '过账' )
                                ( key = '&CANCEL' text = '冲销' )
                                      ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_ACTION'
      values = gt_list.

START-OF-SELECTION.


  SELECT * FROM zafo_head
    WHERE afono IN @s_afono
    AND status IN @s_status
    AND bustyp IN @s_bustyp
    INTO TABLE @DATA(gt_head).

  IF sy-subrc NE 0.
    MESSAGE s899(zmm) WITH '无数据' .
    STOP.
  ENDIF.

  DATA(msg) = NEW zwft_message( ).

  LOOP AT gt_head INTO DATA(l_head).
    zafo_class=>action_ext( EXPORTING afono = l_head-afono aciton = p_action
                                          IMPORTING return = DATA(lt_return) ).
    msg->add_table( lt_return ).
  ENDLOOP.

  msg->pop_all_msg( ).
*  zwft_falv=>create( CHANGING ct_table = msg->t_return )->display( ).
