*----------------------------------------------------------------------*
***INCLUDE LZAFO_EXI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.

    WHEN '&OK'.
      CALL METHOD g_grid_9001->check_changed_data.
      CALL METHOD g_grid_9001->free.
      FREE g_grid_9001.
      LEAVE TO SCREEN 0.
    WHEN '&CANCEL'.
      CALL METHOD g_grid_9001->free.
      FREE g_grid_9001.
      CLEAR batch_item[].
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.


MODULE user_command_8001 INPUT.

  CASE sy-ucomm.

    WHEN '&OK'.
      g_confirm = 'X'.
    WHEN '&CANCEL'.
      CLEAR g_confirm.
  ENDCASE.

  CALL METHOD g_grid_8001->free.
  FREE g_grid_8001.

  REFRESH gt_fcat.
  REFRESH gt_item.

  LEAVE TO SCREEN 0.
ENDMODULE.


MODULE user_command_7001 INPUT.

  CASE sy-ucomm.

    WHEN '&OK'.
      g_confirm = 'X'.
    WHEN '&CANCEL'.
      CLEAR g_confirm.
  ENDCASE.

  CALL METHOD g_grid_7001->free.
  FREE g_grid_8001.

  REFRESH gt_fcat.
  REFRESH gt_item.

  LEAVE TO SCREEN 0.
ENDMODULE.
