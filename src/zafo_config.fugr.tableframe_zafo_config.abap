*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZAFO_CONFIG
*   generation date: 2021-07-01 at 13:24:29
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZAFO_CONFIG        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
