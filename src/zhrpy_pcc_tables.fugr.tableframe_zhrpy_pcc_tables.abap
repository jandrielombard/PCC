*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZHRPY_PCC_TABLES
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZHRPY_PCC_TABLES   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.