*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZZHRPY_PCC_TXTKY
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZZHRPY_PCC_TXTKY   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
