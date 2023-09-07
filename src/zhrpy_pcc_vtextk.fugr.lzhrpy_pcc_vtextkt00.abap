*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_VTEXTK................................*
DATA:  BEGIN OF STATUS_ZHRPY_PCC_VTEXTK              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRPY_PCC_VTEXTK              .
CONTROLS: TCTRL_ZHRPY_PCC_VTEXTK
            TYPE TABLEVIEW USING SCREEN '0222'.
*.........table declarations:.................................*
TABLES: *ZHRPY_PCC_VTEXTK              .
TABLES: ZHRPY_PCC_VTEXTK               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
