*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_VTEXTK.................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_VTEXTK               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_VTEXTK               .
CONTROLS: TCTRL_ZUSE_PCC_VTEXTK
            TYPE TABLEVIEW USING SCREEN '2222'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_VTEXTK               .
TABLES: ZUSE_PCC_VTEXTK                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
