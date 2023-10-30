*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_TXTKEY.................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_TXTKEY               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_TXTKEY               .
CONTROLS: TCTRL_ZUSE_PCC_TXTKEY
            TYPE TABLEVIEW USING SCREEN '0111'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_TXTKEY               .
TABLES: ZUSE_PCC_TXTKEY                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
