*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_TXTKEY................................*
DATA:  BEGIN OF STATUS_ZHRPY_PCC_TXTKEY              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRPY_PCC_TXTKEY              .
CONTROLS: TCTRL_ZHRPY_PCC_TXTKEY
            TYPE TABLEVIEW USING SCREEN '0111'.
*.........table declarations:.................................*
TABLES: *ZHRPY_PCC_TXTKEY              .
TABLES: ZHRPY_PCC_TXTKEY               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
