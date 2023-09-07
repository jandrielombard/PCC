*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_TABLES................................*
DATA:  BEGIN OF STATUS_ZHRPY_PCC_TABLES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRPY_PCC_TABLES              .
CONTROLS: TCTRL_ZHRPY_PCC_TABLES
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZHRPY_PCC_TABLES              .
TABLES: ZHRPY_PCC_TABLES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
