*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_TABLES.................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_TABLES               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_TABLES               .
CONTROLS: TCTRL_ZUSE_PCC_TABLES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_TABLES               .
TABLES: ZUSE_PCC_TABLES                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
