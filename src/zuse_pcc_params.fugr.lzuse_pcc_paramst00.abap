*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_PARAMS.................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_PARAMS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_PARAMS               .
CONTROLS: TCTRL_ZUSE_PCC_PARAMS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_PARAMS               .
TABLES: ZUSE_PCC_PARAMS                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
