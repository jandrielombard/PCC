*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_TEXT...................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_TEXT                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_TEXT                 .
CONTROLS: TCTRL_ZUSE_PCC_TEXT
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_TEXT                 .
TABLES: ZUSE_PCC_TEXT                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
