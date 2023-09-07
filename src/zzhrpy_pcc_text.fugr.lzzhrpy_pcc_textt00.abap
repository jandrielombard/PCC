*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_TEXT..................................*
DATA:  BEGIN OF STATUS_ZHRPY_PCC_TEXT                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRPY_PCC_TEXT                .
CONTROLS: TCTRL_ZHRPY_PCC_TEXT
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZHRPY_PCC_TEXT                .
TABLES: ZHRPY_PCC_TEXT                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
