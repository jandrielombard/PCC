*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_JOBNM.................................*
DATA:  BEGIN OF STATUS_ZHRPY_PCC_JOBNM               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRPY_PCC_JOBNM               .
CONTROLS: TCTRL_ZHRPY_PCC_JOBNM
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZHRPY_PCC_JOBNM               .
TABLES: ZHRPY_PCC_JOBNM                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
