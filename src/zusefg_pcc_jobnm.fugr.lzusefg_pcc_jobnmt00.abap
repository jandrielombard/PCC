*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_JOBNM..................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_JOBNM                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_JOBNM                .
CONTROLS: TCTRL_ZUSE_PCC_JOBNM
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_JOBNM                .
TABLES: ZUSE_PCC_JOBNM                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
