*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_VTEXT..................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_VTEXT                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_VTEXT                .
CONTROLS: TCTRL_ZUSE_PCC_VTEXT
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_VTEXT                .
TABLES: ZUSE_PCC_VTEXT                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
