*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_VTEXT.................................*
DATA:  BEGIN OF STATUS_ZHRPY_PCC_VTEXT               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRPY_PCC_VTEXT               .
CONTROLS: TCTRL_ZHRPY_PCC_VTEXT
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZHRPY_PCC_VTEXT               .
TABLES: ZHRPY_PCC_VTEXT                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
