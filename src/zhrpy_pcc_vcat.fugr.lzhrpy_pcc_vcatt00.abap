*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_VCAT..................................*
DATA:  BEGIN OF STATUS_ZHRPY_PCC_VCAT                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRPY_PCC_VCAT                .
CONTROLS: TCTRL_ZHRPY_PCC_VCAT
            TYPE TABLEVIEW USING SCREEN '0110'.
*.........table declarations:.................................*
TABLES: *ZHRPY_PCC_VCAT                .
TABLES: ZHRPY_PCC_VCAT                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
