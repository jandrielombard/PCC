*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_VCAT...................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_VCAT                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_VCAT                 .
CONTROLS: TCTRL_ZUSE_PCC_VCAT
            TYPE TABLEVIEW USING SCREEN '0110'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_VCAT                 .
TABLES: ZUSE_PCC_VCAT                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
