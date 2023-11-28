*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_AUDTINF................................*
DATA:  BEGIN OF STATUS_ZUSE_PCC_AUDTINF              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_PCC_AUDTINF              .
CONTROLS: TCTRL_ZUSE_PCC_AUDTINF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZUSE_PCC_AUDTINF              .
TABLES: ZUSE_PCC_AUDTINF               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
