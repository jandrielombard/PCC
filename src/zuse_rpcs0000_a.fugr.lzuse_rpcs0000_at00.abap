*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_RPCS0000_A.................................*
DATA:  BEGIN OF STATUS_ZUSE_RPCS0000_A               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZUSE_RPCS0000_A               .
CONTROLS: TCTRL_ZUSE_RPCS0000_A
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZUSE_RPCS0000_A               .
TABLES: ZUSE_RPCS0000_A                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
