*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_VCATV..................................*
TABLES: ZUSE_PCC_VCATV, *ZUSE_PCC_VCATV. "view work areas
CONTROLS: TCTRL_ZUSE_PCC_VCATV
TYPE TABLEVIEW USING SCREEN '0108'.
DATA: BEGIN OF STATUS_ZUSE_PCC_VCATV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZUSE_PCC_VCATV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZUSE_PCC_VCATV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZUSE_PCC_VCATV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZUSE_PCC_VCATV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZUSE_PCC_VCATV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZUSE_PCC_VCATV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZUSE_PCC_VCATV_TOTAL.

*.........table declarations:.................................*
TABLES: PYD_D_TYT                      .
TABLES: ZUSE_PCC_VCAT                  .
