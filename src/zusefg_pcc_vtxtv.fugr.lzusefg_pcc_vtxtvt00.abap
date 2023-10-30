*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_VTEXTV.................................*
TABLES: ZUSE_PCC_VTEXTV, *ZUSE_PCC_VTEXTV. "view work areas
CONTROLS: TCTRL_ZUSE_PCC_VTEXTV
TYPE TABLEVIEW USING SCREEN '0111'.
DATA: BEGIN OF STATUS_ZUSE_PCC_VTEXTV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZUSE_PCC_VTEXTV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZUSE_PCC_VTEXTV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZUSE_PCC_VTEXTV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZUSE_PCC_VTEXTV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZUSE_PCC_VTEXTV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZUSE_PCC_VTEXTV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZUSE_PCC_VTEXTV_TOTAL.

*.........table declarations:.................................*
TABLES: PYD_D_TYT                      .
TABLES: ZUSE_PCC_VTEXT                 .
