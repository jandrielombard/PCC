*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_VTEXTV................................*
TABLES: ZHRPY_PCC_VTEXTV, *ZHRPY_PCC_VTEXTV. "view work areas
CONTROLS: TCTRL_ZHRPY_PCC_VTEXTV
TYPE TABLEVIEW USING SCREEN '0111'.
DATA: BEGIN OF STATUS_ZHRPY_PCC_VTEXTV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRPY_PCC_VTEXTV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRPY_PCC_VTEXTV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRPY_PCC_VTEXTV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRPY_PCC_VTEXTV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRPY_PCC_VTEXTV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRPY_PCC_VTEXTV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRPY_PCC_VTEXTV_TOTAL.

*.........table declarations:.................................*
TABLES: PYD_D_TYT                      .
TABLES: ZHRPY_PCC_VTEXT                .
