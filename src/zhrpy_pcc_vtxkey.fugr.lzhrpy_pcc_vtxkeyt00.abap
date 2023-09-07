*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_VTXKEY................................*
TABLES: ZHRPY_PCC_VTXKEY, *ZHRPY_PCC_VTXKEY. "view work areas
CONTROLS: TCTRL_ZHRPY_PCC_VTXKEY
TYPE TABLEVIEW USING SCREEN '0101'.
DATA: BEGIN OF STATUS_ZHRPY_PCC_VTXKEY. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRPY_PCC_VTXKEY.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRPY_PCC_VTXKEY_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRPY_PCC_VTXKEY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRPY_PCC_VTXKEY_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRPY_PCC_VTXKEY_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRPY_PCC_VTXKEY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRPY_PCC_VTXKEY_TOTAL.

*.........table declarations:.................................*
TABLES: PYD_D_CLST                     .
TABLES: PYD_D_TYT                      .
TABLES: ZHRPY_PCC_TXTKEY               .
