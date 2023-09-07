*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRPY_PCC_TEXTV.................................*
TABLES: ZHRPY_PCC_TEXTV, *ZHRPY_PCC_TEXTV. "view work areas
CONTROLS: TCTRL_ZHRPY_PCC_TEXTV
TYPE TABLEVIEW USING SCREEN '0222'.
DATA: BEGIN OF STATUS_ZHRPY_PCC_TEXTV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRPY_PCC_TEXTV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRPY_PCC_TEXTV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRPY_PCC_TEXTV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRPY_PCC_TEXTV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRPY_PCC_TEXTV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRPY_PCC_TEXTV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRPY_PCC_TEXTV_TOTAL.

*.........table declarations:.................................*
TABLES: PYD_D_CLST                     .
TABLES: PYD_D_TYT                      .
TABLES: ZHRPY_PCC_TEXT                 .
