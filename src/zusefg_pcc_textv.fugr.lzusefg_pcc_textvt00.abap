*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_TEXTV..................................*
TABLES: ZUSE_PCC_TEXTV, *ZUSE_PCC_TEXTV. "view work areas
CONTROLS: TCTRL_ZUSE_PCC_TEXTV
TYPE TABLEVIEW USING SCREEN '0222'.
DATA: BEGIN OF STATUS_ZUSE_PCC_TEXTV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZUSE_PCC_TEXTV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZUSE_PCC_TEXTV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZUSE_PCC_TEXTV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZUSE_PCC_TEXTV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZUSE_PCC_TEXTV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZUSE_PCC_TEXTV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZUSE_PCC_TEXTV_TOTAL.

*.........table declarations:.................................*
TABLES: PYD_D_CLST                     .
TABLES: PYD_D_TYT                      .
TABLES: ZUSE_PCC_TEXT                  .
