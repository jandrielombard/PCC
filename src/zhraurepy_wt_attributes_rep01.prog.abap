*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_WT_ATTRIBUTES_REP01
*&---------------------------------------------------------------------*
***********************************************************************
* INCLUDE:      ZHRAUREPY_WT_ATTRIBUTES_REP01                         *
* TITLE:        Wage Type Report                                      *
* AUTHOR:
* DATE:                                                               *
* R/3 RELEASE:  SAP R/3 Enterprise
***********************************************************************

TABLES: T512T, T512W, T500L, T500P, T005T.

TABLES: T52D1,
        T52D2,
        T52D3,
        T52D4,
        T52D8,         T52D9,
        T52DA,         T52DB,
        T599I,         T599J,
        OBJSUBT,      " Object: Short description of subobject
        OBJSUB,       " Subobjects
        DSYAT,        " Structures: Texts for Maintenance Structure
        CUSAH,
        CUSAL,
        DD03L,
        DD04T,
        T511,
        T538T,
        Q511.

DATA:   LVNAM      LIKE T599I-LVNAM,
        PARVA      LIKE T599I-PARVA,
        PARAM      LIKE T599I-PARAM,
        SUBOBJNAME LIKE OBJSUBT-SUBOBJNAME,
        SIMG_TITLE LIKE DSYAT-CHILD_TITL.

DATA:  CHAR96(96)  TYPE C,             "char-feld fuer 96 char 0/1
       HEXIN(12)   TYPE X,             "hex feld  96 bit 0/1
       X128        TYPE X   VALUE '80',
       SWITCH      TYPE C,
       HEX         TYPE X,
       HEXTM       TYPE X,
       PS          TYPE P,
       PC          TYPE P.

DATA:  BEGIN OF AKL OCCURS 100,
         SPRSL LIKE T512U-SPRSL,
         KLNUM(2),
         KLWRT(2),
         KLTXT LIKE T512U-DTEXT,
       END OF AKL.
* itab for excel file
DATA:  BEGIN OF AKL1 OCCURS 100,
         LGART LIKE T512T-LGART,
         LGTXT LIKE T512T-LGTXT,
         KZTXT LIKE T512T-KZTXT,
         KLNUM(2),
         KLTXT LIKE T512U-DTEXT,
         KLWRT(2),
         KLTXT1 LIKE T512U-DTEXT,
       END OF AKL1.

DATA:  BEGIN OF VKL OCCURS 500,
         SPRSL LIKE T512U-SPRSL,
         KLNUM(2),
         KLWRT(2),
         KLTXT LIKE T512U-DTEXT,
       END OF VKL.
* itab for excel file
DATA:  BEGIN OF VKL1 OCCURS 500,
        LGART LIKE T512T-LGART,
        LGTXT LIKE T512T-LGTXT,
        KZTXT LIKE T512T-KZTXT,
        KLNUM(2),
        KLTXT LIKE T512U-DTEXT,
        KLWRT(2),
        KLTXT1 LIKE T512U-DTEXT,
      END OF VKL1.


DATA:  BEGIN OF BW OCCURS 500.
        INCLUDE STRUCTURE T512W.
DATA:    KUMULA LIKE CHAR96,
         KUMUFLAG(1),
         D1DURCH LIKE CHAR96(32),
         D2DURCH LIKE CHAR96(32),
         D3DURCH LIKE CHAR96(32),
         D4DURCH LIKE CHAR96(32),
         D5DURCH LIKE CHAR96(32),
         DURCHFLAG(1),
         AKLFLAG(1),
       END OF BW.

DATA:  BEGIN OF KU OCCURS 096,
         LGART LIKE T512T-LGART,
         LGTXT LIKE T512T-LGTXT,
       END OF KU.
* ITAB KU1 FOR EXCEL FILE-CUMULATIVE CLASSES
DATA:  BEGIN OF KU1 OCCURS 096,
         LGART LIKE T512T-LGART,
         LGTXT LIKE T512T-LGTXT,
         KZTXT LIKE T512T-KZTXT,
         KLNUM(2),
         LGART1 LIKE T512T-LGART,
         CHECK_BOX,
         LGTXT1 LIKE T512T-LGTXT,
       END OF KU1.

DATA:  BEGIN OF DG OCCURS 0,
         LGART LIKE T512T-LGART,
         LGTXT LIKE T512T-LGTXT,
       END OF DG.
* ITAB DG1 FOR EXCEL FILE-AVERAGE Bases
DATA:  BEGIN OF DG1 OCCURS 0,
         LGART LIKE T512T-LGART,
         LGTXT LIKE T512T-LGTXT,
         KZTXT LIKE T512T-KZTXT,
         KLNUM(2),
         LGART1 LIKE T512T-LGART,
         CHECK_BOX,
         LGTXT1 LIKE T512T-LGTXT,
       END OF DG1.

DATA:  BEGIN OF BG OCCURS 0,
         LGART LIKE T512T-LGART,
         LGTXT LIKE T512T-LGTXT,
       END OF BG.
* ITAB BG1 FOR EXCEL FILE-Valuation Bases
DATA:  BEGIN OF BG1 OCCURS 0,
         LGART LIKE T512T-LGART,
         LGTXT LIKE T512T-LGTXT,
         KZTXT LIKE T512T-KZTXT,
         SLNO TYPE I,
         KLNUM(2),
         KLTXT LIKE T512U-DTEXT,
         KLWRT(2),
         LGART1 LIKE T512T-LGART,
         LGTXT1 LIKE T512T-LGTXT,
       END OF BG1.
* Wage type Cheracterstics.
DATA:  BEGIN OF CH OCCURS 0,
         LGART LIKE T512T-LGART,
         COLNO(3) TYPE N,
         VALUE(20) TYPE C,
       END OF CH.
DATA: BEGIN OF LIST OCCURS 100,
        KLNUM(2),
        KLWRT LIKE T512T-LGART,
        LGTXT(72),
        LGART LIKE T512T-LGART,
        KZTXT LIKE T512T-KZTXT,
      END OF LIST.

DATA: BEGIN OF ITAB OCCURS 0,
        SRTSEQ(2) TYPE N,
        LGART TYPE LGART,
        KZTXT LIKE T512T-KZTXT,
        LGTXT LIKE T512T-LGTXT,
        CLASS(12) TYPE C,
        COL1(4) TYPE C,
        COL3(4) TYPE C,
        COL2 LIKE T52DA-EVCLT,
        COL4 LIKE T52DA-EVCLT,
      END OF ITAB.

DATA: BEGIN OF ITAB1 OCCURS 0,
        SRTSEQ(2) TYPE N,
        LGART TYPE LGART,
        KZTXT LIKE T512T-KZTXT,
        LGTXT LIKE T512T-LGTXT,
      END OF ITAB1.

DATA: BEGIN OF ITAB2 OCCURS 0,
      MOLGA LIKE T512W-MANDT,
      LGART LIKE T512W-LGART,
      LGTXT LIKE T512T-LGTXT,
      GVBLA LIKE T512W-GVBLA,
      GVALA LIKE T512W-GVALA,
      GVPRO LIKE T512W-GVPRO,
      PZBLA LIKE T512W-PZBLA,
      PZALA LIKE T512W-PZALA,
      PZPRO LIKE T512W-PZPRO,
      FZBLA LIKE T512W-FZBLA,
      FZALA LIKE T512W-FZALA,
      FZPRO LIKE T512W-FZPRO,
      BEGDA LIKE T512W-BEGDA,
      ENDDA LIKE T512W-ENDDA,
      END OF ITAB2.

DATA:  LISEL_1(79), LISEL_2(79), HEADER(79), HEADER_2(79).
DATA:  LGART LIKE T512W-LGART,
       INTCA LIKE T500L-INTCA,
       GS_T500L like T500L.

DATA:  MOLGAT LIKE T005T-LANDX.
FIELD-SYMBOLS: <COL>, <COLTEXT>.
FIELD-SYMBOLS: <DATATAB> TYPE TABLE.

* Matrix listing
DATA: EXCELDATA              TYPE REF TO DATA.
DATA: EXCELTABLE             TYPE REF TO DATA.
FIELD-SYMBOLS: <EXCELTAB>    TYPE TABLE,
               <REP_MAIN>    TYPE ANY,
               <REPMAINDESC> TYPE ANY.
DATA: BEGIN OF FS,
        F1(14) TYPE C VALUE '<REP_MAIN>-COL',
        NUM(4) TYPE N,
      END OF FS.
DATA: BEGIN OF FS1,
        EF1(3) TYPE C VALUE 'COL',
        NUM(4)  TYPE N,
      END OF FS1.
DATA: LOOPVAR(4)  TYPE N,
      LOOPVAR1(4) TYPE N,
      L_TRAILING_TEXT(1).
DATA: BEGIN OF WT_TAB OCCURS 0,
        LGART     LIKE T512W-LGART,
        LGTXT     LIKE T512T-LGTXT,
        KUMUL     LIKE T512W-KUMUL,
        COLNO(4)  TYPE N,
      END OF WT_TAB.
