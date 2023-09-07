*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_WT_ATTRIBUTES_REP02
*&---------------------------------------------------------------------*
***********************************************************************
* INCLUDE:      ZHRAUREPY_WT_ATTRIBUTES_REP02                         *
*
* TITLE:        Wage Type Report                                      *
* AUTHOR:
* DATE:                                                               *
* R/3 RELEASE:  SAP R/3 Enterprise
***********************************************************************

*====> SELECTION SCREEN ===============================================

SELECTION-SCREEN BEGIN OF BLOCK EINGABE
                 WITH FRAME TITLE TEXT-116.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-113.
PARAMETERS MODLGART LIKE T500L-MOLGA MEMORY ID MOL.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS SEL FOR LGART.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-114.
PARAMETERS BEGDAT LIKE T512W-BEGDA DEFAULT SY-DATUM.
SELECTION-SCREEN COMMENT 52(5) TEXT-115.
PARAMETERS ENDDAT LIKE T512W-ENDDA DEFAULT SY-DATUM.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK EINGABE.
SELECTION-SCREEN BEGIN OF BLOCK OPTION
                 WITH FRAME TITLE TEXT-210.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(46) TEXT-212.
*PARAMETERS: DETAIL LIKE RPDXXXXX-INH_VRZ RADIOBUTTON GROUP MOD1
*                        DEFAULT 'X'.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF BLOCK OPTIONAL
*                 WITH FRAME TITLE TEXT-110.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(40) TEXT-112.
PARAMETERS: WITHOWT LIKE RPDXXXXX-INH_VRZ RADIOBUTTON GROUP MOD
                        DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(40) TEXT-111.
PARAMETERS: WITHWT LIKE RPDXXXXX-INH_VRZ RADIOBUTTON GROUP MOD.
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK OPTIONAL.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(40) TEXT-211.
PARAMETERS: MATRIX LIKE RPDXXXXX-INH_VRZ RADIOBUTTON GROUP MOD.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(40) TEXT-311.
PARAMETERS: VALUATE LIKE RPDXXXXX-INH_VRZ RADIOBUTTON GROUP MOD.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK OPTION.
