*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_WT_ATTRIBUTES_REP04
*&---------------------------------------------------------------------*
***********************************************************************
* INCLUDE:      ZHRAUREPY_WT_ATTRIBUTES_REP04                         *
* TITLE:        Wage Type Report                                      *
* AUTHOR:
* DATE:                                                               *
* R/3 RELEASE:  SAP R/3 Enterprise
***********************************************************************


FORM CREATE_FORM_CONTENT_TOL_ALV
        CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA:        LR_GRID        TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
               LR_LABEL       TYPE REF TO CL_SALV_FORM_LABEL,
               LR_TEXT        TYPE REF TO CL_SALV_FORM_TEXT.

  CREATE OBJECT LR_GRID.

  LR_GRID->CREATE_HEADER_INFORMATION(
   ROW    = 1
   COLUMN = 1
   TEXT   = 'Processing Classes, Cumulation and Evaluation Classes And Valuation Basis').

  LR_GRID->CREATE_LABEL(
    ROW    = 4
    COLUMN = 1
    TEXT   = MOLGAT ).
*
  CR_CONTENT = LR_GRID.

ENDFORM.                    "CREATE_FORM_CONTENT_TOL1_ALV
