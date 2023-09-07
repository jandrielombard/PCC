*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_WT_ATTRIBUTES_REP03
*&---------------------------------------------------------------------*
***********************************************************************
* INCLUDE:      ZHRAUREPY_WT_ATTRIBUTES_REP03
*                                                                     *
* TITLE:        Wage Type Report                                      *
* AUTHOR:
* DATE:                                                               *
* R/3 RELEASE:  SAP R/3 Enterprise
***********************************************************************

FORM matrix_table_creation.
*
  DATA: struct_type TYPE REF TO cl_abap_structdescr,
        comp_tab    TYPE cl_abap_structdescr=>component_table,
        comp        LIKE LINE OF comp_tab.
  DATA: fieldname(30) TYPE c.

  DATA: l_oref    TYPE REF TO cx_root,
        l_errtext TYPE string.

  TRY.
      CLEAR: comp, comp_tab, comp_tab[].

      comp-name = 'DESC'.
      fieldname = 'T512U-DTEXT'.
      comp-type ?= cl_abap_typedescr=>describe_by_name( fieldname ).
      APPEND comp TO comp_tab.

      LOOP AT wt_tab.
        fs1-num = wt_tab-colno.
        ASSIGN fs1 TO <col>.

        comp-name = fs1.
        fieldname = 'T512D-FNAME'.
        comp-type ?= cl_abap_typedescr=>describe_by_name( fieldname ).
        APPEND comp TO comp_tab.

      ENDLOOP.

      comp-name = 'COLINFO'.
      fieldname = 'LVC_T_SCOL'.
      comp-type ?= cl_abap_typedescr=>describe_by_name( fieldname ).
      APPEND comp TO comp_tab.


    CATCH cx_root INTO l_oref.
      l_errtext = l_oref->get_text( ).
      MESSAGE e016(pg) WITH l_errtext.
  ENDTRY.

  TRY.
      struct_type = cl_abap_structdescr=>create( comp_tab ).

      CREATE DATA exceldata TYPE HANDLE struct_type.
      ASSIGN exceldata->* TO <rep_main>.

      CREATE DATA exceltable LIKE STANDARD TABLE OF <rep_main>
                          WITH NON-UNIQUE DEFAULT KEY.
      ASSIGN exceltable->* TO <exceltab>.

    CATCH cx_root INTO l_oref.
      l_errtext = l_oref->get_text( ).
      MESSAGE e016(pg) WITH l_errtext.
  ENDTRY.

ENDFORM.                    "MATRIX_TABLE_CREATION
*----------------------------------------------------------------------*
*       FORM MAIN                                                      *
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM main.
**FILL INTERNAL TABLES
  DATA: lga          LIKE t512w-lgart,
        cumm_flag(1).
  PERFORM re512u.                      "AKL And VKLA
  PERFORM re512w.
  PERFORM re512t-ku.
  PERFORM re512t-dg.
  PERFORM re512t-bg.
  PERFORM re511t-ch.
  PERFORM endlos_list.
*  IF DETAIL = 'X '.
  IF matrix = ' ' AND valuate EQ ' '.
********* APPENDING ALL VALUES TO ITAB AND PASSING TO SPREAD SHEET *****
    SELECT * FROM t512w WHERE molga EQ modlgart AND lgart IN sel
                              AND begda LE enddat
                              AND endda GE begdat.

      SELECT SINGLE * FROM t512t WHERE lgart = t512w-lgart.
      IF sy-subrc <> 0.
      ENDIF.
******************* APPENDING PROCESSING CLASSES **********************
      SORT vkl1 BY lgart  klnum  klwrt.
      LOOP AT vkl1 WHERE lgart = t512w-lgart.
        itab-srtseq  = 1.
        itab-lgart = vkl1-lgart.
        itab-lgtxt = vkl1-lgtxt.
        itab-kztxt = vkl1-kztxt.
        itab-class = 'Processing'.
        itab-col1 =  vkl1-klnum.
        itab-col2 =  vkl1-kltxt.
        itab-col3 =  vkl1-klwrt.
        itab-col4 =  vkl1-kltxt1.
        APPEND itab.
      ENDLOOP.
***************** APPENDING EVALUTION CLASSES *************************
      SORT akl1 BY lgart  klnum  klwrt.
      LOOP AT akl1 WHERE lgart = t512w-lgart.
        itab-srtseq = 2.
        itab-lgart = akl1-lgart.
        itab-lgtxt = akl1-lgtxt.
        itab-kztxt = akl1-kztxt.
        itab-class = 'Evaluation'.
        itab-col1 = akl1-klnum.
        itab-col2 = akl1-kltxt.
        itab-col3 = akl1-klwrt.
        itab-col4 = akl1-kltxt1.
        APPEND itab.
      ENDLOOP.
**************APPENDING CUMULATIONS***********************
      SORT ku1 BY lgart klnum lgart1.
      LOOP AT ku1 WHERE lgart = t512w-lgart.
        itab-srtseq = 3.
        itab-lgart = ku1-lgart.
        itab-lgtxt = ku1-lgtxt.
        itab-kztxt = ku1-kztxt.
        itab-class = 'Cumulation'.
        itab-col1 = ku1-klnum.
        itab-col2 = ku1-lgart1.
        itab-col3 = ku1-check_box.
        itab-col4 = ku1-lgtxt1.
        APPEND itab.
      ENDLOOP.
*************Average Bases********************************
      SORT dg1 BY lgart  klnum  lgart1.
      LOOP AT dg1 WHERE lgart = t512w-lgart.
        itab-srtseq  = 4.
        itab-lgart = dg1-lgart.
        itab-lgtxt = dg1-lgtxt.
        itab-kztxt = dg1-kztxt.
        itab-class = 'Average'.
        itab-col1 =  dg1-klnum.
        itab-col2 =  dg1-lgart1.
        itab-col3 =  dg1-check_box.
        itab-col4 =  dg1-lgtxt1.
        APPEND itab.
      ENDLOOP.
*************Valuation Bases********************************
      SORT bg1 BY lgart  slno  klnum.
      LOOP AT bg1 WHERE lgart = t512w-lgart.
        itab-srtseq  = 5.
        itab-lgart = bg1-lgart.
        itab-lgtxt = bg1-lgtxt.
        itab-kztxt = bg1-kztxt.
        itab-class = 'Valuation'.
        itab-col1 =  bg1-klwrt.
        itab-col2 =  bg1-kltxt.
        itab-col3 =  bg1-klnum.
        itab-col4 =  bg1-lgtxt1.
        APPEND itab.
      ENDLOOP.
    ENDSELECT.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.
* Create output list for matrix listing.
  IF matrix = 'X'.

********* APPENDING ALL VALUES TO ITAB AND PASSING TO SPREAD SHEET *****
    loopvar = 0.
    SELECT * FROM t512w WHERE molga EQ modlgart AND lgart IN sel
                              AND begda LE enddat
                              AND endda GE begdat
                         ORDER BY lgart ASCENDING .
      ADD 1 TO loopvar.
      SELECT SINGLE * FROM t512t WHERE molga = modlgart
                                   AND sprsl = sy-langu
                                   AND lgart = t512w-lgart.
      IF sy-subrc = 0.
        MOVE: t512w-lgart TO wt_tab-lgart,
              t512w-kumul TO wt_tab-kumul,
              t512t-lgtxt TO wt_tab-lgtxt,
              loopvar TO wt_tab-colno.
        APPEND wt_tab.
      ENDIF.
    ENDSELECT.
    IF sy-subrc <> 0.
    ENDIF.
    PERFORM: matrix_table_creation.
******************* APPENDING Wage type Characteristics ***************
    CLEAR: <rep_main>.
    ASSIGN COMPONENT 'DESC' OF STRUCTURE <rep_main> TO <repmaindesc>.
    MOVE: 'Wage Type Characteristics (T511)' TO <repmaindesc>.
    PERFORM set_header_color.
    APPEND: <rep_main> TO <exceltab>.CLEAR: <rep_main>.

    SELECT * FROM dd03l WHERE tabname = 'T511'
                          AND fieldname NE '.INCLUDE'
                          AND position GE 8
                          ORDER BY position ASCENDING .

      SELECT SINGLE ddtext INTO <repmaindesc> FROM dd04t
                           WHERE rollname = dd03l-rollname
                              AND ddlanguage = sy-langu.
      IF sy-subrc <> 0.
      ENDIF.
      CASE dd03l-position.
        WHEN 8.
          MOVE ' Deduction Wage type' TO <repmaindesc>.
        WHEN OTHERS.
      ENDCASE.
      IF dd03l-position > 10.
        ADD 1 TO dd03l-position.
      ENDIF.
      SORT ch BY lgart colno.
      LOOP AT wt_tab.
        fs-num = wt_tab-colno.
        ASSIGN (fs) TO <col>.
        READ TABLE ch WITH KEY lgart = wt_tab-lgart
                               colno = dd03l-position BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE: ch-value TO <col>.
        ELSE.
          MOVE: space TO <col>.
        ENDIF.
        UNASSIGN <col>.
      ENDLOOP.
      IF dd03l-position = 10.
        MOVE 'Input combination amount' TO <repmaindesc>.
        APPEND <rep_main> TO <exceltab>.
        CLEAR: <rep_main>.
        MOVE 'Input combination for number/unit' TO <repmaindesc>.
        LOOP AT wt_tab.
          fs-num = wt_tab-colno.
          ASSIGN (fs) TO <col>.
          READ TABLE ch WITH KEY lgart = wt_tab-lgart
                                 colno = 11 BINARY SEARCH.
          IF sy-subrc = 0.
            MOVE: ch-value TO <col>.
          ELSE.
            MOVE: space TO <col>.
          ENDIF.
          UNASSIGN <col>.
        ENDLOOP.
      ENDIF.
      APPEND <rep_main> TO <exceltab>. CLEAR: <rep_main>.
    ENDSELECT.
    IF sy-subrc <> 0.
    ENDIF.

***************** APPENDING Valuation Basis *************************
    CLEAR: <rep_main>.
    MOVE: 'Valuation Basis (V_512W_B)' TO <repmaindesc>.
    PERFORM set_header_color.
    APPEND: <rep_main> TO <exceltab>. CLEAR: <rep_main>.

    SELECT * FROM dd03l WHERE tabname = 'T512W'
                           AND position GE 6 AND position LE 14
                           ORDER BY position ASCENDING .

      SELECT SINGLE ddtext INTO <repmaindesc> FROM dd04t
                           WHERE rollname = dd03l-rollname
                           AND ddlanguage = sy-langu.
      IF sy-subrc <> 0.
      ENDIF.
*      MOVE DD04T-DDTEXT TO <REPMAINDESC>.
      LOOP AT wt_tab.
        fs-num = wt_tab-colno.
        ASSIGN (fs) TO <col>.
        SELECT SINGLE * FROM t512w WHERE molga = modlgart AND lgart = wt_tab-lgart.
        IF sy-subrc = 0.
          CASE dd03l-fieldname.
            WHEN 'GVBLA'.
              MOVE: t512w-gvbla TO <col>.
            WHEN 'GVALA'.
              MOVE: t512w-gvala TO <col>.
            WHEN 'GVPRO'.
              MOVE: t512w-gvpro TO <col>.
            WHEN 'PZBLA'.
              MOVE: t512w-pzbla TO <col>.
            WHEN 'PZALA'.
              MOVE: t512w-pzala TO <col>.
            WHEN 'PZPRO'.
              MOVE: t512w-pzpro TO <col>.
            WHEN 'FZBLA'.
              MOVE: t512w-fzbla TO <col>.
            WHEN 'FZALA'.
              MOVE: t512w-fzala TO <col>.
            WHEN 'FZPRO'.
              MOVE: t512w-fzpro TO <col>.
            WHEN OTHERS.
          ENDCASE.
*          MOVE: SPACE TO <COL>.
          UNASSIGN <col>.
        ENDIF.
      ENDLOOP.
      APPEND <rep_main> TO <exceltab>. CLEAR: <rep_main>.
    ENDSELECT.
    IF sy-subrc <> 0.
    ENDIF.

******************* APPENDING PROCESSING CLASSES ***********************
    CLEAR: <rep_main>.
    MOVE: 'PAYROLL PROCESSING' TO <repmaindesc>.
    PERFORM set_header_color1.
    APPEND: <rep_main> TO <exceltab>.CLEAR: <rep_main>.

    CLEAR: <rep_main>.
    MOVE: 'Processing Classes (V_512W_D)' TO <repmaindesc>.
    PERFORM set_header_color.
    APPEND: <rep_main> TO <exceltab>.

    SELECT * FROM t52d1 WHERE molga EQ modlgart.
      CLEAR: <rep_main>.
      SELECT SINGLE * FROM  t52d8
             WHERE  sprsl  = sy-langu
             AND    molga  = modlgart
             AND    prcls  = t52d1-prcls.
      IF sy-subrc = 0.
        SPLIT t52d8-prclt AT '-->' INTO <repmaindesc>
                               l_trailing_text.
      ENDIF.
      CONCATENATE t52d1-prcls <repmaindesc>
      INTO <repmaindesc> SEPARATED BY space.
      LOOP AT wt_tab.
        fs-num = wt_tab-colno.
        ASSIGN (fs) TO <col>.
        SORT vkl1 BY lgart klnum.
        READ TABLE vkl1 WITH KEY lgart = wt_tab-lgart
                                 klnum = t52d1-prcls BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE: vkl1-klwrt TO <col>.
        ELSE.
          MOVE: space TO <col>.
        ENDIF.
        UNASSIGN <col>.
      ENDLOOP.
      APPEND <rep_main> TO <exceltab>. CLEAR: <rep_main>.
    ENDSELECT.
    IF sy-subrc <> 0.
    ENDIF.
**************APPENDING CUMULATIONS***********************
    CLEAR: <rep_main>.
    MOVE: 'Cumulation Classes (V_512W_D)' TO <repmaindesc>.
    PERFORM set_header_color.
    APPEND: <rep_main> TO <exceltab>. CLEAR: <rep_main>.

    loopvar1 = 0.
    DO 96 TIMES.
      loopvar1  = loopvar1 + 1.
      lga(2)   = '/1'.
      lga+2(2) = loopvar1.
      CLEAR: <rep_main>.
      PERFORM get_lga_text
              USING '/1' loopvar1 <repmaindesc>.
      IF NOT <repmaindesc> IS INITIAL.
        LOOP AT wt_tab.
          fs-num = wt_tab-colno.
          ASSIGN (fs) TO <col>.
          CLEAR: cumm_flag.
          PERFORM get_bit
                  USING wt_tab-kumul loopvar1 cumm_flag.
          IF cumm_flag = 'X'.
            MOVE: 'Y' TO <col>.
          ELSE.
            MOVE: space TO <col>.
          ENDIF.
          UNASSIGN <col>.
        ENDLOOP.
        APPEND <rep_main> TO <exceltab>. CLEAR: <rep_main>.
      ENDIF.
    ENDDO.
***************** APPENDING EVALUTION CLASSES *************************
    CLEAR: <rep_main>.
    MOVE: 'Evaluation Classes (V_512W_D)' TO <repmaindesc>.
    PERFORM set_header_color.
    APPEND: <rep_main> TO <exceltab>. CLEAR: <rep_main>.


    SELECT * FROM t52d3 WHERE molga EQ modlgart.
      CLEAR: <rep_main>.
      SELECT SINGLE * FROM  t52da
             WHERE  sprsl  = sy-langu
             AND    molga  = modlgart
             AND    evcls  = t52d3-evcls.
      IF sy-subrc = 0.
        SPLIT t52da-evclt AT '-->' INTO <repmaindesc>
                               l_trailing_text.
      ENDIF.
      CONCATENATE t52d3-evcls <repmaindesc>
      INTO <repmaindesc> SEPARATED BY space.
      LOOP AT wt_tab.
        fs-num = wt_tab-colno.
        ASSIGN (fs) TO <col>.
        SORT akl1 BY lgart klnum.
        READ TABLE akl1 WITH KEY lgart = wt_tab-lgart
                                 klnum = t52d3-evcls BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE: akl1-klwrt TO <col>.
        ELSE.
          MOVE: space TO <col>.
        ENDIF.
        UNASSIGN <col>.
      ENDLOOP.
      APPEND <rep_main> TO <exceltab>. CLEAR: <rep_main>.
    ENDSELECT.
    IF sy-subrc <> 0.
    ENDIF.

***************** APPENDING Valuation Basis *************************
*   CLEAR: <REP_MAIN>.
*   MOVE: 'Valuation Basis (V_512W_B)' TO <REPMAINDESC>.
*   PERFORM SET_HEADER_COLOR.
*   APPEND: <REP_MAIN> TO <EXCELTAB>. CLEAR: <REP_MAIN>.
*
*   SELECT * FROM DD03L WHERE TABNAME = 'T512W'
*                          AND POSITION GE 6 AND POSITION LE 14
*                          ORDER BY POSITION ASCENDING .
*
*      SELECT SINGLE DDTEXT INTO <REPMAINDESC> FROM DD04T
*                           WHERE ROLLNAME = DD03L-ROLLNAME
*                           AND DDLANGUAGE = SY-LANGU.
**      MOVE DD04T-DDTEXT TO <REPMAINDESC>.
*      LOOP AT WT_TAB.
*          FS-NUM = WT_TAB-COLNO.
*          ASSIGN (FS) TO <COL>.
*       SELECT SINGLE * FROM T512W WHERE MOLGA = MODLGART AND LGART = WT_TAB-LGART.
*         IF SY-SUBRC = 0.
*          CASE DD03L-FIELDNAME.
*          WHEN 'GVBLA'.
*          MOVE: T512W-GVBLA TO <COL>.
*          WHEN 'GVALA'.
*          MOVE: T512W-GVALA TO <COL>.
*          WHEN 'GVPRO'.
*          MOVE: T512W-GVPRO TO <COL>.
*          WHEN 'PZBLA'.
*          MOVE: T512W-PZBLA TO <COL>.
*          WHEN 'PZALA'.
*          MOVE: T512W-PZALA TO <COL>.
*          WHEN 'PZPRO'.
*          MOVE: T512W-PZPRO TO <COL>.
*          WHEN 'FZBLA'.
*          MOVE: T512W-FZBLA TO <COL>.
*          WHEN 'FZALA'.
*          MOVE: T512W-FZALA TO <COL>.
*          WHEN 'FZPRO'.
*          MOVE: T512W-FZPRO TO <COL>.
*          ENDCASE.
**          MOVE: SPACE TO <COL>.
*         UNASSIGN <COL>.
*        ENDIF.
*       ENDLOOP.
*      APPEND <REP_MAIN> TO <EXCELTAB>. CLEAR: <REP_MAIN>.
*    ENDSELECT.
  ENDIF.

  IF valuate = 'X'.
    CLEAR itab2. REFRESH itab2.
*  SELECT * FROM T512W INTO CORRESPONDING FIELDS OF TABLE ITAB2
*                      WHERE MOLGA EQ MODLGART
*                      AND LGART IN SEL
*                      AND BEGDA LE BEGDAT AND ENDDA GT ENDDAT.

    SELECT molga lgart gvbla gvala gvpro pzbla pzala pzpro fzbla fzala fzpro begda endda
       FROM t512w INTO CORRESPONDING FIELDS OF TABLE itab2
                        WHERE molga EQ modlgart
                        AND lgart IN sel
                        AND begda LE begdat AND endda GT enddat.

    IF sy-subrc = 0.
      COLLECT itab2.
    ENDIF.

    LOOP AT itab2.
      SELECT SINGLE lgtxt FROM t512t INTO itab2-lgtxt WHERE lgart EQ itab2-lgart  AND molga EQ modlgart.
      IF sy-subrc = 0.
        MODIFY itab2.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                               "end of MAIN
*---------------------------------------------------------------------*
*       FORM RE512U                                                   *
*---------------------------------------------------------------------*
*    EVALUTION CLASS DATA FILL INTO TABLE AKL                         *
*---------------------------------------------------------------------*
FORM re512u.

  DATA: lt_t52d3 TYPE TABLE OF t52d3,
        ls_t52d3 TYPE t52d3,
        lt_t52da TYPE TABLE OF t52da,
        ls_t52da TYPE t52da,
        lt_t52d4 TYPE TABLE OF t52d4,
        ls_t52d4 TYPE t52d4,
        lt_t52db TYPE TABLE OF t52db,
        ls_t52db TYPE t52db,
        lt_t52d1 TYPE TABLE OF t52d1,
        ls_t52d1 TYPE t52d1,
        lt_t52d2 TYPE TABLE OF t52d2,
        ls_t52d2 TYPE t52d2,
        lt_t52d8 TYPE TABLE OF t52d8,
        ls_t52d8 TYPE t52d8,
        lt_t52d9 TYPE TABLE OF t52d9,
        ls_t52d9 TYPE t52d9.

  SELECT * FROM  t52da INTO TABLE lt_t52da
           WHERE  sprsl  = sy-langu
           AND    molga  = modlgart.
  IF sy-subrc = 0.
    SORT lt_t52da BY evcls.
  ENDIF.

  SELECT * FROM  t52db INTO TABLE lt_t52db
           WHERE  sprsl  = sy-langu
           AND    molga  = modlgart.
  IF sy-subrc = 0.
    SORT lt_t52db BY evcls evclv.
  ENDIF.

  SELECT * FROM t52d4 INTO TABLE lt_t52d4
           WHERE  molga  = modlgart.
  IF sy-subrc = 0.
    SORT lt_t52d4 BY evcls.
  ENDIF.

  SELECT * FROM  t52d3 INTO TABLE lt_t52d3
         WHERE  molga = modlgart.

  IF sy-subrc = 0.
    LOOP AT lt_t52d3 INTO ls_t52d3.
      akl-klnum  = ls_t52d3-evcls.
      akl-klwrt  = ' '.
      akl-kltxt  = ' '.
*    SELECT * FROM  T52DA
*
*           WHERE  SPRSL  = SY-LANGU
*           AND    MOLGA  = MODLGART
*           AND    EVCLS  = T52D3-EVCLS.
*      AKL-KLTXT = T52DA-EVCLT.
*    ENDSELECT.
      CLEAR ls_t52da.
      READ TABLE lt_t52da INTO ls_t52da WITH KEY evcls  = ls_t52d3-evcls BINARY SEARCH.
      IF sy-subrc = 0.
        akl-kltxt = ls_t52da-evclt.
      ENDIF.
      APPEND akl.

*      SELECT * FROM T52D4
*             WHERE  MOLGA  = MODLGART
*             AND    EVCLS  = T52D3-EVCLS.
      CLEAR ls_t52d4.
      LOOP AT lt_t52d4 INTO ls_t52d4 WHERE evcls = ls_t52d3-evcls.
        akl-klnum  = ls_t52d3-evcls.
        akl-klwrt  = ls_t52d4-evclv.
        akl-kltxt  = ' '.

*        SELECT * FROM  T52DB
*               WHERE  SPRSL  = SY-LANGU
*               AND    MOLGA  = MODLGART
*               AND    EVCLS  = T52D3-EVCLS
*               AND    EVCLV  = T52D4-EVCLV.
*          AKL-KLTXT = T52DB-EVCVT.
*        ENDSELECT.
        CLEAR ls_t52db.
        READ TABLE lt_t52db INTO ls_t52db WITH KEY evcls  = ls_t52d3-evcls
          evclv  = t52d4-evclv BINARY SEARCH.
        IF sy-subrc = 0.
          akl-kltxt = ls_t52db-evcvt.
        ENDIF.
        APPEND akl.

*    ENDSELECT
        CLEAR ls_t52d4..
      ENDLOOP.
      CLEAR ls_t52d3.
    ENDLOOP.
*  ENDSELECT.
  ENDIF.

  SORT akl BY klnum klwrt.

*---------------------------------------------------------------------*
*  PEOCESSING CLASS FILL INTO  VKL               *
*---------------------------------------------------------------------*
  SELECT * FROM  t52d1 INTO TABLE lt_t52d1
         WHERE  molga = modlgart.
  IF sy-subrc <> 0.
  ENDIF.

  SELECT * FROM t52d2 INTO TABLE lt_t52d2
           WHERE  molga  = modlgart.
  IF sy-subrc = 0.
    SORT lt_t52d2 BY prcls.
  ENDIF.

  SELECT * FROM  t52d9 INTO TABLE lt_t52d9
           WHERE  sprsl  = sy-langu
           AND    molga  = modlgart.
  IF sy-subrc = 0.
    SORT lt_t52d9 BY prcls prclv.
  ENDIF.

  SELECT * FROM  t52d8 INTO TABLE lt_t52d8
         WHERE  sprsl  = sy-langu
         AND    molga  = modlgart.
  IF sy-subrc = 0.
    SORT lt_t52d8 BY prcls.
  ENDIF.

  CLEAR ls_t52d1.
  LOOP AT lt_t52d1 INTO ls_t52d1.
    vkl-klnum  = ls_t52d1-prcls.
    vkl-klwrt  = ' '.
    vkl-kltxt  = ' '.

*    SELECT * FROM  T52D8
*           WHERE  SPRSL  = SY-LANGU
*           AND    MOLGA  = MODLGART
*           AND    PRCLS  = T52D1-PRCLS.
*      VKL-KLTXT = T52D8-PRCLT.
*    ENDSELECT.
    CLEAR ls_t52d8.
    READ TABLE lt_t52d8 INTO ls_t52d8 WITH KEY prcls  = ls_t52d1-prcls BINARY SEARCH.
    IF sy-subrc = 0.
      vkl-kltxt = ls_t52d8-prclt.
    ENDIF.
    APPEND vkl.

    CLEAR ls_t52d2.
    LOOP AT lt_t52d2 INTO ls_t52d2.
*    SELECT * FROM T52D2
*           WHERE  MOLGA  = MODLGART
*           AND    PRCLS  = T52D1-PRCLS.
      vkl-klnum  = ls_t52d1-prcls.
      vkl-klwrt  = ls_t52d2-prclv.
      vkl-kltxt  = ' '.

*      SELECT * FROM  T52D9
*             WHERE  SPRSL  = SY-LANGU
*             AND    MOLGA  = MODLGART
*             AND    PRCLS  = T52D1-PRCLS
*             AND    PRCLV  = T52D2-PRCLV.
      CLEAR ls_t52d9.
      READ TABLE lt_t52d9 INTO ls_t52d9 WITH KEY prcls  = ls_t52d1-prcls
           prclv  = ls_t52d2-prclv BINARY SEARCH.
      IF sy-subrc = 0.
        vkl-kltxt = ls_t52d9-prcvt.
      ENDIF.
      CLEAR ls_t52d2.
    ENDLOOP.
*      ENDSELECT.

    IF NOT ( vkl-klwrt = space ).
      APPEND vkl.
    ENDIF.
*    ENDSELECT.

*  ENDSELECT.
    CLEAR ls_t52d1.
  ENDLOOP.
  SORT vkl.

ENDFORM.                                                    "re512u

*---------------------------------------------------------------------*
*       FORM RE512T-KU                                                *
*---------------------------------------------------------------------*
FORM re512t-ku.
  SELECT * FROM t512t WHERE sprsl EQ sy-langu
                        AND molga EQ modlgart
                        AND lgart LIKE '/1%'.
    CHECK t512t-lgart+2(2) CO '0123456789'.
    ku-lgart = t512t-lgart.
    ku-lgtxt = t512t-lgtxt.
    APPEND  ku.
  ENDSELECT.
  IF sy-subrc = 0.
  ENDIF.
ENDFORM.                               "end of RE512T-KU.

*---------------------------------------------------------------------*
*       FORM RE512T-DG                                                *
*---------------------------------------------------------------------*
FORM re512t-dg.
  SELECT * FROM t512t WHERE sprsl EQ sy-langu
                        AND molga EQ modlgart
                        AND lgart LIKE '/2%'.
    CHECK t512t-lgart+2(1) CO '0123'.
    CHECK t512t-lgart+2(2) CO '0123456789'.
    IF t512t-lgart+2(1) EQ '3' AND t512t-lgart+3(1) CN '012'.
      CHECK 0 EQ 1.
    ENDIF.
    dg-lgart = t512t-lgart.
    dg-lgtxt = t512t-lgtxt.
    APPEND  dg.
  ENDSELECT.
  IF sy-subrc <> 0.
  ENDIF.
ENDFORM.                               "end of RE512T-DG.

*---------------------------------------------------------------------*
*       FORM RE512T-BG                                                *
*---------------------------------------------------------------------*
FORM re512t-bg.
  DATA alphnum(36) TYPE c.
  CONCATENATE sy-abcde '0123456789' INTO alphnum.

  SELECT * FROM t512t WHERE sprsl EQ sy-langu
                        AND molga EQ modlgart
                        AND lgart LIKE    '/0%'.
    CHECK t512t-lgart+2(1) CO alphnum.
    bg-lgart = t512t-lgart.
    bg-lgtxt = t512t-lgtxt.
    APPEND  bg.
  ENDSELECT.
  IF sy-subrc <> 0.
  ENDIF.
  bg = space.
  bg-lgart+2(2) = 'K '.
  bg-lgtxt = text-016.
  APPEND  bg.
  bg-lgart+2(2) = 'T '.
  bg-lgtxt = text-017.
  APPEND  bg.
  bg-lgart+2(2) = 'TS'.
  bg-lgtxt = text-018.
  APPEND  bg.
  bg-lgart+2(2) = 'TG'.
  bg-lgtxt = text-019.
  APPEND  bg.
ENDFORM.                               "end of RE512T-BG.

*---------------------------------------------------------------------*
*       FORM RE512W                                                   *
*---------------------------------------------------------------------*
FORM re512w.
  SELECT * FROM t512w WHERE molga EQ modlgart AND lgart IN sel.
    IF t512w-endda GE begdat AND t512w-begda LE enddat.
      MOVE t512w TO bw.
      IF bw-aklas IS INITIAL. bw-aklflag = 'X'. ENDIF.
      CLEAR: char96, hexin.
      hexin = t512w-kumul.
      PERFORM hexinchar USING 12.
      bw-kumula = char96.
      IF NOT bw-kumula IS INITIAL.
        bw-kumuflag = 'X'.
      ENDIF.
      CLEAR: char96, hexin.
      hexin(4) = t512w-d1kum.
      PERFORM hexinchar USING 4.
      bw-d1durch = char96(32).
      IF NOT bw-d1durch IS INITIAL. bw-durchflag = 'X'. ENDIF.
      CLEAR: char96, hexin.
      hexin(4) = t512w-d2kum.
      PERFORM hexinchar USING 4.
      bw-d2durch = char96(32).
      IF NOT bw-d2durch IS INITIAL. bw-durchflag = 'X'. ENDIF.
      CLEAR: char96, hexin.
      hexin(4) = t512w-d3kum.
      PERFORM hexinchar USING 4.
      bw-d3durch = char96(32).
      IF NOT bw-d3durch IS INITIAL. bw-durchflag = 'X'. ENDIF.
      CLEAR: char96, hexin.
      hexin(4) = t512w-d4kum.
      PERFORM hexinchar USING 4.
      bw-d4durch = char96(32).
      IF NOT bw-d4durch IS INITIAL. bw-durchflag = 'X'. ENDIF.
      CLEAR: char96, hexin.
      hexin(4) = t512w-d5kum.
      PERFORM hexinchar USING 4.
      bw-d5durch = char96(32).
      IF NOT bw-d5durch IS INITIAL. bw-durchflag = 'X'. ENDIF.
      APPEND bw.
    ENDIF.
  ENDSELECT.
  IF sy-subrc = 0.
  ENDIF.
ENDFORM.                               "end of RE512W.
*---------------------------------------------------------------------*
*       FORM RE511T-CH                                                *
*---------------------------------------------------------------------*
FORM re511t-ch.
  DATA: BEGIN OF itab OCCURS 0,
*000001 Type X not allowed in Unicode programs with CHAR
*           word(20) type x,
          word(20) TYPE c,
*000001 End ***********************************************************
        END OF itab.

  DATA: temp(4096)   TYPE c,
*000001 Type X not allowed in Unicode programs with CHAR
*       delimiter type x value '09'.
        delimiter(1) TYPE c VALUE cl_abap_char_utilities=>vertical_tab.
*000001 End ***********************************************************
  DATA: d_t511_anmin(15) TYPE c,
        d_t511_anmax(15) TYPE c,
        d_kombi(10)      TYPE c.

  DATA: lt_t511  TYPE TABLE OF t511,
        ls_t511  TYPE t511,
        lt_t538t TYPE TABLE OF t538t,
        ls_t538t TYPE t538t.

  SELECT * FROM  t538t INTO TABLE lt_t538t
             WHERE  sprsl  = sy-langu.
  IF sy-subrc = 0.
    SORT lt_t538t BY zeinh.
  ENDIF.

  SELECT * FROM t511 INTO TABLE lt_t511 WHERE molga EQ modlgart AND lgart IN sel
                       AND begda LE enddat
                       AND endda GE begdat
                      ORDER BY lgart ASCENDING .
  IF sy-subrc = 0.
    CLEAR ls_t511.
    LOOP AT lt_t511 INTO ls_t511.

      WRITE: ls_t511-anmin TO d_t511_anmin NO-GROUPING,
              ls_t511-anmax TO d_t511_anmax NO-GROUPING.

      CLEAR: ls_t538t.
*      IF ls_T511-ZEINH NE SPACE.
*        SELECT SINGLE * FROM  T538T
*               WHERE  SPRSL  = SY-LANGU
*               AND    ZEINH  = T511-ZEINH.
      READ TABLE lt_t538t INTO ls_t538t WITH KEY zeinh  = ls_t511-zeinh BINARY SEARCH.
      IF sy-subrc <> 0.
      ENDIF.
*    endif.
      CLEAR: q511.
      PERFORM kombi.
      CONCATENATE  ls_t511-mandt
                   ls_t511-molga
                   ls_t511-lgart
                   ls_t511-endda
                   ls_t511-begda
                   ls_t511-abtyz
                   ls_t511-wktyz
                   ls_t511-opken
                   ls_t538t-etext
                   q511-kbbtr
                   q511-kbanz
                   ls_t511-btmin
                   ls_t511-btmax
                   ls_t511-adsum
                   ls_t511-modna
                   ls_t511-modsp
                   ls_t511-rutyp
                   ls_t511-rudiv
                   ls_t511-addjn
                   ls_t511-mod01
                   ls_t511-mod02
                   ls_t511-mod03
                   d_t511_anmin
                   d_t511_anmax
              INTO temp
              SEPARATED BY delimiter.

      SPLIT temp AT delimiter INTO TABLE itab.

      LOOP AT itab.
* Change value's to the display format.
        CLEAR: ch.
        CASE sy-tabix.
          WHEN 8.
            IF itab-word = 'A'.
              MOVE 'Y' TO ch-value.
            ELSE.
              MOVE 'N' TO ch-value.
            ENDIF.
          WHEN 12 OR 13.
            IF itab-word = '000000000'.
              MOVE space TO ch-value.
            ENDIF.
          WHEN 14.
            IF itab-word = 'X'.
              MOVE 'Y' TO ch-value.
            ELSE.
              MOVE 'N' TO ch-value.
            ENDIF.
          WHEN 18.
            IF itab-word = '000000'.
              MOVE space TO ch-value.
            ENDIF.
          WHEN 21.
            IF itab-word = 'F'.
              MOVE 'N' TO ch-value.
            ELSEIF itab-word = ' ' OR itab-word = 'O'.
              MOVE 'Y' TO ch-value.
            ENDIF.
          WHEN 23 OR 24.
            IF itab-word = '0.00'.
              MOVE space TO ch-value.
            ENDIF.
          WHEN OTHERS.
            MOVE itab-word TO ch-value.
        ENDCASE.

        MOVE: ls_t511-lgart TO ch-lgart,
              sy-tabix TO ch-colno.
        APPEND ch.
      ENDLOOP.

      CLEAR ls_t511.
    ENDLOOP.
  ENDIF.
*  ENDSELECT.
*  if sy-subrc <> 0.
*  endif.
ENDFORM.                               "end of RE512T-KU.

*----------------------------------------------------------------------*
*       FORM ENDLOS_LIST                                               *
*----------------------------------------------------------------------*
*       DISPLAYING LIST                    *
*----------------------------------------------------------------------*
FORM endlos_list.

* PROCESSING CLASSES
  LOOP AT vkl.
    IF vkl-klwrt EQ space.
      vkl1-klnum = vkl-klnum.
      vkl1-kltxt =  vkl-kltxt .
    ENDIF.

    AT END OF klnum.
      PERFORM sc512w-vkl.
      PERFORM liste_5_vkl.
    ENDAT.
  ENDLOOP.

* EVALUATION CLASSES
  LOOP AT akl.
    IF akl-klwrt EQ space.
      akl1-klnum = akl-klnum.
      akl1-kltxt =  akl-kltxt.
    ENDIF.

    AT END OF klnum.
      PERFORM sc512w-akl.
      PERFORM liste_5_akl.
    ENDAT.
  ENDLOOP.
*CUMULATIVE CLASSES
  LOOP AT ku.
    ku1-lgart1 = ku-lgart .
    ku1-lgtxt1 = ku-lgtxt .
    PERFORM sc512w-ku.
    PERFORM liste_5_ku.
  ENDLOOP.
*Average Bases
  LOOP AT dg.
    dg1-lgart1 = dg-lgart.
    dg1-lgtxt1 = dg-lgtxt.
    PERFORM sc512w-dg.
    PERFORM liste_5_dg.
  ENDLOOP.
* Valuation Bases.
  LOOP AT bg.
    bg1-lgart1 = bg-lgart.
    bg1-lgtxt1 = bg-lgtxt.
    PERFORM sc512w-bg.
    PERFORM liste_5_bg.
  ENDLOOP.
ENDFORM.                    "endlos_list

*---------------------------------------------------------------------*
*       FORM SC512W-AKL                                               *
*---------------------------------------------------------------------*
FORM sc512w-akl.
  REFRESH list.
  CLEAR list.
  LOOP AT bw.
    IF bw-aklflag IS INITIAL.
      MOVE-CORRESPONDING bw TO t512w.
      ps = ( akl-klnum - 1 ) * 2.
      SHIFT t512w-aklas BY ps PLACES.
      IF t512w-aklas(2) NE space.
        CASE akl-klwrt.
          WHEN space.                    "CURSOR-SELECTION ON KLNUM ONLY
            PERFORM prlgart USING akl-klnum t512w-aklas(2).
          WHEN '**'.                     "AT END OF KLNUM
            PERFORM prlgart USING akl-klnum t512w-aklas(2).
          WHEN OTHERS.                  "CURSOR-SELECTION ON KLNUM KLWRT
            IF akl-klwrt EQ t512w-aklas(2).
              PERFORM prlgart USING akl-klnum t512w-aklas(2).
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "end of SC512W-AKL.

*---------------------------------------------------------------------*
*       FORM SC512W-VKL                                               *
*---------------------------------------------------------------------*
FORM sc512w-vkl.
  REFRESH list.
  CLEAR list.
  LOOP AT bw.
    MOVE-CORRESPONDING bw TO t512w.
    ps = vkl-klnum - 1.
    SHIFT t512w-vklas BY ps PLACES.
    IF t512w-vklas(1) NE space.
      t512w-vklas+1(1) = space.
      CASE vkl-klwrt.
        WHEN space.                    "CURSOR-SELECTION ON KLNUM ONLY
          PERFORM prlgart USING vkl-klnum t512w-vklas(2).
        WHEN '**'.                     "AT END OF KLNUM
          PERFORM prlgart USING vkl-klnum t512w-vklas(2).
        WHEN OTHERS.                   "CURSOR-SELECTION ON KLNUM KLWRT
          IF vkl-klwrt EQ t512w-vklas(1).
            PERFORM prlgart USING vkl-klnum t512w-vklas(2).
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "end of SC512W-VKL.
*
*---------------------------------------------------------------------*
*       FORM SC512W-KU                                                *
*---------------------------------------------------------------------*
FORM sc512w-ku.
  REFRESH list.
  CLEAR list.
  LOOP AT bw.
    IF NOT bw-kumuflag IS INITIAL.
      MOVE-CORRESPONDING bw TO t512w.
      ps = ku-lgart+2(2) - 1.
      char96 = bw-kumula.
      SHIFT char96 BY ps PLACES.
      IF char96(1) EQ '1'.
        PERFORM prlgart USING ku-lgart+2(2) '  '.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "end of SC512W-KU.
*---------------------------------------------------------------------*
*       FORM SC512W-DG                                                *
*---------------------------------------------------------------------*
FORM sc512w-dg.
  REFRESH list.
  CLEAR list.
  LOOP AT bw.
    IF NOT bw-durchflag IS INITIAL.
      MOVE-CORRESPONDING bw TO t512w.
      switch = 'N'.
      ps = dg-lgart+2(2) - 1.
      SHIFT bw-d1durch BY ps PLACES.
      SHIFT bw-d2durch BY ps PLACES.
      SHIFT bw-d3durch BY ps PLACES.
      SHIFT bw-d4durch BY ps PLACES.
      SHIFT bw-d5durch BY ps PLACES.
      IF bw-d1durch(1) EQ '1'.
        switch = 'J'.
      ELSEIF bw-d2durch(1) EQ '1'.
        switch = 'J'.
      ELSEIF bw-d3durch(1) EQ '1'.
        switch = 'J'.
      ELSEIF bw-d4durch(1) EQ '1'.
        switch = 'J'.
      ELSEIF bw-d5durch(1) EQ '1'.
        switch = 'J'.
      ENDIF.
      IF switch EQ 'J'.
        PERFORM prlgart USING dg-lgart+2(2) '  '.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "end of SC512W-DG.

*---------------------------------------------------------------------*
*       FORM SC512W-BG                                                *
*---------------------------------------------------------------------*
FORM sc512w-bg.
  REFRESH list.
  CLEAR list.
  LOOP AT bw.
    MOVE-CORRESPONDING bw TO t512w.
    IF t512w-gvbla EQ bg-lgart+2(2).
      PERFORM prlgart USING bg-lgart+2(2) 'GV'.
    ENDIF.
    IF t512w-pzbla EQ bg-lgart+2(2).
      PERFORM prlgart USING bg-lgart+2(2) 'PZ'.
    ENDIF.
    IF t512w-fzbla EQ bg-lgart+2(2).
      PERFORM prlgart USING bg-lgart+2(2) 'FZ'.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "end of SC512W-BG.

*---------------------------------------------------------------------*
*       FORM PRLGART                                                  *
*---------------------------------------------------------------------*
FORM prlgart USING nummer wert.
  SELECT SINGLE * FROM t512t WHERE sprsl EQ sy-langu
                               AND molga EQ modlgart
                               AND lgart EQ t512w-lgart.
  IF sy-subrc NE 0. t512t-lgtxt = '(N.N.)'. ENDIF. "text not found
  list-klnum = nummer.
  list-klwrt = wert.
  list-lgart = t512w-lgart.
  list-lgtxt = t512t-lgtxt.
  list-kztxt = t512t-kztxt.
  APPEND list.
ENDFORM.                               "end of PRLGART

*---------------------------------------------------------------------*
*       FORM HEXINCHAR                                                *
*---------------------------------------------------------------------*
FORM hexinchar USING anzahl.           "und Feld HEXIN.

  IF NOT hexin IS INITIAL.
    DO anzahl TIMES
*000001 Type X not allowed in Unicode programs with CHAR
      VARYING hex FROM hexin(1) NEXT hexin+1(1) RANGE hexin.
*      varying hex from hexin next hexin+1.
*000001 End ***********************************************************
      SHIFT char96 BY 8 PLACES.
      ps = 0.                            "set sum
      pc = 10000000.                     "set char
      hextm = x128.                      "set hex
      DO 8 TIMES.
        IF hex O hextm. ps = ps + pc. ENDIF.
        hextm = hextm / 2. pc = pc / 10.
      ENDDO.
      UNPACK ps TO char96+88(8).
    ENDDO.
    ps = ( 12 - anzahl ) * 8. "MAX = 12, maximale Laenge HEXINPUT
    SHIFT char96 BY ps PLACES.

  ENDIF.
ENDFORM.                               "END OF HEXINCHAR
*---------------------------------------------------------------------*
*      FORM CALL_IMG                                                  *
*---------------------------------------------------------------------*
*                                                                     *
*---------------------------------------------------------------------*
FORM call_img USING name.

  DATA: subobjname TYPE objsub-subobjname.

  subobjname = name.

  CALL FUNCTION 'OBJECT_MAINTENANCE_CALL'
    EXPORTING
      tabname    = 'T512W'
      objectname = 'V_512W_D'
      objecttype = 'V'
      subobjname = subobjname
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "call_img
*****processing classes values************
*---------------------------------------------------------------------*
*        FORM LISTE_5_vkl                                             *
*---------------------------------------------------------------------*
FORM liste_5_vkl.
  DATA: linecount TYPE i.
  CLEAR list.
  DESCRIBE TABLE list LINES linecount.
  IF linecount NE 0.
    LOOP AT list.
      LOOP AT vkl WHERE klnum = list-klnum AND klwrt =  list-klwrt .
      ENDLOOP.
      vkl1-lgart = list-lgart.
      vkl1-lgtxt = list-lgtxt.
      vkl1-kztxt = list-kztxt.
      vkl1-klwrt = list-klwrt.
      vkl1-kltxt1 =  vkl-kltxt.
      APPEND vkl1.
    ENDLOOP.
  ELSE.
*    perform no_lines_in_table.
  ENDIF.
ENDFORM.                    "liste_5_vkl
**********************evaluation classes values************************
*---------------------------------------------------------------------*
*        FORM LISTE_5                                                 *
*---------------------------------------------------------------------*
*       FILL TABLE ITAB AKL1                              *
*---------------------------------------------------------------------*
FORM liste_5_akl.
  DATA: linecount TYPE i.
  CLEAR list.
  DESCRIBE TABLE list LINES linecount.
  IF linecount NE 0.
    LOOP AT list.
      LOOP AT akl WHERE klnum = list-klnum AND klwrt =  list-klwrt .
      ENDLOOP.
      akl1-lgart = list-lgart.
      akl1-lgtxt = list-lgtxt.
      akl1-kztxt = list-kztxt.
      akl1-klwrt = list-klwrt.
      akl1-kltxt1 =  akl-kltxt.
      APPEND akl1.
    ENDLOOP.
  ELSE.
*    perform no_lines_in_table.
  ENDIF.
ENDFORM.                    "liste_5_akl
*&---------------------------------------------------------------------*
*&      Form  LISTE_5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM liste_5_ku.
  DATA: linecount TYPE i.
  CLEAR list.
  DESCRIBE TABLE list LINES linecount.
  IF linecount NE 0.
    LOOP AT list.
      ku1-lgart = list-lgart .
      ku1-lgtxt = list-lgtxt .
      ku1-kztxt = list-kztxt.
      ku1-klnum = list-klnum .
      ku1-check_box = 'X' .
      APPEND ku1.
    ENDLOOP.
  ELSE.
*    PERFORM NO_LINES_IN_TABLE.
  ENDIF.

ENDFORM.                                                    " LISTE_5
*
*---------------------------------------------------------------------*
*        FORM LISTE_5                                                 *
*---------------------------------------------------------------------*
*        Ausgaberoutine für Endlosliste                               *
*---------------------------------------------------------------------*
FORM liste_5_dg.
  DATA: linecount TYPE i.
  CLEAR list.
  DESCRIBE TABLE list LINES linecount.
  IF linecount NE 0.
    LOOP AT list.
      dg1-lgart = list-lgart .
      dg1-lgtxt = list-lgtxt .
      dg1-kztxt = list-kztxt.
      dg1-klnum = list-klnum .
      dg1-check_box = 'X' .
      APPEND dg1.
    ENDLOOP.
  ELSE.
*    perform no_lines_in_table.
  ENDIF.
ENDFORM.                                                    "liste_5_dg
*---------------------------------------------------------------------*
*        FORM LISTE_5                                                 *
*---------------------------------------------------------------------*
*        Ausgaberoutine für Endlosliste                               *
*---------------------------------------------------------------------*
FORM liste_5_bg.
  DATA: linecount TYPE i.
  CLEAR list.
  DESCRIBE TABLE list LINES linecount.
  IF linecount NE 0.
    LOOP AT list.
      bg1-lgart = list-lgart .
      bg1-lgtxt = list-lgtxt .
      bg1-kztxt = list-kztxt.
      bg1-klnum = list-klnum .
      bg1-klwrt = list-klwrt .
      CASE list-klwrt.
        WHEN 'GV'.
          MOVE: 'Current wage type' TO bg1-kltxt,
                1 TO bg1-slno.
        WHEN 'PZ'.
          MOVE: '1st Derived wage type' TO bg1-kltxt,
                2 TO bg1-slno.
        WHEN 'FZ'.
          MOVE: '2nd Derived wage type' TO bg1-kltxt,
                3 TO bg1-slno.
        WHEN OTHERS.
      ENDCASE.
      APPEND bg1.
    ENDLOOP.
  ELSE.
*    perform no_lines_in_table.
  ENDIF.
ENDFORM.                                                    "liste_5_bg

*---------------------------------------------------------------------*
*       FORM no_lines_in_table                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM no_lines_in_table.
  header = text-108.
  WRITE: / header.
ENDFORM.                    "no_lines_in_table
*-----------------------------------------------------------------------
* FORM GET_BIT
*-----------------------------------------------------------------------
* Input : BIT_STRING
*         NUMBER      No of Bit in BIT_STRING (starting with No 1)
* Output: VALUE       Value of that Bit (' ' or 'X')
*-----------------------------------------------------------------------
FORM get_bit USING bit_string number value.
  DATA bits(8) TYPE x   VALUE '8040201008040201'.
  DATA byte_no TYPE i.  byte_no = ( number - 1 ) DIV 8.
  DATA bit_no  TYPE i.  bit_no  = ( number - 1 ) MOD 8.
  DATA byte    TYPE x.

*000001 With type X shift instructions require 'in byte mode'
  SHIFT bit_string BY byte_no PLACES CIRCULAR LEFT IN BYTE MODE.
  SHIFT bits       BY bit_no  PLACES CIRCULAR LEFT IN BYTE MODE.
*  shift bit_string by byte_no places circular left.
*  shift bits       by bit_no  places circular left.
*000001 End ***********************************************************
  byte = bit_string.
  IF byte O bits(1).
    value = 'X'.
  ELSE.
    value = ' '.
  ENDIF.
*000001 With type X shift instructions require 'in byte mode'
  SHIFT bit_string BY byte_no PLACES CIRCULAR RIGHT IN BYTE MODE.
  SHIFT bits       BY bit_no  PLACES CIRCULAR RIGHT IN BYTE MODE.
*  shift bit_string by byte_no places circular right.
*  shift bits       by bit_no  places circular right.
*000001 End ***********************************************************
ENDFORM.                    "get_bit
*-----------------------------------------------------------------------
* FORM GET_LGA_TEXT
*-----------------------------------------------------------------------
* Input : PREFIX      '/1' for '/1xx'-wage types or '/2' for '/2xx'
*         NUMBER      the '..xx'
* Output: TEXT        Text for the wage type
*-----------------------------------------------------------------------
FORM get_lga_text USING prefix number text.
  DATA: lga(4), num2(2) TYPE n.

  lga(2)   = prefix.

  MOVE: number TO num2.

  lga+2(2) = num2.
  SELECT SINGLE * FROM t512t WHERE sprsl = sy-langu
                               AND molga = modlgart
                               AND lgart = lga.
  IF sy-subrc = 0.
    CONCATENATE lga t512t-lgtxt INTO text SEPARATED BY space.
  ELSE.
    text = space.
  ENDIF.
ENDFORM.                    "get_lga_text
*&---------------------------------------------------------------------*
*&      Module  KOMBI  OUTPUT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM kombi.
  CASE t511-kombi.
    WHEN ' '. q511-kbbtr = 'X'. q511-kbanz = 'X'.
    WHEN '0'. q511-kbbtr = '-'. q511-kbanz = '-'.
    WHEN '1'. q511-kbbtr = '+'. q511-kbanz = '-'.
    WHEN '2'. q511-kbbtr = '-'. q511-kbanz = '+'.
    WHEN '3'. q511-kbbtr = '*'. q511-kbanz = '*'.
    WHEN '4'. q511-kbbtr = '+'. q511-kbanz = '.'.
    WHEN '5'. q511-kbbtr = '.'. q511-kbanz = '+'.
    WHEN '6'. q511-kbbtr = '+'. q511-kbanz = '+'.
    WHEN '7'. q511-kbbtr = '.'. q511-kbanz = '.'.
    WHEN '8'. q511-kbbtr = '-'. q511-kbanz = '.'.           "XPSK019204
    WHEN '9'. q511-kbbtr = '.'. q511-kbanz = '-'.           "XPSK019204
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                 " KOMBI  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  set_header_color
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_header_color.
  FIELD-SYMBOLS: <repmaincolinfo> TYPE table.
  DATA: gt_color TYPE lvc_s_scol OCCURS 0 WITH HEADER LINE.

  ASSIGN COMPONENT 'COLINFO' OF STRUCTURE
              <rep_main> TO <repmaincolinfo>.
  IF <repmaincolinfo> IS ASSIGNED.
    CLEAR: gt_color.
    gt_color-fname = ' '.
*   GT_COLOR-COLOR-COL = CL_GUI_RESOURCES=>LIST_COL_HEADING.
    gt_color-color-col = 5.
    gt_color-color-int = 1.
    APPEND gt_color.

    APPEND LINES OF gt_color TO <repmaincolinfo>.
  ENDIF.

ENDFORM.                    "set_header_color

*&--------------------------------------------------------------------*
*&      Form  set_header_color
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_header_color1.
  FIELD-SYMBOLS: <repmaincolinfo> TYPE table.
  DATA: gt_color TYPE lvc_s_scol OCCURS 0 WITH HEADER LINE.

  ASSIGN COMPONENT 'COLINFO' OF STRUCTURE
              <rep_main> TO <repmaincolinfo>.
  IF <repmaincolinfo> IS ASSIGNED.
    CLEAR: gt_color.
    gt_color-fname = ' '.
    gt_color-color-col = 3.
    gt_color-color-int = 1.
    APPEND gt_color.

    APPEND LINES OF gt_color TO <repmaincolinfo>.
  ENDIF.

ENDFORM.                    "set_header_color
