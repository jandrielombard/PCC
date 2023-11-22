*&-------------------------------------------------------------------------------------*
*&  INCLUDE ZUSEIHR_SPOOL_MERGE_FORMS.
*&  Purpose              : Utility program triggered by enhancement
*&  in program RPCS0000 (Scheduler for Parallel Execution of Evaluation Programs)
*&  When multiple job splits occur this custom development has been created to merge
*&  the spool files from the same main job into the one spool report where the
*&  spool report title matches and the report title has been activiated in
*&  configuration table ZHR_RPCS0000_A
*&-------------------------------------------------------------------------------------*
*& Modification Logs    :
*-----------------------------------------------------------------------------------------------------*
*Mod  |Date       |User ID       |Description                       |Change Label  |Workbench Request *
*-----------------------------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_SPOOL
*&---------------------------------------------------------------------*
FORM read_spool  USING  p_spool TYPE ty_spool
                        p_format TYPE zhr_spo1_format.

  DATA : lt_spool_attr_line  TYPE bapixmspoolid,
         lt_spool_list_plain TYPE list_string_table,
         lt_dummy            TYPE STANDARD TABLE OF rspoattr,
         lv_rqattr           TYPE tsp01,
         lv_first_page       TYPE i VALUE 1,
         lv_last_page        TYPE i,
         lv_pages            TYPE c,
         lv_string           TYPE string,
         l_spoolid           TYPE  tsp01-rqident,
         lt_spool_list       TYPE bapixmspow OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS <hex_container> TYPE any.

  CONSTANTS lc_lsum_report TYPE PROGRAMM VALUE 'RPLSUMQ0'. ""++MOD-002

  l_spoolid = p_spool-rqident.

  IF p_format NE 'LIST'.
    CLEAR lt_spool_list_plain.
    CALL FUNCTION 'RSPO_RETURN_SPOOLJOB_DAT'
      EXPORTING
        rqident              = l_spoolid
        first_line           = lv_first_page
      IMPORTING
        buffer_dat           = lt_spool_list_plain
      EXCEPTIONS
        no_such_job          = 1
        not_abap_list        = 2
        job_contains_no_data = 3
        selection_empty      = 4
        no_permission        = 5
        can_not_access       = 6
        read_error           = 7
        OTHERS               = 8.
    IF NOT lt_spool_list_plain IS INITIAL.
      APPEND LINES  OF  lt_spool_list_plain  TO gt_report.
    ENDIF.
  ELSE.

    CALL FUNCTION 'RSPO_RETURN_ABAP_SPOOLJOB'
      EXPORTING
        rqident              = l_spoolid
        first_line           = lv_first_page
      TABLES
        buffer               = lt_spool_list
      EXCEPTIONS
        no_such_job          = 1
        not_abap_list        = 2
        job_contains_no_data = 3
        selection_empty      = 4
        no_permission        = 5
        can_not_access       = 6
        read_error           = 7
        OTHERS               = 8.

*---- Mod-001 Start of change PO-0985 -----*
    IF P_CALL = 'H99CWTR0'.

      APPEND LINES OF lt_spool_list FROM 1 TO 13 TO gt_spool_header.

      IF gv_flag is INITIAL.
        LOOP AT gt_spool_header.
          lv_string = gt_spool_header.
          APPEND lv_string TO gt_report.
        ENDLOOP.
        gv_flag = 'X'.
      ENDIF.

      DELETE lt_spool_list FROM 1 TO 17.
      DELETE lt_spool_list WHERE line CP '*Data statistics*'.
      DELETE lt_spool_list WHERE line CP '*Records passed*'.
      DELETE lt_spool_list WHERE line CP '*Calculated total records*'.
*      DELETE lt_spool_list WHERE line CP '-------*'.

      LOOP AT lt_spool_list.
        lv_string = lt_spool_list.
        APPEND lv_string TO gt_report.
      ENDLOOP.

** Begin of Changes PO-986, ++MOD-002
    ELSEIF p_call eq lc_lsum_report. "Report 'RPLSUMQ0'.

      APPEND LINES OF lt_spool_list TO gt_spool_header.

      IF gv_flag is INITIAL.
        LOOP AT gt_spool_header.
          IF sy-tabix LT 9.
            lv_string = gt_spool_header.
            APPEND lv_string TO gt_report.
          ENDIF.
        ENDLOOP.
        gv_flag = 'X'.
      ENDIF.
* End of Changes PO-986, ++MOD-002
    ELSE.

      LOOP AT lt_spool_list.
        lv_string = lt_spool_list.
        APPEND lv_string TO gt_report.
      ENDLOOP.

    ENDIF.

*    LOOP AT lt_spool_list.
*      lv_string = lt_spool_list.
*      APPEND lv_string TO gt_report.
*    ENDLOOP.

*---- Mod-001 End of change PO-0985 -----*
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  READ_JOBS
*&---------------------------------------------------------------------*
FORM read_jobs  TABLES   pt_tbtco
                USING    p_jobnam    TYPE tbtc_spoolid-jobname
                         p_jobcount  TYPE tbtc_spoolid-jobcount
                         p_mins_cnt  TYPE zhr_mer_var
                         p_date      TYPE d
                CHANGING p_completed TYPE abap_bool
                         p_jobname_start TYPE tbtc_spoolid-jobname.

  DATA: BEGIN OF joblogtab OCCURS 0.
          INCLUDE STRUCTURE tbtc5.
        DATA: END OF joblogtab.

  DATA: l_jobname_start  TYPE tbtc_spoolid-jobname,
        l_jobname_end    TYPE tbtc_spoolid-jobname,
        l_time           TYPE t,
        l_jobcount_start TYPE tbtc_spoolid-jobcount,
        l_jobcount_end   TYPE tbtc_spoolid-jobcount.

  CLEAR pt_tbtco.

* format jobname remove the leading split numbers and
* check if all jobs from splitter program have completed.

  l_jobname_start = p_jobnam.
  SHIFT l_jobname_start RIGHT DELETING TRAILING ' '.
  SHIFT l_jobname_start RIGHT BY 10 PLACES.
  CONDENSE l_jobname_start.

  CONCATENATE l_jobname_start 'zzzzzzzzzz' INTO l_jobname_end.

  p_jobname_start =  l_jobname_start .
** set job counter from and to value ( time job created could be over 1 second )
  l_time =  p_jobcount.
  l_time = l_time + p_mins_cnt.
  l_jobcount_end =   l_time.

  l_time =  p_jobcount.
  l_time = l_time - p_mins_cnt.
  l_jobcount_start =   l_time.

  l_jobcount_start+6(2) = '00'.
  l_jobcount_end+6(2) = '00'.


* read job names and status for calling jobname, number
  SELECT * FROM tbtco INTO TABLE @DATA(lt_tbtco)
    WHERE jobname  BETWEEN @l_jobname_start  AND @l_jobname_end
      AND jobcount BETWEEN @l_jobcount_start AND @l_jobcount_end
      AND reluname EQ @p_user
      AND sdldate  EQ @p_date.

* read job status
  LOOP AT lt_tbtco INTO DATA(ls_tbtco).
    CLEAR joblogtab.
    IF ls_tbtco-status NE 'F'.  "not finished check log
      CALL FUNCTION 'BP_JOBLOG_READ'
        EXPORTING
          jobcount              = ls_tbtco-jobcount
          jobname               = ls_tbtco-jobname
        TABLES
          joblogtbl             = joblogtab
        EXCEPTIONS
          cant_read_joblog      = 1
          jobcount_missing      = 2
          joblog_does_not_exist = 3
          joblog_is_empty       = 4
          joblog_name_missing   = 5
          jobname_missing       = 6
          job_does_not_exist    = 7
          OTHERS                = 8.
      LOOP AT joblogtab INTO DATA(ls_joblog) WHERE text CS sy-repid
                                               AND msgno = '550'. " Merge job started
        ls_tbtco-status = 'F'.
      ENDLOOP.
    ENDIF.

    IF ls_tbtco-status NE 'F'.
      CLEAR p_completed.
    ENDIF.
  ENDLOOP.

  pt_tbtco[] = lt_tbtco[].

  CLEAR lt_tbtco.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
FORM print_report  USING p_rqtitle TYPE tsp01-rqtitle.

  DATA: lo_table         TYPE REF TO cl_salv_table,
        lv_title         TYPE tsp01-rqtitle,
        lv_header        TYPE string,
        l_skip_rec       TYPE abap_bool,
        print_parameters TYPE pri_params,
        valid_flag       TYPE c LENGTH 1.



  FIELD-SYMBOLS: <hex_container>  TYPE any,
                 <hex_headerline> TYPE any.

  CHECK gt_report IS NOT INITIAL.

  lv_title = p_rqtitle.
* if first character is a wildcard,  set the title to the report name
  IF lv_title+0(1) = '*'.
    CONCATENATE g_jobnam '*' INTO lv_title.
  ENDIF.
  lv_title+50(5) = '- All'(017).                    "++MOD-004


  NEW-PAGE PRINT ON LINE-COUNT 65
           LINE-SIZE 1023
           LAYOUT 'X_65_255'
           DEPARTMENT 'HR'
           NO DIALOG.

  CALL FUNCTION 'SET_PRINT_PARAMETERS'
    EXPORTING
      list_text = lv_title.
* write report name
  WRITE : / p_rqtitle.
  ULINE.

  IF p_rqtitle CS '*'.
    SPLIT p_rqtitle AT '*' INTO DATA(lv_title_short) DATA(lv_st4)
                              IN CHARACTER MODE.
  ENDIF.

  CLEAR lv_header.
  LOOP AT gt_report ASSIGNING <hex_container> .
    CHECK <hex_container>  IS  NOT INITIAL . "remove blank lines


    CLEAR l_skip_rec .
* do not show repeated header lines for STP reports
    IF p_rqtitle NS '*'.
      IF <hex_container>  CS 'Records passed' OR
         <hex_container>  CS 'Data statistics' OR
         <hex_container>  EQ p_rqtitle OR
         <hex_container>  EQ lv_header. "first line
        l_skip_rec = abap_true.
      ENDIF.

      IF l_skip_rec IS INITIAL.
*       set first line as header line
        IF lv_header IS INITIAL.
          lv_header = <hex_container>.
        ENDIF.

        WRITE: / <hex_container> .
      ENDIF.

    ELSE.
*     when wild card reports are merged display all fields
      IF  <hex_container>  CS lv_title_short AND  lv_title_short IS NOT INITIAL.
        ULINE.
      ENDIF.

      WRITE: / <hex_container> .


    ENDIF.
    CLEAR <hex_container>.

  ENDLOOP.

  NEW-PAGE PRINT OFF.

  CLEAR gt_report[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INITILATION
*&---------------------------------------------------------------------*
FORM initilation .
* read the report titles from configuration
  SELECT rqtitle job_variance spool_format FROM  zuse_rpcs0000_a INTO TABLE gt_titles
         WHERE  programm   = p_call
         AND    call_prog  = sy-repid.
  IF sy-subrc NE 0.
    MESSAGE e015(zhrau_rpt).
  ELSE.
*   set job counter variance from the configuration ( seconds btw job creations )
    READ TABLE gt_titles INTO DATA(ls_title_first) INDEX 1.
    IF ls_title_first-job_variance GT 0.
      p_secs =  ls_title_first-job_variance.
    ENDIF.

    IF s_title[] IS INITIAL AND p_call IS NOT INITIAL.

      LOOP AT gt_titles INTO DATA(ls_title).
*     wild card at end of the title
        IF ls_title-rqtitle CS '*'.
          SPLIT ls_title-rqtitle AT '*' INTO DATA(lv_st1) DATA(lv_st2)
                                       IN CHARACTER MODE.
          s_title-low = lv_st1.
          CONCATENATE lv_st1 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'
                              INTO s_title-high .
          s_title-sign = 'I'.
          s_title-option = 'BT'.
        ELSE.
          s_title-low = ls_title-rqtitle .
          s_title-sign = 'I'.
          s_title-option = 'EQ'.
        ENDIF.
        APPEND s_title.
        CLEAR s_title.
      ENDLOOP.

    ENDIF.
  ENDIF.

  CLEAR: gt_spool_job, gt_spool.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LOCK_PROGRAM
*&---------------------------------------------------------------------*
FORM lock_program USING p_locked TYPE abap_bool.

  LOOP AT gt_tbtco INTO  gs_tbtco.
    CALL FUNCTION 'ENQUEUE_EZHR_RPCS0000_L'
      EXPORTING
        mode_zhr_rpcs0000_loc = 'X'
        mandt                 = sy-mandt
        programm              = p_call
        uname                 = p_user
        jobname               = gs_tbtco-jobname
        jobcount              = gs_tbtco-jobcount
        _wait                 = ' '
      EXCEPTIONS
        foreign_lock          = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc NE 0.
      g_not_locked = abap_true.
      EXIT.
    ELSE.
      g_not_locked = abap_false.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UNLOCK_PROGRAM
*&---------------------------------------------------------------------*
FORM unlock_program  USING p_locked TYPE abap_bool.

  CHECK   g_not_locked = abap_false.

  LOOP AT gt_tbtco INTO  gs_tbtco.
    CALL FUNCTION 'DEQUEUE_EZHR_RPCS0000_L'
      EXPORTING
        mode_zhr_rpcs0000_loc = 'X'
        mandt                 = sy-mandt
        programm              = p_call
        uname                 = p_user
        jobname               = gs_tbtco-jobname
        jobcount              = gs_tbtco-jobcount.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WRITE_REPORTS
*&---------------------------------------------------------------------*
FORM write_reports .
  DATA : lv_pev_rqtitle  TYPE tsp01-rqtitle,
         lv_merge_format TYPE char4.

  CONSTANTS lc_lsum_report TYPE PROGRAMM VALUE 'RPLSUMQ0'. ""++MOD-002

* sort by title text
  SORT gt_spool BY rqtitle.

  LOOP AT gt_spool INTO DATA(ls_spool).

    PERFORM adjust_title_wild_char CHANGING ls_spool-rqtitle.

    IF ls_spool-rqtitle NE lv_pev_rqtitle AND
      lv_pev_rqtitle IS NOT INITIAL.

      PERFORM print_report USING  lv_pev_rqtitle .

    ENDIF.
    lv_pev_rqtitle  = ls_spool-rqtitle.

    READ TABLE gt_titles WITH KEY rqtitle = ls_spool-rqtitle
                 INTO DATA(ls_title).
    PERFORM read_spool USING ls_spool
                              ls_title-spool_format.

  ENDLOOP.
  IF sy-subrc EQ 0.

** Begin of Changes PO-986, MOD-002
    IF p_call eq lc_lsum_report. " Report 'RPLSUMQ0'.

*    Amend the merged spool to consolidate the hours and amount against categories for each WT
      PERFORM consolidate_rplsumq0_spools USING lv_pev_rqtitle CHANGING gt_spool_header[].   "MOD-003, pass title as parameter
    ELSE.
** End of Changes PO-986, MOD-002

    PERFORM print_report USING  lv_pev_rqtitle .

    ENDIF.                                                      "++MOD-002
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADJUST_TITLE_WILD_CHAR
*&---------------------------------------------------------------------*
FORM adjust_title_wild_char  CHANGING p_rqtitle.

* cater for title in configuration having a wildcard of '*'
  READ TABLE s_title WITH KEY low = p_rqtitle.
  IF sy-subrc NE 0.

    LOOP AT s_title INTO DATA(ls_title) WHERE low  LE p_rqtitle
                                          AND high GE p_rqtitle.

      CONCATENATE  ls_title-low '*' INTO  p_rqtitle.
    ENDLOOP.
  ENDIF.


ENDFORM.

** Begin of Changes PO-986, MOD-002
**Two new subroutines added to conolidate the LSUM report spool
*&---------------------------------------------------------------------*
*&      Form  CONSOLIDATE_RPLSUMQ0_SPOOLS
*&---------------------------------------------------------------------*
*       Consolidate the Wage Type hours and Amount under each category *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consolidate_rplsumq0_spools USING p_title TYPE rspotitle CHANGING p_lt_spool_list LIKE gt_spool_header[] .  "MOD-003++

*Begin of Changes PO-986 MOD-004++
*Change the code to segregate merged spool based on compnay code
DATA :lv_field TYPE char20,
      lv_text TYPE char50,
      lv_flag TYPE char10,
      lv_abkrs TYPE abkrs,
      lv_permo TYPE permo,
      lv_pabrp TYPE pabrp,
      lv_pabrj TYPE pabrj,
      lv_len TYPE i,                        "MOD-004++
      lv_pp_begda TYPE begda,
      lv_pp_endda TYPE endda,
      lv_index TYPE sy-index,
      lv_rowid TYPE sy-tabix,               "MOD-004++
      lv_cocode TYPE bukrs,                 "MOD-004++
      ls_itable_s TYPE ty_table_s,
      ls_itable TYPE ty_table,
      ls_spool_list TYPE bapixmspow,
      lt_itab TYPE TABLE OF char100,
      lt_itable_ea TYPE STANDARD TABLE OF ty_table,
      lt_itable_dd TYPE STANDARD TABLE OF ty_table,
      lt_itable_nw TYPE STANDARD TABLE OF ty_table,
      lt_itable_am TYPE STANDARD TABLE OF ty_table,
      lt_itable_nr TYPE STANDARD TABLE OF ty_table,
      lt_itable_rt TYPE STANDARD TABLE OF ty_table.

CONSTANTS : lc_ern TYPE char10 VALUE 'ERN',
            lc_ret TYPE char10 VALUE 'RET',
            lc_ded TYPE char10 VALUE 'DED',
            lc_not TYPE char10 VALUE 'NOT',
            lc_not_ret TYPE char10 VALUE 'NOT_RET',
            lc_amt TYPE char10 VALUE 'AMT',
            lc_fldnm type char20 VALUE 'ls_itable-field_'.

FIELD-SYMBOLS : <fs_field> TYPE anY,
                <ls_itab> LIKE LINE OF lt_itab .


 LOOP AT p_lt_spool_list INTO ls_spool_list.

*Begin of Changes PO-986 MOD-004++
*     Get company code
      IF ls_spool_list+0(11) eq 'Pay Period:'(019).
        lv_rowid = sy-tabix.
        PERFORM get_company_code USING ls_spool_list CHANGING lv_cocode.
        CONTINUE.

      ENDIF.
*End of Changes PO-986 MOD-004++

*     Get the pay period dates
      IF sy-tabix EQ 2.
        lv_pp_begda = ls_spool_list+6(4) && ls_spool_list+3(2) && ls_spool_list+(2).
        lv_pp_endda = ls_spool_list+19(4) && ls_spool_list+16(2) && ls_spool_list+13(2).
        CONTINUE.                                                         "MOD-004++

      ELSEIF sy-tabix EQ 3.
*       Get pay period
        CONDENSE ls_spool_list NO-GAPS.                                   "MOD-003++
        lv_abkrs = ls_spool_list+15(2).                                   "MOD-003++

*Begin of Changes PO-986 MOD-004++
        IF lv_abkrs IS NOT INITIAL.
          SELECT SINGLE permo FROM t549a INTO lv_permo
                              WHERE abkrs EQ lv_abkrs.
            IF sy-subrc EQ 0.
               SELECT SINGLE pabrp pabrj FROM t549q INTO (lv_pabrp, lv_pabrj)
                                          WHERE permo EQ lv_permo
                                          AND begda EQ lv_pp_begda
                                          AND endda EQ lv_pp_endda.
            ENDIF.
        ENDIF.
        CONTINUE.
*End of Changes PO-986 MOD-004++
      ENDIF.

      SPLIT ls_spool_list AT '|' INTO TABLE lt_itab.

      IF lt_itab IS NOT INITIAL.
        LOOP AT lt_itab ASSIGNING <ls_itab>.
          lv_index = sy-tabix + 1.                            "MOD-004++
          lv_field = lc_fldnm && lv_index.
              REPLACE all OCCURRENCES OF ',' in <ls_itab> WITH ''.
          assign component lv_index of structure ls_itable to <fs_field>.
          <fs_field> =  <ls_itab>.
        ENDLOOP.

*Set the flag for each category of Wage Types
         IF ls_itable-field_1 EQ 'Earnings and Allowances'(007)."
           lv_flag = lc_ern. "'ERN'.
         ELSEIF ls_itable-field_1 EQ 'Retro/Claim from previous'(008).
           lv_flag = lc_ret. "'RET'.
         ELSEIF ls_itable-field_1 EQ 'Deductions'(009).
           lv_flag = lc_ded. "'DED'.
         ELSEIF ls_itable-field_1 EQ 'Total Deductions'(010) .
           lv_flag = lc_amt. "'AMT'.
         ELSEIF ls_itable-field_1 EQ 'Notational Wage Types'(011).
           lv_flag = lc_not. "'NOT'.
         ELSEIF ls_itable-field_1 EQ 'Notational Retro Wage Types'(012).
           lv_flag = lc_not_ret. "'NOT_RET'.
         ENDIF.


        lv_text = ls_itable-field_2 && ls_itable-field_3.
        IF lv_text NE ' List contains no data'(013).

            ls_itable-bukrs  = lv_cocode.                               "MOD-004++
            IF lv_flag EQ lc_ern AND ls_itable-field_2 IS NOT INITIAL .
              APPEND ls_itable to lt_itable_ea.
            ELSEIF lv_flag EQ lc_ret AND ls_itable-field_2 IS NOT INITIAL .
              APPEND ls_itable TO lt_itable_rt.
            ELSEIF lv_flag EQ lc_ded AND ls_itable-field_2 IS NOT INITIAL .
              APPEND ls_itable TO lt_itable_dd.
            ELSEIF lv_flag EQ lc_not AND ls_itable-field_2 IS NOT INITIAL .
*Begin of Changes PO-986 MOD-004++
              lv_len = strlen( ls_itable-field_2 ) - 2.
              IF ls_itable-field_2+lv_len(2) eq ' R'.
                 APPEND ls_itable TO lt_itable_nr.
              ELSE.
                 APPEND ls_itable TO lt_itable_nw.
              ENDIF.
*End of Changes PO-986 MOD-004++
            ELSEIF lv_flag EQ lc_not_ret AND ls_itable-field_2 IS NOT INITIAL .
              APPEND ls_itable TO lt_itable_nr.
            ELSEIF lv_flag EQ lc_amt.

              SHIFT ls_itable_s-field_2 left DELETING LEADING space.
              IF ls_itable-field_2 EQ 'Claim'(014) OR ls_itable-field_2 EQ 'Amount Paid'(015).
              APPEND ls_itable TO lt_itable_am.
              ENDIF.

            ENDIF.

         CLEAR ls_itable.

       ENDIF.
     ENDIF.
    ENDLOOP.
*Begin of Changes PO-986 MOD-004++
   PERFORM generate_report USING lt_itable_ea lt_itable_dd lt_itable_am
                                 lt_itable_nw lt_itable_rt lt_itable_nr
                                 lv_abkrs lv_pabrp lv_pabrj p_title.

   REFRESH : lt_itable_ea, lt_itable_dd, lt_itable_am, lt_itable_nw, lt_itable_rt, lt_itable_nr.
*End of Changes PO-986 MOD-004++
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONVERT_INTO_TABLES
*&---------------------------------------------------------------------*
*       Convert the character values in number type and consolidate the*
*       amount against each wage type for different categories
*----------------------------------------------------------------------*
*      -->P_LT_ITABLE_RT  text
*      -->P_COCODE  text
*      <--P_LT_RE  text
*----------------------------------------------------------------------*
FORM convert_into_tables  USING    p_lt_itable_rt LIKE gt_itable
                                   p_cocode TYPE bukrs             "" ++Mod-004
                          CHANGING p_lt_re LIKE gt_wt.

DATA : lv_wt_chars TYPE char100,
       ls_itable_prev TYPE ty_lsum_wt,
       ls_itable_s TYPE ty_table_s,
       lt_itable TYPE STANDARD TABLE OF ty_table,                  " ++MOD-004
       lt_itable_s TYPE STANDARD TABLE OF ty_table_s.

CONSTANTS : lc_num TYPE char100 VALUE '/0123456789',
            lc_chars TYPE char100 VALUE 'abcdefghijklmonopqrstuvwxyz'.

  REFRESH p_lt_re.                                                " ++MOD-004

  IF p_lt_itable_rt IS INITIAL.
    EXIT.
*Begin of Changes PO-986 MOD-004++
  ELSE.
    MOVE p_lt_itable_rt TO lt_itable.
    DELETE lt_itable WHERE bukrs NE p_cocode.
    DELETE lt_itable WHERE field_2 CO '-'.
*End of Changes PO-986 MOD-004++
  ENDIF.

  lv_wt_chars = lc_num && lc_chars && sy-abcde.

  MOVE-CORRESPONDING lt_itable TO lt_itable_s.                    " ++MOD-004

  SORT lt_itable_s BY field_2.

*iterate each record and consolidate the hours and amount
  LOOP AT lt_itable_s INTO ls_itable_s.

    IF ls_itable_s-field_2(4) CO lv_wt_chars .

*if amount is for same WT, then add the amt and hrs to the workarea
      IF ls_itable_prev-lgart eq ls_itable_s-field_2+(4)
        AND ls_itable_prev-lgtxt eq ls_itable_s-field_2+5(25).    "MOD-004++ additional condition as texts differ for same WTs also
        ls_itable_prev-tp_anzhl = ls_itable_prev-tp_anzhl + ls_itable_s-field_3.
        ls_itable_prev-tp_betrg = ls_itable_prev-tp_betrg + ls_itable_s-field_4.
        ls_itable_prev-mtd_anzhl = ls_itable_prev-mtd_anzhl + ls_itable_s-field_5.
        ls_itable_prev-mtd_betrg = ls_itable_prev-mtd_betrg + ls_itable_s-field_6.
        ls_itable_prev-ytd_anzhl = ls_itable_prev-ytd_anzhl + ls_itable_s-field_7.
        ls_itable_prev-ytd_betrg = ls_itable_prev-ytd_betrg + ls_itable_s-field_8.
       ELSE.
         IF ls_itable_prev is NOT INITIAL.
          APPEND ls_itable_prev to p_lt_re.
         ENDIF.

        ls_itable_prev-lgart = ls_itable_s-field_2+(4).
        ls_itable_prev-lgtxt = ls_itable_s-field_2+5(25).         "MOD-003++ skip blank space, MOD-004++
        ls_itable_prev-tp_anzhl = ls_itable_s-field_3.
        ls_itable_prev-tp_betrg = ls_itable_s-field_4.
        ls_itable_prev-mtd_anzhl = ls_itable_s-field_5.
        ls_itable_prev-mtd_betrg = ls_itable_s-field_6.
        ls_itable_prev-ytd_anzhl = ls_itable_s-field_7.
        ls_itable_prev-ytd_betrg = ls_itable_s-field_8.

      ENDIF.

    ELSEIF ls_itable_s-field_2 eq 'Claim'(014) or ls_itable_s-field_2 eq 'Amount Paid'(015).

*if amount is for same WT, then add the amt and hrs to the workarea
      IF ls_itable_prev-LGTXT eq ls_itable_s-field_2
        AND ls_itable_prev-lgtxt eq ls_itable_s-field_2+5(25).

        ls_itable_prev-TP_ANZHL = ls_itable_prev-TP_ANZHL + ls_itable_s-field_3.
        ls_itable_prev-TP_BETRG = ls_itable_prev-TP_BETRG + ls_itable_s-field_4.
        ls_itable_prev-MTD_ANZHL = ls_itable_prev-MTD_ANZHL + ls_itable_s-field_5.
        ls_itable_prev-MTD_BETRG = ls_itable_prev-MTD_BETRG + ls_itable_s-field_6.
        ls_itable_prev-YTD_ANZHL = ls_itable_prev-YTD_ANZHL + ls_itable_s-field_7.
        ls_itable_prev-YTD_BETRG = ls_itable_prev-YTD_BETRG + ls_itable_s-field_8.
       ELSE.
         IF ls_itable_prev IS NOT INITIAL.
          APPEND ls_itable_prev TO p_lt_re.
         ENDIF.

        ls_itable_prev-LGTXT = ls_itable_s-field_2.
        ls_itable_prev-TP_ANZHL = ls_itable_s-field_3.
        ls_itable_prev-TP_BETRG = ls_itable_s-field_4.
        ls_itable_prev-MTD_ANZHL = ls_itable_s-field_5.
        ls_itable_prev-MTD_BETRG = ls_itable_s-field_6.
        ls_itable_prev-YTD_ANZHL = ls_itable_s-field_7.
        ls_itable_prev-YTD_BETRG = ls_itable_s-field_8.

      ENDIF.

    ENDIF.

  ENDLOOP.

*Begin of Changes PO-986 MOD-004++
  IF ls_itable_prev IS NOT INITIAL.
    APPEND ls_itable_prev TO p_lt_re.
  ENDIF.
  CLEAR ls_itable_prev.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY_CODE
*&---------------------------------------------------------------------*
*       Retrieve Company Code from description
*----------------------------------------------------------------------*
*      -->P_LS_SPOOL_LIST  text
*      <--P_LV_COCODE  text
*----------------------------------------------------------------------*
FORM get_company_code  USING    p_ls_spool_list TYPE bapixmspow
                       CHANGING p_lv_cocode TYPE bukrs.


DATA : lv_len TYPE i,
       lv_cctxt TYPE butxt,
       ls_spool_list TYPE bapixmspow,
       ls_ccode TYPE ty_ccode,
       lt_ccode TYPE STANDARD TABLE OF ty_ccode.

      ls_spool_list =  p_ls_spool_list+14.
      CONDENSE ls_spool_list.
      lv_len = strlen( ls_spool_list ) - 19.
      lv_cctxt = ls_spool_list+(lv_len).

*     check if the company code is already identified
      SORT gt_ccode ASCENDING BY butxt.
      READ TABLE gt_ccode INTO ls_ccode WITH KEY butxt = lv_cctxt.
      IF sy-subrc ne 0 .
*       if company code doesnt exist, read from master table the latest co code
        SELECT bukrs butxt INTO TABLE lt_ccode FROM t001 WHERE butxt eq lv_cctxt.
        IF sy-subrc eq 0.
          SORT lt_ccode DESCENDING BY bukrs.
          READ TABLE lt_ccode INTO ls_ccode INDEX 1.
           p_lv_cocode = ls_ccode-bukrs.
           APPEND ls_ccode TO gt_ccode.
           REFRESH lt_ccode.
        ENDIF.
      ELSE.
        p_lv_cocode = ls_ccode-bukrs.
      ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GENERATE_REPORT
*&---------------------------------------------------------------------*
*       Geenrate the report by merging data for each company code
*----------------------------------------------------------------------*
*      -->P_LT_ITABLE_EA  text
*      -->P_LT_ITABLE_DD  text
*      -->P_LT_ITABLE_AM  text
*      -->P_LT_ITABLE_NW  text
*      -->P_LT_ITABLE_RT  text
*      -->P_LT_ITABLE_NR  text
*      -->P_LT_CCODE  text
*      -->P_lv_abkrs text
*      -->P_lv_pabrp text
*      -->P_lv_pabrj text
*      -->P_p_title text
*----------------------------------------------------------------------*
FORM generate_report  USING    p_lt_itable_ea LIKE gt_itable
                               p_lt_itable_dd LIKE gt_itable
                               p_lt_itable_am LIKE gt_itable
                               p_lt_itable_nw LIKE gt_itable
                               p_lt_itable_rt LIKE gt_itable
                               p_lt_itable_nr LIKE gt_itable
                               p_lv_abkrs TYPE abkrs
                               p_lv_pabrp TYPE pabrp
                               p_lv_pabrj TYPE pabrj
                               p_title TYPE rspotitle
  .

DATA: lt_ern TYPE STANDARD TABLE OF ty_lsum_wt WITH HEADER LINE,
      lt_ded TYPE STANDARD TABLE OF ty_lsum_wt WITH HEADER LINE,
      lt_amtp TYPE STANDARD TABLE OF ty_lsum_wt WITH HEADER LINE,
      lt_not TYPE STANDARD TABLE OF ty_lsum_wt WITH HEADER LINE,
      lt_re TYPE STANDARD TABLE OF ty_lsum_wt WITH HEADER LINE,
      lt_not_re TYPE STANDARD TABLE OF ty_lsum_wt WITH HEADER LINE,
      lt_ret TYPE STANDARD TABLE OF ty_lsum_ret WITH HEADER LINE,
      lt_not_ret TYPE STANDARD TABLE OF ty_lsum_ret WITH HEADER LINE.

DATA : lv_title TYPE tsp01-rqtitle,
       lv_len TYPE i,
       ls_ret TYPE ty_lsum_ret.

FIELD-SYMBOLS : <ls_re> TYPE ty_lsum_wt,
               <ls_ccode> TYPE ty_ccode.

   IF p_title IS NOT INITIAL.
*    Set the title of merged spool
       lv_title = p_title.
*      if first character is a wildcard,  set the title to the report name
       IF lv_title+0(1) = '*'(016).
         CONCATENATE g_jobnam '*' INTO lv_title.
       ENDIF.
       lv_title+50(5) = '- All'(017).

   ENDIF.

    NEW-PAGE PRINT ON LINE-COUNT 65
          LINE-SIZE 255
          LAYOUT 'X_65_255'
          NO DIALOG.
     SORT gt_ccode ASCENDING BY bukrs.
     DELETE ADJACENT DUPLICATES FROM gt_ccode.

*   get the spool generated for each company code
    LOOP AT gt_ccode ASSIGNING <ls_ccode>.

*   Convert the character values in p types and aggregate
    PERFORM convert_into_tables USING p_lt_itable_ea <ls_ccode>-bukrs CHANGING lt_ern[].
    PERFORM convert_into_tables USING p_lt_itable_dd <ls_ccode>-bukrs CHANGING lt_ded[].
    PERFORM convert_into_tables USING p_lt_itable_am <ls_ccode>-bukrs CHANGING lt_amtp[].
    PERFORM convert_into_tables USING p_lt_itable_nw <ls_ccode>-bukrs CHANGING lt_not[].

    PERFORM convert_into_tables USING p_lt_itable_rt <ls_ccode>-bukrs CHANGING lt_re[].
    PERFORM convert_into_tables USING p_lt_itable_nr <ls_ccode>-bukrs CHANGING lt_not_re[].
    MOVE-CORRESPONDING lt_re[] to lt_ret[].

*Begin of Changes PO-986 MOD-004++
*   move the retro WTs flag if present  in RETCHAR
    LOOP AT lt_not_re ASSIGNING <ls_re>.
        lv_len = strlen( <ls_re>-lgtxt ) - 2.
        MOVE-CORRESPONDING <ls_re> to ls_ret.
        ls_ret-lgtxt = <ls_re>-lgtxt+(lv_len). "remove retro indicator)
        ls_ret-retchar = 'R'. "pass the retro indicator to corresponding field
        APPEND ls_ret TO lt_not_ret.
    ENDLOOP.
    CLEAR lv_len.

    IF <ls_re> IS ASSIGNED.
      UNASSIGN <ls_re>.
    ENDIF.

*   Call custum subroutine in the enhanced standard program for listing the spool
    PERFORM merge_summary(rplsumq0) USING lt_ern[] lt_ded[] lt_amtp[]
                                           lt_not[] lt_ret[] lt_not_ret[]
                                           p_lv_abkrs p_lv_pabrp p_lv_pabrj
                                           lv_title <ls_ccode>-bukrs.                        "MOD-003++
    SKIP 1.

    REFRESH : lt_ern[], lt_ded[], lt_amtp[], lt_not[], lt_ret[], lt_not_ret[].

    ENDLOOP.

    WRITE :/'End of Report'(018).
*End of Changes PO-986 MOD-004++
ENDFORM.
