*----------------------------------------------------------------------*
* INCLUDE ZHRAUREPY_PCC_SPOOL_MERGE_FRM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_SPOOL
*&---------------------------------------------------------------------*
form read_spool  using  p_spool type ty_spool
                        p_format type zhr_spo1_format.

  data : lt_spool_attr_line  type bapixmspoolid,
         lt_spool_list_plain type list_string_table,
         lt_dummy            type standard table of rspoattr,
         lv_rqattr           type tsp01,
         lv_first_page       type i value 1,
         lv_last_page        type i,
         lv_pages            type c,
         lv_string           type string,
         l_spoolid           type  tsp01-rqident,
         lt_spool_list       type bapixmspow occurs 0 with header line.

  field-symbols <hex_container> type any.

  l_spoolid = p_spool-rqident.

  if p_format ne 'LIST'.
    clear lt_spool_list_plain.
    call function 'RSPO_RETURN_SPOOLJOB_DAT'
      exporting
        rqident              = l_spoolid
        first_line           = lv_first_page
      importing
        buffer_dat           = lt_spool_list_plain
      exceptions
        no_such_job          = 1
        not_abap_list        = 2
        job_contains_no_data = 3
        selection_empty      = 4
        no_permission        = 5
        can_not_access       = 6
        read_error           = 7
        others               = 8.
    if not lt_spool_list_plain is initial.
      append lines  of  lt_spool_list_plain  to gt_report.
    endif.
  else.

    call function 'RSPO_RETURN_ABAP_SPOOLJOB'
      exporting
        rqident              = l_spoolid
        first_line           = lv_first_page
      tables
        buffer               = lt_spool_list
      exceptions
        no_such_job          = 1
        not_abap_list        = 2
        job_contains_no_data = 3
        selection_empty      = 4
        no_permission        = 5
        can_not_access       = 6
        read_error           = 7
        others               = 8.

    loop at lt_spool_list.
      lv_string = lt_spool_list.
      append lv_string to gt_report.
    endloop.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  READ_JOBS
*&---------------------------------------------------------------------*
form read_jobs  tables   pt_tbtco
                changing p_completed type abap_bool
                         p_jobname_start type tbtc_spoolid-jobname.

  data: begin of joblogtab occurs 0.
          include structure tbtc5.
        data: end of joblogtab.

  data: l_jobname_start  type tbtc_spoolid-jobname,
        l_jobname_end    type tbtc_spoolid-jobname,
        l_time           type t,
        l_jobcount_start type tbtc_spoolid-jobcount,
        l_jobcount_end   type tbtc_spoolid-jobcount.
  data: lt_tbtco type standard table of ty_tbtco.

  clear pt_tbtco.
* Read job names and status for calling jobname, number
  select jobname, jobcount, status from tbtco
    into corresponding fields of table @lt_tbtco
    where jobname  in @s_jobnam
      and jobcount in @s_jobcnt.

* read job status
  loop at lt_tbtco into data(ls_tbtco).
    clear joblogtab.
    if ls_tbtco-status ne 'F'. "not finished check log
      call function 'BP_JOBLOG_READ'
        exporting
          jobcount              = ls_tbtco-jobcount
          jobname               = ls_tbtco-jobname
        tables
          joblogtbl             = joblogtab
        exceptions
          cant_read_joblog      = 1
          jobcount_missing      = 2
          joblog_does_not_exist = 3
          joblog_is_empty       = 4
          joblog_name_missing   = 5
          jobname_missing       = 6
          job_does_not_exist    = 7
          others                = 8.
      loop at joblogtab into data(ls_joblog)
         where text cs sy-repid and msgno = '550'.
        ls_tbtco-status = 'F'.
      endloop.
    endif.

    if ls_tbtco-status ne 'F' .
      clear p_completed.
    endif.
  endloop.

  pt_tbtco[] = lt_tbtco[].
  clear lt_tbtco.

endform.
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
form print_report  using p_rqtitle type tsp01-rqtitle.

  data: lo_table         type ref to cl_salv_table,
        lv_title         type tsp01-rqtitle,
        lv_header        type string,
        l_skip_rec       type abap_bool,
        print_parameters type pri_params,
        valid_flag       type c length 1.

  field-symbols: <hex_container>  type any,
                 <hex_headerline> type any.

  check gt_report is not initial.

  lv_title = p_rqtitle.
* if first character is a wildcard,  set the title to the report name
  if lv_title+0(1) = '*'.
    concatenate g_jobnam '*' into lv_title.
  endif.
  lv_title+50(5) = '- All'.

  new-page print on line-count 65
           line-size 1023
           layout 'X_65_255'
           department 'HR'
           no dialog.

  call function 'SET_PRINT_PARAMETERS'
    exporting
      list_text = lv_title.
* write report name
  write : / p_rqtitle.
  uline.

  if p_rqtitle cs '*'.
    split p_rqtitle at '*' into data(lv_title_short) data(lv_st4)
                              in character mode.
  endif.

  clear lv_header.
  loop at gt_report assigning <hex_container> .
    check <hex_container>  is  not initial . "remove blank lines


    clear l_skip_rec .
* do not show repeated header lines for STP reports
    if p_rqtitle ns '*'.
      if <hex_container>  cs text-007 or
         <hex_container>  cs text-008 or
         <hex_container>  eq p_rqtitle or
         <hex_container>  eq lv_header. "first line
        l_skip_rec = abap_true.
      endif.

      if l_skip_rec is initial.
*       set first line as header line
        if lv_header is initial.
          lv_header = <hex_container>.
        endif.

        write: / <hex_container> .
      endif.

    else.
*     when wild card reports are merged display all fields
      if  <hex_container>  cs lv_title_short and  lv_title_short is not initial.
        uline.
      endif.

      write: / <hex_container> .

    endif.
    clear <hex_container>.

  endloop.

  new-page print off.
  clear gt_report[].

endform.
*&---------------------------------------------------------------------*
*&      Form  INITILATION
*&---------------------------------------------------------------------*
form initilation .
* read the report titles from configuration
  select rqtitle job_variance spool_format from  zuse_rpcs0000_a into table gt_titles
         where  programm   = p_call
         and    call_prog  = gc_orig_spool_merg_prog.
  if sy-subrc ne 0.
    message i015(zhrau_rpt).
  else.
*   set job counter variance from the configuration ( seconds btw job creations )
    read table gt_titles into data(ls_title_first) index 1.
    if ls_title_first-job_variance gt 0.
      p_secs =  ls_title_first-job_variance.
    endif.

    if s_title[] is initial and p_call is not initial.

      loop at gt_titles into data(ls_title).
*     wild card at end of the title
        if ls_title-rqtitle cs '*'.
          split ls_title-rqtitle at '*' into data(lv_st1) data(lv_st2)
                                       in character mode.
          s_title-low = lv_st1.
          concatenate lv_st1 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'
                              into s_title-high .
          s_title-sign = 'I'.
          s_title-option = 'BT'.
        else.
          s_title-low = ls_title-rqtitle .
          s_title-sign = 'I'.
          s_title-option = 'EQ'.
        endif.
        append s_title.
        clear s_title.
      endloop.

    endif.
  endif.

  clear: gt_spool_job, gt_spool.
endform.
*&---------------------------------------------------------------------*
*&      Form  WRITE_REPORTS
*&---------------------------------------------------------------------*
form write_reports .
  data : lv_pev_rqtitle  type tsp01-rqtitle,
         lv_merge_format type char4.

* sort by title text
  sort gt_spool by rqtitle.

  loop at gt_spool into data(ls_spool).

    perform adjust_title_wild_char changing ls_spool-rqtitle.

    if ls_spool-rqtitle ne lv_pev_rqtitle and
      lv_pev_rqtitle is not initial.

      perform print_report using  lv_pev_rqtitle .

    endif.
    lv_pev_rqtitle  = ls_spool-rqtitle.

    read table gt_titles with key rqtitle = ls_spool-rqtitle
                 into data(ls_title).
    perform read_spool using ls_spool
                              ls_title-spool_format.

  endloop.
  if sy-subrc eq 0.
    perform print_report using  lv_pev_rqtitle .
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  ADJUST_TITLE_WILD_CHAR
*&---------------------------------------------------------------------*
form adjust_title_wild_char  changing p_rqtitle.
* cater for title in configuration having a wildcard of '*'
  read table s_title with key low = p_rqtitle.
  if sy-subrc ne 0.
    loop at s_title into data(ls_title) where low  le p_rqtitle
                                          and high ge p_rqtitle.
      concatenate  ls_title-low '*' into  p_rqtitle.
    endloop.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  WRITE_REPORTS_WITH_FORMATTING
*&---------------------------------------------------------------------*
form write_reports_with_formatting.

  data : lv_pev_rqtitle  type tsp01-rqtitle,
         lv_merge_format type char4.

  clear gt_spool_objects.
* sort by title text
  sort gt_spool by rqtitle.

  loop at gt_spool into data(ls_spool).

    perform adjust_title_wild_char changing ls_spool-rqtitle.

    if ls_spool-rqtitle ne lv_pev_rqtitle and
      lv_pev_rqtitle is not initial.
      perform print_report_with_formatting using lv_pev_rqtitle .
    endif.
    lv_pev_rqtitle  = ls_spool-rqtitle.

    read table gt_titles with key rqtitle = ls_spool-rqtitle
                 into data(ls_title).
    perform read_spool_with_formatting
      using ls_spool ls_title-spool_format.

  endloop.
  if sy-subrc eq 0.
    perform print_report_with_formatting using  lv_pev_rqtitle .
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  READ_SPOOL_WITH_FORMATTING
*&---------------------------------------------------------------------*
form read_spool_with_formatting  using  p_spool type ty_spool
                                        p_format type zhr_spo1_format.

  data : lt_spool_attr_line  type bapixmspoolid,
         lt_spool_list_plain type list_string_table,
         lt_dummy            type standard table of rspoattr,
         lv_rqattr           type tsp01,
         lv_first_page       type i value 1,
         lv_last_page        type i,
         lv_pages            type c,
         lv_string           type string,
         l_spoolid           type  tsp01-rqident,
         lt_spool_list       type bapixmspow occurs 0 with header line.

  field-symbols <hex_container> type any.

  l_spoolid = p_spool-rqident.

  clear: gs_spool_objects.
  move l_spoolid to gs_spool_objects-spoolid..
  if p_last eq abap_true.
    lv_first_page =  p_spool-rqapprule - p_pages.
    call function 'ZUSE_RETURN_ABAP_SPOOLJOB'
      exporting
        rqident              = l_spoolid
        first_line           = lv_first_page
*       LAST_LINE            =
*       pages                =
      importing
        list_object          = gs_spool_objects-listobject
      exceptions
        no_such_job          = 1
        not_abap_list        = 2
        job_contains_no_data = 3
        selection_empty      = 4
        no_permission        = 5
        can_not_access       = 6
        read_error           = 7
        others               = 8.
  else.
    call function 'ZUSE_RETURN_ABAP_SPOOLJOB'
      exporting
        rqident              = l_spoolid
        first_line           = lv_first_page
*       LAST_LINE            =
*       PAGES                =
      importing
        list_object          = gs_spool_objects-listobject
      exceptions
        no_such_job          = 1
        not_abap_list        = 2
        job_contains_no_data = 3
        selection_empty      = 4
        no_permission        = 5
        can_not_access       = 6
        read_error           = 7
        others               = 8.
  endif.
  if sy-subrc <> 0.
* Implement suitable error handling here
  else.
    append gs_spool_objects to gt_spool_objects.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT_WITH_FORMATTING
*&---------------------------------------------------------------------*
form print_report_with_formatting  using p_rqtitle type tsp01-rqtitle.

  data: lo_table         type ref to cl_salv_table,
        lv_title         type tsp01-rqtitle,
        lv_header        type string,
        l_skip_rec       type abap_bool,
        print_parameters type pri_params,
        valid_flag       type c length 1.

  field-symbols: <hex_container>  type any,
                 <hex_headerline> type any.

  check gt_spool_objects is not initial.

  lv_title = p_rqtitle.
* if first character is a wildcard,  set the title to the report name
  if lv_title+0(1) = '*'.
    concatenate g_jobnam '*' into lv_title.
  endif.
  lv_title+50(5) = '- All'.
*
  if p_rep is initial.               " Report Directly
    new-page print on line-count 65
             line-size 1023
             layout 'X_65_255'
             department 'HR'
             no dialog.
*  new-page print on no dialog.
  endif.

  call function 'SET_PRINT_PARAMETERS'
    exporting
      list_text = lv_title.

* Write report name
  write : / p_rqtitle.
  uline.

  if p_rqtitle cs '*'.
    split p_rqtitle at '*' into data(lv_title_short) data(lv_st4)
                              in character mode.
  endif.

* Write List Objects
  loop at gt_spool_objects into gs_spool_objects.
    check not gs_spool_objects-listobject is initial.

* Write List
    call function 'WRITE_LIST'
      tables
        listobject = gs_spool_objects-listobject
      exceptions
        others     = 1.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endloop.

  if p_rep is initial.        " Report Directly
    new-page print off.
  endif.

  clear gt_spool_objects[].

endform.
