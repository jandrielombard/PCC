*----------------------------------------------------------------------*
* INCLUDE ZHRAUREPY_PCC_EHIUPD_FRM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      FORM  CHANGE_SCREEN_INPUT
*&---------------------------------------------------------------------*
form change_screen_input.
* Display Only Fields
  loop at screen.
    if screen-name = 'P_PROCNM'.
*     screen-active    = '0'.
      screen-input     = '0'.
    endif.
    modify screen.
  endloop.

endform.                    "CHANGE_SCREEN_INPUT
*&---------------------------------------------------------------------*
*&      FORM  GET_PROCID_NAME
*&---------------------------------------------------------------------*
form get_procid_name using p_procid type pyc_d_pyp-id
                  changing p_procnm type pyc_d_pypt-name.
* Read Process Name
  check p_procid is not initial.
  select  single name into p_procnm
    from  pyc_d_pypt
   where  sprsl = sy-langu and id = p_procid.

endform.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
form initialization.
* Initialize Selection
  gv_proc_id = p_procid.
  gv_pccval = p_pccval.
  move-corresponding s_emp_so[] to gt_pernr_so[].

endform.
*&--------------------------------------------------------------------*
*&      Form  DISPLAY_BATCH_JOBS_EMPLOYEES
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form display_batch_jobs_employees
              tables it_emplist structure gs_emplist.

  data lt_emplist type zusecl_m99_pcc_ehiupdate=>tty_emplist.

* Display only employees with Cancelled status
  lt_emplist = gt_emplist.
  delete lt_emplist
   where not repro eq zusecl_m99_pcc_ehiupdate=>gc_repro_status-cancelled.
* Display Employees list
  clear gv_alv_marker.
  perform display_employees_list tables lt_emplist.

endform.                    "DISPLAY_BATCH_JOBS_EMPLOYEES
*&--------------------------------------------------------------------*
*&      Form  DISPLAY_BATCH_JOBS_LIST
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form display_batch_jobs_list tables it_sel_joblist structure gs_proc_jobslist.
* Display Jobs for Selection
  data prev_ucom like sy-ucomm.
  data lt_jobslist type zusecl_m99_pcc_ehiupdate=>tty_proc_jobslist.
  data ls_jobslist type zusecl_m99_pcc_ehiupdate=>ty_proc_jobslist.
  data lt_emplist type zusecl_m99_pcc_ehiupdate=>tty_emplist.
  data ls_emplist type zusecl_m99_pcc_ehiupdate=>ty_emplist.

* Batch Jobs List.
  ucom = gc_jobs.
  lt_jobslist[] = it_sel_joblist[].
  if not lt_jobslist[] is initial.
* Header & Field Catalog
    clear: gs_jobs_header, gs_jobs_buttons, gv_jobs_title.
    gv_jobs_title =  sy-title.

    clear: gs_jobs_header.
    gs_jobs_header-header1 = text-003.

    if gt_jobs_fields[] is initial.
      perform jobs_fieldcat tables gt_jobs_fields.
    endif.

    clear: gs_jobs_buttons.
    gs_jobs_buttons-text1 = text-004.
    gs_jobs_buttons-text2 = text-005.

* Display List
    sort lt_jobslist by strtdate descending strttime descending jobname.
    while  ucom = gc_jobs.
      gv_alv_marker = 'MARKER'.
      perform display_list tables gt_jobs_fields
                                  lt_jobslist
                            using gs_jobs_header
                                  gs_jobs_buttons
                                  gv_jobs_title.
      check sy-subrc = 0.
      case hr_ret_code.
        when 1.
          prev_ucom = ucom. ucom = gc_step.
          call method zusecl_m99_pcc_ehiupdate=>display_job_steps
            exporting
              it_jobslist = lt_jobslist[].
          ucom = prev_ucom. clear: prev_ucom.
        when 2.
          prev_ucom = ucom. ucom = gc_ehiupd.
          loop at lt_jobslist into ls_jobslist
              where marker eq abap_true.
            loop at gt_emplist into ls_emplist
                where pypi_id eq ls_jobslist-pypi_id
                  and jobname eq ls_jobslist-jobname
                  and jobcount eq ls_jobslist-jobcount.
              append ls_emplist to lt_emplist.
            endloop.
          endloop.

          " Add only employees with Cancelled status
          delete lt_emplist
              where not repro eq zusecl_m99_pcc_ehiupdate=>gc_repro_status-cancelled.
          call method zusecl_m99_pcc_ehiupdate=>add_employees_to_ehitbl
            changing
              ct_emplist = lt_emplist.

          clear gv_alv_marker.
          perform display_employees_list tables lt_emplist.

          ucom = prev_ucom. clear: prev_ucom.
        when others.
          ucom = sy-ucomm.
      endcase.
    endwhile.

  endif.

endform.                    "DISPLAY_BATCH_JOBS_LIST
*---------------------------------------------------------------------*
*       FORM JOBS_FIELDCAT                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form jobs_fieldcat tables it_fieldnames structure gs_fieldnames.
* Jobs list Report Columns
  refresh: it_fieldnames, gt_fieldnames.

  perform initf using 'Pro Status'
                      '' ''   space .
  perform initf using 'Process ID'
                      'PYC_D_PYP' 'ID'   space .
  perform initf using 'Process Name'
                      'PYC_D_PYPT' 'NAME'   space .
  perform initf using 'Process Instance ID'
                      'PYC_D_PYPI' 'PYPI_ID'   space .
  perform initf using 'Process Step ID'
                      'PYC_D_BPC' 'STEP_ID'   space .
  perform initf using 'Process Step Name'
                      'PYC_S_STEP_INST_RES' 'STEP_NAME'   space .
  perform initf using 'Job Name'
                      'TBTCO' 'JOBNAME'   space .
  perform initf using 'Job Count'
                      'TBTCO' 'JOBCOUNT'  space .
  perform initf using 'Start Date'
                      'TBTCO' 'STRTDATE'  space .
  perform initf using 'Start Time'
                      'TBTCO' 'STRTTIME'  space .
  perform initf using 'Status'
                      'TBTCO' 'STATUS'    space .

  it_fieldnames[] = gt_fieldnames[].
*
endform.                   " JOBS_FIELDCAT
*&--------------------------------------------------------------------*
*&      Form  DISPLAY_EMPLOYEES_LIST
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form display_employees_list
    tables it_emplist structure gs_emplist.
* Employees list
  data: lv_lines type i.
  data: lv_char_lines(10) type c.
  data: lv_string type string.
  data: lt_emplist type zusecl_m99_pcc_ehiupdate=>tty_emplist.

  if not it_emplist[] is initial.
* Header & Field Catalog
    clear: gs_emps_header, gs_emps_buttons, gv_emps_title.
    gv_emps_title =  sy-title.

    clear: gs_emps_header.
    gs_emps_header-header1 = text-008.

    " Number of Jobs
    lt_emplist[] = it_emplist[].
    delete adjacent duplicates from lt_emplist comparing jobname jobcount strtdate strttime.
    describe table lt_emplist lines lv_lines.  move lv_lines to lv_char_lines.
    concatenate text-006 lv_char_lines into lv_string separated by space.
    gs_emps_header-header2 = lv_string.

    describe table it_emplist lines lv_lines. move lv_lines to lv_char_lines.
    concatenate text-007 lv_char_lines into lv_string separated by space.
    gs_emps_header-header3 = lv_string.

    if gt_emps_fields[] is initial.
      perform emps_fieldcat tables gt_emps_fields.
    endif.

    clear: gs_emps_buttons.
* Display List
    sort it_emplist by strtdate strttime jobname pernr.
    perform display_list tables gt_emps_fields
                                it_emplist
                          using gs_emps_header
                                gs_emps_buttons
                                gv_emps_title.
  endif.

endform.                    "DISPLAY_BATCH_JOBS_LIST
*---------------------------------------------------------------------*
*       FORM EMPS_FIELDCAT                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form emps_fieldcat tables it_fieldnames structure gs_fieldnames.
*
  refresh: it_fieldnames, gt_fieldnames.

  perform initf using 'Pro Status'
                      '' ''   space .
  perform initf using 'Job Name'
                      'TBTCO' 'JOBNAME'   space .
  perform initf using 'Job Count'
                      'TBTCO' 'JOBCOUNT'  space .
  perform initf using 'Start Date'
                      'TBTCO' 'STRTDATE'  space .
  perform initf using 'Start Time'
                      'TBTCO' 'STRTTIME'  space .
  perform initf using 'Process Instance ID'
                      'PYC_D_PYPI' 'PYPI_ID'   space .
  perform initf using 'Personnal Number'
                      'PA0001' 'PERNR'   space .
  perform initf using 'Message'
                      'BAPIRETURN1' 'MESSAGE'   space .

  it_fieldnames[] = gt_fieldnames[].
*
endform.                   " EMPS_FIELDCAT
*---------------------------------------------------------------------*
*       FORM DISPLAY_LIST                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IT_FIELDNAMES                                                 *
*  -->  IT_DATA                                                       *
*  -->  IS_HEADER                                                     *
*  -->  IS_BUTTONS                                                    *
*  -->  IV_TITLE                                                      *
*---------------------------------------------------------------------*
form display_list tables it_fieldnames
                         it_data
                  using  is_header structure gs_jobs_header
                         is_buttons structure gs_jobs_buttons
                         iv_title like sy-title.
* Display Report
  call function 'DISPLAY_GRID_LIST'
    exporting
      basic_list_title     = iv_title
      file_name            = myreport
      head_line1           = is_header-header1
      head_line2           = is_header-header2
      head_line3           = is_header-header3
      dyn_pushbutton_text1 = is_buttons-text1
      dyn_pushbutton_text2 = is_buttons-text2
      alv_marker           = gv_alv_marker
    importing
      return_code          = hr_ret_code
    tables
      data_tab             = it_data
      fieldname_tab        = it_fieldnames
    exceptions
      alv_error            = 1
      others               = 2.

endform.                           "DISPLAY_LIST
*&---------------------------------------------------------------------*
*&      Form  INITF
*&---------------------------------------------------------------------*
* Initialise HR_DISPLAY_BASIC_LIST column header and field FI help
* reference table and field name.
*----------------------------------------------------------------------*
*      -->field1   Column header
*      -->field2   Ref. Table name
*      -->field3   Ref. Field name
*      -->field4   Display
*----------------------------------------------------------------------*
form initf using field1 field2 field3 field4 .

  clear gs_fieldnames.
  gs_fieldnames-text         = field1.
  gs_fieldnames-reftabname   = field2.
  gs_fieldnames-reffieldname = field3.
  gs_fieldnames-typ          = field4.
  append gs_fieldnames to gt_fieldnames.

endform.                               "INIT_FIELDS
