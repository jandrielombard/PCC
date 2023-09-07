*----------------------------------------------------------------------*
* INCLUDE ZHRAUREPY_PCC_MDLOG_EHIUPD_FRM.
*----------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Form  DISPLAY_EMPLOYEES_LIST
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form display_employees_list tables it_sel_empslist structure gs_empslist.
* And finally report the employees for the payroll admins verification
  data: prev_ucom like sy-ucomm.

* Changed Employees List
  ucom = gc_emps.
  if not it_sel_empslist[] is initial.
* Header & Field Catalog
    clear: gs_emps_header, gs_emps_buttons, gv_emps_title.
    gv_emps_title =  sy-title.

    clear: gs_emps_header.
    gs_emps_header-header1 = text-003.
    message i042(zhrpy_pcc_msg) with p_begda p_begtz p_endda p_endtz into gs_emps_header-header2.

    if gt_emps_fields[] is initial.
      perform emps_fieldcat tables gt_emps_fields.
    endif.
    clear: gs_emps_buttons.

* Display List
    sort it_sel_empslist by pernr bdate btime seqnr.
    while  ucom = gc_emps.
      gv_alv_marker = ''.
      perform display_list tables gt_emps_fields
                                  it_sel_empslist
                            using gs_emps_header
                                  gs_emps_buttons
                                  gv_emps_title.
      check sy-subrc = 0.
      ucom = sy-ucomm.
    endwhile.

  endif.

endform.                    "DISPLAY_EMPLOYEES_LIST
*---------------------------------------------------------------------*
*       FORM EMPS_FIELDCAT                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form emps_fieldcat tables it_fieldnames structure gs_fieldnames.
* Jobs list Report Columns
  refresh: it_fieldnames, gt_fieldnames.

  perform initf using 'Personnel Number'
                      'PLDOC_KEY' 'PERNR'   space .
  perform initf using 'Infotype'
                      'PLDOC_KEY' 'INFTY'  space .
  perform initf using 'Change Date'
                      'PLDOC_KEY' 'BDATE'  space .
  perform initf using 'Change Time'
                      'PLDOC_KEY' 'BTIME'  space .
  perform initf using 'Seq Num'
                      'PLDOC_KEY' 'SEQNR'    space .
  perform initf using 'User Name'
                      'PLDOC_KEY' 'UNAME'   space .
  perform initf using 'Program Id'
                      'PCL4' 'PGMID'   space .
*  perform initf using 'Message'
*                      'BAPIRETURN1' 'MESSAGE'   space .

  it_fieldnames[] = gt_fieldnames[].
*
endform.                   " JOBS_FIELDCAT
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
                  using  is_header structure gs_emps_header
                         is_buttons structure gs_emps_buttons
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
