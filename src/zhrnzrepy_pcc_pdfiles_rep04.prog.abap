*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_FIRECON_FILEDL04
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM DISPLAY_PAYDAYREP_EXCEPTIONS                             *
*---------------------------------------------------------------------*
form display_paydayrep_exceptions
              tables it_sel_fileslist structure gs_error.
* Display Files to users for the next actions
  data: prev_ucom like sy-ucomm.
* FI Reconciliation Files List.
  ucom = gc_files.
  if not it_sel_fileslist[] is initial.
* Header & Field Catalog
    clear: gs_files_header, gs_files_buttons, gv_files_title.
    gv_files_title =  sy-title.

    clear: gs_files_header.
    gs_files_header-header1 = text-003.

    if gt_files_fields[] is initial.
      perform files_fieldcat tables gt_files_fields.
    endif.

    clear: gs_files_buttons.

* Display List
    sort it_sel_fileslist .
    perform display_list tables gt_files_fields
                                it_sel_fileslist
                          using gs_files_header
                                gs_files_buttons
                                gv_files_title.
  endif.

endform.                    "DISPLAY_PAYDAYREP_EXCEPTIONS
*---------------------------------------------------------------------*
*       FORM JOBS_FIELDCAT                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form files_fieldcat tables it_fieldnames structure gs_fieldnames.
* Jobs list Report Columns
  refresh: it_fieldnames, gt_fieldnames.

  perform initf using 'Status'
                      '' ''   space .
  perform initf using 'Personal Number'
                      'PERNR' 'PERNR'   space .
  perform initf using 'Employee Name'
                      'P0001' 'SNAME'   space .
  perform initf using 'Message'
                      'BAPIRETURN1' 'MESSAGE'   space .

  it_fieldnames[] = gt_fieldnames[].
*
endform.                   " JOBS_FIELDCAT

*---------------------------------------------------------------------*
*      FORM READ_FILE_DATA                                            *
*---------------------------------------------------------------------*
form read_file_data tables it_fileslist structure gs_payday_file.
* Download Selecetd Files
  data: lt_fileslist like table of gs_payday_file.
  data: lv_lines type i.
  data: lv_asfile type string.
  data: lv_xstring type xstring.
  data: lv_string type string.

* Read all files data
  loop at it_fileslist into gs_payday_file.
    clear: go_cx_paydayfl, lv_xstring, lv_string.
    try.
        concatenate gv_asdir_name gs_payday_file-filename into lv_asfile.
        call method zcl_m99_pcc_utilities=>read_asfile_to_binstring
          exporting
            iv_asfile    = lv_asfile
          receiving
            rv_binstring = lv_xstring.

        if not lv_xstring is initial.
          call function 'HR_KR_XSTRING_TO_STRING'
            exporting
              in_xstring = lv_xstring
            importing
              out_string = lv_string.

          clear: gt_efile.
          split lv_string at cl_abap_char_utilities=>cr_lf into table gt_efile.

          loop at gt_efile into gs_efile.
            clear gs_error.
            split gs_efile at ','
              into gs_error-icon gs_error-pernr gs_error-ee_name gs_error-message.
            append gs_error to gt_error.
          endloop.
        endif.
      catch zcx_fi_recon into go_cx_paydayfl.
    endtry.
  endloop.

endform.                            " READ_FILE_DATA
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
                  using  is_header structure gs_files_header
                         is_buttons structure gs_files_buttons
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
*----------------------------------------------------------------------*
