*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_FIRECON_FILEDL04
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM DISPLAY_FILES_FOR_DOWNLOAD                               *
*---------------------------------------------------------------------*
form display_files_for_download
              tables it_sel_fileslist structure gs_audit_file.
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
    gs_files_buttons-text1 = text-004.
    if gv_non_production_system eq abap_true.
      gs_files_buttons-text2 = text-005.
    endif.

* Display List
    sort it_sel_fileslist .
    while  ucom = gc_files.
      gv_alv_marker = 'MARKER'.
      perform display_list tables gt_files_fields
                                  it_sel_fileslist
                            using gs_files_header
                                  gs_files_buttons
                                  gv_files_title.
      check sy-subrc = 0.
      case hr_ret_code.
        when 1.
          prev_ucom = ucom. ucom = gc_download.
          perform download_sel_files tables it_sel_fileslist.
          ucom = prev_ucom. clear: prev_ucom.
        when 2.
          prev_ucom = ucom. ucom = gc_delete.
          if gv_non_production_system eq abap_true.
            perform delete_sel_files tables it_sel_fileslist.
          endif.
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
form files_fieldcat tables it_fieldnames structure gs_fieldnames.
* Jobs list Report Columns
  refresh: it_fieldnames, gt_fieldnames.

  perform initf using 'Filename'
                      'ZHRAUST_PCC_EPSFILI' 'NAME'   space .
  perform initf using 'Process ID'
                      'PYC_D_PYP' 'ID'   space .
  perform initf using 'Process Name'
                      'PYC_D_PYPT' 'NAME'   space .
  perform initf using 'Process Instance'
                      'PYC_D_PYPI' 'PYPI_ID'   space .
*  perform initf using 'Time Selection'
*                      '' ''   space .
  perform initf using 'Period Start date'
                      'T549Q' 'BEGDA'   space .
  perform initf using 'Period End date'
                      'T549Q' 'ENDDA'   space .
  perform initf using 'Off-cycle payroll payment date'
                      'PYC_S_OC_INFO' 'BONDT'   space .
  perform initf using 'Message'
                      'BAPIRETURN1' 'MESSAGE'   space .

  it_fieldnames[] = gt_fieldnames[].
*
endform.                   " JOBS_FIELDCAT

*---------------------------------------------------------------------*
*      FORM DOWNLOAD_SEL_FILES                                        *
*---------------------------------------------------------------------*
form download_sel_files tables it_fileslist structure gs_audit_file.
* Download Selecetd Files
  data: lt_fileslist like table of gs_audit_file.
  data: lv_lines type i.
  data: lv_asfile type string.
  data: lv_psfile type string.
  data: lv_xstring type xstring.
  data: lv_index type sy-tabix.

* Check selection.

  loop at it_fileslist into gs_audit_file
      where marker eq abap_true.
    lv_index = sy-tabix.
    clear: go_cx_auditfl.

    try.
        concatenate gv_asdir_name gs_audit_file-filename into lv_asfile.
        call method zusecl_m99_pcc_utilities=>read_asfile_to_binstring
          exporting
            iv_asfile    = lv_asfile
          receiving
            rv_binstring = lv_xstring.

        if not lv_xstring is initial.
          " Download files to local directory
          concatenate gv_psdir_name gs_audit_file-filename into lv_psfile.
          call method zusecl_m99_pcc_utilities=>save_binstring_to_psfile
            exporting
              iv_filename  = lv_psfile
              iv_binstring = lv_xstring.
        endif.
     " catch  "zcx_fi_recon into go_cx_auditfl.
    endtry.
* Report errors
    if not go_cx_auditfl is initial.
      gs_audit_file-msg = go_cx_auditfl->get_text( ).
    else.
      message i063(zhrpy_pcc_msg) into gs_audit_file-msg.
    endif.
    modify it_fileslist from gs_audit_file index lv_index transporting msg.
  endloop.

endform.                            " DOWNLOAD_SEL_FILES
*---------------------------------------------------------------------*
*      FORM DELETE_SEL_FILES                                        *
*---------------------------------------------------------------------*
form delete_sel_files tables it_fileslist structure gs_audit_file.
* Delete Selecetd Files
  data: lv_index type sy-tabix.
  data: lv_filename type string.
  data: lv_lines type i.

* delete all selected files from the list
  loop at it_fileslist into gs_audit_file
      where marker eq abap_true.
    lv_index = sy-tabix.

    concatenate gv_asdir_name gs_audit_file-filename into lv_filename.
    call method zusecl_py_fi_recon_utilities=>delete_asdir_file
      exporting
        iv_file_name     = lv_filename
      exceptions
        no_authorization = 1
        file_open_error  = 2
        others           = 3.
    case sy-subrc.
      when 0.
        delete it_fileslist index lv_index.
      when 1.
        message i066(zhrpy_pcc_msg) into gs_audit_file-msg.
        modify it_fileslist from gs_audit_file index lv_index transporting msg.
      when 2.
        message i067(zhrpy_pcc_msg) into gs_audit_file-msg.
        modify it_fileslist from gs_audit_file index lv_index transporting msg.
    endcase.
  endloop.
*
  describe table it_fileslist lines lv_lines.
  if lv_lines eq 0.
    message i068(zx_py_fi_recon_msg) into gv_msg.
    skip 1.
    write: /1 gv_msg.
  endif.

endform.                            " DELETE_SEL_FILES
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
