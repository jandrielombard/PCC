"Name: \PR:PYC_SUPPORT_DL_AUDIT_TRAIL\FO:DOWNLOAD_EXCEL\SE:BEGIN\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_DL_AUDIT_TRAIL.
*-----------------------------------------------------------------------*
* Description   : Wrapper program to execute standard SAP program       *
*                 PYC_SUPPORT_DL_AUDIT_TRAIL in automated way, so that  *
*                 Download of Audit Trail for PCC Payroll Process       *
*                 Instances can be scheduled as a background job        *
*                                                                       *
* This Enhancement to standard SAP program is using the ZIP file        *
* download option to avoid any formating issues                         *
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |22-DEC-2022 |1130848  |Initial creation             |CFAK902680   *
*    |            |         |                             |CFAK902839   *
*    |            |         |                             |CFAK902842   *
*-----------------------------------------------------------------------*
  " Only for Batch Run and Wrapper program Submission
  data: lv_wow_xstring           type xstring,
        l_wow_length             type i,
        lv_wow_raw               type xstring,
        lv_wow_validation_active type boole_d.
  data: lv_wow_proc_id   type pyc_proc_id,
        lv_wow_proc_name type pyc_proc_name,
        lv_new_file_name type string.
  data: lo_zip      type ref to cl_abap_zip.
  data: lv_subrc   type sy-subrc,
        lv_wow_msg type bapi_msg.
  constants: lc_zip(4)  type c value '.zip'.

  constants: lc_struc_name1 type dd02l-tabname value 'ZHRAUSTPY_PI_ALH_CSV_EXPORT',
             lc_struc_name2 type dd02l-tabname value 'ZHRAUSTPY_PROC_INST_PH_EXPORT',
             lc_struc_name3 type dd02l-tabname value 'ZHRAUSTPY_KPI_AL_EXPORT'.

  data: lt_pi_ph            type  zhrauttpy_proc_inst_ph.
  data: lt_pi_alh_export    type  zhrauttpy_pi_alh_csv_export.
  data: lt_pi_ph_export     type  zhrauttpy_proc_inst_ph_export.
  data: lt_pi_kpi_al_export type  zhrauttpy_kpi_al_export.
  data: lt_attachment       type  cl_pyc_process_manager_mpc=>tt_attachment.
  data: ls_attachment       type  cl_pyc_process_manager_mpc=>ts_attachment.

  if sy-batch eq abap_true and p_dir eq cl_pyc_pi_alh_aux=>gc_logical_path.
    " Change the file name, Include Process Template name and change file extention to ZIP
    lv_wow_proc_id = lv_file_name+0(25).
    select single name into lv_wow_proc_name
      from pyc_d_pypt
     where sprsl = 'E' and id = lv_wow_proc_id.

    lv_new_file_name = |{ lv_wow_proc_name+0(4) }_{ lv_file_name }|.
    replace all occurrences of lv_file_name in lv_full_file_name with lv_new_file_name.
    replace all occurrences of c_url_end in lv_full_file_name with lc_zip.

    perform init.
    check lo_spreadsheet is bound.
*        perform fill_data.

    if c_pi_al eq 'X'.
      sort lt_pi_alh by tsl_char descending.
      lt_pi_alh_export = zusecl_m99_pcc_dl_audit_trail=>convert_alert_log_tsl( lt_pi_alh ).
      perform feed_sheets tables lt_pi_alh_export using c_sheet1 lc_struc_name1 .
    endif.

    if c_pi eq 'X'.
      clear: lt_filter,ls_filter,ls_filter-select_options.
      ls_filter-property = cl_pyc_rt_facade=>gcs_filter-step_inst_al-proc_inst_id. " 'ProcessInstanceID'
      append value #( sign = 'I' option = 'EQ' low = lv_pi ) to ls_filter-select_options.
      append ls_filter to lt_filter.
      refresh: lt_pi_al.
      lt_pi_al = lo_pyc_rt_facade->v3_pi_action_log_gl( iv_convert_msg_vars = abap_false it_filter = lt_filter ).
      lt_pi_ph = zusecl_m99_pcc_dl_audit_trail=>enrich_process_log( lt_pi_al ).
      sort lt_pi_ph by tsl descending.
      lt_pi_ph_export = zusecl_m99_pcc_dl_audit_trail=>convert_process_log_tsl( lt_pi_ph ).
      perform feed_sheets tables lt_pi_ph_export using c_sheet2 lc_struc_name2 .
    endif.

    if c_pi_kpi eq 'X'.
      if lv_is_tsk eq abap_true.
        sort lt_pi_tskkpi_al by tsl descending.
        perform feed_sheets tables lt_pi_tskkpi_al using c_sheet3 c_struc_name4 .
      else.
        sort lt_pi_kpi_al by tsl descending.
        lt_pi_kpi_al_export = zusecl_m99_pcc_dl_audit_trail=>convert_analytics_log_tsl( lt_pi_kpi_al ).
        perform feed_sheets tables lt_pi_kpi_al_export using c_sheet3 lc_struc_name3 .
      endif.
    endif.
* Audit Trail in Excel file
    lo_doc_data = lo_spreadsheet->get_document( ).

* Read Process instance Attachments
    refresh: lt_attachment.
    call method zusecl_m99_pcc_dl_audit_trail=>get_proc_inst_atachements
      exporting
        iv_pi          = lv_pi
        it_process_log = lt_pi_al
*       it_alert_item  =
      importing
        et_attachment  = lt_attachment.

*   Create ZIP file to avoid file format and encoding errors
    create object lo_zip.
*   Audit Trail File
    lo_zip->add( name    = lv_new_file_name
                 content = lo_doc_data ).
*   All Attachment for the instance
    loop at lt_attachment into ls_attachment.
      lo_zip->add( name    = ls_attachment-file_name
                   content = ls_attachment-content ).
    endloop.
    lv_wow_xstring = lo_zip->save( ).

    call function 'FILE_VALIDATE_NAME'
      exporting
        logical_filename           = cl_pyc_pi_alh_aux=>gc_logical_path
      importing
        validation_active          = lv_wow_validation_active
      changing
        physical_filename          = lv_full_file_name
      exceptions
        logical_filename_not_found = 1
        validation_failed          = 2
        others                     = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    check lv_wow_validation_active = abap_true.
    data(lv_wow_dset) = lv_full_file_name.
    delete dataset lv_wow_dset.

    lv_subrc = 4.
    call method zusecl_m99_pcc_utilities=>save_binstring_to_asfile
      exporting
        iv_filename  = lv_full_file_name
        iv_binstring = lv_wow_xstring
      importing
        ev_msg       = lv_wow_msg
        ev_is_error  = lv_subrc.

    if lv_subrc eq 0.
      ls_log-message_icon = icon_led_green.
      ls_log-file_path = lv_full_file_name.
      ls_log-file_rows = lines( lt_sheet_names ).
      ls_log-message_text = text-t07.
      append ls_log to lt_log.
    else.
      ls_log-message_icon = icon_led_red.
      ls_log-file_path = lv_full_file_name.
      ls_log-message_text = lv_wow_msg.
      append ls_log to lt_log.
    endif.

    clear: lo_doc, lo_spreadsheet, lo_doc_data.
    free lo_spreadsheet.

    exit.
  endif.
ENDENHANCEMENT.
