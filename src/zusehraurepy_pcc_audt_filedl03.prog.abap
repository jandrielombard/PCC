*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_FIRECON_FILEDL03
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_GLOBAL_VARIABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form initialize_global_variables.
* Initilize Variables
  refresh: gt_dir_list,
           gt_audit_files.

* Directory Name  and File mask
  call method zusecl_m99_pcc_utilities=>get_audit_files_asdir
    importing
      ev_asdir_name = gv_asdir_name
      ev_file_mask  = gv_file_mask.

* Local PC File directory
  gv_psdir_name = p_psdir.

* Check System
  select single * from t000 where mandt eq sy-mandt.
  if t000-cccategory <> 'P'.
    gv_non_production_system = abap_true.
  endif.

endform.                    "INITIALIZE_GLOBAL_VARIABLES

*&---------------------------------------------------------------------*
*&      Form  FILE_DIRECTORY_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DIR_NAME text
*----------------------------------------------------------------------*
form file_directory_read.
* Read Files from FI Reconciliation Report Directory
  refresh: gt_dir_list.
  clear: gv_okfile_counter, gv_error_counter.

  call method zusecl_m99_pcc_utilities=>get_asdir_file_list
    exporting
      iv_asdir_name         = gv_asdir_name
      iv_file_mask          = gv_file_mask
    importing
      ev_dir_name           = gv_asdir_name
      ev_file_counter       = gv_okfile_counter
      ev_error_counter      = gv_error_counter
      et_dir_list           = gt_dir_list
    exceptions
      read_directory_failed = 1
      too_many_read_errors  = 2
      empty_directory_list  = 3
      others                = 4.

* Implement suitable error handling here
  case sy-subrc .
    when 0.
    when 3.
      gv_msg = |{ text-002 } { gv_asdir_name } { text-007 }  { gv_file_mask }| .
      call method zusecl_m99_pcc_utilities=>raise_longtext_msg
        exporting
          iv_msgid = zusecl_m99_pcc_utilities=>gc_pcc_msgcls
          iv_msgty = zusecl_m99_pcc_utilities=>gc_msgty_error
          iv_msgno = '000'
          iv_text  = gv_msg
        receiving
          rv_msg   = gv_msg.
      write: /1 gv_msg.
      exit.
    when others.
      gv_msg = |{ text-001 } { gv_asdir_name }| .
      call method zusecl_m99_pcc_utilities=>raise_longtext_msg
        exporting
          iv_msgid = zusecl_m99_pcc_utilities=>gc_pcc_msgcls
          iv_msgty = zusecl_m99_pcc_utilities=>gc_msgty_error
          iv_msgno = '000'
          iv_text  = gv_msg
        receiving
          rv_msg   = gv_msg.
      write: /1 gv_msg.
      exit.
  endcase.

endform.                    "FILE_DIRECTORY_READ

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_SORT_ATTRIBUTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_DIR_LIST  text
*----------------------------------------------------------------------*
form assign_sort_attributes.
*
  data: lt_dir_list type table of zhraust_pcc_epsfili.
  data: ls_dir_list type zhraust_pcc_epsfili.

  data: lv_document_date(10) type c.
  data: lv_strlen type i.
  data: lv_file_name type string.
  data: lv_fileextn type string.
  data  lv_period_info type string.

  data: lv_payroll_area type tvarv_val,
        lv_pabrj        type pabrj,
        lv_pabrp        type pabrp,
        lv_vabrj        type vabrj,
        lv_vabrp        type vabrp,
        lv_abkrs        type abkrs,
        lv_begda        type begda,
        lv_endda        type endda,
        lo_payroll_area type ref to cl_hr_payroll_area.
*
* Fill In File Attributes list.
  clear: lt_dir_list.
  lt_dir_list[] = gt_dir_list[].

* Get Process ID details
  select a~id a~molga a~pypte_category b~value c~name
    into corresponding fields of table gt_pyp_dtls
    from pyc_d_pyp as a
         inner join pyc_d_pypisp as b on a~id = b~id
         inner join pyc_d_pypt   as c on a~id = c~id
   where c~sprsl = 'E'.

* Populate File Details
  loop at lt_dir_list into ls_dir_list.
*
    clear: gs_audit_file.
    gs_audit_file-filename  = ls_dir_list-name.
    gs_audit_file-proc_id  = ls_dir_list-name+5(25).

    split ls_dir_list-name at '.' into lv_file_name lv_fileextn.
    gs_audit_file-pypi_id = lv_file_name+5.

    " Process details
    clear gs_pyp_dtls.
    read table gt_pyp_dtls into gs_pyp_dtls
      with key id = gs_audit_file-proc_id.

    if sy-subrc eq 0.
      gs_audit_file-pyp_name = gs_pyp_dtls-name.
* Period Info
      lv_period_info = gs_audit_file-pypi_id+26.
      lv_strlen = strlen( lv_period_info ).
      case lv_strlen.
        when 6.
          lv_abkrs = gs_pyp_dtls-value.
          if not lv_abkrs is initial.
            lo_payroll_area = cl_hr_payroll_area=>get_instance( imp_area = lv_abkrs ).
            lv_pabrj = lv_period_info(4).
            lv_pabrp = lv_period_info+4(2).

            lo_payroll_area->get_period_info(
              exporting
                imp_pabrj = lv_pabrj
                imp_pabrp = lv_pabrp
              importing
                exp_vabrj = lv_vabrj
                exp_vabrp = lv_vabrp
                exp_begda = lv_begda
                exp_endda = lv_endda ).

            gs_audit_file-period_begda = lv_begda.
            gs_audit_file-period_endda = lv_endda.
          endif.
        when others.
          gs_audit_file-bondt = gs_audit_file-pypi_id+26(8).
      endcase.
*    time_sel_par_val

      append gs_audit_file to gt_audit_files.
    endif.
  endloop.

endform.                    "ASSIGN_SORT_ATTRIBUTES
*&---------------------------------------------------------------------*
*&      Form  f4_path
*&---------------------------------------------------------------------*
form f4_local_directory  changing p_localdir.
* Local Directory Selection
  data: lv_path         type string,
        lv_full_path    type string,
        lv_window_title type string.

  lv_window_title = text-001.
  call method cl_gui_frontend_services=>directory_browse
    exporting
      window_title    = lv_window_title
      initial_folder  = lv_path
    changing
      selected_folder = lv_full_path.

  call method cl_gui_cfw=>flush.
  concatenate lv_full_path '\' into p_localdir.

endform.
