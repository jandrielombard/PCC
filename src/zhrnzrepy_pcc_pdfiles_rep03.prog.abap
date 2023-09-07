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
           gt_payday_files.

* Directory Name from logical file path
  data: lv_lg_file_name(60) value 'HR_NZ_EMS_FILENAME',
        lv_default_path     type string.

  clear: gv_asdir_name, gv_file_mask.
* Read Pay Day Files File path which is maintained in FILE trasaction
  call function 'FILE_GET_NAME'
    exporting
      logical_filename = lv_lg_file_name
      including_dir    = 'X'
    importing
      file_name        = lv_default_path
    exceptions
      others           = 1.
  if sy-subrc <> 0.
    if lv_default_path is initial.
      message e319(3e).
      exit.
    endif.
  endif.

* Server Directory Name
  gv_asdir_name = lv_default_path.

* File Mask
  if r_eds eq abap_true.
    gv_file_text = gc_eds_text.
  else.
    gv_file_text = gc_eis_text.
  endif.
  if noc eq abap_true.
    concatenate p_abkrs p_pabrp p_pabrj gv_file_text into gv_file_mask.
  else.
    concatenate p_abkrs p_pabrp p_pabrj p_ocrsn p_bondt gv_file_text into gv_file_mask.
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

  call method zcl_m99_pcc_utilities=>get_asdir_file_list
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
      call method zcl_m99_pcc_utilities=>raise_longtext_msg
        exporting
          iv_msgid = zcl_m99_pcc_utilities=>gc_pcc_msgcls
          iv_msgty = zcl_m99_pcc_utilities=>gc_msgty_error
          iv_msgno = '000'
          iv_text  = gv_msg
        receiving
          rv_msg   = gv_msg.
      write: /1 gv_msg.
      exit.
    when others.
      gv_msg = |{ text-001 } { gv_asdir_name }| .
      call method zcl_m99_pcc_utilities=>raise_longtext_msg
        exporting
          iv_msgid = zcl_m99_pcc_utilities=>gc_pcc_msgcls
          iv_msgty = zcl_m99_pcc_utilities=>gc_msgty_error
          iv_msgno = '000'
          iv_text  = gv_msg
        receiving
          rv_msg   = gv_msg.
      write: /1 gv_msg.
      exit.
  endcase.

endform.                    "FILE_DIRECTORY_READ

*&---------------------------------------------------------------------*
*&      Form READ_PAYDAY_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form read_payday_file.
* Read exceptional file for the pay run and read file data
  data: lt_dir_list type table of zhraust_pcc_epsfili.
  data: ls_dir_list type zhraust_pcc_epsfili.

  data: lv_document_date(10) type c.
  data: lv_strlen type i.
  data: lv_fileextn type string.
  data  lv_period_info type string.

* Fill In File Attributes list.
  clear: lt_dir_list.
  lt_dir_list[] = gt_dir_list[].

* Populate File Details
  loop at lt_dir_list into ls_dir_list.
    if noc eq abap_true.
      check ( ls_dir_list-name+0(2) = p_abkrs and
              ls_dir_list-name+2(2) = p_pabrp and
              ls_dir_list-name+4(4) = p_pabrj ).
    else.
      check ( ls_dir_list-name+0(2) = p_abkrs and
              ls_dir_list-name+2(2) = p_pabrp and
              ls_dir_list-name+4(4) = p_pabrj and
              ls_dir_list-name+8(8) = p_bondt ).
    endif.

    clear: gs_payday_file.
    gs_payday_file-filename  = ls_dir_list-name.
    gs_payday_file-abkrs = ls_dir_list-name+0(2).
    gs_payday_file-pabrp = ls_dir_list-name+2(2).
    gs_payday_file-pabrj = ls_dir_list-name+4(4).
    gs_payday_file-bondt = ls_dir_list-name+8(10).

    append gs_payday_file to gt_payday_files.
  endloop.

* read file data
  perform read_file_data tables gt_payday_files.

endform.                    "READ_PAYDAY_FILE
