*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_FIRECON_FILEDL01
*&---------------------------------------------------------------------*
tables: t000, t549t.

* For reconcilation report output
constants:
  c_rp_head  type c value '1',
  c_rp_data  type c value '2',
  c_rp_total type c value '3'.

data: gv_non_production_system type boolean.
data: gv_asdir_name     type  epsf-epsdirnam,
      gv_psdir_name     type  string,
      gv_file_mask      type  epsf-epsfilnam value space,
      gv_okfile_counter type  epsf-epsfilsiz,
      gv_error_counter  type  epsf-epsfilsiz,
      gt_dir_list       type table of zhraust_pcc_epsfili.
data: gt_t549t type table of t549t.

ranges: r_file_mask for admi_files-filename.
data: g_sysid type syst-sysid,
      g_mandt type mandt.
ranges: r_sysid for syst-sysid.

types: begin of ty_pyp_dtls,
         id             type pyc_proc_id,
         molga          type molga,
         pypte_category type pyc_proc_templ_cat,
         value          type pyc_par_val,
         name           type pyc_proc_name.
types: end of ty_pyp_dtls.
data: gt_pyp_dtls type table of ty_pyp_dtls.
data: gs_pyp_dtls type ty_pyp_dtls.

types: begin of ty_audit_file,
         filename     type fileextern,
         proc_id      type pyc_proc_id,
         pyp_name     type pyc_d_pypt-name,
         pypi_id      type pyc_proc_inst_id,
*         time_sel_par_val type tvarv_val,
         period_begda type begda,
         period_endda type endda,
         bondt        type bondt,
         msg          type bapi_msg,
         marker       type xfeld.
types:   end of ty_audit_file.
data: gt_audit_files type table of ty_audit_file .
data: gs_audit_file type ty_audit_file.
data: go_cx_auditfl type ref to cx_root,
      gv_msg        type string.


* Used in HR BASIC DISPLAY LIST
constants: gc_files    type sy-ucomm value 'DISPLAYFILES',
           gc_download type sy-ucomm value 'DOWLOADFILES',
           gc_delete   type sy-ucomm value 'DELETEFILES'.

data: t_color type slis_t_specialcol_alv with header line.
data: ucom    like sy-ucomm value 'DISPLAYFILES'.

types: begin of ty_header,
         header1(132),
         header2(132),
         header3(132),
         footnote1(132),
         footnote2(132),
         footnote3(132).
types: end of ty_header.
data: gs_files_header type ty_header.
types: begin of ty_buttons,
         text1(40),
         text2(40),
         text3(40),
         text4(40),
         text5(40),
         text6(40).
types: end of ty_buttons.
data: gs_files_buttons type ty_buttons.

data: gv_title       like sy-title,
      gv_files_title like gv_title.

data: myreport          like sy-repid,
      datum(10),
      gv_list_level(2),
      gv_alv_coltab(10),
      gv_alv_marker(10) value 'MARKER',
      gv_no_alv_grid.

data: hr_ret_code like  sy-subrc.

data: gt_fieldnames type table of zhraustpy_pcc_fldnames.
data: gs_fieldnames type zhraustpy_pcc_fldnames.
data: gt_files_fields type table of zhraustpy_pcc_fldnames.

* Selection Screen
selection-screen begin of block frm1 with frame title text-f01.
parameters: p_psdir type string default 'C:\temp\' lower case.
selection-screen end of block frm1.

at selection-screen on value-request for p_psdir.
  call method zcl_py_fi_recon_utilities=>f4_localfile_directory
    changing
      cv_localdir = gv_psdir_name.

  p_psdir = gv_psdir_name.
