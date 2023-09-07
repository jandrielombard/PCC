*&---------------------------------------------------------------------*
*&  Include ZHRAUREPY_PCC_SPOOL_MERGE_TOP
*&---------------------------------------------------------------------*
tables: pernr, tbtco.

constants: gc_jobs   type sy-ucomm value 'JOBS',
           gc_step   type sy-ucomm value 'STEP',
           gc_ehiupd type sy-ucomm value 'EHIUPD'.

data: t_color type slis_t_specialcol_alv with header line.
data: ucom    like sy-ucomm value 'JOBS'.
field-symbols: <datatab> type table.
*
data gv_proc_id type pyc_d_pyp-id.
data gv_pccval  type boolean.
data: go_ehiupdate type ref to zcl_m99_pcc_ehiupdate.

data gt_proc_jobslist type zcl_m99_pcc_ehiupdate=>tty_proc_jobslist.
data gs_proc_jobslist type zcl_m99_pcc_ehiupdate=>ty_proc_jobslist.
data gt_emplist type zcl_m99_pcc_ehiupdate=>tty_emplist.
data gs_emplist type zcl_m99_pcc_ehiupdate=>ty_emplist.
data gv_msg type bapi_msg.
data: gt_pernr_so type /iwbep/t_cod_select_options.

* Used in HR BASIC DISPLAY LIST
types: begin of ty_header,
         header1(132),
         header2(132),
         header3(132),
         footnote1(132),
         footnote2(132),
         footnote3(132).
types: end of ty_header.
data: gs_jobs_header type ty_header.
data: gs_emps_header type ty_header.
types: begin of ty_buttons,
         text1(40),
         text2(40),
         text3(40),
         text4(40),
         text5(40),
         text6(40).
types: end of ty_buttons.
data: gs_jobs_buttons type ty_buttons.
data: gs_emps_buttons type ty_buttons.

data: gv_title      like sy-title,
      gv_jobs_title like gv_title,
      gv_emps_title like gv_title.

data: myreport          like sy-repid,
      datum(10),
      gv_list_level(2),
      gv_alv_coltab(10),
      gv_alv_marker(10),
      gv_no_alv_grid.

data: hr_ret_code like  sy-subrc.
*
data: gt_fieldnames type table of zhraustpy_pcc_fldnames.
data: gs_fieldnames type zhraustpy_pcc_fldnames.
data: gt_jobs_fields type table of zhraustpy_pcc_fldnames.
data: gt_emps_fields type table of zhraustpy_pcc_fldnames.

* Selection Screen
selection-screen begin of block blk01 with frame title text-001.
parameters: p_procid type pyc_d_pyp-id obligatory
                          matchcode object zhrpy_sh_pyp.
parameters: p_procnm type pyc_d_pypt-name modif id pnm.
parameters: p_pccval type boolean no-display.
select-options: s_emp_so for pernr-pernr no-display.
selection-screen end of block blk01.

at selection-screen output.
  perform change_screen_input.

at selection-screen on p_procnm.
  perform get_procid_name using p_procid changing p_procnm.
