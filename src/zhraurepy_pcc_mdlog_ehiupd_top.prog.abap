*&---------------------------------------------------------------------*
*&  Include ZHRAUREPY_PCC_SPOOL_MERGE_TOP
*&---------------------------------------------------------------------*
tables: pcl4, t582a, tvarvc.

constants: gc_emps          type sy-ucomm value 'EMPS',
           gc_ehiupd        type sy-ucomm value 'EHIUPD'.

data: t_color type slis_t_specialcol_alv with header line.
data: ucom    like sy-ucomm value 'EMPS'.
field-symbols: <datatab> type table.

data: go_mdlog_ehiupdate type ref to zcl_m99_pcc_mdlog_ehiupdate.
* Employee Master data Changes
data: gt_empslist type zcl_m99_pcc_mdlog_ehiupdate=>tty_empslist,
      gs_empslist type zcl_m99_pcc_mdlog_ehiupdate=>ty_empslist.
* Employee Payroll Area
data: gt_it0001 type zcl_m99_pcc_mdlog_ehiupdate=>tty_it0001,
      gs_it0001 type zcl_m99_pcc_mdlog_ehiupdate=>ty_it0001.

* Variable Table Entries for User Name and Infotypes
data: gt_exuname type tab_range_uname,
      gt_exinfty type infty_range_tab.

* Used in HR BASIC DISPLAY LIST
types: begin of ty_header,
         header1(132),
         header2(132),
         header3(132),
         footnote1(132),
         footnote2(132),
         footnote3(132).
types: end of ty_header.
data: gs_emps_header type ty_header.
types: begin of ty_buttons,
         text1(40),
         text2(40),
         text3(40),
         text4(40),
         text5(40),
         text6(40).
types: end of ty_buttons.
data: gs_emps_buttons type ty_buttons.

data: gv_title      like sy-title,
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
data: gt_emps_fields type table of zhraustpy_pcc_fldnames.

* Selection Screen
selection-screen begin of block a1 with frame title text-001.
parameters: p_pypiid type pyc_d_pypi-pypi_id obligatory.
selection-screen skip 1.
selection-screen begin of block a12 with frame title text-012.
selection-screen begin of line.
selection-screen comment 1(31) text-s01 for field p_begda .
parameters: p_begda type sy-datum default sy-datum obligatory.
selection-screen comment 52(5) text-s02 for field p_endda.
parameters: p_begtz type sy-uzeit default sy-uzeit obligatory.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(31) text-s03 for field p_begda .
parameters: p_endda type sy-datum default sy-datum obligatory.
selection-screen comment 52(5) text-s04 for field p_endda.
parameters: p_endtz type sy-uzeit default sy-uzeit obligatory.
selection-screen end of line.
selection-screen skip 1.
parameters: p_aest type xfeld as checkbox default ' '.
selection-screen end of block a12.
selection-screen skip 1.
select-options: s_usrnm for pcl4-uname no intervals.
select-options: s_infty for t582a-infty no intervals.
selection-screen skip 1.
selection-screen begin of block a2 with frame title text-013.
parameters: p_test type xfeld default 'X'.
selection-screen end of block a2.
selection-screen end of block a1.
