*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_FIRECON_FILEDL01
*&---------------------------------------------------------------------*
tables: t549t.
constants: gc_logical_path type filepath-pathintern value 'HR_NZ_EMS_FILENAME',
           gc_eds_text     type string value 'EDexception.CSV',
           gc_eis_text     type string value 'EIexception.CSV'.

data: gv_asdir_name     type  epsf-epsdirnam,
      gv_file_mask      type  epsf-epsfilnam value space,
      gv_okfile_counter type  epsf-epsfilsiz,
      gv_error_counter  type  epsf-epsfilsiz,
      gt_dir_list       type  zhrau_tt_pcc_filelist,
      gv_file_text      type string.

data: gt_t549t type table of t549t.

ranges: r_file_mask for admi_files-filename.
data: g_sysid type syst-sysid,
      g_mandt type mandt.
ranges: r_sysid for syst-sysid.

* types for error
types : begin of ty_error,
          icon(4),                       "icon
          pernr        type pernr_d,     "emp no
          ee_name(82),                   "emp name
          message(150),                  "error message
        end of ty_error.
data: gt_error type table of ty_error,      "IT to display error log
      gs_error type ty_error.               "work ared to display error log

* Exception file download to application server
types: begin of ty_efile,
         record(500) type c,
       end of ty_efile.
data: gt_efile type table of ty_efile,      "IT to hold efile
      gs_efile type ty_efile.               "work area to hold efile

types: begin of ty_payday_file,
         filename type string,
         abkrs    type abkrs,
         pabrp    type pabrp,
         pabrj    type pabrj,
         bondt    type bondt,
         msg      type bapi_msg.
types:   end of ty_payday_file.
data: gt_payday_files type table of ty_payday_file .
data: gs_payday_file type ty_payday_file.
data: go_cx_paydayfl type ref to cx_root,
      gv_msg         type string.

* Used in HR BASIC DISPLAY LIST
constants: gc_files    type sy-ucomm value 'DISPLAYFILES'.
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
parameters: p_abkrs like t569v-abkrs obligatory.
selection-screen end of block frm1.

selection-screen begin of block frm2 with frame title text-f02.
selection-screen begin of line.
parameters: noc type h99cwtr-nooc default 'X' radiobutton group oc
        modif id pe2.
selection-screen comment 4(31) text-s12 for field noc modif id pe2.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-s14 for field p_pabrp
modif id pa1.
selection-screen position pos_low.
parameters:
  p_pabrp like t549q-pabrp modif id pe1, "calculation period
  p_pabrj like t549q-pabrj modif id pe1.
selection-screen end of line.

selection-screen begin of line.
parameters: yoc type h99cwtr-yesoc radiobutton group oc modif id pe2.
selection-screen comment 4(31) text-s13 for field yoc modif id pe2.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-s15 for field p_ocrsn.
parameters: p_ocrsn   like pc261-ocrsn modif id spe.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-s11 modif id pe2
    for field p_payty.
parameters: p_payty like pc261-payty modif id pe2 value check,
            p_payid like pc261-payid modif id pe2,
            p_bondt like pc261-bondt modif id pe2.
selection-screen end of line.
selection-screen end of block frm2.

selection-screen begin of block frm3 with frame title text-f03.
parameters: r_eds type xfeld default 'X' radiobutton group prep.
parameters: r_eis type xfeld radiobutton group prep.
selection-screen end of block frm3.
