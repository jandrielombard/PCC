*&---------------------------------------------------------------------*
*& Report           : ZUSEREPY_PCC_MDLOG_EHIUPD00                      *
*& Tiltle           : PCC EHI Update using Master Data Log (PCL4)      *
*& Transactio Code  : ZUSE_PCC_MDDLOGEHI                               *
*&                                                                     *
*&  This custom program has been created to read master data log for   *
*&  the selected period and add the changed employees to event handler *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*&---------------------------------------------------------------------*
*& DATE        User    Description                          TR Number  *
*&---------------------------------------------------------------------*
*&                                                                     *
*&---------------------------------------------------------------------*
report zhraurepy_pcc_mdlog_ehiupd00
       line-size 1023 no standard page heading.

include lbtchdef.
INCLUDE ZUSEREPY_PCC_MDLOG_EHIUPD_TOP.
*include zhraurepy_pcc_mdlog_ehiupd_top.
INCLUDE ZUSEREPY_PCC_MDLOG_EHIUPD_FRM.
*include zhraurepy_pcc_mdlog_ehiupd_frm.

initialization.
* Initialize Exclude User names and Infotypes
* Populate Exclude User Names and infty from TVARVC
  call method zusecl_m99_pcc_mdlog_ehiupdate=>get_ehiupd_exc_uname_infty
    importing
      et_exuname = s_usrnm[]
      et_exinfty = s_infty[].

************************************************************************
start-of-selection.
************************************************************************
* Create Object and Read Master data changes for the Selection
  create object go_mdlog_ehiupdate
    exporting
      iv_pypi_id = p_pypiid
      iv_begda   = p_begda
      iv_endda   = p_endda
      iv_begtz   = p_begtz
      iv_endtz   = p_endtz
      iv_aest    = p_aest
      it_exuname = s_usrnm[]
      it_exinfty = s_infty[].

* Read Master data changes for the Selection
  call method go_mdlog_ehiupdate->read_masterdata_changes.

************************************************************************
end-of-selection.
************************************************************************
* Update Event Handler Table
  call method go_mdlog_ehiupdate->add_employees_to_ehitable
    exporting
      iv_test     = p_test
    importing
      et_empslist = gt_empslist.

  if gt_empslist[] is initial.      "Changes found
    write: /1 text-011.
  else.
    perform display_employees_list tables gt_empslist.
  endif.
************************************************************************
