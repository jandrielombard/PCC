*&---------------------------------------------------------------------*
*& Report           : ZHRAUREPY_PCC_EHIUPD00                           *
*& Tiltle           : PCC EHI Table Update for cancelled jobs Employees*
*& Transactio Code  : ZHRPY_M99_EHIUPD                                 *
*& Create Date      : 26 Apr 2021                                      *
*& Release          : ECC 6.0                                          *
*& Author           : Satya Aluru                                      *
*&                                                                     *
*&  This custom program has been created to add all                    *
*&  Cancelled Jobs Employees list to the Event handler table           *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*&---------------------------------------------------------------------*
*& DATE        User    Description                          TR Number  *
*&---------------------------------------------------------------------*
*&                                                                     *
*&---------------------------------------------------------------------*
report  zhraurepy_pcc_ehiupd00 line-size 1023 no standard page heading.

include zhraurepy_pcc_ehiupd_top.
include zhraurepy_pcc_ehiupd_frm.

************************************************************************
start-of-selection.
************************************************************************
* Set Parameters
  perform initialization.

* Read Jobs and Employees for the Selection
  create object go_ehiupdate
    exporting
      iv_proc_id  = gv_proc_id
      iv_pccval   = gv_pccval
      it_pernr_so = gt_pernr_so.

  call method go_ehiupdate->get_proc_jobslist
    importing
      et_proc_jobslist = gt_proc_jobslist
      et_emplist       = gt_emplist
      ev_msg           = gv_msg.

************************************************************************
end-of-selection.
************************************************************************
  if gv_pccval eq abap_true.
    perform display_batch_jobs_employees tables gt_emplist.
  else.
    if gt_proc_jobslist[] is initial.    "No jobs found
      if not gv_msg is initial.
        write: /1 gv_msg.
      else.
        write: /1 text-011.
      endif.
    else.
      perform display_batch_jobs_list tables gt_proc_jobslist.
    endif.
  endif.
************************************************************************
