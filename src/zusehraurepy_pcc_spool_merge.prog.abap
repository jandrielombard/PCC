*&---------------------------------------------------------------------*
*& Report           : ZUSEHRAUREPY_PCC_SPOOL_MERGE                     *
*& Tiltle           : Payroll Control Center jobs Spool Merge          *
*&                                                                     *
*&  This custom program has been created to merge output of Parallel   *
*&  Processing PCC Step into single job into single spool output.      *
*&  When the job has multiple spool output program matache the output  *
*&  using spool report title and produces separate consolidated report *
*&  by report title                                                    *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*&---------------------------------------------------------------------*
*& DATE        User    Description                          TR Number  *
*&---------------------------------------------------------------------*
*&                                                                     *
*&---------------------------------------------------------------------*
report  zhraurepy_pcc_spool_merge
   line-size 1023 no standard page heading.

INCLUDE ZUSEHREPY_PCC_SPOOL_MERGE_TOP.
*include zhraurepy_pcc_spool_merge_top.
INCLUDE ZUSEHREPY_PCC_SPOOL_MERGE_FRM.
*include zhraurepy_pcc_spool_merge_frm.

************************************************************************
start-of-selection.
************************************************************************
  perform initilation.

  g_completed  = abap_true.
  g_not_locked = abap_true.
  g_not_locked = abap_false.
*-------------------------------------------------------
* continue only if all job of the job set have completed
* and this merge prorgam is not already running
*-------------------------------------------------------
  perform read_jobs tables gt_tbtco
                  changing g_completed g_jobnam.

  check g_completed = abap_true.
  check gt_tbtco[] is not initial.  "jobs found

* Read spool id created by the job name/s
  select spoolid  jobname  jobcount
    from  tbtc_spoolid into table gt_spool_job
    for all entries in gt_tbtco
   where jobname   = gt_tbtco-jobname
     and jobcount  = gt_tbtco-jobcount.

* Format spool number
  loop at gt_spool_job assigning field-symbol(<fs_spool>).
    <fs_spool>-rqident = <fs_spool>-spoolid.
  endloop.

* read spool titles
  if not gt_spool_job is initial.
    select rqident, rq1name, rq2name, rqtitle, rqapprule
      from tsp01 for all entries in @gt_spool_job
          where rqident = @gt_spool_job-rqident
            and rqclient = @sy-mandt
            and rqtitle in @s_title
    into corresponding fields of table @gt_spool.
  endif.

************************************************************************
end-of-selection.
************************************************************************
  if gt_tbtco[] is initial.
    message i016(zhrau_rpt).      "No jobs/spool reports selected to merge'.
  else.
    if  g_not_locked = abap_true.
      message i013(zhrau_rpt).    "Not processed as jobs active'.
    else.
      if gt_spool[] is initial.
        message i016(zhrau_rpt) . "No jobs/spool reports selected to merge'.
      else.
        if p_wfmt eq abap_false.
          perform write_reports.
        else.
          perform write_reports_with_formatting.
        endif.
        message i014(zhrau_rpt) .  "Merge reports complete'.
      endif.
    endif.
  endif.
************************************************************************
