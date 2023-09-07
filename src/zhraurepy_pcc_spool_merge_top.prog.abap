*&---------------------------------------------------------------------*
*&  Include ZHRAUREPY_PCC_SPOOL_MERGE_TOP
*&---------------------------------------------------------------------*
tables: tsp01, tbtc_spoolid, tbtco, zhr_rpcs0000_a.

types: begin of ty_spool,
         spoolid   type tbtc_spoolid-spoolid,
         jobname   type tbtc_spoolid-jobname,
         jobcount  type tbtc_spoolid-jobcount,
         rqident   type tsp01-rqident,
         rq1name   type tsp01-rq1name,
         rq2name   type tsp01-rq2name,
         rqtitle   type tsp01-rqtitle,
         rqapprule type tsp01-rqapprule,
       end of ty_spool.

types: begin of ty_title,
         rqtitle      type zhr_rpcs0000_a-rqtitle,
         job_variance type zhr_rpcs0000_a-job_variance,
         spool_format type zhr_rpcs0000_a-spool_format,
       end of ty_title.

types: begin of ty_tbtco,
         jobname  type btcjob,
         jobcount type btcjobcnt,
         status   type btcstatus,
       end of ty_tbtco.

data: g_completed  type abap_bool,
      g_not_locked type abap_bool,
      gt_tbtco     type standard table of ty_tbtco,
      gs_tbtco     type ty_tbtco,
      gt_spool_job type standard table of ty_spool,
      gt_spool     type standard table of ty_spool,
      gt_titles    type standard table of ty_title,
      gt_report    type list_string_table,
      g_jobnam     type tbtc_spoolid-jobname.
constants: gc_orig_spool_merg_prog type zhr_mer_prg
                           value 'ZHR_SPOOL_MERGE'.

types: begin of ty_spool_objects,
         spoolid    type btclistid,
         listobject type table_abaplist.
types: end of ty_spool_objects.
data: gt_spool_objects type table of ty_spool_objects,
      gs_spool_objects type ty_spool_objects.

selection-screen begin of block a1 with frame title text-006.
parameters: p_call type sy-repid no-display,
            p_secs type zhr_mer_var default '600' no-display.
select-options: s_jobnam for tbtc_spoolid-jobname no intervals,
                s_jobcnt for tbtc_spoolid-jobcount no intervals.
select-options: s_title  for tsp01-rqtitle no intervals.
parameters p_wfmt type boolean as checkbox.
parameters p_rep type boolean no-display.
parameters p_last type boolean no-display.
parameters p_pages type numc2 no-display.

selection-screen end of block a1.
