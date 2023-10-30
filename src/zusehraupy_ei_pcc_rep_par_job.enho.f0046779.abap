"Name: \TY:CL_PYC_BPC_REPORT_PARALLEL\ME:GET_JOB_NAME\SE:BEGIN\EI
ENHANCEMENT 0 ZUSEHRAUPY_EI_PCC_REP_PAR_JOB.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *                                                       *
*---------------------------------------------------------------------------------------------*
  CONSTANTS: lc_parallel_id_separator TYPE c VALUE '/',
             lc_numbers(10) type c value '0123456789'.

  DATA: lr_job_name            type ZBTCJOB_RANGE_TABLE,
        lv_id                  TYPE string,
        lv_result_temp         LIKE rv_result,
        lv_max_length          TYPE i.

* Job Name Mapping
  data: lv_rpt_name type syrepid,
        lv_job_name type pyc_bpc_job_base_name.
* Change Base Job Name using mapping table
  lv_rpt_name = get_report_name( io_res_context  = io_res_context
                                 iv_report_index = iv_report_index
                                ) .
* Read Job Name Mapping
  if not ( lv_rpt_name is initial and mv_job_base_name is initial ).
    select single job_name into lv_job_name from zuse_pcc_jobnm
      where rpt_name = lv_rpt_name and job_base_name = mv_job_base_name.
      if sy-subrc eq 0.
        if not lv_job_name is INITIAL.
          move lv_job_name to mv_job_base_name.
        endif.
      endif.
  endif.

  lr_job_name = get_job_names_to_change( ).
  IF mv_job_base_name in lr_job_name.
    lv_id = get_rule_summary(
              iv_row_id = iv_row_id
              io_res_context = io_res_context
              iv_job_base_name = mv_job_base_name ).
    IF lv_id IS NOT INITIAL.
      lv_result_temp = |{ mv_process_name(4) }{ lc_parallel_id_separator }| &&
        |{ iv_row_id+2(3) }{ lc_parallel_id_separator }| &&
        |{ lv_id }|.
      DESCRIBE FIELD rv_result LENGTH lv_max_length IN CHARACTER MODE.
      IF strlen( lv_result_temp ) > lv_max_length.
        rv_result = lv_result_temp(lv_max_length).
      ELSE.
        rv_result = lv_result_temp.
      ENDIF.
      EXIT.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
