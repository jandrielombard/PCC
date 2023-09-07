"Name: \TY:CL_PYC_BPC_REPORT_SIMPLE\ME:GET_JOB_NAME\SE:BEGIN\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_JOB_NAME.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |01-12-2021 |1130848|change background job name using  |PTX-3763      |CFAK9011268       *
*                         mapping table.                                                          *
*---------------------------------------------------------------------------------------------*
* Job Name Mapping
  data: lv_rpt_name type syrepid,
        lv_job_name type pyc_bpc_job_base_name.
* Change Base Job Name using mapping table
  lv_rpt_name = get_report_name( io_res_context  = io_res_context
                                 iv_report_index = iv_report_index
                                ) .
* Read Job Name Mapping
  if not ( lv_rpt_name is initial and mv_job_base_name is initial ).
    select single job_name into lv_job_name from zhrpy_pcc_jobnm
      where rpt_name = lv_rpt_name and job_base_name = mv_job_base_name.
      if sy-subrc eq 0.
        if not lv_job_name is INITIAL.
          move lv_job_name to mv_job_base_name.
        endif.
      endif.
  endif.
ENDENHANCEMENT.
