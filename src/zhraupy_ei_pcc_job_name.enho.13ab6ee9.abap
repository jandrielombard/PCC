"Name: \TY:CL_PYC_BPC_REPORT_SIMPLE\ME:GET_JOB_NAME\SE:END\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_JOB_NAME.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |18-10-2021 |1130848|change background job name for    |PTX-3763      |CFAK900994        *
*                         Process Step Jobs.                                                  *
*---------------------------------------------------------------------------------------------*
* Add Process template name for all Process Step Job Names
 constants: lc_parallel_id_separator type c value '/',
            lc_numbers(10)           type c value '0123456789'.

 data: lv_result_temp like rv_result,
       lv_max_length  type i.
 data: lv_process_name  type pyc_d_pypt-name.

 if io_res_context is bound.
   read table io_res_context->mt_par into data(ls_par)
     with key par_type = cl_pyc_dt_proc_processes=>mc_pyp_proc.
   if sy-subrc = 0.
     select single name into @lv_process_name
       from pyc_d_pypt  where id = @ls_par-low.
   endif.
 endif.
*  In case of standard process step prefix the name with Process template name
 lv_result_temp = |{ lv_process_name(4) }{ lc_parallel_id_separator }| &&
                  |{ rv_result }|.
*  Adjust the name to max length
 describe field rv_result length lv_max_length in character mode.
 if strlen( lv_result_temp ) > lv_max_length.
   rv_result = lv_result_temp(lv_max_length).
 else.
   rv_result = lv_result_temp.
 endif.
ENDENHANCEMENT.
