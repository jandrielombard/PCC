class ZUSECL_PYC_STT_CREATE_POST_RUN definition
  public
  inheriting from CL_PYC_STT_CREATE_POSTING_RUN
  final
  create public .

public section.
  protected section.

    constants gcv_par_paralle type pyd_par_type value 'PYP_INTERVAL' ##NO_TEXT.

    methods fp3_bpc_det_rt_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_PYC_STT_CREATE_POST_RUN IMPLEMENTATION.


  method FP3_BPC_DET_RT_GET_LIST.

    data ls_dto type cl_pyc_rd_dto_bpc=>ty_s_rd.
    data: lv_runid   type char32 .

*>>> Start of WOW Specific code
    data: ls_paralle_value type n length 5.
    ls_paralle_value = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_paralle
                          it_par      = io_res_context->mt_par ).
    if ls_paralle_value > 0 .
      "CREATE
      clear ls_dto.
      ls_dto-rpt_chain_id = '001'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_PARALLEL'.
      ls_dto-sel_rt = 'CL_PYC_BPC_SELECTION_PNPCE_REP'.
      ls_dto-rpt_type = 'PROGRAM'.
      ls_dto-var_type ='VARIANT'.
      ls_dto-job_base_name = text-s02.
      append ls_dto to r_result.

    else.
*<<< End of WOW Specific Code
      lv_runid = gen_pers_get(
           iv_gp_type     = cl_pyc_stt_create_posting_run=>gc_gen_process_key-run_id
           io_res_context = io_res_context
           ).

      if lv_runid is not initial .
        "DELETE FIRST
        clear ls_dto.
        ls_dto-rpt_chain_id = '001'.
        ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
        ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
        ls_dto-sel_rt = 'CL_PYC_BPC_SEL_POSTING_RUNID'.
        ls_dto-rpt_type = 'PROGRAM01'.
        ls_dto-var_type ='VARIANT01'.
        ls_dto-job_base_name = text-s03.
        append ls_dto to r_result.

        "CREATE
        clear ls_dto.
        ls_dto-rpt_chain_id = '002'.
        ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
        ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
        ls_dto-sel_rt = 'CL_PYC_BPC_SELECTION_PNPCE'.
        ls_dto-rpt_type = 'PROGRAM'.
        ls_dto-var_type ='VARIANT'.
        ls_dto-job_base_name = text-s02.
        append ls_dto to r_result.
      else.
        "CREATE
        clear ls_dto.
        ls_dto-rpt_chain_id = '001'.
        ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
        ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
        ls_dto-sel_rt = 'CL_PYC_BPC_SELECTION_PNPCE'.
        ls_dto-rpt_type = 'PROGRAM'.
        ls_dto-var_type ='VARIANT'.
        ls_dto-job_base_name = text-s02.
        append ls_dto to r_result.
      endif.
*>>> Start of WOW Specific code
    endif.
*<<< End of WOW Specific Code
  endmethod.
ENDCLASS.
