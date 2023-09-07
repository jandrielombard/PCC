class zcl_pyc_stt_stp_generation definition
  public
  inheriting from cl_pyc_stt_async_batch_base
  final
  create public .

  public section.
  protected section.

    constants mc_selection_cls_stp type pyd_par_type value 'ZCL_PYC_BPC_SELECTION_STP' ##NO_TEXT.
    constants mc_para_selection_cls_stp type pyd_par_type value 'ZCL_PYC_BPC_PARA_SELECTION_STP' ##NO_TEXT.
    constants gcv_par_paralle type pyd_par_type value 'PYP_INTERVAL' ##NO_TEXT.
    constants: gcv_par_test_run type pyd_par_type value 'Z99_TEST_RUN'.

    methods fp3_bpc_det_rt_get_list
        redefinition .
    methods get_param_filler_kinds
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_STT_STP_GENERATION IMPLEMENTATION.


  method fp3_bpc_det_rt_get_list.
* Assign Report, Selection and Log Classes
    data: ls_dto           type cl_pyc_rd_dto_bpc=>ty_s_rd,
          lv_test_run      type xfeld,
          lv_paralle_value type n length 5.

    lv_test_run = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_test_run
                          it_par      = io_res_context->mt_par ).

    lv_paralle_value = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_paralle
                          it_par      = io_res_context->mt_par ).
    if lv_paralle_value > 0 .
      ls_dto-rpt_chain_id = '001'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_type = 'PROGRAM'.
*      ls_dto-var_type = 'VARIANT'.
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_PARALLEL'.
      ls_dto-sel_rt = me->mc_para_selection_cls_stp.
      ls_dto-log_rt = 'CL_PYC_BPC_LOG_BASE'.
      if lv_test_run eq abap_true.
        ls_dto-job_base_name = text-j01.
      else.
        ls_dto-job_base_name = text-j02.
      endif.
      append ls_dto to r_result.
    else.
      ls_dto-rpt_chain_id = '001'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_type = 'PROGRAM'.
*      ls_dto-var_type = 'VARIANT'.
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
      ls_dto-sel_rt = me->mc_selection_cls_stp.
      ls_dto-log_rt = 'CL_PYC_BPC_LOG_BASE'.
      if lv_test_run eq abap_true.
        ls_dto-job_base_name = text-j01.
      else.
        ls_dto-job_base_name = text-j02.
      endif.
      append ls_dto to r_result.
    endif.

  endmethod.


  method get_param_filler_kinds.
* Set Result type
    r_result = cl_pyc_param_filler_factory=>gc_kinds-pnp_free .
  endmethod.
ENDCLASS.
