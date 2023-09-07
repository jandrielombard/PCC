class ZCL_PYC_STT_NZ_EDS_GEN definition
  public
  inheriting from CL_PYC_STT_ASYNC_BATCH_BASE
  final
  create public .

public section.
protected section.

  constants MC_SELECTION_CLS_EDS type PYD_PAR_TYPE value 'ZCL_PYC_BPC_SELECTION_EDS' ##NO_TEXT.
  constants GCV_PAR_TEST_RUN type PYD_PAR_TYPE value 'Z99_TEST_RUN' ##NO_TEXT.

  methods FP3_BPC_DET_RT_GET_LIST
    redefinition .
  methods GET_PARAM_FILLER_KINDS
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_STT_NZ_EDS_GEN IMPLEMENTATION.


  method FP3_BPC_DET_RT_GET_LIST.
* Assign Report, Selection and Log Classes
    data: ls_dto      type cl_pyc_rd_dto_bpc=>ty_s_rd,
          lv_test_run type xfeld.

    lv_test_run = cl_pyd_fnd_aux=>get_resp_fixed_value(
                     iv_par_type = gcv_par_test_run
                     it_par      = io_res_context->mt_par ).


    ls_dto-rpt_chain_id = '001'.
    ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
    ls_dto-rpt_type = 'PROGRAM'.
    ls_dto-var_type = 'VARIANT'.
    ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
    ls_dto-sel_rt = me->mc_selection_cls_eds.
    ls_dto-log_rt = 'CL_PYC_BPC_LOG_BASE'.
    if lv_test_run eq abap_true.
      ls_dto-job_base_name = text-j01.
    else.
      ls_dto-job_base_name = text-j02.
    endif.
    append ls_dto to r_result.

  endmethod.


  method GET_PARAM_FILLER_KINDS.
* Set Result type
    r_result = cl_pyc_param_filler_factory=>gc_kinds-pnp_free .
  endmethod.
ENDCLASS.
