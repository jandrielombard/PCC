class ZCL_PYC_STT_TFN_CREATE definition
  public
  inheriting from CL_PYC_STT_ASYNC_BATCH_BASE
  final
  create public .

public section.
protected section.

  constants MC_SELECTION_CLS_STP type PYD_PAR_TYPE value 'ZCL_PYC_BPC_SELECTION_STP' ##NO_TEXT.
  constants MC_PARA_SELECTION_CLS_STP type PYD_PAR_TYPE value 'ZCL_PYC_BPC_PARA_SELECTION_STP' ##NO_TEXT.
  constants GCV_PAR_PARALLE type PYD_PAR_TYPE value 'PYP_INTERVAL' ##NO_TEXT.

  methods FP3_BPC_DET_RT_GET_LIST
    redefinition .
  methods GET_PARAM_FILLER_KINDS
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_STT_TFN_CREATE IMPLEMENTATION.


  method FP3_BPC_DET_RT_GET_LIST.
* Assign Report, Selection and Log Classes
    " one report via one job
*    DATA ls_dto TYPE cl_pyc_rd_dto_bpc=>ty_s_rd.
*    ls_dto-rpt_chain_id = '001'.
*    ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
*    ls_dto-rpt_type = 'PROGRAM'.
*    ls_dto-var_type ='VARIANT'.
*    ls_dto-rpt_rt = cl_pyc_bpc_report_simple=>gc_clsnm.
*    ls_dto-sel_rt = me->mc_selection_cls.
*    APPEND ls_dto TO r_result.

    data: ls_dto           type cl_pyc_rd_dto_bpc=>ty_s_rd,
          ls_paralle_value type n length 5.

    ls_paralle_value = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_paralle
                          it_par      = io_res_context->mt_par ).
    if ls_paralle_value > 0 .
      ls_dto-rpt_chain_id = '001'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_type = 'PROGRAM'.
      ls_dto-var_type = 'VARIANT'.
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_PARALLEL'.
      ls_dto-sel_rt = cl_pyc_bpc_selection_pnp_pernr=>gc_sub_clsnm.
      ls_dto-log_rt = 'CL_PYC_BPC_LOG_BASE'.
      ls_dto-job_base_name = text-j01.
      append ls_dto to r_result.
    else.
      ls_dto-rpt_chain_id = '001'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_type = 'PROGRAM'.
      ls_dto-var_type = 'VARIANT'.
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
      ls_dto-sel_rt = cl_pyc_bpc_selection_pnp=>gc_clsnm .
      ls_dto-log_rt = 'CL_PYC_BPC_LOG_BASE'.
      ls_dto-job_base_name = text-j01.
      append ls_dto to r_result.
    endif.

  endmethod.


  method get_param_filler_kinds.
* Param filler(PNP or Free)
    r_result = cl_pyc_param_filler_factory=>gc_kinds-pnp_free .
  endmethod.
ENDCLASS.
