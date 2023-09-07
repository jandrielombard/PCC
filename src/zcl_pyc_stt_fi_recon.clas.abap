class ZCL_PYC_STT_FI_RECON definition
  public
  inheriting from CL_PYC_STT_ASYNC_BATCH_BASE
  final
  create public .

public section.
  protected section.

    constants mc_sel_cls_fi_recon type pyd_par_type value 'ZCL_PYC_BPC_SEL_FI_RECON' ##NO_TEXT.
    constants gcv_par_paralle type pyd_par_type value 'PYP_INTERVAL' ##NO_TEXT.

    methods fp3_bpc_det_rt_get_list
        redefinition .
    methods get_param_filler_kinds
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_STT_FI_RECON IMPLEMENTATION.


  method FP3_BPC_DET_RT_GET_LIST.
* Assign Report, Selection and Log Classes
    data: ls_dto           type cl_pyc_rd_dto_bpc=>ty_s_rd.

    ls_dto-rpt_chain_id = '001'.
    ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
    ls_dto-rpt_type = 'PROGRAM'.
    ls_dto-var_type = 'VARIANT'.
    ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
    ls_dto-sel_rt = mc_sel_cls_fi_recon .
    ls_dto-log_rt = 'CL_PYC_BPC_LOG_BASE'.
    ls_dto-job_base_name = text-j01.
    append ls_dto to r_result.

  endmethod.


  method GET_PARAM_FILLER_KINDS.
* Param filler(PNP or Free)
    r_result = cl_pyc_param_filler_factory=>gc_kinds-pnp_free .
  endmethod.
ENDCLASS.
