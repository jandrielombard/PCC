class ZCL_PYC_STT_WAGE_COSTING_REP definition
  public
  inheriting from CL_PYC_STT_ASYNC_BATCH_BASE
  final
  create public .

public section.
protected section.

  constants MC_SELECTION_CLS type PYD_PAR_TYPE value 'ZCL_PYC_BPC_SELECTION_WCR' ##NO_TEXT.

  methods FP3_BPC_DET_RT_GET_LIST
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_PYC_STT_WAGE_COSTING_REP IMPLEMENTATION.


  method FP3_BPC_DET_RT_GET_LIST.
* Assign Report, Selection and Log Classes
    " one report via one job
    DATA ls_dto TYPE cl_pyc_rd_dto_bpc=>ty_s_rd.
    ls_dto-rpt_chain_id = '001'.
    ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
    ls_dto-rpt_type = 'PROGRAM'.
    ls_dto-var_type ='VARIANT'.
    ls_dto-rpt_rt = cl_pyc_bpc_report_simple=>gc_clsnm.
    ls_dto-sel_rt = me->mc_selection_cls.
    ls_dto-job_base_name = text-j01.
    APPEND ls_dto TO r_result.
  endmethod.
ENDCLASS.
