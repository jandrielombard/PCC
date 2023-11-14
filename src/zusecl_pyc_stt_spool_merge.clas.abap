class ZUSECL_PYC_STT_SPOOL_MERGE definition
  public
  inheriting from CL_PYC_STT_ASYNC_BATCH_BASE
  final
  create public .

public section.
protected section.

  constants MC_SELECTION_CLS_SPOOLMERGE type PYD_PAR_TYPE value 'ZCL_PYC_BPC_SELECTION_SM' ##NO_TEXT.

  methods FP3_BPC_DET_RT_GET_LIST
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_PYC_STT_SPOOL_MERGE IMPLEMENTATION.


  method FP3_BPC_DET_RT_GET_LIST.
* Assign Report, Selection and Log Classes
    " one report via one job
    data ls_dto type cl_pyc_rd_dto_bpc=>ty_s_rd.
    ls_dto-rpt_chain_id = '001'.
    ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
    ls_dto-rpt_type = 'PROGRAM'.
    ls_dto-var_type ='VARIANT'.
    ls_dto-rpt_rt = cl_pyc_bpc_report_simple=>gc_clsnm.
*    ls_dto-sel_rt = me->mc_selection_cls.
    ls_dto-sel_rt = me->mc_selection_cls_spoolmerge.
    ls_dto-job_base_name = text-j01.
    append ls_dto to r_result.

  endmethod.
ENDCLASS.
