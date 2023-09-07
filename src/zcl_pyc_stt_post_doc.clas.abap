class ZCL_PYC_STT_POST_DOC definition
  public
  inheriting from CL_PYC_STT_POST_DOC
  final
  create public .

public section.
protected section.

  methods FP3_BPC_DET_RT_GET_LIST
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_PYC_STT_POST_DOC IMPLEMENTATION.


  method fp3_bpc_det_rt_get_list.

    data: ls_dto type cl_pyc_rd_dto_bpc=>ty_s_rd.

    ls_dto-rpt_chain_id = '001'.
    ls_dto-rpt_cat  = if_pyc_bpc_report=>category-pm_support .
    ls_dto-rpt_rt   = 'CL_PYC_BPC_REPORT_SIMPLE'.
    ls_dto-sel_rt   = 'ZCL_PYC_BPC_SEL_POSTING_RUNID'.
    ls_dto-rpt_type = 'PROGRAM'.
    ls_dto-var_type = 'VARIANT'.
    ls_dto-job_base_name = text-011.
    append ls_dto to r_result.

  endmethod.
ENDCLASS.
