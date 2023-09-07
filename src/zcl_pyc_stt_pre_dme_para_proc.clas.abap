class ZCL_PYC_STT_PRE_DME_PARA_PROC definition
  public
  inheriting from CL_PYC_STT_PRE_DME_PROC
  final
  create public .

public section.
protected section.

  constants GCV_PAR_PARALLE type PYD_PAR_TYPE value 'PYP_INTERVAL' ##NO_TEXT.

  methods FP3_BPC_DET_RT_GET_LIST
    redefinition .
  methods VETO_CHECK
    redefinition .
private section.

  methods CONCAT_RUN_STAMP
    importing
      !IV_LAUFD type LAUFD
      !IV_LAUFI type LAUFI
    returning
      value(RV_ROID) type PYD_ROID .
  methods PARSE_RUN_STAMP
    importing
      !IV_ROID type PYD_ROID
    exporting
      !RV_LAUFD type LAUFD
      !RV_LAUFI type LAUFI .
ENDCLASS.



CLASS ZCL_PYC_STT_PRE_DME_PARA_PROC IMPLEMENTATION.


  method CONCAT_RUN_STAMP.
    CONCATENATE iv_laufd iv_laufi INTO rv_roid SEPARATED BY '/'.
  endmethod.


  method FP3_BPC_DET_RT_GET_LIST.
* WOW Specific Code to Implement Parallel processing for Pre DME

* This process step also excludes deletion step, Hence it's not suitable for
* pre DME execution using Set flag for transfers option.
    data ls_dto type cl_pyc_rd_dto_bpc=>ty_s_rd.
    data: ls_paralle_value type n length 5.

    ls_paralle_value = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_paralle
                          it_par      = io_res_context->mt_par ).

    if ls_paralle_value > 0 .
      "create - Parallel Processing
      clear ls_dto.
      ls_dto-rpt_chain_id = '001'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_PARALLEL'.
      ls_dto-sel_rt = 'CL_PYC_BPC_SELECTION_PNP_PERNR'.
      ls_dto-rpt_type = 'PROGRAM'.
      ls_dto-var_type ='VARIANT'.
      ls_dto-job_base_name = text-j02.
      append ls_dto to r_result.
    else.
      "create
      clear ls_dto.
      ls_dto-rpt_chain_id = '000'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
      ls_dto-sel_rt = 'CL_PYC_BPC_SELECTION_PNP'.
      ls_dto-rpt_type = 'PROGRAM'.
      ls_dto-var_type ='VARIANT'.
      ls_dto-job_base_name = text-j02.
      append ls_dto to r_result.

    endif.

  endmethod.


  method PARSE_RUN_STAMP.
    split iv_roid at '/' into rv_laufd rv_laufi.
  endmethod.


  method VETO_CHECK.

* Trigger Employee numbers population for initial run
    rv_veto_raised = abap_false.
    check iv_rd_type = cl_pyc_pt_sts_err=>gcs_operation-addl_act.
    rv_veto_raised = abap_true.

    data: lt_par_type_so   type /iwbep/t_cod_select_options.
    lt_par_type_so = cl_pyd_fnd_aux=>set_so_fixed_value( cl_pyd_par_type_pernr=>gc_par_type ).
    if dev_sel_obj_get_list(
      io_res_context = io_res_context
      it_par_type_so = lt_par_type_so ) is not initial.
      rv_veto_raised = abap_false.   "deviating selection for PERNR exists -> additional activity active
    else.
      rv_veto_raised = abap_true.    "no deviating selection for PERNR -> additional activity not active
    endif.

  endmethod.
ENDCLASS.
