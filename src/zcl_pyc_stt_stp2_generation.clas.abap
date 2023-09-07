class ZCL_PYC_STT_STP2_GENERATION definition
  public
  inheriting from CL_PYC_STT_ASYNC_BATCH_BASE
  final
  create public .

public section.
protected section.

  constants MC_SELECTION_CLS_STP2 type PYD_PAR_TYPE value 'ZCL_PYC_BPC_SELECTION_STP2' ##NO_TEXT.
  constants MC_PARA_SELECTION_CLS_STP2 type PYD_PAR_TYPE value 'ZCL_PYC_BPC_PARA_SELECTON_STP2' ##NO_TEXT.
  constants GCV_PAR_PARALLE type PYD_PAR_TYPE value 'PYP_INTERVAL' ##NO_TEXT.
  constants GCV_PAR_TEST_RUN type PYD_PAR_TYPE value 'Z99_TEST_RUN' ##NO_TEXT.

  methods FP3_BPC_DET_RT_GET_LIST
    redefinition .
  methods GET_PARAM_FILLER_KINDS
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_STT_STP2_GENERATION IMPLEMENTATION.


  METHOD fp3_bpc_det_rt_get_list.
*----------------------------------------------------------------------*
*Date      |User ID |Description              |Chg. Ref.|WkBh Req      *
*----------------------------------------------------------------------*
*01/03/2023|1106254 |Initial development      |STPS-187 |CFAK902866
*                    Copy of STT STP class               Revtrac - 1220
*                    updated with STP2 selectn
*                    with Test run update DB
*----------------------------------------------------------------------*
* Assign Report, Selection and Log Classes
    DATA: ls_dto           TYPE cl_pyc_rd_dto_bpc=>ty_s_rd,
          lv_test_run      TYPE xfeld,
          lv_paralle_value TYPE n LENGTH 5.

    CONSTANTS: lc_001                        TYPE char3 VALUE '001',
               lc_program                    TYPE char7 VALUE 'PROGRAM',
               lc_cl_pyc_bpc_log_base        TYPE char19 VALUE 'CL_PYC_BPC_LOG_BASE',
               lc_cl_pyc_bpc_report_simple   TYPE char24 VALUE 'CL_PYC_BPC_REPORT_SIMPLE',
               lc_cl_pyc_bpc_report_parallel TYPE char26 VALUE 'CL_PYC_BPC_REPORT_PARALLEL'.

    lv_test_run = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_test_run
                          it_par      = io_res_context->mt_par ).

    lv_paralle_value = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_paralle
                          it_par      = io_res_context->mt_par ).

    IF lv_paralle_value > 0 .
      ls_dto-rpt_chain_id = lc_001.
      ls_dto-rpt_cat      = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_type     = lc_program.
      ls_dto-rpt_rt       = lc_cl_pyc_bpc_report_parallel.
      ls_dto-sel_rt       = me->mc_para_selection_cls_stp2.
      ls_dto-log_rt       = lc_cl_pyc_bpc_log_base.

      IF lv_test_run EQ abap_true.
        ls_dto-job_base_name = text-j01.
      ELSE.
        ls_dto-job_base_name = text-j02.
      ENDIF.
      APPEND ls_dto TO r_result.
    ELSE.
      ls_dto-rpt_chain_id = lc_001.
      ls_dto-rpt_cat      = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_type     = lc_program.
      ls_dto-rpt_rt       = lc_cl_pyc_bpc_report_simple.
      ls_dto-sel_rt       = me->mc_selection_cls_stp2.
      ls_dto-log_rt       = lc_cl_pyc_bpc_log_base.

      IF lv_test_run EQ abap_true.
        ls_dto-job_base_name = text-j01.
      ELSE.
        ls_dto-job_base_name = text-j02.
      ENDIF.
      APPEND ls_dto TO r_result.
    ENDIF.

  ENDMETHOD.


  method GET_PARAM_FILLER_KINDS.
* Set Result type
    r_result = cl_pyc_param_filler_factory=>gc_kinds-pnp_free .
  endmethod.
ENDCLASS.
