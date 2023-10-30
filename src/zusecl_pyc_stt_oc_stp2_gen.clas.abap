class ZUSECL_PYC_STT_OC_STP2_GEN definition
  public
  inheriting from CL_PYC_STT_OC_ASYNC_BASE
  create public

  global friends CL_PYD_RDT_AUX .

public section.

  types:
    begin of job_s_key,
        job_name  type btcjob,
        job_count type btcjobcnt,
      end of job_s_key .
  types:
    job_t_key type standard table of job_s_key .

  constants GC_SUB_CLSNM type CLASSNAME value 'ZCL_PYC_BPC_SELECTION_OC_STP2' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IS_IMPORT type TY_S_CONSTR
    raising
      CX_PYD_FND .
  protected section.

    methods fp3_bpc_det_rt_get_list
        redefinition .
    methods veto_check
        redefinition .
PRIVATE SECTION.

*  constants:
*    begin of gcs_run_payroll_otr_alias,
*        main_act type sotr_alias value 'PAOC_PAY_PYD_PCC_CONT/PYC_STT_RUN_PY_MAIN_ACT',
*      end of gcs_run_payroll_otr_alias .

  CONSTANTS gcv_par_paralle TYPE pyd_par_type VALUE 'PYP_INTERVAL' ##NO_TEXT.
  CONSTANTS gcv_par_test_run TYPE pyd_par_type VALUE 'Z99_TEST_RUN' ##NO_TEXT.
ENDCLASS.



CLASS ZUSECL_PYC_STT_OC_STP2_GEN IMPLEMENTATION.


  method CONSTRUCTOR.

    data:
      lx_exc type ref to cx_pyc_frw.

    call method super->constructor( is_import = is_import ).
*Register step specific activitie: run payroll with matchcode W
    try.
        call method sts_operation_main_act_clone(
            iv_operation  = cl_pyc_pt_sts_err=>gcs_operation-addl_act
            iv_sort_field = 2000 ).

      catch cx_pyc_frw into lx_exc.
        raise exception type cx_pyd_fnd exporting previous = lx_exc.

    endtry.
  endmethod.


  METHOD FP3_BPC_DET_RT_GET_LIST.
*----------------------------------------------------------------------*
*Date      |User ID |Description              |Chg. Ref.|WkBh Req      *
*----------------------------------------------------------------------*
*01/03/2023|1106254 |Initial development      |STPS-187 |CFAK902866
*                    Copy of STT STP class               Revtrac - 1220
*                    updated with STP2 selectn
*                    with Test run update DB
*----------------------------------------------------------------------*

* Assign Report, Selection and Log Classes
    DATA:
      ls_dto           TYPE cl_pyc_rd_dto_bpc=>ty_s_rd,
      lv_test_run      TYPE xfeld,
      ls_paralle_value TYPE n LENGTH 5,
      lv_proc_inst_id  TYPE pyc_proc_inst_id,
      lo_sel_access    TYPE REF TO if_pyc_selection_oc_access,
      lt_selection     TYPE if_pyc_selection_oc_access=>ty_t_selection,
      ls_selection     TYPE if_pyc_selection_oc_access=>ty_s_selection.

    CONSTANTS : lc_program                    TYPE char7 VALUE 'PROGRAM',
                lc_variant                    TYPE char7 VALUE 'VARIANT',
                lc_cl_pyc_bpc_log_base        TYPE char19 VALUE 'CL_PYC_BPC_LOG_BASE',
                lc_cl_pyc_bpc_report_simple   TYPE char24 VALUE 'CL_PYC_BPC_REPORT_SIMPLE',
                lc_cl_pyc_bpc_report_parallel TYPE char26 VALUE 'CL_PYC_BPC_REPORT_PARALLEL'.

    lv_test_run = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_test_run
                          it_par      = io_res_context->mt_par ).

    lv_proc_inst_id = cl_pyd_fnd_aux=>get_resp_fixed_value(
       iv_par_type = cl_pyc_rt_facade=>gcs_par_type-proc_inst
       it_par      = io_res_context->mt_par ).

    ls_paralle_value = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_paralle
                          it_par      = io_res_context->mt_par ).

    lo_sel_access = cl_pyc_selection_oc_access=>get_instance( ).
    lt_selection = lo_sel_access->selection_list_get( iv_proc_inst_id = lv_proc_inst_id ).

    LOOP AT lt_selection INTO ls_selection.
      AT NEW package_id.
        IF ls_paralle_value > 0 .
          ls_dto-rpt_chain_id  = ls_selection-package_id.
          ls_dto-rpt_cat       = if_pyc_bpc_report=>category-pm_support .
          ls_dto-rpt_rt        = lc_cl_pyc_bpc_report_parallel.
          ls_dto-sel_rt        = gc_sub_clsnm.
          ls_dto-log_rt        = lc_cl_pyc_bpc_log_base.
          ls_dto-rpt_type      = lc_program.
          ls_dto-var_type      = lc_variant.

          IF lv_test_run EQ abap_true.
            ls_dto-job_base_name = text-j01.
          ELSE.
            ls_dto-job_base_name = text-j02.
          ENDIF.
          APPEND ls_dto TO r_result.
        ELSE.
          ls_dto-rpt_chain_id  = ls_selection-package_id.
          ls_dto-rpt_cat       = if_pyc_bpc_report=>category-pm_support .
          ls_dto-rpt_rt        = lc_cl_pyc_bpc_report_simple.
          ls_dto-sel_rt        = gc_sub_clsnm.
          ls_dto-log_rt        = lc_cl_pyc_bpc_log_base.
          ls_dto-rpt_type      = lc_program.
          ls_dto-var_type      = lc_variant.

          IF lv_test_run EQ abap_true.
            ls_dto-job_base_name = text-j01.
          ELSE.
            ls_dto-job_base_name = text-j02.
          ENDIF.
          APPEND ls_dto TO r_result.
        ENDIF.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  method VETO_CHECK.

    data:
      lt_par_type_so type /iwbep/t_cod_select_options,
      lt_dev_sel_obj type if_pyd_shadow_access=>ty_t_shadow_item.

    rv_veto_raised = abap_false.
    if iv_rd_type <> cl_pyc_pt_sts_err=>gcs_operation-addl_act.
      return.
    endif.

    lt_par_type_so = cl_pyd_fnd_aux=>set_so_fixed_value( cl_pyd_par_type_pernr=>gc_par_type ).
    lt_dev_sel_obj = dev_sel_obj_get_list( io_res_context = io_res_context it_par_type_so = lt_par_type_so ).
    if lt_dev_sel_obj is not initial.
      rv_veto_raised = abap_false. "deviating selection for PERNR exists -> additional activity active
    else.
      rv_veto_raised = abap_true.  "no deviating selection for PERNR -> additional activity not active
    endif.

  endmethod.
ENDCLASS.
