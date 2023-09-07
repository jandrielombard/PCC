class zcl_pyc_stt_oc_stp_generation definition
  public
  inheriting from cl_pyc_stt_oc_async_base
  create public

  global friends cl_pyd_rdt_aux .

  public section.

    types:
      begin of job_s_key,
        job_name  type btcjob,
        job_count type btcjobcnt,
      end of job_s_key .
    types:
      job_t_key type standard table of job_s_key .

    constants gc_sub_clsnm type classname value 'ZCL_PYC_BPC_SELECTION_OC_STP' ##NO_TEXT.

    methods constructor
      importing
        !is_import type ty_s_constr
      raising
        cx_pyd_fnd .
  protected section.

    methods fp3_bpc_det_rt_get_list
        redefinition .
    methods veto_check
        redefinition .
  private section.

    constants:
      begin of gcs_run_payroll_otr_alias,
        main_act type sotr_alias value 'PAOC_PAY_PYD_PCC_CONT/PYC_STT_RUN_PY_MAIN_ACT',
      end of gcs_run_payroll_otr_alias .
    constants:
      begin of gcs_payroll_act_otr_alias,
        main_act_prd type sotr_alias value 'PAOC_PAY_PYD_PCC_CONT/PYC_STT_PAYROLL_ACT_MAIN_PRD',
        op_decision  type sotr_alias value 'PAOC_PAY_PYD_PCC_CONT/PYC_STT_PAYROLL_ACT_DECISION',
      end of gcs_payroll_act_otr_alias .
    constants gcv_tpy_res type pyd_par_type value 'TPY_RES' ##NO_TEXT.
    constants gcv_par_paralle type pyd_par_type value 'PYP_INTERVAL' ##NO_TEXT.
    constants: gcv_par_test_run type pyd_par_type value 'Z99_TEST_RUN'.
ENDCLASS.



CLASS ZCL_PYC_STT_OC_STP_GENERATION IMPLEMENTATION.


  method constructor.

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


  method fp3_bpc_det_rt_get_list.
* Assign Report, Selection and Log Classes

    data:
      ls_dto           type cl_pyc_rd_dto_bpc=>ty_s_rd,
      lv_test_run      type xfeld,
      ls_paralle_value type n length 5,
      lv_proc_inst_id  type pyc_proc_inst_id,
      lo_sel_access    type ref to if_pyc_selection_oc_access,
      lt_selection     type if_pyc_selection_oc_access=>ty_t_selection,
      ls_selection     type if_pyc_selection_oc_access=>ty_s_selection.

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

    loop at lt_selection into ls_selection.
      at new package_id.
        if ls_paralle_value > 0 .
          ls_dto-rpt_chain_id  = ls_selection-package_id.
          ls_dto-rpt_cat       = if_pyc_bpc_report=>category-pm_support .
          ls_dto-rpt_rt        = 'CL_PYC_BPC_REPORT_PARALLEL'.
          ls_dto-sel_rt        = gc_sub_clsnm.
          ls_dto-log_rt        = 'CL_PYC_BPC_LOG_BASE'.
          ls_dto-rpt_type      = 'PROGRAM'.
          ls_dto-var_type      = 'VARIANT'.
          if lv_test_run eq abap_true.
            ls_dto-job_base_name = text-j01.
          else.
            ls_dto-job_base_name = text-j02.
          endif.
          append ls_dto to r_result.
        else.
          ls_dto-rpt_chain_id  = ls_selection-package_id.
          ls_dto-rpt_cat       = if_pyc_bpc_report=>category-pm_support .
          ls_dto-rpt_rt        = 'CL_PYC_BPC_REPORT_SIMPLE'.
          ls_dto-sel_rt        = gc_sub_clsnm.
          ls_dto-log_rt        = 'CL_PYC_BPC_LOG_BASE'.
          ls_dto-rpt_type      = 'PROGRAM'.
          ls_dto-var_type      = 'VARIANT'.
          if lv_test_run eq abap_true.
            ls_dto-job_base_name = text-j01.
          else.
            ls_dto-job_base_name = text-j02.
          endif.
          append ls_dto to r_result.
        endif.
      endat.
    endloop.

  endmethod.


  method veto_check.

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
