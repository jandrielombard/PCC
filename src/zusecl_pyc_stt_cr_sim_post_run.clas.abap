class ZUSECL_PYC_STT_CR_SIM_POST_RUN definition
  public
  inheriting from CL_PYC_STT_ASYNC_BATCH_BASE
  create public

  global friends CL_PYD_FND_AUX .

public section.

  constants:
    begin of gc_gen_process_key,
        run_id type  cl_pyc_stt_base=>ty_gp_type  value 'PYCRUNID',
      end of gc_gen_process_key .

  methods CONSTRUCTOR
    importing
      !IS_IMPORT type TY_S_CONSTR
    raising
      CX_PYD_FND .

  methods IF_PYC_BPC_CONSUMER~HANDLE_CONTAINER_NOTIFY
    redefinition .
  protected section.

    constants gcv_par_paralle type pyd_par_type value 'PYP_INTERVAL' ##NO_TEXT.

    methods al_header_cat_name_get
        redefinition .
    methods exe_det_operation_name_get
        redefinition .
    methods fp3_bpc_det_rt_get_list
        redefinition .
    methods fp3_exe_det_info_get
        redefinition .
    methods fp3_get_txt_cfgs_internal
        redefinition .
    methods get_fill_params
        redefinition .
    methods get_log_msgnr
        redefinition .
    methods get_param_filler_kinds
        redefinition .
    methods get_txt_cfgs_internal
        final redefinition .
    methods job_det_sts_get_list
        redefinition .
    methods old_exe_det_info_get
        redefinition .
    methods refresh_other_info
        redefinition .
    methods write_other_info_to_reso
        redefinition .
    methods veto_check
        redefinition .
  private section.

    constants:
      begin of gcs_post_otr_alias,
        main_act     type sotr_alias value 'PAOC_PAY_PYD_PCC_CONT/PYC_STT_SIMU_POST_MAIN_ACT',
        main_act_fp3 type sotr_alias value 'PAOC_PAY_PYD_PCC_CONT/PYC_STT_SIMU_POST_MN_ACT_FP3',
        op_decision  type sotr_alias value 'PAOC_PAY_PYD_PCC_CONT/PYC_STT_SIMU_POST_ACT_DEC',
      end of gcs_post_otr_alias .
    data mv_post_run_text type rsparams-low .

    methods get_text_run
      importing
        !io_res_context type ref to if_pyd_res_context
      returning
        value(r_result) type rsparams-low .
    methods write_run_id_to_reso
      importing
        !iv_run_id      type p_evnum
        !io_res_context type ref to if_pyd_res_context
      raising
        cx_pyd_fnd .
    methods get_run_number
      importing
        !io_res_context type ref to if_pyd_res_context
      returning
        value(r_result) type p_evnum
      raising
        cx_pyd_fnd .
ENDCLASS.



CLASS ZUSECL_PYC_STT_CR_SIM_POST_RUN IMPLEMENTATION.


  method AL_HEADER_CAT_NAME_GET.
    case iv_al_header_cat.
      when gcs_al_header_cat-main_act.
        rv_name = text-006.
      when gcs_al_header_cat-sub_sts_chg.
        rv_name = text-007.
      when gcs_al_header_cat-confirm.
        rv_name = text-008.
      when gcs_asy_batch_al_header_cat-sts_job_plan_fail.
        rv_name = text-009.
      when gcs_asy_batch_al_header_cat-sts_job_sts_fail.
        rv_name = text-010.
      when gcs_asy_batch_al_header_cat-sts_job_sts_ok.
        rv_name = text-011.
      when others.
        rv_name = super->al_header_cat_name_get( iv_al_header_cat ).
    endcase.

  endmethod.


  method CONSTRUCTOR.
    call method super->constructor( is_import = is_import ).
    try.
        call method sts_operation_main_act_clone(
            iv_operation  = cl_pyc_pt_sts_err=>gcs_operation-addl_act
            iv_sort_field = 2000 ).

      catch cx_pyc_frw into data(lx_exc).
        raise exception type cx_pyd_fnd exporting previous = lx_exc.
    endtry.
  endmethod.


  method EXE_DET_OPERATION_NAME_GET.
    case iv_operation.
      when cl_pyc_pt_sts_err=>gcs_operation-main_act.
        rv_name = text-014.
      when cl_pyc_pt_sts_err=>gcs_operation-addl_act.
        rv_name = text-015.
      when others.
        call method super->exe_det_operation_name_get(
            iv_operation   = iv_operation
            io_res_context = io_res_context ).
    endcase.
  endmethod.


  method FP3_BPC_DET_RT_GET_LIST.
* Re defined standard SAP method to allow parallel processing of posting simulation program
    data ls_dto type cl_pyc_rd_dto_bpc=>ty_s_rd.
*>>> Start of WOW Specific code
    data: ls_paralle_value type n length 5.

    ls_paralle_value = cl_pyd_fnd_aux=>get_resp_fixed_value(
                          iv_par_type = gcv_par_paralle
                          it_par      = io_res_context->mt_par ).
    if ls_paralle_value > 0 .
      "CREATE
      clear ls_dto.
      ls_dto-rpt_chain_id = '001'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_PARALLEL'.
      ls_dto-sel_rt = 'CL_PYC_BPC_SELECTION_PNPCE_REP'.
      ls_dto-log_rt = 'CL_PYC_BPC_LOG_POSTING_SIM'. "'CL_PYC_BPC_LOG_BASE'. "for reading rejected object message generically
      ls_dto-rpt_type = 'PROGRAM'.
      ls_dto-var_type ='VARIANT'.
      ls_dto-job_base_name = text-012.
      ls_dto-rejo_cat = 'SIMPO'.
      append ls_dto to r_result.

      if io_res_context->ms_shadow_type_info-shadow_type <> 'RC' and
         io_res_context->ms_shadow_type_info-shadow_type <> 'EH'.
        "DISPLAY DOCUMENT if it is not re-check or event handler process
        clear ls_dto.
        ls_dto-rpt_chain_id = '002'.
        ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
        ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
        ls_dto-sel_rt = 'ZUSECL_PYC_BPC_SEL_POSTG_RUNID'.
        ls_dto-rpt_name = 'RPCIPS00'.
        ls_dto-var_type ='DEFAULT'.  "a dummy variant type to avoid variant taken from the main report.
        ls_dto-job_base_name = text-013.
        append ls_dto to r_result.
      endif.
    else.
*<<< End of WOW Specific Code
      "CREATE
      clear ls_dto.
      ls_dto-rpt_chain_id = '001'.
      ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
      ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
      ls_dto-sel_rt = 'CL_PYC_BPC_SELECTION_PNPCE'.
      ls_dto-log_rt = 'CL_PYC_BPC_LOG_POSTING_SIM'. "'CL_PYC_BPC_LOG_BASE'. "for reading rejected object message generically
      ls_dto-rpt_type = 'PROGRAM'.
      ls_dto-var_type ='VARIANT'.
      ls_dto-job_base_name = text-012.
      ls_dto-rejo_cat = 'SIMPO'.
      append ls_dto to r_result.

      if io_res_context->ms_shadow_type_info-shadow_type <> 'RC' and
         io_res_context->ms_shadow_type_info-shadow_type <> 'EH'.
        "DISPLAY DOCUMENT if it is not re-check or event handler process
        clear ls_dto.
        ls_dto-rpt_chain_id = '002'.
        ls_dto-rpt_cat = if_pyc_bpc_report=>category-pm_support .
        ls_dto-rpt_rt = 'CL_PYC_BPC_REPORT_SIMPLE'.
        ls_dto-sel_rt = 'CL_PYC_BPC_SEL_POSTING_RUNID'.
        ls_dto-rpt_name = 'RPCIPS00'.
        ls_dto-var_type ='DEFAULT'.  "a dummy variant type to avoid variant taken from the main report.
        ls_dto-job_base_name = text-013.
        append ls_dto to r_result.
      endif.
*>>> Start of WOW Specific code
    endif.
*<<< End of WOW Specific Code
  endmethod.


  method FP3_EXE_DET_INFO_GET.
    rs_redef-text = otr_text_get( gcs_post_otr_alias-main_act_fp3 ).
    rs_redef-sts_op_decision_text = otr_text_get( gcs_post_otr_alias-op_decision ).
  endmethod.


  method FP3_GET_TXT_CFGS_INTERNAL.

    data ls_txt_cfg type ty_s_txt_cfg.

    try.
        call method super->fp3_get_txt_cfgs_internal
          exporting
            io_res_context = io_res_context
          importing
            et_txt_cfgs    = et_txt_cfgs.

        "excute description
        clear ls_txt_cfg .
        ls_txt_cfg-text_group_name  = if_pyc_txt_provider=>exe_dtl_desc_grp-group_name.
        ls_txt_cfg-text_type        = if_pyc_txt_provider=>exe_dtl_desc_grp-execution_details_descripion .
        ls_txt_cfg-inner_text_type  = gcs_post_otr_alias-main_act_fp3 .
        ls_txt_cfg-text_source_type = if_pyc_txt_provider=>text_source_type-all.
        ls_txt_cfg-description      = text-002.

        call method add_txt_cfg
          exporting
            is_txt_cfg = ls_txt_cfg
          changing
            ct_txt_cfg = et_txt_cfgs.

      catch cx_pyd_fnd .
    endtry.
  endmethod.


  method GET_FILL_PARAMS.
    "EVALNAME
    data:
      ls_params type rsparams,
      lt_params type rsparams_tt.

    ls_params-kind = 'P' .
    ls_params-option = 'EQ'.
    ls_params-selname = 'EVALNAME'.
    ls_params-sign = 'I'.
    ls_params-low = get_text_run( io_res_context = io_res_context ).
    append ls_params to lt_params .

    "TSTLVL
    clear ls_params.
    ls_params-kind = 'P' .
    ls_params-option = 'EQ'.
    ls_params-selname = 'TSTLVL'.
    ls_params-sign = 'I'.
    ls_params-low = 'S'.
    append ls_params to lt_params .
    r_result = lt_params.
  endmethod.


  method GET_LOG_MSGNR.
    data lv_dummy(1) type c.
    data lv_step_name type pyd_d_tyt-name.

    select single name from pyd_d_tyt into lv_step_name
     where sprsl = sy-langu
       and type = if_pyd_ty_rt~mv_type.
    case iv_log_type .
      when gcs_log_type-pyp_log_main_act .
*        r_result = '083' .
        cs_msg-msgnr = '140'.
        cs_msg-value1 = lv_step_name.
        if 1 = 0.
          message i140(pyc_cont) into lv_dummy ##MG_MISSING.
        endif.
      when gcs_log_type-pyp_log_addl_act .
*        r_result = '008' .
        cs_msg-msgnr = '140'.
        cs_msg-value1 = lv_step_name.
        if 1 = 0.
          message i141(pyc_cont) into lv_dummy ##MG_MISSING.
        endif.
      when gcs_log_type-pyp_log_job_plan_fail .
        cs_msg-msgnr = '084' .
        if 1 = 0.
          message i084(pyc_cont) into lv_dummy ##MG_MISSING.
        endif.
      when gcs_log_type-pyp_log_job_plan_ok .
        cs_msg-msgnr = '085' .
        if 1 = 0.
          message i085(pyc_cont) into lv_dummy ##MG_MISSING.
        endif.
      when gcs_log_type-pyp_log_job_name .
        cs_msg-msgnr = '086' .
        if 1 = 0.
          message i086(pyc_cont) into lv_dummy ##MG_MISSING.
        endif.
      when gcs_log_type-pyp_log_job_runing .
        cs_msg-msgnr = '087' .
        if 1 = 0.
          message i087(pyc_cont) into lv_dummy ##MG_MISSING.
        endif.
      when gcs_log_type-pyp_log_job_run_fail .
        cs_msg-msgnr = '088' .
        if 1 = 0.
          message i088(pyc_cont) into lv_dummy ##MG_MISSING.
        endif.
      when gcs_log_type-pyp_log_job_run_ok .
        cs_msg-msgnr = '089' .
        if 1 = 0.
          message i089(pyc_cont) into lv_dummy ##MG_MISSING.
        endif.
    endcase.

    r_result = cs_msg-msgnr.
  endmethod.


  method GET_PARAM_FILLER_KINDS.
    r_result = cl_pyc_param_filler_factory=>gc_kinds-pnp_free .
  endmethod.


  method GET_RUN_NUMBER.
    data:
      lv_roid       type pyd_roid,
      lt_run_id     type standard table of p_evnum,
      lv_run_id     type p_evnum,
      lv_run_id_num type i,
      ls_pevsh      type pevsh,
      ls_job_header type tbtcjob,
      ls_debug_str  type tvarv_val.

    "get run text .
    ls_job_header = get_job_header(  io_res_context = io_res_context ) .

    check  ls_job_header is not initial .
    clear   lv_roid.
    call method io_res_context->result_object_singleton_get(
      exporting
        iv_par_type = cl_pyc_pt_sts_post_run_text=>gc_par_type
      importing
        ev_roid     = lv_roid ).

    " search in the table
    check lv_roid is not initial .
    select runid into table lt_run_id from pevst where name = lv_roid .

    if sy-subrc = 0 .
      lv_run_id_num = lines( lt_run_id ) .
      if lv_run_id_num = 1.
        read table lt_run_id into  lv_run_id  index 1.  "#EC CI_NOORDER
        r_result = lv_run_id .
      else.
        " check time
        loop at lt_run_id into lv_run_id .
          select single * into ls_pevsh from pevsh  where runid = lv_run_id .
          if sy-subrc = 0 .
            if ls_pevsh-creadate > ls_job_header-strtdate and  ls_pevsh-creadate < ls_job_header-enddate .
              if ls_pevsh-creatime > ls_job_header-strttime and  ls_pevsh-creatime < ls_job_header-endtime .
                r_result = lv_run_id .
              endif.
            endif.
          endif.
        endloop.
      endif.
    endif.
  endmethod.


  method GET_TEXT_RUN.
    data:
      lv_tmp_dt type string,
      lv_tmp_tm type string,
      lv_date   type dats,
      lv_time   type tims,
      lv_abkrs  type abkrs,
      lv_dummy  type string.

    data: lcx_pyd_fnd type ref to cx_pyd_fnd,
          lv_tmp_str  type tvarv_val.

    clear mv_post_run_text.
    try.
        lv_abkrs = cl_pyd_fnd_aux=>get_resp_fixed_value(
                    iv_par_type = if_pyd_cont_types=>gcs_par_type-abkrs it_par = io_res_context->mt_par  ).
        lv_date = sy-datum .
        lv_time = sy-uzeit.
        lv_tmp_dt = lv_date .
        lv_tmp_dt = lv_tmp_dt+1(7).
        lv_tmp_tm = lv_time .
      catch cx_pyd_fnd into lcx_pyd_fnd .
        lv_tmp_str = lcx_pyd_fnd->get_text( ) .
        log_append( iv_msgnr = '072' iv_var1 = lv_tmp_str io_res_context = io_res_context ).
    endtry.
    concatenate lv_abkrs lv_tmp_dt lv_tmp_tm into  mv_post_run_text .
    r_result = mv_post_run_text .
    log_append( iv_msgnr = '038' iv_var1 = r_result  io_res_context = io_res_context ).
    if 1 = 0 .
      message i038(pyc_cont) into lv_dummy ##MG_MISSING.
    endif.
  endmethod.


  method GET_TXT_CFGS_INTERNAL .
    data:
      ls_txt_cfg  type  ty_s_txt_cfg,
      lt_txt_cfgs type ty_t_txt_cfgs.
    data:
      lv_txt_type  type pyc_text_type,
      lv_index_str type char1 value '1'.

    call method super->get_txt_cfgs_internal
      importing
        et_txt_cfgs = et_txt_cfgs.

*sts_det_info_get
    clear ls_txt_cfg .
    ls_txt_cfg-text_group_name = if_pyc_txt_provider=>exe_dtl_desc_grp-group_name.
    ls_txt_cfg-text_type =  if_pyc_txt_provider=>exe_dtl_desc_grp-execution_details_descripion .
    ls_txt_cfg-inner_text_type = gcs_post_otr_alias-main_act .
    ls_txt_cfg-text_source_type = if_pyc_txt_provider=>text_source_type-all.
    call method add_txt_cfg
      exporting
        is_txt_cfg = ls_txt_cfg
      changing
        ct_txt_cfg = et_txt_cfgs.
*status group 2
    clear lv_txt_type .
    clear ls_txt_cfg .
    ls_txt_cfg-text_group_name = get_sub_sts_dtl_group_name( if_pyc_txt_provider=>sub_sts_dtl_index-second ) .
    ls_txt_cfg-text_type = get_sub_sts_dtl_gov_group_name(
       iv_grp_index = if_pyc_txt_provider=>sub_sts_dtl_index-second
       iv_gov_index = '_3'
       ).
    ls_txt_cfg-inner_text_type =   gcs_job_det-job_post_number_head.
    ls_txt_cfg-text_source_type = if_pyc_txt_provider=>text_source_type-all.
    ls_txt_cfg-description =   'Number of Posting Run'(s02).
    call method add_txt_cfg
      exporting
        is_txt_cfg = ls_txt_cfg
      changing
        ct_txt_cfg = et_txt_cfgs.
*status group 2  label
    clear ls_txt_cfg .
    ls_txt_cfg-text_group_name = get_sub_sts_dtl_group_name( if_pyc_txt_provider=>sub_sts_dtl_index-second ).
    ls_txt_cfg-text_type  = get_sub_sts_dtl_gov_lbl(
       iv_grp_index = if_pyc_txt_provider=>sub_sts_dtl_index-second
       iv_gov_index = '_3'
       iv_lbl_index = '_1'
       ).
    ls_txt_cfg-inner_text_type =  gcs_job_det-job_post_number_lbl.
    ls_txt_cfg-text_source_type = if_pyc_txt_provider=>text_source_type-all.
    ls_txt_cfg-description =   'Number of Posting Run'(s02).
    call method add_txt_cfg
      exporting
        is_txt_cfg = ls_txt_cfg
      changing
        ct_txt_cfg = et_txt_cfgs.

  endmethod.


  method IF_PYC_BPC_CONSUMER~HANDLE_CONTAINER_NOTIFY.

    data:
      lv_run_id          type p_evnum,
      lv_element         type swc_elem,
      ls_container       type line of swconttab,
      lo_res_context     type ref to if_pyd_res_context,
      lo_res_context_int type ref to cl_pyd_res_context_base,
      lx_exc             type ref to cx_root.

    loop at it_container into ls_container .
      lv_element = ls_container-element.
      condense lv_element .
      check lv_element = 'P_RUNID'.
      lv_run_id = ls_container-value.
      " save P_RUNID
      check lv_run_id is not initial .

      try .
          write_run_id_to_reso( iv_run_id = lv_run_id io_res_context = io_res_context ).

          call method gen_pers_store(
              iv_gp_type     = cl_pyc_stt_create_posting_run=>gc_gen_process_key-run_id
              iv_gp_value    = conv #( lv_run_id )
              io_res_context = io_res_context ).
        catch cx_root into lx_exc.
          raise exception type cx_pyc_cont exporting previous = lx_exc.
      endtry.

      exit .
    endloop.

  endmethod.


  method JOB_DET_STS_GET_LIST.
    data:
      lt_dto_gov type cl_pyd_rd_dto_gov=>ty_t_rd,
      ls_dto_gov type cl_pyd_rd_dto_gov=>ty_s_rd,
      lv_rowid   type pyd_rowid,
      lv_roid    type pyd_roid.

    data:
      lv_text_type_value type pyd_name .
    clear lt_dto_gov .
    lt_dto_gov  = super->job_det_sts_get_list( io_res_context ) .
    r_result = lt_dto_gov .


    call method io_res_context->result_object_singleton_get(
      exporting
        iv_par_type = cl_pyc_pt_sts_post_run_id=>gc_par_type
      importing
        ev_roid     = lv_roid ).
    lv_rowid = lines( lt_dto_gov ).
    add 1 to lv_rowid.
    ls_dto_gov-row_id = lv_rowid.
    ls_dto_gov-groupid = '003'.
    lv_text_type_value = 'Number of Posting Run'(s02).
    ls_dto_gov-group_name = get_txt_by_text_type( iv_text_type = gcs_job_det-job_post_number_head iv_default_val = lv_text_type_value )."'Number of Posting Run'(s02).
    ls_dto_gov-text = get_txt_by_text_type( iv_text_type = gcs_job_det-job_post_number_lbl iv_default_val = lv_text_type_value ). "'Number of Posting Run'(s02).
    ls_dto_gov-value = lv_roid .
    append ls_dto_gov to r_result.
  endmethod.


  method OLD_EXE_DET_INFO_GET.
    rs_redef-text = otr_text_get( gcs_post_otr_alias-main_act ).
  endmethod.


  method REFRESH_OTHER_INFO.
    " get run number .
    "write run number to reso
    data lv_run_id type p_evnum .
    data lv_str  type  tvarv_val  .
    data lv_dummy type string .

    lv_run_id = get_run_number(  io_res_context = io_res_context ).

    lv_str = lv_run_id .
    log_append( iv_msgnr = '039' iv_var1 = lv_str  io_res_context = io_res_context ).
    if 1 = 0 .
      message i039(pyc_cont) into lv_dummy ##MG_MISSING.
    endif.

    check lv_run_id is not initial .
    write_run_id_to_reso( iv_run_id = lv_run_id io_res_context = io_res_context ).
  endmethod.


  method VETO_CHECK.
* Trigger Employee numbers lation for initial run
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


  method WRITE_OTHER_INFO_TO_RESO.
    " store the run text
    data ls_reso_key    type pyd_s_reso_key.
    "clear ls_reso_key
    ls_reso_key-reskey   = io_res_context->ms_res_key.
    ls_reso_key-par_type = cl_pyc_pt_sts_post_run_text=>gc_par_type.
    ls_reso_key-id       = mv_post_run_text .
    call method io_res_context->result_object_key_change( ls_reso_key ).
  endmethod.


  method WRITE_RUN_ID_TO_RESO.
    data ls_reso_key    type pyd_s_reso_key.
    "clear ls_reso_key
    ls_reso_key-reskey   = io_res_context->ms_res_key.
    ls_reso_key-par_type = cl_pyc_pt_sts_post_run_id=>gc_par_type.
    ls_reso_key-id       = iv_run_id .
    call method io_res_context->result_object_key_change( ls_reso_key ).
  endmethod.
ENDCLASS.
