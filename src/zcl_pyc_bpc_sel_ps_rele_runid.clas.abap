class zcl_pyc_bpc_sel_ps_rele_runid definition
  public
  inheriting from cl_pyc_bpc_selection_npnp
  final
  create public .

  public section.
    types: tty_runid type table of p_evnum.
  protected section.

    constants gc_evattr_akper type pevat-attr value 'AKPER' ##NO_TEXT.
    constants:
      begin of gc_par_type,
        abkrs  type pyd_par_type value 'ABKRS',
        period type pyd_par_type value 'PERIOD',
        slasg  type c value '/',
      end of gc_par_type .
    constants gc_slash type c value '/' ##NO_TEXT.
    constants mc_post_doc_parallel type pyc_proc_step_template_id value 'ZHR_V2_POST_DOC_PARALLEL' ##NO_TEXT.

    methods get_run_numbers
      importing
        !io_context     type ref to if_pyd_res_context
        !iv_shadow_id   type pyd_shadow_id
      exporting
        value(rt_runid) type tty_runid .
    methods get_posting_job_details
      importing
        !it_par               type if_pyd_fnd_types=>ty_t_resp
      returning
        value(et_jobs_detail) type tbtcjob_tt .

    methods get_other_params
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_BPC_SEL_PS_RELE_RUNID IMPLEMENTATION.


  method get_other_params.
    data:
      lt_runid      type tty_runid,
      lv_runid      type char32,
      ls_sel_params type rsparams.

    call method super->get_other_params
      exporting
        io_context    = io_context
        iv_shadow_id  = iv_shadow_id
      importing
        et_sel_params = et_sel_params.

* Read Run Numbers
    call method me->get_run_numbers
      exporting
        io_context   = io_context
        iv_shadow_id = iv_shadow_id
      importing
        rt_runid     = lt_runid.

* Populate S_RUNID Selection
    loop at lt_runid into lv_runid.
      ls_sel_params-selname = 'S_RUNID' .
      ls_sel_params-kind = 'P' .
      ls_sel_params-low = lv_runid .
      ls_sel_params-option = 'EQ' .
      ls_sel_params-sign = 'I' .
      append ls_sel_params to et_sel_params.
    endloop.

    if lines( et_sel_params ) = 0 .
      raise exception type cx_pyc_cont
        exporting
          textid = cx_pyc_cont=>get_leading_time_selection_err.
    endif.

  endmethod.


  method get_posting_job_details.
* Read Posting Jobs Headers
    data: ls_par          type pyd_s_resp.
    data:
      lv_proc_inst_id          type pyc_proc_inst_id,
      ls_proc_inst_id_so       type if_pyd_fnd_types=>ty_s_so,
      lo_proc_inst_aux         type ref to  cl_pyc_proc_inst_aux,
      lt_proc_inst_id_so       type /iwbep/t_cod_select_options,
      lx_exc_frw               type ref to  cx_pyc_frw,
      lt_step_inst             type cl_pyc_proc_inst_aux=>ty_t_step_inst,
      ls_step_inst             type cl_pyc_proc_inst_aux=>ty_s_step_inst,
      lt_step_inst_enrich      type cl_pyc_rt_facade=>ty_t_step_inst,
      ls_step_inst_enrich      type cl_pyc_rt_facade=>ty_s_step_inst,
      lv_current_step_id       type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id,
      lv_create_step_id        type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id,
      lv_create_step_sortfield type cl_pyc_proc_inst_aux=>ty_s_step_inst-si_sort_field.

* Job Details
    data: lv_pyc_bpc_id type pyc_bpc_id.
    types: begin of ty_pyc_d_bpc_job ,
             bpc_id    type pyc_bpc_id,
             row_id    type pyd_rowid_long,
             job_name  type btcjob,
             job_count type btcjobcnt.
    types: end of ty_pyc_d_bpc_job.
    data: lt_pyc_d_bpc_job type table of ty_pyc_d_bpc_job,
          ls_pyc_d_bpc_job type ty_pyc_d_bpc_job.

* Job Header
    data: ls_job_header type tbtcjob.
    data: lo_exec_prog type ref to if_pyc_process_prog.

    clear: et_jobs_detail.
* Read Current Step ID
    read table it_par into ls_par index 1.
    move ls_par-instid to lv_current_step_id.

* Read Process Instance ID
    try.
        lv_proc_inst_id = cl_pyd_fnd_aux=>get_resp_fixed_value(
                            iv_par_type = cl_pyc_rt_facade=>gcs_par_type-proc_inst
                            it_par      = it_par ).
        if lv_proc_inst_id is initial.
          return.
        endif.
      catch cx_pyd_fnd.
        " do nothing. Just keep empty table.
        return.
    endtry.

* Read Process Instance Steps
    ls_proc_inst_id_so-so = cl_pyd_fnd_aux=>set_so_fixed_value( lv_proc_inst_id ).
    try .

        lt_proc_inst_id_so = cl_pyd_fnd_aux=>set_so_fixed_value( lv_proc_inst_id ).
      catch cx_pyd_fnd.
        " do nothing. Just keep empty table.
        return.
    endtry.
    try.
        lo_proc_inst_aux = cl_pyc_proc_inst_aux=>get_instance( mo_fnd_factory->mo_transaction ).
        lt_step_inst = lo_proc_inst_aux->step_inst_get_list(
                                        it_proc_inst_id_so = lt_proc_inst_id_so
                                        iv_with_names      = abap_false ).

* Read Step Template IDs for the Process
        lt_step_inst_enrich = lo_proc_inst_aux->step_inst_enrich(
              it_step_inst      = lt_step_inst
              iv_with_names     = abap_true
              iv_with_pi_info   = abap_false
              iv_with_p_info    = abap_false
              iv_with_s_info    = abap_true
              iv_with_st_info   = abap_true
              iv_with_stgt_info = abap_true
              iv_with_gi_info   = abap_false ).
      catch cx_pyc_frw into lx_exc_frw.
        " do nothing. Just keep empty table.
        return.
    endtry.

* Identify Create Posting Step ID
    clear: ls_step_inst_enrich, lv_create_step_id.
    read table lt_step_inst_enrich into ls_step_inst_enrich
      with key step_template_id	= mc_post_doc_parallel.
    if sy-subrc eq 0.
      lv_create_step_id = ls_step_inst_enrich-step_id.
    endif.

    if not lv_create_step_id is initial.
* Read Batch Processing Component ID
      clear: lv_pyc_bpc_id.
      select single id into lv_pyc_bpc_id from  pyc_d_bpc
       where  pypi_id  = lv_proc_inst_id
         and  step_id  = lv_create_step_id.
      if sy-subrc eq 0.
* Read Job names and Job number details for the previous step
        select bpc_id row_id job_name job_count
          into corresponding fields of table lt_pyc_d_bpc_job
          from pyc_d_bpc_job
          where bpc_id = lv_pyc_bpc_id.
      endif.
    endif.
    sort lt_pyc_d_bpc_job by row_id.

* Read Jobs Header Info
    create object lo_exec_prog type cl_pyc_basic_process_prog.
    loop at lt_pyc_d_bpc_job into ls_pyc_d_bpc_job.
      call method lo_exec_prog->get_job_info(
        exporting
          imp_job_name  = ls_pyc_d_bpc_job-job_name       "job name
          imp_job_count = ls_pyc_d_bpc_job-job_count      "job count
        importing
          es_job_header = ls_job_header ).
      append ls_job_header to et_jobs_detail.
    endloop.

  endmethod.


  method get_run_numbers.
* Read Posting Run numbers for the Instance
    data:
      lt_par            type if_pyd_fnd_types=>ty_t_resp,
      ls_par            type if_pyd_fnd_types=>ty_s_resp,
      lv_payroll_area   type abkrs,
      lv_payroll_period type fpper,
      lv_runid          type p_evnum,
      ls_pevst          type pevst,
      lv_void           type p_evname,
      lt_pevst          type standard table of pevst,
      lt_pevsh          type standard table of pevsh,
      ls_pevsh          type pevsh,
      lv_akper          type pevat-value,
      lv_attr_value     type pevat-value,
      lt_job_header     type tbtcjob_tt,
      ls_job_header     type tbtcjob.

    data: lv_jobs_strtdate type datum,
          lv_jobs_enddate  type datum,
          lv_jobs_strttime type uzeit,
          lv_jobs_endtime  type uzeit.
    data: lx_exc           type ref to cx_root.
*
    refresh: lt_job_header, lt_pevst, lt_pevsh, rt_runid.
    lt_par = io_context->mt_par.

    "get instance posting job headers
    lt_job_header = get_posting_job_details( lt_par ) .
    check  lt_job_header is not initial .
* Jobs Start Date and Time
    sort lt_job_header by strtdate ascending strttime ascending.
    loop at lt_job_header into ls_job_header.
      lv_jobs_strtdate = ls_job_header-strtdate.
      lv_jobs_strttime = ls_job_header-strttime.
      exit.
    endloop.
* Jobs End date and Time
    sort lt_job_header by enddate descending endtime descending.
    loop at lt_job_header into ls_job_header.
      lv_jobs_enddate = ls_job_header-enddate.
      lv_jobs_endtime = ls_job_header-endtime.
      exit.
    endloop.

* Stored RunID for the run
    clear lv_runid.
    try.
        lv_runid = gen_pers_get(
              iv_gp_type     = cl_pyc_stt_create_posting_run=>gc_gen_process_key-run_id
              io_res_context = io_context
              iv_shadow_id = iv_shadow_id
            ).
      catch cx_root into lx_exc.
    endtry.
    check lv_runid is not initial .

* Read All RUNID's for the Evaluation Name
    clear ls_pevst.
    select single * into ls_pevst from pevst where runid = lv_runid.
    check ls_pevst is not initial .

* Read RUNID with in the jobs execution period
    refresh: lt_pevsh.
    select * into table lt_pevsh
      from pevsh
     where type = ls_pevst-type
      and actual = abap_true
      and ( creadate between lv_jobs_strtdate and lv_jobs_enddate )
      and ( creatime between lv_jobs_strttime and lv_jobs_endtime ).

    check lt_pevsh is not initial.
* Read Payroll Area and Period
    read table lt_par into ls_par with key par_type = gc_par_type-abkrs.
    if sy-subrc = 0.
      lv_payroll_area = ls_par-low.
    endif.
    read table lt_par into ls_par with key par_type = gc_par_type-period.
    if sy-subrc = 0.
      lv_payroll_period = ls_par-low.
    endif.
* Build Posting Attibute AKPER
    lv_akper = lv_payroll_area && gc_slash
               && lv_payroll_period+4(2) && gc_slash && lv_payroll_period+0(4).

    loop at lt_pevsh into ls_pevsh  .
* Check Evaluation Name
      select single name into lv_void from pevst where runid = ls_pevsh-runid.
      check lv_void eq ls_pevst-name.
* Check Payroll Area and Period
      clear: lv_attr_value.
      call function 'HR_EVAL_ATTR_GET'
        exporting
          type           = ls_pevsh-type
          runid          = ls_pevsh-runid
          attr           = gc_evattr_akper
        importing
          value          = lv_attr_value
        exceptions
          run_not_found  = 1
          attr_not_found = 2
          others         = 3.
      if lv_attr_value = lv_akper.
        append ls_pevsh-runid to rt_runid.
      endif.
    endloop.

  endmethod.
ENDCLASS.
