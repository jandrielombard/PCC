class zusecl_pyc_bpc_selection_sm definition
  public
  inheriting from cl_pyc_bpc_selection_pnp
  final
  create public .

  public section.
  protected section.

    constants:
      begin of gc_par_type,
        period type pyd_par_type value 'PERIOD',
        abkrs  type pyd_par_type value 'ABKRS',
      end of gc_par_type .

    methods get_leading_time_selection
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_PYC_BPC_SELECTION_SM IMPLEMENTATION.


  method get_leading_time_selection.
* Populate Selection Parameters
    constants: mc_abkrs      type pyd_par_type value 'ABKRS',
               mc_period     type pyd_par_type value 'PERIOD',
               mc_variant    type pyd_par_type value 'VARIANT',
               mc_par_jobnam type rsscr_name value 'S_JOBNAM',
               mc_par_jobcnt type rsscr_name value 'S_JOBCNT',
               mc_par_wfmt   type rsscr_name value 'P_WFMT',
               mc_par_prognm type rsscr_name value 'P_CALL'.

    data: ls_par        type pyd_s_resp,
          ls_sel_params type line of rsparams_tt,
          lv_sel_name   type char8.

    data:
      lv_proc_inst_id        type pyc_proc_inst_id,
      ls_proc_inst_id_so     type if_pyd_fnd_types=>ty_s_so,
      lo_proc_inst_aux       type ref to  cl_pyc_proc_inst_aux,
      lt_proc_inst_id_so     type /iwbep/t_cod_select_options,
      lx_exc_frw             type ref to  cx_pyc_frw,
      lt_step_inst           type cl_pyc_proc_inst_aux=>ty_t_step_inst,
      ls_step_inst           type cl_pyc_proc_inst_aux=>ty_s_step_inst,
      lv_step_id             type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id,
      lv_prev_step_id        type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id,
      lv_prev_step_sortfield type cl_pyc_proc_inst_aux=>ty_s_step_inst-si_sort_field.

    constants: mc_pty_program type pyd_d_instp-par_type value 'PROGRAM'.
    data: lv_py_program  type pyd_d_instp-low.

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

* Read Current Step ID
    read table it_par into ls_par index 1.
    move ls_par-instid to lv_step_id.

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
      catch cx_pyc_frw into lx_exc_frw.
        " do nothing. Just keep empty table.
        return.
    endtry.

* Determine Previous Step
    read table lt_step_inst into ls_step_inst
      with key step_id = lv_step_id.
    if sy-subrc eq 0.
      lv_prev_step_sortfield = ls_step_inst-si_sort_field - 1.
      read table lt_step_inst into ls_step_inst
        with key si_sort_field = lv_prev_step_sortfield.
      if sy-subrc eq 0.
        move ls_step_inst-step_id to lv_prev_step_id.
      endif.
    endif.

    if not lv_prev_step_id is initial.
* Read Batch Processing Component ID
      clear: lv_pyc_bpc_id.
      select single id into lv_pyc_bpc_id from  pyc_d_bpc
       where  pypi_id  = lv_proc_inst_id
         and    step_id  = lv_prev_step_id.
      if sy-subrc eq 0.
* Read Job names and Job number details for the previous step
        select bpc_id row_id job_name job_count
          into corresponding fields of table lt_pyc_d_bpc_job
          from pyc_d_bpc_job
          where bpc_id = lv_pyc_bpc_id.
      endif.

* Read Process Step Program Name
      select single low into lv_py_program
        from pyd_d_instp
       where id        = lv_prev_step_id
         and par_type  = mc_pty_program.

    endif.
    sort lt_pyc_d_bpc_job by row_id.

* Populate Spool Merge Program Selection
    clear et_sel_params.
* Selection Parameter - Program Name
    lv_sel_name = mc_par_prognm.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind = 'P' .
    ls_sel_params-option = 'EQ' .
    ls_sel_params-sign = 'I' .
    ls_sel_params-low = lv_py_program.
    append ls_sel_params to et_sel_params .

    loop at lt_pyc_d_bpc_job into ls_pyc_d_bpc_job.
* Selection Option - Job Name
      lv_sel_name = mc_par_jobnam.
      clear ls_sel_params .
      ls_sel_params-selname = lv_sel_name.
      ls_sel_params-kind = 'S' .
      ls_sel_params-option = 'EQ' .
      ls_sel_params-sign = 'I' .
      ls_sel_params-low = ls_pyc_d_bpc_job-job_name.
      append ls_sel_params to et_sel_params .
* Selection Option - Job Number
      lv_sel_name = mc_par_jobcnt.
      clear ls_sel_params .
      ls_sel_params-selname = lv_sel_name.
      ls_sel_params-kind = 'S' .
      ls_sel_params-option = 'EQ' .
      ls_sel_params-sign = 'I' .
      ls_sel_params-low = ls_pyc_d_bpc_job-job_count.
      append ls_sel_params to et_sel_params .
    endloop.

* Selection Parameter - With Formatting
*    lv_sel_name = mc_par_wfmt.
*    clear ls_sel_params .
*    ls_sel_params-selname = lv_sel_name.
*    ls_sel_params-kind = 'P' .
*    ls_sel_params-option = 'EQ' .
*    ls_sel_params-sign = 'I' .
*    ls_sel_params-low = 'X'.
*    append ls_sel_params to et_sel_params .


    if lines( et_sel_params ) = 0 .
      raise exception type cx_pyc_cont
        exporting
          textid = cx_pyc_cont=>get_leading_time_selection_err.
    endif.

  endmethod.
ENDCLASS.
