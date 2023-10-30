class ZUSECL_PYC_KPI_PAYROLL_EE_CNT definition
  public
  inheriting from CL_PYC_KPI_CHART_BASE
  create protected

  global friends CL_PYD_FND_AUX .

public section.

  types:
    begin of ty_spool,
        spoolid  type tbtc_spoolid-spoolid,
        jobname  type tbtc_spoolid-jobname,
        jobcount type tbtc_spoolid-jobcount,
        rqident  type tsp01-rqident,
        rq1name  type tsp01-rq1name,
        rq2name  type tsp01-rq2name,
        rqtitle  type tsp01-rqtitle,
      end of ty_spool .
protected section.

  methods GET_PAYROLL_JOBS_DETAIL
    importing
      !IT_PAR type IF_PYD_FND_TYPES=>TY_T_RESP
    exporting
      !ET_JOBNAM type ZBTCJOB_RANGE_TABLE
      !ET_JOBCNT type TCJRANGE .
  methods GET_SPOOL_FOR_ALL_JOBS
    importing
      !IT_JOBNAM type ZBTCJOB_RANGE_TABLE
      !IT_JOBCNT type TCJRANGE
    exporting
      !ET_LIST type LIST_STRING_TABLE .

  methods CHART_INFO_CALCULATE
    redefinition .
  methods CHART_ITEM_TEXT_GET
    redefinition .
  methods FOOTER_GET
    redefinition .
  methods HEADER_GET
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_PYC_KPI_PAYROLL_EE_CNT IMPLEMENTATION.


  method CHART_INFO_CALCULATE.
* Payroll Process Statastics using bar chart
    data:
      lv_selected       type i,
      lv_successful     type i,
      lv_num_of_periods type i,
      lv_rejected       type i,
      lv_msg_count      type i,
      ls_kpi_chart_item type ty_s_kpi_chart_item,
      ls_par            type if_pyd_fnd_types=>ty_s_resp,
      lo_cx_fnd         type ref to cx_pyd_fnd,
      lo_cx_frw         type ref to cx_pyc_frw.

    data: lt_sel_jobnam type  ZBTCJOB_RANGE_TABLE,
          lt_sel_jobcnt type  tcjrange.
    data: lt_list type  list_string_table,
          ls_list type string.
    data: lv_count type numc10.
    field-symbols: <hex_container>  type any.
    data: lv_length type i.

* Build the Chart
    clear et_kpi_chart_item.
    try.
        call method me->get_payroll_jobs_detail
          exporting
            it_par    = io_res_context->mt_par
          importing
            et_jobnam = lt_sel_jobnam
            et_jobcnt = lt_sel_jobcnt.

        call method me->get_spool_for_all_jobs
          exporting
            it_jobnam = lt_sel_jobnam
            it_jobcnt = lt_sel_jobcnt
          importing
            et_list   = lt_list.


        check lt_list is not initial.
* Read Payroll Process Statastics
        loop at lt_list into ls_list .
          check ls_list  is  not initial . " No need to read blank lines
          lv_length = strlen( ls_list ).
          check lv_length > 51.

          if ls_list cs text-004.
            move ls_list+51 to lv_count.
            add lv_count to lv_selected.
          endif.
          if ls_list cs text-005.
            move ls_list+51 to lv_count.
            add lv_count to lv_successful.
          endif.
          if ls_list cs text-006.
            move ls_list+51 to lv_count.
            add lv_count to lv_num_of_periods.
          endif.
          if ls_list cs text-007.
            move ls_list+51 to lv_count.
            add lv_count to lv_rejected.
          endif.
          if ls_list cs text-008.
            move ls_list+51 to lv_count.
            add lv_count to lv_msg_count.
          endif.

          clear ls_list.
        endloop.

        ls_kpi_chart_item-p_type = cl_pyc_aux=>gcs_p_type-int4.
        ls_kpi_chart_item-color  = cl_pyc_rt_facade=>gcs_kpi_color-neutral.
      catch cx_pyd_fnd into lo_cx_fnd.
        raise exception lo_cx_fnd.
      catch cx_pyc_frw into lo_cx_frw.
        raise exception lo_cx_frw.
    endtry.

    ls_kpi_chart_item-index = '1'.
    ls_kpi_chart_item-int4 = lv_selected.
    ls_kpi_chart_item-color  = cl_pyc_rt_facade=>gcs_kpi_color-neutral.
    insert ls_kpi_chart_item into table et_kpi_chart_item.

    ls_kpi_chart_item-index = '2'.
    ls_kpi_chart_item-int4 = lv_successful.
    ls_kpi_chart_item-color  = cl_pyc_rt_facade=>gcs_kpi_color-good.
    insert ls_kpi_chart_item into table et_kpi_chart_item.

    ls_kpi_chart_item-index = '3'.
    ls_kpi_chart_item-int4 = lv_rejected.
    ls_kpi_chart_item-color  = cl_pyc_rt_facade=>gcs_kpi_color-error.
    insert ls_kpi_chart_item into table et_kpi_chart_item.

    ls_kpi_chart_item-index = '4'.
    ls_kpi_chart_item-int4 = lv_num_of_periods.
    ls_kpi_chart_item-color  = cl_pyc_rt_facade=>gcs_kpi_color-neutral.
    insert ls_kpi_chart_item into table et_kpi_chart_item.

    ls_kpi_chart_item-index = '5'.
    ls_kpi_chart_item-int4 = lv_msg_count.
    ls_kpi_chart_item-color  = cl_pyc_rt_facade=>gcs_kpi_color-neutral.
    insert ls_kpi_chart_item into table et_kpi_chart_item.

  endmethod.


  method CHART_ITEM_TEXT_GET.
* Payroll Process Statastics Text
    case iv_chart_index.
      when '1'.
        rv_text = text-004.
      when '2'.
        rv_text = text-005.
      when '3'.
        rv_text = text-007.
      when '4'.
        rv_text = text-006.
      when '4'.
        rv_text = text-008.
      when others.
    endcase.

  endmethod.


  method FOOTER_GET.

    ev_text = text-001.

  endmethod.


  method GET_PAYROLL_JOBS_DETAIL.
* populate spool merge selection parameters
    constants: mc_abkrs       type pyd_par_type value 'ABKRS',
               mc_period      type pyd_par_type value 'PERIOD',
               mc_pty_program type pyd_d_instp-par_type value 'PROGRAM',
               mc_py_program  type pyd_d_instp-low value 'RPCALCQ0'.

    data: ls_par        type pyd_s_resp,
          ls_sel_params type line of rsparams_tt,
          lv_sel_name   type char8.

    data:
      lv_proc_inst_id           type pyc_proc_inst_id,
      ls_proc_inst_id_so        type if_pyd_fnd_types=>ty_s_so,
      lo_proc_inst_aux          type ref to  cl_pyc_proc_inst_aux,
      lt_proc_inst_id_so        type /iwbep/t_cod_select_options,
      lx_exc_frw                type ref to  cx_pyc_frw,
      lt_step_inst              type cl_pyc_proc_inst_aux=>ty_t_step_inst,
      ls_step_inst              type cl_pyc_proc_inst_aux=>ty_s_step_inst,
      lv_step_id                type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id,
      lv_payroll_step_id        type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id,
      lv_payroll_step_sortfield type cl_pyc_proc_inst_aux=>ty_s_step_inst-si_sort_field.

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

    data: lv_id type pyd_d_instp-id.
    data: ls_sel_jobnam type ZBTCJOB_RANGE,
          ls_sel_jobcnt type cjrange.

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

* Determine Payroll Step ID
    loop at lt_step_inst into ls_step_inst.
* Read Process Step for Payroll Driver
      select single id into lv_id
        from pyd_d_instp
       where id        = ls_step_inst-si_dsr_instid
         and par_type  = mc_pty_program
         and low       = mc_py_program.

      if sy-subrc eq 0.
        move ls_step_inst-step_id to lv_payroll_step_id.
      endif.
    endloop.

    if not lv_payroll_step_id is initial.
* Read Batch Processing Component ID
      clear: lv_pyc_bpc_id.
      select single id into lv_pyc_bpc_id from  pyc_d_bpc
       where  pypi_id  = lv_proc_inst_id
         and  step_id  = lv_payroll_step_id.
      if sy-subrc eq 0.
* Read Job names and Job number details for the previous step
        select bpc_id row_id job_name job_count
          into corresponding fields of table lt_pyc_d_bpc_job
          from pyc_d_bpc_job
          where bpc_id = lv_pyc_bpc_id.
      endif.
    endif.
    sort lt_pyc_d_bpc_job by row_id.

* Populate Spool Merge Program Selection
    clear: et_jobnam, et_jobcnt.
    loop at lt_pyc_d_bpc_job into ls_pyc_d_bpc_job.
* Selection Option - Job Name
      clear ls_sel_jobnam.
      ls_sel_jobnam-option = 'EQ' .
      ls_sel_jobnam-sign = 'I' .
      ls_sel_jobnam-low = ls_pyc_d_bpc_job-job_name.
      append ls_sel_jobnam to et_jobnam .

* Selection Option - Job Number
      clear ls_sel_jobcnt.
      ls_sel_jobcnt-opt = 'EQ' .
      ls_sel_jobcnt-sign = 'I' .
      ls_sel_jobcnt-low = ls_pyc_d_bpc_job-job_count.
      append ls_sel_jobcnt to et_jobcnt .
    endloop.

  endmethod.


  method GET_SPOOL_FOR_ALL_JOBS.

    data: lt_alist type standard table of abaplist,
          lt_tlist type list_string_table.
* Submit Spool Merge Program to Collect All the Spool Outputs.
    submit zhraurepy_pcc_spool_merge
      with s_jobnam in it_jobnam
      with s_jobcnt in it_jobcnt
      with p_wfmt eq abap_true
      with p_rep eq abap_true
      with p_last eq abap_true
      with p_pages eq 2
    exporting list to memory and return.

* Extract List Object
    call function 'LIST_FROM_MEMORY'
      tables
        listobject = lt_alist
      exceptions
        not_found  = 1
        others     = 2.

* Free List Memory
    if sy-subrc <> 0.
* Implement suitable error handling here
    else.
      call function 'LIST_FREE_MEMORY'.
* Convert list to Text
      call function 'LIST_TO_ASCI'
        importing
          list_string_ascii = lt_tlist
        tables
          listobject        = lt_alist
        exceptions
          others            = 0.

      et_list[] = lt_tlist[].
    endif.

  endmethod.


  method HEADER_GET.
* Header Text
    ev_text = text-000.

  endmethod.
ENDCLASS.
