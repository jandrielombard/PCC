class ZUSECL_M99_PCC_EHIUPDATE definition
  public
  final
  create public .

public section.

  types:
* Process Jobs List
    begin of ty_proc_jobslist,
        repro     type char15,
        proc_id   type pyc_proc_id,
        proc_name type pyc_proc_name,
        pypi_id   type pyc_proc_inst_id,
        step_id   type pyc_proc_step_id,
        step_name type pyd_name,
        jobname   type tbtcjob-jobname,
        jobcount  type tbtcjob-jobcount,
        strtdate  type tbtcjob-strtdate,
        strttime  type tbtcjob-strttime,
        status    type tbtcjob-status,
        marker    type xfeld.
    types: end of ty_proc_jobslist .
  types:
    tty_proc_jobslist type standard table of ty_proc_jobslist
                                      with key proc_id pypi_id step_id jobname jobcount .
  types:
* Job Steps
    begin of ty_job_step,
        jobname   type btcjob,
        jobcount  type btcjobcnt,
        stepcount type btcstepcnt,
        progname  type btcprog,
        variant   type btcvariant.
    types: end of ty_job_step .
  types:
    tty_job_step type standard table of ty_job_step with key jobname jobcount stepcount .
  types:
    begin of ty_pypi_id,
        pypi_id  type pyc_proc_inst_id,
        pyp_name type pyc_proc_name,
      end of ty_pypi_id .
  types:
    tty_pypi_id type standard table of ty_pypi_id with key pypi_id .
  types:
* Employees list
    begin of ty_emplist,
        repro     type char15,
        jobname   type tbtcjob-jobname,
        jobcount  type tbtcjob-jobcount,
        strtdate  type tbtcjob-strtdate,
        strttime  type tbtcjob-strttime,
        pypi_id   type pyc_proc_inst_id,
        pernr     type pernr-pernr,
        message   type bapireturn1-message,
        reprodate type tbtcjob-strtdate,
        reprotime type tbtcjob-strttime,
        runtime   type timestamp.
    types: end of ty_emplist .
  types:
    tty_emplist type standard table of ty_emplist with key jobname jobcount pypi_id pernr .
  types:
    begin of ty_employee_status,
        pernr type p_pernr,
        stat2 type stat2,
      end of ty_employee_status .
  types:
    tty_employee_status type table of ty_employee_status .

  constants:
    begin of gcs_action ,
        start_step      type pyd_rd_type value cl_pyc_pt_sts_err=>gcs_operation-main_act,
        start_process   type pyd_rd_type value 'PYP_STS_EXE_OPEN',
        repeat          type pyd_rd_type value 'PYP_STS_EXE_RESET',
        confirm         type pyd_rd_type value 'PYP_STS_EXE_CLOSE',
        edit_assignment type pyd_rd_type value cl_pyc_pt_sts_err=>gcs_data-status_details,
        status_details  type pyd_rd_type value cl_pyc_pt_sts_err=>gcs_data-status_details,
      end of gcs_action .
  constants GC_EHIUPD type SY-UCOMM value 'EHIUPD' ##NO_TEXT.
  constants GC_PAR_TYPE_PERNR type PYD_PAR_TYPE value 'PERNR' ##NO_TEXT.
  constants:
    begin of gc_range_val,
        include type tvarv_sign value 'I',
        between type tvarv_opti value 'BT',
      end of gc_range_val .
  constants GC_UTC type TZNZONE value 'UTC' ##NO_TEXT.
  constants:
    begin of gc_repro_status,
        cancelled   type char15 value 'Cancelled',
        reprocessed type char15 value 'Reprocessed',
        withdrawn   type char15 value 'Withdrawn',
      end of gc_repro_status .

  methods CONSTRUCTOR
    importing
      !IV_PROC_ID type PYC_D_PYPI-PROC_ID
      !IV_PCCVAL type BOOLEAN
      !IT_PERNR_SO type /IWBEP/T_COD_SELECT_OPTIONS .
  methods GET_PROC_JOBSLIST
    exporting
      !ET_PROC_JOBSLIST type TTY_PROC_JOBSLIST
      !ET_EMPLIST type ZUSECL_M99_PCC_EHIUPDATE=>TTY_EMPLIST
      !EV_MSG type BAPI_MSG .
  class-methods DISPLAY_JOB_STEPS
    importing
      !IT_JOBSLIST type TTY_PROC_JOBSLIST .
  class-methods ADD_EMPLOYEES_TO_EHITBL
    changing
      !CT_EMPLIST type ZUSECL_M99_PCC_EHIUPDATE=>TTY_EMPLIST .
protected section.

* Selection Ranges
  data GT_AEDTM_SO type /IWBEP/T_COD_SELECT_OPTIONS .
  data GT_CHANGE_PERIOD type PPFSRTSTMP .
  data GV_TIMESTAMP_LOW type TIMESTAMP .
  data GV_TIMESTAMP_HIGH type TIMESTAMP .
  data GV_PROC_ID type PYC_PROC_ID .
  data GV_PROC_NAME type PYC_PROC_NAME .
  data GV_PCCVAL type BOOLEAN .
  data GT_PROC_INST type CL_PYC_RT_FACADE=>TY_T_PROC_INST .
  data GT_PYPI_ID type TTY_PYPI_ID .
  data GS_PYPI_ID type TY_PYPI_ID .
  data GT_PROC_JOBSLIST type TTY_PROC_JOBSLIST .
  data GS_PROC_JOBSLIST type TY_PROC_JOBSLIST .
  data:
    gt_job_step type table of ty_job_step .
  data GS_JOB_STEP type TY_JOB_STEP .
  data GT_EMPLIST type TTY_EMPLIST .
  data GS_EMPLIST type TY_EMPLIST .
  data GT_PERNR_SO type /IWBEP/T_COD_SELECT_OPTIONS .
  data GT_ALL_PAYROLL_JOBS type TTY_PROC_JOBSLIST .
  private section.

    methods get_instance_parameters
      importing
        !iv_pypi_id  type pyc_proc_inst_id
      exporting
        !ev_abkrs    type abkrs
        !ev_inper    type iperi
        !ev_tpy_res  type hrdct_is_tpy
        !ev_proc_cat type pyc_proc_category
        !ev_begda    type begda
        !ev_endda    type endda .
    methods read_batchjobs_employees
      importing
        !it_jobslist      type tty_proc_jobslist
      returning
        value(rt_emplist) type tty_emplist .
    methods filter_by_payroll_runtime
      changing
        !ct_emplist type ZUSECL_M99_pcc_ehiupdate=>tty_emplist .
    methods get_jobs_for_process_instance
      importing
        !is_pypi_id type ty_pypi_id
      raising
        cx_pyc_cont .
    methods get_active_process_instances
      returning
        value(rv_msg) type bapi_msg .
    methods filter_by_employee_status
      changing
        !ct_emplist type ZUSECL_M99_pcc_ehiupdate=>tty_emplist .
    methods filter_by_next_payrun
      changing
        !ct_emplist type ZUSECL_M99_pcc_ehiupdate=>tty_emplist .
    methods get_next_pay_process_time
      returning
        value(rt_pay_process_time) type tty_emplist .
ENDCLASS.



CLASS ZUSECL_M99_PCC_EHIUPDATE IMPLEMENTATION.


  method ADD_EMPLOYEES_TO_EHITBL.
* Add Employees to Event Handler Table
    data: ls_emplist type ZUSECL_M99_pcc_ehiupdate=>ty_emplist.
    data: lo_event_handler type ref to if_pyc_event_handler,
          lv_par_type      type pyd_par_type,
          lv_roid          type pyd_roid.
*
    data: lt_proc_inst type if_pyc_event_handler=>ty_t_proc_inst,
          lv_proc_inst type pyc_proc_inst_id.
    data: lv_index type sy-tabix.

* Add Employees to Event Handler
    if not ct_emplist is initial.
      try .
          lo_event_handler = cl_pyc_event_handler_factory=>get_event_handler_instance( ).
        catch cx_pyc_eh.
          return.
      endtry.

      loop at ct_emplist into ls_emplist.
        lv_index = sy-tabix.

        if ls_emplist-pypi_id is initial.
          ls_emplist-message = text-010.
        else.
          refresh: lt_proc_inst.
          append ls_emplist-pypi_id to lt_proc_inst.

          lv_roid     = ls_emplist-pernr.
          lv_par_type = gc_par_type_pernr.

          try .
              call method lo_event_handler->event_handler_item_create_list
                exporting
                  iv_par_type  = lv_par_type
                  iv_id        = lv_roid
                  it_proc_inst = lt_proc_inst.
              if sy-subrc eq 0.
                message i061(zhrpy_pcc_msg) into ls_emplist-message.
              else.
                message i062(zhrpy_pcc_msg) into ls_emplist-message.
              endif.
            catch cx_pyc_eh.
          endtry.
        endif.

        modify ct_emplist from ls_emplist index lv_index transporting message.
      endloop.
    endif.

  endmethod.


  method CONSTRUCTOR.
* Initialize Selection.
    gv_proc_id = iv_proc_id.
    gv_pccval = iv_pccval.
    gt_pernr_so = it_pernr_so.

  endmethod.


  method DISPLAY_JOB_STEPS.
* Display job step
    include lbtchdef.
    data: rc type i.
    data: begin of ls_stpl_jobhead.
            include type tbtcjob.
          data: end of ls_stpl_jobhead.

    types: begin of ty_stpl_steplist.
             include type tbtcstep.
           types: end of ty_stpl_steplist.
    data: lt_stpl_steplist type table of ty_stpl_steplist.

    data: lt_jobslist type tty_proc_jobslist.
    data: ls_proc_jobslist type ty_proc_jobslist.
    data: lv_lines type i.

* Check selection.
    lt_jobslist[] = it_jobslist[].

    delete lt_jobslist where marker ne abap_true.
    describe table lt_jobslist lines lv_lines.
    if lv_lines > 1.
      message i011(bt).
      exit.
    endif.

    loop at lt_jobslist into ls_proc_jobslist.

      call function 'BP_JOB_READ'
        exporting
          job_read_jobname  = ls_proc_jobslist-jobname
          job_read_jobcount = ls_proc_jobslist-jobcount
          job_read_opcode   = btc_read_all_jobdata
        importing
          job_read_jobhead  = ls_stpl_jobhead
        tables
          job_read_steplist = lt_stpl_steplist
        exceptions
          job_doesnt_exist  = 1
          others            = 99.

      case sy-subrc.
        when 0.
        when 1.
          message e127(bt) with ls_proc_jobslist-jobname.
        when others.
          message e155(bt) with ls_proc_jobslist-jobname.
      endcase.

* Display Step list editor
      call function 'BP_STEPLIST_EDITOR'
        exporting
          steplist_dialog = btc_yes
          steplist_opcode = btc_show_steplist
        tables
          steplist        = lt_stpl_steplist
        exceptions
          others          = 99.

      exit.
    endloop.

  endmethod.


  method FILTER_BY_EMPLOYEE_STATUS.
* Filter out in active employees
    data lt_emplist type ZUSECL_M99_pcc_ehiupdate=>tty_emplist.
    data ls_emplist type ZUSECL_M99_pcc_ehiupdate=>ty_emplist.

    data lt_pypi_emplist type ZUSECL_M99_pcc_ehiupdate=>tty_emplist.
    data lt_pernr_so type /iwbep/t_cod_select_options.
    data lv_lines type i.
    data lv_prev_pernr type pernr_d.
    data lv_index type index.

    data lv_period_begda  type begda.
    data lv_period_endda  type endda.
    data lv_pypi_id type pyc_proc_inst_id.

    data lt_employee_status type ZUSECL_M99_pcc_ehiupdate=>tty_employee_status.
    data lv_employee_status type stat2.

* Selection Option
    lt_emplist = ct_emplist.
    sort lt_emplist by pypi_id pernr.
    loop at lt_emplist into data(lt_pypi_group)
             group by lt_pypi_group-pypi_id.

      lt_pypi_emplist = value #( for ls_pypi_group in group lt_pypi_group ( ls_pypi_group ) ).

      " Prepare Employee selection range
      sort lt_pypi_emplist by pernr.
      delete adjacent duplicates from lt_pypi_emplist comparing pernr.
      read table lt_pypi_emplist into ls_emplist index 1.
      if sy-subrc eq 0.
        lv_pypi_id = ls_emplist-pypi_id.
      endif.

* Read Instance Parameters
      call method me->get_instance_parameters
        exporting
          iv_pypi_id = lv_pypi_id
        importing
          ev_begda   = lv_period_begda
          ev_endda   = lv_period_endda.

* Read Employee Status
      if not lt_pypi_emplist is initial.
        select pernr stat2
          into corresponding fields of table lt_employee_status
          from pa0000
           for all entries in lt_pypi_emplist
         where pernr eq lt_pypi_emplist-pernr
           and begda le lv_period_endda
           and endda ge lv_period_begda.
      endif.
      delete lt_employee_status where stat2 ne '3'.

* Filter Master Data changes
      sort lt_employee_status by pernr.
      clear: lv_employee_status.
      loop at ct_emplist into ls_emplist where pypi_id = lv_pypi_id.
        lv_index = sy-tabix.

        if ls_emplist-pernr <> lv_prev_pernr.
          clear: lv_employee_status.
          read table lt_employee_status into data(ls_employee_status)
            with key pernr = ls_emplist-pernr binary search.
          if sy-subrc eq 0.
            lv_employee_status = ls_employee_status-stat2.
          endif.

          lv_prev_pernr = ls_emplist-pernr.
        endif.

        if lv_employee_status is initial.
          ls_emplist-repro = gc_repro_status-withdrawn.
        endif.

        modify ct_emplist from ls_emplist index lv_index transporting repro .
      endloop.

    endloop.            "PYPI_ID Group

    "Delete All Withdrawn Employees
    delete ct_emplist where repro = gc_repro_status-withdrawn.

  endmethod.


  method FILTER_BY_NEXT_PAYRUN.
* Update Reprocess status using Payroll Reprocess Time
    data ls_emplist type ZUSECL_M99_pcc_ehiupdate=>ty_emplist.
    data lv_prev_pernr type pernr_d.
    data lv_prev_pypi_id type pyc_proc_inst_id.
    data lv_index type index.

    data lv_pay_process_time type timestamp.
    data lt_pay_process_time type ZUSECL_M99_pcc_ehiupdate=>tty_emplist.
    data ls_pay_process_time type ZUSECL_M99_pcc_ehiupdate=>ty_emplist.

* Read Employees processed after Cancelled jobs
    lt_pay_process_time = me->get_next_pay_process_time( ).
    sort lt_pay_process_time by pypi_id pernr strtdate descending strttime descending.
    delete adjacent duplicates from lt_pay_process_time comparing pypi_id pernr.

* Filter Master Data changes
    loop at ct_emplist into ls_emplist.
      lv_index = sy-tabix.

      if not ( ls_emplist-pernr eq lv_prev_pernr and
               ls_emplist-pypi_id eq lv_prev_pypi_id ).
        clear ls_pay_process_time.
        read table lt_pay_process_time into ls_pay_process_time
          with key pypi_id = ls_emplist-pypi_id
                   pernr   = ls_emplist-pernr binary search.

        lv_prev_pypi_id = ls_emplist-pypi_id.
        lv_prev_pernr = ls_emplist-pernr.
      endif.

      ls_emplist-reprodate = ls_pay_process_time-strtdate.
      ls_emplist-reprotime = ls_pay_process_time-strttime.
      lv_pay_process_time = |{ ls_pay_process_time-strtdate }{ ls_pay_process_time-strttime }|.

      if lv_pay_process_time gt ls_emplist-runtime.
        ls_emplist-repro = gc_repro_status-reprocessed.
      else.
        ls_emplist-repro = gc_repro_status-cancelled.
      endif.

      modify ct_emplist from ls_emplist index lv_index transporting repro reprodate reprotime.
    endloop.

  endmethod.


  method FILTER_BY_PAYROLL_RUNTIME.
* apply payroll run time condition
    data lt_emplist type ZUSECL_M99_pcc_ehiupdate=>tty_emplist.
    data ls_emplist type ZUSECL_M99_pcc_ehiupdate=>ty_emplist.
    data lt_pypi_emplist type ZUSECL_M99_pcc_ehiupdate=>tty_emplist.
    data lt_pernr_so type /iwbep/t_cod_select_options.
    data lv_lines type i.
    data lv_prev_pernr type pernr_d.
    data lv_index type index.

    data lv_abkrs	type abkrs.
    data lv_inper	type iperi.
    data lv_tpy_res type xfeld.
    data lv_pypi_id type pyc_proc_inst_id.

    data lv_pay_runtime type timestamp.
    data lt_pay_runtime type ZUSECL_M99_pcc_utilities=>tty_payrun_time.
    data ls_pay_runtime type ZUSECL_M99_pcc_utilities=>ty_payrun_time.

* Selection Option
    lt_emplist = ct_emplist.
    sort lt_emplist by pypi_id pernr.
    loop at lt_emplist into data(lt_pypi_group)
             group by lt_pypi_group-pypi_id.

      lt_pypi_emplist = value #( for ls_pypi_group in group lt_pypi_group ( ls_pypi_group ) ).

      " Prepare Employee selection range
      sort lt_pypi_emplist by pernr.
      delete adjacent duplicates from lt_pypi_emplist comparing pernr.
      describe table lt_pypi_emplist lines lv_lines.

      if lv_lines > 10000.
        "Read for entire payroll area
        refresh: lt_pernr_so.
      else.
        " Read for selected personal numbers
        refresh: lt_pernr_so.
        loop at lt_pypi_emplist into data(ls_pypi_emplist).
          lv_pypi_id = ls_pypi_emplist-pypi_id.
          try.
              call method cl_pyd_fnd_aux=>append_so_fixed_value
                exporting
                  iv_value = ls_pypi_emplist-pernr
                changing
                  ct_so    = lt_pernr_so.
            catch cx_pyd_fnd.
          endtry.
        endloop.
      endif.

* Instance Parameters
      read table lt_pypi_emplist into ls_emplist index 1.
      if sy-subrc eq 0.
        lv_pypi_id = ls_emplist-pypi_id.
      endif.

      call method me->get_instance_parameters
        exporting
          iv_pypi_id = lv_pypi_id
        importing
          ev_abkrs   = lv_abkrs
          ev_inper   = lv_inper
          ev_tpy_res = lv_tpy_res.

* Payroll Run Time
      call method ZUSECL_M99_pcc_utilities=>get_employees_payroll_runtime
        exporting
          iv_abkrs    = lv_abkrs
          iv_inper    = lv_inper
          it_pernr_so = lt_pernr_so
          iv_tpy      = lv_tpy_res
        importing
          et_runtime  = lt_pay_runtime.

* Filter Master Data changes
      sort lt_pay_runtime by pernr.
      loop at ct_emplist into ls_emplist where pypi_id = lv_pypi_id.
        lv_index = sy-tabix.

        if ls_emplist-pernr <> lv_prev_pernr.
          clear ls_pay_runtime.
          read table lt_pay_runtime into ls_pay_runtime
            with key pernr = ls_emplist-pernr binary search.

          lv_prev_pernr = ls_emplist-pernr.
        endif.

        ls_emplist-reprodate = ls_pay_runtime-rundt.
        ls_emplist-reprotime = ls_pay_runtime-runtm.
        lv_pay_runtime = |{ ls_pay_runtime-rundt }{ ls_pay_runtime-runtm }|.

        if lv_pay_runtime gt ls_emplist-runtime.
          ls_emplist-repro = gc_repro_status-reprocessed.
        else.
          ls_emplist-repro = gc_repro_status-cancelled.
        endif.

        modify ct_emplist from ls_emplist index lv_index transporting repro reprodate reprotime.
      endloop.

    endloop.            "PYPI_ID Group

  endmethod.


  method GET_ACTIVE_PROCESS_INSTANCES.
* Read Process Instances

    data: lo_transaction   type ref to if_pyd_transaction,
          lo_pyc_rt_facade type ref to cl_pyc_rt_facade,
          lx_exc           type ref to cx_root,
          lv_string        type string.

    data: lt_so        type /iwbep/t_cod_select_options,
          ls_so        type /iwbep/s_cod_select_option,
          lt_filter    type /iwbep/t_mgw_select_option,
          ls_filter    type /iwbep/s_mgw_select_option,
          lt_proc_inst type cl_pyc_rt_facade=>ty_t_proc_inst.
    data: lv_step_id type pyc_d_msi-step_id.

    data ls_pypi_dtls type ty_pypi_id.

    clear rv_msg.
* Read Process Instances for Process
    try .
        "Read PCC Configuration
        lo_transaction   = cl_pyd_transaction_factory=>create_new_transaction( ).
        lo_pyc_rt_facade = cl_pyc_rt_facade=>get_instance( io_transaction = lo_transaction ).

        "Process ID
        clear lt_so.
        ls_filter-property = cl_pyc_rt_facade=>gcs_filter-proc_inst-proc_id.
        lt_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = gv_proc_id ).
        append lines of lt_so to ls_filter-select_options.
        append ls_filter to lt_filter. clear ls_filter.

        "Execution Status - Only Active Process Instances
        clear lt_so.
        ls_filter-property = cl_pyc_rt_facade=>gcs_filter-proc_inst-exe_sts.
        lt_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = cl_pyc_pt_sts_exe=>gcs_value-in_execution ).
        append lines of lt_so to ls_filter-select_options.
        append ls_filter to lt_filter. clear ls_filter.

        lt_proc_inst[] = lo_pyc_rt_facade->proc_inst_get_list( exporting it_filter = lt_filter[] ).
      catch cx_root into lx_exc.
        lv_string = lx_exc->get_text( ).
        message e016(pg) with lv_string '' '' ''.
        exit.
    endtry.

* All Active Process Instances for the selected Process
    gt_proc_inst = lt_proc_inst.
    loop at lt_proc_inst into data(ls_proc_inst).
      gv_proc_name = ls_proc_inst-pyp_name.

      if gv_pccval eq abap_false.
        " Consider process instances with Event Handler
        select single step_id into lv_step_id
          from  pyc_d_msi
         where  pypi_id  = ls_proc_inst-pypi_id.
        check sy-subrc eq 0.
      endif.


      clear ls_pypi_dtls.
      ls_pypi_dtls-pypi_id   = ls_proc_inst-pypi_id.
      ls_pypi_dtls-pyp_name  = ls_proc_inst-pyp_name.
      append ls_pypi_dtls to gt_pypi_id.
    endloop.

    if gt_pypi_id[] is initial.
      message i016(pg) with text-001 '' '' '' into rv_msg.
    endif.

  endmethod.


  method GET_INSTANCE_PARAMETERS.
* Read Specific Instance Parameters
    data: ls_proc_inst type cl_pyc_rt_facade=>ty_s_proc_inst.

* Populate details from Process Instance data
    read table gt_proc_inst into ls_proc_inst
      with key pypi_id = iv_pypi_id.

    if sy-subrc eq 0.
      ev_inper = ls_proc_inst-time_sel_par_val.
      ev_proc_cat = ls_proc_inst-pypte_category.
      clear: ev_tpy_res.
      case ls_proc_inst-pypte_category.
        when cl_pyc_cfg=>gcs_proc_template_cat-team_monitoring.
          ev_tpy_res = abap_true.
        when others.
          ev_tpy_res = abap_false.
      endcase.

    endif.

* Payroll Area
    select single b~value into ev_abkrs
      from pyc_d_pypi as a
     inner join pyc_d_pypisp as b on a~proc_id = b~id
     where a~pypi_id = iv_pypi_id.

* Payroll Period
    data:
      lv_pabrj        type pabrj,
      lv_pabrp        type pabrp,
      lv_vabrj        type vabrj,
      lv_vabrp        type vabrp,
      lv_begda        type begda,
      lv_endda        type endda,
      lo_payroll_area type ref to cl_hr_payroll_area.

    lo_payroll_area = cl_hr_payroll_area=>get_instance( imp_area = ev_abkrs ).
    lv_pabrj = ev_inper(4).
    lv_pabrp = ev_inper+4(2).

    lo_payroll_area->get_period_info(
      exporting
        imp_pabrj = lv_pabrj
        imp_pabrp = lv_pabrp
      importing
        exp_begda = ev_begda
        exp_endda = ev_endda ).

  endmethod.


  method GET_JOBS_FOR_PROCESS_INSTANCE.
* Use Process Instance to read Process steps
    constants: mc_pty_program     type pyd_d_instp-par_type value 'PROGRAM',
               mc_payroll_program type pyd_d_instp-low value 'RPCALCQ0',
               mc_posting_program type pyd_d_instp-low value 'RPCIPE01',
               mc_cat_policy      type pyc_step_templ_cat value 'POLICY',
               mc_cat_moni        type pyc_step_templ_cat value 'MONI',
               mc_cat_other       type pyc_step_templ_cat value 'OTHER'.
    constants: mc_status_cancelled type btcstatus value 'C',
               mc_status_aborted   type btcstatus value 'A'.

    data lt_program_so  type /iwbep/t_cod_select_options.
    data lt_step_id_so  type /iwbep/t_cod_select_options.
    data lt_trigger_rdt_so type /iwbep/t_cod_select_options.

* Process Step Read
    data: lo_fnd_factory   type ref to cl_pyd_fnd_factory,
          lo_proc_inst_aux type ref to  cl_pyc_proc_inst_aux.

    data: lv_id type pyd_d_instp-id.
    data: ls_proc_inst_id_so  type if_pyd_fnd_types=>ty_s_so,
          lt_proc_inst_id_so  type /iwbep/t_cod_select_options,
          lx_exc_frw          type ref to  cx_pyc_frw,
          lx_fnd_exc          type ref to cx_pyd_fnd,
          lt_step_inst        type cl_pyc_proc_inst_aux=>ty_t_step_inst,
          ls_step_inst        type cl_pyc_proc_inst_aux=>ty_s_step_inst,
          lt_step_inst_enrich type cl_pyc_rt_facade=>ty_t_step_inst,
          ls_step_inst_enrich type cl_pyc_rt_facade=>ty_s_step_inst,
          lv_step_id          type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id,
          lv_payroll_step_id  type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id.

*	Identify and read Payroll Driver process step start time
    data: ls_pyd_d_instp type pyd_d_instp.
    data: begin of ls_pyd_d_al,
            instid       type pyd_instid,
            par_hash     type pyd_parhash,
            tsl	         type timestampl,
            trigger_rdt	 type pyd_rd_type,
            reserved_id2 type char40,
          end of ls_pyd_d_al.
    data: lt_pyd_d_al like table of ls_pyd_d_al.
    data: ls_pyd_d_al_rel like ls_pyd_d_al.
    data: lv_pyd_d_al_tsl type pyd_d_al-tsl,
          lv_tstamps      type  timestamp.

* EHIUPD Parameters
    data: lv_start_date type begda,
          lv_start_time type beguz,
          lv_end_date   type endda,
          lv_end_time   type enduz.

* Job Details
    data: lv_pyc_bpc_id type pyc_bpc_id.
    types: begin of ty_pyc_d_bpc_job ,
             bpc_id	   type pyc_bpc_id,
             pypi_id   type pyc_proc_inst_id,
             step_id   type pyc_proc_step_id,
             row_id    type pyd_rowid_long,
             job_name  type btcjob,
             job_count type btcjobcnt,
             cr_dt     type timestampl.
    types: end of ty_pyc_d_bpc_job.
    data: lt_pyc_d_bpc_job type table of ty_pyc_d_bpc_job,
          ls_pyc_d_bpc_job type ty_pyc_d_bpc_job.

* Job Header
    data: ls_job_header type tbtcjob.
    data: lo_exec_prog type ref to if_pyc_process_prog.

* Collect Process Instances
    try.
        lt_proc_inst_id_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = is_pypi_id-pypi_id ).
      catch cx_pyd_fnd.
    endtry.

    try.
        lo_fnd_factory = cl_pyd_fnd_factory=>get_instance( ).
        lo_proc_inst_aux = cl_pyc_proc_inst_aux=>get_instance( lo_fnd_factory->mo_transaction ).
        lt_step_inst = lo_proc_inst_aux->step_inst_get_list(
                          it_proc_inst_id_so = lt_proc_inst_id_so
                          iv_with_names      = abap_false ).

* Read Step Template IDs for the Process
*	Enrich Process step details, which will populate the process step start time
        lt_step_inst_enrich = lo_proc_inst_aux->step_inst_enrich(
                                  it_step_inst      = lt_step_inst
                                  iv_with_names     = abap_true
                                  iv_with_pi_info   = abap_true
                                  iv_with_p_info    = abap_true
                                  iv_with_gi_info   = abap_true
                                  iv_with_stgt_info = abap_true
                                  iv_with_s_info    = abap_true
                                  iv_with_st_info   = abap_true ).

      catch cx_pyc_frw into lx_exc_frw.
        " do nothing. Just keep empty table.
        return.
      catch cx_pyd_fnd into lx_fnd_exc.
        " do nothing. Just keep empty table.
        return.
    endtry.

* Read relevant Step ID's
    clear: lt_program_so, lt_step_id_so.
    try.
        call method cl_pyd_fnd_aux=>append_so_fixed_value
          exporting
            iv_value = mc_payroll_program
          changing
            ct_so    = lt_program_so.

        call method cl_pyd_fnd_aux=>append_so_fixed_value
          exporting
            iv_value = mc_posting_program
          changing
            ct_so    = lt_program_so.

        loop at lt_step_inst_enrich into ls_step_inst_enrich.
          case ls_step_inst_enrich-st_cat.
            when mc_cat_policy.
* Read Process Step for Initiate Policies
* Initiate Policies step Excluded as it may force to include entire payroll area
* Employees to the Event handler Table
*              call method cl_pyd_fnd_aux=>append_so_fixed_value
*                exporting
*                  iv_value = ls_step_inst_enrich-step_id
*                changing
*                  ct_so    = lt_step_id_so.

            when mc_cat_other.
* Read Process Step for Payroll Driver and Posting Simulation
              select single id, low
                into corresponding fields of @ls_pyd_d_instp
                from pyd_d_instp
               where id        = @ls_step_inst_enrich-si_dsr_instid
                 and par_type  = @mc_pty_program
                 and low       in @lt_program_so.
              if sy-subrc eq 0.
                call method cl_pyd_fnd_aux=>append_so_fixed_value
                  exporting
                    iv_value = ls_step_inst_enrich-step_id
                  changing
                    ct_so    = lt_step_id_so.

                " Set Payroll Driver Step ID
                if ls_pyd_d_instp-low eq mc_payroll_program.
                  lv_payroll_step_id = ls_step_inst_enrich-step_id.
                endif.

              endif.
          endcase.
        endloop.
      catch cx_pyd_fnd .
    endtry.

* Relevant Trigger RDT
    try.
        call method cl_pyd_fnd_aux=>append_so_fixed_value
          exporting
            iv_value = cl_pyc_pt_sts_err=>gcs_operation-main_act " 'PYP_STS_ERR_MAIN_ACT'
          changing
            ct_so    = lt_trigger_rdt_so.

        call method cl_pyd_fnd_aux=>append_so_fixed_value
          exporting
            iv_value = cl_pyc_pt_sts_err=>gcs_operation-addl_act " 'PYP_STS_ERR_ADDL_ACT'
          changing
            ct_so    = lt_trigger_rdt_so.

        call method cl_pyd_fnd_aux=>append_so_fixed_value
          exporting
            iv_value = gcs_action-repeat                        "'PYP_STS_EXE_RESET'
          changing
            ct_so    = lt_trigger_rdt_so.
      catch cx_pyd_fnd .
    endtry.

* Read PCC Audit Log for the Process step Execution/Reset Times
    if not lt_step_id_so is initial.
      clear: lt_pyd_d_al, lv_pyd_d_al_tsl.
      select instid par_hash tsl trigger_rdt reserved_id2
        from  pyd_d_al
        into corresponding fields of table lt_pyd_d_al
             where  instid        in lt_step_id_so
             and    trigger_rdt   in  lt_trigger_rdt_so
             and    reserved_id2  in lt_proc_inst_id_so.

      if sy-subrc eq 0.
        " Identify the start time of current processing period
        sort lt_pyd_d_al by tsl descending.
        loop at lt_pyd_d_al into ls_pyd_d_al.
          "Collect processes details and Process start time frm latest to the start
          lv_pyd_d_al_tsl = ls_pyd_d_al-tsl.
          move-corresponding ls_pyd_d_al to ls_pyd_d_al_rel.

          " Exit the loop for latest Payroll step reset
          " Incase of Initial Run Start date will set to main act of Payroll Process
          if ls_pyd_d_al-instid eq lv_payroll_step_id   " Payroll Process Step Reset
             and ls_pyd_d_al-trigger_rdt eq gcs_action-repeat.
            exit.
          endif.
        endloop.

      endif.

* Read Job names and Job number details for the Steps
      clear: lt_pyc_d_bpc_job.
      select a~id as bpc_id a~pypi_id a~step_id
             b~row_id b~job_name b~job_count b~cr_dt
        into corresponding fields of table lt_pyc_d_bpc_job
        from pyc_d_bpc as a inner join pyc_d_bpc_job as b
          on  a~id eq b~bpc_id
       where a~pypi_id  in lt_proc_inst_id_so
         and a~step_id  in lt_step_id_so
         and a~cr_dt ge lv_pyd_d_al_tsl.

      sort lt_pyc_d_bpc_job by cr_dt descending bpc_id row_id.

* Read Jobs Header Info
      create object lo_exec_prog type cl_pyc_basic_process_prog.
      loop at lt_pyc_d_bpc_job into ls_pyc_d_bpc_job.
        call method lo_exec_prog->get_job_info(
          exporting
            imp_job_name  = ls_pyc_d_bpc_job-job_name       "job name
            imp_job_count = ls_pyc_d_bpc_job-job_count      "job count
          importing
            es_job_header = ls_job_header ).

        clear: gs_proc_jobslist.
        move: gv_proc_id                 to gs_proc_jobslist-proc_id,
              gv_proc_name               to gs_proc_jobslist-proc_name,
              ls_pyc_d_bpc_job-pypi_id   to gs_proc_jobslist-pypi_id,
              ls_pyc_d_bpc_job-step_id   to gs_proc_jobslist-step_id,
              ls_pyc_d_bpc_job-job_name  to gs_proc_jobslist-jobname,
              ls_pyc_d_bpc_job-job_count to gs_proc_jobslist-jobcount,
              ls_job_header-strtdate     to gs_proc_jobslist-strtdate,
              ls_job_header-strttime     to gs_proc_jobslist-strttime,
              ls_job_header-status       to gs_proc_jobslist-status.

        read table lt_step_inst_enrich into ls_step_inst_enrich
         with key pypi_id = ls_pyc_d_bpc_job-pypi_id
                  step_id = ls_pyc_d_bpc_job-step_id.
        if sy-subrc eq 0.
          move ls_step_inst_enrich-step_name to gs_proc_jobslist-step_name.
        endif.

        " Only cancelled Jobs for the Process
        if ls_job_header-status eq mc_status_cancelled or
           ls_job_header-status eq mc_status_aborted .
          append gs_proc_jobslist to gt_proc_jobslist.
        endif.
        " All Successfull and Cancelled Payroll Driver Jobs
        " for the subsequent Filter by Next Pay Process
        if gs_proc_jobslist-step_id = lv_payroll_step_id.
          append gs_proc_jobslist to gt_all_payroll_jobs.
        endif.
      endloop.
    endif.

  endmethod.


  method GET_NEXT_PAY_PROCESS_TIME.
* Read Batch Jobs Employees to determine latest Pay process time
    if not gt_all_payroll_jobs is initial.
      " Select All jobs
      loop at gt_all_payroll_jobs into gs_proc_jobslist.
        gs_proc_jobslist-marker = abap_true.
        modify gt_all_payroll_jobs from gs_proc_jobslist index sy-tabix transporting marker.
      endloop.

      " Read Employee List from the Job Variants
      call method me->read_batchjobs_employees
        exporting
          it_jobslist = gt_all_payroll_jobs
        receiving
          rt_emplist  = rt_pay_process_time.
    endif.

  endmethod.


  method GET_PROC_JOBSLIST.
* Read Jobs list for Process
    data lv_index type sy-tabix.
    data lv_cancelled_flg type boolean.

    clear ev_msg.
* Read all process instances with active Event Handler
    ev_msg =  me->get_active_process_instances( ).
    check ev_msg is initial.

* Read all cancelled jobs for each one of the process instance
    loop at gt_pypi_id into gs_pypi_id.
      try.
          call method me->get_jobs_for_process_instance
            exporting
              is_pypi_id = gs_pypi_id.
        catch cx_pyc_cont .
      endtry.
    endloop.

* Read Batch Jobs "Employees for Reprocess"
    if not gt_proc_jobslist is initial.
      " Select All jobs
      loop at gt_proc_jobslist into gs_proc_jobslist.
        gs_proc_jobslist-marker = abap_true.
        modify gt_proc_jobslist from gs_proc_jobslist index sy-tabix transporting marker.
      endloop.

      " Read Employee List from the Job Variants
      call method me->read_batchjobs_employees
        exporting
          it_jobslist = gt_proc_jobslist
        receiving
          rt_emplist  = gt_emplist.

* Apply filters
* Report only selected Empoloyees
      if not gt_pernr_so is initial.
        delete gt_emplist where not pernr in gt_pernr_so.
      endif.

* Apply Active Employee filter
*      call method me->filter_by_employee_status
*        changing
*          ct_emplist = gt_emplist.

** Apply Payroll Runtime filter
*    call method me->filter_by_payroll_runtime
*      changing
*        ct_emplist = gt_emplist.

* Apply Payroll Process Time Filter
      call method me->filter_by_next_payrun
        changing
          ct_emplist = gt_emplist.

* Update batch jobs status
      loop at gt_proc_jobslist into gs_proc_jobslist.
        lv_index = sy-tabix.

        clear: lv_cancelled_flg.
        loop at gt_emplist into gs_emplist
          where jobname  = gs_proc_jobslist-jobname
            and jobcount = gs_proc_jobslist-jobcount
            and pypi_id = gs_proc_jobslist-pypi_id
            and repro = gc_repro_status-cancelled.
          lv_cancelled_flg = abap_true.
          exit.
        endloop.

        if lv_cancelled_flg = abap_true.
          gs_proc_jobslist-repro = gc_repro_status-cancelled.
        else.
          gs_proc_jobslist-repro = gc_repro_status-reprocessed.
        endif.

        clear gs_proc_jobslist-marker.
        modify gt_proc_jobslist from gs_proc_jobslist index lv_index transporting repro marker.
      endloop.

    endif.

* Export Data
    et_proc_jobslist = gt_proc_jobslist.
    et_emplist = gt_emplist.

  endmethod.


  method READ_BATCHJOBS_EMPLOYEES.
* Jobs List
    data: lt_jobslist type ZUSECL_M99_pcc_ehiupdate=>tty_proc_jobslist,
          ls_jobslist type ZUSECL_M99_pcc_ehiupdate=>ty_proc_jobslist.
    data: lt_emplist type ZUSECL_M99_pcc_ehiupdate=>tty_emplist,
          ls_emplist type ZUSECL_M99_pcc_ehiupdate=>ty_emplist.
    data: lv_lines type i.

* Job Steps
    types: begin of ty_job_step,
             jobname   type btcjob,
             jobcount  type btcjobcnt,
             stepcount type btcstepcnt,
             progname  type btcprog,
             variant   type btcvariant.
    types: end of ty_job_step.

    data: lt_job_steps type table of ty_job_step,
          ls_job_step  type ty_job_step.
    data: lv_payroll_period type iperi.
    data: lt_pernr_list type hcm_pernr_table,
          ls_pernr_list type hcm_pernr_list.
    data: lv_index type sy-tabix.

* Check selection.
    lt_jobslist[] = it_jobslist[].

    delete lt_jobslist where marker ne abap_true.
    describe table lt_jobslist lines lv_lines.
    if lv_lines eq 0.
      message i093(bt).
      exit.
    endif.

    if not lt_jobslist is initial.
      clear: lt_job_steps.
      select tstep~jobname tstep~jobcount
             tstep~stepcount tstep~progname tstep~variant
        into corresponding fields of table lt_job_steps
        from tbtcp as tstep
         for all entries in lt_jobslist
       where jobname  = lt_jobslist-jobname
         and jobcount = lt_jobslist-jobcount.
    endif.

* Read Employee List from the Job Variants
    loop at lt_job_steps into ls_job_step.
      refresh: lt_pernr_list.
      call method ZUSECL_M99_pcc_utilities=>get_variant_pnpindex_list
        exporting
          iv_program    = ls_job_step-progname
          iv_variant    = ls_job_step-variant
        importing
          ev_period     = lv_payroll_period
          et_pernr_list = lt_pernr_list.

      clear ls_jobslist.
      read table lt_jobslist into ls_jobslist
        with key jobname = ls_job_step-jobname
                 jobcount = ls_job_step-jobcount.

      loop at lt_pernr_list into ls_pernr_list.
        clear: ls_emplist.
        move-corresponding ls_jobslist to ls_emplist.
        move ls_jobslist-pypi_id to ls_emplist-pypi_id.
        move ls_pernr_list-pernr to ls_emplist-pernr.
        ls_emplist-runtime = |{ ls_emplist-strtdate }{ ls_emplist-strttime }|.
        append ls_emplist to lt_emplist.
      endloop.
    endloop.

* Return Employee List
    rt_emplist[] = lt_emplist.

  endmethod.
ENDCLASS.
