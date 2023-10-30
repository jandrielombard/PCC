class ZUSECL_M99_PCC_UTILITIES definition
  public
  final
  create public .

public section.

  types:
    begin of ty_payrun_time,
        pernr type p_pernr,
        rundt type rundt,
        runtm type runtm,
      end of ty_payrun_time .
  types:
    tty_payrun_time type table of ty_payrun_time .

  constants MC_PARAM_PNPINDEX type RSSCR_NAME value 'PNPINDEX' ##NO_TEXT.
  constants MC_PAR_TYPE_PROG type PYD_PAR_TYPE value 'PROGRAM' ##NO_TEXT.
  constants MC_PARAM_PNPPABRP type RSSCR_NAME value 'PNPPABRP' ##NO_TEXT.
  constants MC_PARAM_PNPPABRJ type RSSCR_NAME value 'PNPPABRJ' ##NO_TEXT.
  constants:
    begin of gc_tvarvc,
        exuname type rvari_vnam value 'ZPCC_MDLOG_EHIUPD_EXUNAME',
        exinfty type rvari_vnam value 'ZPCC_MDLOG_EHIUPD_EXINFTY',
        type_p  type rsscr_kind value 'P',
      end of gc_tvarvc .
  constants GC_BATCH_USER_TYPE type PYD_ADMIN_TYPE value 'PYD_DAE_US' ##NO_TEXT.
  constants GC_IT0003 type INFTY value '0003' ##NO_TEXT.
  constants GC_PCC_MSGCLS type SY-MSGID value 'ZHRPY_PCC_MSG' ##NO_TEXT.
  constants GC_MSGTY_ERROR type SY-MSGTY value 'E' ##NO_TEXT.
  constants GC_MSGTY_INFO type SY-MSGTY value 'I' ##NO_TEXT.

  class-methods SET_EH_INTERVAL_FOR_PAYROLL
    importing
      !IO_RES_CONTEXT type ref to IF_PYD_RES_CONTEXT
    changing
      !CV_SPLIT_INTERVAL type I .
  class-methods GET_CANCELLED_JOBS_EMPLOYEES
    importing
      !IV_PROGRAM type BTCPROG
      !IO_CONTEXT type ref to IF_PYD_RES_CONTEXT
      !IV_SHADOW_ID type PYD_SHADOW_ID
    exporting
      !ET_SEL_PARAMS type RSPARAMS_TT .
  class-methods GET_VARIANT_PNPINDEX_LIST
    importing
      !IV_PROGRAM type BTCPROG
      !IV_VARIANT type BTCVARIANT
    exporting
      !EV_PERIOD type IPERI
      value(ET_PERNR_LIST) type HCM_PERNR_TABLE .
  class-methods GET_JOB_PROCESS_INSTANCE_ID
    importing
      !IV_SEL_PAR_TYPE type PYD_PAR_TYPE default 'PERIOD'
      !IV_SEL_PAR_VAL type TVARV_VAL
      !IV_JOBNAME type BTCJOB
    returning
      value(RV_PYPI_ID) type PYC_PROC_INST_ID .
  class-methods SET_STATUS_FOR_SKIP_PRO_STEP
    changing
      !CT_STEPS_TAB type CL_PYC_RT_FACADE=>TY_T_STEP_INST .
  class-methods ENQUEUE_PY_MSG
    importing
      !IV_PERNR type PERNR_D
      !IV_RUN_TYPE type HRDCT_IS_TPY .
  class-methods DEQUEUE_PY_MSG
    importing
      !IV_PERNR type PERNR_D
      !IV_RUN_TYPE type HRDCT_IS_TPY .
  class-methods ADJUST_EVENT_HANDLER_ITEM_LIST
    changing
      !CT_EHI type IF_PYC_EVENT_HANDLER=>TY_T_EHI .
  class-methods GET_TIME_OF_PAYROLL_RUN
    importing
      !IV_ABKRS type ABKRS
      !IV_INPER type IPERI
      !IT_PERNR_SO type /IWBEP/T_COD_SELECT_OPTIONS
      !IV_TPY type HRDCT_IS_TPY
    exporting
      value(ET_RUNTIME) type ZUSECL_M99_PCC_UTILITIES=>TTY_PAYRUN_TIME .
  class-methods GET_EMPLOYEES_PAYROLL_RUNTIME
    importing
      !IV_ABKRS type ABKRS
      !IV_INPER type IPERI
      !IT_PERNR_SO type /IWBEP/T_COD_SELECT_OPTIONS
      !IV_TPY type HRDCT_IS_TPY
    exporting
      !ET_RUNTIME type ZUSECL_M99_PCC_UTILITIES=>TTY_PAYRUN_TIME .
  class-methods GET_PERNR_PAYROLL_RUNTIME
    importing
      !IV_PERNR type PERNR_D
      !IO_RES_CONTEXT type ref to IF_PYD_RES_CONTEXT
    exporting
      !ET_PAY_RUNTIME type ZUSECL_M99_PCC_UTILITIES=>TTY_PAYRUN_TIME .
  class-methods GET_AUDIT_FILES_ASDIR
    exporting
      !EV_ASDIR_NAME type EPSF-EPSDIRNAM
      !EV_FILE_MASK type EPSF-EPSFILNAM .
  class-methods GET_ASDIR_FILE_LIST
    importing
      !IV_ASDIR_NAME type EPSF-EPSDIRNAM
      !IV_FILE_MASK type EPSF-EPSFILNAM
    exporting
      !EV_DIR_NAME type EPSF-EPSDIRNAM
      !EV_FILE_COUNTER type EPSF-EPSFILSIZ
      !EV_ERROR_COUNTER type EPSF-EPSFILSIZ
      !ET_DIR_LIST type ZUSEAUTT_PCC_FILELIST
    exceptions
      READ_DIRECTORY_FAILED
      TOO_MANY_READ_ERRORS
      EMPTY_DIRECTORY_LIST .
  class-methods READ_ASFILE_TO_BINSTRING
    importing
      !IV_ASFILE type STRING
    returning
      value(RV_BINSTRING) type XSTRING .
  class-methods SAVE_BINSTRING_TO_PSFILE
    importing
      !IV_FILENAME type STRING
      !IV_BINSTRING type XSTRING .
  class-methods SAVE_BINSTRING_TO_ASFILE
    importing
      !IV_FILENAME type STRING
      !IV_BINSTRING type XSTRING
    exporting
      !EV_MSG type BAPI_MSG
      value(EV_IS_ERROR) type SY-SUBRC .
  class-methods RAISE_LONGTEXT_MSG
    importing
      !IV_MSGID type SY-MSGID
      !IV_MSGTY type SY-MSGTY
      !IV_MSGNO type SY-MSGNO
      !IV_TEXT type STRING
    returning
      value(RV_MSG) type STRING .
  protected section.
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PCC_UTILITIES IMPLEMENTATION.


  method ADJUST_EVENT_HANDLER_ITEM_LIST.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |21-11-2022 |1130848|Event Handler List Control        |PTX-3763      |CFAK902560        *
*    |           |       |Initial Implementation            |              |                  *
*---------------------------------------------------------------------------------------------*
* PCC Implementation - Event Handler Employee List Control
    constants: begin of lc_tvarvc,
                 eh_emps_for_recurrence
                    type rvari_vnam value 'ZPCC_EH_EMPS_FOR_RECURRENCE',
                 type_p
                    type rsscr_kind value 'P',
               end of lc_tvarvc.
    data: lv_emps_for_recurrence type tvarvc-low.
    data: lv_emps_number type i.

    types: begin of ty_s_ehi.
             include type pyc_s_ehi.
           types: end of ty_s_ehi .
    types: ty_t_ehi type table of ty_s_ehi.
    data: lt_init_ehi  type ty_t_ehi.
    data: lt_final_ehi type ty_t_ehi.
    data: lv_ee_count type i.

* Read Recurrence Employees Number fro TVARVC table
    select single low from tvarvc
      into @lv_emps_for_recurrence
     where name eq @lc_tvarvc-eh_emps_for_recurrence
       and type eq @lc_tvarvc-type_p.
    if sy-subrc eq 0.
      lv_emps_number = lv_emps_for_recurrence.
    else.
      lv_emps_number = 1500.
    endif.
* Restrict the number of Employees for recurrence
    clear: lt_init_ehi, lt_final_ehi , lv_ee_count.
    lt_init_ehi[] = ct_ehi[]. refresh: ct_ehi.

    sort lt_init_ehi by id pypi_id.
    loop at lt_init_ehi into data(lt_ehi_group)
             group by lt_ehi_group-id.
      if lv_ee_count eq lv_emps_number.
        exit.
      endif.

      loop at group lt_ehi_group into data(ls_ehi_group).
        append ls_ehi_group to lt_final_ehi.
      endloop.
      add 1 to lv_ee_count.
    endloop.

    sort lt_final_ehi by pypi_id tsl par_type id.
    ct_ehi[] = lt_final_ehi[].

  endmethod.


  method DEQUEUE_PY_MSG.
* Unlock Employee Records
    call function 'DEQUEUE_EZPYC_D_PY_MSG'
      exporting
*       mode_pyc_d_py_msg = 'E'
        mandt    = sy-mandt
        pernr    = iv_pernr
*       IABKR    =
*       IAPER    =
*       IENDD    =
*       INPTY    =
*       INPID    =
        test_res = iv_run_type
*       SEQNR    =
*       X_PERNR  = ' '
*       X_IABKR  = ' '
*       X_IAPER  = ' '
*       X_IENDD  = ' '
*       X_INPTY  = ' '
*       X_INPID  = ' '
*       X_TEST_RES        = ' '
*       X_SEQNR  = ' '
*       _SCOPE   = '3'
*       _SYNCHRON         = ' '
*       _COLLECT = ' '
      .

  endmethod.


  method ENQUEUE_PY_MSG.
* Execute Employee level data lock
    call function 'ENQUEUE_EZPYC_D_PY_MSG'
      exporting
        mode_pyc_d_py_msg = 'V'
        mandt             = sy-mandt
        pernr             = iv_pernr
*       IABKR             =
*       IAPER             =
*       IENDD             =
*       INPTY             =
*       INPID             =
        test_res          = iv_run_type
*       SEQNR             =
*       X_PERNR           = ' '
*       X_IABKR           = ' '
*       X_IAPER           = ' '
*       X_IENDD           = ' '
*       X_INPTY           = ' '
*       X_INPID           = ' '
*       X_TEST_RES        = ' '
*       X_SEQNR           = ' '
*       _SCOPE            = '2'
        _wait             = abap_true
*       _COLLECT          = ' '
      exceptions
        foreign_lock      = 1
        system_failure    = 2
        others            = 3.

  endmethod.


  method GET_ASDIR_FILE_LIST.
* read application server directory files
    constants: gc_1000 type i value 1000.
    data: ls_dir_list type zuseaust_pcc_epsfili.
    data: begin of file,
            dirname(75) type c, " name of directory. (possibly truncated.)
            name(75)    type c, " name of entry. (possibly truncated.)
            type(10)    type c, " type of entry.
            len(8)      type p, " length in bytes.
            owner(8)    type c, " owner of the entry.
            mtime(6)    type p, " last modification date, seconds since 1970
            mode(9)     type c, " like "rwx-r-x--x": protection mode.
            errno(3)    type c,
            errmsg(40)  type c,
          end of file.

* get directory listing
    call 'C_DIR_READ_FINISH'                  " just to be sure
          id 'ERRNO'  field file-errno
          id 'ERRMSG' field file-errmsg.

    call 'C_DIR_READ_START'
          id 'DIR'    field iv_asdir_name
          id 'FILE'   field iv_file_mask
          id 'ERRNO'  field file-errno
          id 'ERRMSG' field file-errmsg.
    if sy-subrc <> 0.
      raise read_directory_failed.
    endif.

    refresh et_dir_list.
    clear ev_file_counter.
    clear ev_error_counter.
    clear et_dir_list.
    do.
      clear file.
      clear ls_dir_list.
      call 'C_DIR_READ_NEXT'
            id 'TYPE'   field file-type
            id 'NAME'   field file-name
            id 'LEN'    field file-len
            id 'OWNER'  field file-owner
            id 'MTIME'  field file-mtime
            id 'MODE'   field file-mode
            id 'ERRNO'  field file-errno
            id 'ERRMSG' field file-errmsg.

*   handle files > 2147483647 byte (int 4) - hen 9.9.2004
      if file-len > 2147483647.
        ls_dir_list-size  = -99.
      else.
        ls_dir_list-size  = file-len.
      endif.
      ls_dir_list-name = file-name.
      if sy-subrc = 0.
        if file-type(1) = 'f' or              " regular file
           file-type(1) = 'F'.
          add 1 to ev_file_counter.
          ls_dir_list-rc   = 0.
          append ls_dir_list to et_dir_list.
        endif.
      elseif sy-subrc = 1.
        exit.
      else.
        if ev_error_counter > gc_1000.
          call 'C_DIR_READ_FINISH'
                id 'ERRNO'  field file-errno
                id 'ERRMSG' field file-errmsg.
          raise too_many_read_errors.
        endif.
        add 1 to ev_error_counter.
        ls_dir_list-rc  = 18.
        append ls_dir_list to et_dir_list.
      endif.
    enddo.

    call 'C_DIR_READ_FINISH'
          id 'ERRNO'  field file-errno
          id 'ERRMSG' field file-errmsg.

    if ev_file_counter > 0.
      sort et_dir_list by name ascending.
    else.
      raise empty_directory_list.
    endif.

  endmethod.


  method GET_AUDIT_FILES_ASDIR.
* Directory Name from logical file path
    data: lo_aux             type ref to cl_pyc_pi_alh_aux,
          lv_full_file_name  type string,
          lv_file_name       type string,
          lv_path_no_defined type boole_d.

    clear: ev_asdir_name, ev_file_mask.

* Read Audit Files File path
    lv_file_name = '*'.
    lo_aux = cl_pyc_pi_alh_aux=>get_instance( ).
    lv_path_no_defined = lo_aux->logical_path_is_not_defined( exporting iv_file_name = lv_file_name
                                                              importing ev_full_file_name = lv_full_file_name ).

    check lv_path_no_defined eq abap_false.

* Server Directory Name
    split lv_full_file_name at '*' into ev_asdir_name lv_file_name.

* File Mask
    ev_file_mask = '*'.
  endmethod.


  method GET_CANCELLED_JOBS_EMPLOYEES.
* Read Employees from Cancelled Jobs
    constants: lc_status_cancelled type btcstatus value 'C'.
    data: ls_par             type pyd_s_resp.
    data: lt_par             type  if_pyd_fnd_types=>ty_t_resp.
    data: lv_proc_inst_id    type pyc_proc_inst_id,
          lv_current_step_id type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id.

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
    data: lt_cancelled_jobs type tbtcjob_tt.

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

* Pernr LIST
    data: lt_pernr_list     type hcm_pernr_table,
          ls_pernr_list     type hcm_pernr_list,
          lt_all_pernr_list type hcm_pernr_table.
    data: lv_period type iperi.

* Parameters
    lt_par  = io_context->mt_par.
* Read Current Step ID
    read table lt_par into ls_par index 1.
    move ls_par-instid to lv_current_step_id.

* Read Process Instance ID
    try.
        lv_proc_inst_id = cl_pyd_fnd_aux=>get_resp_fixed_value(
                            iv_par_type = cl_pyc_rt_facade=>gcs_par_type-proc_inst
                            it_par      = lt_par ).
        if lv_proc_inst_id is initial.
          return.
        endif.
      catch cx_pyd_fnd.
        " do nothing. Just keep empty table.
        return.
    endtry.

* Check for Payroll Driver Step
    read table lt_par into ls_par with key par_type = mc_par_type_prog.
    check sy-subrc eq 0.  check ls_par-low eq iv_program.

* Read Batch Processing Component ID
    clear: lv_pyc_bpc_id.
    select single id into lv_pyc_bpc_id from  pyc_d_bpc
     where  pypi_id = lv_proc_inst_id
       and  step_id = lv_current_step_id.
    if sy-subrc eq 0.
* Read Job names and Job number details for the process step
      select bpc_id row_id job_name job_count
        into corresponding fields of table lt_pyc_d_bpc_job
        from pyc_d_bpc_job
        where bpc_id = lv_pyc_bpc_id.
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

      if ls_job_header-status eq lc_status_cancelled.
        append ls_job_header to lt_cancelled_jobs.
      endif.
    endloop.

    if not lt_cancelled_jobs is initial.
* Select Variants for Cancelled Jobs
      clear: lt_job_steps.
      select tstep~jobname tstep~jobcount
             tstep~stepcount tstep~progname tstep~variant
        into corresponding fields of table lt_job_steps
        from tbtcp as tstep
         for all entries in lt_cancelled_jobs
       where jobname  = lt_cancelled_jobs-jobname
         and jobcount = lt_cancelled_jobs-jobcount
         and progname = iv_program.

* Read Employee List from the Job Variants
      loop at lt_job_steps into ls_job_step.
        call method ZUSECL_M99_PCC_UTILITIES=>get_variant_pnpindex_list
          exporting
            iv_program    = iv_program
            iv_variant    = ls_job_step-variant
          importing
            ev_period     = lv_period
            et_pernr_list = lt_pernr_list.

        append lines of lt_pernr_list to lt_all_pernr_list.
      endloop.

    endif.

* Merge Employees list to the Selection list
    data: ls_sel_params  type rsparams.
    loop at lt_all_pernr_list into ls_pernr_list.
      clear ls_sel_params.
      move 'I'  to ls_sel_params-sign.
      move 'P'  to ls_sel_params-kind.
      move 'EQ' to ls_sel_params-option.
      move ls_pernr_list-pernr to ls_sel_params-low.
      ls_sel_params-selname = mc_param_pnpindex .
      append ls_sel_params to et_sel_params.
    endloop.

  endmethod.


  method GET_EMPLOYEES_PAYROLL_RUNTIME.
* Employee Range Table
    data: lt_pernr_range type	/iwbep/t_cod_select_options,
          ls_pernr_range like line of lt_pernr_range.
    data: lv_count type i.
    data: lt_runtime type ZUSECL_M99_PCC_UTILITIES=>tty_payrun_time.

* Process large number of Employee selection in batches
    if not it_pernr_so is initial.
      clear: lt_pernr_range, lv_count.
      loop at it_pernr_so into data(ls_pernr).
        move-corresponding ls_pernr to ls_pernr_range.
        append ls_pernr_range to lt_pernr_range.
        lv_count = lv_count + 1.

        if lv_count eq 2000.
          refresh lt_runtime .
          call method ZUSECL_M99_PCC_UTILITIES=>get_time_of_payroll_run
            exporting
              iv_abkrs    = iv_abkrs
              iv_inper    = iv_inper
              it_pernr_so = lt_pernr_range
              iv_tpy      = iv_tpy
            importing
              et_runtime  = lt_runtime.
          append lines of lt_runtime to et_runtime.

          clear: lt_pernr_range, lv_count.
        endif.
      endloop.

      if not lt_pernr_range is initial.
        refresh lt_runtime .
        call method ZUSECL_M99_PCC_UTILITIES=>get_time_of_payroll_run
          exporting
            iv_abkrs    = iv_abkrs
            iv_inper    = iv_inper
            it_pernr_so = lt_pernr_range
            iv_tpy      = iv_tpy
          importing
            et_runtime  = lt_runtime.
        append lines of lt_runtime to et_runtime.
      endif.
      " Process Entire Payroll Area when number range is not specified
    else.
      call method ZUSECL_M99_PCC_UTILITIES=>get_time_of_payroll_run
        exporting
          iv_abkrs    = iv_abkrs
          iv_inper    = iv_inper
          it_pernr_so = it_pernr_so
          iv_tpy      = iv_tpy
        importing
          et_runtime  = et_runtime.
    endif.

  endmethod.


  method GET_JOB_PROCESS_INSTANCE_ID.
* Read Process Instance ID
    data: gv_pypi_id type pyc_d_pypi-pypi_id,
          gv_proc_id type pyc_d_pypi-proc_id.

    data: lv_abkrs type abkrs.
    data: lv_original_pypte_id type pyc_d_pypi-original_pypte_id.

    clear: rv_pypi_id.
    " Payroll Area
    lv_abkrs = iv_jobname+0(2).
    case iv_jobname+3(1).
      when '1'.
        concatenate '%' lv_abkrs '_MONITORING%' into lv_original_pypte_id.
      when '2'.
        concatenate '%' lv_abkrs '_TEST%' into lv_original_pypte_id.
      when '3'.
        concatenate '%' lv_abkrs '_PRODUCTIVE%' into lv_original_pypte_id.
      when others.
    endcase.

    "get process instance
    select single pypi_id proc_id into ( gv_pypi_id, gv_proc_id )
      from pyc_d_pypi as a
     inner join pyc_d_pypisp as b
      on a~proc_id = b~id
    where a~time_sel_par_type = iv_sel_par_type
      and a~time_sel_par_val  = iv_sel_par_val
      and a~original_pypte_id like lv_original_pypte_id
      and b~value             = lv_abkrs.

    if sy-subrc eq 0.
      rv_pypi_id = gv_pypi_id.
    endif.

  endmethod.


  method GET_PERNR_PAYROLL_RUNTIME.
    constants: mc_abkrs   type pyd_par_type value 'ABKRS',
               mc_period  type pyd_par_type value 'PERIOD',
               mc_tpy_res type pyd_par_type value 'TPY_RES'.

    data lt_par  type if_pyd_fnd_types=>ty_t_resp.
    data ls_par  type pyd_s_resp.

    data lt_pernr_so type /iwbep/t_cod_select_options.
    data lv_abkrs	type abkrs.
    data lv_inper	type iperi.
    data lv_tpy_res type xfeld.

* Employee Number
    try.
        call method cl_pyd_fnd_aux=>append_so_fixed_value
          exporting
            iv_value = iv_pernr
          changing
            ct_so    = lt_pernr_so.
      catch cx_pyd_fnd.
    endtry.

* Parameters
    lt_par  = io_res_context->mt_par.
* Set ABKRS, Period Begda and Period Endda
    loop at lt_par into ls_par.
      case ls_par-par_type.
        when mc_abkrs.
          lv_abkrs = ls_par-low.
        when mc_period.
          lv_inper = ls_par-low.
        when mc_tpy_res.
          lv_tpy_res = ls_par-low.
        when others.
      endcase.
    endloop.

* Payroll Run Time
    refresh et_pay_runtime .
    call method ZUSECL_M99_PCC_UTILITIES=>get_time_of_payroll_run
      exporting
        iv_abkrs    = lv_abkrs
        iv_inper    = lv_inper
        it_pernr_so = lt_pernr_so
        iv_tpy      = lv_tpy_res
      importing
        et_runtime  = et_pay_runtime.

  endmethod.


  method GET_TIME_OF_PAYROLL_RUN.
* Read Employee Numbers
    if iv_tpy is not initial.  "Test Payroll
      select hrdct_tpy_rgdir~dct_pernr as pernr,
             hrdct_tpy_rgdir~rundt as rundt,
             hrdct_tpy_rgdir~runtm as runtm
        into corresponding fields of table @et_runtime
        from hrdct_tpy_rgdir
       where hrdct_tpy_rgdir~dct_pernr in @it_pernr_so
         and hrdct_tpy_rgdir~abkrs eq @iv_abkrs
         and hrdct_tpy_rgdir~fpper = @iv_inper
         and hrdct_tpy_rgdir~inper = @iv_inper
        %_hints adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")'.
    else.                          "Production Payroll
      select p2rx_eval_period~dct_pernr as pernr,
             hrdct_tpy_rgdir~rundt as rundt,
             hrdct_tpy_rgdir~runtm as runtm
        into corresponding fields of table @et_runtime
        from p2rx_eval_period inner join hrdct_tpy_rgdir
              on p2rx_eval_period~dct_pernr eq hrdct_tpy_rgdir~dct_pernr
             and p2rx_eval_period~dct_seqnr eq hrdct_tpy_rgdir~dct_seqnr
       where p2rx_eval_period~dct_pernr in @it_pernr_so
         and p2rx_eval_period~abkrs = @iv_abkrs
         and p2rx_eval_period~fpper = @iv_inper
         and p2rx_eval_period~inper = @iv_inper
        %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC").'.
    endif.

  endmethod.


  method GET_VARIANT_PNPINDEX_LIST.
* Read Employee List
    data: lv_report      type  rsvar-report,
          lv_variant     type rsvar-variant,
          lt_var_content type table of rsparamsl_255,
          ls_var_content type rsparamsl_255.
    data: ls_pernr_list type hcm_pernr_list.
    data lv_pabrp type pabrp.
    data lv_pabrj type pabrj.

* Read Variant Contents
    clear: lt_var_content, et_pernr_list.
    lv_report = iv_program.
    lv_variant = iv_variant.
    call function 'RS_VARIANT_CONTENTS_255'
      exporting
        report               = lv_report
        variant              = lv_variant
      tables
        valutab              = lt_var_content
      exceptions
        variant_non_existent = 1
        variant_obsolete     = 2
        others               = 3.

* read Period
    read table lt_var_content into ls_var_content
      with key selname = mc_param_pnppabrp.
    if sy-subrc eq 0.
      move ls_var_content-low to lv_pabrp.
    endif.
    read table lt_var_content into ls_var_content
      with key selname = mc_param_pnppabrj.
    if sy-subrc eq 0.
      move ls_var_content-low to lv_pabrj.
    endif.
    concatenate lv_pabrj lv_pabrp into ev_period.

* Collect Employee List
    loop at lt_var_content into ls_var_content
        where selname eq mc_param_pnpindex.

      move ls_var_content-low to ls_pernr_list-pernr.
      append ls_pernr_list to et_pernr_list.
    endloop.

  endmethod.


  method RAISE_LONGTEXT_MSG.
* Split Message
    data: lv_msgv1 type  syst_msgv,
          lv_msgv2 type  syst_msgv,
          lv_msgv3 type  syst_msgv,
          lv_msgv4 type  syst_msgv.

    constants: lc_max_length type i value 50.
    data: lt_components type standard table of swastrtab.
    data: begin of ls_msgvn,
            filler(7) type c value 'LV_MSGV',
            num       type c.
    data: end of ls_msgvn.
    field-symbols: <msgvn> type syst_msgv.

    if strlen( iv_text ) > lc_max_length.
      call function 'SWA_STRING_SPLIT'
        exporting
          input_string                 = iv_text
          max_component_length         = lc_max_length
        tables
          string_components            = lt_components
        exceptions
          max_component_length_invalid = 1
          others                       = 2.

      if sy-subrc eq 0.
        loop at lt_components into data(ls_components).
          if sy-tabix le 4.
            move sy-tabix to ls_msgvn-num.
          else.
            exit.
          endif.
          assign (ls_msgvn) to <msgvn>.
          <msgvn>  = ls_components-str.
        endloop.
      else.
        lv_msgv1 = iv_text.
      endif.
    else.
      lv_msgv1 = iv_text.
    endif.

* Build the message
    message id iv_msgid type iv_msgty number iv_msgno
     with lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 into rv_msg.

  endmethod.


  method READ_ASFILE_TO_BINSTRING.
* Read Application Server File to Bianry string
    data ls_rawdata type xstring.
    data lv_msg type string.

    clear rv_binstring.
* Open Data Set
    open dataset iv_asfile for input in binary mode message lv_msg.
    if sy-subrc <> 0.
    else.
      do.
        read dataset iv_asfile into ls_rawdata.
        if sy-subrc <> 0.
          exit.
        endif.
        if not rv_binstring is initial.
          concatenate rv_binstring ls_rawdata into rv_binstring in byte mode.
        else.
          rv_binstring = ls_rawdata.
        endif.
        clear: ls_rawdata.
      enddo.
      close dataset iv_asfile.
    endif.

* Build XSTRING
    if rv_binstring is initial.
      lv_msg = iv_asfile .
    endif.

  endmethod.


  method SAVE_BINSTRING_TO_ASFILE.
* Transfer Binary String to Application Server file
    data lt_rawdata type solix_tab.
    data lv_msg type string.

*    clear: ev_msg, ev_is_error.
    lt_rawdata = cl_bcs_convert=>xstring_to_solix( iv_binstring ).

* Open the file in binary mode
    open dataset iv_filename for output in binary mode message lv_msg.
    if sy-subrc ne 0.
      message lv_msg type 'I'. " into ev_msg.
      ev_is_error = 8.
    else.
* Transfer the data
      loop at lt_rawdata into data(lv_data).
        transfer lv_data to iv_filename.
      endloop.

* close the file
      close dataset iv_filename.
    endif.
    if sy-subrc <> 0.
      message i094(zhrpy_pcc_msg). " into ev_msg.
      ev_is_error = 8.
    else.
      message i095(zhrpy_pcc_msg)." into ev_msg.
    endif.

  endmethod.


  method SAVE_BINSTRING_TO_PSFILE.
* Save File data to loacl directory
    data lt_rawdata type solix_tab.
    data lv_msg type string.

    lt_rawdata = cl_bcs_convert=>xstring_to_solix( iv_binstring ).

    cl_gui_frontend_services=>gui_download(
      exporting
        bin_filesize              = xstrlen( iv_binstring )
        filename                  = iv_filename
        filetype                  = 'BIN'
      changing
        data_tab                  = lt_rawdata
      exceptions
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        others                    = 24 ).

    if sy-subrc <> 0.
      lv_msg = iv_filename .
*      call method ZUSECL_M99_PCC_UTILITIES=>raise_longtext_msg
*        exporting
*          iv_msgid = ZUSECL_M99_PCC_UTILITIES=>gc_fi_recon_msgcls
*          iv_msgty = ZUSECL_M99_PCC_UTILITIES=>gc_msgty_error
*          iv_msgno = '007'
*          iv_text  = lv_msg
*        receiving
*          rv_msg   = lv_msg.
*      write: /1 lv_msg.
*
*      raise exception type zcx_fi_recon
*        exporting
*          textid      = zcx_fi_recon=>file_guidownload_error
*          mv_filename = iv_filename.
    else.
*      lv_msg = iv_filename .
*      call method ZUSECL_M99_PCC_UTILITIES=>raise_longtext_msg
*        exporting
*          iv_msgid = ZUSECL_M99_PCC_UTILITIES=>gc_fi_recon_msgcls
*          iv_msgty = ZUSECL_M99_PCC_UTILITIES=>gc_msgty_error
*          iv_msgno = '006'
*          iv_text  = lv_msg
*        receiving
*          rv_msg   = lv_msg.
*      write: /1 lv_msg.
    endif.

  endmethod.


  method SET_EH_INTERVAL_FOR_PAYROLL.
* This Change allows Event handler to change the PYP_INTERVAL to 300 Employees
* Per job and there by improves the Payroll Driver Execution Time
    constants: lc_eh              type pyd_shadow_type value 'EH',
               lc_par_type_prog   type pyd_par_type    value 'PROGRAM',
               lc_au_payroll_prog type tvarv_val       value 'RPCALCQ0'.

    constants: begin of lc_tvarvc,
                 eh_interval_for_payroll
                    type rvari_vnam value 'ZPCC_EH_INTERVAL_FOR_PAYROLL',
                 type_p
                    type rsscr_kind value 'P',
               end of lc_tvarvc.

    data: lv_split_interval type tvarvc-low.
    data: ls_par                  type pyd_s_resp.

    " Check for Event Handler
    if not io_res_context->ms_shadow_info-shadow_id is initial
       and io_res_context->ms_shadow_info-shadow_type eq lc_eh.

      " Check for Payroll Driver Program
      read table io_res_context->mt_par into ls_par
        with key par_type = lc_par_type_prog.
      if sy-subrc eq 0 and ls_par-low eq lc_au_payroll_prog.

* Change the split interval for Event Handler
        select single low from tvarvc
          into @lv_split_interval
          where name eq @lc_tvarvc-eh_interval_for_payroll
            and type eq @lc_tvarvc-type_p.
        if sy-subrc eq 0.
          cv_split_interval = lv_split_interval.
        else.
          cv_split_interval = 300.
        endif.

      endif.
    endif.

  endmethod.


  method SET_STATUS_FOR_SKIP_PRO_STEP.
* enhance error status for confilicting " Execution and Error Status "
    constants: lc_close_payroll              type pyc_proc_step_template_id value 'ZHR_V2_CLOSE_PAYROLL',
               lc_execution_status_execution type pyc_execution_status value '03',
               lc_error_status_error         type pyc_error_status value '03',
               lc_error_status_ok            type pyc_error_status value '04',
               lc_error_status_preparation   type pyc_error_status value '02',
               lc_exit_payroll               type t569v-state value '3'.

    data: ls_sts_inst_res  type cl_pyc_rt_facade=>ty_s_step_inst.
    data: lv_ei_index type sy-tabix.
    data: lv_value type pyc_d_pypisp-value,
          lv_abkrs type t569v-abkrs,
          lv_state type  t569v-state.

    loop at ct_steps_tab into ls_sts_inst_res.
      lv_ei_index = sy-tabix.

      " Special Process for Process Steps
      case ls_sts_inst_res-step_template_id.
        when lc_close_payroll.
          " Execution status "Execution" and Error Status "OK"
          if ls_sts_inst_res-execution_status eq lc_execution_status_execution and
             ( ls_sts_inst_res-error_status     eq lc_error_status_ok or
               ls_sts_inst_res-error_status     eq lc_error_status_error ).

            "get payroll area
            clear lv_value.
            select single b~value into lv_value
              from pyc_d_pypi as a
             inner join pyc_d_pypisp as b
                on a~proc_id = b~id
             where a~pypi_id = ls_sts_inst_res-pypi_id.

            if sy-subrc eq 0.
              " Check Payroll Area Status
              lv_abkrs = lv_value.
              clear lv_state.
              call function 'PA03_PCR_READ'
                exporting
                  f_abkrs               = lv_abkrs
                importing
                  f_state               = lv_state
                exceptions
                  abkrs_no_accounting   = 1
                  pcr_does_not_exist    = 2
                  abkrs_does_not_exist  = 3
                  period_does_not_exist = 4
                  others                = 5.

              if lv_state eq lc_exit_payroll.
                " Incase Payroll Area in Exit Status set the error status to 02 "Struck
                ls_sts_inst_res-error_status = lc_error_status_preparation.

                modify ct_steps_tab from ls_sts_inst_res index lv_ei_index transporting error_status.
              endif.

            endif.
          endif.
        when others.     " All Other Process Steps

      endcase.
    endloop.

  endmethod.
ENDCLASS.
