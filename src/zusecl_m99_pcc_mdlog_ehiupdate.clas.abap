class ZUSECL_M99_PCC_MDLOG_EHIUPDATE definition
  public
  final
  create public .

public section.

  types:
* change details
    begin of ty_pcl4list,
        srtfd  type pcl4-srtfd,
        aedtm  type pcl4-aedtm,
        uname  type pcl4-uname,
        pgmid  type pcl4-pgmid,
        marker type xfeld.
    types: end of ty_pcl4list .
  types:
    tty_pcl4list type table of ty_pcl4list .
  types:
* Employee Details
    begin of ty_empslist,
        pernr   type pldoc_key-pernr,
        infty   type pldoc_key-infty,
        bdate   type pldoc_key-bdate,
        btime   type pldoc_key-btime,
        seqnr   type pldoc_key-seqnr,
        uname   type pldoc_key-uname,
        pgmid   type pcl4-pgmid,
        chgtime type timestamp,
        message type bapi_msg.
    types: end of ty_empslist .
  types:
    tty_empslist type table of ty_empslist .
  types:
* Employee Payroll Area
    begin of ty_it0001,
        pernr type p0001-pernr,
        abkrs type p0001-abkrs.
    types: end of ty_it0001 .
  types:
    tty_it0001 type sorted table of ty_it0001 with unique key pernr .

  constants GC_EHIUPD type SY-UCOMM value 'EHIUPD' ##NO_TEXT.
  constants GC_LONG_TERM_DOC type PCL4-RELID value 'LA' ##NO_TEXT.
  constants GC_PAR_TYPE_PERNR type PYD_PAR_TYPE value 'PERNR' ##NO_TEXT.
  constants:
    begin of gc_tvarvc,
        exuname type rvari_vnam value 'ZPCC_MDLOG_EHIUPD_EXUNAME',
        exinfty type rvari_vnam value 'ZPCC_MDLOG_EHIUPD_EXINFTY',
        type_p  type rsscr_kind value 'P',
      end of gc_tvarvc .
  constants:
    begin of gc_range_val,
        include type tvarv_sign value 'I',
        between type tvarv_opti value 'BT',
      end of gc_range_val .
  constants GC_UTC type TZNZONE value 'UTC' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_PYPI_ID type PYC_D_PYPI-PYPI_ID
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_BEGTZ type UZEIT
      !IV_ENDTZ type UZEIT
      !IV_AEST type XFELD
      !IT_EXUNAME type TAB_RANGE_UNAME
      !IT_EXINFTY type INFTY_RANGE_TAB .
  class-methods CONVERT_TO_UTC
    changing
      !CV_TIMESTAMP type TIMESTAMP .
  class-methods GET_EHIUPD_EXC_UNAME_INFTY
    exporting
      !ET_EXUNAME type TAB_RANGE_UNAME
      !ET_EXINFTY type INFTY_RANGE_TAB .
  methods ADD_EMPLOYEES_TO_EHITABLE
    importing
      !IV_TEST type XFELD
    exporting
      !ET_EMPSLIST type TTY_EMPSLIST .
  methods READ_MASTERDATA_CHANGES .
  class-methods GET_PAYROLL_PERIOD_DATES
    importing
      !IV_ABKRS type ABKRS
      !IV_INPER type IPERI
    exporting
      !EV_BEGDA type BEGDA
      !EV_ENDDA type ENDDA .
  class-methods SUBMIT_MDLOG_EHIUPDATE
    importing
      !IV_PYPI_ID type PYC_PROC_INST_ID
    raising
      CX_PYC_CONT .
  protected section.

* Selection Ranges
    data gt_aedtm_so type /iwbep/t_cod_select_options .
    data gt_change_period type ppfsrtstmp .
    data gv_timestamp_low type timestamp .
    data gv_timestamp_high type timestamp .
* Global Variables
    data gv_pypi_id type pyc_d_ehi-pypi_id .
    data gv_begda type begda .
    data gv_endda type endda .
    data gv_begtz type uzeit .
    data gv_endtz type uzeit .
    data gv_abkrs type hrpy_rgdir-iabkrs .
    data gv_inper type hrpy_rgdir-inper .
    data gv_ipend type hrpy_rgdir-ipend .
    data gv_period_begda type hrpy_rgdir-fpbeg .
    data gv_period_endda type hrpy_rgdir-fpend .
* Change Details
    data: gs_pcl4list type ty_pcl4list .
    data: gt_pcl4list type table of ty_pcl4list .
* Employee Details
    data: gs_empslist type ty_empslist .
    data: gt_empslist type table of ty_empslist .
* Employee Payroll Area
    data: gs_it0001 type ty_it0001.
    data: gt_it0001 type sorted table of ty_it0001 with unique key pernr .
* Variable Table Entries for User Name and Infotypes
    data gt_exuname type tab_range_uname .
    data gt_exinfty type infty_range_tab .
* Payroll Run time
    data gt_pay_runtime  type ZUSECL_M99_PCC_UTILITIES=>tty_payrun_time.

  private section.

    methods filter_by_payroll_runtime .
ENDCLASS.



CLASS ZUSECL_M99_PCC_MDLOG_EHIUPDATE IMPLEMENTATION.


  method ADD_EMPLOYEES_TO_EHITABLE.
* Update PYC_D_EHI Table with changed Employee list
    data: lo_event_handler type ref to if_pyc_event_handler,
          lv_par_type      type pyd_par_type,
          lv_roid          type pyd_roid.
*
    data: lt_proc_inst type if_pyc_event_handler=>ty_t_proc_inst,
          lv_proc_inst type pyc_proc_inst_id.
    data: lv_period type tvarv_val.
    data: lv_index type sy-tabix.
    data: lt_empslist type table of ty_empslist .

* Add Employees to Event Handler
    if iv_test is initial and not gt_empslist is initial.

      if gv_pypi_id is initial.
        message e000(zhrpy_pcc_msg) with text-010.
      endif.

      lt_empslist = gt_empslist.
      sort lt_empslist by pernr chgtime.
      delete adjacent duplicates from lt_empslist comparing pernr.

      try .
          lo_event_handler = cl_pyc_event_handler_factory=>get_event_handler_instance( ).
        catch cx_pyc_eh.
          return.
      endtry.

      loop at lt_empslist into gs_empslist.
        lv_index = sy-tabix.


        refresh: lt_proc_inst.
        append gv_pypi_id to lt_proc_inst.

        lv_roid     = gs_empslist-pernr.
        lv_par_type = gc_par_type_pernr.

        try .
            call method lo_event_handler->event_handler_item_create_list
              exporting
                iv_par_type  = lv_par_type
                iv_id        = lv_roid
                it_proc_inst = lt_proc_inst.
          catch cx_pyc_eh.
        endtry.

      endloop.
    endif.

* Return the change list for Reporting
    et_empslist[] = gt_empslist[].

  endmethod.


  method CONSTRUCTOR.
* Initialize Selection.
    gv_pypi_id = iv_pypi_id.
    gv_begda = iv_begda.
    gv_endda = iv_endda.
    gv_begtz = iv_begtz.
    gv_endtz = iv_endtz.

* Period Selection
    if gv_begda is initial.
      move sy-datum to gv_begda.
    endif.
    if gv_endda is initial.
      move gv_begda to gv_endda.
    endif.
    if gv_begtz is initial.
      move sy-uzeit to gv_begtz.
    endif.
    if gv_endtz is initial.
      move sy-uzeit to gv_endtz.
    endif.

* Fill in Date Range tables
    gt_aedtm_so = value #( ( sign = gc_range_val-include option = gc_range_val-between low = gv_begda high = gv_endda ) ).
    gv_timestamp_low  = |{ gv_begda }{ gv_begtz }|.
    gv_timestamp_high = |{ gv_endda }{ gv_endtz }|.

* apply date/time conversion if the date/time entered is in AEST
    if iv_aest eq abap_true.
      call method zusecl_m99_pcc_mdlog_ehiupdate=>convert_to_utc
        changing
          cv_timestamp = gv_timestamp_low.

      call method zusecl_m99_pcc_mdlog_ehiupdate=>convert_to_utc
        changing
          cv_timestamp = gv_timestamp_high.
    endif.

*" timestamp range for selection
    gt_change_period = value #( sign = gc_range_val-include option = gc_range_val-between
                              ( low  = gv_timestamp_low  high = gv_timestamp_high ) ).

* Populate Exclude User Names and infty from TVARVC
    call method zusecl_m99_pcc_mdlog_ehiupdate=>get_ehiupd_exc_uname_infty
      importing
        et_exuname = gt_exuname
        et_exinfty = gt_exinfty.

* Populate Exclude User Names
    if not it_exuname[] is initial.
      gt_exuname[] = it_exuname[].
    endif.
* Populate Exclude Infotypes
    if not it_exinfty[] is initial.
      gt_exinfty[] = it_exinfty[].
    endif.

* Get process instance Payroll Area and Period
    select single b~value a~time_sel_par_val into ( gv_abkrs, gv_inper )
      from pyc_d_pypi as a
     inner join pyc_d_pypisp as b on a~proc_id = b~id
     where a~pypi_id = iv_pypi_id.
    if sy-subrc eq 0.
      " Read Period Details
      call method zusecl_m99_pcc_mdlog_ehiupdate=>get_payroll_period_dates
        exporting
          iv_abkrs = gv_abkrs
          iv_inper = gv_inper
        importing
          ev_begda = gv_period_begda
          ev_endda = gv_period_endda.
    endif.

  endmethod.


  method CONVERT_TO_UTC.
*  Converts date and time to UTC timestamp
    data: lv_timestamp_gmt  type timestamp,
          lv_timestamp_char type char15,
          lv_date_utc       type datum,
          lv_time_utc       type uzeit,
          lv_date_aest      type datum,
          lv_time_aest      type uzeit.
*" temp var to allow subfield access
    lv_timestamp_char = cv_timestamp.
    lv_date_aest = lv_timestamp_char+0(8).
    lv_time_aest = lv_timestamp_char+8(6).

    call function 'IB_CONVERT_INTO_TIMESTAMP'
      exporting
        i_datlo     = lv_date_aest
        i_timlo     = lv_time_aest
        i_tzone     = sy-zonlo
      importing
        e_timestamp = lv_timestamp_gmt.

    call function 'IB_CONVERT_FROM_TIMESTAMP'
      exporting
        i_timestamp = lv_timestamp_gmt
        i_tzone     = gc_utc
      importing
        e_datlo     = lv_date_utc
        e_timlo     = lv_time_utc.

    cv_timestamp = |{ lv_date_utc }{ lv_time_utc }|.

  endmethod.


  method FILTER_BY_PAYROLL_RUNTIME.
* Apply Payroll Run Time condition
    data lt_pernr_so type /iwbep/t_cod_select_options.
    data lv_lines type i.
    data lv_pay_runtime type timestamp.

* Selection Option
    describe table gt_it0001 lines lv_lines.
    if lv_lines > 10000.
      "Read for entire payroll area
      refresh: lt_pernr_so.
    else.
      " Read for selected personal numbers
      refresh: lt_pernr_so.
      loop at gt_it0001 into gs_it0001.
        try.
            call method cl_pyd_fnd_aux=>append_so_fixed_value
              exporting
                iv_value = gs_it0001-pernr
              changing
                ct_so    = lt_pernr_so.
          catch cx_pyd_fnd.
        endtry.
      endloop.
    endif.

* Payroll Run Time
    call method ZUSECL_M99_PCC_UTILITIES=>get_employees_payroll_runtime
      exporting
        iv_abkrs    = gv_abkrs
        iv_inper    = gv_inper
        it_pernr_so = lt_pernr_so
        iv_tpy      = abap_true
      importing
        et_runtime  = gt_pay_runtime.

* Filter Master Data changes
    loop at gt_pay_runtime
        into data(gs_pay_runtime).
      lv_pay_runtime = |{ gs_pay_runtime-rundt }{ gs_pay_runtime-runtm }|.

      delete gt_empslist
        where pernr eq gs_pay_runtime-pernr
          and chgtime lt lv_pay_runtime.

    endloop.

  endmethod.


  method GET_EHIUPD_EXC_UNAME_INFTY.
* Read Exlude User Names and Infotypes from TVARVC table
    data: lv_admin_value type pyd_d_atv-admin_value.
    data: lt_so type /iwbep/t_cod_select_options.

* Populate Exclude User Names
    select tvar~sign, tvar~opti as option, tvar~low, tvar~high
      from tvarvc as tvar
      into corresponding fields of table @et_exuname
     where name eq @gc_tvarvc-exuname
       and type eq @gc_tvarvc-type_p.
    if sy-subrc <> 0.
      "Add PCC Daemon user by Default
      clear: lv_admin_value.
      select single admin_value into lv_admin_value
        from  pyd_d_atv where  admin_type   = ZUSECL_M99_PCC_UTILITIES=>gc_batch_user_type. "PYD_DAE_US
      if sy-subrc eq 0.
        clear lt_so.
        lt_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = lv_admin_value   ).
        et_exuname = corresponding #( lt_so ).
      endif.
    endif.

* Populate Exclude Infotypes
    select tvar~sign, tvar~opti as option, tvar~low, tvar~high
      from tvarvc as tvar
     into corresponding fields of table @et_exinfty
    where name eq @gc_tvarvc-exinfty
      and type eq @gc_tvarvc-type_p.
    if sy-subrc <> 0.
      "Add Infotype 0003 by Default
      clear lt_so.
      lt_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ZUSECL_M99_PCC_UTILITIES=>gc_it0003   ).
      et_exinfty = corresponding #( lt_so ).
    endif.

  endmethod.


  method GET_PAYROLL_PERIOD_DATES.
* Read Period Start Date and End Date
    data: lo_payroll_area type ref to cl_hr_payroll_area.
    data: lv_pabrj type pabrj,
          lv_pabrp type pabrp.

    lo_payroll_area = cl_hr_payroll_area=>get_instance( imp_area = iv_abkrs ).
    lv_pabrj = iv_inper(4).
    lv_pabrp = iv_inper+4(2).

    lo_payroll_area->get_period_info(
      exporting
        imp_pabrj = lv_pabrj
        imp_pabrp = lv_pabrp
      importing
        exp_begda = ev_begda
        exp_endda = ev_endda ).

  endmethod.


  method READ_MASTERDATA_CHANGES.
* Read Master Data changes for the selection
    data: lt_empslist like table of gs_empslist,
          lt_list     like table of gs_empslist.
    data: lv_change_time type timestamp.

* Read Master Data Changes
    select srtfd aedtm uname pgmid
      into corresponding fields of table gt_pcl4list
      from pcl4
     where relid = gc_long_term_doc
       and aedtm in gt_aedtm_so.

* Delete all records that are created by the selected exclude usernames
    if not gt_exuname is initial.
      delete gt_pcl4list where uname in gt_exuname.
    endif.

* Rebuild the change list into structure that includes fieldnames
* PERNR, Infotype, Change Date, Time, Sequence number,
* Username, Change Date and Program ID.
    loop at gt_pcl4list into gs_pcl4list.
      move: gs_pcl4list-srtfd+1(8)  to gs_empslist-pernr,
            gs_pcl4list-srtfd+9(4)  to gs_empslist-infty,
            gs_pcl4list-srtfd+13(8) to gs_empslist-bdate,
            gs_pcl4list-srtfd+21(6) to gs_empslist-btime,
            gs_pcl4list-srtfd+27(4) to gs_empslist-seqnr.
      move: gs_pcl4list-uname to gs_empslist-uname,
            gs_pcl4list-pgmid to gs_empslist-pgmid.

      lv_change_time  = gs_pcl4list-srtfd+13(14).
      check lv_change_time in gt_change_period.
      move lv_change_time to gs_empslist-chgtime.

      append gs_empslist to lt_empslist.
    endloop.

* Delete all records that belongs to selected exclude infotypes
    if  not gt_exinfty is initial.
      delete lt_empslist where infty in gt_exinfty.
    endif.

*Collect the Employee list
    gt_empslist[] = lt_empslist[]. sort gt_empslist by pernr.
    delete adjacent duplicates from gt_empslist comparing pernr.

* Add only the employees belongs to selected payroll area
    if not gt_empslist is initial.
      select it0001~pernr as pernr, it0001~abkrs as abkrs
       into corresponding fields of table @gt_it0001
       from pa0001 as it0001
        for all entries in @gt_empslist
      where it0001~pernr = @gt_empslist-pernr
        and it0001~endda >= @gv_period_endda
        and it0001~begda <= @gv_period_endda.

      delete gt_it0001 where abkrs <> gv_abkrs.
    endif.

* Rebuild the Employee Changes list
    refresh: gt_empslist.
* Apply filters
    if not gt_it0001 is initial.
      gt_empslist = filter #( lt_empslist in gt_it0001 where pernr = pernr ).
    endif.
    sort gt_empslist by pernr chgtime.

* Apply Payroll Run Time condition
    call method me->filter_by_payroll_runtime.

  endmethod.


  method SUBMIT_MDLOG_EHIUPDATE.
* Use Process Instance to read Process steps
    constants: mc_pty_program     type pyd_d_instp-par_type value 'PROGRAM',
               mc_payroll_program type pyd_d_instp-low value 'RPCALCQ0',
               mc_posting_program type pyd_d_instp-low value 'RPCIPE01',
               mc_cat_policy      type pyc_step_templ_cat value 'POLICY',
               mc_cat_moni        type pyc_step_templ_cat value 'MONI',
               mc_cat_other       type pyc_step_templ_cat value 'OTHER',
               mc_report_name     type syrepid value 'ZUSEREPY_PCC_MDLOG_EHIUPD00',
               mc_short_repname   type syrepid value 'MDLOGEHIUPD'.

    constants:
      begin of gcs_action ,
        start_step      type pyd_rd_type value cl_pyc_pt_sts_err=>gcs_operation-main_act,
        start_process   type pyd_rd_type value 'PYP_STS_EXE_OPEN',
        repeat          type pyd_rd_type value 'PYP_STS_EXE_RESET',
        confirm         type pyd_rd_type value 'PYP_STS_EXE_CLOSE',
        edit_assignment type pyd_rd_type value cl_pyc_pt_sts_err=>gcs_data-status_details,
        status_details  type pyd_rd_type value cl_pyc_pt_sts_err=>gcs_data-status_details,
      end of gcs_action .

    data lt_program_so  type /iwbep/t_cod_select_options.
    data lt_step_id_so  type /iwbep/t_cod_select_options.
    data lt_trigger_rdt_so type /iwbep/t_cod_select_options.

* Process Step Read
    data: lo_fnd_factory  type ref to cl_pyd_fnd_factory.
    data: lv_id type pyd_d_instp-id.
    data: lv_proc_inst_id     type pyc_proc_inst_id,
          ls_proc_inst_id_so  type if_pyd_fnd_types=>ty_s_so,
          lo_proc_inst_aux    type ref to  cl_pyc_proc_inst_aux,
          lt_proc_inst_id_so  type /iwbep/t_cod_select_options,
          lx_exc_frw          type ref to  cx_pyc_frw,
          lx_fnd_exc          type ref to cx_pyd_fnd,
          lt_step_inst        type cl_pyc_proc_inst_aux=>ty_t_step_inst,
          ls_step_inst        type cl_pyc_proc_inst_aux=>ty_s_step_inst,
          lt_step_inst_enrich type cl_pyc_rt_facade=>ty_t_step_inst,
          ls_step_inst_enrich type cl_pyc_rt_facade=>ty_s_step_inst,
          lv_step_id          type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id,
          lv_moni_step_id     type cl_pyc_proc_inst_aux=>ty_s_step_inst-step_id.

*  Identify and read Payroll Driver process step start time
    data: begin of ls_pyd_d_al,
            instid       type pyd_instid,
            par_hash     type pyd_parhash,
            tsl           type timestampl,
            trigger_rdt   type pyd_rd_type,
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
    data: lt_exuname type tab_range_uname.
    data: lt_exinfty type range of t582a-infty.

* Job Submit
    data: ls_pri_params type pri_params.
    data lo_printer_cfg type ref to if_pyc_printer_cfg  .
    data: lv_jobname   type  btcjob,
          lv_jobcount  type  btcjobcnt,
          lv_timestamp type timestamp,
          lv_rpt_name  type syrepid.

* Read process instance steps
    lv_proc_inst_id = iv_pypi_id.
    try .
        lt_proc_inst_id_so = cl_pyd_fnd_aux=>set_so_fixed_value( lv_proc_inst_id ).
      catch cx_pyd_fnd.
        " do nothing. Just keep empty table.
        return.
    endtry.

    try.
        lo_fnd_factory = cl_pyd_fnd_factory=>get_instance( ).
        lo_proc_inst_aux = cl_pyc_proc_inst_aux=>get_instance( lo_fnd_factory->mo_transaction ).
        lt_step_inst = lo_proc_inst_aux->step_inst_get_list(
                          it_proc_inst_id_so = lt_proc_inst_id_so
                          iv_with_names      = abap_false ).
* Read Step Template IDs for the Process
*  Enrich Process step details, which will populate the process step start time
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
              call method cl_pyd_fnd_aux=>append_so_fixed_value
                exporting
                  iv_value = ls_step_inst_enrich-step_id
                changing
                  ct_so    = lt_step_id_so.
            when mc_cat_moni.
              lv_moni_step_id = ls_step_inst_enrich-step_id.
              call method cl_pyd_fnd_aux=>append_so_fixed_value
                exporting
                  iv_value = ls_step_inst_enrich-step_id
                changing
                  ct_so    = lt_step_id_so.
            when mc_cat_other.
* Read Process Step for Payroll Driver
              select single id into @lv_id
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
            iv_value = gcs_action-repeat " 'PYP_STS_EXE_RESET'
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
             and    reserved_id2  = lv_proc_inst_id.

      if sy-subrc eq 0.
        " Identify the Event Hadler Inactive period
        sort lt_pyd_d_al by tsl descending.
        loop at lt_pyd_d_al into ls_pyd_d_al.
          "Collect processes details and Process start time frm latest to the start
          lv_pyd_d_al_tsl = ls_pyd_d_al-tsl.
          move-corresponding ls_pyd_d_al to ls_pyd_d_al_rel.

          " Exit the loop for latest monitering step reset
          " Incase of Initial Run Start date will set to main act of
          if ls_pyd_d_al-instid eq lv_moni_step_id               " Monitering Step Reset
             and ls_pyd_d_al-trigger_rdt eq gcs_action-repeat.
            exit.
          endif.

        endloop.

      endif.
    endif.

* Start Time
    if not lv_pyd_d_al_tsl is initial.
      move lv_pyd_d_al_tsl to lv_tstamps.
* As a precautionary step add 180 seconds to cover daemon job processing period
      call function 'TIMESTAMP_DURATION_ADD'
        exporting
          timestamp_in    = lv_tstamps
          duration        = -180
        importing
          timestamp_out   = lv_tstamps
        exceptions
          timestamp_error = 1
          others          = 2.

      call function 'IB_CONVERT_FROM_TIMESTAMP'
        exporting
          i_timestamp = lv_tstamps
          i_tzone     = gc_utc
        importing
          e_datlo     = lv_start_date
          e_timlo     = lv_start_time.
    else.
      lv_start_date = syst-datum.
      lv_start_time = syst-uzeit.
    endif.

* End Time.
    get time.
    lv_end_date = syst-datum.
    lv_end_time = syst-uzeit.

* Exlude User names and Infotypes
    call method zusecl_m99_pcc_mdlog_ehiupdate=>get_ehiupd_exc_uname_infty
      importing
        et_exuname = lt_exuname
        et_exinfty = lt_exinfty.

    lv_rpt_name = mc_report_name.

*  Submit independent event handler update program job using the following parameters
* Process Instance             - Input Parameters
* Start Date and Start Time    - Derived from Process Instance
* End Date and End Time        - System Date and Time
* Exclude changes by Usernames – Read the list from TVARVC Variant table
* Exclude Infotypes            - Read the list from TVARVC Variant table
    " Prepare job name
    data lv_pyp_name type pyd_name.
    loop at lt_step_inst_enrich into ls_step_inst_enrich
      where step_id eq ls_pyd_d_al_rel-instid.
      lv_pyp_name = ls_step_inst_enrich-pyp_name.
      exit.
    endloop.

    lv_jobname = |{ lv_pyp_name+0(4) }/{ mc_short_repname }/{ sy-datum }{ sy-uzeit }|.

    "open job
    call function 'JOB_OPEN'
      exporting
        jobname          = lv_jobname
        jobclass         = 'A'
      importing
        jobcount         = lv_jobcount
      exceptions
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        others           = 4.

    "submit
    submit (lv_rpt_name)
       with p_pypiid = lv_proc_inst_id
       with p_begda  = lv_start_date
       with p_begtz  = lv_start_time
       with p_endda  = lv_end_date
       with p_endtz  = lv_end_time
*       with s_uname  = lt_exuname[]
*       with s_infty  = lt_exinfty[]
       with p_test   = abap_false
       user sy-uname
       via job lv_jobname number lv_jobcount
       and return .

    "close job
    call function 'JOB_CLOSE'
      exporting
        jobcount             = lv_jobcount
        jobname              = lv_jobname
        strtimmed            = abap_true
      exceptions
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        others               = 8.
    if sy-subrc <> 0 .
      raise exception type cx_pyc_cont
        exporting
          textid      = cx_pyc_cont=>close_job_error
          mv_jobname  = lv_jobname
          mv_jobcount = lv_jobcount.
    endif.

  endmethod.
ENDCLASS.
