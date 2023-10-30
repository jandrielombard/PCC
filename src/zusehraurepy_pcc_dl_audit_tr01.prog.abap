*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_PCC_DEL_COMPL_PI01
*&---------------------------------------------------------------------*
class lcl_application definition.
  public section.
    data:
      mo_fnd_factory type ref to cl_pyd_fnd_factory.

    constants:
      gc_low_date  type d value '19000101'.

    methods:
      "constructor
      constructor importing it_proc_id_so type /iwbep/t_cod_select_options optional
                            iv_begda      type d
                            iv_endda      type d
                            iv_variant    type raldb_vari,

      "check current user's authorization to execute the app
      check_report_auth,

      " Download Process Instances Audit Log
      download_proc_inst.
  private section.
    types: ty_t_del_completed_pi type standard table of pyc_s_del_completed_pi.

    types: begin of ty_s_log,
             log_info type icon_d,
             msgid    type msgid,
             msgno    type msgno,
             msgtx    type text120,
           end of ty_s_log.
    types: ty_t_log       type table of ty_s_log.

    data:
      mt_proc_inst     type table of pyc_s_proc_inst_res,
      mt_log           type ty_t_log,
      mt_proc_inst_del type table of pyc_s_del_completed_pi,
      mo_proc_inst_aux type ref to cl_pyc_proc_inst_aux,
      mo_pyc_rt_facade type ref to cl_pyc_rt_facade,
      mt_proc_id_so    type /iwbep/t_cod_select_options,
      mv_begda         type d,
      mv_endda         type d,
      mv_variant       type raldb_vari.

    type-pools: icon.

    methods:
      "Read process instance list.
      read_proc_inst    importing it_proc_id_so type /iwbep/t_cod_select_options
                                  iv_begda      type d
                                  iv_endda      type d
                        raising   cx_pyc_frw,

      display_process_instances.
endclass.

class lcl_application implementation.
  method constructor.
* Read Process Instances
    try.
        mo_fnd_factory   = cl_pyd_fnd_factory=>get_instance( ).
        mo_proc_inst_aux = cl_pyc_proc_inst_aux=>get_instance( io_transaction = mo_fnd_factory->mo_transaction ).
        mo_pyc_rt_facade = cl_pyc_rt_facade=>get_instance( io_transaction = mo_fnd_factory->mo_transaction ).
        mt_proc_id_so    = it_proc_id_so.
        mv_begda         = iv_begda.
        mv_endda         = iv_endda.
        mv_variant       = iv_variant.

        read_proc_inst( it_proc_id_so = mt_proc_id_so
                        iv_begda = mv_begda
                        iv_endda = mv_endda ).

      catch cx_pyc_frw into data(lx_frw).
        append value #( log_info = icon_system_cancel msgid = lx_frw->if_t100_message~t100key-msgid msgno = lx_frw->if_t100_message~t100key-msgno msgtx = lx_frw->if_message~get_text( ) ) to mt_log.
      catch cx_pyd_fnd into data(lx_fnd).
        append value #( log_info = icon_system_cancel msgid = lx_fnd->if_t100_message~t100key-msgid msgno = lx_fnd->if_t100_message~t100key-msgno msgtx = lx_fnd->if_message~get_text( ) ) to mt_log.
    endtry.

  endmethod.

  method check_report_auth.
    try.
        if cl_pyd_fnd_aux=>report_auth_check( ) = abap_false.
          message e111(pyd_fnd).
        endif.
      catch cx_pyd_fnd.
        message e111(pyd_fnd).
    endtry.
  endmethod.

  method read_proc_inst.
* Read Process isnatances for the Selection
    data: lv_no_gap        type abap_bool value abap_true,
          ls_proc_inst_alv type pyc_s_del_completed_pi,
          lv_msgtx         type string.

    "read the list of completed process instance before the given date.
    mt_proc_inst = mo_pyc_rt_facade->proc_inst_get_list(
      it_filter = value #( ( property = cl_pyc_rt_facade=>gcs_filter-proc_inst-proc_id
                             select_options = it_proc_id_so )
                         )
      iv_use_sts_join = abap_true
      iv_with_names   = abap_true
    ).

    "loop all process instances. collect qualified completed process instance for download. put completed process with gap into message.
    clear mt_proc_inst_del.
    loop at mt_proc_inst into data(ls_proc_inst)
      where execution_status eq cl_pyc_pt_sts_exe=>gcs_value-closed.

      convert time stamp ls_proc_inst-completion_ts time zone sy-zonlo
        into date data(lv_completion_date) time data(lv_completion_time).
      check lv_completion_date between mv_begda and mv_endda.

      move-corresponding ls_proc_inst to ls_proc_inst_alv.
      ls_proc_inst_alv-completion_ts_formatted = | { ls_proc_inst-completion_ts timezone = sy-zonlo timestamp = environment } |.
      ls_proc_inst_alv-info = icon_system_okay.
      insert ls_proc_inst_alv into table mt_proc_inst_del.
    endloop.

  endmethod.

  method download_proc_inst .
* Batch Job submission for Audit Trail Download.
    constants: mc_report_name   type syrepid value 'PYC_SUPPORT_DL_AUDIT_TRAIL',
               mc_short_repname type syrepid value 'AUDIT_TRAIL_DL'.

    data lr_proc_inst_id type range of pyc_d_pypi-pypi_id.
    data lt_proc_inst_id_so type /iwbep/t_cod_select_options.

* Job Submit
    data: ls_pri_params type pri_params.
    data lo_printer_cfg type ref to if_pyc_printer_cfg  .
    data: lv_jobname   type btcjob,
          lv_jobcount  type btcjobcnt,
          lv_timestamp type timestamp,
          lv_rpt_name  type syrepid.

* Collect list of Process Instances for the download
    loop at mt_proc_inst_del into data(ls_proc_inst).
      try.
          call method cl_pyd_fnd_aux=>append_so_fixed_value
            exporting
              iv_value = ls_proc_inst-pypi_id
            changing
              ct_so    = lt_proc_inst_id_so.
        catch cx_pyd_fnd .
      endtry.
    endloop.

*	Submit Audit Trail Download Program in Background to download
* the data into server directory using logical file name PCC_AL_AH
* We can pass on all selected process instances at the same time and program still creates
* separate file for each process isnatnce
    if not lt_proc_inst_id_so is initial.
      " Prepare job name
      lv_jobname = |{ mc_short_repname }/{ sy-datum }{ sy-uzeit }|.

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

      "submit standard Audit Trail download program
      move-corresponding lt_proc_inst_id_so to lr_proc_inst_id.
      submit pyc_support_dl_audit_trail
        with p_pi in lr_proc_inst_id
        with p_maxrow = 500000
        with p_dir = cl_pyc_pi_alh_aux=>gc_logical_path
        with c_pi_al = abap_true
        with c_pi = abap_true
        with c_pi_kpi = abap_true
        user sy-uname
         via job lv_jobname number lv_jobcount
         and return.

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
        message e164(pyc_cont) with lv_jobname lv_jobcount.
        exit.
      endif.
    endif.

* Report Downloaded Process Instances
    display_process_instances( ).

  endmethod.

  method display_process_instances.
* Report Display
    type-pools: slis.      " SLIS contains all the ALV data types
    data: lt_fieldcat type slis_t_fieldcat_alv,
          ls_fieldcat type slis_fieldcat_alv.
    data ls_layout type slis_layout_alv.

*  Build field catalog
    call function 'REUSE_ALV_FIELDCATALOG_MERGE'
      exporting
        i_program_name         = sy-repid
        i_structure_name       = 'PYC_S_DEL_COMPLETED_PI'
      changing
        ct_fieldcat            = lt_fieldcat
      exceptions
        inconsistent_interface = 1
        program_error          = 2
        others                 = 3.
* Lay Out
    ls_layout-zebra = abap_true.
    ls_layout-colwidth_optimize = abap_true.

* Pass data and field catalog to ALV function module to display ALV list
    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
        i_grid_title  = sy-title
        it_fieldcat   = lt_fieldcat
        is_layout     = ls_layout
      tables
        t_outtab      = mt_proc_inst_del
      exceptions
        program_error = 1
        others        = 2.

  endmethod.

endclass.
