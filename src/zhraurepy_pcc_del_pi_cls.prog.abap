*&--------------------------------------------------------------------------*
*&  Include           PYC_SUPPORT_DEL_PI_CLS
*&--------------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                      |Change Label *
*---------------------------------------------------------------------------*
*001 |09/05/2023  |1107849  |PCD-5335 PCC clean up job change |CFAK903246   *
*---------------------------------------------------------------------------*
class lcl_application definition.
  public section.
    data:
      mo_fnd_factory type ref to cl_pyd_fnd_factory.

    constants:
      gc_low_date  type d value '19000101'.

    methods:
      "constructor
      constructor importing it_proc_id_so type /iwbep/t_cod_select_options optional
                            iv_endda      type d,

      "check current user's authorization to execute the app
      check_report_auth,
*>>> Start of MOD001--
***      "display header and log controls
***      show_docking_controls.
*<<< End of MOD001--
      " Delete Process Instances
      delete_proc_inst,
      "Display Process Instances and Log
      display_process_instances.

  private section.
    types: ty_t_del_completed_pi type standard table of pyc_s_del_completed_pi.

    types: begin of ty_s_tree_node.
             include type treev_node.
             types:  text type text120,
           end of ty_s_tree_node.

    types: begin of ty_s_log,
             log_info type icon_d,
             msgid    type msgid,
             msgno    type msgno,
             msgtx    type text120,
           end of ty_s_log.

    types: ty_t_tree_node type table of ty_s_tree_node,
           ty_t_log       type table of ty_s_log.
    data:
*>>> Start of MOD001--
***      mo_header_docking type ref to cl_gui_docking_container,
***      mo_log_docking    type ref to cl_gui_docking_container,
***      mo_tree           type ref to cl_gui_simple_tree,
***      mo_alv_grid       type ref to cl_gui_alv_grid,
***      mo_log            type ref to cl_gui_alv_grid,
*<<< End of MOD001--
      mt_proc_inst     type table of pyc_s_proc_inst_res,
      mt_log           type ty_t_log,
*>>> Start of MOD001--
***      mv_node_key       type tv_nodekey,
***      mv_warning_node   type tv_nodekey,
***      mv_del_node       type tv_nodekey,
***      mv_err_node       type tv_nodekey,
***      mv_selected_node  type tv_nodekey,
***      mt_node_table     type ty_t_tree_node,
*<<< End of MOD001--
      mt_proc_inst_err type table of pyc_s_del_completed_pi,
      mt_proc_inst_del type table of pyc_s_del_completed_pi,
      mo_proc_inst_aux type ref to cl_pyc_proc_inst_aux,
      mo_pyc_rt_facade type ref to cl_pyc_rt_facade,
      mt_proc_id_so    type /iwbep/t_cod_select_options,
      mv_endda         type d.

    type-pools: icon.

    methods:
      "output controls
*>>> Start of MOD001--
***      initial_header_tree,
***      initial_alv_grid,
***      initial_log_grid,
***      get_alv_grid_layout     returning value(rs_layout) type lvc_s_layo,
***      get_log_grid_layout     returning value(rs_layout) type lvc_s_layo,
***      get_alv_grid_fcat       returning value(rt_fcat)   type lvc_t_fcat,
***      get_log_grid_fcat       returning value(rt_fcat)   type lvc_t_fcat,
*<<< End of MOD001--

      "delete and refresh process instance list.
      update_proc_inst    importing it_proc_id_so type /iwbep/t_cod_select_options
                                    iv_endda      type d
                          raising   cx_pyc_frw,
*>>> Start of MOD001--
***      delete_proc_inst,
*<<< End of MOD001--
      is_bo_proc          returning value(rv_result) type abap_bool,
      delete_proc_inst_bo importing it_proc_inst_del type ty_t_del_completed_pi.
*>>> Start of MOD001--
***      "log features
***      show_message importing iv_msgid type msgid default 'PYC_FRW'
***                             iv_msgno type msgno,
***      update_log   importing it_log type ty_t_log,
***
***      "event handlers
***      "Double click the node in header tree.
***      handle_node_double_click for event node_double_click of cl_gui_simple_tree
***        importing node_key,
***      "update toolbar
***      handle_toolbar for event toolbar of cl_gui_alv_grid
***        importing e_object e_interactive,
***      "user command:
***      "&DELETE: delete all can-be-deleted completed process instances
***      handle_user_command for event user_command of cl_gui_alv_grid
***        importing e_ucomm,
***      "list hotspot click: show log/message detail info
***      handle_hotspot_click for event hotspot_click of cl_gui_alv_grid
***        importing e_row_id e_column_id es_row_no.
*<<< End of MOD001--
endclass.

class lcl_application implementation.
  method constructor.
*>>> Start of MOD001--
***    "initialize the output controls
***    if mo_alv_grid is initial.
***      me->initial_alv_grid( ).
***    endif.
*<<< End of MOD001--
    try.
        mo_fnd_factory   = cl_pyd_fnd_factory=>get_instance( ).
        mo_proc_inst_aux = cl_pyc_proc_inst_aux=>get_instance( io_transaction = mo_fnd_factory->mo_transaction ).
        mo_pyc_rt_facade = cl_pyc_rt_facade=>get_instance( io_transaction = mo_fnd_factory->mo_transaction ).
        mt_proc_id_so    = it_proc_id_so.
        mv_endda         = iv_endda.

        update_proc_inst( it_proc_id_so = mt_proc_id_so
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
  method display_process_instances.
* Report Display
    type-pools: slis.      " SLIS contains all the ALV data types
    data: lt_fieldcat type slis_t_fieldcat_alv,
          ls_fieldcat type slis_fieldcat_alv.
    data ls_layout type slis_layout_alv.
    data: lt_events type slis_t_event,
          ls_events type slis_alv_event.

* Initialize
    call function 'REUSE_ALV_BLOCK_LIST_INIT'
      exporting
        i_callback_program = sy-repid.

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
    refresh lt_events.
    ls_events-form = 'TOP_OF_PAGE'.
    ls_events-name = 'TOP_OF_LIST'.
    append ls_events to lt_events.

    call function 'REUSE_ALV_BLOCK_LIST_APPEND'
      exporting
        is_layout                  = ls_layout
        it_fieldcat                = lt_fieldcat
        i_tabname                  = 'PYPI'
        it_events                  = lt_events
      tables
        t_outtab                   = mt_proc_inst_del
      exceptions
        program_error              = 1
        maximum_of_appends_reached = 2
        others                     = 3.

* Log Details
* Build field catalog for Log table
    refresh: lt_fieldcat, lt_events.
    call function 'REUSE_ALV_FIELDCATALOG_MERGE'
      exporting
        i_program_name         = sy-repid
        i_structure_name       = 'ZHRAU_ST_PCC_PI_DEL_LOG'
      changing
        ct_fieldcat            = lt_fieldcat
      exceptions
        inconsistent_interface = 1
        program_error          = 2
        others                 = 3.

    call function 'REUSE_ALV_BLOCK_LIST_APPEND'
      exporting
        is_layout                  = ls_layout
        it_fieldcat                = lt_fieldcat
        i_tabname                  = 'DELLOG'
        it_events                  = lt_events
      tables
        t_outtab                   = mt_log
      exceptions
        program_error              = 1
        maximum_of_appends_reached = 2
        others                     = 3.

* Display Output
    call function 'REUSE_ALV_BLOCK_LIST_DISPLAY'.

  endmethod.
*>>> Start of MOD001--
***method show_docking_controls.
***    "header for group of completed process instances selected.
***    if mo_tree is initial.
***      initial_header_tree( ).
***    endif.
***
***    "log and messages.
***    if mo_log is initial.
***      initial_log_grid( ).
***    endif.
***  endmethod.

***  method initial_header_tree.
***    data:
***      lt_events    type cntl_simple_events,
***      ls_event     type cntl_simple_event,
***      lv_node_text type text120.
***
***    create object mo_header_docking
***      exporting
***        repid = sy-repid
***        dynnr = sy-dynnr
***        side  = cl_gui_docking_container=>dock_at_top
***        ratio = 10.
***
***    create object mo_tree
***      exporting
***        parent              = mo_header_docking
***        node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single.
***
***    ls_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
***    ls_event-appl_event = 'X'. " process PAI if event occurs
***    append ls_event to lt_events.
***
***    call method mo_tree->set_registered_events
***      exporting
***        events                    = lt_events
***      exceptions
***        cntl_error                = 1
***        cntl_system_error         = 2
***        illegal_event_combination = 3.
***    if sy-subrc <> 0.
***      message e014(pyc_frw) .
***    endif.
***
****   assign event handlers in the application class to each desired event
***    set handler handle_node_double_click for mo_tree.
***
***    clear mt_node_table.
***
***    mv_node_key = mv_del_node = mv_node_key + 1.
***    append value #( node_key = mv_node_key text = text-001 hidden = abap_false disabled = abap_false n_image  = icon_list ) to mt_node_table.
***
***    mv_node_key = mv_err_node = mv_node_key + 1.
***    append value #( node_key = mv_node_key text = text-002 hidden = abap_false disabled = abap_false n_image  = icon_message_orphaned ) to mt_node_table.
***
***    mo_tree->add_nodes(
***      exporting
***        table_structure_name           = 'PYC_S_DYN_PRC_TREE_NODE'
***        node_table                     = mt_node_table ).
***
***    handle_node_double_click( node_key = mv_del_node ).
***  endmethod.

***  method initial_alv_grid.
***    data: lt_fcat   type lvc_t_fcat,
***          ls_layout type lvc_s_layo.
***
***    create object mo_alv_grid
***      exporting
***        i_parent = cl_gui_container=>default_screen.
***
***    set handler handle_toolbar for mo_alv_grid.
***    set handler handle_user_command for mo_alv_grid.
***    set handler handle_hotspot_click for mo_alv_grid.
***
***    ls_layout = get_alv_grid_layout( ).
***    lt_fcat   = get_alv_grid_fcat( ).
***    mo_alv_grid->set_table_for_first_display(
***      exporting is_layout = ls_layout
***      changing  it_outtab = mt_proc_inst_del
***                it_fieldcatalog = lt_fcat ).
***
***    call method cl_gui_cfw=>flush.
***
***  endmethod.
***
***  method initial_log_grid.
***    create object mo_log_docking
***      exporting
***        repid = sy-repid
***        dynnr = sy-dynnr
***        side  = cl_gui_docking_container=>dock_at_bottom
***        ratio = 40.
***
***    create object mo_log
***      exporting
***        i_parent = mo_log_docking.
***
***    set handler handle_hotspot_click for mo_log.
***
***    message w095(pyc_frw) into data(lv_dummy).
***    append value #( log_info = icon_system_help msgno = '095' msgtx = lv_dummy ) to mt_log.
***    update_log( mt_log ).
***
***    call method cl_gui_cfw=>flush.
***  endmethod.
***
***  method get_alv_grid_fcat.
***    data: lv_col_pos type i.
***    clear rt_fcat.
***
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'INFO' icon = abap_true scrtext_l = text-007 hotspot = abap_true ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'PYPI_ID' ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'PYP_NAME' outputlen = '50' scrtext_l = text-003 ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'PROC_INST_NAME' outputlen = '30' scrtext_l = text-004 ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'TIME_SEL_PAR_VAL' outputlen = '30' scrtext_l = text-005 ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'TIME_SEL_PAR_VAL_BEGDA') to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'TIME_SEL_PAR_VAL_ENDDA' ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'OCRSN' outputlen = '15' ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'BONDT' outputlen = '15' ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'PAYID' outputlen = '15' ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'COMPLETION_TS_FORMATTED' outputlen = '30' scrtext_l = text-006 ) to rt_fcat.
***
***    call function 'LVC_FIELDCATALOG_MERGE'
***      exporting
***        i_structure_name = 'PYC_S_DEL_COMPLETED_PI'
***      changing
***        ct_fieldcat      = rt_fcat.
***
***    loop at rt_fcat assigning field-symbol(<fs_fcat>).
***      if <fs_fcat>-col_pos > lv_col_pos.
***        <fs_fcat>-no_out = abap_true.
***      endif.
***    endloop.
***  endmethod.
***
***  method get_log_grid_fcat.
***    data: lv_col_pos type i.
***    clear rt_fcat.
***
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'LOG_INFO' icon = abap_true scrtext_l = text-007 hotspot = abap_true ) to rt_fcat.
***    add 1 to lv_col_pos.
***    append value #( col_pos = lv_col_pos fieldname = 'MSGTX' scrtext_l = text-008 ) to rt_fcat.
***
***    call function 'LVC_FIELDCATALOG_MERGE'
***      exporting
***        i_structure_name = 'PYC_S_DEL_COMPLETED_PI'
***      changing
***        ct_fieldcat      = rt_fcat.
***
***    loop at rt_fcat assigning field-symbol(<fs_fcat>).
***      if <fs_fcat>-col_pos > lv_col_pos.
***        <fs_fcat>-no_out = abap_true.
***      endif.
***    endloop.
***  endmethod.
***
***  method get_alv_grid_layout.
***    clear rs_layout.
***    data: lv_cdate_string type pyd_name_db.
***
***    rs_layout-stylefname = 'CELLSTYLES'.
***    rs_layout-zebra = abap_true.
***    rs_layout-no_rowmark = abap_true.
***  endmethod.
***
***  method get_log_grid_layout.
***    clear rs_layout.
***    data: lv_cdate_string type pyd_name_db.
***
***    rs_layout-stylefname = 'CELLSTYLES'.
***    rs_layout-zebra = abap_true.
***    rs_layout-no_rowmark = abap_true.
***    rs_layout-no_toolbar = abap_true.
***  endmethod.
*<<< End of MOD001--
  method update_proc_inst.
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

    "loop all process instances. collect qualified completed process instance for deletion. put completed process with gap into message.
    clear mt_proc_inst_del.
*>>> Start of MOD001--
***    loop at mt_proc_inst into data(ls_proc_inst) where time_sel_par_val_endda between gc_low_date and mv_endda or execution_status <> cl_pyc_pt_sts_exe=>gcs_value-closed.
*<<< End of MOD001--
*>>> Start of MOD001++

    loop at mt_proc_inst into data(ls_proc_inst)
       where execution_status eq cl_pyc_pt_sts_exe=>gcs_value-closed.

      convert time stamp ls_proc_inst-completion_ts time zone sy-zonlo
       into date data(lv_completion_date) time data(lv_completion_time).

      check lv_completion_date between gc_low_date and mv_endda.
*<<< End of MOD001--
*>>> Start of MOD001--
***      "if not completed, set the no gap flag to false. completed process found afterwards, will be marked with error.
***      "SPECIAL CASE: do not check gap for off-cycle.
***      if ls_proc_inst-execution_status <> cl_pyc_pt_sts_exe=>gcs_value-closed and
***         mo_proc_inst_aux->proc_inst_is_oc( iv_proc_inst_id = ls_proc_inst-pypi_id ) = abap_false.
***        lv_no_gap = abap_false.
***        "completed with no gap, can be deleted.
***        "find shared validation rules, if found show warning that validation rules may not be deleted.
***      elseif ls_proc_inst-execution_status = cl_pyc_pt_sts_exe=>gcs_value-closed and lv_no_gap = abap_true.
***        "further check the validation rules. if shared with other process instances, put a warning.
*<<< End of MOD001--
      move-corresponding ls_proc_inst to ls_proc_inst_alv.
      ls_proc_inst_alv-completion_ts_formatted = | { ls_proc_inst-completion_ts timezone = sy-zonlo timestamp = environment } |.
      ls_proc_inst_alv-info = icon_system_okay.
      insert ls_proc_inst_alv into table mt_proc_inst_del.
*>>> Start of MOD001--
***       "completed with gap, cannot be deleted.
***      elseif ls_proc_inst-execution_status = cl_pyc_pt_sts_exe=>gcs_value-closed and lv_no_gap = abap_false.
***        move-corresponding ls_proc_inst to ls_proc_inst_alv.
***        ls_proc_inst_alv-completion_ts_formatted = | { ls_proc_inst-completion_ts timezone = sy-zonlo timestamp = environment } |.
***        message e096(pyc_frw) into lv_msgtx.
***        ls_proc_inst_alv-msgno = '096'.
***        ls_proc_inst_alv-info = icon_system_cancel.
***        insert ls_proc_inst_alv into table mt_proc_inst_err.
***      endif.
*<<< End of MOD001--
    endloop.
*>>> Start of MOD001--
***    mo_alv_grid->refresh_table_display( ).
***    cl_gui_cfw=>flush( ).
*<<< End of MOD001--
  endmethod.

  method delete_proc_inst.
    data: lt_gen_log       type cl_pyc_proc_inst_aux=>ty_t_gen_log,
          lo_pi_dt         type ref to if_pyc_dt_proc_instance,
          ls_log           type ty_s_log,
          lt_log           type ty_t_log,
          lt_proc_inst_del type table of pyc_s_del_completed_pi,
          lv_is_ok         type abap_bool,
          lv_answer(1).

    data: lv_index type sy-tabix.               "MOD001++

    "show warning message
    message i095(pyc_frw) into data(lv_dummy).
    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = text-d01
        text_question         = lv_dummy
        display_cancel_button = abap_false
      importing
        answer                = lv_answer
      exceptions
        text_not_found        = 01
        others                = 02.

    check lv_answer = '1'.    "Yes

    try.
        loop at mt_proc_inst_del into data(ls_proc_inst).
          lv_index = sy-tabix.                                     "MOD001++

          lv_is_ok = abap_true.
          lo_pi_dt = cl_pyc_dt_proc_instance_base=>get_dt_instance(
                       imp_begda      = ls_proc_inst-time_sel_par_val_begda
                       imp_endda      = ls_proc_inst-time_sel_par_val_endda
                       imp_proc_id    = ls_proc_inst-proc_id
                       imp_no_flush   = abap_true
                       io_transaction = mo_fnd_factory->mo_transaction
                     ).
          lt_gen_log = lo_pi_dt->delete_instance_from_db(
                         imp_pypi_id           = ls_proc_inst-pypi_id " Payroll Process Instance ID
                         iv_enforce_generation = abap_true            " Enforce Deletion?
                       ).

          loop at lt_gen_log into data(ls_gen_log).
            clear ls_log.
            case ls_gen_log-severity.
              when 'A'.
                ls_log-log_info = icon_failure.
                lv_is_ok = abap_false.
              when 'W'.
                ls_log-log_info = icon_led_yellow .
              when 'E'.
                ls_log-log_info = icon_led_red .
                lv_is_ok = abap_false.
              when others.
                ls_log-log_info = icon_led_green.
            endcase.
            ls_log-msgno = ls_gen_log-msgnr.
            message id ls_gen_log-arbgb type ls_gen_log-severity number ls_gen_log-msgnr
               with ls_gen_log-value1 ls_gen_log-value2  ls_gen_log-value3 ls_gen_log-value4
               into ls_log-msgtx.
            append ls_log to lt_log.
          endloop.
          if lv_is_ok = abap_true.
            insert ls_proc_inst into table lt_proc_inst_del.
            message i032(pyc_frw) into ls_proc_inst-msgtx.    "MOD001++
          else.                                               "MOD001++
            message i096(pyc_frw) into ls_proc_inst-msgtx.    "MOD001++
          endif.

          modify mt_proc_inst_del                                     "MOD001++
           from ls_proc_inst index lv_index transporting msgno msgtx. "MOD001++
        endloop.

        delete_proc_inst_bo( lt_proc_inst_del ). "delete bopf artifacts
*>>> Start of MOD001--
***        update_proc_inst( it_proc_id_so = mt_proc_id_so
***                          iv_endda = mv_endda ).

***        update_log( lt_log ).
*<<< End of MOD001--

*>>> Start of MOD001++
        mt_log = lt_log.                                       "MOD001++
*<<< End of MOD001++

      catch cx_pyc_frw into data(lx_frw).
        append value #( log_info = icon_system_cancel msgid = lx_frw->if_t100_message~t100key-msgid msgno = lx_frw->if_t100_message~t100key-msgno msgtx = lx_frw->if_message~get_text( ) ) to mt_log.
      catch cx_pyd_fnd into data(lx_fnd).
        append value #( log_info = icon_system_cancel msgid = lx_fnd->if_t100_message~t100key-msgid msgno = lx_fnd->if_t100_message~t100key-msgno msgtx = lx_fnd->if_message~get_text( ) ) to mt_log.
      catch cx_pyc_p_info_handler into data(lx_pih).
        append value #( log_info = icon_system_cancel msgid = lx_pih->if_t100_message~t100key-msgid msgno = lx_pih->if_t100_message~t100key-msgno msgtx = lx_pih->if_message~get_text( ) ) to mt_log.
    endtry.
  endmethod.

  method is_bo_proc.
    select single * from pyc_d_pyp where id in @mt_proc_id_so into @data(ls_pyp).
    if sy-subrc = 0.
      rv_result = ls_pyp-is_ns_protected.
    endif.
  endmethod.

  method delete_proc_inst_bo.
    data: lo_pmr_dt      type ref to cl_pyc_dt_simp_conf_proc,
          lt_key         type /bobf/t_frw_key,
          lt_pmr_rec_del type pyc_t_bo_pmr_recurrence_c,
          lt_rec_so      type /iwbep/t_cod_select_options.
    if is_bo_proc( ) = abap_true.
      loop at it_proc_inst_del into data(ls_proc_inst).
        append value #( sign = 'I' option = 'EQ' low = ls_proc_inst-time_sel_par_val ) to lt_rec_so.
      endloop.
     if lt_rec_so is not initial.                    "Mod-001++
      select r~* from pyc_d_bo_pmr as p
        inner join pyc_d_bo_pmrrecu as r on r~parent_key = p~db_key
        where p~referenceid in @mt_proc_id_so and r~value in @lt_rec_so
         into table @data(lt_pmrrecu).

      delete pyc_d_bo_pmrrecu from table lt_pmrrecu.
     endif.                                          "Mod-001++
    endif.
  endmethod.
*>>> Start of MOD001--
***  method show_message.
***    data : lt_dselc type table of dselc,
***           lt_dval  type table of dval,
***           lv_msgid type msgid.
***
***    if iv_msgid is initial.
***      lv_msgid = 'PYC_FRW'. "default message class.
***    else.
***      lv_msgid = iv_msgid.
***    endif.
***
***    call function 'HELP_START'
***      exporting
***        help_infos   = value help_info( call = 'D' messageid = lv_msgid messagenr = iv_msgno docuid = 'NA' )
***      tables
***        dynpselect   = lt_dselc
***        dynpvaluetab = lt_dval.
***  endmethod.
***
***  method update_log.
***    mt_log = it_log.
***    data(lt_fcat) = get_log_grid_fcat( ).
***    mo_log->set_table_for_first_display(
***      exporting is_layout = get_log_grid_layout( )
***      changing  it_outtab = mt_log
***                it_fieldcatalog = lt_fcat ).
***  endmethod.
***
***  method handle_node_double_click.
***    data(lt_fcat) = get_alv_grid_fcat( ).
***    data(ls_layout) = get_alv_grid_layout( ).
***
***    mv_selected_node = node_key.
***
***    case node_key.
***      when mv_del_node.
***        "display can be deleted process instances.
***        mo_alv_grid->set_table_for_first_display(
***          exporting is_layout = ls_layout
***          changing  it_outtab = mt_proc_inst_del
***                    it_fieldcatalog = lt_fcat ).
***
***      when mv_err_node.
***        "display cannot be deleted process instances with messages.
***        mo_alv_grid->set_table_for_first_display(
***          exporting is_layout = ls_layout
***          changing  it_outtab = mt_proc_inst_err
***                    it_fieldcatalog = lt_fcat ).
***
***      when others.
***        "not support.
***    endcase.
***
***    mo_alv_grid->refresh_table_display( ).
***    cl_gui_cfw=>flush( ).
***  endmethod.
***
***  method handle_toolbar.
***    "only enable for can be deleted list.
***    check mv_selected_node = mv_del_node.
***
***    data: lv_delete_is_disabled type abap_bool value abap_false.
***
***    "separator
***    insert value #( butn_type = 3 ) into e_object->mt_toolbar index 1.
***
***    "button DELETE ALL, disable if list is empty.
***    if mt_proc_inst_del is initial.
***      lv_delete_is_disabled = abap_true.
***    endif.
***    insert value #( function = '&DELETE' icon = icon_delete text = text-b01 quickinfo = text-b01 disabled = lv_delete_is_disabled ) into e_object->mt_toolbar index 1.
***
***    "disable filtering features for can-be-deleted list.
***    loop at e_object->mt_toolbar assigning field-symbol(<fs_tb>) where function = '&MB_FILTER'.
***      <fs_tb>-disabled = abap_true.
***    endloop.
***
***  endmethod.
***
***  method handle_user_command.
***    case e_ucomm.
***      when '&DELETE'.
***        delete_proc_inst( ).
***      when others.
***        "user command is not supported.
***    endcase.
***  endmethod.
***
***  method handle_hotspot_click.
***    case e_column_id.
***      when 'INFO'.      "info in process instance list
***        case mv_selected_node.
***          when mv_err_node.   "info of the cannot-be-deleted completed process instance
***            read table mt_proc_inst_err index e_row_id into data(ls_pi_err).
***            "active or upcoming process instances exists before the current one. Deletion is not allowed.
***            show_message( ls_pi_err-msgno ).
***          when others.
***            "no other cases.
***        endcase.
***      when 'LOG_INFO'.  "info in log control, display long text of the messages.
***        read table mt_log index e_row_id into data(ls_log).
***        if ls_log-log_info = icon_system_help or ls_log-log_info = icon_system_cancel.
***          show_message( iv_msgid = ls_log-msgid
***                        iv_msgno = ls_log-msgno ).
***        endif.
***      when others.
***        "not supported
***    endcase.
***  endmethod.
*<<< End of MOD001--
endclass.
