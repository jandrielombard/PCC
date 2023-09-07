class zcl_m99_pcc_dl_audit_trail definition
  public
  final
  create public .

  public section.

    constants c_utc type ttzz-tzone value 'UTC' ##NO_TEXT.

    class-methods enrich_process_log
      importing
        !it_pi_al       type zhrauttpy_proc_inst_al
      returning
        value(rt_pi_ph) type zhrauttpy_proc_inst_ph .
    class-methods fill_message_parameter
      importing
        !value                   type tvarv_val
        !date                    type sydatum
        !datetime                type timestamp
        !currency                type waers
        !unit                    type msehi
      exporting
        !message_parameter_value type syst_msgv .
    class-methods convert_process_log_tsl
      importing
        !it_pi_ph              type zhrauttpy_proc_inst_ph
      returning
        value(rt_pi_ph_export) type zhrauttpy_proc_inst_ph_export .
    class-methods convert_alert_log_tsl
      importing
        !it_pi_alh              type pyc_t_pi_alh_csv
      returning
        value(rt_pi_alh_export) type zhrauttpy_pi_alh_csv_export .
    class-methods convert_analytics_log_tsl
      importing
        !it_pi_kpi_al              type zhrauttpy_kpi_al
      returning
        value(rt_pi_kpi_al_export) type zhrauttpy_kpi_al_export .
    class-methods convert_timestamp_to_str
      importing
        !iv_time_stamp   type rs_timestmp
        !iv_tz           type ttzz-tzone
      returning
        value(rv_output) type string .
    class-methods get_proc_inst_atachements
      importing
        !iv_pi          type pyc_proc_inst_id
        !it_process_log type cl_pyc_cont_003_mpc=>tt_processlog optional
        !it_alert_item  type cl_pyc_cont_003_mpc=>tt_alertitem optional
      exporting
        !et_attachment  type cl_pyc_process_manager_mpc=>tt_attachment .
    class-methods read_attachments
      importing
        !it_filter     type /iwbep/t_mgw_select_option
      exporting
        !et_attachment type cl_pyc_process_manager_mpc=>tt_attachment .
    class-methods add_filter
      importing
        !iv_property      type string
        !it_select_option type /iwbep/t_cod_select_options
      changing
        !ct_filter        type /iwbep/t_mgw_select_option .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_M99_PCC_DL_AUDIT_TRAIL IMPLEMENTATION.


  method add_filter.
* Add entry to Filter parameters
    data: ls_filter type /iwbep/s_mgw_select_option.

    if iv_property is initial.
      return.
    endif.

    ls_filter-property = iv_property.
    ls_filter-select_options = it_select_option.
    append ls_filter to ct_filter.

  endmethod.


  method convert_alert_log_tsl.
    data: ls_pi_alh        like line of it_pi_alh,
          ls_pi_alh_export like line of rt_pi_alh_export.
    data: lv_filler type string.
    data: lv_timestamp type  rs_timestmp.

* Convert Process log Time to display format
    loop at it_pi_alh into ls_pi_alh.
      move-corresponding ls_pi_alh to ls_pi_alh_export.
      "TSL_CHAR
      split ls_pi_alh-tsl_char at '.' into ls_pi_alh-tsl_char lv_filler.
      lv_timestamp = ls_pi_alh-tsl_char.
      call method zcl_m99_pcc_dl_audit_trail=>convert_timestamp_to_str
        exporting
          iv_time_stamp = lv_timestamp
          iv_tz         = zcl_m99_pcc_dl_audit_trail=>c_utc
        receiving
          rv_output     = ls_pi_alh_export-tsl_char.

      "DATE_TIME_UTC
      lv_timestamp = ls_pi_alh-date_time_utc.
      call method zcl_m99_pcc_dl_audit_trail=>convert_timestamp_to_str
        exporting
          iv_time_stamp = lv_timestamp
          iv_tz         = zcl_m99_pcc_dl_audit_trail=>c_utc
        receiving
          rv_output     = ls_pi_alh_export-date_time_utc.

      "DATE_TIME_USER
      lv_timestamp = ls_pi_alh-tsl_char.
      call method zcl_m99_pcc_dl_audit_trail=>convert_timestamp_to_str
        exporting
          iv_time_stamp = lv_timestamp
          iv_tz         = ls_pi_alh-user_tzone
        receiving
          rv_output     = ls_pi_alh_export-date_time_user.

      append ls_pi_alh_export to rt_pi_alh_export.
    endloop.

  endmethod.


  method convert_analytics_log_tsl.
* IT_PI_KPI_AL
    data: ls_pi_kpi_al        like line of it_pi_kpi_al,
          ls_pi_kpi_al_export like line of rt_pi_kpi_al_export.
    data: lv_filler type string.
    data: lv_tsl_char  type pyc_tsl_char,
          lv_timestamp type  rs_timestmp.

* Convert Process log Time to display format
    loop at it_pi_kpi_al into ls_pi_kpi_al.
      move-corresponding ls_pi_kpi_al to ls_pi_kpi_al_export.
      "TSL
      lv_tsl_char = ls_pi_kpi_al-tsl.
      split lv_tsl_char at '.' into lv_tsl_char lv_filler.
      lv_timestamp = lv_tsl_char.
      call method zcl_m99_pcc_dl_audit_trail=>convert_timestamp_to_str
        exporting
          iv_time_stamp = lv_timestamp
          iv_tz         = zcl_m99_pcc_dl_audit_trail=>c_utc
        receiving
          rv_output     = ls_pi_kpi_al_export-tsl.

      append ls_pi_kpi_al_export to rt_pi_kpi_al_export.
    endloop.

  endmethod.


  method convert_process_log_tsl.
    data: ls_pi_ph        like line of it_pi_ph,
          ls_pi_ph_export like line of rt_pi_ph_export.
    data: lv_filler type string.
    data: lv_timestamp type  rs_timestmp.

* Convert Process log Time to display format
    loop at it_pi_ph into ls_pi_ph.
      move-corresponding ls_pi_ph to ls_pi_ph_export.
      "TSL
      split ls_pi_ph-tsl_char at '.' into ls_pi_ph-tsl_char lv_filler.
      lv_timestamp = ls_pi_ph-tsl_char.
      call method zcl_m99_pcc_dl_audit_trail=>convert_timestamp_to_str
        exporting
          iv_time_stamp = lv_timestamp
          iv_tz         = zcl_m99_pcc_dl_audit_trail=>c_utc
        receiving
          rv_output     = ls_pi_ph_export-tsl.

      "DATE_TIME_UTC
      lv_timestamp = ls_pi_ph-date_time_utc.
      call method zcl_m99_pcc_dl_audit_trail=>convert_timestamp_to_str
        exporting
          iv_time_stamp = lv_timestamp
          iv_tz         = zcl_m99_pcc_dl_audit_trail=>c_utc
        receiving
          rv_output     = ls_pi_ph_export-date_time_utc.

      "DATE_TIME_USER
      lv_timestamp = ls_pi_ph-tsl_char.
      call method zcl_m99_pcc_dl_audit_trail=>convert_timestamp_to_str
        exporting
          iv_time_stamp = lv_timestamp
          iv_tz         = ls_pi_ph-user_tzone
        receiving
          rv_output     = ls_pi_ph_export-date_time_user.

      append ls_pi_ph_export to rt_pi_ph_export.
    endloop.
  endmethod.


  method convert_timestamp_to_str.
* Convert Time stamp to String
    convert time stamp iv_time_stamp time zone iv_tz
            into date data(dat) time data(tim).

    rv_output = |{ dat date = user } { tim time = iso }|.
  endmethod.


  method enrich_process_log.
* Enrich Process log in line with UI Process log Download
    data:
      lt_filter        type /iwbep/t_mgw_select_option,
      lt_filter_pypi   type /iwbep/t_mgw_select_option,
      lt_pypi_so       type /iwbep/t_cod_select_options,
      ls_filter        type /iwbep/s_mgw_select_option,
      lt_search_so     type /iwbep/t_cod_select_options,
      lt_source_key    type /iwbep/t_mgw_tech_pairs,
      ls_source_key    like line of lt_source_key,
      lo_pyc_rt_facade type ref to cl_pyc_rt_facade,
      ls_pi_ph         like line of rt_pi_ph,
      lx_pyc_frw       type ref to cx_pyc_frw,
      lx_pyc_cont      type ref to cx_pyc_cont,
      lx_pyd_fnd       type ref to cx_pyd_fnd,
      ls_pi_al         type cl_pyc_rt_facade=>ty_s_proc_inst_al,
      lt_pi_al         type cl_pyc_rt_facade=>ty_t_proc_inst_al,
      lv_msgv1         type syst_msgv,
      lv_msgv2         type syst_msgv,
      lv_msgv3         type syst_msgv,
      lv_msgv4         type syst_msgv.

    field-symbols:
      <ls_pi_phset> type cl_pyc_cont_003_mpc=>tt_processlogdownload.

    types: begin of ty_proc,
             pypi_id type pyc_proc_inst_id,
             proc_id type pyc_proc_id,
             name    type pyd_name,
           end of ty_proc.
    data: lt_proc  type table of ty_proc.
    data: ls_proc  type ty_proc.

    clear: rt_pi_ph.

    " Constructor facade filters from source keys of navigation
    loop at it_pi_al into ls_pi_al.
      clear ls_filter.
      ls_filter-property = cl_pyc_rt_facade=>gcs_filter-step_inst_al-proc_inst_id. " 'ProcessInstanceID'
      append value #( sign = 'I' option = 'EQ' low = ls_pi_al-pypi_id ) to ls_filter-select_options.
      append ls_filter to lt_filter.
      lt_pypi_so = ls_filter-select_options.
      append ls_filter to lt_filter_pypi.
      exit.
    endloop.
*
    if lt_filter_pypi is not initial.
      select a~pypi_id a~proc_id b~name from pyc_d_pypt as b
      inner join pyc_d_pypi as a
      on b~id = a~proc_id
      into table lt_proc
      where a~pypi_id in lt_pypi_so
      and b~sprsl = sy-langu.                          "#EC CI_BUFFJOIN
    endif.

    loop at it_pi_al into ls_pi_al.
      move-corresponding ls_pi_al to ls_pi_ph.

      read table lt_proc into ls_proc with key pypi_id = ls_pi_al-pypi_id.
      if sy-subrc = 0.
        ls_pi_ph-pypi_name = ls_proc-name.
      endif.
      " Time zone for current user, i.g. UTC+8
      cl_abap_tstmp=>move(
        exporting
          tstmp_src  = ls_pi_al-tsl
        importing
          tstmp_tgt  = ls_pi_ph-date_time_utc ).

      convert time stamp ls_pi_ph-date_time_utc time zone sy-zonlo
        into date data(lv_date) time data(lv_time).

      convert date lv_date time lv_time into time stamp data(lv_ts_user)
        time zone 'UTC   '.

      " tbd...remove..!!
      " convert TSL to TSL char type for downloading
      ls_pi_ph-tsl_char = ls_pi_al-tsl.
      ls_pi_ph-tsl_char_user = ls_pi_al-tsl.

      ls_pi_ph-date_time_user = lv_ts_user.
      ls_pi_ph-user_tzone = sy-zonlo.

      call method zcl_m99_pcc_dl_audit_trail=>fill_message_parameter
        exporting
          value                   = ls_pi_al-value1
          date                    = ls_pi_al-datum1
          datetime                = ls_pi_al-date_time1
          currency                = ls_pi_al-currency1
          unit                    = ls_pi_al-unit1
        importing
          message_parameter_value = lv_msgv1.
      call method zcl_m99_pcc_dl_audit_trail=>fill_message_parameter
        exporting
          value                   = ls_pi_al-value2
          date                    = ls_pi_al-datum2
          datetime                = ls_pi_al-date_time2
          currency                = ls_pi_al-currency2
          unit                    = ls_pi_al-unit2
        importing
          message_parameter_value = lv_msgv2.
      call method zcl_m99_pcc_dl_audit_trail=>fill_message_parameter
        exporting
          value                   = ls_pi_al-value3
          date                    = ls_pi_al-datum3
          datetime                = ls_pi_al-date_time3
          currency                = ls_pi_al-currency3
          unit                    = ls_pi_al-unit3
        importing
          message_parameter_value = lv_msgv3.
      call method zcl_m99_pcc_dl_audit_trail=>fill_message_parameter
        exporting
          value                   = ls_pi_al-value4
          date                    = ls_pi_al-datum4
          datetime                = ls_pi_al-date_time4
          currency                = ls_pi_al-currency4
          unit                    = ls_pi_al-unit4
        importing
          message_parameter_value = lv_msgv4.

      if ls_pi_ph-header_text is not initial.
        replace '&1' in ls_pi_ph-header_text with lv_msgv1.
        replace '&2' in ls_pi_ph-header_text with lv_msgv2.
        replace '&3' in ls_pi_ph-header_text with lv_msgv3.
        replace '&4' in ls_pi_ph-header_text with lv_msgv4.
      endif.

      append ls_pi_ph to rt_pi_ph.
      clear ls_pi_ph.
    endloop.

  endmethod.


  method fill_message_parameter.
* Fill Message with Parameter
    if value is not initial.
      if currency is not initial.
        write value to message_parameter_value currency currency.
      else.
        if unit is not initial.
          write value to message_parameter_value unit unit.
        else.
          write value to message_parameter_value.
        endif.
      endif.
    else.
      if date is not initial.
        write date to message_parameter_value.
      else.
        if datetime is not initial.
          write datetime to message_parameter_value.
        endif.
      endif.
    endif.

  endmethod.


  method get_proc_inst_atachements.
* Read Process Instance ID Attachments
    data: lt_attachment    type cl_pyc_cont_003_mpc=>tt_attachment,
          ls_attachment    type cl_pyc_cont_003_mpc=>ts_attachment,
          lv_attachment_id type pyd_attachment_id,
          ls_process_log   type cl_pyc_cont_003_mpc=>ts_processlog,
          lo_pyc_rt_facade type ref to cl_pyc_rt_facade,
          lx_pyc_frw       type ref to cx_pyc_frw,
          lt_filter_facade type /iwbep/t_mgw_select_option.
    data: lx_pyd_exc        type ref to cx_pyd_fnd,
          lx_pyc_cont       type ref to cx_pyc_cont,
          lo_cont_rt_facade type ref to cl_pyc_cont_rt_facade,
          lt_filter         type /iwbep/t_mgw_select_option,
          ls_filter         type /iwbep/s_mgw_select_option.
    data: lt_alert_item	type cl_pyc_cont_003_mpc=>tt_alertitem,
          ls_alert_item type cl_pyc_cont_003_mpc=>ts_alertitem.
    data: lv_attachment_exists_fl type boolean.

    try .
        if not it_alert_item is initial.
          lt_alert_item = it_alert_item.
        else.
          lo_cont_rt_facade = cl_pyc_cont_rt_facade=>get_instance( ).

          "add process instance for authorization check
          add_filter( exporting iv_property      = cl_pyc_cont_003_dpc_ext=>gcs_attributes-process_instance_id
                                it_select_option = value #( ( sign = 'I' option = 'EQ' low = iv_pi ) )
                      changing  ct_filter        = lt_filter ).

          lo_cont_rt_facade->v3_pi_alert_log_new_get_list(
                  exporting it_filter = lt_filter
                  importing et_alert_log = lt_alert_item ).
        endif.
        sort lt_alert_item  by pypi_id check_id.

* Read attachments for the Process log and
        lo_pyc_rt_facade = cl_pyc_rt_facade=>get_instance( ).
        "add process instance for authorization check
        add_filter( exporting iv_property      = cl_pyc_cont_003_dpc_ext=>gcs_attributes-process_instance_id
                              it_select_option = value #( ( sign = 'I' option = 'EQ' low = iv_pi ) )
                    changing  ct_filter        = lt_filter_facade ).

        clear: lv_attachment_exists_fl.
        loop at it_process_log into ls_process_log
            where attachment_id is not initial.
          lv_attachment_id = ls_process_log-attachment_id.

          add_filter( exporting iv_property      = cl_pyc_cont_003_dpc_ext=>gcs_attributes-attachment_id
                                it_select_option = value #( ( sign = 'I' option = 'EQ' low = lv_attachment_id ) )
                      changing  ct_filter        = lt_filter_facade ).
          lv_attachment_exists_fl = abap_true.
        endloop.

        loop at lt_alert_item into ls_alert_item
           where attachment_id is not initial.
          lv_attachment_id = ls_alert_item-attachment_id.

          add_filter( exporting iv_property      = cl_pyc_cont_003_dpc_ext=>gcs_attributes-attachment_id
                                it_select_option = value #( ( sign = 'I' option = 'EQ' low = lv_attachment_id ) )
                      changing  ct_filter        = lt_filter_facade ).
          lv_attachment_exists_fl = abap_true.
        endloop.

        if lv_attachment_exists_fl eq abap_true.
          zcl_m99_pcc_dl_audit_trail=>read_attachments( exporting it_filter     = lt_filter_facade
                                                        importing et_attachment = lt_attachment ).
        endif.

        loop at lt_attachment into ls_attachment.
          " prefix Date Time to file name for process log attachments
          read table it_process_log into ls_process_log
            with key attachment_id = ls_attachment-attach_id.
          if sy-subrc eq 0.
            ls_attachment-file_name = |{ ls_process_log-utc_date_time }_{ ls_attachment-file_name }|.
          else.
            " prefix Result ID to file name for alert log attachments
            read table lt_alert_item into ls_alert_item
            with key attachment_id = ls_attachment-attach_id.
            if sy-subrc eq 0.
              ls_attachment-file_name = |{ ls_alert_item-roid }_{ ls_attachment-file_name }|.
            endif.
          endif.

          append ls_attachment to et_attachment.
        endloop.

      catch cx_pyc_frw into lx_pyc_frw.
      catch cx_pyd_fnd into lx_pyd_exc.
      catch cx_pyc_cont into lx_pyc_cont.
    endtry.

  endmethod.


  method read_attachments.
* Read PCC attachements details
    data: ls_filter                 type /iwbep/s_mgw_select_option,
          lt_filter_facade          type /iwbep/t_mgw_select_option,
          lv_filter_facade_property type /iwbep/s_mgw_select_option-property,
          lo_pyc_rt_facade          type ref to cl_pyc_rt_facade,
          lx_exc                    type ref to cx_pyc_frw,
          lx_pyd_fnd                type ref to cx_pyd_fnd,
          lx_pyd_rt                 type ref to cx_pyd_rt.

    loop at it_filter into ls_filter.
      clear lv_filter_facade_property.
      case ls_filter-property.
        when cl_pyc_cont_003_dpc_ext=>gcs_attributes-process_instance_id.
          lv_filter_facade_property = cl_pyc_rt_facade=>gcs_filter-attachment-proc_inst_id.
        when cl_pyc_cont_003_dpc_ext=>gcs_attributes-attachment_id.
          lv_filter_facade_property = cl_pyc_rt_facade=>gcs_filter-attachment-attach_id.
      endcase.
      check lv_filter_facade_property is not initial.
      add_filter( exporting iv_property      = lv_filter_facade_property
                            it_select_option = ls_filter-select_options
                  changing  ct_filter        = lt_filter_facade ).
    endloop.

    try.
        lo_pyc_rt_facade = cl_pyc_rt_facade=>get_instance( ).
        "facade method assume at most one process instance is passed.
        et_attachment = lo_pyc_rt_facade->v3_attachment_get_list( it_filter = lt_filter_facade ).

      catch cx_pyc_frw into lx_exc.
      catch cx_pyd_rt  into lx_pyd_rt.
        create object lx_exc exporting previous = lx_pyd_rt.
      catch cx_pyd_fnd into lx_pyd_fnd.
        create object lx_exc exporting previous = lx_pyd_fnd.

    endtry.

  endmethod.
ENDCLASS.
