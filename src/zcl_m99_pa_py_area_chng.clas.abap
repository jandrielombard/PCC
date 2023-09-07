class zcl_m99_pa_py_area_chng definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.
  protected section.

    types:
      begin of ty_data,
        pernr type p0001-pernr,
        begda type p0001-begda,
        endda type p0001-endda,
        abkrs type p0001-abkrs.
    types: end of ty_data .
    types:
      tty_data type table of ty_data .
    types:
      begin of ty_abkrs_count,
        pernr type pernr_d,
        count type i.
    types: end of ty_abkrs_count .
    types:
      tty_abkrs_count type table of ty_abkrs_count .
    types:
      begin of ty_abkrs,
        abkrs type t549a-abkrs.
    types:   end of ty_abkrs .
    types:
      tty_abkrs type table of ty_abkrs .

    constants mc_itemid_transfer type pyd_itemid value 'TRANSFER' ##NO_TEXT.
    data: mt_exe_z_abkrs type /iwbep/t_cod_select_options .

    methods read_employee_abkrs
      importing
        !iv_pernr        type pernr_d
      exporting
        value(et_it0001) type tty_data .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_PY_AREA_CHNG IMPLEMENTATION.


  method check.
* Validation to check the employees payroll area
* on the last day of the payroll period vs the first day of the payroll period.
* Where they are different it means the employees has transfered PY area
* at some point during the payroll period. This will trigger an alert
    data: lt_emp_data  type  tty_data.
    data: lt_output type tty_abkrs_count.
    data: ls_output type ty_abkrs_count.

* Read Employee Payroll Area
    select distinct it0001~pernr, it0001~abkrs
      into corresponding fields of table @lt_emp_data
      from pa0001 as it0001
      where it0001~pernr in @it_pernr_so
        and it0001~sprps = @if_hrpa_read_infotype=>unlocked
        and it0001~begda <= @mv_endda
        and it0001~endda >= @mv_begda
        and exists ( select 1
                      from pa0000 as it00
                      inner join pa0001 as it01 on
                            it00~pernr = it01~pernr
                      where it00~pernr = it0001~pernr
                        and it00~begda <= @mv_endda
                        and it00~endda >= @mv_begda
                        and it00~sprps = @if_hrpa_read_infotype=>unlocked
                        and it00~stat2 in @mt_stat2
                        and it01~begda <= @mv_endda
                        and it01~endda >= @mv_endda
                        and it01~sprps = @if_hrpa_read_infotype=>unlocked
                        and it01~abkrs in @mt_payroll_areas
                        and it01~bukrs in @mt_bukrs
                        and it01~werks in @mt_werks
                        and it01~btrtl in @mt_btrtl
                        and it01~persg in @mt_persg
                        and it01~persk in @mt_persk
                        and it01~kostl in @mt_kostl ).

* Identify Payroll Area changes in the period
    if lt_emp_data is not initial.
      delete lt_emp_data where abkrs in mt_exe_z_abkrs.
      delete adjacent duplicates from lt_emp_data comparing all fields.

      loop at lt_emp_data into data(ls_emp_data).
        move: ls_emp_data-pernr to ls_output-pernr,
              1 to ls_output-count.
        collect ls_output into lt_output.
      endloop.
      delete lt_output where count le 1.

* Prepare Result table
      loop at lt_output  into ls_output.
        insert value #(
                par_type = if_pyd_cont_types=>gcs_par_type-pernr
                id = ls_output-pernr ) into table rt_result.
      endloop.
    endif.
    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Display error message
    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr  type p_pernr.
    data: lv_msg type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
    data: lv_text type pyd_name.
    data: lv_begda(10) type c,
          lv_endda(10) type c.
    data: lt_it0001 type tty_data.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters
        get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).

        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.

* Generic Message
          clear ls_err_ov-sfo_tab.
          lv_text = text-001.
          lv_msg = text-002.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_transfer
              iv_text                     = lv_text
              iv_value                    = lv_msg
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Read Employee Payroll Areas
          call method me->read_employee_abkrs
            exporting
              iv_pernr  = lv_pernr
            importing
              et_it0001 = lt_it0001.

          loop at lt_it0001 into data(ls_it0001).
            write: ls_it0001-begda to lv_begda dd/mm/yyyy.
            write: ls_it0001-endda to lv_endda dd/mm/yyyy.

            lv_text = | { lv_begda } - { lv_endda } |.
            lv_msg = | { text-003 } { ls_it0001-abkrs } |.

            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_transfer
                iv_text                     = lv_text
                iv_value                    = lv_msg
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_specifc_custmizing.

    data lt_param_so  type /iwbep/t_cod_select_options.

    data: lv_srch_str type abkrs value 'Z%'.
    data: lt_abkrs type tty_abkrs.

    try.
* Read Z Payroll Areas
        select distinct abkrs
          into corresponding fields of table @lt_abkrs
          from t549a
          where abkrs like @lv_srch_str.

        " Build Z ABKRS Range Table
        loop at lt_abkrs into data(ls_abkrs).
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_abkrs-abkrs   ).
          append lines of lt_param_so to mt_exe_z_abkrs.
        endloop.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.


  method read_employee_abkrs.
* Read Employee Payroll Area
    data: lt_data type tty_data.
    data: ls_data type ty_data.
    data: ls_output type ty_data.
    data: lv_flag1 type c.

    refresh: et_it0001.
* Read Employee Payroll Area
    select distinct it0001~pernr, it0001~begda, it0001~endda, it0001~abkrs
      into corresponding fields of table @lt_data
      from pa0001 as it0001
      where it0001~pernr = @iv_pernr
        and it0001~sprps = @if_hrpa_read_infotype=>unlocked
        and it0001~begda <= @mv_endda
        and it0001~endda >= @mv_begda.

    delete lt_data where abkrs in mt_exe_z_abkrs.

    sort lt_data by begda.
    provide fields * from lt_data into ls_data
                          valid lv_flag1
                          bounds begda and endda
      between mv_begda and mv_endda.
      if ls_output is initial.
        move-corresponding ls_data to ls_output.
      else.
        if ls_data-abkrs eq ls_output-abkrs.
          move ls_data-endda to ls_output-endda.
        else.
          collect ls_output into et_it0001.

          move-corresponding ls_data to ls_output.
        endif.
      endif.
    endprovide.
    if not ls_output is initial.
      collect ls_output into et_it0001.
    endif.

  endmethod.
ENDCLASS.
