class ZUSECL_PA_IT0002_BDAY_IT0014 definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
protected section.

  types:
    begin of ty_it0002_dtls,
             gbdat type gbdat.
    types: end of ty_it0002_dtls .
  types:
    begin of ty_it0002,
             pernr type pernr_d,
             gbdat type gbdat,
             gbjhr type gbjhr,
             gbmon type gbmon,
             gbtag type gbtag.
    types:end of ty_it0002 .
  types:
    tty_it0002 type table of ty_it0002 .
  types:
    begin of ty_it0014,
             pernr type pernr_d,
             subty type subty,
             begda type begda,
             endda type endda,
             betrg type maxbt,
             waers type waers.
    types: end of ty_it0014 .
  types:
    tty_it0014 type table of ty_it0014 .

  data MT_IT0014 type TTY_IT0014 .
  data MS_IT0014 type TY_IT0014 .
  data MT_IT0002 type TTY_IT0002 .
  data MS_IT0002 type TY_IT0002 .
  data MS_IT0002_DTLS type TY_IT0002_DTLS .
  constants MC_INFTY_0014 type INFTY value '0014' ##NO_TEXT.
  constants MC_ITEMID_AGEALERR type PYD_ITEMID value 'AGEALERR' ##NO_TEXT.
  constants MC_FILTER_LGART type PYD_PAR_TYPE value 'Z99_FILTER_LGART' ##NO_TEXT.
  constants MC_EXC_FILTER_LGART type PYD_PAR_TYPE value 'Z99_EXC_FILTER_LGART' ##NO_TEXT.
  data MT_FILTER_LGART type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_FKBER type /IWBEP/T_COD_SELECT_OPTIONS .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
private section.

  data MC_FKBER type PYD_PAR_TYPE value 'Z99_FKBER' ##NO_TEXT.
  data MC_EXC_FKBER type PYD_PAR_TYPE value 'Z99_EXC_FKBER' ##NO_TEXT.
ENDCLASS.



CLASS ZUSECL_PA_IT0002_BDAY_IT0014 IMPLEMENTATION.


  method CHECK.
* Read Employees Data for Location Allowance & Birthday Increases
    types: begin of ty_mon_and_date,
             gbmon type gbmon,
             gbtag type gbtag.
    types: end of ty_mon_and_date.
    data: lt_mon_and_date type table of ty_mon_and_date,
          ls_mon_and_date type ty_mon_and_date.
*
    data: lv_year         type numc4,
          lv_21year_begda type begda,
          lv_21year_endda type endda.
    data: lt_it0014 type table of ty_it0014.
    data: lt_it0002 type table of ty_it0002.
    data: lt_dates type table of  scscp_period_str.

    data: ls_result  type ty_s_result.

* Set 21 Year PERIOD
    lv_21year_endda = mv_endda.
    lv_21year_begda = mv_endda.
    lv_year = mv_endda+0(4).
    lv_year = lv_year - 21.
    lv_21year_begda+0(4) = lv_year.


* Read Employees with Birthday in the period
* Determine period dates
    call function 'CSCP_PARA1_GET_PERIODS'
      exporting
        i_datuv    = mv_begda
        i_datub    = mv_endda
        i_timeunit = 'D'
      tables
        et_dates   = lt_dates.

* Populate Month and Date from all dates
    loop at lt_dates into data(ls_dates).
      clear ls_mon_and_date.
      ls_mon_and_date-gbmon = ls_dates-datub+4(2). " MONTH
      ls_mon_and_date-gbtag = ls_dates-datub+6(2). " DATE
      append ls_mon_and_date to lt_mon_and_date.
    endloop.

* Fetch Employees Whose Birthday occurs in the period dates
    select it0002~pernr, it0002~gbdat, it0002~gbjhr, it0002~gbmon, it0002~gbtag
      into corresponding fields of table @lt_it0002 from pa0002 as it0002
       for all entries in @lt_mon_and_date
     where it0002~gbmon = @lt_mon_and_date-gbmon
       and it0002~gbtag = @lt_mon_and_date-gbtag
       and it0002~gbjhr between @lv_21year_begda and @lv_21year_endda
      and it0002~begda <= @mv_endda
      and it0002~endda >= @mv_begda
      and exists ( select it0001~pernr
                    from pa0000 as it0000 inner join pa0001 as it0001 on
                         it0000~pernr = it0001~pernr
                   where it0000~pernr = it0002~pernr
                     and it0000~begda <= @mv_endda
                     and it0000~endda >= @mv_begda
                     and it0000~stat2 in @mt_stat2
                     and it0000~sprps = ' '
                     and it0001~begda <= @mv_endda
                     and it0001~endda >= @mv_begda
                     and it0001~sprps = ' '
                     and it0001~abkrs in @mt_payroll_areas
                     and it0001~bukrs in @mt_bukrs
                     and it0001~werks in @mt_werks
                     and it0001~btrtl in @mt_btrtl
                     and it0001~persg in @mt_persg
                     and it0001~persk in @mt_persk
                     and it0001~kostl in @mt_kostl
                     and it0001~fkber in @mt_fkber ).

    sort lt_it0002 by pernr.
    delete adjacent duplicates from lt_it0002 comparing pernr.

* All relevant employees with GPPL Payment Records
    if not lt_it0002[] is initial.
      select it0014~pernr, it0014~subty, it0014~begda, it0014~endda, it0014~betrg, it0014~waers
        into corresponding fields of table @lt_it0014 from pa0014 as it0014
        for all entries in @lt_it0002
          where pernr = @lt_it0002-pernr
            and it0014~subty in @mt_filter_lgart
            and it0014~sprps = ' '
            and it0014~begda <= @mv_endda
            and it0014~endda >= @mv_begda.
    endif.
*
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it0002 into data(ls_it0002).
      read table lt_it0014 into data(ls_it0014)
       with key pernr = ls_it0002-pernr binary search.
      if sy-subrc eq 0.
* Build Results table
        ls_result-id = ls_it0002-pernr.
        insert ls_result into table rt_result.

* Collect All Current records for Reporting
        move-corresponding ls_it0002 to ms_it0002.
        append ms_it0002 to mt_it0002.
      endif.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.
  endmethod.


  method ERR_OV_GET_LIST.
* Method for Overview list display
    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr,
      lv_value  type pyd_item_value.
    data: lv_text type pyd_name.

    data lo_message_handler    type ref to cl_hrpa_message_list.
    create object lo_message_handler.
    data: lv_age type num2,
          lv_ok  type boole_d.

* Populate SFO tab
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          clear ls_err_ov-sfo_tab.

          clear ls_sfo.
          call function 'HR_ECM_CALC_AGE'
            exporting
              pernr           = lv_pernr
              keydt           = mv_endda
              datar           = ' '
              message_handler = lo_message_handler
            importing
              age             = lv_age
              is_ok           = lv_ok.
          if lv_ok eq abap_true.
            message i017(zhrpy_pcc_msg) with lv_age into lv_value.
          endif.

          clear ls_sfo.
          lv_text = text-001.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_agealerr
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.


      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Read Custom Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.

    try.
* Read Functional Area
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_fkber
            iv_exc_par_type  = me->mc_exc_fkber
          changing
            ct_parameter_tab = mt_fkber.

* Read Allowance Wage types
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_lgart
            iv_exc_par_type  = me->mc_exc_filter_lgart
          changing
            ct_parameter_tab = mt_filter_lgart.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
