class zcl_m99_pa_it0008_mva_chng definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.

    types:
      begin of ty_t549q,
        abkrs type t549a-abkrs,
        begda type t549q-begda,
        endda type t549q-endda,
      end of ty_t549q .
    types:
      tty_t549q type sorted table of ty_t549q with unique key table_line .
    types:
      begin of ty_lgart_list,
        lgart type p0008-lga01,      "wagetype
        betrg type p0008-bet01,      "amount
        anzhl type p0008-anz01,
      end of ty_lgart_list .
    types:
      tty_lgart_list type table of ty_lgart_list .
    types:
      tty_p0008 type table of p0008 .

    constants mc_filter_lgart_01 type pyd_par_type value 'Z99_FILTER_LGART_01' ##NO_TEXT.
    constants mc_itemid_it08_period type pyd_s_rdsfo_ext-itemid value 'IT08PERIOD' ##NO_TEXT.
    constants mc_infotype_0008 type infty value '0008' ##NO_TEXT.
    constants mc_low_date type sy-datum value '18000101' ##NO_TEXT.

    class-methods read_any_infotype
      importing
        !iv_pernr         type pernr-pernr
        !iv_infty         type infty
        !iv_begda         type begda
        !iv_endda         type endda
        !iv_sprps         type prelp-sprps
        !iv_bypass_buffer type flag
      exporting
        !et_pnnnn         type standard table .
protected section.

  types:
    begin of ty_it08_comp_wt_compact,
      begda  type pa0008-begda,
      endda  type pa0008-endda,
      status type char1,
    end of ty_it08_comp_wt_compact .
  types:
    begin of ty_it08_comp_wt,
      pernr type pa0008-pernr.
      include type ty_it08_comp_wt_compact.
    types: end of ty_it08_comp_wt .
  types:
    tty_it08_comp_wt type standard table of ty_it08_comp_wt .
  types:
    begin of ty_t549a,
      abkrs type t549a-abkrs,
      permo type t549a-permo,
    end of ty_t549a .
  types:
    tty_t549a type hashed table of ty_t549a with unique key abkrs .
  types:
    begin of ty_lgart,
      lgart     type pa0008-lga01,
      begda_min type pa0008-begda,
      endda_max type pa0008-endda,
    end of ty_lgart .
  types:
    tty_lgart type hashed table of ty_lgart with unique key lgart .
  types:
    begin of ty_data,
      pernr    type pa0008-pernr,
      subty    type pa0008-subty,
      begda    type pa0008-begda,
      endda    type pa0008-endda,
      begda_01 type pa0001-begda,
      endda_01 type pa0001-endda,
      abkrs_01 type pa0001-abkrs,
    end of ty_data .

  constants MC_MVA_START type C value 'S' ##NO_TEXT.
  constants MC_MVA_CEASING type C value 'C' ##NO_TEXT.
  data MT_FILTER_LGART_01 type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_T549Q type TTY_T549Q .
  data MT_OUTPUT type TTY_IT08_COMP_WT .
  data MV_NUMBER_OF_WAGETYPES_0008 type NUMC2 .

  methods ADJUST_PAY_PERIOD_BY_STATUS
    importing
      !IV_PERNR type PERNR_D
      !IV_PAY_PERIOD_BEGDA type BEGDA
      !IV_PAY_PERIOD_ENDDA type ENDDA
    exporting
      !EV_PAY_PERIOD_BEGDA_NEW type BEGDA
      !EV_PAY_PERIOD_ENDDA_NEW type ENDDA
      !EV_IS_ERROR type ABAP_BOOL .
  methods GET_MVA_WTLIST
    importing
      !IS_P0008 type P0008
      !IT_FILTER_LGART type /IWBEP/T_COD_SELECT_OPTIONS
    exporting
      !ET_MVA_WTAMT type TTY_LGART_LIST
      !ET_MVA_WT type /IWBEP/T_COD_SELECT_OPTIONS .
  methods PREVREC_START_COND_CHECK
    importing
      !IS_P0008 type P0008
      !IT_P0008 type TTY_P0008
      !IT_MVA_WT type /IWBEP/T_COD_SELECT_OPTIONS
    returning
      value(RV_IS_ERROR) type ABAP_BOOL .
  methods GET_PAY_PERIOD_BEGDA_ENDDA
    importing
      !IS_P0008 type P0008
      !IV_DATE type DATUM
    exporting
      !EV_PERIOD_BEGDA type BEGDA
      !EV_PERIOD_ENDDA type ENDDA
      !EV_NON_WEEKLY type ABAP_BOOL
      !EV_IS_ERROR type ABAP_BOOL .
  methods MVA_START_CONDITIONS
    importing
      !IS_P0008 type P0008
      !IT_P0008 type TTY_P0008
      !IT_MVA_WT type /IWBEP/T_COD_SELECT_OPTIONS
    returning
      value(RV_IS_ERROR) type ABAP_BOOL .
  methods MVA_END_CONDITIONS
    importing
      !IS_P0008 type P0008
      !IT_P0008 type TTY_P0008
      value(IT_MVA_WT) type /IWBEP/T_COD_SELECT_OPTIONS
    returning
      value(RV_IS_ERROR) type ABAP_BOOL .
  methods PREVREC_END_COND_CHECK
    importing
      !IS_P0008 type P0008
      !IT_P0008 type TTY_P0008
      !IT_MVA_WT type /IWBEP/T_COD_SELECT_OPTIONS
    returning
      value(RV_IS_ERROR) type ABAP_BOOL .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_IT0008_MVA_CHNG IMPLEMENTATION.


  method adjust_pay_period_by_status.
* Adjust pay period to period when the team member is still active.
    data: lv_hire_date type datum,
          lv_term_date type datum,
          lv_is_ok     type boolean,
          lv_msg       type ref to if_hrpa_message_handler.

* Initialize.
    ev_pay_period_begda_new = iv_pay_period_begda.
    ev_pay_period_endda_new = iv_pay_period_endda.

* Read Employee Hire Date and Termination Date
    clear: lv_hire_date.
    call function 'HR_ECM_GET_HIRE_DATE'
      exporting
        pernr           = iv_pernr
        message_handler = lv_msg
      importing
        hire_date       = lv_hire_date.

* Change Period Start Date
    if iv_pay_period_begda lt lv_hire_date.
      ev_pay_period_begda_new = lv_hire_date.
    endif.

* Read Employee Hire Date and Termination Date
    clear: lv_term_date.
    call function 'HR_ECM_GET_LEAVING_DATE'
      exporting
        pernr           = iv_pernr
*       SELECTION_END_DATE       = '99991231'
        message_handler = lv_msg
      importing
        leaving_date    = lv_term_date
        is_ok           = lv_is_ok.
    if lv_term_date is initial.
      lv_term_date = mc_high_date.
    endif.

* Change Period End Date
    if iv_pay_period_endda gt lv_term_date.
      ev_pay_period_endda_new = lv_term_date.
    endif.

  endmethod.


  method check.
* Check MVA Start and Ceasing conditions
    data: lt_data     type sorted table of ty_data
            with unique key pernr subty endda begda begda_01 endda_01 abkrs_01,
          ls_output   like line of mt_output,
          ls_result   like line of rt_result,
          lv_is_error type abap_bool,
          lt_mva_wt   type /iwbep/t_cod_select_options.

    data: lv_prev_pernr type p0008-pernr,
          lt_p0008      type table of p0008,
          ls_p0008      type p0008.
    data: lv_mva_status type char1.
*

* Employee Data Selection
    select it08~pernr, it08~subty, it08~endda, it08~begda,
           it01~begda as begda_01, it01~endda as endda_01, it01~abkrs as abkrs_01
          into corresponding fields of table @lt_data
          from pa0008 as it08 left outer join pa0001 as it01
                                           on it01~pernr = it08~pernr
                                          and it01~begda <= it08~begda
                                          and it01~endda >= it08~begda
          where it08~pernr in @it_pernr_so
            and it08~aedtm >= @mv_change_begda
            and it08~sprps = @if_hrpa_read_infotype=>unlocked
            and exists ( select 1
                          from pa0000 as it0000 inner join pa0001 as it0001 on
                               it0000~pernr = it0001~pernr
                         where it0000~pernr = it08~pernr
                           and it0000~begda <= @mv_endda
                           and it0000~endda >= @mv_begda
                           and it0000~stat2 in @mt_stat2
                           and it0000~sprps = @if_hrpa_read_infotype=>unlocked
                           and it0001~begda <= @mv_endda
                           and it0001~endda >= @mv_begda
                           and it0001~sprps = @if_hrpa_read_infotype=>unlocked
                           and it0001~abkrs in @mt_payroll_areas
                           and it0001~bukrs in @mt_bukrs
                           and it0001~werks in @mt_werks
                           and it0001~btrtl in @mt_btrtl
                           and it0001~persg in @mt_persg
                           and it0001~persk in @mt_persk
                           and it0001~kostl in @mt_kostl )
    order by it08~pernr, it08~subty, it08~endda, it08~begda, it01~endda, it01~begda.
*
    loop at lt_data into data(ls_data).

      clear: lv_is_error, lt_mva_wt, lv_mva_status.
* Read Employee Infotype Data
      if ls_data-pernr ne lv_prev_pernr.
        refresh: lt_p0008.
        call method zcl_m99_pa_it0008_mva_chng=>read_any_infotype
          exporting
            iv_pernr         = ls_data-pernr
            iv_infty         = mc_infotype_0008
            iv_begda         = mc_low_date
            iv_endda         = mc_high_date
            iv_sprps         = space
            iv_bypass_buffer = abap_true
          importing
            et_pnnnn         = lt_p0008.

        lv_prev_pernr = ls_data-pernr.
      endif.

* Check record
      read table lt_p0008 into ls_p0008
        with key begda = ls_data-begda endda = ls_data-endda binary search.

      check sy-subrc eq 0.
* Read MVA Wage type for the record.
      call method me->get_mva_wtlist
        exporting
          is_p0008        = ls_p0008
          it_filter_lgart = mt_filter_lgart_01
        importing
*         et_mva_wtamt    =
          et_mva_wt       = lt_mva_wt.

      if not lt_mva_wt is initial.
        " Check MVA Start Conditions
        call method me->mva_start_conditions
          exporting
            is_p0008    = ls_p0008
            it_p0008    = lt_p0008
            it_mva_wt   = lt_mva_wt
          receiving
            rv_is_error = lv_is_error.

        lv_mva_status = mc_mva_start.
      else.
        " Check MVA ceasing Conditions
        call method me->mva_end_conditions
          exporting
            is_p0008    = ls_p0008
            it_p0008    = lt_p0008
            it_mva_wt   = mt_filter_lgart_01
          receiving
            rv_is_error = lv_is_error.

        lv_mva_status = mc_mva_ceasing.
      endif.

* Collect Error Records
      if lv_is_error = abap_true.
        "generate error.
        read table rt_result transporting no fields with key
          par_type = if_pyd_cont_types=>gcs_par_type-pernr
                id = ls_data-pernr.
        if sy-subrc <> 0.
          clear: ls_result.
          ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
          ls_result-id = ls_data-pernr.
          insert ls_result into table rt_result.
        endif.

        read table mt_output transporting no fields with key
          pernr = ls_data-pernr
          begda = ls_data-begda
          endda = ls_data-endda.
        if sy-subrc <> 0.
          clear: ls_output.
          ls_output-pernr = ls_data-pernr.
          ls_output-begda = ls_data-begda.
          ls_output-endda = ls_data-endda.
          ls_output-status = lv_mva_status.
          insert ls_output into table mt_output.
        endif.
      endif.

    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    data:
      ls_err_ov         type ty_s_err_ov,
      ls_sfo            type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr          type p_pernr,
      lv_value          type char060,
      ls_output_compact type ty_it08_comp_wt_compact,
      lt_sfo_tab_temp   like ls_err_ov-sfo_tab,
      lv_value_string   type string.
    data: ls_p0008 type p0008,
          lv_endda type endda.
    data: lt_mva_wtamt type tty_lgart_list,
          lt_mva_wt	   type /iwbep/t_cod_select_options,
          lv_wt_amt    type char20,
          lv_wt_text   type lgtxt.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.
        loop at ct_err_ov into ls_err_ov.
          "move SFO_TAB records to temporary table
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          lv_pernr = ls_err_ov-id.

          clear: ls_err_ov-sfo_tab.
          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            clear: ls_output_compact, lv_value, lv_value_string.
            if ls_sfo_tab_temp-itemid = mc_itemid_it08_period.
              lv_value = ls_sfo_tab_temp-value.
              call method me->copy_structure_to_other
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ls_output_compact.

              " Read Infotype 0008 record
              if ls_output_compact-status eq mc_mva_start.
                lv_endda = ls_output_compact-endda.
              else.
                lv_endda = ls_output_compact-begda - 1.
              endif.
              select single * from pa0008
                into corresponding fields of @ls_p0008
                where pernr = @lv_pernr and endda = @lv_endda
                  and sprps = @if_hrpa_read_infotype=>unlocked.
              call method me->get_mva_wtlist
                exporting
                  is_p0008        = ls_p0008
                  it_filter_lgart = mt_filter_lgart_01
                importing
                  et_mva_wtamt    = lt_mva_wtamt
                  et_mva_wt       = lt_mva_wt.

* Message
              if ls_output_compact-status eq mc_mva_start.
                message i045 with ls_p0008-begda ls_p0008-endda into lv_value_string.
              endif.
              if ls_output_compact-status eq mc_mva_ceasing.
                message i046 with ls_p0008-begda ls_p0008-endda into lv_value_string.
              endif.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_it08_period
                  iv_text                     = |{ text-001 }|
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.
* Wage type List
              loop at lt_mva_wtamt into data(ls_mva_wtamt).

                write: ls_mva_wtamt-betrg to lv_wt_amt currency ls_p0008-waers.
                select single lgtxt into lv_wt_text from t512t
*                  where sprsl eq sy-langu and molga eq mc_molga_au and lgart eq ls_mva_wtamt-lgart.  "MOD001--
                   where sprsl eq sy-langu and molga eq mv_molga and lgart eq ls_mva_wtamt-lgart.     "MOD001++

                message i047(zhrpy_pcc_msg)
                     with ls_mva_wtamt-lgart lv_wt_text lv_wt_amt into lv_value_string.

                call method me->add_record_to_sfo_tab
                  exporting
                    iv_itemid                   = mc_itemid_it08_period
                    iv_text                     = ''
                    iv_value                    = lv_value_string
                    iv_text_for_1st_record_only = abap_true
                  changing
                    ct_sfo_tab                  = ls_err_ov-sfo_tab.

              endloop.
            else.
              insert ls_sfo_tab_temp into table ls_err_ov-sfo_tab.
            endif.
          endloop.

          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_output into data(ls_output) where pernr = lv_pernr.
            clear: lv_value.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_it08_period.
            move-corresponding ls_output to ls_output_compact.

            call method me->copy_structure_to_other
              exporting
                p_struct1 = ls_output_compact
              changing
                p_struct2 = lv_value.

            ls_sfo-value = lv_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.
          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_mva_wtlist.
* Read MVA Wage type from the Record
    data: lt_lgart_list type tty_lgart_list.
    data: ls_lgart_list type ty_lgart_list.
    data: ls_p0008 type p0008.

    refresh: et_mva_wtamt, et_mva_wt.
    ls_p0008 = corresponding #( is_p0008 ).
    do mv_number_of_wagetypes_0008 times
      varying ls_lgart_list-lgart from ls_p0008-lga01 next ls_p0008-lga02
      varying ls_lgart_list-betrg from ls_p0008-bet01 next ls_p0008-bet02
      varying ls_lgart_list-anzhl from ls_p0008-anz01 next ls_p0008-anz02.
      if not ls_lgart_list-lgart is initial.
        if ls_lgart_list-lgart in it_filter_lgart.
          append ls_lgart_list to lt_lgart_list.
        endif.
      else.
        exit.
      endif.
    enddo.

    et_mva_wtamt[] = lt_lgart_list[].

    et_mva_wt = value #( for rs_mva_wt in lt_lgart_list
                           ( sign = if_dmf_constants_c=>gc_range_sign_inclusive
                             option = if_dmf_constants_c=>gc_range_option_equal
                             low = rs_mva_wt-lgart ) ).

  endmethod.


  method get_pay_period_begda_endda.
* Get the payperiod begin date for the record
    data: lv_abkrs        type abkrs,
          lv_period_begda type begda,
          lv_period_endda type endda.
*
    call function 'HR_MX_GET_PAYROLL_AREA'
      exporting
        pernr        = is_p0008-pernr
        date         = iv_date
      importing
        payroll_area = lv_abkrs
      exceptions
        it1_error    = 1
        others       = 2.

*Get pay period for a payroll area that covers record bigin date
    data: ls_t549q type ty_t549q.
    loop at mt_t549q into ls_t549q where abkrs = lv_abkrs
        and begda <= iv_date and endda >= iv_date.
      exit.
    endloop.

    if sy-subrc eq 0.
      ev_period_begda = ls_t549q-begda.
      ev_period_endda = ls_t549q-endda.
    else.
      call function 'HR_MX_GET_PAYROLL_PERIOD'
        exporting
          payroll_area = lv_abkrs
          date         = iv_date
        importing
          period_begin = lv_period_begda
          period_end   = lv_period_endda
        exceptions
          t549a_error  = 1
          t549q_error  = 2
          others       = 3.

      if sy-subrc eq 0.
        ls_t549q-abkrs = lv_abkrs.
        ls_t549q-begda = lv_period_begda.
        ls_t549q-endda = lv_period_endda.
        modify table mt_t549q from ls_t549q.
      endif.
    endif.
* Set Monthly / Weekly Payroll Area  floag
    clear: ev_non_weekly.
    if ( lv_period_endda - lv_period_begda ) > 7.
      ev_non_weekly = abap_true.
    endif.
* if the employee is hired mid pay period, this method will adjust
* the start date to the hiring date. The same scenario if the employee has become inactive,
* the end date is adjusted to the end of the active period
    adjust_pay_period_by_status(
      exporting
        iv_pernr = is_p0008-pernr
        iv_pay_period_begda = lv_period_begda
        iv_pay_period_endda = lv_period_endda
      importing
        ev_pay_period_begda_new = ev_period_begda
        ev_pay_period_endda_new = ev_period_endda
        ev_is_error = ev_is_error ).

    if ev_is_error = abap_true.
      return.
    endif.

  endmethod.


  method get_specifc_custmizing.
    try.
        " Allowance Wage Types
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_lgart_01
          changing
            ct_parameter_tab = mt_filter_lgart_01.

        call function 'RP_NUMBER_OF_WAGETYPES_0008'
          importing
            wt_count = mv_number_of_wagetypes_0008.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method mva_end_conditions.
    " Check MVA Ceasing Conditions
    call method me->prevrec_end_cond_check
      exporting
        is_p0008    = is_p0008
        it_p0008    = it_p0008
        it_mva_wt   = it_mva_wt
      receiving
        rv_is_error = rv_is_error.

  endmethod.


  method mva_start_conditions.
* Check MVA Start Conditions
    call method me->prevrec_start_cond_check
      exporting
        is_p0008    = is_p0008
        it_p0008    = it_p0008
        it_mva_wt   = it_mva_wt
      receiving
        rv_is_error = rv_is_error.

  endmethod.


  method prevrec_end_cond_check.
* check if wage types in param IT_WAGE_TYPE_TO_CHECK have to start
* at start of pay period (lv_pay_begda) and end after end of pay period (lv_pay_period_endda_new)
    data: lv_prevpay_begda type begda,
          lv_prevpay_endda type endda,
          lv_non_weekly    type abap_bool.

    data: lv_prevrec_endda  type endda,
          ls_prev_p0008     type p0008,
          lt_prevrec_mva_wt type /iwbep/t_cod_select_options.
    data: lv_days type i.

    " Check Previous Record MVA WT list
    lv_prevrec_endda = is_p0008-begda - 1.
    read table it_p0008 into ls_prev_p0008
      with key endda = lv_prevrec_endda binary search.
    if sy-subrc eq 0.
      call method me->get_mva_wtlist
        exporting
          is_p0008        = ls_prev_p0008
          it_filter_lgart = it_mva_wt
        importing
          et_mva_wt       = lt_prevrec_mva_wt.
      if lt_prevrec_mva_wt is initial.
        rv_is_error = abap_false.
        exit.
      else.
        " Check End date is Period End date for Weekly Payroll Area
        get_pay_period_begda_endda(
          exporting
            is_p0008 = ls_prev_p0008
            iv_date  = ls_prev_p0008-endda
          importing
            ev_period_begda = lv_prevpay_begda
            ev_period_endda = lv_prevpay_endda
            ev_non_weekly  = lv_non_weekly
            ev_is_error = rv_is_error ).

        "Check the Prev Rec End date
        if lv_non_weekly eq abap_true.
          " Monthly Payroll Area - Raise Alert
          rv_is_error = abap_true.
        else.
          " Weekly Payroll Area
          if ls_prev_p0008-endda eq lv_prevpay_endda.
            " Ended on Payroll Period End Date
            rv_is_error = abap_false.
          else.
            " Not Ended on Payroll Period End Date
            rv_is_error = abap_true.
          endif.
        endif.
      endif.

    endif.

  endmethod.


  method prevrec_start_cond_check.
* check if wage types in param IT_WAGE_TYPE_TO_CHECK have to start
* at start of pay period (lv_pay_begda) and end after end of pay period (lv_pay_period_endda_new)
    data: lv_prevrec_endda  type endda,
          ls_prev_p0008     type p0008,
          lt_prevrec_mva_wt type /iwbep/t_cod_select_options,
          lt_diff_mva_wt    type /iwbep/t_cod_select_options.

    data: ls_period_begda type begda,
          ls_period_endda type endda,
          lv_non_weekly   type abap_bool.

    " Check Previous Record MVA WT list
    lv_prevrec_endda = is_p0008-begda - 1.
    read table it_p0008 into ls_prev_p0008
      with key endda = lv_prevrec_endda binary search.

    clear: lt_prevrec_mva_wt.
    if not ls_prev_p0008 is initial.
      call method me->get_mva_wtlist
        exporting
          is_p0008        = ls_prev_p0008
          it_filter_lgart = it_mva_wt
        importing
          et_mva_wt       = lt_prevrec_mva_wt.
    endif.

    clear: lt_diff_mva_wt.
    call function 'BKK_COMPARE_TABLES'
      exporting
        i_tab            = it_mva_wt
        i_tab_sub        = lt_prevrec_mva_wt
      importing
        e_tab_difference = lt_diff_mva_wt.

    if lt_diff_mva_wt is initial.
      " Same Wage types no Alert
      rv_is_error = abap_false.
    else.
* get the payperiod begin date for the record
      get_pay_period_begda_endda(
        exporting
          is_p0008 = is_p0008
          iv_date = is_p0008-begda
        importing
          ev_period_begda = ls_period_begda
          ev_period_endda = ls_period_endda
          ev_non_weekly = lv_non_weekly
          ev_is_error = rv_is_error ).

      if lv_non_weekly eq abap_true.
        " Monthly Payroll Area Need to be Investigated
        rv_is_error = abap_true.
      else.
        " Weekly Payroll Area
        if is_p0008-begda eq ls_period_begda.
          " MVA record started on Pay period Begin Date
          rv_is_error = abap_false.
        else.
          " MVA record not started on Pay period Begin Date
          rv_is_error = abap_true.
        endif.
      endif.
    endif.

    " No Need to Check the Bigin date of Existing records
*       "Check the Prev Rec bigin date
*        if ls_prev_p0008-begda gt iv_pay_begda.
*          if iv_non_weekly eq abap_true.
*            rv_is_error = abap_false.
*          else.
*            rv_is_error = abap_true.
*          endif.
*        elseif ls_prev_p0008-begda eq iv_pay_begda.
*          rv_is_error = abap_false.
*        else.
*           Check Previous Record
*          call method me->mva_start_conditions
*            exporting
*              is_p0008    = ls_prev_p0008
*              it_p0008    = it_p0008
*              it_mva_wt   = it_mva_wt
*            receiving
*              rv_is_error = rv_is_error.
*        endif.

  endmethod.


  method read_any_infotype.
    data: l_subrc like sy-subrc.
**
    call function 'HR_INITIALIZE_BUFFER'
      exceptions
        others = 1.

    refresh et_pnnnn.
    clear   et_pnnnn.
    call function 'HR_READ_INFOTYPE'
      exporting
        tclas           = 'A'
        pernr           = iv_pernr
        infty           = iv_infty
        begda           = iv_begda
        endda           = iv_endda
        sprps           = iv_sprps
        bypass_buffer   = iv_bypass_buffer
      importing
        subrc           = l_subrc
      tables
        infty_tab       = et_pnnnn
      exceptions
        infty_not_found = 4
        others          = 8.

    if sy-subrc eq 0.
      sy-subrc = l_subrc.
    endif.

  endmethod.
ENDCLASS.
