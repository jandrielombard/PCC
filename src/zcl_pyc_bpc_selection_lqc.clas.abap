class zcl_pyc_bpc_selection_lqc definition
  public
  inheriting from cl_pyc_bpc_selection_pnp
  final
  create public .

  public section.
  protected section.

    constants:
      begin of gc_par_type,
        period type pyd_par_type value 'PERIOD',
        abkrs  type pyd_par_type value 'ABKRS',
      end of gc_par_type .

    methods get_leading_time_selection
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_BPC_SELECTION_LQC IMPLEMENTATION.


  method get_leading_time_selection.
* Populate Selection Parameters
    constants: mc_abkrs        type pyd_par_type value 'ABKRS',
               mc_period       type pyd_par_type value 'PERIOD',
               mc_variant      type pyd_par_type value 'VARIANT',
               mc_stat2        type pyd_par_type value 'Z99_STAT2',
               mc_sel_timr6    type rsscr_name value 'PNPTIMR6',
               mc_sel_timra    type rsscr_name value 'PNPTIMRA',
               mc_par_pnpbegda type rsscr_name value 'PNPBEGDA',
               mc_par_pnpendda type rsscr_name value 'PNPENDDA',
               mc_par_pnpabkrs type rsscr_name value 'PNPABKRS',
               mc_par_pnpstat2 type rsscr_name value 'PNPSTAT2'.

    data:
      ls_par        type pyd_s_resp,
      ls_sel_params type line of rsparams_tt,
      lv_sel_name   type char8.

    data: lv_payroll_area type tvarv_val,
          lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_vabrj        type vabrj,
          lv_vabrp        type vabrp,
          lv_abkrs        type abkrs,
          lv_begda        type begda,
          lv_endda        type endda,
          lo_payroll_area type ref to cl_hr_payroll_area.


    clear et_sel_params.

*check PERIOD exist; if exist set to the other period
    read table it_par into ls_par with key par_type = gc_par_type-period.
    if sy-subrc = 0.
      clear ls_sel_params .
      ls_sel_params-selname = mc_sel_timr6.
      ls_sel_params-kind = 'P' .
      ls_sel_params-low = 'X' .
      ls_sel_params-option = 'EQ' .
      ls_sel_params-sign = 'I' .
      append ls_sel_params to et_sel_params .

      clear ls_sel_params .
      ls_sel_params-selname = mc_sel_timra .
      ls_sel_params-kind = 'P' .
      ls_sel_params-low = ' ' .
      ls_sel_params-option = 'EQ' .
      ls_sel_params-sign = 'I' .
      append ls_sel_params to et_sel_params .
    endif.

* Read Payroll Area.
    loop at it_par into ls_par
       where par_type eq mc_abkrs.
      lv_payroll_area = ls_par-low.
    endloop.

* Set ABKRS, Period Begda and Period Endda
    loop at it_par into ls_par.
      case ls_par-par_type.
        when mc_abkrs.

          lv_sel_name = mc_par_pnpabkrs.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-option = 'EQ' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-selname = lv_sel_name.
          append ls_sel_params to et_sel_params  .

*if period exist, assign other period
        when mc_period.

          lv_abkrs = lv_payroll_area.
          lo_payroll_area = cl_hr_payroll_area=>get_instance( imp_area = lv_abkrs ).
          lv_pabrj = ls_par-low(4).
          lv_pabrp = ls_par-low+4(2).

          lo_payroll_area->get_period_info(
            exporting
              imp_pabrj = lv_pabrj
              imp_pabrp = lv_pabrp
            importing
              exp_vabrj = lv_vabrj
              exp_vabrp = lv_vabrp
              exp_begda = lv_begda
              exp_endda = lv_endda ).

          "LQC requirement: start date minus 28 days, end date is the last day of the year
          lv_begda = lv_begda - 28.
          lv_endda = lv_endda + 90.

          lv_sel_name = mc_par_pnpbegda.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-option = 'EQ' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low = lv_begda.
          append ls_sel_params to et_sel_params .

          lv_sel_name = mc_par_pnpendda.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-option = 'EQ' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low = lv_endda.
          append ls_sel_params to et_sel_params .

        when mc_stat2.
* Parameter Employee Status
          lv_sel_name = mc_par_pnpstat2.
          clear ls_sel_params .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-kind = 'P' .
          ls_sel_params-option = 'EQ' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-low = ls_par-low.
          append ls_sel_params to et_sel_params.
        when others.
      endcase.
    endloop.

    if lines( et_sel_params ) = 0 .
      raise exception type cx_pyc_cont
        exporting
          textid = cx_pyc_cont=>get_leading_time_selection_err.
    endif.
  endmethod.
ENDCLASS.
