class ZCL_PYC_BPC_SELECTION_STP definition
  public
  inheriting from CL_PYC_BPC_SELECTION_PNP
  final
  create public .

public section.
protected section.

  constants:
    begin of gc_par_type,
        period type pyd_par_type value 'PERIOD',
        abkrs  type pyd_par_type value 'ABKRS',
      end of gc_par_type .

  methods GET_LEADING_TIME_SELECTION
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_BPC_SELECTION_STP IMPLEMENTATION.


  method get_leading_time_selection.
* Populate Selection Parameters
    constants: mc_abkrs        type pyd_par_type value 'ABKRS',
               mc_period       type pyd_par_type value 'PERIOD',
               mc_variant      type pyd_par_type value 'VARIANT',
               mc_test_run     type pyd_par_type value 'Z99_TEST_RUN',

               mc_sel_timed    type rsscr_name value 'PNPTIMED',
               mc_par_pnppabrp type rsscr_name value 'PNPPABRP',
               mc_par_pnppabrj type rsscr_name value 'PNPPABRJ',
               mc_par_pnpxabkr type rsscr_name value 'PNPXABKR',
               mc_par_pnpabkrs type rsscr_name value 'PNPABKRS',
               mc_par_submit   type rsscr_name value 'SUBMIT',
               mc_par_update   type rsscr_name value 'UPDATE',
               mc_par_runty    type rsscr_name value 'RUNTY',
               mc_par_spool    type rsscr_name value 'SPOOL'.

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
      ls_sel_params-selname = mc_sel_timed.
      ls_sel_params-kind = 'P' .
      ls_sel_params-sign = 'I' .
      ls_sel_params-option = 'EQ' .
      ls_sel_params-low = '2' .
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
* Populate PNPXABKR
          lv_sel_name = mc_par_pnpxabkr.

          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-sign = 'I' .
          ls_sel_params-option = 'EQ' .
          ls_sel_params-selname = lv_sel_name.
          append ls_sel_params to et_sel_params  .

* Populate PNPABKRS
          lv_sel_name = mc_par_pnpabkrs.

          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-option = 'EQ' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-selname = lv_sel_name.
          append ls_sel_params to et_sel_params  .

*if period exist, assign other period
        when mc_period.
          lv_pabrj = ls_par-low(4).
          lv_pabrp = ls_par-low+4(2).

          lv_sel_name = mc_par_pnppabrp.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-option = 'EQ' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low = lv_pabrp.
          append ls_sel_params to et_sel_params .

          lv_sel_name = mc_par_pnppabrj.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-option = 'EQ' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low = lv_pabrj.
          append ls_sel_params to et_sel_params .

        when mc_test_run.
* Parameter Run Type
          lv_sel_name = mc_par_runty.
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

* Parameter Submit
    lv_sel_name = mc_par_submit.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind = 'P' .
    ls_sel_params-option = 'EQ' .
    ls_sel_params-sign = 'I' .
    ls_sel_params-low = abap_true.
    append ls_sel_params to et_sel_params .

* Parameter Update
    lv_sel_name = mc_par_update.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind = 'P' .
    ls_sel_params-option = 'EQ' .
    ls_sel_params-sign = 'I' .
    ls_sel_params-low = ' '.
    append ls_sel_params to et_sel_params .

* Parameter Spool Down load
    lv_sel_name = mc_par_spool.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind = 'P' .
    ls_sel_params-option = 'EQ' .
    ls_sel_params-sign = 'I' .
    ls_sel_params-low = abap_true.
    append ls_sel_params to et_sel_params.

    if lines( et_sel_params ) = 0 .
      raise exception type cx_pyc_cont
        exporting
          textid = cx_pyc_cont=>get_leading_time_selection_err.
    endif.

  endmethod.
ENDCLASS.
