class ZCL_PYC_BPC_PARA_SELECTION_TE definition
  public
  inheriting from CL_PYC_BPC_SELECTION_PNP_PERNR
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



CLASS ZCL_PYC_BPC_PARA_SELECTION_TE IMPLEMENTATION.


  method get_leading_time_selection.
* Populate selection parameters

    constants: mc_abkrs        type pyd_par_type value 'ABKRS',
               mc_period       type pyd_par_type value 'PERIOD',
               mc_variant      type pyd_par_type value 'VARIANT',
               mc_sel_timr9    type rsscr_name value 'PNPTIMR9',
               mc_sel_timra    type rsscr_name value 'PNPTIMRA',
               mc_par_pnpxabkr type rsscr_name value 'PNPXABKR',
               mc_par_pnpabkrs type rsscr_name value 'PNPABKRS',
               mc_par_pnppabrp type rsscr_name value 'PNPPABRP',
               mc_par_pnppabrj type rsscr_name value 'PNPPABRJ',
               mc_par_enddate  type rsscr_name value 'ENDDATE'.
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

* Read Payroll Area.
    loop at it_par into ls_par
       where par_type eq mc_abkrs.
      lv_payroll_area = ls_par-low.
    endloop.

    loop at it_par into ls_par.
      case ls_par-par_type.
        when 'ABKRS'.

          lv_sel_name = mc_par_pnpabkrs.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          move ls_par-opti to ls_sel_params-option.
          ls_sel_params-selname = lv_sel_name.
          append ls_sel_params to et_sel_params  .

*if period exist, assign other period
        when 'PERIOD'.
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

          clear ls_sel_params .
          ls_sel_params-selname = mc_par_enddate.
          ls_sel_params-kind = 'P' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-option = 'EQ' .
          ls_sel_params-low = lv_endda .
          append ls_sel_params to et_sel_params .

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
