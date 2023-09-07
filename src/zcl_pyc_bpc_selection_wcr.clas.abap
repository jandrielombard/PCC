class zcl_pyc_bpc_selection_wcr definition
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



CLASS ZCL_PYC_BPC_SELECTION_WCR IMPLEMENTATION.


  method get_leading_time_selection.
* Populate Selection Parameters
    constants: mc_abkrs     type pyd_par_type value 'ABKRS',
               mc_period    type pyd_par_type value 'PERIOD',
               mc_variant   type pyd_par_type value 'VARIANT',
               mc_sel_parea type rsscr_name value 'S_PAREA',
               mc_par_begda type rsscr_name value 'P_BEGD',
               mc_par_endda type rsscr_name value 'P_ENDD'.

    data:
      ls_par        type pyd_s_resp,
      ls_sel_params type line of rsparams_tt,
      lv_sel_name   type char8.

    data:
      lv_variant type rsvar-variant.

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
    loop at it_par into ls_par.
      case ls_par-par_type.
        when  mc_abkrs.
          lv_payroll_area = ls_par-low.
        when mc_variant.
          lv_variant = ls_par-low.
      endcase.
    endloop.

* Set ABKRS
    loop at it_par into ls_par.
      case ls_par-par_type.
* Populate Period Paraeters
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

          lv_sel_name = mc_par_begda.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-sign = 'I'.
          ls_sel_params-option = 'EQ'.
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low = lv_begda.
          append ls_sel_params to et_sel_params .

          lv_sel_name = mc_par_endda.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-sign = 'I'.
          ls_sel_params-option = 'EQ'.
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low = lv_endda.
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
