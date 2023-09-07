class ZCL_PYC_BPC_SEL_OC_FI_RECON definition
  public
  inheriting from CL_PYC_BPC_SELECTION_PNP
  create public .

public section.
protected section.

  methods GET_OTHER_PARAMS
    redefinition .
  methods GET_LEADING_TIME_SELECTION
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_PYC_BPC_SEL_OC_FI_RECON IMPLEMENTATION.


  method get_leading_time_selection.

  endmethod.


  method get_other_params.
* Populate Selection Parameters
    constants: mc_abkrs      type pyd_par_type value 'ABKRS',
               mc_period     type pyd_par_type value 'PERIOD',
               mc_par_abkrs  type rsscr_name value 'P_ABKRS',
               mc_par_pabrp  type rsscr_name value 'P_PABRP',
               mc_par_pabrj  type rsscr_name value 'P_PABRJ',
               mc_par_payty  type rsscr_name value 'P_PAYTY',
               mc_par_payid  type rsscr_name value 'P_PAYID',
               mc_par_bondt  type rsscr_name value 'P_BONDT',
               mc_reg_pay    type rsscr_name value 'NOC',
               mc_offc_pay   type rsscr_name value 'YOC',
               mc_par_pccrun type rsscr_name value 'P_PCCRUN',
               mc_test_run   type pyd_par_type value 'Z99_TEST_RUN'.

    constants: mc_i  type tvarv_sign value 'I',
               mc_eq type tvarv_opti value 'EQ'.

    data:
      lv_proc_inst_id type pyc_proc_inst_id,
      lo_sel_access   type ref to if_pyc_selection_oc_access,
      lt_selection    type if_pyc_selection_oc_access=>ty_t_selection,
      ls_selection    type if_pyc_selection_oc_access=>ty_s_selection,
      ls_sel_params   type rsparams,
      lv_is_pnpce     type boole_d,
      lx_pyc_frw      type ref to cx_pyc_frw,
      lx_pyd_fnd      type ref to cx_pyd_fnd,
      lv_dummy        type string.
    data lt_par	type if_pyd_fnd_types=>ty_t_resp.

    data: ls_par      type pyd_s_resp,
          lv_sel_name type char8.

    data: lv_payroll_area type abkrs,
          lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_abkrs        type abkrs.

    try .
        lv_proc_inst_id = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = cl_pyc_rt_facade=>gcs_par_type-proc_inst
                                                                it_par      = io_context->mt_par ).

        lo_sel_access = cl_pyc_selection_oc_access=>get_instance( ).
        lt_selection = lo_sel_access->selection_list_get( iv_proc_inst_id = lv_proc_inst_id ).
        lt_par  = io_context->mt_par.

        "If no selection object is found, raise exception
        if lt_selection is initial.
          raise exception type cx_pyc_cont
            exporting
              textid = cx_pyc_cont=>oc_empty_selection.

          message i222(pyc_cont) into lv_dummy  ##STMNT_EXIT.
        endif.

* Read Payroll Area.
        loop at lt_par into ls_par.
          case ls_par-par_type.
            when  mc_abkrs.
              lv_payroll_area = ls_par-low.
          endcase.
        endloop.

        " Payroll Area
        lv_abkrs = lv_payroll_area.
        lv_sel_name = mc_par_abkrs.
        clear ls_sel_params .
        move-corresponding ls_par to ls_sel_params.
        ls_sel_params-sign = mc_i.
        ls_sel_params-option = mc_eq.
        ls_sel_params-selname = lv_sel_name.
        ls_sel_params-low = lv_abkrs.
        append ls_sel_params to et_sel_params .

        " Inject parameters from selection package infomation
        read table lt_selection into ls_selection with key package_id = iv_rpt_chain_id.
        if sy-subrc = 0.
          if ls_selection-pabrj is initial.
* Regular payroll run
            lv_sel_name = mc_reg_pay.
            clear ls_sel_params .
            ls_sel_params-selname = lv_sel_name.
            ls_sel_params-sign = mc_i.
            ls_sel_params-option = mc_eq.
            ls_sel_params-low = abap_false.
            append ls_sel_params to et_sel_params .
* Off Cycle Payroll Run
            lv_sel_name = mc_offc_pay.
            clear ls_sel_params .
            ls_sel_params-selname = lv_sel_name.
            ls_sel_params-sign = mc_i.
            ls_sel_params-option = mc_eq.
            ls_sel_params-low = abap_true.
            append ls_sel_params to et_sel_params .
* Pay Date
            clear ls_sel_params.
            ls_sel_params-selname = mc_par_bondt .
            ls_sel_params-low     = ls_selection-bondt.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
* Pay Type
            clear ls_sel_params.
            ls_sel_params-selname = mc_par_payty .
            ls_sel_params-low     = ls_selection-payty.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
* Pay ID
            clear ls_sel_params.
            ls_sel_params-selname = mc_par_payid .
            ls_sel_params-low     = ls_selection-payid.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

          else.
            " double check mandatory info to be used for selection parameters
            if ls_selection-pabrj is initial
              or ls_selection-pabrp is initial
              or ls_selection-abkrs is initial.
              raise exception type cx_pyc_cont. " David, tbd, message content
            endif.
* Regular payroll run
            lv_sel_name = mc_reg_pay.
            clear ls_sel_params .
            ls_sel_params-selname = lv_sel_name.
            ls_sel_params-sign = mc_i.
            ls_sel_params-option = mc_eq.
            ls_sel_params-low = abap_true.
            append ls_sel_params to et_sel_params .
* Off Cycle Payroll Run
            lv_sel_name = mc_offc_pay.
            clear ls_sel_params .
            ls_sel_params-selname = lv_sel_name.
            ls_sel_params-sign = mc_i.
            ls_sel_params-option = mc_eq.
            ls_sel_params-low = abap_false.
            append ls_sel_params to et_sel_params .
* Pay Period Year
            clear ls_sel_params.
            ls_sel_params-selname = mc_par_pabrj .
            ls_sel_params-low     = ls_selection-pabrj.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
* Pay Period
            clear ls_sel_params.
            ls_sel_params-selname = mc_par_pabrp .
            ls_sel_params-low     = ls_selection-pabrp.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
          endif.

* PCC Run
          lv_sel_name = mc_par_pccrun.
          clear ls_sel_params .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-sign = mc_i.
          ls_sel_params-option = mc_eq.
          ls_sel_params-low = abap_true.
          append ls_sel_params to et_sel_params .

        endif.
      catch cx_pyc_frw into lx_pyc_frw.
        raise exception type cx_pyc_cont exporting previous = lx_pyc_frw.
      catch cx_pyd_fnd into lx_pyd_fnd.
        raise exception type cx_pyc_cont exporting previous = lx_pyd_fnd.
    endtry.

  endmethod.
ENDCLASS.
