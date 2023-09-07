class zcl_pyc_bpc_sel_fi_recon definition
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
    methods get_other_params
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_BPC_SEL_FI_RECON IMPLEMENTATION.


  method get_leading_time_selection.
* No Need to populate Period Info

  endmethod.


  method get_other_params.
* Populate Period Parameters
    constants: mc_abkrs      type pyd_par_type value 'ABKRS',
               mc_period     type pyd_par_type value 'PERIOD',
               mc_par_abkrs  type rsscr_name value 'P_ABKRS',
               mc_sel_abkrs  type rsscr_name value 'S_ABKRS',
               mc_par_pabrp  type rsscr_name value 'P_PABRP',
               mc_par_pabrj  type rsscr_name value 'P_PABRJ',
               mc_reg_pay    type rsscr_name value 'NOC',
               mc_offc_pay   type rsscr_name value 'YOC',
               mc_par_pccrun type rsscr_name value 'P_PCCRUN'.

    constants:
      mc_generic_selinfo type pyd_par_type value 'Z99_GENERIC_SELINFO',
      mc_sel_separator   type c value ','.
    data lt_par	type if_pyd_fnd_types=>ty_t_resp.
    data ls_par type pyd_s_resp.
    data ls_sel_params   type rsparams.

    constants: mc_i  type tvarv_sign value 'I',
               mc_eq type tvarv_opti value 'EQ'.

    data: lx_pyc_frw type ref to cx_pyc_frw,
          lx_pyd_fnd type ref to cx_pyd_fnd,
          lv_dummy   type string.

    data: lv_sel_name type char8.

* Variant Contents
    data: lv_report      type  rsvar-report,
          lv_variant     type rsvar-variant,
          lt_var_content type table of rsparamsl_255,
          ls_var_content type rsparamsl_255.

* Payroll area and Period
    data: lt_abkrs type zhrau_tt_abkrs.
    data: lv_payroll_area type abkrs,
          lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_abkrs        type abkrs,
          lv_fpper        type faper,
          lt_error        type table of edimessage.

    try .
* Read Payroll Area from Variant
        if iv_var_name is initial.
          raise exception type cx_pyc_cont
            exporting
              textid = cx_pyc_cont=>variant_not_exist.

          message i120(pyc_cont) into lv_dummy  ##STMNT_EXIT.
        endif.

* Read Variant Contents
        clear: lt_var_content.
        lv_report = iv_rpt_name.
        lv_variant = iv_var_name.
        call function 'RS_VARIANT_CONTENTS_255'
          exporting
            report               = lv_report
            variant              = lv_variant
          tables
            valutab              = lt_var_content
          exceptions
            variant_non_existent = 1
            variant_obsolete     = 2
            others               = 3.

* read Period
        read table lt_var_content into ls_var_content
          with key selname = mc_par_abkrs.
        if sy-subrc eq 0.
          move ls_var_content-low to lv_abkrs.
        endif.

* Read Payroll Area.
        if lv_abkrs is initial.
          raise exception type cx_pyc_cont
            exporting
              textid = cx_pyc_cont=>no_abkrs.
          message i221(pyc_cont) into lv_dummy  ##STMNT_EXIT.
        endif.

        " Current Period
        call function 'PA03_PERIODDATES_GET'
          exporting
            f_abkrs               = lv_abkrs
          changing
            f_current_period      = lv_pabrp
            f_current_year        = lv_pabrj
          exceptions
            pcr_does_not_exist    = 1
            abkrs_does_not_exist  = 2
            period_does_not_exist = 3
            others                = 4.

        if sy-subrc <> 0.
          raise exception type cx_pyc_cont
            exporting
              textid = cx_pyc_cont=>no_content.
          message i190(pyc_cont) into lv_dummy  ##STMNT_EXIT.
        endif.

        " Payroll Area
        lt_par  = io_context->mt_par.
        read table lt_par into ls_par with key par_type = mc_generic_selinfo.
        if sy-subrc = 0.
          split ls_par-low at mc_sel_separator into table lt_abkrs.
          loop at lt_abkrs into lv_abkrs.
            " Generic SelInfo
            lv_sel_name = mc_sel_abkrs.
            clear ls_sel_params .
            ls_sel_params-sign = mc_i.
            ls_sel_params-option = mc_eq.
            ls_sel_params-selname = lv_sel_name.
            ls_sel_params-low = lv_abkrs.
            append ls_sel_params to et_sel_params .
          endloop.
        else.
          " Populate from Payroll Area
          lv_sel_name = mc_sel_abkrs.
          clear ls_sel_params.
          ls_sel_params-sign = mc_i.
          ls_sel_params-option = mc_eq.
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low = lv_abkrs.
          append ls_sel_params to et_sel_params .

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
        lv_sel_name = mc_par_pabrj.
        clear ls_sel_params.
        ls_sel_params-selname = lv_sel_name.
        ls_sel_params-low     = lv_pabrj.
        ls_sel_params-sign    = mc_i.
        ls_sel_params-option  = mc_eq.
        append ls_sel_params to et_sel_params.

* Pay Period
        lv_sel_name = mc_par_pabrp.
        clear ls_sel_params.
        ls_sel_params-selname = lv_sel_name.
        ls_sel_params-low     = lv_pabrp.
        ls_sel_params-sign    = mc_i.
        ls_sel_params-option  = mc_eq.
        append ls_sel_params to et_sel_params.

* PCC Run
        lv_sel_name = mc_par_pccrun.
        clear ls_sel_params .
        ls_sel_params-selname = lv_sel_name.
        ls_sel_params-sign = mc_i.
        ls_sel_params-option = mc_eq.
        ls_sel_params-low = abap_true.
        append ls_sel_params to et_sel_params .

      catch cx_pyc_frw into lx_pyc_frw.
        raise exception type cx_pyc_cont exporting previous = lx_pyc_frw.
      catch cx_pyd_fnd into lx_pyd_fnd.
        raise exception type cx_pyc_cont exporting previous = lx_pyd_fnd.
    endtry.
  endmethod.
ENDCLASS.
