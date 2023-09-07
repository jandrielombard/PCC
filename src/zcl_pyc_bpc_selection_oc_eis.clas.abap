class zcl_pyc_bpc_selection_oc_eis definition
  public
  inheriting from cl_pyc_bpc_selection_pnp
  create public .

  public section.

    constants:
      begin of gc_par_type,
        period type pyd_par_type value 'PERIOD',
        abkrs  type pyd_par_type value 'ABKRS',
      end of gc_par_type .
    constants mc_i type tvarv_sign value 'I' ##NO_TEXT.
    constants mc_eq type tvarv_opti value 'EQ' ##NO_TEXT.
  protected section.

    constants: mc_abkrs    type pyd_par_type value 'ABKRS',
               mc_period   type pyd_par_type value 'PERIOD',
               mc_variant  type pyd_par_type value 'VARIANT',
               mc_test_run type pyd_par_type value 'Z99_TEST_RUN'.

    constants: mc_par_ocrsn    type rsscr_name value 'P_OCRSN',
               mc_par_bondt    type rsscr_name value 'P_BONDT',
               mc_par_payty    type rsscr_name value 'P_PAYTY',
               mc_par_payid    type rsscr_name value 'P_PAYID',
               mc_par_pnpxabkr type rsscr_name value 'PNPXABKR',
               mc_par_pnptimed type rsscr_name value 'PNPTIMED',
               mc_par_pnptimr9 type rsscr_name value 'PNPTIMR9',
               mc_par_pnptimra type rsscr_name value 'PNPTIMRA',
               mc_par_pnppabrj type rsscr_name value 'PNPPABRJ',
               mc_par_pnppabrp type rsscr_name value 'PNPPABRP',
               mc_sel_pnpindex type rsscr_name value 'PNPINDEX',
               mc_sel_pnpabkrs type rsscr_name value 'PNPABKRS',
               mc_par_tst_on   type rsscr_name value 'P_TST_ON',
               mc_par_ch_fdown type rsscr_name value 'CH_FDOWN',
               mc_par_p_fname  type rsscr_name value 'P_FNAME',
               mc_fname_text   type string value 'EIexception.CSV'.
    methods get_other_params
        redefinition .
    methods convert_dev_sel_obj_to_params
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_PYC_BPC_SELECTION_OC_EIS IMPLEMENTATION.


  method convert_dev_sel_obj_to_params.
* Populate PNPINDEX selection
    data:
      ls_dev_sel_obj type if_pyd_shadow_access=>ty_s_shadow_item,
      ls_sel_params  type rsparams.

    loop at it_dev_sel_obj into ls_dev_sel_obj where par_type = 'PERNR'.
      clear ls_sel_params.
      move 'I'  to ls_sel_params-sign.
      move 'P'  to ls_sel_params-kind.
      move 'EQ' to ls_sel_params-option.
      move ls_dev_sel_obj-par_val to ls_sel_params-low.
      ls_sel_params-selname = 'PNPINDEX' .
      append ls_sel_params to et_sel_params.
    endloop.
  endmethod.


  method get_other_params.
* Populate Selection Parameters
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

    data: lv_payroll_area type tvarv_val,
          lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_ocrsn        type pay_ocrsn,
          lv_bondt        type sy-datum,
          lv_fname        type string.

* Selection
    try .
        lv_proc_inst_id = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = cl_pyc_rt_facade=>gcs_par_type-proc_inst
                                                                it_par      = io_context->mt_par ).

        lo_sel_access = cl_pyc_selection_oc_access=>get_instance( ).
        lt_selection = lo_sel_access->selection_list_get( iv_proc_inst_id = lv_proc_inst_id ).

        "If no selection object is found, raise exception
        if lt_selection is initial.
          raise exception type cx_pyc_cont
            exporting
              textid = cx_pyc_cont=>oc_empty_selection.

          message i222(pyc_cont) into lv_dummy  ##STMNT_EXIT.
        endif.

        " Inject parameters from selection package infomation
        read table lt_selection into ls_selection with key package_id = iv_rpt_chain_id.
        if sy-subrc = 0.
          clear ls_sel_params.
          ls_sel_params-selname = mc_par_ocrsn .
          ls_sel_params-low     = ls_selection-ocrsn.
          ls_sel_params-sign    = mc_i.
          ls_sel_params-option  = mc_eq.
          append ls_sel_params to et_sel_params.
          lv_ocrsn = ls_selection-ocrsn.

          clear ls_sel_params.
          ls_sel_params-selname = mc_par_bondt.
          ls_sel_params-low     = ls_selection-bondt.
          ls_sel_params-sign    = mc_i.
          ls_sel_params-option  = mc_eq.
          append ls_sel_params to et_sel_params.
          lv_bondt = ls_selection-bondt.

          clear ls_sel_params.
          ls_sel_params-selname = mc_par_payty .
          ls_sel_params-low     = ls_selection-payty.
          ls_sel_params-sign    = mc_i.
          ls_sel_params-option  = mc_eq.
          append ls_sel_params to et_sel_params.

          clear ls_sel_params.
          ls_sel_params-selname = mc_par_payid.
          ls_sel_params-low     = ls_selection-payid.
          ls_sel_params-sign    = mc_i.
          ls_sel_params-option  = mc_eq.
          append ls_sel_params to et_sel_params.

          " fill leading ABKRS and Period
          clear ls_sel_params.
          ls_sel_params-selname = mc_par_pnpxabkr .
          ls_sel_params-low     = ls_selection-abkrs.
          ls_sel_params-sign    = mc_i.
          ls_sel_params-option  = mc_eq.
          append ls_sel_params to et_sel_params.
          lv_payroll_area = ls_selection-abkrs.

          " fill leading period to set it as other period, tbd, need to verify PNP or PNPCE
          if ls_selection-pabrj is initial.
            clear ls_sel_params.
            ls_sel_params-selname = mc_par_pnptimr9 .
            ls_sel_params-kind    = 'P'.
            ls_sel_params-low     = 'X'.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            clear ls_sel_params.
            ls_sel_params-selname = mc_par_pnptimra .
            ls_sel_params-kind    = 'P'.
            ls_sel_params-low     = ''.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
          else.
            clear ls_sel_params.
            ls_sel_params-selname = mc_par_pnptimr9 .
            ls_sel_params-kind    = 'P'.
            ls_sel_params-low     = ''.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            clear ls_sel_params.
            ls_sel_params-selname = mc_par_pnptimra .
            ls_sel_params-kind    = 'P'.
            ls_sel_params-low     = 'X'.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            " fill leading time selection
            clear ls_sel_params.
            ls_sel_params-selname = mc_par_pnppabrj .
            ls_sel_params-low     = ls_selection-pabrj.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
            lv_pabrj = ls_selection-pabrj.

            clear ls_sel_params.
            ls_sel_params-selname = mc_par_pnppabrp .
            ls_sel_params-low     = ls_selection-pabrp.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
            lv_pabrp = ls_selection-pabrp.
          endif.

* Common Parameters
* Personal Number
          loop at lt_selection into ls_selection where package_id = iv_rpt_chain_id.
            clear ls_sel_params.
            ls_sel_params-selname = 'PNPINDEX' .
            ls_sel_params-low     = ls_selection-pernr.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
          endloop.
        endif.

* Download Exceptions File to AS
*        clear ls_sel_params .
*        ls_sel_params-selname = mc_par_ch_fdown.
*        ls_sel_params-kind = 'P' .
*        ls_sel_params-option = mc_eq .
*        ls_sel_params-sign = mc_i .
*        ls_sel_params-low = abap_true.
*        append ls_sel_params to et_sel_params.

* Dataset Name
*        concatenate lv_payroll_area
*          lv_pabrp lv_pabrj lv_ocrsn lv_bondt mc_fname_text into lv_fname.
*
*        clear ls_sel_params .
*        ls_sel_params-selname = mc_par_p_fname.
*        ls_sel_params-kind = 'P' .
*        ls_sel_params-option = mc_eq .
*        ls_sel_params-sign = mc_i .
*        ls_sel_params-low = lv_fname.
*        append ls_sel_params to et_sel_params.

      catch cx_pyc_frw into lx_pyc_frw.
        raise exception type cx_pyc_cont exporting previous = lx_pyc_frw.
      catch cx_pyd_fnd into lx_pyd_fnd.
        raise exception type cx_pyc_cont exporting previous = lx_pyd_fnd.
    endtry.

  endmethod.
ENDCLASS.
