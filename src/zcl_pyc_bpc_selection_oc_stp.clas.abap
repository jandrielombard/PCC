class ZCL_PYC_BPC_SELECTION_OC_STP definition
  public
  inheriting from CL_PYC_BPC_SELECTION_PNP
  create public .

public section.

  constants GC_SUB_CLSNM type CLASSNAME value 'CL_PYC_BPC_SELECTION_OC_PNP_P' ##NO_TEXT.
protected section.

  methods GET_OTHER_PARAMS
    redefinition .
  methods CONVERT_DEV_SEL_OBJ_TO_PARAMS
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_PYC_BPC_SELECTION_OC_STP IMPLEMENTATION.


  METHOD CONVERT_DEV_SEL_OBJ_TO_PARAMS.
* Populate PNPINDEX selection
    DATA:
      ls_dev_sel_obj TYPE if_pyd_shadow_access=>ty_s_shadow_item,
      ls_sel_params  TYPE rsparams.

    LOOP AT it_dev_sel_obj INTO ls_dev_sel_obj WHERE par_type = 'PERNR'.
      CLEAR ls_sel_params.
      MOVE 'I'  TO ls_sel_params-sign.
      MOVE 'P'  TO ls_sel_params-kind.
      MOVE 'EQ' TO ls_sel_params-option.
      MOVE ls_dev_sel_obj-par_val TO ls_sel_params-low.
      ls_sel_params-selname = 'PNPINDEX' .
      APPEND ls_sel_params TO et_sel_params.
    ENDLOOP.
  ENDMETHOD.


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
    data lt_par	type if_pyd_fnd_types=>ty_t_resp.

    constants: mc_sel_bondt    type rsscr_name value 'PNPBONDT',
               mc_sel_payty    type rsscr_name value 'PNPPAYTY',
               mc_sel_payid    type rsscr_name value 'PNPPAYID',
               mc_sel_pnpxabkr type rsscr_name value 'PNPXABKR',
               mc_sel_pnptimed type rsscr_name value 'PNPTIMED',
               mc_sel_pnptimr9 type rsscr_name value 'PNPTIMR9',
               mc_sel_pnptimra type rsscr_name value 'PNPTIMRA',
               mc_sel_pnppabrj type rsscr_name value 'PNPPABRJ',
               mc_sel_pnppabrp type rsscr_name value 'PNPPABRP',
               mc_sel_pnpindex type rsscr_name value 'PNPINDEX',
               mc_par_submit   type rsscr_name value 'SUBMIT',
               mc_par_update   type rsscr_name value 'UPDATE',
               mc_par_runty    type rsscr_name value 'RUNTY',
               mc_par_spool    type rsscr_name value 'SPOOL',
               mc_test_run     type pyd_par_type value 'Z99_TEST_RUN'.
    data: lv_sel_name   type char8.

    constants: mc_i  type tvarv_sign value 'I',
               mc_eq type tvarv_opti value 'EQ'.
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

        " Inject parameters from selection package infomation
        read table lt_selection into ls_selection with key package_id = iv_rpt_chain_id.
        if sy-subrc = 0.
          if ls_selection-pabrj is initial.
            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_pnptimed .
            ls_sel_params-kind    = 'P'.
            ls_sel_params-low     = '3'.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_bondt .
            ls_sel_params-low     = ls_selection-bondt.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_payty .
            ls_sel_params-low     = ls_selection-payty.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_payid .
            ls_sel_params-low     = ls_selection-payid.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_pnpxabkr .
            ls_sel_params-low     = ls_selection-abkrs.
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

            " fill leading ABKRS and Period
            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_pnpxabkr .
            ls_sel_params-low     = ls_selection-abkrs.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            " fill leading period to set it as other period, tbd, need to verify PNP or PNPCE
            if iv_rpt_is_pnpce eq abap_true.
              clear ls_sel_params.
              ls_sel_params-selname = mc_sel_pnptimed .
              ls_sel_params-kind    = 'S'.
              ls_sel_params-low     = '2'.
              ls_sel_params-sign    = mc_i.
              ls_sel_params-option  = mc_eq.
              append ls_sel_params to et_sel_params.
            else.
              clear ls_sel_params.
              ls_sel_params-selname = mc_sel_pnptimr9 .
              ls_sel_params-kind    = 'P'.
              ls_sel_params-low     = ''.
              ls_sel_params-sign    = mc_i.
              ls_sel_params-option  = mc_eq.
              append ls_sel_params to et_sel_params.

              clear ls_sel_params.
              ls_sel_params-selname = mc_sel_pnptimra .
              ls_sel_params-kind    = 'P'.
              ls_sel_params-low     = 'X'.
              ls_sel_params-sign    = mc_i.
              ls_sel_params-option  = mc_eq.
              append ls_sel_params to et_sel_params.
            endif.

            " fill leading time selection
            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_pnppabrj .
            ls_sel_params-low     = ls_selection-pabrj.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.

            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_pnppabrp .
            ls_sel_params-low     = ls_selection-pabrp.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
          endif.

* Common Parameters
* Personal Number
          loop at lt_selection into ls_selection where package_id = iv_rpt_chain_id.
            clear ls_sel_params.
            ls_sel_params-selname = mc_sel_pnpindex .
            ls_sel_params-low     = ls_selection-pernr.
            ls_sel_params-sign    = mc_i.
            ls_sel_params-option  = mc_eq.
            append ls_sel_params to et_sel_params.
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

* Set Run type
          loop at lt_par into data(ls_par).
            case ls_par-par_type.
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

* Parameter Spool Down load
          lv_sel_name = mc_par_spool.
          clear ls_sel_params .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-kind = 'P' .
          ls_sel_params-option = 'EQ' .
          ls_sel_params-sign = 'I' .
          ls_sel_params-low = abap_true.
          append ls_sel_params to et_sel_params.
        endif.

      catch cx_pyc_frw into lx_pyc_frw.
        raise exception type cx_pyc_cont exporting previous = lx_pyc_frw.
      catch cx_pyd_fnd into lx_pyd_fnd.
        raise exception type cx_pyc_cont exporting previous = lx_pyd_fnd.
    endtry.

  endmethod.
ENDCLASS.
