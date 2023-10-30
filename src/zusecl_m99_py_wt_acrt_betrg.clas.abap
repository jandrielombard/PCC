class ZUSECL_M99_PY_WT_ACRT_BETRG definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.

  methods READ_CHECK_DATA
    importing
      !IT_PERNR type HR99S_PERNR_RANGE .
  protected section.

    types:
      begin of ty_data_result,
        dct_pernr type p_pernr,
        abnum     type p13_abnum,
        brnum     type p13_brnum,
        wflag     type wflag,
        fpper     type fpper,
        lgart     type lgart,
        betrg     type maxbt,
      end of ty_data_result .
    types:
      tty_data_result type table of ty_data_result .
    types:
      begin of ty_result_count,
        dct_pernr type p_pernr,
        count     type anzhl,
      end of ty_result_count .
    types:
      tty_result_count type table of ty_result_count .
    types:
      begin of ty_data_abn,
        dct_pernr type p_pernr,
        abnum     type p13_abnum,
        brnum     type p13_brnum,
        wflag     type wflag,
        lgart     type lgart,
        betrg     type maxbt,
      end of ty_data_abn .
    types:
      tty_data_abn type table of ty_data_abn .
    types:
      begin of tyl_s_rt_with_text,
        groupid    type pyd_cont_groupid,
        group_name type pyd_name,
        pernr      type p_pernr,
        lgart      type lgart,
        abnum      type p13_abnum,
        brnum      type p13_brnum,
        wflag      type wflag,
        betrg      type maxbt,
        anzhl      type anzhl,
        betpe      type betpe,
        lgtxt      type lgtxt,
        fpper      type fpper,
        zeinh      type pt_zeinh,
        amt_curr   type waers,
        rte_curr   type waers,
        exclude    type p13_exclude,
      end of tyl_s_rt_with_text .
    types:
      tyl_t_rt_with_text type standard table of tyl_s_rt_with_text .

    constants mc_itemid_numerr type pyd_itemid value 'NUMERR' ##NO_TEXT.
    constants mc_lgart_r type pyd_par_type value 'Z99_LGART' ##NO_TEXT.
    constants mc_lgart_ind type pyd_par_type value 'Z99_LGART_IND' ##NO_TEXT.
    constants mc_proc_class type pyd_par_type value 'Z99_PROC_CLASS' ##NO_TEXT.
    constants mc_eval_class type pyd_par_type value 'Z99_EVAL_CLASS' ##NO_TEXT.
    constants mc_betrg_high type pyd_par_type value 'Z99_AMT_HIGH' ##NO_TEXT.
    constants mc_betrg_low type pyd_par_type value 'Z99_AMT_LOW' ##NO_TEXT.
    data mt_lgart_betrg type /iwbep/t_cod_select_options .
    data ms_lgart type /iwbep/s_cod_select_option .
    data mv_compare_betrg_high type betrg .
    data mv_compare_betrg_low type betrg .
    data mt_eval_class type /iwbep/t_cod_select_options .
    data mt_proc_class type /iwbep/t_cod_select_options .
    data mv_eval_class type char4 .
    data mv_proc_class type char3.
    data mv_lgart_ind type char1 .
    data mt_bat_abn type tty_data_abn .
    data mt_all_abn type tty_data_abn .
    data ms_abn type ty_data_abn .
    data mt_bat_result_count type tty_result_count .
    data mt_all_result_count type tty_result_count .
    data mv_stp_year type pabrj .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
    methods sap_swt
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PY_WT_ACRT_BETRG IMPLEMENTATION.


  method CHECK.
* Performing the actual check
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lt_all_result_count type tty_result_count,
          lt_all_abn          type tty_data_abn,
          lt_result_count     type tty_result_count,
          ls_result_count     type ty_result_count,
          ls_result           type ty_s_result.
    data: lt_pernr type tty_pernr.
    data: lv_abs_objname  type abap_abstypename.
    data: lv_objname  type seoclsname.

    refresh mt_objdata.
* Read Employee Numbers
    if mv_tpy_res is not initial.  "Test Payroll
      select hrdct_tpy_rgdir~dct_pernr as dct_pernr
        into corresponding fields of table @lt_pernr
        from hrdct_tpy_rgdir
       where hrdct_tpy_rgdir~dct_pernr in @it_pernr_so
         and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
         and hrdct_tpy_rgdir~inper = @mv_payroll_period
        %_hints adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")'.

    else.                          "Production Payroll
      select p2rx_eval_period~dct_pernr as dct_pernr
        into corresponding fields of table @lt_pernr
        from p2rx_eval_period
       where p2rx_eval_period~dct_pernr in @it_pernr_so
         and p2rx_eval_period~abkrs in @mt_payroll_areas
         and p2rx_eval_period~inper = @mv_payroll_period
        %_hints adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")'.
    endif.

    sort lt_pernr by dct_pernr.
    delete adjacent duplicates from lt_pernr.

    lv_abs_objname = cl_abap_classdescr=>get_class_name( me ).
    split lv_abs_objname at mc_equal into data(lv_clstext) lv_objname.

* Perform Parallel Processing
    call method me->multithread_process
      exporting
        iv_clsname = lv_objname
        it_pernr   = lt_pernr.

* Collect the data
    data: lo_cloned_chkobj type ref to zusecl_m99_py_wt_acrt_betrg.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.
          append lines of lo_cloned_chkobj->mt_bat_result_count to lt_all_result_count.
          append lines of lo_cloned_chkobj->mt_bat_abn to lt_all_abn.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref.  "MOD001++
      endtry.

    endloop.

* Collect data for Overview
    append lines of lt_all_result_count to me->mt_all_result_count.
    append lines of lt_all_abn to me->mt_all_abn.

* Build results table
    sort lt_all_result_count by dct_pernr ascending.
    delete adjacent duplicates from lt_all_result_count comparing dct_pernr.

    loop at lt_all_result_count into ls_result_count.
      ls_result-id = ls_result_count-dct_pernr.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      append ls_result to rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Populating Overview screen
    data:
      ls_err_ov     type ty_s_err_ov,
      ls_sfo        type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif      type abap_bool,
      lv_pernr      type p_pernr,
      lv_value      type pyd_item_value,
      lv_char_value type char060,
      lv_number(20) type c.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read or if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute..

        loop at ct_err_ov into ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
          if ls_err_ov-sfo_tab is initial.
            clear ls_err_ov-sfo_tab.
            clear ls_sfo.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_numerr.
            ls_sfo-value = text-001.
            ls_sfo-text = text-002.
            insert ls_sfo into table ls_err_ov-sfo_tab.

            loop at mt_all_abn into ms_abn where dct_pernr = lv_pernr.

              add 1 to ls_sfo-row_id.
              ls_sfo-itemid = mc_itemid_numerr.
              ls_sfo-value = ms_abn-abnum.
              ls_sfo-text = text-003.
              insert ls_sfo into table ls_err_ov-sfo_tab.

              add 1 to ls_sfo-row_id.
              ls_sfo-itemid = mc_itemid_numerr.
              ls_sfo-value = ms_abn-lgart.
              ls_sfo-text = text-004.
              insert ls_sfo into table ls_err_ov-sfo_tab.

              add 1 to ls_sfo-row_id.
              ls_sfo-itemid = mc_itemid_numerr.
              write ms_abn-betrg to lv_char_value left-justified.
              ls_sfo-value = lv_char_value.
              ls_sfo-text = text-005.
              insert ls_sfo into table ls_err_ov-sfo_tab.

            endloop.

            modify ct_err_ov from ls_err_ov.

          endif.

        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Populating custom paramters for the validation
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
*002 |06-APR-2023 |1130848  |Accept Multiple Eval / Proc  |CFAK903066   *
*-----------------------------------------------------------------------*
    data: lt_param_so type /iwbep/t_cod_select_options,
          ls_par      type if_pyd_fnd_types=>ty_s_resp,
          lt_wt       type table of t512w,
          lv_index    type i.
    data: lv_molga type molga.

    try .
        lv_molga = mo_context->ms_inst-molga.       "MOD001++
* Wage Types for BETRG
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_r.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low(4)   ).
          append lines of lt_param_so to mt_lgart_betrg.
        endloop.


* Amount Low Values
        mv_compare_betrg_low =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_betrg_low
                                                                      it_par      = mo_context->mt_par ).

* Amount High Values
        mv_compare_betrg_high =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_betrg_high
                                                                       it_par      = mo_context->mt_par ).

* Evaluation Class
*        mv_eval_class =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_eval_class     "MOD002--
*                                                               it_par      = mo_context->mt_par ). "MOD002--
        loop at mo_context->mt_par into ls_par where par_type = me->mc_eval_class.                  "MOD002++
          clear lt_param_so.                                                                        "MOD002++
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).              "MOD002++
          append lines of lt_param_so to mt_eval_class.                                             "MOD002++
        endloop.                                                                                    "MOD002++

* Processing Class
*        mv_proc_class =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_proc_class     "MOD002--
*                                                               it_par      = mo_context->mt_par ). "MOD002--
        loop at mo_context->mt_par into ls_par where par_type = me->mc_proc_class.                  "MOD002++
          clear lt_param_so.                                                                        "MOD002++
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).              "MOD002++
          append lines of lt_param_so to mt_proc_class.                                             "MOD002++
        endloop.                                                                                    "MOD002++

* Individual WT indicator
        mv_lgart_ind =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_lgart_ind
                                                              it_par      = mo_context->mt_par ).

* Check WTs
*        if mv_eval_class is not initial.                      "MOD002--
        if not mt_eval_class is initial.                       "MOD002++
          loop at mt_eval_class into data(ms_eval_class).      "MOD002++
            mv_eval_class = ms_eval_class-low.                 "MOD002++

            lv_index = 2 * mv_eval_class(2) - 2.
            select lgart aklas into corresponding fields of table lt_wt
*            from t512w where molga = mc_molga_au and          "MOD001--
              from t512w where molga = lv_molga and            "MOD001++
                               endda >= mv_begda and
                               begda <= mv_endda.

            loop at lt_wt into data(ls_wt) where aklas+lv_index(2) = mv_eval_class+2(2).
              clear lt_param_so.
              lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_wt-lgart ).
              append lines of lt_param_so to mt_lgart_betrg.
            endloop.
          endloop.                                                "MOD002++
        endif.

*        if mv_proc_class is not initial.                         "MOD002--
        if not mt_proc_class is initial.                          "MOD002++
          loop at mt_proc_class into data(ms_proc_class).         "MOD002++
            mv_proc_class = ms_proc_class-low.                    "MOD002++

            lv_index = mv_proc_class(2) - 1.
            select lgart vklas into corresponding fields of table lt_wt
*            from t512w where molga = mc_molga_au and          "MOD001--
              from t512w where molga = lv_molga and            "MOD001++
                               endda >= mv_begda and
                               begda <= mv_endda.

            loop at lt_wt into data(ls_wt1) where vklas+lv_index(1) = mv_proc_class+2(1).
              clear lt_param_so.
              lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_wt1-lgart ).
              append lines of lt_param_so to mt_lgart_betrg.
            endloop.
          endloop.                                                "MOD002++
        endif.

* Build SWT Wage type list
        append lines of mt_lgart_betrg to mt_swt_lgart.

        mv_swt_exc_retro = abap_true.
* STP Year
        mv_stp_year = mv_payroll_period+0(4).
* STP year is the payroll year +1
        mv_stp_year = mv_stp_year + 1.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method READ_CHECK_DATA.
* Called in RFC to perform data selection and populate result validation table
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Changes to P2RQ_ACRT Table |CFAK903066   *
*    |            |         |Data Read                  |             *
*---------------------------------------------------------------------*
    data: lt_data_result       type standard table of ty_data_result,
          lt_data_result_2     type standard table of ty_data_result,
          lt_data_result_lgart type standard table of ty_data_result,
          ls_data_result       type ty_data_result,
          lv_sum               type betrg,
          ls_result_count      type ty_result_count.
    types: begin of ty_t5qso_betrg,
             pernr   type t5qso-pernr,
             betrg   type t5qso-betrg,
             exclude type t5qso-exclude.
    types: end of ty_t5qso_betrg.
    data: lt_t5qso_betrg type table of ty_t5qso_betrg,
          ls_t5qso_betrg type ty_t5qso_betrg.
    data lv_raise_alert type boolean.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.

      "LGART Total current period
      select p2rq_acrt~dct_pernr, p2rq_acrt~abnum, p2rq_acrt~brnum, p2rq_acrt~wflag,
            hrdct_tpy_rgdir~fpper, p2rq_acrt~lgart,
*            sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1         "MOD001--
*                          else betrg end ) as betrg                          "MOD001--
              sum( p2rq_acrt~betrg ) as betrg                                 "MOD001++
              into corresponding fields of table @lt_data_result
                       from hrdct_tpy_rgdir inner join p2rq_acrt
                                     on hrdct_tpy_rgdir~dct_pernr eq p2rq_acrt~dct_pernr
                                    and hrdct_tpy_rgdir~dct_seqnr eq p2rq_acrt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr and
                   hrdct_tpy_rgdir~abkrs in @mt_payroll_areas and
                   hrdct_tpy_rgdir~dct_is_tpy eq @abap_true   and             "MOD001++
                   hrdct_tpy_rgdir~fpper = @mv_payroll_period and             "MOD001++
                   hrdct_tpy_rgdir~inper = @mv_payroll_period and
                   p2rq_acrt~lgart in @mt_lgart_betrg
               and exists ( select 1
                           from p2rx_wpbp
                           where p2rx_wpbp~dct_pernr eq p2rq_acrt~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rq_acrt~dct_seqnr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rq_acrt~dct_pernr, p2rq_acrt~abnum, p2rq_acrt~brnum, p2rq_acrt~wflag, hrdct_tpy_rgdir~fpper, p2rq_acrt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      append lines of lt_data_result to lt_data_result_2.
      sort lt_data_result_2 by dct_pernr ascending.
      clear lt_data_result.
      loop at lt_data_result_2 into data(ls_data_result_2).
        collect ls_data_result_2 into lt_data_result.
      endloop.


    else. "Production Payroll

      "LGART Total inc retro
      select p2rq_acrt~dct_pernr, p2rq_acrt~abnum, p2rq_acrt~brnum, p2rq_acrt~wflag,
             p2rx_eval_period~fpper, p2rq_acrt~lgart,
*              sum( case p2rx_eval_period~srtza when 'P' then betrg * -1 else betrg end ) as betrg  "MOD001--
              sum( p2rq_acrt~betrg ) as betrg                                                       "MOD001++
              into corresponding fields of table @lt_data_result
                       from p2rx_eval_period inner join p2rq_acrt
                                     on p2rx_eval_period~dct_pernr eq p2rq_acrt~dct_pernr
                                    and p2rx_eval_period~dct_seqnr eq p2rq_acrt~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr and
                   p2rx_eval_period~abkrs in @mt_payroll_areas and
                   p2rx_eval_period~fpper = @mv_payroll_period and                                  "MOD001++
                   p2rx_eval_period~inper = @mv_payroll_period and
                   p2rq_acrt~lgart in @mt_lgart_betrg
               and exists ( select 1
                           from p2rx_wpbp
                           where p2rx_wpbp~dct_pernr eq p2rq_acrt~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rq_acrt~dct_seqnr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rq_acrt~dct_pernr, p2rq_acrt~abnum, p2rq_acrt~brnum, p2rq_acrt~wflag, p2rx_eval_period~fpper, p2rq_acrt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RQ_ACRT~PCC")'.

    endif.

    sort lt_data_result by dct_pernr abnum fpper lgart.
    delete adjacent duplicates from lt_data_result.
    delete lt_data_result where fpper <> mv_payroll_period.
    delete lt_data_result where betrg eq 0.

* Rebuild the list with out WT details
    lt_data_result_2[] = lt_data_result[].
    lt_data_result_lgart[] = lt_data_result[].

    refresh lt_data_result.
    loop at lt_data_result_2 into data(ls_betrg_data_2).
      if not mv_lgart_ind eq mc_abap_yes.
        clear: ls_betrg_data_2-lgart.
      endif.
      collect ls_betrg_data_2 into lt_data_result.
    endloop.

* Collect Results according to criteria
    loop at lt_data_result into ls_data_result.
      clear lv_raise_alert.
      check ls_data_result-betrg between mv_compare_betrg_low and mv_compare_betrg_high.
* Compare after adding YTD Adjustments
* Read Employee Adjust YTD Amounts for STP Reporting
      select pernr, sum( betrg ) as betrg, exclude
        into corresponding fields of table @lt_t5qso_betrg
        from t5qso
       where stp_year = @mv_stp_year
         and pernr    = @ls_data_result-dct_pernr
         and abnum    = @ls_data_result-abnum
         and brnum    = @ls_data_result-brnum
         and wflag    = @ls_data_result-wflag
         and lgart    = @ls_data_result-lgart
        group by pernr, abnum, brnum, wflag, lgart, exclude.
      if sy-subrc eq 0.
        loop at lt_t5qso_betrg into ls_t5qso_betrg
           where exclude eq abap_false.
          " EXCLUDE flag (T5QSO-EXCLUDE) if = X, then ignore T5QSO record.
          " Else, where an entry is found add the amount of the override
          " to value from the ACRT to update the SUM_VALUE
          ls_data_result-betrg = ls_data_result-betrg + ls_t5qso_betrg-betrg.
        endloop.
        if ls_data_result-betrg
          between mv_compare_betrg_low and mv_compare_betrg_high.
          lv_raise_alert = abap_true.
        endif.
      else.
        lv_raise_alert = abap_true.
      endif.

      check lv_raise_alert eq abap_true.

      ls_result_count-dct_pernr = ls_data_result-dct_pernr.
      ls_result_count-count = 1.
      append ls_result_count to mt_bat_result_count.

      ms_abn-dct_pernr = ls_data_result-dct_pernr.
      ms_abn-abnum = ls_data_result-abnum.
      ms_abn-brnum = ls_data_result-brnum.
      ms_abn-wflag = ls_data_result-wflag.
      ms_abn-betrg = ls_data_result-betrg.
      if not mv_lgart_ind eq mc_abap_yes.
        read table lt_data_result_lgart into data(ls_lgart)
          with key dct_pernr = ls_data_result-dct_pernr
                    abnum = ls_data_result-abnum
                    brnum = ls_data_result-brnum
                    wflag = ls_data_result-wflag
                    fpper = ls_data_result-fpper.
        if sy-subrc = 0.
          ms_abn-lgart = ls_lgart-lgart.
        endif.
      else.
        ms_abn-lgart = ls_data_result-lgart.
      endif.

      append ms_abn to mt_bat_abn.
    endloop.

  endmethod.


  method SAP_SWT.
* Redefined to populate SWT output from ACRT table rather than RT

    types:
      begin of t_lgart,
        sign   type ddsign,
        option type ddoption,
        low    type persa,
        high   type persa,
      end of t_lgart.

    types:
      begin of ty_s_rd,
        row_id type pyd_rowid,
        fpper  type fpper,
        lgart  type lgart,
        abnum  type p13_abnum.
        include type pyd_s_rdswt_ext as ext.
      types:
        text   type pyd_name,
      end of ty_s_rd .

    data: lt_rt     type ty_t_rt_with_text,
          lt_rt_2   type ty_t_rt_with_text,
          ls_rt     type ty_s_rt_with_text,
          ls_swt    type cl_pyd_rd_dto_swt=>ty_s_rd,
          lt_swt    type cl_pyd_rd_dto_swt=>ty_t_rd,
          lv_row_id type pyd_rowid,
          lv_param  type string,
          lv_pernr  type p_pernr,
          lv_curr   type waers.
    data: lt_char_rt type ty_t_rt_char_betpe,
          ls_char_rt type ty_s_rt_char_betpe.

    data lt_s_rd type sorted table of ty_s_rd with non-unique key lgart abnum.
    data ls_s_rd type ty_s_rd.
    data ls_output_rt like line of et_rt.
    data lv_currency type waers.
    data lv_bit type c length 1.
    data lt_t512w type table of t512w.
    data lt_lgart type range of lgart.
    data ls_lgart like line of lt_lgart.
    data lv_molga type molga.
    data mv_persa type persa.
    data row_id type i.
    data ls_par like line of mt_exec_parameters.
    data lt_sel_lgart type range of lgart.
    data ls_sel_lgart like line of lt_sel_lgart.

    field-symbols <ls_t512w> type t512w.

    lv_pernr  = is_rd-id.

    call function 'RH_PM_GET_MOLGA_FROM_PERNR'
      exporting
        pernr           = lv_pernr
        begda           = sy-datum
        endda           = sy-datum
      importing
        molga           = lv_molga
      exceptions
        nothing_found   = 1
        no_active_plvar = 2
        others          = 3.

    "get currency per company code
    call method me->get_currency_by_pernr
      exporting
        iv_pernr = lv_pernr
        iv_endda = mv_endda
      receiving
        rv_curr  = lv_curr.

*>>> WOW Specific Code
    "List of WTs for the Display
    call method get_parameters
      exporting
        it_par         = it_par
        io_res_context = io_res_context.

    if not mt_swt_lgart is initial.
      refresh: lt_sel_lgart.
      loop at mt_swt_lgart into data(ms_swt_lgart).
        move-corresponding ms_swt_lgart to ls_sel_lgart.
        collect ls_sel_lgart into lt_sel_lgart.
      endloop.
    else.
      "Work out what the list of WTs you want to show yourself
      clear ls_lgart.
      ls_lgart-low = '/101'.
      ls_lgart-high = '/101'.
      ls_lgart-option = 'BT'.
      ls_lgart-sign = 'I'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/106'.
      ls_lgart-high = '/106'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/110'.
      ls_lgart-high = '/110'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/116'.
      ls_lgart-high = '/116'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/142'.
      ls_lgart-high = '/142'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/172'.
      ls_lgart-high = '/172'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/559'.
      ls_lgart-high = '/559'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/401'.
      ls_lgart-high = '/4ZZ'.
      append ls_lgart to lt_sel_lgart.
    endif.
*<<< WOW Specific Code

    clear ls_lgart.

    if mv_tpy_res = 'X'. "Test Payroll results

      " WTs for IN period A results
*****Note 2955368 - include rte_curr field from p2rq_acrt in query to correctly display the hourly rate upto 5 decimal places *******
*      SELECT hrdct_tpy_rgdir~fpper AS groupid, hrdct_tpy_rgdir~fpper AS group_name, p2rq_acrt~lgart AS lgart, p2rq_acrt~betpe AS betpe, p2rq_acrt~anzhl AS anzhl, p2rq_acrt~rte_curr AS rte_curr, t512t~lgtxt AS lgtxt,"p2rx_eval_period~fpper as fpper,
*****Note 2959827 - SUM ANZHL field to appropriately display cumulated hours in case there are splits generated in the wagetype *****
**** Note 2964127 - Incorrect Currency conversion *****
      select hrdct_tpy_rgdir~fpper as groupid, hrdct_tpy_rgdir~fpper as group_name,
             p2rq_acrt~lgart as lgart, p2rq_acrt~abnum, p2rq_acrt~brnum, p2rq_acrt~wflag,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then anzhl * -1 else anzhl end ) as anzhl,
            p2rq_acrt~amt_curr as amt_curr,  t512t~lgtxt as lgtxt,"p2rx_eval_period~fpper as fpper,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1 else betrg end ) as betrg
             into corresponding fields of table @lt_rt
                      from p2rq_acrt inner join hrdct_tpy_rgdir
                                    on hrdct_tpy_rgdir~dct_pernr eq p2rq_acrt~dct_pernr
                                   and hrdct_tpy_rgdir~dct_seqnr eq p2rq_acrt~dct_seqnr
                         inner join t512t
                                   on t512t~sprsl = @sy-langu
                                   and t512t~molga = @lv_molga
                                   and t512t~lgart = p2rq_acrt~lgart
            where hrdct_tpy_rgdir~abkrs = @mv_payroll_area and
                  hrdct_tpy_rgdir~dct_pernr = @lv_pernr and
                  hrdct_tpy_rgdir~inper = @mv_payroll_period and
                  ( p2rq_acrt~betrg <> 0 or p2rq_acrt~anzhl <> 0 ) and
                  p2rq_acrt~lgart in @lt_sel_lgart
            group by hrdct_tpy_rgdir~fpper, p2rq_acrt~lgart, p2rq_acrt~abnum,  p2rq_acrt~brnum, p2rq_acrt~wflag, p2rq_acrt~anzhl, p2rq_acrt~amt_curr, t512t~lgtxt .

      append lines of lt_rt to lt_rt_2.
      sort lt_rt_2 ascending.

      refresh lt_char_rt.
      loop at lt_rt_2 into ls_rt.
        move-corresponding ls_rt to ls_char_rt.
        collect ls_char_rt into lt_char_rt.
      endloop.

    else.
*****Note 2955368 - include rte_curr field from p2rq_acrt in query to correctly display the hourly rate upto 5 decimal places *******
*      SELECT hrdct_tpy_rgdir~fpper AS groupid, hrdct_tpy_rgdir~fpper AS group_name, p2rq_acrt~lgart AS lgart, p2rq_acrt~betpe AS betpe, p2rq_acrt~anzhl AS anzhl, p2rq_acrt~rte_curr AS rte_curr, t512t~lgtxt AS lgtxt,"p2rx_eval_period~fpper as fpper,
*****Note 2959827 - SUM ANZHL field to appropriately display cumulated hours in case there are splits generated in the wagetype *****
**** Note 2964127 - Incorrect Currency conversion *****
      select p2rx_eval_period~fpper as groupid, p2rx_eval_period~fpper as group_name,
        p2rq_acrt~lgart as lgart, p2rq_acrt~abnum,  p2rq_acrt~brnum, p2rq_acrt~wflag,
        sum( case p2rx_eval_period~srtza when 'P' then anzhl * -1 else anzhl end ) as anzhl,
        p2rq_acrt~amt_curr as amt_curr, t512t~lgtxt as lgtxt,
     sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                   else betrg end ) as betrg
       into corresponding fields of table @lt_rt
                from p2rq_acrt inner join p2rx_eval_period
                              on p2rx_eval_period~dct_pernr eq p2rq_acrt~dct_pernr
                             and p2rx_eval_period~dct_seqnr eq p2rq_acrt~dct_seqnr
                   inner join t512t
                             on t512t~sprsl = @sy-langu
                             and t512t~molga = @lv_molga
                             and t512t~lgart = p2rq_acrt~lgart
      where p2rx_eval_period~abkrs = @mv_payroll_area and
            p2rx_eval_period~dct_pernr = @lv_pernr and
            p2rx_eval_period~inper = @mv_payroll_period and
            ( p2rq_acrt~betrg <> 0 or p2rq_acrt~anzhl <> 0 ) and
            p2rq_acrt~lgart in @lt_sel_lgart
      group by p2rx_eval_period~fpper, p2rq_acrt~lgart, p2rq_acrt~abnum,  p2rq_acrt~brnum, p2rq_acrt~wflag, p2rq_acrt~anzhl, p2rq_acrt~amt_curr, t512t~lgtxt .

      refresh lt_char_rt.
      loop at lt_rt into ls_rt.
        move-corresponding ls_rt to ls_char_rt.
        collect ls_char_rt into lt_char_rt.
      endloop.
    endif.

    sort lt_char_rt by groupid descending lgart ascending.
* Exclude Retro Pay data
    if mv_swt_exc_retro = abap_true.
      delete lt_char_rt where groupid <> mv_payroll_period.
    endif.
* Apply All Other Validation restrictions
    delete lt_char_rt where betrg eq 0.
    delete lt_char_rt where betrg not between mv_compare_betrg_low and mv_compare_betrg_high.

* add stp adjustment details
    if not lt_char_rt is initial.
      refresh: lt_rt.
      select t5qso~stp_year as groupid,  t5qso~lgart as lgart, t5qso~abnum, t5qso~brnum, t5qso~wflag,
             t5qso~betrg as betrg, t5qso~amt_curr as amt_curr, t5qso~descr as lgtxt, t5qso~exclude as exclude
            into corresponding fields of table @lt_rt
            from t5qso
         for all entries in @lt_char_rt
           where stp_year = @mv_stp_year
             and pernr    = @lv_pernr
             and abnum = @lt_char_rt-abnum
             and brnum = @lt_char_rt-brnum
             and wflag = @lt_char_rt-wflag
             and lgart = @lt_char_rt-lgart.
      loop at lt_rt into ls_rt where exclude eq abap_false.
        clear: ls_char_rt.
        move-corresponding ls_rt to ls_char_rt.
        move mv_payroll_period to:  ls_char_rt-groupid, ls_char_rt-group_name.
        move text-006 to ls_char_rt-lgtxt.
        collect ls_char_rt into lt_char_rt.
      endloop.
    endif.

    refresh et_rt.
    loop at lt_char_rt into ls_char_rt.
      move-corresponding ls_char_rt to ls_rt.
      append ls_rt to et_rt.

      ls_swt-group_name = 'FOR Period ' && '&' && ls_rt-group_name+4(2) && '/' && ls_rt-group_name(4).
      replace '&' with space into ls_swt-group_name.
      ls_swt-groupid = ls_rt-group_name.
      ls_swt-row_id = lv_row_id.
      ls_swt-itemid = ls_rt-lgart.
      ls_swt-text = ls_rt-lgtxt.
      ls_swt-amt  = ls_rt-betrg.
      ls_swt-num  = ls_rt-anzhl.
      "ls_swt-rte  = ls_rt-betpe.
      if ls_rt-amt_curr is initial.
        ls_swt-amt_curr = lv_curr.
      else.
        ls_swt-amt_curr = ls_rt-amt_curr.   " Note 2964127 - Incorrect Currency conversion
      endif.

      add 1 to lv_row_id .
      append ls_swt to lt_swt.



    endloop.

    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_swt.
      catch cx_pyd_fnd .
    endtry.

  endmethod.
ENDCLASS.
