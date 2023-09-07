class zcl_m99_py_retro_periods definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.

    methods read_check_data
      importing
        !it_pernr type hr99s_pernr_range .
  protected section.

    types:
      begin of ty_betrg_data,
        dct_pernr type p_pernr,
        fpper     type fpper,
        lgart     type lgart,
        betrg     type maxbt,
        waers     type waers,
      end of ty_betrg_data .
    types:
      tty_betrg_data type table of ty_betrg_data .
    types:
      begin of ty_data_result,
        dct_pernr type p_pernr,
        fpper     type fpper,
      end of ty_data_result .
    types:
      begin of ty_retro_dtls,
        dct_pernr   type pernr_d,
        fpper       type fpper,
        retro_count type numc3.
    types: end of ty_retro_dtls .
    types:
      tty_retro_dtls type table of ty_retro_dtls .

    constants mc_z99_lgart type pyd_par_type value 'Z99_LGART' ##NO_TEXT.
    constants mc_amt_high type pyd_par_type value 'Z99_AMT_HIGH' ##NO_TEXT.
    constants mc_amt_low type pyd_par_type value 'Z99_AMT_LOW' ##NO_TEXT.
    constants mc_itemid_retroerr type pyd_itemid value 'RETROERR' ##NO_TEXT.
    constants mc_condition_periods type pyd_par_type value 'Z99_CONDITION_RETRO_PERIODS' ##NO_TEXT.
    constants mc_q2 type abkrs value 'Q2' ##NO_TEXT.
    constants mc_q3 type abkrs value 'Q3' ##NO_TEXT.
    constants mc_n3 type abkrs value 'N3' ##NO_TEXT.
    data mv_condition_periods type numc3 .
    data mt_retro_dtls type tty_retro_dtls .
    data ms_retro_dtls type ty_retro_dtls .
    data mt_lgart type /iwbep/t_cod_select_options .
    data ms_lgart type /iwbep/s_cod_select_option .
    data mv_amt_high type betrg .
    data mv_amt_low type betrg .
    data mt_bat_betrg_data type standard table of ty_betrg_data .
    data mt_all_betrg_data type standard table of ty_betrg_data .
    data ms_betrg_data type ty_betrg_data .
    data mv_period_for type fpper .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PY_RETRO_PERIODS IMPLEMENTATION.


  method check.
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903081   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lv_date_for    type datum,
          ls_result      type ty_s_result,
          lt_data_result type standard table of ty_data_result.

    data: lt_pernr type tty_pernr.
    data: ls_pernr type ty_pernr.
    data: lv_months type  numc3.
    data: lv_abs_objname  type abap_abstypename.
    data: lv_objname  type seoclsname.
    data: lt_betrg_data type standard table of ty_betrg_data,
          ls_betrg_data type ty_betrg_data.

    refresh mt_objdata.
    " Find period date to check if employee has retro prior
    case mv_payroll_area.
      when mc_q2.
        lv_date_for = mv_begda - mv_condition_periods * 14.
      when mc_q3 or mc_n3.
        lv_months = mv_condition_periods.
        call function 'CCM_GO_BACK_MONTHS'
          exporting
            currdate   = mv_begda
            backmonths = lv_months
          importing
            newdate    = lv_date_for.
      when others.
        lv_date_for = mv_begda - mv_condition_periods * 7.
    endcase.

    "Get pay period of that retro date
    cl_hr_payroll_area=>get_instance( mv_payroll_area )->get_periode_with_date(
      exporting
        imp_date   = lv_date_for
      receiving
        ret_period = mv_period_for ).

    "Check if a Test Payroll
    if mv_tpy_res is not initial.

      "Find employees with pay results prior to that period
      select hrdct_tpy_rgdir~dct_pernr, hrdct_tpy_rgdir~fpper
              into corresponding fields of table @lt_data_result
              from hrdct_tpy_rgdir
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr_so
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~fpper <= @mv_period_for
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                           where p2rx_wpbp_index~dct_pernr eq hrdct_tpy_rgdir~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq hrdct_tpy_rgdir~dct_seqnr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by hrdct_tpy_rgdir~dct_pernr, hrdct_tpy_rgdir~fpper
            %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' .


    else. "Production Payroll

      "Find employees with pay results prior to that period
      select p2rx_eval_period~dct_pernr, p2rx_eval_period~fpper
              into corresponding fields of table @lt_data_result
                       from p2rx_eval_period
             where p2rx_eval_period~dct_pernr in @it_pernr_so
               and p2rx_eval_period~abkrs in @mt_payroll_areas
               and p2rx_eval_period~fpper <= @mv_period_for
               and p2rx_eval_period~inper = @mv_payroll_period
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_eval_period~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_eval_period~dct_seqnr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_eval_period~dct_pernr, p2rx_eval_period~fpper
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")'.

    endif.

    sort: lt_data_result by dct_pernr fpper descending.
    loop at lt_data_result into data(ls_data_result).
      append ls_data_result-dct_pernr to lt_pernr.
    endloop.

* Read Payroll results for the Employees and filter out Emoployees with
* Acceptable payments in the period.
    sort lt_pernr by dct_pernr.
    delete adjacent duplicates from lt_pernr.
    if not lt_pernr is initial.
* Perform Parallel Processing
      lv_abs_objname = cl_abap_classdescr=>get_class_name( me ).
      split lv_abs_objname at mc_equal into data(lv_clstext) lv_objname.
      call method me->multithread_process
        exporting
          iv_clsname = lv_objname
          it_pernr   = lt_pernr.

* Collect the data
      data: lo_cloned_chkobj type ref to zcl_m99_py_retro_periods.
      loop at mt_objdata into ms_objdata.
        try.
            call method me->convert_objdata
              exporting
                is_objdata       = ms_objdata
              changing
                io_cloned_object = lo_cloned_chkobj.
            append lines of lo_cloned_chkobj->mt_bat_betrg_data to lt_betrg_data.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref.  "MOD001++
        endtry.
      endloop.
    endif.

* Collect data for Overview
    append lines of lt_betrg_data to mt_all_betrg_data.

* Prepare the error list
    sort lt_betrg_data by dct_pernr ascending.
    delete adjacent duplicates from lt_betrg_data comparing dct_pernr.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_betrg_data into ls_betrg_data.
      move ls_betrg_data-dct_pernr to ls_result-id.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Method for Overview list display
    data:
      ls_err_ov     type ty_s_err_ov,
      ls_sfo        type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif      type abap_bool,
      lv_pernr      type p_pernr,
      lv_value      type pyd_item_value,
      lv_text       type pyd_name,
      lv_number(20) type c.

    data lv_retro_count type i.
    data lt_data_result   type standard table of ty_data_result.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
* Read Parameters
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
* Read Employee Retro details
          if mv_tpy_res is not initial.
            "Find employees with pay results prior to that period
            select hrdct_tpy_rgdir~dct_pernr, hrdct_tpy_rgdir~fpper
              into corresponding fields of table @lt_data_result
              from hrdct_tpy_rgdir
             where hrdct_tpy_rgdir~dct_pernr = @lv_pernr
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and hrdct_tpy_rgdir~fpper <> @mv_payroll_period.
          else. "Production Payroll
            "Find employees with pay results prior to that period
            select p2rx_eval_period~dct_pernr, p2rx_eval_period~fpper
              into corresponding fields of table @lt_data_result
              from p2rx_eval_period
             where p2rx_eval_period~dct_pernr = @lv_pernr
               and p2rx_eval_period~fpper <> @mv_payroll_period
               and p2rx_eval_period~inper = @mv_payroll_period.
          endif.
          sort lt_data_result by fpper ascending.
          read table lt_data_result into data(ls_data_result) index 1.
          describe table lt_data_result lines lv_retro_count.

          clear ls_err_ov-sfo_tab.
          lv_text = text-001.
          message i018(zhrpy_pcc_msg)
             with ls_data_result-fpper+4(2) ls_data_result-fpper+0(4) lv_retro_count into lv_value.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_retroerr
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.


      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_specifc_custmizing.
* Read Check Specific Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.
    data ls_par       type if_pyd_fnd_types=>ty_s_resp.
    data: lt_wt    type table of t512w,
          lv_index type i.

    try .
* Condition Periods
        mv_condition_periods =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_condition_periods
                                                                      it_par      = mo_context->mt_par ).
* Wage Types from Z99_LGART
        refresh: mt_lgart.
        loop at mo_context->mt_par into ls_par where par_type = me->mc_z99_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart.
        endloop.

* Amount Low Value
        mv_amt_low =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_amt_low
                                                              it_par    = mo_context->mt_par ).

* Amount High Value
        mv_amt_high =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_amt_high
                                                               it_par    = mo_context->mt_par ).

* Build SWT Wage type list
        refresh: mt_swt_lgart.
        append lines of mt_lgart to mt_swt_lgart.

* Exclude retro payments display
        mv_swt_exc_retro = abap_true.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.


  method read_check_data.
* called in rfc to perform data selection and populate result validation table

    data: lt_betrg_data   type standard table of ty_betrg_data,
          lt_betrg_data_2 type standard table of ty_betrg_data.

    data: lv_curr type waers.
    data  lv_retcd type sy-subrc.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.
      "LGART Total current period
      refresh: lt_betrg_data.
      select p2rx_rt~dct_pernr as dct_pernr, hrdct_tpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1
                          else betrg end ) as betrg
              into corresponding fields of table @lt_betrg_data
                       from hrdct_tpy_rgdir inner join p2rx_rt
                        on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                       and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~fpper = @mv_payroll_period
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and p2rx_rt~lgart in @mt_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                             and ( p2rx_wpbp~stat2 = @mc_stat2_active and p2rx_wpbp~endda >= @mv_endda )
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_rt~dct_pernr, hrdct_tpy_rgdir~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      delete lt_betrg_data where lgart not in mt_lgart.

*      "LGART Total from retros
*      refresh: lt_betrg_data_2.
*      select p2rx_rt~dct_pernr as dct_pernr, hrpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
*            sum( case hrpy_rgdir~srtza when 'A' then betrg * -1
*                          else 0 end ) as betrg
*              into corresponding fields of table @lt_betrg_data_2
*                from hrdct_tpy_rgdir inner join hrpy_rgdir
*                      on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
*                      and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
*                    inner join p2rx_rt
*                      on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
*                      and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
*             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
*               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
*               and hrdct_tpy_rgdir~fpper = @mv_payroll_period
*               and hrdct_tpy_rgdir~inper = @mv_payroll_period
*               and p2rx_rt~lgart in @mt_lgart
*               and exists ( select 1
*                           from p2rx_wpbp_index
*                           inner join p2rx_wpbp
*                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
*                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
*                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
*                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
*                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
*                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
*                             and ( p2rx_wpbp~stat2 = @mc_stat2_active and p2rx_wpbp~endda >= @mv_endda )
*                             and p2rx_wpbp~bukrs in @mt_bukrs
*                             and p2rx_wpbp~werks in @mt_werks
*                             and p2rx_wpbp~btrtl in @mt_btrtl
*                             and p2rx_wpbp~persg in @mt_persg
*                             and p2rx_wpbp~persk in @mt_persk
*                             and p2rx_wpbp~kostl in @mt_kostl )
*             group by p2rx_rt~dct_pernr, hrpy_rgdir~fpper, p2rx_rt~lgart
*             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.
*
*      delete lt_betrg_data_2 where lgart not in mt_lgart.

      append lines of lt_betrg_data to lt_betrg_data_2.
      sort lt_betrg_data_2 by dct_pernr  fpper lgart.

      refresh lt_betrg_data.
      loop at lt_betrg_data_2 into data(ls_betrg_data).
        collect ls_betrg_data into lt_betrg_data.
      endloop.

    else. "Production Payroll

      "LGART Total Inc retro
      refresh: lt_betrg_data.
      select p2rx_rt~dct_pernr as dct_pernr, p2rx_eval_period~fpper as fpper,
             p2rx_rt~lgart as lgart,
            sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                          else betrg end ) as betrg
              into corresponding fields of table @lt_betrg_data
              from p2rx_eval_period inner join p2rx_rt
                on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
               and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr
               and p2rx_eval_period~abkrs in @mt_payroll_areas
               and p2rx_eval_period~fpper = @mv_payroll_period
               and p2rx_eval_period~inper = @mv_payroll_period
               and p2rx_rt~lgart in @mt_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                                 inner join p2rx_wpbp
                                    on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                                   and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                                   and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                             and ( p2rx_wpbp~stat2 = @mc_stat2_active and p2rx_wpbp~endda >= @mv_endda )
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_rt~dct_pernr, p2rx_eval_period~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      delete lt_betrg_data where lgart not in mt_lgart.
    endif.

    sort lt_betrg_data by dct_pernr fpper lgart.
    delete lt_betrg_data where betrg eq 0.

* Rebuild the list with out WT details
    lt_betrg_data_2 = lt_betrg_data.
    refresh lt_betrg_data.
    loop at lt_betrg_data_2 into data(ls_betrg_data_2).
      collect ls_betrg_data_2 into lt_betrg_data.
    endloop.

* Check the Amount and Report the Error
    loop at lt_betrg_data into ls_betrg_data.
      check ls_betrg_data-betrg between mv_amt_low and mv_amt_high.
      append ls_betrg_data to mt_bat_betrg_data.
    endloop.

  endmethod.
ENDCLASS.
