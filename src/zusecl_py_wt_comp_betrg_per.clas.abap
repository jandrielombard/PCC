class ZUSECL_PY_WT_COMP_BETRG_PER definition
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
    begin of ty_payroll_result,
        dct_pernr type p_pernr,
        inper     type iperi,
        fpper     type fpper,
        lgart     type lgart,
        betrg     type maxbt,
      end of ty_payroll_result .
  types:
    tty_payroll_result type table of ty_payroll_result .
  types:
    begin of ty_sum_amt,
        dct_pernr type hrpayco_pernr_char,
        betrg     type maxbt,
      end of ty_sum_amt .
  types:
    tty_sum_amt type table of ty_sum_amt .

  constants MC_Z99_LGART type PYD_PAR_TYPE value 'Z99_LGART' ##NO_TEXT.
  constants MC_Z99_LGART2 type PYD_PAR_TYPE value 'Z99_LGART2' ##NO_TEXT.
  constants MC_CONDITION_LGART type PYD_PAR_TYPE value 'Z99_CONDITION_LGART' ##NO_TEXT.
  data MT_Z99_LGART type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_Z99_LGART2 type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_CONDITION_LGART type /IWBEP/T_COD_SELECT_OPTIONS .
  data MS_Z99_LGART type /IWBEP/S_COD_SELECT_OPTION .
  data MS_Z99_LGART2 type /IWBEP/S_COD_SELECT_OPTION .
  data MV_PREV_PAYROLL_PERIOD type IPERI .
  data MT_BAT_CP_RESULT type TTY_PAYROLL_RESULT .
  data MT_BAT_PP_RESULT type TTY_PAYROLL_RESULT .
  data MT_BAT_CP_SUM type TTY_SUM_AMT .
  data MT_BAT_PP_SUM type TTY_SUM_AMT .
  data MT_BAT_CP_SUM2 type TTY_SUM_AMT .
  data MT_BAT_PP_SUM2 type TTY_SUM_AMT .
  data MT_ALL_CP_SUM type TTY_SUM_AMT .
  data MT_ALL_PP_SUM type TTY_SUM_AMT .
  data MT_ALL_CP_SUM2 type TTY_SUM_AMT .
  data MT_ALL_PP_SUM2 type TTY_SUM_AMT .
  data MS_CP_SUM type TY_SUM_AMT .
  data MS_PP_SUM type TY_SUM_AMT .

  methods READ_CP_PAYROLL_RESULT
    importing
      !IT_PERNR type HR99S_PERNR_RANGE
      !IT_LGART type /IWBEP/T_COD_SELECT_OPTIONS
    exporting
      !ET_PAYROLL_RESULT type TTY_PAYROLL_RESULT .
  methods READ_PP_PAYROLL_RESULT
    importing
      !IT_PERNR type HR99S_PERNR_RANGE
      !IT_LGART type /IWBEP/T_COD_SELECT_OPTIONS
    exporting
      !ET_PAYROLL_RESULT type TTY_PAYROLL_RESULT .
  methods COMPARE_CPPAY_NE_PPPAY
    importing
      !IT_ALL_CP_SUM type TTY_SUM_AMT
      !IT_ALL_PP_SUM type TTY_SUM_AMT
    changing
      !CT_PERNR type HR99S_PERNR_RANGE .
  methods COMPARE_CPPAY_EQ_PPPAY
    importing
      !IT_ALL_CP_SUM type TTY_SUM_AMT
      !IT_ALL_PP_SUM type TTY_SUM_AMT
      !IT_SEL_PERNR type HR99S_PERNR_RANGE
    changing
      !CT_PERNR type HR99S_PERNR_RANGE .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
  methods SAP_SWT
    redefinition .
  private section.

    events cx_pyd_fnd .
ENDCLASS.



CLASS ZUSECL_PY_WT_COMP_BETRG_PER IMPLEMENTATION.


  method CHECK.
* Read Check Data Using Parallel Processing and build result Table
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lt_allcp_result   type tty_payroll_result,
          lt_allpp_result   type tty_payroll_result,
          ls_payroll_result type ty_payroll_result.

    data lt_cp_sum type tty_sum_amt .
    data lt_pp_sum type tty_sum_amt .
    data: ls_cp_sum type ty_sum_amt,
          ls_pp_sum type ty_sum_amt.
    data: ls_result  type ty_s_result.

    data: lt_pernr type tty_pernr.
    data: ls_pernr type ty_pernr.
    data: lt_sel_pernr type hr99s_pernr_range.
    data: ls_sel_pernr type sel_pernr.
    data: lt_result_pernr type hr99s_pernr_range.
    data: ls_result_pernr type sel_pernr.

    data: ls_textid type scx_t100key.
    data  lv_retcd  type sy-subrc.
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

* Perform Parallel Processing
    lv_abs_objname = cl_abap_classdescr=>get_class_name( me ).
    split lv_abs_objname at mc_equal into data(lv_clstext) lv_objname.
    call method me->multithread_process
      exporting
        iv_clsname = lv_objname
        it_pernr   = lt_pernr.

* Collect the data
    data: lo_cloned_chkobj type ref to zusecl_py_wt_comp_betrg_per.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.
          append lines of lo_cloned_chkobj->mt_bat_cp_result to lt_allcp_result.
          append lines of lo_cloned_chkobj->mt_bat_pp_result to lt_allpp_result.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref.  "MOD001++
      endtry.
    endloop.

* First Wage Type Comparison
* Current Period
    loop at lt_allcp_result into ls_payroll_result
      where lgart in mt_z99_lgart.
      move-corresponding ls_payroll_result to ls_cp_sum.
      collect ls_cp_sum into lt_cp_sum.
    endloop.

* Previous Period
    loop at lt_allpp_result into ls_payroll_result
      where lgart in mt_z99_lgart.
      move-corresponding ls_payroll_result to ls_pp_sum.
      collect ls_pp_sum into lt_pp_sum.
    endloop.

* Compare the amount between Curent Period and Previous Period.
    refresh: lt_sel_pernr.
    sort lt_cp_sum by dct_pernr ascending.
    sort lt_pp_sum by dct_pernr ascending.
    call method me->compare_cppay_ne_pppay
      exporting
        it_all_cp_sum = lt_cp_sum
        it_all_pp_sum = lt_pp_sum
      changing
        ct_pernr      = lt_sel_pernr.

* Second Wage Type Comparison
    if not lt_sel_pernr is initial.
      if not mt_z99_lgart2 is initial.
* Current period
        refresh: lt_cp_sum, lt_pp_sum.
        loop at lt_allcp_result into ls_payroll_result
          where lgart in mt_z99_lgart2
            and dct_pernr in lt_sel_pernr.
          move-corresponding ls_payroll_result to ls_cp_sum.
          collect ls_cp_sum into lt_cp_sum.
        endloop.

* Previous Period
        loop at lt_allpp_result into ls_payroll_result
         where lgart in mt_z99_lgart2
           and dct_pernr in lt_sel_pernr.
          move-corresponding ls_payroll_result to ls_pp_sum.
          collect ls_pp_sum into lt_pp_sum.
        endloop.

* Compare the amount between Curent Period and Previous Period.
        refresh: lt_result_pernr.
        sort lt_cp_sum by dct_pernr ascending.
        sort lt_pp_sum by dct_pernr ascending.
        call method me->compare_cppay_eq_pppay
          exporting
            it_all_cp_sum = lt_cp_sum
            it_all_pp_sum = lt_pp_sum
            it_sel_pernr  = lt_sel_pernr
          changing
            ct_pernr      = lt_result_pernr.
      else.
        lt_result_pernr = lt_sel_pernr.
      endif.

* Build result table
      sort lt_result_pernr by low.
      delete adjacent duplicates from lt_result_pernr.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_result_pernr into ls_result_pernr.
        ls_result-id = ls_result_pernr-low.
        append ls_result to rt_result.
      endloop.

    endif.
    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method COMPARE_CPPAY_EQ_PPPAY.
* Compare the CP Pay with PP Pay
    data: ls_cp_sum type ty_sum_amt,
          ls_pp_sum type ty_sum_amt.
    data: ls_sel_pernr type sel_pernr,
          ls_pernr     type sel_pernr.

* Compare the Curent Period amount equal to Previous Period amount.
    refresh: ct_pernr.
    ls_pernr-sign = 'I'. ls_pernr-option = 'EQ'.
    loop at it_all_cp_sum into ls_cp_sum.
      clear: ls_pp_sum.
      read table it_all_pp_sum into ls_pp_sum
      with key dct_pernr = ls_cp_sum-dct_pernr binary search.
      if sy-subrc eq 0.
        if ls_cp_sum-betrg eq ls_pp_sum-betrg.
          ls_pernr-low = ls_pernr-high = ls_cp_sum-dct_pernr.
          append ls_pernr to ct_pernr.
        endif.
      endif.
    endloop.

  endmethod.


  method COMPARE_CPPAY_NE_PPPAY.
* Compare the CP Pay with PP Pay
    data: ls_cp_sum type ty_sum_amt,
          ls_pp_sum type ty_sum_amt.
    data: ls_pernr type sel_pernr.

* Compare the amount between Curent Period and Previous Period.
    refresh: ct_pernr.
    ls_pernr-sign = 'I'. ls_pernr-option = 'EQ'.
    loop at it_all_cp_sum into ls_cp_sum.
      clear: ls_pp_sum.
      read table it_all_pp_sum into ls_pp_sum
      with key dct_pernr = ls_cp_sum-dct_pernr binary search.
      if sy-subrc eq 0.
        if ls_cp_sum-betrg <> ls_pp_sum-betrg.
          ls_pernr-low = ls_pernr-high = ls_cp_sum-dct_pernr.
          append ls_pernr to ct_pernr.
        endif.
      else.
        ls_pernr-low = ls_pernr-high = ls_cp_sum-dct_pernr.
        append ls_pernr to ct_pernr.
      endif.
    endloop.

* Compare the amount between Prev Period and Current Period.
    loop at it_all_pp_sum into ls_pp_sum.
      clear: ls_cp_sum.
      read table it_all_cp_sum into ls_cp_sum
      with key dct_pernr = ls_pp_sum-dct_pernr binary search.
      if sy-subrc eq 0.
        if ls_pp_sum-betrg <> ls_cp_sum-betrg.
          ls_pernr-low = ls_pernr-high = ls_pp_sum-dct_pernr.
          append ls_pernr to ct_pernr.
        endif.
      else.
        ls_pernr-low = ls_pernr-high = ls_pp_sum-dct_pernr.
        append ls_pernr to ct_pernr.
      endif.
    endloop.

  endmethod.


  method ERR_OV_GET_LIST.
* Method for Overview list display
    data:
      ls_err_ov     type ty_s_err_ov,
      ls_sfo        type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif      type abap_bool,
      lv_pernr      type p_pernr,
      lv_value      type pyd_item_value,
      lv_number(20) type c.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
          if ls_err_ov-sfo_tab is initial.
            clear ls_err_ov-sfo_tab.
            clear ls_sfo.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = 'PAYCOMP'.
            ls_sfo-value  = text-001.
            ls_sfo-text = text-002.
            insert ls_sfo into table ls_err_ov-sfo_tab.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = 'PAYCOMP'.
            ls_sfo-value  = text-006.
            ls_sfo-text = ''.
            insert ls_sfo into table ls_err_ov-sfo_tab.

            modify ct_err_ov from ls_err_ov.
          else.

          endif.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Read Check Specific Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.
    data ls_par       type if_pyd_fnd_types=>ty_s_resp.
    data: lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_vabrj        type vabrj,
          lv_vabrp        type vabrp,
          lv_abkrs        type abkrs,
          lv_begda        type begda,
          lv_endda        type endda,
          lo_payroll_area type ref to cl_hr_payroll_area.

    try .
* Condition Wage Types
        loop at mo_context->mt_par into ls_par where par_type = me->mc_condition_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_condition_lgart.
        endloop.

* Wage Types for Comparison
        loop at mo_context->mt_par into ls_par where par_type = me->mc_z99_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_z99_lgart.
        endloop.

* Wage Types for Comparison 2
        loop at mo_context->mt_par into ls_par where par_type = me->mc_z99_lgart2.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_z99_lgart2.
        endloop.
*
        lv_abkrs = mv_payroll_area.
        lo_payroll_area = cl_hr_payroll_area=>get_instance( imp_area = lv_abkrs ).
        lv_pabrj = mv_payroll_period(4).
        lv_pabrp = mv_payroll_period+4(2).

        lo_payroll_area->get_period_info(
          exporting
            imp_pabrj = lv_pabrj
            imp_pabrp = lv_pabrp
          importing
            exp_vabrj = lv_vabrj
            exp_vabrp = lv_vabrp
            exp_begda = lv_begda
            exp_endda = lv_endda ).

        "previous payroll period, used by to calc wage type differences"
        mv_prev_payroll_period = lv_vabrj && lv_vabrp.

* Build SWT Wage type list
        append lines of mt_z99_lgart to mt_swt_lgart.
        append lines of mt_z99_lgart2 to mt_swt_lgart.

* Exclude retro period data display
        mv_swt_exc_retro = abap_false.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method READ_CHECK_DATA.
* Read check data using parallel processing and build result table
    data: lv_cond_lgart_exists type boolean.
    data: lt_allcp_data     type tty_payroll_result,
          lt_allpp_data     type tty_payroll_result,
          ls_payroll_result type ty_payroll_result.

    data: lv_curr type waers.
    data: lt_all_lgart type /iwbep/t_cod_select_options.
    data: lt_pernr type hr99s_pernr_range.
    data: ls_pernr type sel_pernr.

* Read Current Period Payroll Result
    append lines of mt_z99_lgart to lt_all_lgart.
    append lines of mt_z99_lgart2 to lt_all_lgart.
    append lines of mt_condition_lgart to lt_all_lgart.

    if not lt_all_lgart[] is initial.
      call method me->read_cp_payroll_result
        exporting
          it_pernr          = it_pernr
          it_lgart          = lt_all_lgart
        importing
          et_payroll_result = lt_allcp_data.
    endif.

* Identify Employees with Conditional Wage type amount
    ls_pernr-sign = 'I'. ls_pernr-option = 'EQ'.
    loop at lt_allcp_data into ls_payroll_result
      where lgart in mt_condition_lgart.
      ls_pernr-low = ls_pernr-high = ls_payroll_result-dct_pernr.
      collect ls_pernr into lt_pernr.
    endloop.

    delete lt_allcp_data where not dct_pernr in lt_pernr.

* Perform rest of the processes for Employees with Condition Wage types
* Read Previous Payroll Data for the
    if not mt_z99_lgart[] is initial.
      check not lt_pernr is initial.
      call method me->read_pp_payroll_result
        exporting
          it_pernr          = lt_pernr
          it_lgart          = lt_all_lgart
        importing
          et_payroll_result = lt_allpp_data.

    endif.
*
    append lines of lt_allcp_data to mt_bat_cp_result.
    append lines of lt_allpp_data to mt_bat_pp_result.

  endmethod.


  method READ_CP_PAYROLL_RESULT.

    data: lt_payroll_result   type tty_payroll_result,
          lt_payroll_result_2 type tty_payroll_result.

    data: lv_curr type waers.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.

      "LGART Total current period
      refresh: lt_payroll_result.
      select p2rx_rt~dct_pernr as dct_pernr, hrdct_tpy_rgdir~inper as inper,
             hrdct_tpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1
                          else betrg end ) as betrg
              into corresponding fields of table @lt_payroll_result
                       from hrdct_tpy_rgdir inner join p2rx_rt
                         on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                        and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and p2rx_rt~lgart in @it_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_rt~dct_pernr, hrdct_tpy_rgdir~inper, hrdct_tpy_rgdir~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      "LGART Total for retros
      refresh: lt_payroll_result_2.
      select p2rx_rt~dct_pernr as dct_pernr, hrpy_rgdir~inper as inper,
             hrpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case hrpy_rgdir~srtza when 'A' then betrg * -1
                          else 0 end ) as betrg
              into corresponding fields of table @lt_payroll_result_2
               from hrdct_tpy_rgdir inner join hrpy_rgdir
                      on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
                      and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
                    inner join p2rx_rt
                      on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
                      and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and p2rx_rt~lgart in @it_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                              and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_rt~dct_pernr, hrpy_rgdir~inper, hrpy_rgdir~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      append lines of lt_payroll_result to lt_payroll_result_2.
      sort lt_payroll_result_2 by dct_pernr ascending.

      refresh lt_payroll_result.
      loop at lt_payroll_result_2 into data(ls_payroll_result).
        collect ls_payroll_result into lt_payroll_result.
      endloop.

    else. "Production Payroll

      "LGART Total Inc retro
      refresh: lt_payroll_result.
      select p2rx_rt~dct_pernr as dct_pernr, p2rx_eval_period~inper as inper,
            p2rx_eval_period~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                          else betrg end ) as betrg
              into corresponding fields of table @lt_payroll_result
                       from p2rx_eval_period inner join p2rx_rt
                                     on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                                    and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr
               and p2rx_eval_period~abkrs in @mt_payroll_areas
               and p2rx_eval_period~inper = @mv_payroll_period
               and p2rx_rt~lgart in @it_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                                 inner join p2rx_wpbp
                                    on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                                   and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                                   and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                              and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_rt~dct_pernr, p2rx_eval_period~inper, p2rx_eval_period~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

    endif.

    sort lt_payroll_result by dct_pernr inper fpper.
    delete adjacent duplicates from lt_payroll_result.
    delete lt_payroll_result where fpper <> mv_payroll_period.
    et_payroll_result = lt_payroll_result.

  endmethod.


  method READ_PP_PAYROLL_RESULT.

    data: lt_payroll_result   type standard table of ty_payroll_result,
          lt_payroll_result_2 type standard table of ty_payroll_result.

    data: lv_curr type waers.

    "LGART Total Inc retro
    refresh: lt_payroll_result.
    select p2rx_rt~dct_pernr as dct_pernr, p2rx_eval_period~inper as inper,
          p2rx_eval_period~fpper as fpper, p2rx_rt~lgart as lgart,
          sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                        else betrg end ) as betrg
            into corresponding fields of table @lt_payroll_result
                     from p2rx_eval_period inner join p2rx_rt
                                   on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                                  and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
           where p2rx_eval_period~dct_pernr in @it_pernr
             and p2rx_eval_period~abkrs in @mt_payroll_areas
             and p2rx_eval_period~inper = @mv_prev_payroll_period
             and p2rx_rt~lgart in @it_lgart
             and exists ( select 1
                         from p2rx_wpbp_index
                               inner join p2rx_wpbp
                                  on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                                 and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                                 and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                         where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                           and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                           and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                            and p2rx_wpbp~bukrs in @mt_bukrs
                           and p2rx_wpbp~werks in @mt_werks
                           and p2rx_wpbp~btrtl in @mt_btrtl
                           and p2rx_wpbp~persg in @mt_persg
                           and p2rx_wpbp~persk in @mt_persk
                           and p2rx_wpbp~kostl in @mt_kostl )
           group by p2rx_rt~dct_pernr, p2rx_eval_period~inper, p2rx_eval_period~fpper, p2rx_rt~lgart
           %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

    sort lt_payroll_result by dct_pernr inper fpper lgart.
    delete adjacent duplicates from lt_payroll_result.
    delete lt_payroll_result where fpper <> mv_prev_payroll_period.
    et_payroll_result = lt_payroll_result.

  endmethod.


  method SAP_SWT.

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
        lgart  type lgart.
        include type pyd_s_rdswt_ext as ext.
      types:
        text   type pyd_name,
      end of ty_s_rd .

    data: lt_rt     type ty_t_rt_with_text,
          ls_rt     type ty_s_rt_with_text,
          ls_swt    type cl_pyd_rd_dto_swt=>ty_s_rd,
          lt_swt    type cl_pyd_rd_dto_swt=>ty_t_rd,
          lv_row_id type pyd_rowid,
          lv_param  type string,
          lv_pernr  type p_pernr,
          lv_curr   type waers.
    data: lt_char_rt type ty_t_rt_char_betpe,
          ls_char_rt type ty_s_rt_char_betpe.

    data lty_s_rd type sorted table  of ty_s_rd with non-unique key lgart.
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

    refresh et_rt.
* Read Current Period results
    try.
        call method super->sap_swt
          exporting
            is_rd          = is_rd
            it_par         = it_par
            io_res_context = io_res_context
            iv_access_mode = iv_access_mode
          importing
            et_rt          = et_rt.
      catch cx_pyd_fnd.
    endtry.

* Read Pay Period Results.
    try.
        call method is_rd-rd->get_data
          importing
            et_data = lt_swt.
      catch cx_pyd_fnd .
    endtry.

* Read Previous Payroll Results
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

    clear ls_lgart.

    select p2rx_eval_period~fpper as groupid, p2rx_eval_period~fpper as group_name, p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe, sum( case p2rx_eval_period~srtza when 'P' then anzhl * -1
                       else anzhl end ) as anzhl, p2rx_rt~amt_curr as amt_curr, p2rx_rt~rte_curr as rte_curr, t512t~lgtxt as lgtxt,"p2rx_eval_period~fpper as fpper,
       sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                 else betrg end ) as betrg
      into corresponding fields of table @lt_rt
              from p2rx_rt inner join p2rx_eval_period
                            on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                           and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
                 inner join t512t
                           on t512t~sprsl = @sy-langu
                           and t512t~molga = @lv_molga
                           and t512t~lgart = p2rx_rt~lgart
    where p2rx_eval_period~abkrs = @mv_payroll_area and
          p2rx_eval_period~dct_pernr = @lv_pernr and
          p2rx_eval_period~inper = @mv_prev_payroll_period and
          ( p2rx_rt~betrg <> 0 or p2rx_rt~anzhl <> 0 or p2rx_rt~betpe <> 0 ) and
          p2rx_rt~lgart in @lt_sel_lgart
    group by p2rx_eval_period~fpper, p2rx_rt~lgart, p2rx_rt~betpe, p2rx_rt~anzhl, p2rx_rt~amt_curr, p2rx_rt~rte_curr, t512t~lgtxt . "#EC CI_BUFFJOIN

    refresh lt_char_rt.
    loop at lt_rt into ls_rt.
      move-corresponding ls_rt to ls_char_rt.
      collect ls_char_rt into lt_char_rt.
    endloop.

    delete lt_char_rt where betrg eq 0.
    sort lt_char_rt by groupid descending lgart ascending.
* Exclude Retro Pay data
    if mv_swt_exc_retro = abap_true.
      delete lt_char_rt where groupid <> mv_prev_payroll_period.
    endif.

    describe table lt_swt lines lv_row_id.
    loop at lt_char_rt into ls_char_rt.
      ls_swt-group_name = 'FOR Period ' && '&' && ls_rt-group_name+4(2) && '/' && ls_rt-group_name(4).
      replace '&' with space into ls_swt-group_name.
      ls_swt-groupid = ls_char_rt-group_name.
      ls_swt-row_id = lv_row_id.
      ls_swt-itemid = ls_char_rt-lgart.
      ls_swt-text = ls_char_rt-lgtxt.
      ls_swt-amt  = ls_char_rt-betrg.
      ls_swt-num  = ls_char_rt-anzhl.
      ls_swt-rte  = ls_char_rt-betpe.
      if ls_char_rt-amt_curr is initial.
        ls_swt-amt_curr = lv_curr.
      else.
        ls_swt-amt_curr = ls_char_rt-amt_curr.
      endif.
      if ls_char_rt-rte_curr is initial.
        ls_swt-rte_curr = lv_curr.
      else.
        ls_swt-rte_curr = ls_char_rt-rte_curr.
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
