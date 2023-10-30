class ZUSECL_PY_WT_COMP_ANZHL_MULTI definition
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
        inper     type iperi,
        fpper     type fpper,
        lgart     type lgart,
        anzhl     type pranz,
      end of ty_data_result .
    types:
      tty_data_result type table of ty_data_result .
    types:
      begin of ty_result_count,
        dct_pernr type p_pernr,
        count     type i,
      end of ty_result_count .
    types:
      tty_result_count type table of ty_result_count .

    constants mc_condition_lgart type pyd_par_type value 'Z99_CONDITION_LGART' ##NO_TEXT.
    constants mc_exc_condition_lgart type pyd_par_type value 'Z99_EXC_CONDITION_LGART' ##NO_TEXT.
    constants mc_lgart_01 type pyd_par_type value 'Z99_FILTER_LGART_01' ##NO_TEXT.
    constants mc_lgart_02 type pyd_par_type value 'Z99_FILTER_LGART_02' ##NO_TEXT.
    constants mc_lgart_03 type pyd_par_type value 'Z99_FILTER_LGART_03' ##NO_TEXT.
    constants mc_num_low_01 type pyd_par_type value 'Z99_NUM_LOW_01' ##NO_TEXT.
    constants mc_num_low_02 type pyd_par_type value 'Z99_NUM_LOW_02' ##NO_TEXT.
    constants mc_num_low_03 type pyd_par_type value 'Z99_NUM_LOW_03' ##NO_TEXT.
    constants mc_num_high_01 type pyd_par_type value 'Z99_NUM_HIGH_01' ##NO_TEXT.
    constants mc_num_high_02 type pyd_par_type value 'Z99_NUM_HIGH_02' ##NO_TEXT.
    constants mc_num_high_03 type pyd_par_type value 'Z99_NUM_HIGH_03' ##NO_TEXT.
    data mt_condition_lgart type /iwbep/t_cod_select_options .
    data mt_exc_condition_lgart type /iwbep/t_cod_select_options .
    data mt_lgart_01 type /iwbep/t_cod_select_options .
    data mt_lgart_02 type /iwbep/t_cod_select_options .
    data mt_lgart_03 type /iwbep/t_cod_select_options .
    data ms_lgart type /iwbep/s_cod_select_option .
    data mv_num_low_01 type pranz .
    data mv_num_low_02 type pranz .
    data mv_num_low_03 type pranz .
    data mv_num_high_01 type pranz .
    data mv_num_high_02 type pranz .
    data mv_num_high_03 type pranz .
    data mt_bat_result_count type tty_result_count .
    data ms_bat_result_count type ty_result_count .
    data mt_all_result_count type tty_result_count .
    data ms_all_result_count type ty_result_count .

    methods read_payroll_result
      importing
        !it_pernr       type hr99s_pernr_range
        !it_lgart       type /iwbep/t_cod_select_options
      exporting
        !et_data_result type tty_data_result .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.

ENDCLASS.



CLASS ZUSECL_PY_WT_COMP_ANZHL_MULTI IMPLEMENTATION.


  method CHECK.
* Read Check data using Parallel Processing and build result table
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lt_all_result_count type tty_result_count,
          ls_result_count     type ty_result_count,
          ls_result           type ty_s_result.
    data: lv_curr type waers.

    data: lt_pernr type tty_pernr.
    data: ls_pernr type ty_pernr.
    data  lv_retcd  type sy-subrc.
    data: lv_abs_objname  type abap_abstypename.
    data: lv_objname  type seoclsname.

    refresh mt_objdata.
    refresh: lt_all_result_count.
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
    data: lo_cloned_chkobj type ref to zusecl_py_wt_comp_anzhl_multi.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.
          append lines of lo_cloned_chkobj->mt_bat_result_count to lt_all_result_count.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref.  "MOD001++
      endtry.

    endloop.

* Collect Data for Overview
    append lines of lt_all_result_count to me->mt_all_result_count.

* Build Result Table
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
            ls_sfo-itemid = 'MULTI'.
            ls_sfo-value  = text-001.
            ls_sfo-text = text-002.
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

    try .
* Include Condition Wage types
        loop at mo_context->mt_par into ls_par where par_type = me->mc_condition_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_condition_lgart.
        endloop.

* Exclude Condition Wage types
        loop at mo_context->mt_par into ls_par where par_type = me->mc_exc_condition_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_exc_condition_lgart.
        endloop.

* Wage Types 01
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_01.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart_01.
        endloop.

* Wage Types 02
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_02.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart_02.
        endloop.

* Wage Types 03
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_03.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart_03.
        endloop.

* Amount Low Values
        mv_num_low_01 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_low_01
                                                               it_par      = mo_context->mt_par ).
        mv_num_low_02 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_low_02
                                                               it_par      = mo_context->mt_par ).
        mv_num_low_03 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_low_03
                                                               it_par      = mo_context->mt_par ).

* Amout High Values
        mv_num_high_01 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_high_01
                                                                it_par      = mo_context->mt_par ).
        mv_num_high_02 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_high_01
                                                                it_par      = mo_context->mt_par ).
        mv_num_high_03 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_high_01
                                                                it_par      = mo_context->mt_par ).

* Build SWT Wage type list
        append lines of mt_lgart_01 to mt_swt_lgart.
        append lines of mt_lgart_02 to mt_swt_lgart.
        append lines of mt_lgart_03 to mt_swt_lgart.
* Exclude retro period data display
        mv_swt_exc_retro = abap_true.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method READ_CHECK_DATA.
* Read Payroll Result for Selected Employees
    data: lv_chk_number type numc2.
    data: lt_all_result_count type tty_result_count,
          lt_result_count     type tty_result_count,
          ls_result_count     type ty_result_count.
    data: lt_all_lgart type /iwbep/t_cod_select_options.
    data: lt_all_data_result type tty_data_result.

    data: lt_data_result type tty_data_result.
    data: ls_data_result type ty_data_result.
    data: lt_data_result_2 type tty_data_result.
    data: ls_data_result_2 type ty_data_result.
    data: lt_pernr type hr99s_pernr_range.
    data: ls_pernr type sel_pernr.

* Collect All Wage Types for Payroll Data Read
    append lines of mt_condition_lgart to lt_all_lgart.
    append lines of mt_exc_condition_lgart to lt_all_lgart.
    append lines of mt_lgart_01 to lt_all_lgart.
    append lines of mt_lgart_02 to lt_all_lgart.
    append lines of mt_lgart_03 to lt_all_lgart.
    call method me->read_payroll_result
      exporting
        it_pernr       = it_pernr
        it_lgart       = lt_all_lgart
      importing
        et_data_result = lt_all_data_result.

* Identify Employees with Conditional Wage type amount
    refresh: lt_pernr.
    if not mt_condition_lgart is initial.
      ls_pernr-sign = 'I'. ls_pernr-option = 'EQ'.
      loop at lt_all_data_result into ls_data_result
        where lgart in mt_condition_lgart.
        ls_pernr-low = ls_pernr-high = ls_data_result-dct_pernr.
        collect ls_pernr into lt_pernr.
      endloop.
* And delete all Employees not having Conditional Wage type
      if not lt_pernr is initial.
        delete lt_all_data_result where not dct_pernr in lt_pernr.
      endif.
    endif.

    refresh: lt_pernr.
    if not mt_exc_condition_lgart is initial.
      ls_pernr-sign = 'I'. ls_pernr-option = 'EQ'.

      loop at lt_all_data_result into ls_data_result
        where lgart in mt_exc_condition_lgart.
        ls_pernr-low = ls_pernr-high = ls_data_result-dct_pernr.
        collect ls_pernr into lt_pernr.
      endloop.

* And delete all Employees not having Conditional Wage type
      if not lt_pernr is initial.
        delete lt_all_data_result where dct_pernr in lt_pernr.
      endif.
    endif.

    refresh: lt_all_result_count.
* Read Data result for Group 1
    refresh: lt_result_count .
    if not mt_lgart_01[] is initial.
      add 1 to lv_chk_number.

      lt_data_result = lt_all_data_result.
      delete lt_data_result where lgart not in mt_lgart_01.
* Rebuild the list with out WT details
      lt_data_result_2 = lt_data_result.
      refresh lt_data_result.
      loop at lt_data_result_2 into ls_data_result_2.
        clear: ls_data_result_2-lgart.
        collect ls_data_result_2 into lt_data_result.
      endloop.
      delete lt_data_result where anzhl not between mv_num_low_01 and mv_num_high_01.

* Delete Results not in between number high and number low
      refresh lt_result_count.
      loop at lt_data_result into ls_data_result.
        if ls_data_result-anzhl between mv_num_low_01 and mv_num_high_01.
          ls_result_count-dct_pernr = ls_data_result-dct_pernr.
          ls_result_count-count = 1.
          append ls_result_count to lt_result_count.
        endif.
      endloop.

      loop at lt_result_count into ls_result_count.
        collect ls_result_count into lt_all_result_count.
      endloop.
    endif.

* Read Data result for Group 2
    refresh: lt_result_count .
    if not mt_lgart_02[] is initial.
      add 1 to lv_chk_number.

      lt_data_result = lt_all_data_result.
      delete lt_data_result where lgart not in mt_lgart_02.
* Rebuild the list with out WT details
      lt_data_result_2 = lt_data_result.
      refresh lt_data_result.
      loop at lt_data_result_2 into ls_data_result_2.
        clear: ls_data_result_2-lgart.
        collect ls_data_result_2 into lt_data_result.
      endloop.
      delete lt_data_result where anzhl not between mv_num_low_02 and mv_num_high_02.

* Delete Results not in between number high and number low
      refresh lt_result_count.
      loop at lt_data_result into ls_data_result.
        if ls_data_result-anzhl between mv_num_low_02 and mv_num_high_02.
          ls_result_count-dct_pernr = ls_data_result-dct_pernr.
          ls_result_count-count = 1.
          append ls_result_count to lt_result_count.
        endif.
      endloop.

      loop at lt_result_count into ls_result_count.
        collect ls_result_count into lt_all_result_count.
      endloop.
    endif.

* Read Data result for Group 3
    refresh: lt_result_count .
    if not mt_lgart_03[] is initial.
      add 1 to lv_chk_number.

      lt_data_result = lt_all_data_result.
      delete lt_data_result where lgart not in mt_lgart_03.
* Rebuild the list with out WT details
      lt_data_result_2 = lt_data_result.
      refresh lt_data_result.
      loop at lt_data_result_2 into ls_data_result_2.
        clear: ls_data_result_2-lgart.
        collect ls_data_result_2 into lt_data_result.
      endloop.
      delete lt_data_result where anzhl not between mv_num_low_03 and mv_num_high_03.

* Delete Results not in between number high and number low
      refresh lt_result_count.
      loop at lt_data_result into ls_data_result.
        if ls_data_result-anzhl between mv_num_low_03 and mv_num_high_03.
          ls_result_count-dct_pernr = ls_data_result-dct_pernr.
          ls_result_count-count = 1.
          append ls_result_count to lt_result_count.
        endif.
      endloop.

      loop at lt_result_count into ls_result_count.
        collect ls_result_count into lt_all_result_count.
      endloop.
    endif.
*
    sort lt_all_result_count by dct_pernr ascending.
    delete lt_all_result_count where count ne lv_chk_number.

    append lines of lt_all_result_count to mt_bat_result_count.

  endmethod.


  method READ_PAYROLL_RESULT.

    data: lt_data_result   type standard table of ty_data_result,
          lt_data_result_2 type standard table of ty_data_result,
          ls_result_count  type ty_result_count.

    data: lv_curr type waers.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.

      "LGART Total current period
      refresh: lt_data_result.
      select p2rx_rt~dct_pernr as dct_pernr, hrdct_tpy_rgdir~inper as inper,
             hrdct_tpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then anzhl * -1
                          else anzhl end ) as anzhl
              into corresponding fields of table @lt_data_result
                       from hrdct_tpy_rgdir inner join p2rx_rt
                                     on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                                    and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
*               and hrdct_tpy_rgdir~fpper = @mv_payroll_period
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
      refresh: lt_data_result_2.
      select p2rx_rt~dct_pernr as dct_pernr, hrpy_rgdir~inper as inper,
             hrpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case hrpy_rgdir~srtza when 'A' then anzhl * -1
                          else 0 end ) as anzhl
              into corresponding fields of table @lt_data_result_2
                     from hrdct_tpy_rgdir inner join hrpy_rgdir
                      on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
                      and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
                    inner join p2rx_rt
                      on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
                      and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
*               and hrdct_tpy_rgdir~fpper = @mv_payroll_period
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

      append lines of lt_data_result to lt_data_result_2.
      sort lt_data_result_2 by dct_pernr inper fpper lgart.

      refresh lt_data_result.
      loop at lt_data_result_2 into data(ls_data_result).
        collect ls_data_result into lt_data_result.
      endloop.

    else. "Production Payroll

      "LGART Total Inc retro
      refresh: lt_data_result.
      select p2rx_rt~dct_pernr as dct_pernr, p2rx_eval_period~inper as inper,
            p2rx_eval_period~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case p2rx_eval_period~srtza when 'P' then anzhl * -1
                          else anzhl end ) as anzhl
              into corresponding fields of table @lt_data_result
                       from p2rx_eval_period inner join p2rx_rt
                                     on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                                    and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr
               and p2rx_eval_period~abkrs in @mt_payroll_areas
*               and p2rx_eval_period~fpper = @mv_payroll_period
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

    sort lt_data_result by dct_pernr inper fpper lgart.
    delete adjacent duplicates from lt_data_result.

    delete lt_data_result where fpper <> mv_payroll_period.
    delete lt_data_result where anzhl eq 0.

    et_data_result = lt_data_result.

  endmethod.
ENDCLASS.
