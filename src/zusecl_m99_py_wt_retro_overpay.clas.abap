class ZUSECL_M99_PY_WT_RETRO_OVERPAY definition
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
        dct_pernr  type p_pernr,
        fpper      type fpper,
        lgart      type lgart,
        betrg      type betrg,
        betrg_comp type betrg,
      end of ty_data_result .
    types:
      tty_data_result type table of ty_data_result .
    types:
      begin of ty_betrg_dtls,
        lgart      type lgart,
        betrg      type betrg,
        betrg_comp type betrg,
      end of ty_betrg_dtls .
    types:
      begin of ty_result_count,
        dct_pernr type p_pernr,
        count     type anzhl,
      end of ty_result_count .
    types:
      tty_result_count type table of ty_result_count .

    constants mc_lgart_1 type pyd_par_type value 'Z99_FILTER_LGART_01' ##NO_TEXT.
    constants mc_lgart_2 type pyd_par_type value 'Z99_FILTER_LGART_02' ##NO_TEXT.
    constants mc_net_calc type lgart value '/552' ##NO_TEXT.
    data mt_lgart_1 type /iwbep/t_cod_select_options .
    data ms_lgart type /iwbep/s_cod_select_option .
    data mv_sum1 type betrg .
    data mv_sum2 type betrg .
    constants mc_itemid_numerr type pyd_itemid value 'NUMERR' ##NO_TEXT.

    data mt_bat_betrg_data type tty_data_result .
    data mt_all_betrg_data type tty_data_result .
    data ms_betrg_data type ty_data_result .
    data ms_betrg_details type ty_betrg_dtls .

    data mt_bat_result_count type tty_result_count .
    data mt_all_result_count type tty_result_count .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.

    data mt_lgart_2 type /iwbep/t_cod_select_options .
ENDCLASS.



CLASS ZUSECL_M99_PY_WT_RETRO_OVERPAY IMPLEMENTATION.


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
    data: lt_all_betrg_data   type tty_data_result,
          lt_all_result_count type tty_result_count.
    data: ls_result_count type ty_result_count,
          ls_result       type ty_s_result.
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

* Perform Parallel Processing
    lv_abs_objname = cl_abap_classdescr=>get_class_name( me ).
    split lv_abs_objname at mc_equal into data(lv_clstext) lv_objname.
    call method me->multithread_process
      exporting
        iv_clsname = lv_objname
        it_pernr   = lt_pernr.

* Collect the data
    data: lo_cloned_chkobj type ref to zusecl_m99_py_wt_retro_overpay.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.

          append lines of lo_cloned_chkobj->mt_bat_result_count to lt_all_result_count.
          append lines of lo_cloned_chkobj->mt_bat_betrg_data to lt_all_betrg_data.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref. "MOD001++
      endtry.
    endloop.
* Collect Data for Overview
    append lines of lt_all_result_count to me->mt_all_result_count.
    append lines of lt_all_betrg_data to me->mt_all_betrg_data.

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
      lv_number(20) type c,
      lv_text       type pyd_name,
      lv_char_value type char060.

    data: lv_longtext  type  t512t-lgtxt,
          lv_shorttext type  t512t-kztxt.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters
        get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).
        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_betrg_details.

            clear: lv_text.

            "Sum1
            lv_text = text-001.
            write ms_betrg_details-betrg to lv_char_value left-justified.
            lv_value = lv_char_value.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_numerr
                iv_text                     = lv_text
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

            "Sum2
            lv_text = text-002.
            write ms_betrg_details-betrg_comp to lv_char_value left-justified.
            lv_value = lv_char_value.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_numerr
                iv_text                     = lv_text
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

          endloop.


* Action
          lv_text = text-003.
          lv_value = text-004.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_numerr
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_all_betrg_data into ms_betrg_data
            where dct_pernr = lv_pernr.

            move-corresponding ms_betrg_data to ms_betrg_details.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_betrg_details
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_numerr.
            ls_sfo-value = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.


      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Populating custom paramters for the validation

    data: lt_param_so type /iwbep/t_cod_select_options,
          ls_par      type if_pyd_fnd_types=>ty_s_resp.

    try .

* Wage Types for BETRG 1
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_1.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart_1.
        endloop.


* Wage Types for BETRG 2
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_2.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart_2.
        endloop.


* Build SWT Wage type list
        append lines of mt_lgart_1 to mt_swt_lgart.
        append lines of mt_lgart_2 to mt_swt_lgart.

        mv_swt_exc_retro = abap_true.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method READ_CHECK_DATA.
* Called in RFC to perform data selection and populate result validation table

    data: lt_data_result_1     type standard table of ty_data_result,
          lt_data_result_2     type standard table of ty_data_result,
          lt_data_result_retro type standard table of ty_data_result,
          lt_data_result_552   type standard table of ty_data_result,
          ls_data_result       type ty_data_result,
          ls_data_result_2     type ty_data_result,
          lt_lgart_combined    type /iwbep/t_cod_select_options,
          lv_sum1              type betrg,
          lv_sum2              type betrg,
          ls_result_count      type ty_result_count.

    "combine WTs for performance
    clear lt_lgart_combined.
    append lines of mt_lgart_1 to lt_lgart_combined.
    append lines of mt_lgart_2 to lt_lgart_combined.


    "Check if a Test Payroll
    if mv_tpy_res is not initial.

      "Total current period
      select hrdct_tpy_rgdir~dct_pernr, hrdct_tpy_rgdir~fpper, p2rx_rt~lgart,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1
                          else betrg end ) as betrg
              into corresponding fields of table @lt_data_result_1
                       from hrdct_tpy_rgdir inner join p2rx_rt
                                     on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                                    and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr and
                   hrdct_tpy_rgdir~abkrs in @mt_payroll_areas and
                   hrdct_tpy_rgdir~inper = @mv_payroll_period and
                   p2rx_rt~lgart in @lt_lgart_combined
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
      group by hrdct_tpy_rgdir~dct_pernr, hrdct_tpy_rgdir~fpper, p2rx_rt~lgart
      %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      "LGART Total for retros
      refresh: lt_data_result_retro.
      select hrdct_tpy_rgdir~dct_pernr as dct_pernr, hrpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case hrpy_rgdir~srtza when 'A' then betrg * -1
                          else 0 end ) as betrg
              into corresponding fields of table @lt_data_result_retro
               from hrdct_tpy_rgdir inner join hrpy_rgdir
                      on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
                      and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
                    inner join p2rx_rt
                      on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
                      and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr and
                   hrdct_tpy_rgdir~abkrs in @mt_payroll_areas and
                   hrdct_tpy_rgdir~inper = @mv_payroll_period and
                   p2rx_rt~lgart in @lt_lgart_combined
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
      group by hrdct_tpy_rgdir~dct_pernr, hrpy_rgdir~fpper, p2rx_rt~lgart
      %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      append lines of lt_data_result_1 to lt_data_result_retro.
      sort lt_data_result_retro by dct_pernr ascending.
      clear lt_data_result_1.
      loop at lt_data_result_retro into data(ls_data_result_retro).
        collect ls_data_result_retro into lt_data_result_1.
      endloop.


    else. "Production Payroll

      "LGART1 Total inc retro

      select p2rx_rt~dct_pernr, p2rx_eval_period~fpper, p2rx_rt~lgart,
              sum( case p2rx_eval_period~srtza when 'P' then betrg * -1 else betrg end ) as betrg
              into corresponding fields of table @lt_data_result_1
                       from p2rx_eval_period inner join p2rx_rt
                                     on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                                    and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr and
                   p2rx_eval_period~abkrs in @mt_payroll_areas and
                   p2rx_eval_period~inper = @mv_payroll_period and
                   p2rx_rt~lgart in @lt_lgart_combined
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
             group by p2rx_rt~dct_pernr, p2rx_eval_period~fpper, p2rx_rt~lgart
%_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.


    endif.

    "consolidate both tables
    lt_data_result_2[] = lt_data_result_1[].

    delete lt_data_result_1 where lgart in mt_lgart_2. "only keep mt_lgart_1
    delete lt_data_result_2 where lgart in mt_lgart_1. "and here only keep mt_lgart_2

    "cleanse the data
    sort lt_data_result_1 by dct_pernr ascending.
    delete adjacent duplicates from lt_data_result_1.
    delete lt_data_result_1 where fpper <> mv_payroll_period.

    "cleanse the data
    sort lt_data_result_2 by dct_pernr ascending.
    delete adjacent duplicates from lt_data_result_2.
    delete lt_data_result_2 where fpper <> mv_payroll_period.

    "collect to account for /552
    lt_data_result_552[] = lt_data_result_2[].

    refresh lt_data_result_2.
    loop at lt_data_result_552 into data(ls_betrg_data_552).
      if ls_betrg_data_552-lgart eq mc_net_calc.
        ls_betrg_data_552-betrg = ls_betrg_data_552-betrg * -1.
      endif.

      clear: ls_betrg_data_552-lgart.

      collect ls_betrg_data_552 into lt_data_result_2.
    endloop.

    "check if both sums are not equal per each pernr
    loop at lt_data_result_1 into ls_data_result.
      loop at lt_data_result_2 into ls_data_result_2
        where dct_pernr = ls_data_result-dct_pernr.

        if  abs( ls_data_result-betrg ) - abs( ls_data_result_2-betrg ) ne 0.

          move-corresponding ls_data_result to ms_betrg_data.
          ms_betrg_data-betrg_comp = ls_data_result_2-betrg.
          append ms_betrg_data to mt_bat_betrg_data.

          ls_result_count-dct_pernr = ls_data_result-dct_pernr.
          ls_result_count-count = 1.
          append ls_result_count to mt_bat_result_count.

        endif.

      endloop.
    endloop.


  endmethod.
ENDCLASS.
