class ZUSECL_PY_WT_COMP_ANZHL_2_BET definition
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
    begin of ty_time_data,
      pernr type pernr_d,
      infty type infty,
      subty type subty,
      begda type begda,
      endda type endda,
      seqnr type seqnr,
      awart type awart,
      stdaz type abstd.
  types: end of ty_time_data .
  types:
    tty_time_data type table of ty_time_data .
  types:
    begin of ty_data_result,
      dct_pernr type p_pernr,
      fpper     type fpper,
      lgart     type lgart,
      anzhl     type pranz,
      betrg     type betrg,
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

  constants MC_CONDITION_LGART type PYD_PAR_TYPE value 'Z99_CONDITION_LGART' ##NO_TEXT.
  constants MC_EXC_CONDITION_LGART type PYD_PAR_TYPE value 'Z99_EXC_CONDITION_LGART' ##NO_TEXT.
  constants MC_LGART_ANZHL type PYD_PAR_TYPE value 'Z99_FILTER_LGART_01' ##NO_TEXT.
  constants MC_LGART_BETRG type PYD_PAR_TYPE value 'Z99_FILTER_LGART_02' ##NO_TEXT.
  constants MC_ANZHL_HIGH type PYD_PAR_TYPE value 'Z99_NUM_HIGH_01' ##NO_TEXT.
  constants MC_ANZHL_LOW type PYD_PAR_TYPE value 'Z99_NUM_LOW_01' ##NO_TEXT.
  constants MC_BETRG_HIGH type PYD_PAR_TYPE value 'Z99_AMT_HIGH_02' ##NO_TEXT.
  constants MC_BETRG_LOW type PYD_PAR_TYPE value 'Z99_AMT_LOW_02' ##NO_TEXT.
  constants MC_NUM_WOSTD type PYD_PAR_TYPE value 'Z99_NUM_WOSTD' ##NO_TEXT.
  constants MC_NUM_MOSTD type PYD_PAR_TYPE value 'Z99_NUM_MOSTD' ##NO_TEXT.
  constants MC_ANZHL type C value 'A' ##NO_TEXT.
  constants MC_BETRG type C value 'B' ##NO_TEXT.
  constants MC_HIGH type BETRG value '999999' ##NO_TEXT.
  data MT_CONDITION_LGART type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_EXC_CONDITION_LGART type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_LGART_ANZHL type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_LGART_BETRG type /IWBEP/T_COD_SELECT_OPTIONS .
  data MS_LGART type /IWBEP/S_COD_SELECT_OPTION .
  data MV_COMPARE_ANZHL_HIGH type BETRG .
  data MV_COMPARE_BETRG_HIGH type BETRG .
  data MV_COMPARE_ANZHL_LOW type BETRG .
  data MV_COMPARE_BETRG_LOW type BETRG .
  data MT_BAT_RESULT_COUNT_1 type TTY_RESULT_COUNT .
  data MT_BAT_RESULT_COUNT_2 type TTY_RESULT_COUNT .
  data MT_ALL_RESULT_COUNT_1 type TTY_RESULT_COUNT .
  data MT_ALL_RESULT_COUNT_2 type TTY_RESULT_COUNT .
  data MV_NUM_WOSTD type CHAR10 .
  data MV_MULTIPLICATION_NUM type WOSTD .
  data MV_COMPARISON_OPARATOR type ZHRAU_DE_CHECKOP .
  data MV_COMP_WOSTD type P0007-WOSTD .
  data MV_NUM_MOSTD type CHAR10 .
  data MV_MOSTD_MULT_NUM type MOSTD .
  data MV_MOSTD_COMP_OPARATOR type ZHRAU_DE_CHECKOP .
  constants MC_ITEMID_COMP type PYD_ITEMID value 'COMP' ##NO_TEXT.
  constants MC_IT2001 type INFTY value '2001' ##NO_TEXT.

  methods READ_TIME_DATA
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    exporting
      !ET_TIME_DATA type TTY_TIME_DATA .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_PY_WT_COMP_ANZHL_2_BET IMPLEMENTATION.


  method CHECK.
* Read payroll data using parallel processing and build result table
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lt_all_result_count type tty_result_count,
          lt_result_count_1   type tty_result_count,
          lt_result_count_2   type tty_result_count,
          ls_result_count_1   type ty_result_count,
          ls_result_count_2   type ty_result_count,
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

* Perform Parallel Processing
    lv_abs_objname = cl_abap_classdescr=>get_class_name( me ).
    split lv_abs_objname at mc_equal into data(lv_clstext) lv_objname.
    call method me->multithread_process
      exporting
        iv_clsname = lv_objname
        it_pernr   = lt_pernr.

* Collect the data
    data: lo_cloned_chkobj type ref to zusecl_py_wt_comp_anzhl_2_bet.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.
          append lines of lo_cloned_chkobj->mt_bat_result_count_1 to lt_result_count_1.
          append lines of lo_cloned_chkobj->mt_bat_result_count_2 to lt_result_count_2.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref.  "MOD001++
      endtry.
    endloop.

* Collect Result for Overview
    append lines of lt_result_count_1 to me->mt_all_result_count_1.
    append lines of lt_result_count_2 to me->mt_all_result_count_2.

    sort lt_result_count_1 by dct_pernr.
    sort lt_result_count_2 by dct_pernr.

    delete adjacent duplicates from lt_result_count_1 comparing dct_pernr.
    delete adjacent duplicates from lt_result_count_2 comparing dct_pernr.

    "double check that WTs on both sides match
    loop at lt_result_count_1 into ls_result_count_1.
      loop at lt_result_count_2 into ls_result_count_2 where dct_pernr = ls_result_count_1-dct_pernr.
        collect ls_result_count_2 into lt_all_result_count.
      endloop.
    endloop.

    sort lt_all_result_count by dct_pernr ascending.
    delete adjacent duplicates from lt_all_result_count comparing dct_pernr.

    loop at lt_all_result_count into ls_result_count_1.
      ls_result-id = ls_result_count_1-dct_pernr.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      append ls_result to rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
*----------------------------------------------------------------------------------------------*
* CHANGE HISTORY
*----------------------------------------------------------------------------------------------*
*Mod | Date      | User ID|Description                       |Change Label  |Workbench Request *
*----------------------------------------------------------------------------------------------*
*001 |22-06-2022 | 1130848| Display Absence Details          | PTX-3763     |CFAK901879        *
*----------------------------------------------------------------------------------------------*
*002 |02-03-2023 | 1130848| MOLGA Enhancements               | PTX-1251     |CFAK902930        *
*----------------------------------------------------------------------------------------------*
* Populating Overview screen
    data:
      ls_err_ov     type ty_s_err_ov,
      ls_sfo        type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif      type abap_bool,
      lv_pernr      type p_pernr,
      lv_value      type pyd_item_value,
      lv_number(20) type c.

    data: lv_msg type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
    data: lv_text type pyd_name.
    data: lv_al_begda(10) type c,
          lv_ab_begda(10) type c,
          lv_ab_endda(10) type c.

    data: lt_time_data type standard table of ty_time_data.
    data: lv_timedata_anzhl type enanz,
          lv_timedata_text  type pyd_name,
          lv_stext          type  sbttx.
    data: lt_eblgart    type  wfa_ptbindbw_t,
          lv_lgtxt      type  t512t-lgtxt,
          lv_char_betrg type lgtxt.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters
        get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).

        loop at ct_err_ov into ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.

* Generic Message
          clear ls_err_ov-sfo_tab.
          lv_msg = text-001.
          lv_text = text-002.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_comp
              iv_text                     = lv_text
              iv_value                    = lv_msg
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Read Time Data
          refresh: lt_time_data.
          call method me->read_time_data
            exporting
              iv_pernr     = lv_pernr
              iv_begda     = mv_begda
              iv_endda     = mv_endda
            importing
              et_time_data = lt_time_data.

          loop at lt_time_data into data(ls_time_data).
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = ls_time_data-infty
                subty               = ls_time_data-subty
                persnr              = lv_pernr
                begda               = ls_time_data-begda
                endda               = ls_time_data-endda
*               molga               = mc_molga_au           "MOD002--
                molga               = mv_molga              "MOD002++
              importing
                stext               = lv_stext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.

            write: ls_time_data-begda to lv_ab_begda dd/mm/yyyy.
            write: ls_time_data-endda to lv_ab_endda dd/mm/yyyy.
            concatenate lv_stext '(' ls_time_data-subty ')' into lv_msg.
            lv_msg = | { lv_ab_begda }  -  { lv_ab_endda }   { lv_msg } |.

            clear: lv_text.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_comp
                iv_text                     = lv_text
                iv_value                    = lv_msg
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.
* Infotype 8 details
          call function 'WFA_GET_WAGETYPE_AMOUNT'
            exporting
              iv_pernr   = lv_pernr
              iv_begda   = mv_endda
              iv_endda   = mv_endda
            importing
              et_eblgart = lt_eblgart.

          loop at lt_eblgart into data(ls_eblgart)
             where lgart in mt_lgart_betrg.
            " read Text
            select single lgtxt into lv_lgtxt
               from t512t
              where sprsl = sy-langu
                and molga = io_res_context->ms_inst-molga
                and lgart = ls_eblgart-lgart.

            write ls_eblgart-betrg to lv_char_betrg currency ls_eblgart-waers.
            lv_msg = | { ls_eblgart-lgart }  -  { lv_lgtxt } |.

            clear: lv_text.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_comp
                iv_text                     = lv_text
                iv_value                    = lv_msg
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

            clear: lv_text.
            lv_msg =  lv_char_betrg.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_comp
                iv_text                     = lv_text
                iv_value                    = lv_msg
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.


      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Populating custom paramters for the validation

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

* Wage Types for ANZHL
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_anzhl.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart_anzhl.
        endloop.

* Wage Types for BETRG
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_betrg.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart_betrg.
        endloop.


* Amount Low Values
        mv_compare_anzhl_low =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_anzhl_low
                                                                      it_par      = mo_context->mt_par ).
        mv_compare_betrg_low =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_betrg_low
                                                                      it_par      = mo_context->mt_par ).

* Amount High Values
        mv_compare_anzhl_high =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_anzhl_high
                                                                       it_par      = mo_context->mt_par ).
        mv_compare_betrg_high =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_betrg_high
                                                                       it_par      = mo_context->mt_par ).

* Build SWT Wage type list
        append lines of mt_lgart_anzhl to mt_swt_lgart.
        append lines of mt_lgart_betrg to mt_swt_lgart.

* Working Weeks
        mv_num_wostd =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_wostd
                                                              it_par    = mo_context->mt_par ).
        mv_comparison_oparator = mv_num_wostd+0(2).
        mv_multiplication_num = mv_num_wostd+2.

* Working Months
        mv_num_mostd =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_mostd
                                                                it_par    = mo_context->mt_par ).
        mv_mostd_comp_oparator = mv_num_mostd+0(2).
        mv_mostd_mult_num = mv_num_mostd+2.

        "mv_swt_exc_retro = abap_true.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method READ_CHECK_DATA.
* Called in RFC to perform data selection and populate result validation table

    data: lt_data_result   type standard table of ty_data_result,
          ls_data_result   type ty_data_result,
          lt_data_betrg    type standard table of ty_data_result,
          lt_data_anzhl    type standard table of ty_data_result,
          lt_cond_data     type standard table of ty_data_result,
          lt_data_betrg_2  type standard table of ty_data_result,
          lt_data_anzhl_2  type standard table of ty_data_result,
          lt_cond_data_2   type standard table of ty_data_result,
          lt_data_result_2 type standard table of ty_data_result,
          lt_pernr         type hr99s_pernr_range,
          ls_pernr         type sel_pernr,
          lt_lgart         type /iwbep/t_cod_select_options,
          ls_result_count  type ty_result_count.

    data: lt_p0007            type table of p0007,
          ls_p0007            type p0007,
          lv_wostd            type p0007-wostd,
          lv_mostd            type p0007-mostd,
          lv_wostd_comparison type boolean,
          lv_mostd_comparison type boolean.
    data  lv_retcd type sy-subrc.

    refresh: lt_data_result, lt_data_result_2, lt_data_betrg, lt_data_anzhl, lt_data_betrg_2, lt_data_anzhl_2, lt_lgart.

    append lines of mt_condition_lgart to lt_lgart.
    append lines of mt_exc_condition_lgart to lt_lgart.
    append lines of mt_lgart_betrg to lt_lgart.
    append lines of mt_lgart_anzhl to lt_lgart.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.

      "LGART Total current period

      select hrdct_tpy_rgdir~dct_pernr as dct_pernr, hrdct_tpy_rgdir~fpper as fpper, p2rx_rt~lgart,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1 else betrg end ) as betrg,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then anzhl * -1 else anzhl end ) as anzhl
              into corresponding fields of table @lt_data_result
                       from hrdct_tpy_rgdir inner join p2rx_rt
                        on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                       and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~fpper = @mv_payroll_period
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and p2rx_rt~lgart in @lt_lgart
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
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk )
             group by hrdct_tpy_rgdir~dct_pernr, hrdct_tpy_rgdir~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      lt_cond_data[] = lt_data_result[].
      delete lt_cond_data where ( lgart in mt_lgart_betrg or
                                  lgart in mt_lgart_anzhl ).
      lt_data_betrg[] = lt_data_result[].
      delete lt_data_betrg where lgart not in mt_lgart_betrg.
      lt_data_anzhl[] = lt_data_result[].
      delete lt_data_anzhl where lgart not in mt_lgart_anzhl.

      "LGART total for retros
      select hrdct_tpy_rgdir~dct_pernr as dct_pernr, hrpy_rgdir~fpper as fpper, p2rx_rt~lgart,
            sum( case hrpy_rgdir~srtza when 'A' then betrg * -1 else 0 end ) as betrg,
            sum( case hrpy_rgdir~srtza when 'A' then anzhl * -1 else 0 end ) as anzhl
              into corresponding fields of table @lt_data_result_2
                          from hrdct_tpy_rgdir inner join hrpy_rgdir
                                 on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
                                and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
                           inner join p2rx_rt
                                on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
                               and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~fpper = @mv_payroll_period
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and p2rx_rt~lgart in @lt_lgart
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
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk )
             group by hrdct_tpy_rgdir~dct_pernr, hrpy_rgdir~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      lt_cond_data_2[] = lt_data_result_2[].
      delete lt_cond_data_2 where ( lgart in mt_lgart_betrg or
                                    lgart in mt_lgart_anzhl ).
      lt_data_betrg_2[] = lt_data_result_2[].
      delete lt_data_betrg_2 where lgart not in mt_lgart_betrg.
      lt_data_anzhl_2[] = lt_data_result_2[].
      delete lt_data_anzhl_2 where lgart not in mt_lgart_anzhl.

      "Conditional Data
      append lines of lt_cond_data to lt_cond_data_2.
      sort lt_cond_data_2 by dct_pernr ascending.
      clear lt_cond_data.
      loop at lt_cond_data_2 into data(ls_cond_data).
        collect ls_cond_data into lt_cond_data.
      endloop.

      "anzhl
      append lines of lt_data_anzhl to lt_data_anzhl_2.
      sort lt_data_anzhl_2 by dct_pernr ascending.
      clear lt_data_anzhl.
      loop at lt_data_anzhl_2 into data(ls_data_anzhl).
        clear ls_data_anzhl-lgart.
        collect ls_data_anzhl into lt_data_anzhl.
      endloop.

      "betrg
      append lines of lt_data_betrg to lt_data_betrg_2.
      sort lt_data_betrg_2 by dct_pernr ascending.
      clear lt_data_betrg.
      loop at lt_data_betrg_2 into data(ls_data_betrg).
        clear ls_data_betrg-lgart.
        collect ls_data_betrg into lt_data_betrg.
      endloop.

    else. "Production Payroll

      "LGART Total inc retro
      select p2rx_rt~dct_pernr as dct_pernr, p2rx_eval_period~fpper as fpper, p2rx_rt~lgart,
            sum( case p2rx_eval_period~srtza when 'P' then betrg * -1 else betrg end ) as betrg,
            sum( case p2rx_eval_period~srtza when 'P' then anzhl * -1 else anzhl end ) as anzhl
              into corresponding fields of table @lt_data_result
                       from p2rx_eval_period inner join p2rx_rt
                                     on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                                    and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr
               and p2rx_eval_period~abkrs in @mt_payroll_areas
               and p2rx_eval_period~fpper = @mv_payroll_period
               and p2rx_eval_period~inper = @mv_payroll_period
               and p2rx_rt~lgart in @lt_lgart
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
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk )
             group by p2rx_rt~dct_pernr, p2rx_eval_period~fpper, p2rx_rt~lgart
%_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      lt_cond_data[] = lt_data_result[].
      delete lt_cond_data where ( lgart in mt_lgart_betrg or
                                  lgart in mt_lgart_anzhl ).
      lt_data_betrg[] = lt_data_result[].
      delete lt_data_betrg where lgart not in mt_lgart_betrg.
      lt_data_anzhl[] = lt_data_result[].
      delete lt_data_anzhl where lgart not in mt_lgart_anzhl.

      "Conditional  Data
      append lines of lt_cond_data to lt_cond_data_2.
      sort lt_cond_data_2 by dct_pernr ascending.
      clear lt_cond_data.
      loop at lt_cond_data_2 into data(ls_cond_data_2).
        collect ls_cond_data_2 into lt_cond_data.
      endloop.

      "anzhl
      append lines of lt_data_anzhl to lt_data_anzhl_2.
      sort lt_data_anzhl_2 by dct_pernr ascending.
      clear lt_data_anzhl.
      loop at lt_data_anzhl_2 into data(ls_data_anzhl_2).
        clear ls_data_anzhl_2-lgart.
        collect ls_data_anzhl_2 into lt_data_anzhl.
      endloop.

      "betrg
      append lines of lt_data_betrg to lt_data_betrg_2.
      sort lt_data_betrg_2 by dct_pernr ascending.
      clear lt_data_betrg.
      loop at lt_data_betrg_2 into data(ls_data_betrg_2).
        clear ls_data_betrg_2-lgart.
        collect ls_data_betrg_2 into lt_data_betrg.
      endloop.

    endif. "test/production

* Apply Conditional Wage types
* identify employees with conditional wage type amount
    refresh: lt_pernr.
    if not mt_condition_lgart is initial.
      ls_pernr-sign = 'I'. ls_pernr-option = 'EQ'.
      loop at lt_cond_data into ls_cond_data
        where lgart in mt_condition_lgart.
        ls_pernr-low = ls_pernr-high = ls_cond_data-dct_pernr.
        collect ls_pernr into lt_pernr.
      endloop.
* And delete all Employees not having Conditional Wage type
      if not lt_pernr is initial.
        delete lt_data_anzhl where not dct_pernr in lt_pernr.
        delete lt_data_betrg where not dct_pernr in lt_pernr.
      endif.
    endif.

    refresh: lt_pernr.
    if not mt_exc_condition_lgart is initial.
      ls_pernr-sign = 'I'. ls_pernr-option = 'EQ'.

      loop at lt_cond_data into ls_cond_data
        where lgart in mt_exc_condition_lgart.
        ls_pernr-low = ls_pernr-high = ls_cond_data-dct_pernr.
        collect ls_pernr into lt_pernr.
      endloop.

* And delete all Employees not having Conditional Wage type
      if not lt_pernr is initial.
        delete lt_data_anzhl where dct_pernr in lt_pernr.
        delete lt_data_betrg where dct_pernr in lt_pernr.
      endif.
    endif.

    sort lt_data_betrg by dct_pernr ascending.
    sort lt_data_anzhl by dct_pernr ascending.
    delete adjacent duplicates from lt_data_betrg.
    delete adjacent duplicates from lt_data_anzhl.
    delete lt_data_betrg where fpper <> mv_payroll_period.
    delete lt_data_anzhl where fpper <> mv_payroll_period.

* Collect Results according to criteria
    loop at lt_data_anzhl into ls_data_result.

*      check ls_data_result-anzhl between mv_compare_anzhl_low and mv_compare_anzhl_high.

* Check if Z99_NUM_WOSTD parameter present, if it is not then use the HIGH & LOW values.
      if mv_num_wostd is initial and mv_num_mostd is initial.
        check ls_data_result-anzhl between mv_compare_anzhl_low and mv_compare_anzhl_high.
      else.
*Flag to reference field WOSTD for the NUM value. The number value in the parameter is the multiplier for field WOSTD
*i.e.IT7 weekly working hours field (WOSTD) multiplied by the value held in the parameter.
        refresh: lt_p0007.
* Read infotype 0007
        call function 'HR_READ_INFOTYPE'
          exporting
            pernr           = ls_data_result-dct_pernr
            infty           = '0007'
            begda           = mv_endda
            endda           = mv_endda
          importing
            subrc           = lv_retcd
          tables
            infty_tab       = lt_p0007
          exceptions
            infty_not_found = 1.

        if not lt_p0007 is initial.
          clear: ls_p0007.
          read table lt_p0007 into ls_p0007 index 1.

* Weekly Hours
          if not mv_num_wostd is initial.
            lv_wostd = ls_p0007-wostd * mv_multiplication_num.

            clear lv_wostd_comparison.
            call method zusecl_m99_pcc_chk_fp4_base=>dynamic_check_operation
              exporting
                iv_opcode = mv_comparison_oparator
                iv_var01  = ls_data_result-anzhl
                iv_var02  = lv_wostd
              receiving
                rv_result = lv_wostd_comparison.

            check lv_wostd_comparison eq abap_true.
          endif.

* Monthly Hours
          if not mv_num_mostd is initial.
            lv_mostd = ls_p0007-mostd * mv_mostd_mult_num.
            clear lv_mostd_comparison.
            call method zusecl_m99_pcc_chk_fp4_base=>dynamic_check_operation
              exporting
                iv_opcode = mv_mostd_comp_oparator
                iv_var01  = ls_data_result-anzhl
                iv_var02  = lv_mostd
              receiving
                rv_result = lv_mostd_comparison.

            check lv_mostd_comparison eq abap_true.
          endif.

        endif.
      endif.

      ls_result_count-dct_pernr = ls_data_result-dct_pernr.
      ls_result_count-count = 1.
      append ls_result_count to mt_bat_result_count_1.

    endloop.

    loop at lt_data_betrg into ls_data_result.

      check ls_data_result-betrg between mv_compare_betrg_low and mv_compare_betrg_high.

      ls_result_count-dct_pernr = ls_data_result-dct_pernr.
      ls_result_count-count = 1.
      append ls_result_count to mt_bat_result_count_2.

    endloop.


  endmethod.


  method READ_TIME_DATA.
* Read Absence data from it 2001
    data: lt_it2001    type standard table of ty_time_data with non-unique key table_line,
          lt_time_data type standard table of ty_time_data with non-unique key table_line.
    data: lv_tabix type sy-tabix.

                                                            "IT 2001
    refresh: lt_it2001.
    select it2001~pernr as pernr, it2001~subty as subty,
           it2001~begda as begda, it2001~endda as endda, it2001~seqnr as seqnr,
           it2001~awart as awart, it2001~stdaz as stdaz
       into corresponding fields of table @lt_it2001 from pa2001 as it2001
     where it2001~pernr = @iv_pernr
       and it2001~sprps = ' '
       and it2001~endda >= @iv_begda
       and it2001~begda <= @iv_endda.
    loop at lt_it2001 into data(ls_it2001).
      lv_tabix = sy-tabix.
      move mc_it2001 to ls_it2001-infty.
      modify lt_it2001 index lv_tabix from ls_it2001 transporting infty.
    endloop.

    append lines of lt_it2001 to lt_time_data.
    refresh: lt_it2001.

    sort lt_time_data by pernr begda endda subty.
    et_time_data[] = lt_time_data[].

  endmethod.
ENDCLASS.
