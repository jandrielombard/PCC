class ZUSECL_M99_PA_IT0015_GPPL definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.

    types:
      begin of ty_it0015_dtls,
        subty   type subty,
        betrg   type maxbt,
        exc_sum type maxbt,
        waers   type waers.
    types: end of ty_it0015_dtls .

    types:
      begin of ty_it0015,
        pernr   type pernr_d,
        subty   type subty,
        begda   type begda,
        betrg   type maxbt,
        exc_sum type maxbt,
        waers   type waers,
        lgtxt   type t512t-lgtxt.
    types: end of ty_it0015 .
    types:
      tty_it0015 type table of ty_it0015 .

    types:
      begin of ty_it0015_sum,
        pernr   type pernr_d,
        subty   type subty,
        betrg   type maxbt,
        exc_sum type maxbt,
        waers   type waers.
    types: end of ty_it0015_sum .
    types:
      tty_it0015_sum type table of ty_it0015_sum .

    data mt_it0015 type tty_it0015 .
    data ms_it0015 type ty_it0015 .

    data mt_it0015_sum type tty_it0015_sum .
    data ms_it0015_sum type ty_it0015_sum .

    data ms_it0015_dtls type ty_it0015_dtls .
    constants mc_infty_0015 type infty value '0015' ##NO_TEXT.
    constants mc_itemid_gpplerr type pyd_itemid value 'GPPLERR' ##NO_TEXT.
    constants mc_filter_lgart_01 type pyd_par_type value 'Z99_FILTER_LGART_01' ##NO_TEXT.
    constants mc_exc_filter_lgart_01 type pyd_par_type value 'Z99_EXC_FILTER_LGART_01' ##NO_TEXT.
    constants mc_text_001 type pyd_name value 'amount paid in last 12 months'.
    constants mc_text_002 type pyd_name value 'Threshold'.
    constants mc_text_003 type pyd_name value 'has been exceeded by' .
    data mt_filter_lgart_01 type /iwbep/t_cod_select_options .
    data mc_amt_low_01 type pyd_par_type value 'Z99_AMT_LOW_01' ##NO_TEXT.
    data mv_amt_low_01 type maxbt .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT0015_GPPL IMPLEMENTATION.


  method CHECK.
* Reached the GPPL Payment Threshold in the Current Period
    data: lv_1year_begda type begda,
          lv_1year_endda type endda.
    data: lt_it0015       type table of ty_it0015.
    data: lt_chgd_it0015  type table of ty_it0015.
    data: lt_yr_it0015 type table of ty_it0015.
    data: ls_it0015    type ty_it0015.
    data: lt_it0015_sum type  tty_it0015_sum,
          ls_it0015_sum type  ty_it0015_sum.
    data: lv_prev_pernr type pernr_d.

    data: ls_result  type ty_s_result.

* Set Year Begda and Endda
    lv_1year_endda = mv_endda.
    call function 'CCM_GO_BACK_MONTHS'
      exporting
        currdate   = lv_1year_endda
        backmonths = 12
      importing
        newdate    = lv_1year_begda.

* All relevant employees with GPPL Payment Records in current period
    select it0015~pernr, it0015~subty, it0015~begda, it0015~betrg, it0015~waers
      into corresponding fields of table @lt_it0015 from pa0015 as it0015
        where it0015~pernr in @it_pernr_so
          and it0015~subty in @mt_filter_lgart_01
          and it0015~sprps = ' '
          and it0015~begda <= @mv_endda
          and it0015~endda >= @mv_begda
          and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0015~pernr
                         and it0000~begda <= @mv_endda
                         and it0000~endda >= @mv_begda
                         and it0000~stat2 in @mt_stat2
                         and it0000~sprps = ' '
                         and it0001~begda <= @mv_endda
                         and it0001~endda >= @mv_begda
                         and it0001~sprps = ' '
                         and it0001~abkrs in @mt_payroll_areas
                         and it0001~bukrs in @mt_bukrs
                         and it0001~werks in @mt_werks
                         and it0001~btrtl in @mt_btrtl
                         and it0001~persg in @mt_persg
                         and it0001~persk in @mt_persk
                         and it0001~kostl in @mt_kostl ).

* All employees with GPPL Payment Records changed in current period
    select it0015~pernr, it0015~subty, it0015~begda, it0015~betrg, it0015~waers
      into corresponding fields of table @lt_chgd_it0015 from pa0015 as it0015
        where it0015~pernr in @it_pernr_so
          and it0015~subty in @mt_filter_lgart_01
          and it0015~sprps = ' '
          and it0015~begda <= @lv_1year_endda
          and it0015~endda >= @lv_1year_begda
          and it0015~aedtm >= @mv_begda
          and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0015~pernr
                         and it0000~begda <= @mv_endda
                         and it0000~endda >= @mv_begda
                         and it0000~stat2 in @mt_stat2
                         and it0000~sprps = ' '
                         and it0001~begda <= @mv_endda
                         and it0001~endda >= @mv_begda
                         and it0001~sprps = ' '
                         and it0001~abkrs in @mt_payroll_areas
                         and it0001~bukrs in @mt_bukrs
                         and it0001~werks in @mt_werks
                         and it0001~btrtl in @mt_btrtl
                         and it0001~persg in @mt_persg
                         and it0001~persk in @mt_persk
                         and it0001~kostl in @mt_kostl ).

* Consolidate the records for Validation Process
    append lines of lt_it0015 to lt_chgd_it0015.
    sort lt_chgd_it0015 by pernr subty begda descending.
    delete adjacent duplicates from lt_chgd_it0015 comparing all fields.

    refresh: lt_it0015.
    lt_it0015[] = lt_chgd_it0015[].
    delete adjacent duplicates from lt_it0015 comparing pernr.

    if not lt_it0015[] is initial.

* Read Sum of GPPL Payments for the year
      if not lt_it0015[] is initial.
        select it0015~pernr, it0015~subty, it0015~begda, it0015~betrg, it0015~waers
          into corresponding fields of table @lt_yr_it0015
          from pa0015 as it0015
          for all entries in @lt_it0015
         where it0015~pernr = @lt_it0015-pernr
           and it0015~subty in @mt_filter_lgart_01
           and it0015~sprps = ' '
           and it0015~begda <= @lv_1year_endda
           and it0015~endda >= @lv_1year_begda.
      endif.

* SUM Is not possible for All entries hence use collect to cal sum
      loop at lt_yr_it0015 into data(ls_yr_it0015).
        move-corresponding ls_yr_it0015 to ls_it0015_sum.
        collect ls_it0015_sum into lt_it0015_sum.
      endloop.
      sort lt_it0015_sum.
      if not mv_amt_low_01 is initial.
        delete lt_it0015_sum where not betrg gt mv_amt_low_01.
      endif.

    endif.

* Prepare Result
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    clear: lv_prev_pernr.
    loop at lt_it0015_sum into ls_it0015_sum.
      if ls_it0015_sum-pernr ne lv_prev_pernr.
* Build Results table
        ls_result-id = ls_it0015_sum-pernr.
        insert ls_result into table rt_result.

        lv_prev_pernr = ls_it0015_sum-pernr.

* Collect the details for Overview
        move-corresponding ls_it0015_sum to ms_it0015_sum.
        ms_it0015_sum-exc_sum = ms_it0015_sum-betrg - mv_amt_low_01.
        append ms_it0015_sum to mt_it0015_sum.

      endif.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.
  endmethod.


  method ERR_OV_GET_LIST.
* Method for Overview list display
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr,
      lv_value  type pyd_item_value.
    data: lv_text type pyd_name.

    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
    data: lv_it_begda(10) type c.
    data: lv_threshold_vlaue type ltext,
          lv_exc_sum         type ltext.


* Populate SFO tab
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

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
                p_struct2 = ms_it0015_dtls.

            move-corresponding ms_it0015_dtls to ms_it0015.
            ms_it0015-pernr = lv_pernr.

* Read IT0015 Subtype text
            select single lgtxt into ms_it0015-lgtxt
              from  t512t
             where sprsl  = sy-langu
*               and molga  = mc_molga_au                 "MOD001--
               and molga  = mv_molga                     "MOD001++
               and lgart  = ms_it0015-subty.

            concatenate ms_it0015-lgtxt '(' ms_it0015-subty ')'
              into data(lv_wt_text).

            if not ms_it0015-betrg is initial.
              write ms_it0015-betrg to lv_char_value currency ms_it0015-waers left-justified.
              lv_text = |$| && lv_char_value.
            endif.

* Message 1
            concatenate lv_wt_text mc_text_001 lv_text into lv_text
             separated by space.
            lv_txt = mc_text_reason.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_gpplerr
                iv_text                     = lv_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Message 2
            clear: lv_text.
            lv_threshold_vlaue = |$| && mv_amt_low_01.
            if not ms_it0015-exc_sum is initial.
              write ms_it0015-exc_sum to lv_char_value currency ms_it0015-waers left-justified.
              lv_exc_sum = |$| && lv_char_value.
            endif.

            concatenate mc_text_002 lv_threshold_vlaue mc_text_003 lv_exc_sum
              into lv_text separated by space.

            clear lv_txt.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_gpplerr
                iv_text                     = lv_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

* Populate SFO tab for table Save
          loop at mt_it0015_sum into ms_it0015_sum
            where pernr = lv_pernr.

            move-corresponding ms_it0015_sum to ms_it0015_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_it0015_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_gpplerr.
            ls_sfo-datum  = ms_it0015-begda.
            ls_sfo-value  = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Read Check Specific Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.

    try.
* Read GPPL Wage types
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_lgart_01
            iv_exc_par_type  = me->mc_exc_filter_lgart_01
          changing
            ct_parameter_tab = mt_filter_lgart_01.

* Read Amount Low
        me->mv_amt_low_01 = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_amt_low_01
                                                                    it_par = me->get_context( )->mt_par ).

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
