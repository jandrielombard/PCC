class zcl_m99_pa_it7_w_it8 definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.

    types:
      begin of ty_it08,
        pernr type pa0008-pernr,
        subty type pa0008-subty,
        objps type pa0008-objps,
        sprps type pa0008-sprps,
        endda type pa0008-endda,
        begda type pa0008-begda,
        seqnr type pa0008-seqnr,
        lga01 type  pa0008-lga01,  bet01 type  pa0008-bet01,
        lga02 type  pa0008-lga02,  bet02 type  pa0008-bet02,
        lga03 type  pa0008-lga03,  bet03 type  pa0008-bet03,
        lga04 type  pa0008-lga04,  bet04 type  pa0008-bet04,
        lga05 type  pa0008-lga05,  bet05 type  pa0008-bet05,
        lga06 type  pa0008-lga06,  bet06 type  pa0008-bet06,
        lga07 type  pa0008-lga07,  bet07 type  pa0008-bet07,
        lga08 type  pa0008-lga08,  bet08 type  pa0008-bet08,
        lga09 type  pa0008-lga09,  bet09 type  pa0008-bet09,
        lga10 type  pa0008-lga10,  bet10 type  pa0008-bet10,
        lga11 type  pa0008-lga11,  bet11 type  pa0008-bet11,
        lga12 type  pa0008-lga12,  bet12 type  pa0008-bet12,
        lga13 type  pa0008-lga13,  bet13 type  pa0008-bet13,
        lga14 type  pa0008-lga14,  bet14 type  pa0008-bet14,
        lga15 type  pa0008-lga15,  bet15 type  pa0008-bet15,
        lga16 type  pa0008-lga16,  bet16 type  pa0008-bet16,
        lga17 type  pa0008-lga17,  bet17 type  pa0008-bet17,
        lga18 type  pa0008-lga18,  bet18 type  pa0008-bet18,
        lga19 type  pa0008-lga19,  bet19 type  pa0008-bet19,
        lga20 type  pa0008-lga20,  bet20 type  pa0008-bet20,
        lga21 type  pa0008-lga21,  bet21 type  pa0008-bet21,
        lga22 type  pa0008-lga22,  bet22 type  pa0008-bet22,
        lga23 type  pa0008-lga23,  bet23 type  pa0008-bet23,
        lga24 type  pa0008-lga24,  bet24 type  pa0008-bet24,
        lga25 type  pa0008-lga25,  bet25 type  pa0008-bet25,
        lga26 type  pa0008-lga26,  bet26 type  pa0008-bet26,
        lga27 type  pa0008-lga27,  bet27 type  pa0008-bet27,
        lga28 type  pa0008-lga28,  bet28 type  pa0008-bet28,
        lga29 type  pa0008-lga29,  bet29 type  pa0008-bet29,
        lga30 type  pa0008-lga30,  bet30 type  pa0008-bet30,
        lga31 type  pa0008-lga31,  bet31 type  pa0008-bet31,
        lga32 type  pa0008-lga32,  bet32 type  pa0008-bet32,
        lga33 type  pa0008-lga33,  bet33 type  pa0008-bet33,
        lga34 type  pa0008-lga34,  bet34 type  pa0008-bet34,
        lga35 type  pa0008-lga35,  bet35 type  pa0008-bet35,
        lga36 type  pa0008-lga36,  bet36 type  pa0008-bet36,
        lga37 type  pa0008-lga37,  bet37 type  pa0008-bet37,
        lga38 type  pa0008-lga38,  bet38 type  pa0008-bet38,
        lga39 type  pa0008-lga39,  bet39 type  pa0008-bet39,
        lga40 type  pa0008-lga40,  bet40 type  pa0008-bet40,
      end of ty_it08 .

    constants mc_itemid_it07_with_it08 type pyd_s_rdsfo_ext-itemid value 'IT7_W_IT8' ##NO_TEXT.
    constants mc_filter_lgart type pyd_par_type value 'Z99_FILTER_LGART' ##NO_TEXT.
    constants mc_exc_filter_lgart type pyd_par_type value 'Z99_EXC_FILTER_LGART' ##NO_TEXT.
    constants mc_number_of_wagetypes type i value 40 ##NO_TEXT.
    constants mc_dates_mismatch type char1 value 'A'.
    constants mc_no_salary_update type char1 value 'B'.
    constants mc_infty_0007 type infty value '0007'.
    constants mc_infty_0008 type infty value '0008'.

  protected section.

    types:
      begin of ty_it7chg_it8nochg,
        pernr     type pa0007-pernr,
        begda     type pa0007-begda,
        endda     type pa0007-endda,
        begda_pay type pa0008-begda,
        endda_pay type pa0008-endda,
        reason    type char1,
      end of ty_it7chg_it8nochg .
    types:
      tty_it7chg_it8nochg type standard table of ty_it7chg_it8nochg .
    types:
      begin of ty_error_dtls,
        begda     type pa0007-begda,
        endda     type pa0007-endda,
        begda_pay type pa0008-begda,
        endda_pay type pa0008-endda,
        reason    type char1,
      end of ty_error_dtls .
    types:
      tty_amount type sorted table of pad_amt7s with unique key table_line .

    data mt_it7chg_it8nochg type tty_it7chg_it8nochg .
    data ms_error_dtls type ty_error_dtls .
    data mt_filter_lgart type /iwbep/t_cod_select_options .

    methods append_data
      importing
        !is_data type ty_it7chg_it8nochg
      changing
        !ct_data type tty_it7chg_it8nochg .
    methods get_distinct_wage_types
      importing
        !is_it08            type ty_it08
        !it_filter_lgart    type /iwbep/t_cod_select_options
      changing
        !ct_distinct_amount type tty_amount .
    methods read_infotype_details
      importing
        !iv_pernr  type pernr_d
      exporting
        !et_p0007  type p0007_tab
        !et_ppbwla type hrhcp00_pbwla .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.

    types:
      begin of ty_it07,
        pernr type pa0007-pernr,
        subty type pa0007-subty,
        objps type pa0007-objps,
        sprps type pa0007-sprps,
        endda type pa0007-endda,
        begda type pa0007-begda,
        seqnr type pa0007-seqnr,
        wostd type pa0007-wostd,
      end of ty_it07 .
ENDCLASS.



CLASS ZCL_M99_PA_IT7_W_IT8 IMPLEMENTATION.


  method append_data.
    read table ct_data transporting no fields with key
      pernr = is_data-pernr
      begda = is_data-begda
      endda = is_data-endda
      begda_pay = is_data-begda_pay
      endda_pay = is_data-endda_pay.
    if sy-subrc <> 0.
      insert is_data into table ct_data.
    endif.
  endmethod.


  method check.
*check if base hour (IT07-WOSTD) changes but salary does NOT change

    data: lt_it07            type sorted table of ty_it07 with unique key pernr subty objps sprps endda begda seqnr,
          lt_it07_prev       like lt_it07,
          lt_it07_all        like lt_it07,
          lt_it08            type sorted table of ty_it08 with unique key pernr subty objps sprps endda begda seqnr,
          lt_it08_prev       like lt_it08,
          ls_it7chg_it8nochg like line of mt_it7chg_it8nochg,
          ls_result          like line of rt_result.

    data: lt_distinct_number        type sorted table of pad_amt7s with unique key table_line,
          lt_distinct_number_prev   like lt_distinct_number,
          lv_wt_val_for_prev_it0007 type anzhl,
          lv_count                  type i,
          lv_tabix                  type sy-tabix,
          lv_prev_date              type d,

          lv_it7_min_begda          type d,
          lv_it7_max_endda          type d,
          lv_it8_min_begda          type d,
          lv_it8_max_endda          type d.
    field-symbols: <fs_wt>  type lgart,
                   <fs_val> type pad_amt7s.

    data: ls_distinct_number      type pad_amt7s,
          ls_distinct_number_prev type pad_amt7s.

    delete mt_filter_lgart
      where sign <> cl_cbase_constants=>ggc_range_equal-sign
         or option <> cl_cbase_constants=>ggc_range_equal-option
         or low is initial.
    if mt_filter_lgart is initial.
      return.
    endif.
    delete mt_filter_lgart from 2.

    select distinct it07~pernr, it07~subty,
      it07~objps, it07~sprps,
      it07~endda, it07~begda,
      it07~seqnr, it07~wostd
    into table @lt_it07
    from pa0007 as it07
    where it07~pernr in @it_pernr_so                     and
*        it07~aedtm between @mv_begda and @mv_endda_plus1 and
        it07~aedtm >= @mv_change_begda and
        it07~sprps = @if_hrpa_read_infotype=>unlocked    and
        "This is to check that the previous IT0007 has different
        "hours per week (WOSTD)
        exists ( select *
                 from pa0007 as it07_2
                 where it07_2~pernr = it07~pernr     and
                       it07_2~subty = it07~subty     and
                       it07_2~sprps = @if_hrpa_read_infotype=>unlocked and
                       it07_2~endda < it07~begda     and
                       it07_2~begda < it07~begda     and
                       it07_2~wostd <> it07~wostd    and
                       "This is to make sure that only the immediate
                       "previous record without any gap in date
                       not exists ( select *
                                    from pa0007 as it07_3
                                    where it07_3~pernr = it07_2~pernr  and
                                          it07_3~subty = it07_2~subty  and
                                          it07_3~sprps = @if_hrpa_read_infotype=>unlocked and
                                          it07_3~endda > it07_2~endda  and
                                          it07_3~begda > it07_2~endda  and
                                          it07_3~endda < it07~begda    and
                                          it07_3~begda < it07~begda
                                   ) )
      and exists ( select 1
                    from pa0000 as it00 inner join pa0001 as it01 on
                      it00~pernr = it01~pernr
                    where it00~pernr = it07~pernr            and
                          (
                            "Scenario: retro or future dated infotype record (outside pay period)
                            "because last modified date is used, this means that old/future record
                            "can be included as long as the last modified date
                            "is in current period. Thus, it00~begda <= it07~endda &
                            "it00~endda >= it07~begda cannot be used because
                            "period of IT07 does not intersect with current period.
                            (
*                              it07~aedtm between @mv_begda and @mv_endda_plus1 and
                               it07~aedtm >= @mv_change_begda and
                              (
                                ( it07~begda < @mv_begda       and it07~endda < @mv_begda ) or
                                ( it07~endda > @mv_endda_plus1 and it07~begda > @mv_endda_plus1 )
                              )
                            )
                            or
                            (
                              "Scenario: infotype record within pay period
                              "last modified date is used and IT07 period intersects with
                              "current period. it00~begda <= it07~endda &
                              "it00~endda >= it07~begda can be used to get more accurate
                              "result if there is a split in current period
*                              it07~aedtm between @mv_begda and @mv_endda_plus1 and
                              it07~aedtm >= @mv_change_begda and
                              it07~begda <= @mv_endda_plus1  and
                              it07~endda >= @mv_begda
                            )
                          )                                  and
                          it00~begda <= @mv_endda_plus1      and
                          it00~endda >= @mv_begda            and
                          it01~begda <= @mv_endda_plus1      and
                          it01~endda >= @mv_begda            and
                          it00~stat2 in @mt_stat2            and
                          it00~sprps = @if_hrpa_read_infotype=>unlocked and
                          it01~sprps = @if_hrpa_read_infotype=>unlocked and
                          it01~abkrs in @mt_payroll_areas    and
                          it01~bukrs in @mt_bukrs            and
                          it01~werks in @mt_werks            and
                          it01~btrtl in @mt_btrtl            and
                          it01~persg in @mt_persg            and
                          it01~persk in @mt_persk            and
                          it01~kostl in @mt_kostl )
      order by it07~pernr, it07~subty, it07~objps, it07~endda, it07~begda.

    "Get the previous IT0007 record
    if lt_it07 is not initial.
      select it07_2~pernr, it07_2~subty,
        it07_2~objps, it07_2~sprps,
        it07_2~endda, it07_2~begda,
        it07_2~seqnr, it07_2~wostd
      into corresponding fields of table @lt_it07_prev
      from pa0007 as it07_2
        for all entries in @lt_it07
      where
        it07_2~pernr = @lt_it07-pernr and
        it07_2~subty = @lt_it07-subty and
        it07_2~sprps = @if_hrpa_read_infotype=>unlocked and
        it07_2~begda < @lt_it07-begda and
        it07_2~endda < @lt_it07-begda and
        exists ( select *
            from pa0000 as it00
            where it00~pernr = it07_2~pernr  and
                  it00~begda <= it07_2~endda and
                  it00~endda >= it07_2~begda and
                  it00~stat2 in @mt_stat2
          ) and
        not exists ( select *
                       from pa0007 as it07_3
                       where it07_3~pernr = it07_2~pernr   and
                             it07_3~subty = it07_2~subty   and
                             it07_3~sprps = @if_hrpa_read_infotype=>unlocked and
                             it07_3~endda > it07_2~endda   and
                             it07_3~begda > it07_2~endda   and
                             it07_3~endda < @lt_it07-begda and
                             it07_3~begda < @lt_it07-begda )
       order by primary key.
    endif.

    insert lines of lt_it07 into table lt_it07_all.
    "there may be duplicate record so check each record to make
    "sure that the record does not exist in internal table
    "LT_IT07_ALL already
    loop at lt_it07_prev into data(ls_it07_prev).
      loop at lt_it07_all transporting no fields
        where pernr = ls_it07_prev-pernr
          and endda >= ls_it07_prev-begda
          and begda <= ls_it07_prev-endda.
        exit.
      endloop.
      if sy-subrc <> 0.
        insert ls_it07_prev into table lt_it07_all.
      endif.
    endloop.

    if lt_it07_all is not initial.
      select it08~pernr, it08~subty, it08~objps, it08~sprps,
        it08~endda, it08~begda, it08~seqnr,
        it08~lga01, it08~bet01, it08~lga02, it08~bet02,
        it08~lga03, it08~bet03, it08~lga04, it08~bet04,
        it08~lga05, it08~bet05, it08~lga06, it08~bet06,
        it08~lga07, it08~bet07, it08~lga08, it08~bet08,
        it08~lga09, it08~bet09, it08~lga10, it08~bet10,
        it08~lga11, it08~bet11, it08~lga12, it08~bet12,
        it08~lga13, it08~bet13, it08~lga14, it08~bet14,
        it08~lga15, it08~bet15, it08~lga16, it08~bet16,
        it08~lga17, it08~bet17, it08~lga18, it08~bet18,
        it08~lga19, it08~bet19, it08~lga20, it08~bet20,
        it08~lga21, it08~bet21, it08~lga22, it08~bet22,
        it08~lga23, it08~bet23, it08~lga24, it08~bet24,
        it08~lga25, it08~bet25, it08~lga26, it08~bet26,
        it08~lga27, it08~bet27, it08~lga28, it08~bet28,
        it08~lga29, it08~bet29, it08~lga30, it08~bet30,
        it08~lga31, it08~bet31, it08~lga32, it08~bet32,
        it08~lga33, it08~bet33, it08~lga34, it08~bet34,
        it08~lga35, it08~bet35, it08~lga36, it08~bet36,
        it08~lga37, it08~bet37, it08~lga38, it08~bet38,
        it08~lga39, it08~bet39, it08~lga40, it08~bet40
      into corresponding fields of table @lt_it08
      from pa0008 as it08
        for all entries in @lt_it07_all
        where it08~pernr = @lt_it07_all-pernr
        and it08~endda >= @lt_it07_all-begda
        and it08~begda <= @lt_it07_all-endda
        and it08~sprps = @if_hrpa_read_infotype=>unlocked
      order by primary key.
    endif.

    "only loop through 'current' IT07
    loop at lt_it07 into data(ls_it07).
      clear: lt_distinct_number,
             lt_distinct_number_prev,
             lv_it7_min_begda,
             lv_it7_max_endda,
             lv_it8_min_begda,
             lv_it8_max_endda,
             ls_it7chg_it8nochg.

      lv_it7_min_begda = ls_it07-begda.
      lv_it7_max_endda = ls_it07-endda.

      loop at lt_it08 into data(ls_it08) where
        pernr = ls_it07-pernr and
        begda <= ls_it07-endda and
        endda >= ls_it07-begda.
        if lv_it8_min_begda is initial or lv_it8_min_begda > ls_it08-begda.
          lv_it8_min_begda = ls_it08-begda.
        endif.
        if lv_it8_max_endda is initial or lv_it8_max_endda < ls_it08-endda.
          lv_it8_max_endda = ls_it08-endda.
        endif.

        get_distinct_wage_types(
          exporting
            is_it08 = ls_it08
            it_filter_lgart = mt_filter_lgart
          changing
            ct_distinct_amount = lt_distinct_number ).
      endloop.

      describe table lt_distinct_number lines lv_count.
* check for existence of wage type in Z99_FILTER_LGART
* -If exists continue with validation
* -If DOES NOT exist, end validation for employee
      if lv_count eq 0.
        continue.
      endif.

      if lv_count > 1.
        "more than one WT values for one IT0007 record so it's invalid
        ls_it7chg_it8nochg = value ty_it7chg_it8nochg(
            pernr = ls_it07-pernr
            begda = lv_it7_min_begda
            endda = lv_it7_max_endda
            begda_pay = lv_it8_min_begda
            endda_pay = lv_it8_max_endda
            reason = mc_dates_mismatch ).
        append_data(
          exporting
            is_data = ls_it7chg_it8nochg
          changing
            ct_data = mt_it7chg_it8nochg ).

        clear: ls_result.
        ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
        ls_result-id = ls_it7chg_it8nochg-pernr.
        insert ls_result into table rt_result.
        continue.
      endif.

      "get only the closest previous IT0007 record.
      "There may be more than one previous IT0007 records so get the closest one.
      clear: lv_prev_date.
      lv_prev_date = ls_it07-begda - 1.
      loop at lt_it07_prev into ls_it07_prev where
        pernr = ls_it07-pernr and
        subty = ls_it07-subty and
        objps = ls_it07-objps and
        begda <= lv_prev_date and
        endda >= lv_prev_date and
        begda < ls_it07-begda and
        endda < ls_it07-begda.
        lv_tabix = sy-tabix.
      endloop.
      if sy-subrc = 0.
        read table lt_it07_prev into ls_it07_prev index lv_tabix.
        if sy-subrc = 0.

          "get IT0008 records for previous IT0007 record
          loop at lt_it08 into ls_it08
            where pernr = ls_it07_prev-pernr
            and begda <= ls_it07_prev-endda
            and endda >= ls_it07_prev-begda.

            get_distinct_wage_types(
              exporting
                is_it08 = ls_it08
                it_filter_lgart = mt_filter_lgart
              changing
                ct_distinct_amount = lt_distinct_number_prev ).
          endloop.

          describe table lt_distinct_number_prev lines lv_count.
          if lv_count > 1.
            "more than one WT values for one IT0007 record so it's invalid
            ls_it7chg_it8nochg = value ty_it7chg_it8nochg(
                pernr = ls_it07-pernr
                begda = lv_it7_min_begda
                endda = lv_it7_max_endda
                begda_pay = lv_it8_min_begda
                endda_pay = lv_it8_max_endda
                reason = mc_dates_mismatch ).
            append_data(
              exporting
                is_data = ls_it7chg_it8nochg
              changing
                ct_data = mt_it7chg_it8nochg ).

            clear: ls_result.
            ls_result-id = ls_it7chg_it8nochg-pernr.
            ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
            insert ls_result into table rt_result.
            continue.
          endif.

          clear: ls_distinct_number, ls_distinct_number_prev.
          read table lt_distinct_number into ls_distinct_number index 1.
          read table lt_distinct_number_prev into ls_distinct_number_prev index 1.
          "if IT08 WT amount is the same as previous record, we want to raise a message since
          "the amounts should not be the same because IT07-WOSTD values are not the same
          if ls_distinct_number = ls_distinct_number_prev.
            ls_it7chg_it8nochg = value ty_it7chg_it8nochg(
                pernr = ls_it07-pernr
                begda = lv_it7_min_begda
                endda = lv_it7_max_endda
                begda_pay = lv_it8_min_begda
                endda_pay = lv_it8_max_endda
                reason = mc_no_salary_update ).
            append_data(
              exporting
                is_data = ls_it7chg_it8nochg
              changing
                ct_data = mt_it7chg_it8nochg ).

            clear: ls_result.
            ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
            ls_result-id = ls_it7chg_it8nochg-pernr.
            insert ls_result into table rt_result.
          endif.

        endif.
      endif.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Display error message
    data:
      ls_err_ov          type ty_s_err_ov,
      ls_sfo             type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif           type abap_bool,
      lv_pernr           type p_pernr,
      lv_value           type pyd_s_rdsfo_ext-value,
      ls_it7chg_it8nochg like line of mt_it7chg_it8nochg.

    data: lv_text type pyd_name.
    data: lt_p0007  type p0007_tab,
          lt_ppbwla type  hrhcp00_pbwla.

    data: lv_char_value type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_info_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    "get parameters
    get_parameters( it_par         = it_par
                    io_res_context = io_res_context ).
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.
          clear ls_err_ov-sfo_tab.

          call method me->read_infotype_details
            exporting
              iv_pernr  = lv_pernr
            importing
              et_p0007  = lt_p0007
              et_ppbwla = lt_ppbwla.

          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_error_dtls.

            move-corresponding ms_error_dtls to ls_it7chg_it8nochg.
            move lv_pernr to ls_it7chg_it8nochg-pernr.

* Reason
            lv_info_txt = text-001.
            case ls_it7chg_it8nochg-reason.
              when mc_dates_mismatch.
                lv_text = text-002.
              when mc_no_salary_update.
                lv_text = text-003.
            endcase.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_it07_with_it08
                iv_text                     = lv_info_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.
* Report Infotype Details
          loop at lt_p0007 into data(ls_p0007).
            clear: lv_info_txt.
            if sy-tabix = 1.
              lv_info_txt = text-004.
            endif.

            message e038 with ls_p0007-begda
              ls_p0007-endda ls_p0007-wostd into lv_value.

            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_it07_with_it08
                iv_text                     = lv_info_txt
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.
* IT 00008 Details
          loop at lt_ppbwla into data(ls_ppbwla).
            clear: lv_info_txt.
            if sy-tabix = 1.
              lv_info_txt = text-005.
            endif.

            message e039 with ls_ppbwla-begda
              ls_ppbwla-endda ls_ppbwla-betrg into lv_value.

            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_it07_with_it08
                iv_text                     = lv_info_txt
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
* Execute Mode
        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_it7chg_it8nochg into ls_it7chg_it8nochg
            where pernr = lv_pernr.

            move-corresponding ls_it7chg_it8nochg to ms_error_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_error_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_it07_with_it08.
            ls_sfo-value = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_distinct_wage_types.
    data: lv_field_no(2)    type n,
          lv_field_wt_name  type string,
          lv_field_val_name type string.
    field-symbols: <fs_wt>  type lgart,
                   <fs_val> type pad_amt7s.
    do mc_number_of_wagetypes times.
      lv_field_no = sy-index.
      concatenate zcl_m99_pa_it0008_comp_wt=>mc_field_prefix_wage_type lv_field_no into lv_field_wt_name.
      concatenate zcl_m99_pa_it0008_comp_wt=>mc_field_prefix_amount lv_field_no into lv_field_val_name.

      assign component lv_field_wt_name of structure is_it08 to <fs_wt>.
      if <fs_wt> is assigned and <fs_wt> in mt_filter_lgart.
        assign component lv_field_val_name of structure is_it08 to <fs_val>.
        if <fs_val> is assigned.
          insert <fs_val> into table ct_distinct_amount.
          unassign: <fs_val>.
        endif.
        unassign: <fs_wt>.
      endif.
    enddo.
  endmethod.


  method get_specifc_custmizing.
* Get parameters specific for this validation rule
    try.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_lgart
          changing
            ct_parameter_tab = mt_filter_lgart.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.


  method read_infotype_details.
* Read Infotype Details
    data: lv_begda type begda.
    data: lt_p0001 type p0001_tab,
          lt_p0007 type p0007_tab,
          lt_p0008 type p0008_tab.
    data lt_ppbwla  type hrhcp00_pbwla.

* Read Previous Period and Current Period
    lv_begda = mv_begda - 1.
* Read Employee IT 0007
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr           = iv_pernr
        infty           = mc_infty_0007
      tables
        infty_tab       = lt_p0007
      exceptions
        infty_not_found = 1
        invalid_input   = 2
        others          = 3.

    delete lt_p0007
     where not ( begda <= mv_endda and endda >= lv_begda ).
    et_p0007[] = lt_p0007[]. clear lt_p0007.

    call function 'HR_READ_INFOTYPE'
      exporting
        pernr           = iv_pernr
        infty           = mc_infty_0008
      tables
        infty_tab       = lt_p0008
      exceptions
        infty_not_found = 1
        invalid_input   = 2
        others          = 3.

    loop at lt_p0008 into data(ls_p0008)
        where begda <= mv_endda and endda >= lv_begda .
* Read IT 00008 Details
      call function 'RP_FILL_WAGE_TYPE_TABLE_EXT'
        exporting
          pernr                        = iv_pernr
          begda                        = ls_p0008-begda
          endda                        = ls_p0008-endda
        tables
          pp0001                       = lt_p0001
          pp0007                       = lt_p0007
          pp0008                       = lt_p0008
          ppbwla                       = lt_ppbwla
        exceptions
          error_at_indirect_evaluation = 1
          others                       = 2.

* Filter the wage types
      delete lt_ppbwla where lgart not in mt_filter_lgart.
      append lines of lt_ppbwla to et_ppbwla.
    endloop.

  endmethod.
ENDCLASS.
