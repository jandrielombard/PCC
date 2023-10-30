class ZUSECL_M99_PA_IT0008_COMP_WT definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  types:
    begin of ty_lgart_text,
        lgart      type lgart,
        lgart_text type t512t-lgtxt,
      end of ty_lgart_text .
  types:
    tty_lgart_text type hashed table of ty_lgart_text with unique key lgart .
  types:
    begin of ty_it08,
        pernr type pa0008-pernr,
        subty type pa0008-subty,
        objps type pa0008-objps,
        sprps type pa0008-sprps,
        endda type pa0008-endda,
        begda type pa0008-begda,
        seqnr type pa0008-seqnr,
        lga01 type  pa0008-lga01,  bet01 type  pa0008-bet01, ein01 type  pa0008-ein01,
        lga02 type  pa0008-lga02,  bet02 type  pa0008-bet02, ein02 type  pa0008-ein02,
        lga03 type  pa0008-lga03,  bet03 type  pa0008-bet03, ein03 type  pa0008-ein03,
        lga04 type  pa0008-lga04,  bet04 type  pa0008-bet04, ein04 type  pa0008-ein04,
        lga05 type  pa0008-lga05,  bet05 type  pa0008-bet05, ein05 type  pa0008-ein05,
        lga06 type  pa0008-lga06,  bet06 type  pa0008-bet06, ein06 type  pa0008-ein06,
        lga07 type  pa0008-lga07,  bet07 type  pa0008-bet07, ein07 type  pa0008-ein07,
        lga08 type  pa0008-lga08,  bet08 type  pa0008-bet08, ein08 type  pa0008-ein08,
        lga09 type  pa0008-lga09,  bet09 type  pa0008-bet09, ein09 type  pa0008-ein09,
        lga10 type  pa0008-lga10,  bet10 type  pa0008-bet10, ein10 type  pa0008-ein10,
        lga11 type  pa0008-lga11,  bet11 type  pa0008-bet11, ein11 type  pa0008-ein11,
        lga12 type  pa0008-lga12,  bet12 type  pa0008-bet12, ein12 type  pa0008-ein12,
        lga13 type  pa0008-lga13,  bet13 type  pa0008-bet13, ein13 type  pa0008-ein13,
        lga14 type  pa0008-lga14,  bet14 type  pa0008-bet14, ein14 type  pa0008-ein14,
        lga15 type  pa0008-lga15,  bet15 type  pa0008-bet15, ein15 type  pa0008-ein15,
        lga16 type  pa0008-lga16,  bet16 type  pa0008-bet16, ein16 type  pa0008-ein16,
        lga17 type  pa0008-lga17,  bet17 type  pa0008-bet17, ein17 type  pa0008-ein17,
        lga18 type  pa0008-lga18,  bet18 type  pa0008-bet18, ein18 type  pa0008-ein18,
        lga19 type  pa0008-lga19,  bet19 type  pa0008-bet19, ein19 type  pa0008-ein19,
        lga20 type  pa0008-lga20,  bet20 type  pa0008-bet20, ein20 type  pa0008-ein20,
        lga21 type  pa0008-lga21,  bet21 type  pa0008-bet21, ein21 type  pa0008-ein21,
        lga22 type  pa0008-lga22,  bet22 type  pa0008-bet22, ein22 type  pa0008-ein22,
        lga23 type  pa0008-lga23,  bet23 type  pa0008-bet23, ein23 type  pa0008-ein23,
        lga24 type  pa0008-lga24,  bet24 type  pa0008-bet24, ein24 type  pa0008-ein24,
        lga25 type  pa0008-lga25,  bet25 type  pa0008-bet25, ein25 type  pa0008-ein25,
        lga26 type  pa0008-lga26,  bet26 type  pa0008-bet26, ein26 type  pa0008-ein26,
        lga27 type  pa0008-lga27,  bet27 type  pa0008-bet27, ein27 type  pa0008-ein27,
        lga28 type  pa0008-lga28,  bet28 type  pa0008-bet28, ein28 type  pa0008-ein28,
        lga29 type  pa0008-lga29,  bet29 type  pa0008-bet29, ein29 type  pa0008-ein29,
        lga30 type  pa0008-lga30,  bet30 type  pa0008-bet30, ein30 type  pa0008-ein30,
        lga31 type  pa0008-lga31,  bet31 type  pa0008-bet31, ein31 type  pa0008-ein31,
        lga32 type  pa0008-lga32,  bet32 type  pa0008-bet32, ein32 type  pa0008-ein32,
        lga33 type  pa0008-lga33,  bet33 type  pa0008-bet33, ein33 type  pa0008-ein33,
        lga34 type  pa0008-lga34,  bet34 type  pa0008-bet34, ein34 type  pa0008-ein34,
        lga35 type  pa0008-lga35,  bet35 type  pa0008-bet35, ein35 type  pa0008-ein35,
        lga36 type  pa0008-lga36,  bet36 type  pa0008-bet36, ein36 type  pa0008-ein36,
        lga37 type  pa0008-lga37,  bet37 type  pa0008-bet37, ein37 type  pa0008-ein37,
        lga38 type  pa0008-lga38,  bet38 type  pa0008-bet38, ein38 type  pa0008-ein38,
        lga39 type  pa0008-lga39,  bet39 type  pa0008-bet39, ein39 type  pa0008-ein39,
        lga40 type  pa0008-lga40,  bet40 type  pa0008-bet40, ein40 type  pa0008-ein40,
      end of ty_it08 .
  types:
    tty_lgart type sorted table of pa0008-lga01 with unique key table_line .
  types:
    begin of ty_lgart_sum,
        bet_sum type pa0008-bet01,
        ein_sum type pa0008-ein01,
      end of ty_lgart_sum .
  types:
    begin of ty_it08_comp_wt,
        pernr      type pa0008-pernr,
        t_lgart_01 type tty_lgart,
        bet_sum_01 type ty_lgart_sum,
        t_lgart_02 type tty_lgart,
        bet_sum_02 type ty_lgart_sum,
      end of ty_it08_comp_wt .
  types:
    tty_it08_comp_wt type standard table of ty_it08_comp_wt with non-unique key pernr .

  constants MC_FILTER_LGART_01 type PYD_PAR_TYPE value 'Z99_FILTER_LGART_01' ##NO_TEXT.
  constants MC_FILTER_LGART_02 type PYD_PAR_TYPE value 'Z99_FILTER_LGART_02' ##NO_TEXT.
  constants MC_CONDITION_SUBTY type PYD_PAR_TYPE value 'Z99_CONDITION_SUBTY' ##NO_TEXT.
  constants MC_FIELD_PREFIX_WAGE_TYPE type NAME_FELD value 'LGA' ##NO_TEXT.
  constants MC_FIELD_PREFIX_AMOUNT type NAME_FELD value 'BET' ##NO_TEXT.
  constants MC_FIELD_PREFIX_UNIT type NAME_FELD value 'EIN' ##NO_TEXT.
  constants MC_NUMBER_OF_WAGETYPES type I value 40 ##NO_TEXT.
  constants MC_FIELD_PREFIX_NUMBER type NAME_FELD value 'ANZ' ##NO_TEXT.
  constants MC_ITEMID_IT0008_COMP_WT type PYD_S_RDSFO_EXT-ITEMID value 'LGART' ##NO_TEXT.
  constants MC_VALUE_SEPARATOR type CHAR01 value ',' ##NO_TEXT.

  class-methods SUM_OF_WT
    importing
      !IS_IT08 type TY_IT08
      !IT_FILTER type /IWBEP/T_COD_SELECT_OPTIONS
    exporting
      !ES_SUM type TY_LGART_SUM
      !EV_ERROR type ABAP_BOOL
      !ET_LGART type TTY_LGART .
  class-methods IS_WT_FOUND
    importing
      !IV_LGART type LGART
      !IV_DATA type TY_IT08
    exporting
      !EV_FOUND type ABAP_BOOL
      !EV_BETXX type PA0008-BET01
      !EV_ANZXX type PA0008-ANZ01
      !EV_EINXX type PA0008-EIN01 .
  protected section.

    data mt_filter_lgart_01 type /iwbep/t_cod_select_options .
    data mt_filter_lgart_02 type /iwbep/t_cod_select_options .
    data mt_condition_subty type /iwbep/t_cod_select_options .
    data mt_it08_comp_wt type tty_it08_comp_wt .
    data mt_lgart_text type tty_lgart_text .

    methods get_lgart_text
      importing
        !iv_lgart      type lgart
      returning
        value(rv_text) type t512t-lgtxt .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT0008_COMP_WT IMPLEMENTATION.


  method CHECK.
    data: ls_it08_comp_wt     like line of mt_it08_comp_wt,
          lt_it08             type sorted table of ty_it08 with unique key pernr subty objps endda begda seqnr,
          ls_result           like line of rt_result,
          lv_bet_sum_01       type ty_lgart_sum,
          lv_bet_sum_02       type ty_lgart_sum,
          lt_lgart_01         type tty_lgart,
          lt_lgart_02         type tty_lgart,
          lv_is_error         type abap_bool,
          lt_filter_lgart_all like mt_filter_lgart_01.

    loop at mt_filter_lgart_01 into data(ls_filter_lgart_01).
      read table lt_filter_lgart_all transporting no fields with key
        sign = ls_filter_lgart_01-sign
        option = ls_filter_lgart_01-option
        low = ls_filter_lgart_01-low
        high = ls_filter_lgart_01-high.
      if sy-subrc <> 0.
        append ls_filter_lgart_01 to lt_filter_lgart_all.
      endif.
    endloop.
    loop at mt_filter_lgart_02 into data(ls_filter_lgart_02).
      read table lt_filter_lgart_all transporting no fields with key
        sign = ls_filter_lgart_02-sign
        option = ls_filter_lgart_02-option
        low = ls_filter_lgart_02-low
        high = ls_filter_lgart_02-high.
      if sy-subrc <> 0.
        append ls_filter_lgart_02 to lt_filter_lgart_all.
      endif.
    endloop.

    if lt_filter_lgart_all is initial.
      return.
    endif.

    select it08~pernr, it08~subty, it08~objps,
           it08~endda, it08~begda, it08~seqnr,
           it08~lga01, it08~bet01, it08~ein01, it08~lga02, it08~bet02, it08~ein02,
           it08~lga03, it08~bet03, it08~ein03, it08~lga04, it08~bet04, it08~ein04,
           it08~lga05, it08~bet05, it08~ein05, it08~lga06, it08~bet06, it08~ein06,
           it08~lga07, it08~bet07, it08~ein07, it08~lga08, it08~bet08, it08~ein08,
           it08~lga09, it08~bet09, it08~ein09, it08~lga10, it08~bet10, it08~ein10,
           it08~lga11, it08~bet11, it08~ein11, it08~lga12, it08~bet12, it08~ein12,
           it08~lga13, it08~bet13, it08~ein13, it08~lga14, it08~bet14, it08~ein14,
           it08~lga15, it08~bet15, it08~ein15, it08~lga16, it08~bet16, it08~ein16,
           it08~lga17, it08~bet17, it08~ein17, it08~lga18, it08~bet18, it08~ein18,
           it08~lga19, it08~bet19, it08~ein19, it08~lga20, it08~bet20, it08~ein20,
           it08~lga21, it08~bet21, it08~ein21, it08~lga22, it08~bet22, it08~ein22,
           it08~lga23, it08~bet23, it08~ein23, it08~lga24, it08~bet24, it08~ein24,
           it08~lga25, it08~bet25, it08~ein25, it08~lga26, it08~bet26, it08~ein26,
           it08~lga27, it08~bet27, it08~ein27, it08~lga28, it08~bet28, it08~ein28,
           it08~lga29, it08~bet29, it08~ein29, it08~lga30, it08~bet30, it08~ein30,
           it08~lga31, it08~bet31, it08~ein31, it08~lga32, it08~bet32, it08~ein32,
           it08~lga33, it08~bet33, it08~ein33, it08~lga34, it08~bet34, it08~ein34,
           it08~lga35, it08~bet35, it08~ein35, it08~lga36, it08~bet36, it08~ein36,
           it08~lga37, it08~bet37, it08~ein37, it08~lga38, it08~bet38, it08~ein38,
           it08~lga39, it08~bet39, it08~ein39, it08~lga40, it08~bet40, it08~ein40
      into corresponding fields of table @lt_it08
      from pa0008 as it08
        where it08~pernr in @it_pernr_so                    and
              it08~endda >= @mv_begda                       and
              it08~begda <= @mv_endda                       and
              it08~sprps = @if_hrpa_read_infotype=>unlocked and
              ( it08~lga01 in @lt_filter_lgart_all or it08~lga02 in @lt_filter_lgart_all or
                it08~lga03 in @lt_filter_lgart_all or it08~lga04 in @lt_filter_lgart_all or
                it08~lga05 in @lt_filter_lgart_all or it08~lga06 in @lt_filter_lgart_all or
                it08~lga07 in @lt_filter_lgart_all or it08~lga08 in @lt_filter_lgart_all or
                it08~lga09 in @lt_filter_lgart_all or it08~lga10 in @lt_filter_lgart_all or
                it08~lga11 in @lt_filter_lgart_all or it08~lga12 in @lt_filter_lgart_all or
                it08~lga13 in @lt_filter_lgart_all or it08~lga14 in @lt_filter_lgart_all or
                it08~lga15 in @lt_filter_lgart_all or it08~lga16 in @lt_filter_lgart_all or
                it08~lga17 in @lt_filter_lgart_all or it08~lga18 in @lt_filter_lgart_all or
                it08~lga19 in @lt_filter_lgart_all or it08~lga20 in @lt_filter_lgart_all or
                it08~lga21 in @lt_filter_lgart_all or it08~lga22 in @lt_filter_lgart_all or
                it08~lga23 in @lt_filter_lgart_all or it08~lga24 in @lt_filter_lgart_all or
                it08~lga25 in @lt_filter_lgart_all or it08~lga26 in @lt_filter_lgart_all or
                it08~lga27 in @lt_filter_lgart_all or it08~lga28 in @lt_filter_lgart_all or
                it08~lga29 in @lt_filter_lgart_all or it08~lga30 in @lt_filter_lgart_all or
                it08~lga31 in @lt_filter_lgart_all or it08~lga32 in @lt_filter_lgart_all or
                it08~lga33 in @lt_filter_lgart_all or it08~lga34 in @lt_filter_lgart_all or
                it08~lga35 in @lt_filter_lgart_all or it08~lga36 in @lt_filter_lgart_all or
                it08~lga37 in @lt_filter_lgart_all or it08~lga38 in @lt_filter_lgart_all or
                it08~lga39 in @lt_filter_lgart_all or it08~lga40 in @lt_filter_lgart_all ) and
            exists ( select 1
                    from pa0000 as it00 inner join pa0001 as it01 on
                      it00~pernr = it01~pernr
                    where it00~pernr = it08~pernr            and
                          it00~begda <= it08~endda           and
                          it00~endda >= it08~begda           and
                          it00~begda <= @mv_endda            and
                          it00~endda >= @mv_begda            and
                          it00~stat2 in @mt_stat2            and
                          it00~sprps = @if_hrpa_read_infotype=>unlocked and
                          it01~begda <= it08~endda           and
                          it01~endda >= it08~begda           and
                          it01~begda <= @mv_endda            and
                          it01~endda >= @mv_begda            and
                          it01~sprps = @if_hrpa_read_infotype=>unlocked and
                          it01~abkrs in @mt_payroll_areas    and
                          it01~bukrs in @mt_bukrs            and
                          it01~werks in @mt_werks            and
                          it01~btrtl in @mt_btrtl            and
                          it01~persg in @mt_persg            and
                          it01~persk in @mt_persk            and
                          it01~kostl in @mt_kostl )          and
            not exists ( select 1
                      from pa2001 as it2001
                    where it2001~pernr = it08~pernr          and
                          it2001~subty in @mt_condition_subty and
                          it2001~begda <= @mv_endda          and
                          it2001~endda >= @mv_begda          and
                          it2001~sprps = @if_hrpa_read_infotype=>unlocked )
      order by primary key.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.

    loop at lt_it08 into data(ls_it08).
      clear: lv_bet_sum_01,
             lv_bet_sum_02,
             lv_is_error,
             lt_lgart_01,
             lt_lgart_02,
             ls_it08_comp_wt.
      sum_of_wt(
        exporting
          is_it08 = ls_it08
          it_filter = mt_filter_lgart_01
        importing
          es_sum = lv_bet_sum_01
          ev_error = lv_is_error
          et_lgart = lt_lgart_01 ).
      if lv_is_error = abap_false.
        sum_of_wt(
          exporting
            is_it08 = ls_it08
            it_filter = mt_filter_lgart_02
          importing
            es_sum = lv_bet_sum_02
            ev_error = lv_is_error
            et_lgart = lt_lgart_02 ).

        if lv_is_error = abap_false.
          "if not found, treated as 0
          if lv_bet_sum_01-ein_sum <> lv_bet_sum_02-ein_sum.
            lv_is_error = abap_true.
          elseif lv_bet_sum_01-bet_sum <> lv_bet_sum_02-bet_sum.
            lv_is_error = abap_true.
          endif.
        endif.
      endif.

      if lv_is_error = abap_true.

        ls_result-id = ls_it08-pernr.
        insert ls_result into table rt_result.

        ls_it08_comp_wt-pernr = ls_it08-pernr.
        ls_it08_comp_wt-t_lgart_01 = lt_lgart_01.
        ls_it08_comp_wt-bet_sum_01 = lv_bet_sum_01.
        ls_it08_comp_wt-t_lgart_02 = lt_lgart_02.
        ls_it08_comp_wt-bet_sum_02 = lv_bet_sum_02.
        insert ls_it08_comp_wt into table mt_it08_comp_wt.
      endif.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.

    data:
      ls_err_ov       type ty_s_err_ov,
      ls_sfo          type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif        type abap_bool,
      lv_pernr        type p_pernr,
      lv_value        type string,
      ls_it08_comp_wt like line of mt_it08_comp_wt,
      lv_lgart_01     type string,
      lv_lgart_02     type string.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear: ls_sfo.

          lv_pernr = ls_err_ov-id.

          loop at mt_it08_comp_wt into ls_it08_comp_wt
            where pernr = lv_pernr.


            lv_lgart_01 = reduce #( init text = `` sep = ``
               for ls_lgart_temp in ls_it08_comp_wt-t_lgart_01[]
               next text = text && sep && get_lgart_text( ls_lgart_temp ) && '(' && ls_lgart_temp && ')'
                    sep = mc_value_separator && ` ` ).
            lv_lgart_02 = reduce #( init text = `` sep = ``
               for ls_lgart_temp in ls_it08_comp_wt-t_lgart_02[]
               next text = text && sep && get_lgart_text( ls_lgart_temp ) && '(' && ls_lgart_temp && ')'
                    sep = mc_value_separator && ` ` ).

            if lines( ls_it08_comp_wt-t_lgart_01 ) <= 1.
              message e040 into lv_value.
              lv_value = | { lv_lgart_01 } - { ls_it08_comp_wt-bet_sum_01-bet_sum } { lv_value } { lv_lgart_02 } - { ls_it08_comp_wt-bet_sum_02-bet_sum }|.
            else.
              message e041 into lv_value.
              lv_value = |{ lv_lgart_01 } - { ls_it08_comp_wt-bet_sum_01-bet_sum } { lv_value } { lv_lgart_02 } - { ls_it08_comp_wt-bet_sum_02-bet_sum }|.
            endif.

            add_record_to_sfo_tab(
              exporting
                iv_itemid = mc_itemid_it0008_comp_wt
                iv_text   = |{ text-001 }|
                iv_value  = lv_value
              changing
                ct_sfo_tab = ls_err_ov-sfo_tab ).
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_LGART_TEXT.
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    read table mt_lgart_text into data(ls_lgart_text) with table key lgart = iv_lgart.
    if sy-subrc = 0.
      rv_text = ls_lgart_text-lgart_text.
    else.
      call function 'HR_GET_LGART_TEXT'
        exporting
          p_lgart          = iv_lgart
*         p_molga          = mc_molga_au        "MOD001--
          p_molga          = mv_molga           "MOD001++
        importing
          p_longtext       = rv_text
        exceptions
          no_entry_in_512t = 1
          others           = 2.
      clear: ls_lgart_text.
      ls_lgart_text-lgart = iv_lgart.
      ls_lgart_text-lgart_text = rv_text.
      insert ls_lgart_text into table mt_lgart_text.
    endif.
  endmethod.


  method GET_SPECIFC_CUSTMIZING.
    try.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_lgart_01
          changing
            ct_parameter_tab = mt_filter_lgart_01.

        delete mt_filter_lgart_01 where
          sign <> if_dmf_constants_c=>gc_range_sign_inclusive or
          option <> if_dmf_constants_c=>gc_range_option_equal or
          low is initial.

        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_lgart_02
          changing
            ct_parameter_tab = mt_filter_lgart_02.

        delete mt_filter_lgart_02 where
          sign <> if_dmf_constants_c=>gc_range_sign_inclusive or
          option <> if_dmf_constants_c=>gc_range_option_equal or
          low is initial.

* Read Conditional Subtypes for IT 2001
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_condition_subty
          changing
            ct_parameter_tab = mt_condition_subty.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method IS_WT_FOUND.
    data: lv_field_no(2)       type n,
          lv_field_wt_name     type string,
          lv_field_val_name    type string,
          lv_field_number_name type string,
          lv_field_unit_name   type string.
    field-symbols: <fs_wt>     type pa0008-lga01,
                   <fs_val>    type pa0008-bet01,
                   <fs_number> type pa0008-anz01,
                   <fs_unit>   type pa0008-ein01.
    clear: ev_found, ev_betxx, ev_anzxx, ev_einxx.
    check iv_lgart is not initial.

    do mc_number_of_wagetypes times.
      lv_field_no = sy-index.
      concatenate mc_field_prefix_wage_type lv_field_no into lv_field_wt_name.
      concatenate mc_field_prefix_amount lv_field_no into lv_field_val_name.
      concatenate mc_field_prefix_unit lv_field_no into lv_field_unit_name.

      assign component lv_field_wt_name of structure iv_data to <fs_wt>.
      if <fs_wt> is assigned.
        if <fs_wt> is initial.
          "if there is no wage type in current sy-index,
          "remaining fields will not be populated.
          "Thus, there is no need to search further
          unassign <fs_wt>.
          exit.
        elseif <fs_wt> = iv_lgart.
          assign component lv_field_val_name of structure iv_data to <fs_val>.
          if <fs_val> is assigned.
            ev_betxx = <fs_val>.
            ev_found = abap_true.
            unassign <fs_val>.
          endif.

          concatenate mc_field_prefix_unit lv_field_no into lv_field_unit_name.
          assign component lv_field_unit_name of structure iv_data to <fs_unit>.
          if <fs_unit> is assigned.
            ev_einxx = <fs_unit>.
            unassign <fs_unit>.
          endif.

          if ev_anzxx is requested.
            concatenate mc_field_prefix_number lv_field_no into lv_field_number_name.
            assign component lv_field_number_name of structure iv_data to <fs_number>.
            if <fs_number> is assigned.
              ev_anzxx = <fs_number>.
              unassign <fs_number>.
            endif.
          endif.
          unassign <fs_wt>.
          exit.
        endif.
      endif.
    enddo.
  endmethod.


  method SUM_OF_WT.
    data: lv_betxx_01 type pa0008-bet01,
          lv_einxx_01 type pa0008-ein01,
          lv_found    type abap_bool.
    clear: es_sum, ev_error, et_lgart.

    loop at it_filter into data(ls_filter)
      where sign = if_dmf_constants_c=>gc_range_sign_inclusive
        and option = if_dmf_constants_c=>gc_range_option_equal
        and low is not initial.
      clear: lv_betxx_01, lv_einxx_01, lv_found.

      is_wt_found(
        exporting
          iv_lgart = |{ ls_filter-low }|
          iv_data  = is_it08
        importing
          ev_betxx = lv_betxx_01
          ev_einxx = lv_einxx_01
          ev_found = lv_found ).
      if lv_found = abap_true.
        if es_sum is initial.
          es_sum-bet_sum = lv_betxx_01.
          es_sum-ein_sum = lv_einxx_01.
          insert conv lgart( ls_filter-low ) into table et_lgart.
        else.
          if es_sum-ein_sum <> lv_einxx_01.
            ev_error = abap_true.
            exit.
          else.
            add lv_betxx_01 to es_sum-bet_sum.
            insert conv lgart( ls_filter-low ) into table et_lgart.
          endif.
        endif.
      endif.
    endloop.
  endmethod.
ENDCLASS.
