class zcl_m99_pa_it0045_coy_code definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.

    constants mc_itemid_subty type pyd_s_rdsfo_ext-itemid value 'SUBTY' ##NO_TEXT.
    constants mc_param_subty type pyd_par_type value 'Z99_FILTER_SUBTYPE_01' ##NO_TEXT.
    constants mc_infty_0045 type infty value '0045' ##NO_TEXT.
protected section.

  types:
    begin of ty_data_for_db,
        subty type subty,
        begda type begda,
      end of ty_data_for_db .
  types:
    begin of ty_data,
        pernr type pernr_d.
        include type ty_data_for_db.
      types: end of ty_data .
  types:
    tty_data type sorted table of ty_data with unique key pernr subty begda .
  types:
    begin of ty_subty_text,
        subty      type subty,
        subty_text type sbttx,
      end of ty_subty_text .
  types:
    tty_subty_text type hashed table of ty_subty_text with unique key subty .

  data MT_OUTPUT type TTY_DATA .
  data MT_FILTER_SUBTY type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_SUBTY_TEXT type TTY_SUBTY_TEXT .
  data MV_PREV_PAYROLL_BEGDA type BEGDA .
  data MV_PREV_PAYROLL_ENDDA type ENDDA .

  methods GET_SUBTY_TEXT
    importing
      !IV_PERNR type PERNR_D
      !IV_SUBTY type SUBTY
    returning
      value(RV_TEXT) type SBTTX .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_IT0045_COY_CODE IMPLEMENTATION.


  method check.
*check if IT45 exists in pay period and the team member changes payroll area.
    types: begin of lty_data,
             pernr   type pa0045-pernr,
             subty   type pa0045-subty,
             begda_1 type pa0001-begda,
             endda_1 type pa0001-endda,
             begda_2 type pa0001-begda,
             endda_2 type pa0001-endda,
           end of lty_data.
    types: begin of ty_it01_abkrs,
             pernr type pa0001-pernr,
             begda type pa0001-begda,
             endda type pa0001-endda,
             abkrs type pa0001-abkrs,
             permo type t549a-permo,
           end of ty_it01_abkrs.

    data: lt_data   type standard table of lty_data,
          ls_output like line of mt_output.
    data: lt_pa01                      type standard table of ty_it01_abkrs,
          lv_prev_permo                type t549a-permo,
          lv_date_payroll_area_changed type d.

    data: lt_output type  tty_data.

* Previous Period.

*
    select distinct it45~pernr, it45~subty,
      it01_1~begda as begda_1, it01_1~endda as endda_1,
      it01_2~begda as begda_2, it01_2~endda as endda_2
      into corresponding fields of table @lt_data
      from pa0045 as it45
        inner join pa0001 as it01_1
          on it45~pernr = it01_1~pernr
        inner join pa0001 as it01_2
          on it01_1~pernr = it01_2~pernr
      where it45~pernr in @it_pernr_so                      and
            it45~subty in @mt_filter_subty                  and
            it45~sprps = @if_hrpa_read_infotype=>unlocked   and
            it45~begda <= @mv_endda                         and
            it45~endda >= @mv_endda                         and
            it01_1~sprps = @if_hrpa_read_infotype=>unlocked and
            it01_2~sprps = @if_hrpa_read_infotype=>unlocked and
            "check if one of the following conditions is true:
            "1. Payroll Area between start & end date of current period are different
            "2. Payroll Area in current period is different to
            "   one in previous period (current pay period start date - 1)
            "Please note: I do not join with pa0045 & pa0001 with
            "pa0045-begda <= pa0001-endda and pa0045-endda >= pa0001-begda
            "because pa0045 can start mid-pay period and we want to check all
            "pa0001 records in one pay period
            (
              "1st condition
              (
                it01_1~begda <= @mv_endda      and
                it01_1~endda >= @mv_begda      and
                it01_2~begda <= @mv_endda      and
                it01_2~endda >= @mv_begda      and
                it01_1~abkrs <> it01_2~abkrs
              ) or
              "2nd condition
              (
                it01_1~begda <= @mv_prev_payroll_endda and
                it01_1~endda >= @mv_prev_payroll_begda and
                it01_2~begda <= @mv_endda             and
                it01_2~endda >= @mv_begda             and
                it01_1~abkrs <> it01_2~abkrs          and
                "make sure that IT45 record exists in prev period as well
                exists (
                  select 1
                  from pa0045 as it45_prev
                  where it45_prev~pernr = it01_1~pernr                     and
                        it45~subty in @mt_filter_subty                     and
                        it45_prev~sprps = @if_hrpa_read_infotype=>unlocked and
                        it45_prev~begda <= @mv_prev_payroll_endda          and
                        it45_prev~endda >= @mv_prev_payroll_begda
                )
              )
            ) and
            exists ( select 1
              from pa0000 as it00
                inner join pa0001 as it01 on
                  it00~pernr = it01~pernr
              where it00~pernr = it45~pernr                   and
                it00~begda <= it45~endda                      and
                it00~endda >= it45~begda                      and
                it01~begda <= it45~endda                      and
                it01~endda >= it45~begda                      and
                it00~begda <= @mv_endda                       and
                it00~endda >= @mv_begda                       and
                it01~begda <= @mv_endda                       and
                it01~endda >= @mv_endda                       and
                it00~stat2 in @mt_stat2                       and
                it00~sprps = @if_hrpa_read_infotype=>unlocked and
                it01~sprps = @if_hrpa_read_infotype=>unlocked and
                it01~abkrs in @mt_payroll_areas               and
                it01~bukrs in @mt_bukrs                       and
                it01~werks in @mt_werks                       and
                it01~btrtl in @mt_btrtl                       and
                it01~persg in @mt_persg                       and
                it01~persk in @mt_persk                       and
                it01~kostl in @mt_kostl
            ).

    "Find the date when Payroll Area changes.
    "Please note that, the BEGDA & ENDDA of IT01 records from above query
    "may not be necessarily the date when the Payroll Area changes.
    "For example: monthly pay period 01.02.2020 - 29.02.2020
    "01.02.2020 - 09.02.2020 - Payroll Area: Q1
    "10.02.2020 - 19.02.2020 - Payroll Area: Q3
    "20.02.2020 - 29.02.2020 - Payroll Area: Q3
    "Above query returns 1st and 3rd record only.

    if lt_data is not initial.
      select distinct t01~pernr t01~begda t01~endda t01~abkrs ta~permo
        into corresponding fields of table lt_pa01
        from pa0001 as t01
        inner join t549a as ta
          on t01~abkrs = ta~abkrs
        for all entries in lt_data
        where t01~pernr = lt_data-pernr and
              t01~sprps = if_hrpa_read_infotype=>unlocked and
              t01~begda <= lt_data-endda_2 and
              t01~endda >= lt_data-begda_1.
      if sy-subrc = 0.
        sort lt_pa01 by pernr begda endda.
        loop at lt_data into data(ls_data).
          clear: lv_prev_permo,
                 lv_date_payroll_area_changed.
          loop at lt_pa01 into data(ls_pa01) where pernr = ls_data-pernr.
            if lv_prev_permo is initial.
              lv_prev_permo = ls_pa01-permo.
            endif.
            if lv_prev_permo <> ls_pa01-permo.
              lv_date_payroll_area_changed = ls_pa01-begda.
              exit.
            endif.
          endloop.

          if not lv_date_payroll_area_changed is initial.
            clear: ls_output.
            ls_output-pernr = ls_data-pernr.
            ls_output-subty = ls_data-subty.
            ls_output-begda = lv_date_payroll_area_changed.
            insert ls_output into table lt_output.
          endif.
        endloop.
      endif.
    endif.

* Collect data for Overview
    append lines of lt_output to mt_output.

    loop at lt_output into ls_output
      group by ( pernr = ls_output-pernr ) ascending
      without members
      assigning field-symbol(<group>).
      append value #(  par_type = if_pyd_cont_types=>gcs_par_type-pernr
              id = <group>-pernr ) to rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Display error message
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    data:
      ls_err_ov        type ty_s_err_ov,
      ls_sfo           type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr         type p_pernr,
      lv_text          type text120,
      lv_value         type char060,
      ls_output_for_db type ty_data_for_db,
      lt_sfo_tab_temp  like ls_err_ov-sfo_tab,
      lv_value_string  type string,
      lv_subty_text    type sbttx.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters                                     "MOD001++
        get_parameters( it_par         = it_par             "MOD001++
                        io_res_context = io_res_context ).  "MOD001++
        loop at ct_err_ov into ls_err_ov.
          "move SFO_TAB records to temporary table
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.
          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            if ls_sfo_tab_temp-itemid = mc_itemid_subty.
              clear: ls_output_for_db, lv_value, lv_value_string, lv_text.
              lv_value = ls_sfo_tab_temp-value.
              call method me->copy_structure_to_other
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ls_output_for_db.

              write ls_output_for_db-begda to lv_text.
              lv_subty_text = get_subty_text( iv_pernr = lv_pernr iv_subty = ls_output_for_db-subty ).
              message i085 with lv_subty_text ls_output_for_db-subty into lv_value_string.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_subty
                  iv_text                     = |{ lv_text }|
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.
            else.
              insert ls_sfo_tab_temp into table ls_err_ov-sfo_tab.
            endif.
          endloop.

          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.
          loop at mt_output into data(ls_output) where pernr = lv_pernr.
            clear: lv_value.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_subty.
            ls_output_for_db = corresponding #( ls_output ).

            call method me->copy_structure_to_other
              exporting
                p_struct1 = ls_output_for_db
              changing
                p_struct2 = lv_value.

            ls_sfo-value = lv_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.
          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_specifc_custmizing.
* Get parameters specific for this validation rule
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

    try.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_param_subty
          changing
            ct_parameter_tab = mt_filter_subty.

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

        lv_pabrj = lv_vabrj .
        lv_pabrp = lv_vabrp.
        lo_payroll_area->get_period_info(
         exporting
           imp_pabrj = lv_pabrj
           imp_pabrp = lv_pabrp
         importing
           exp_vabrj = lv_vabrj
           exp_vabrp = lv_vabrp
           exp_begda = lv_begda
           exp_endda = lv_endda ).

        mv_prev_payroll_begda = lv_begda.
        mv_prev_payroll_endda = lv_endda.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method get_subty_text.
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    read table mt_subty_text into data(ls_subty_text) with table key subty = iv_subty.
    if sy-subrc = 0.
      rv_text = ls_subty_text-subty_text.
    else.
      call function 'HR_GET_SUBTYPE_TEXT'
        exporting
          infty               = mc_infty_0045
          subty               = iv_subty
          persnr              = iv_pernr
          begda               = sy-datum
          endda               = sy-datum
*         molga               = mc_molga_au     "MOD001--
          molga               = mv_molga        "MOD001++
        importing
          stext               = rv_text
        exceptions
          infty_not_found     = 1
          subty_not_found     = 2
          infty_not_supported = 3
          others              = 4.
      if sy-subrc = 0.
        clear: ls_subty_text.
        ls_subty_text-subty = iv_subty.
        ls_subty_text-subty_text = rv_text.
        insert ls_subty_text into table mt_subty_text.
      endif.
    endif.
  endmethod.
ENDCLASS.
