class zcl_m99_pa_pay_b4_hire definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.

    types:
      begin of ty_it41,
        pernr type pa0041-pernr,
        begda type pa0041-begda,
        endda type pa0041-endda.
        include type ps0041.
      types: end of ty_it41 .

    constants mc_itemid_pay_b4_hire type pyd_s_rdsfo_ext-itemid value 'HIRE_DATE' ##NO_TEXT.
    constants mc_field_prefix_date_type type name_feld value 'DAR' ##NO_TEXT.
    constants mc_field_prefix_date type name_feld value 'DAT' ##NO_TEXT.
    constants mc_payroll_area_all_q type abkrs value 'Q*' ##NO_TEXT.
    constants mc_payroll_area_all_n type abkrs value 'N*' ##NO_TEXT.

    class-methods get_it41_date_type_for_hire_dt
      importing
        !iv_molga           type molga default '13'
      returning
        value(rv_date_type) type pa0041-dar01 .
    class-methods is_date_type_found
      importing
        !iv_date_type type pa0041-dar01
        !is_it41      type ty_it41
      exporting
        !ev_found     type abap_bool
        !ev_date      type pa0041-dat01 .
  protected section.

    types:
      begin of ty_output_compact,
        abkrs_on_hire_date type pa0001-abkrs,
        hire_date_41       type pa0041-dat01,
        hire_date          type pa0000-begda,
      end of ty_output_compact,
      begin of ty_output,
        pernr    type pa0000-pernr,
        begda_41 type pa0041-begda,
        endda_41 type pa0041-endda.
        include type ty_output_compact.
      types: end of ty_output .
    types:
      tty_output type sorted table of ty_output with unique key pernr hire_date begda_41 endda_41 .

    data mt_output type tty_output .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_PAY_B4_HIRE IMPLEMENTATION.


  method check.

*compare between hire date derived from IT00 and hire date from IT41.
*Validate only team members with:
*- empty PA0003-ABRDT
*- only IT01 which payroll area starts with Q

    types: begin of lty_ee,
             pernr type pa0000-pernr,
             begda type pa0000-begda,
             endda type pa0000-endda,
           end of lty_ee.

    types: begin of lty_ee_hire_date,
             pernr     type pa0000-pernr,
             hire_date type pa0000-begda,
             abkrs     type pa0001-abkrs,
           end of lty_ee_hire_date.
    data: lv_date_type_for_hire_date type pa0041-dar01,
          ls_result                  like line of rt_result,
          lt_ee                      type sorted table of lty_ee with unique key pernr endda,
          lt_it41                    type sorted table of ty_it41 with unique key pernr begda endda,
          lt_hire_dates              type sorted table of lty_ee_hire_date with unique key pernr hire_date abkrs,
          lt_latest_hire_date        type sorted table of lty_ee_hire_date with unique key pernr,
          ls_latest_hire_date        like line of lt_latest_hire_date,
          lv_it41_hire_date          type pa0041-dat01,
          ls_output                  like line of mt_output,
          lr_abkrs_to_check          type range of pa0001-abkrs.

    lv_date_type_for_hire_date = get_it41_date_type_for_hire_dt( ).

    lr_abkrs_to_check = value #( (
      sign = if_dmf_constants_c=>gc_range_sign_inclusive
      option = if_dmf_constants_c=>gc_range_option_cp
      low = mc_payroll_area_all_q )
      (
      sign = if_dmf_constants_c=>gc_range_sign_inclusive
      option = if_dmf_constants_c=>gc_range_option_cp
      low = mc_payroll_area_all_n ) ).

    "get list of employees to process
    select distinct it00~pernr, it00~endda, it00~begda
      into corresponding fields of table @lt_ee
      from pa0000 as it00 inner join pa0001 as it01 on
        it00~pernr = it01~pernr  and
        it00~begda <= it01~endda and
        it00~endda >= it01~begda
    where it00~pernr in @it_pernr_so                    and
          it00~begda <= @mv_endda                       and
          it00~endda >= @mv_begda                       and
          it00~sprps = @if_hrpa_read_infotype=>unlocked and
          it00~stat2 in @mt_stat2                       and
          it01~begda <= @mv_endda                       and
          it01~endda >= @mv_begda                       and
          it01~sprps = @if_hrpa_read_infotype=>unlocked and
          it01~abkrs in @mt_payroll_areas               and
          it01~bukrs in @mt_bukrs                       and
          it01~werks in @mt_werks                       and
          it01~btrtl in @mt_btrtl                       and
          it01~persg in @mt_persg                       and
          it01~persk in @mt_persk                       and
          exists (
            select 1
            from pa0003 as it03
            where it03~pernr = it00~pernr  and
                  it03~begda <= it00~endda and
                  it03~endda >= it00~begda and
                  it03~abrdt eq '00000000'
          ).

    if lt_ee is initial.
      return.
    endif.

    "Get hire dates of employees.
    "Please note that there can be multiple hire dates if the employee
    "left and has joined back
    select distinct it00~pernr, it00~begda as hire_date, it01~abkrs
      into corresponding fields of table @lt_hire_dates
      from pa0000 as it00 inner join pa0001 as it01 on
        it00~pernr = it01~pernr and
        it00~begda <= it01~endda and
        it00~endda >= it01~begda
      for all entries in @lt_ee
      where it00~pernr = @lt_ee-pernr and
            it00~begda <= @lt_ee-endda and
            it00~endda <= @lt_ee-endda and
            it00~sprps = @if_hrpa_read_infotype=>unlocked and
            (
              (
                "no previous IT00 record
                not exists ( select *
                     from pa0000 as it00_prev
                     where it00_prev~pernr = it00~pernr and
                           it00_prev~endda < it00~begda )
              ) or
              (
                "there is an immediate previous IT00 record that is not active
                "or the immediate record is in another payroll area (not in @lr_abkrs_to_check)
                exists
                ( select *
                  from pa0000 as it00_prev
                  where it00_prev~pernr = it00~pernr and
                        it00_prev~endda < it00~begda and
                        (
                          ( it00_prev~stat2 <> '3' )
                          or
                          (
                            exists ( select *
                                from pa0001 as it01_prev
                                where it01_prev~pernr = it00_prev~pernr and
                                      it01_prev~begda <= it00_prev~endda and
                                      it01_prev~endda >= it00_prev~begda and
                                      it01_prev~abkrs not in @lr_abkrs_to_check
                            )
                          )
                        ) and
                        "this condition is needed to make sure that there is no gap between
                        "it00 record and it00_prev record
                        not exists ( select *
                                     from pa0000 as it00_prev_2
                                     where it00_prev_2~pernr = it00_prev~pernr and
                                           it00_prev_2~endda > it00_prev~endda and
                                           it00_prev_2~begda > it00_prev~endda and
                                           it00_prev_2~endda < it00~begda    and
                                           it00_prev_2~begda < it00~endda )
                )
              )
            ) and
            it01~abkrs in @lr_abkrs_to_check.

    "Get the highest hire date for each employee only for hire date that's before end of pay period.
    loop at lt_hire_dates into data(ls_hire_dates) where hire_date <= mv_endda.
      read table lt_latest_hire_date into ls_latest_hire_date with table key pernr = ls_hire_dates-pernr.
      if sy-subrc = 0.
        if ls_latest_hire_date-hire_date <> ls_hire_dates-hire_date.
          ls_latest_hire_date-hire_date = ls_hire_dates-hire_date.
          ls_latest_hire_date-abkrs = ls_hire_dates-abkrs.
          modify table lt_latest_hire_date from ls_latest_hire_date transporting hire_date abkrs.
        endif.
      else.
        ls_latest_hire_date-pernr = ls_hire_dates-pernr.
        ls_latest_hire_date-hire_date = ls_hire_dates-hire_date.
        ls_latest_hire_date-abkrs = ls_hire_dates-abkrs.
        insert ls_latest_hire_date into table lt_latest_hire_date.
      endif.
    endloop.

    if lt_latest_hire_date is initial.
      return.
    endif.

    select pernr, begda, endda,
      dar01, dat01, dar02, dat02,
      dar03, dat03, dar04, dat04,
      dar05, dat05, dar06, dat06,
      dar07, dat07, dar08, dat08,
      dar09, dat09, dar10, dat10,
      dar11, dat11, dar12, dat12,
      dar13, dat13, dar14, dat14,
      dar15, dat15, dar16, dat16,
      dar17, dat17, dar18, dat18,
      dar19, dat19, dar20, dat20,
      dar21, dat21, dar22, dat22,
      dar23, dat23, dar24, dat24
    from pa0041
      into corresponding fields of table @lt_it41
      for all entries in @lt_latest_hire_date
      where pernr = @lt_latest_hire_date-pernr and
        begda <= @mv_endda and
        endda >= @mv_begda and
        sprps = @if_hrpa_read_infotype=>unlocked and
        ( dar01 = @lv_date_type_for_hire_date or dar02 = @lv_date_type_for_hire_date or
          dar03 = @lv_date_type_for_hire_date or dar04 = @lv_date_type_for_hire_date or
          dar05 = @lv_date_type_for_hire_date or dar06 = @lv_date_type_for_hire_date or
          dar07 = @lv_date_type_for_hire_date or dar08 = @lv_date_type_for_hire_date or
          dar09 = @lv_date_type_for_hire_date or dar10 = @lv_date_type_for_hire_date or
          dar11 = @lv_date_type_for_hire_date or dar12 = @lv_date_type_for_hire_date or
          dar13 = @lv_date_type_for_hire_date or dar14 = @lv_date_type_for_hire_date or
          dar15 = @lv_date_type_for_hire_date or dar16 = @lv_date_type_for_hire_date or
          dar17 = @lv_date_type_for_hire_date or dar18 = @lv_date_type_for_hire_date or
          dar19 = @lv_date_type_for_hire_date or dar20 = @lv_date_type_for_hire_date or
          dar21 = @lv_date_type_for_hire_date or dar22 = @lv_date_type_for_hire_date or
          dar23 = @lv_date_type_for_hire_date or dar24 = @lv_date_type_for_hire_date ).

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_latest_hire_date into ls_latest_hire_date.
      loop at lt_it41 into data(ls_it41) where
        pernr = ls_latest_hire_date-pernr.
        clear: lv_it41_hire_date.

        zcl_m99_pa_pay_b4_hire=>is_date_type_found(
          exporting
            iv_date_type = lv_date_type_for_hire_date
            is_it41      = ls_it41
          importing
*            ev_found     =
            ev_date      = lv_it41_hire_date ).

        if lv_it41_hire_date is not initial and
          ls_latest_hire_date-hire_date < lv_it41_hire_date.

          read table rt_result transporting no fields with key
            par_type = if_pyd_cont_types=>gcs_par_type-pernr
            id = ls_latest_hire_date-pernr.
          if sy-subrc <> 0.
            ls_result-id = ls_latest_hire_date-pernr.
            insert ls_result into table rt_result.
          endif.

          read table mt_output transporting no fields with table key
            pernr = ls_latest_hire_date-pernr
            hire_date = ls_latest_hire_date-hire_date
            begda_41 = ls_it41-begda
            endda_41 = ls_it41-endda.
          if sy-subrc <> 0.
            clear: ls_output.
            ls_output-pernr = ls_latest_hire_date-pernr.
            ls_output-hire_date = ls_latest_hire_date-hire_date.
            ls_output-abkrs_on_hire_date = ls_latest_hire_date-abkrs.
            ls_output-begda_41 = ls_it41-begda.
            ls_output-endda_41 = ls_it41-endda.
            ls_output-hire_date_41 = lv_it41_hire_date.
            insert ls_output into table mt_output.
          endif.
        endif.
      endloop.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Display error message
    data:
      ls_err_ov         type ty_s_err_ov,
      ls_sfo            type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr          type p_pernr,
      lv_text           type text120,
      lv_value          type char060,
      ls_output         like line of mt_output,
      ls_output_compact type ty_output_compact,
      lt_sfo_tab_temp   like ls_err_ov-sfo_tab,
      lv_value_string   type string.


    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          "move SFO_TAB records to temporary table
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.
          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            clear: ls_output_compact, lv_value, lv_value_string, lv_text.
            if ls_sfo_tab_temp-itemid = mc_itemid_pay_b4_hire.
              lv_value = ls_sfo_tab_temp-value.
              me->copy_structure_to_other(
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ls_output_compact ).

              write ls_output_compact-hire_date to lv_text.
              message i050 with
                ls_output_compact-abkrs_on_hire_date
                ls_output_compact-hire_date_41 into lv_value_string.
              me->add_record_to_sfo_tab(
                exporting
                  iv_itemid                   = mc_itemid_pay_b4_hire
                  iv_text                     = |{ lv_text }|
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab ).
            else.
              insert ls_sfo_tab_temp into table ls_err_ov-sfo_tab.
            endif.
          endloop.
          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_output into ls_output where pernr = lv_pernr.
            clear: ls_output_compact, lv_value.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_pay_b4_hire.
            ls_output_compact = corresponding #( ls_output ).
            me->copy_structure_to_other(
              exporting
                p_struct1 = ls_output_compact
              changing
                p_struct2 = lv_value ).
            ls_sfo-value = lv_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_it41_date_type_for_hire_dt.

* Get IT41 date type for hire date

    constants: lc_feature_entry               type t549d-namen value 'ENTRY',
               lc_field_name_for_data_type(5) type c value 'DATYP',
               lc_data_type_default           type pa0041-dar01 value '10'.
    types: begin of field,
             name(5)   type c,
             blank(1)  type c,
             value(32) type c,
           end of field.
    data: ls_for_feature type pme36,
          lt_return      type string_t,
          lt_field       type standard table of field,
          ls_field       like line of lt_field.

    try.
        ls_for_feature-molga = iv_molga.

        cl_hrpa_feature=>get_table(
          exporting
            feature       = lc_feature_entry
            struc_content = ls_for_feature
          importing
            return_table  = lt_return ).

        loop at lt_return into data(ls_return).
          ls_field = ls_return.
          if ls_field-name = lc_field_name_for_data_type.
            rv_date_type = ls_field-value.
          endif.
        endloop.
      catch cx_hrpa_violated_assertion .
        rv_date_type = lc_data_type_default.
    endtry.
  endmethod.


  method is_date_type_found.
* Find IT41 date in IT41 structure
    data: lv_field_no(2)        type n,
          lv_field_dt_type_name type string,
          lv_field_val_name     type string,
          ls_output             like line of mt_output.
    field-symbols: <fs_dt_type> like is_it41-dar01,
                   <fs_val>     like is_it41-dat01.
    clear: ev_found, ev_date.
    check iv_date_type is not initial.

    do if_hrpa_constants=>gc_number_of_date_types_it0041 times.
      lv_field_no = sy-index.
      concatenate mc_field_prefix_date_type lv_field_no into lv_field_dt_type_name.
      concatenate mc_field_prefix_date lv_field_no into lv_field_val_name.
      assign component lv_field_dt_type_name of structure is_it41 to <fs_dt_type>.
      if <fs_dt_type> is assigned.
        if <fs_dt_type> is initial.
          unassign <fs_dt_type>.
          exit.
        elseif <fs_dt_type> = iv_date_type.
          assign component lv_field_val_name of structure is_it41 to <fs_val>.
          if <fs_val> is assigned.
            ev_found = abap_true.
            ev_date = <fs_val>.
            unassign <fs_val>.
          endif.
          exit.
        endif.
      endif.
    enddo.
  endmethod.
ENDCLASS.
