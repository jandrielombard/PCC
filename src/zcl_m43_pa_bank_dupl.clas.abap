class zcl_m43_pa_bank_dupl definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.


  protected section.

    types: begin of ty_it0009,
             pernr  type p0009-pernr,
             begda  type p0009-begda,
             bankl  type p0009-bankl,
             bankn  type p0009-bankn,
             others type string,
           end of ty_it0009.
    types: tty_it0009 type table of ty_it0009.

    constants mc_itemid_dupl type pyd_s_rdsfo_ext-itemid value 'BANKDUPL' ##NO_TEXT.
    data: mt_it0009 type tty_it0009.
    data: ms_it0009 type ty_it0009.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_BANK_DUPL IMPLEMENTATION.


  method check.
* Bank Details: Check for Duplicate Bank Account
    data: lt_all_it0009 type tty_it0009.
    data: lt_dup_it0009 type tty_it0009.
    data: ls_it0009 type ty_it0009.
    data: ls_result like line of rt_result.
    data: lv_aedtm type aedtm.

* Set data read period
* Weekly payrolls: change on date = or > PCC period start date + 1 day
* Monthly payroll: changed on date > 14th of previous month
* (to allow for payroll being run ~14th of month)
    if ( mv_endda - mv_begda ) > 7.  " Monthly
      call function 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
        exporting
          i_date_old = mv_begda
        importing
          e_date_new = lv_aedtm.
      lv_aedtm+6(2) = '14'.
    else.                            " Weekly
      lv_aedtm = mv_begda + 1.
    endif.

* All relevant employees who updated Bank Account
    select it0009~pernr, it0009~begda, it0009~bankl, it0009~bankn
       into table @lt_all_it0009
        from pa0009 as it0009
        where it0009~pernr in @it_pernr_so
          and it0009~aedtm >= @lv_aedtm
          and it0009~sprps = @if_hrpa_read_infotype=>unlocked
          " Entity restiction
          and exists ( select 1
                        from pa0000 as it0000
                       inner join pa0001 as it0001 on it0000~pernr = it0001~pernr
                       where it0000~pernr = it0009~pernr
                         and it0000~begda <= @mv_endda
                         and it0000~endda >= @mv_begda
                         and it0000~stat2 in @mt_stat2
                         and it0000~sprps = @if_hrpa_read_infotype=>unlocked
                         and it0001~begda <= @mv_endda
                         and it0001~endda >= @mv_begda
                         and it0001~sprps = @if_hrpa_read_infotype=>unlocked
                         and it0001~abkrs in @mt_payroll_areas
                         and it0001~bukrs in @mt_bukrs
                         and it0001~werks in @mt_werks
                         and it0001~btrtl in @mt_btrtl
                         and it0001~persg in @mt_persg
                         and it0001~persk in @mt_persk
                         and it0001~kostl in @mt_kostl ).

* All relevant employees with GPPL Payment Records
    if not lt_all_it0009[] is initial.
      sort lt_all_it0009 by pernr begda bankl bankn.
      delete adjacent duplicates from lt_all_it0009 comparing pernr bankl bankn.
      select it0009_2~pernr, it0009_2~bankl, it0009_2~bankl, it0009_2~bankn
        into corresponding fields of table @lt_dup_it0009
        from pa0009 as it0009_2
        for all entries in @lt_all_it0009
       where it0009_2~pernr <> @lt_all_it0009-pernr
         and it0009_2~sprps = @if_hrpa_read_infotype=>unlocked
         and it0009_2~bankl = @lt_all_it0009-bankl
         and it0009_2~bankn = @lt_all_it0009-bankn.
    endif.
*
    sort lt_all_it0009 by pernr begda bankl bankn.
    loop at lt_all_it0009 into ls_it0009.
      loop at lt_dup_it0009 into data(ls_dup_it0009)
        where pernr <> ls_it0009-pernr
          and bankl = ls_it0009-bankl
          and bankn = ls_it0009-bankn.

        if ls_it0009-others is initial.
          ls_it0009-others = ls_dup_it0009-pernr.
        else.
          concatenate ls_it0009-others ls_dup_it0009-pernr
                 into ls_it0009-others separated by ','.
        endif.
      endloop.

* Collect All records for Reporting
      if not ls_it0009-others is initial.
        move-corresponding ls_it0009 to ms_it0009.
        append ms_it0009 to mt_it0009.
      endif.
    endloop.

* Build Results table
    if not mt_it0009 is initial.
      lt_all_it0009[] = mt_it0009[].
      sort lt_all_it0009 by pernr.
      delete adjacent duplicates from lt_all_it0009 comparing pernr.

      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_all_it0009 into ls_it0009.
        ls_result-id = ls_it0009-pernr.
        insert ls_result into table rt_result.
      endloop.
    endif.
    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Display error message
    data: ls_err_ov       type ty_s_err_ov,
          lv_value_string type string.
    data: ls_sfo   type cl_pyd_rd_dto_sfo=>ty_s_rd,
          lv_modif type abap_bool,
          lv_pernr type p_pernr,
          lv_value type pyd_item_value.

    data: lv_text type pyd_name.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.

* Overview messages
    case iv_access_mode.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.
        loop at ct_err_ov into ls_err_ov.
          "move SFO_TAB records to temporary table
          lt_sfo_tab = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.
* Reason
          lv_value_string = text-001.
          me->add_record_to_sfo_tab(
              exporting
                iv_itemid                   = mc_itemid_dupl
                iv_text                     = |{ text-002 }|
                iv_value                    = lv_value_string
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab ).
* Employees
          clear lv_value_string.
          loop at lt_sfo_tab into ls_sfo_tab.
            if lv_value_string is initial.
              lv_value_string = ls_sfo_tab-value.
            else.
              concatenate lv_value_string ls_sfo_tab-value
                     into lv_value_string.
            endif.
          endloop.
          me->add_record_to_sfo_tab(
              exporting
                iv_itemid                   = mc_itemid_dupl
                iv_text                     = |{ text-003 }|
                iv_value                    = lv_value_string
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab ).

          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

        "Execute mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

* Populate SFO tab for Table Save
          loop at mt_it0009 into ms_it0009
            where pernr = lv_pernr.

            lv_value_string = ms_it0009-others.
            me->add_record_to_sfo_tab(
             exporting
               iv_itemid                   = mc_itemid_dupl
               iv_text                     = ''
               iv_value                    = lv_value_string
               iv_text_for_1st_record_only = abap_true
             changing
               ct_sfo_tab                  = ls_err_ov-sfo_tab ).
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.
      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.
ENDCLASS.
