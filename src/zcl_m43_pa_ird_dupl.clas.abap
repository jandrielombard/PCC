class zcl_m43_pa_ird_dupl definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.


  protected section.
    types: begin of ty_it0309,
             pernr  type p0309-pernr,
             irdnr  type p0309-irdnr,
             others type string,
           end of ty_it0309.
    types: tty_it0309 type table of ty_it0309.

    constants mc_itemid_duplird type pyd_s_rdsfo_ext-itemid value 'DUPLIRD' ##NO_TEXT.
    data: mt_it0309 type tty_it0309.
    data: ms_it0309 type ty_it0309.
    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_IRD_DUPL IMPLEMENTATION.


  method check.
* Duplicate IRD
    data: lt_all_it0309 type tty_it0309.
    data: lt_dup_it0309 type tty_it0309.
    data: ls_it0309 type ty_it0309.
    data: ls_result like line of rt_result.
    data: lv_aedtm type aedtm.

* All relevant employees who provided Duplicate Bank Account
    select distinct it0309~pernr, it0309~irdnr
        into corresponding fields of table @lt_all_it0309
        from pa0309 as it0309
        where it0309~pernr in @it_pernr_so
          and it0309~begda <= @mv_endda
          and it0309~endda >= @mv_begda
          and it0309~sprps = @if_hrpa_read_infotype=>unlocked
          " Entity restiction
          and exists ( select 1
                        from pa0000 as it0000
                       inner join pa0001 as it0001 on it0000~pernr = it0001~pernr
                       where it0000~pernr = it0309~pernr
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

* all relevant employees with gppl payment records
    if not lt_all_it0309[] is initial.
      sort lt_all_it0309 by pernr irdnr.
      delete adjacent duplicates from lt_all_it0309 comparing pernr irdnr.
      select it0309_2~pernr, it0309_2~irdnr
        into corresponding fields of table @lt_dup_it0309
        from pa0309 as it0309_2
        for all entries in @lt_all_it0309
       where it0309_2~pernr <> @lt_all_it0309-pernr
         and it0309_2~sprps = @if_hrpa_read_infotype=>unlocked
         and it0309_2~irdnr = @lt_all_it0309-irdnr.
    endif.
*
    sort lt_all_it0309 by pernr irdnr.
    loop at lt_all_it0309 into ls_it0309.
      loop at lt_dup_it0309 into data(ls_dup_it0309)
        where pernr <> ls_it0309-pernr
          and irdnr = ls_it0309-irdnr.

        if ls_it0309-others is initial.
          ls_it0309-others = ls_dup_it0309-pernr.
        else.
          concatenate ls_it0309-others ls_dup_it0309-pernr
                 into ls_it0309-others separated by ','.
        endif.
      endloop.

* Collect All records for Reporting
      if not ls_it0309-others is initial.
        move-corresponding ls_it0309 to ms_it0309.
        append ms_it0309 to mt_it0309.
      endif.
    endloop.

* Build Results table
    if not mt_it0309 is initial.
      lt_all_it0309[] = mt_it0309[].
      sort lt_all_it0309 by pernr.
      delete adjacent duplicates from lt_all_it0309 comparing pernr.

      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_all_it0309 into ls_it0309.
        ls_result-id = ls_it0309-pernr.
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
                iv_itemid                   = mc_itemid_duplird
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
                iv_itemid                   = mc_itemid_duplird
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
          loop at mt_it0309 into ms_it0309
            where pernr = lv_pernr.
            lv_value_string = ms_it0309-others.

            me->add_record_to_sfo_tab(
             exporting
               iv_itemid                   = mc_itemid_duplird
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
