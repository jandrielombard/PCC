class zcl_m43_pa_sl_sdr_expire definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.
  protected section.

    types:
      begin of ty_it0313,
        pernr    type persno,
        stlpc    type pa0313-stlpc,
        sdate    type pa0313-sdate,
        errty(1) type c.
    types: end of ty_it0313 .
    types: tty_it0313 type table of ty_it0313.

    constants mc_itemid_sdrexpire type pyd_s_rdsfo_ext-itemid value 'SDREXPIRE' ##NO_TEXT.
    constants mc_errty_nodate type c value 'A' ##NO_TEXT.
    constants mc_errty_dateincp type c value 'B' ##NO_TEXT.
    data mt_it0313 type tty_it0313.
    data ms_it0313 type ty_it0313.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_SL_SDR_EXPIRE IMPLEMENTATION.


  method check.
* Student Loan SDR expiry Date
    data: ls_result like line of rt_result.
    data: lt_all_it0313 type table of ty_it0313.
    data: lt_err_it0313 type table of ty_it0313.
    data: ls_it0313 type ty_it0313.

* All relevant employees with IT 0313 records
    select it0313~pernr, it0313~stlpc, it0313~sdate
      into corresponding fields of table @lt_all_it0313
      from pa0313 as it0313
      where it0313~pernr in @it_pernr_so
         and it0313~sprps = @if_hrpa_read_infotype=>unlocked
         and it0313~endda >= @mv_begda
         and it0313~begda <= @mv_endda
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0313~pernr
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
                         and it0001~kostl in @mt_kostl )
    order by it0313~pernr.

* Check if P0313-STLPC is not 0 or blank.
* Check if P0313-SDATE is blank.
* Raise an alert for missing Student Loan SDR expiry date (Reason A)
* Check if P0313-SDATE is in current pay period
* Raise an alert to update the SDR date and expiry date (Reason B)
    loop at lt_all_it0313 into ls_it0313.
      "  Check if P0313-SDATE is blank.
      if ls_it0313-stlpc is not initial and ls_it0313-sdate is initial.
        move mc_errty_nodate to ls_it0313-errty.
        append ls_it0313 to lt_err_it0313.

        " Check if P0313-SDATE is in current pay period
      elseif ls_it0313-stlpc is not initial and ls_it0313-sdate between mv_begda and mv_endda.
        move mc_errty_dateincp to ls_it0313-errty.
        append ls_it0313 to lt_err_it0313.
      endif.
    endloop.

* Report all Error Employees
    mt_it0313[] = lt_err_it0313[].

    sort lt_err_it0313.
    delete adjacent duplicates from lt_err_it0313 comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_err_it0313 into ls_it0313.
      ls_result-id = ls_it0313-pernr.
      append ls_result to rt_result.
    endloop.

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
          clear lv_value_string.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_text = |{ text-001 } { ls_sfo_tab-value }|.
            case ls_sfo_tab-value.
              when mc_errty_nodate.
                lv_value_string = text-002.
              when mc_errty_dateincp.
                lv_value_string = text-003.
            endcase.

            me->add_record_to_sfo_tab(
                exporting
                  iv_itemid                   = mc_itemid_sdrexpire
                  iv_text                     = lv_text
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab ).
          endloop.

          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

        "Execute mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

* Populate SFO tab for Table Save
          loop at mt_it0313 into ms_it0313
            where pernr = lv_pernr.

            lv_value_string = ms_it0313-errty.
            me->add_record_to_sfo_tab(
             exporting
               iv_itemid                   = mc_itemid_sdrexpire
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
