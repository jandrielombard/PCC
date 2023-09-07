class zcl_m43_pa_kiwi_status definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.
  protected section.

    types:
      begin of ty_it0310,
        pernr    type p0310-pernr,
        begda    type p0310-begda,
        fcode    type p0310-fcode,
        joind    type p0310-joind,
        cnhol    type p0310-cnhol,
        nekse    type p0310-nekse,
        rescd    type p0310-rescd,
        netex    type p0310-netex,
        errty(1) type c.
    types: end of ty_it0310 .
    types: tty_it0310 type table of ty_it0310.

    constants mc_itemid_kiwistatus type pyd_s_rdsfo_ext-itemid value 'KIWISTATUS' ##NO_TEXT.
    constants mc_rescd_09 type p0310-rescd value '09' ##NO_TEXT.
    constants mc_errty_cnhol type c value 'A' ##NO_TEXT.
    constants mc_errty_rescd type c value 'B' ##NO_TEXT.
    constants mc_errty_netex type c value 'C' ##NO_TEXT.
    data mt_it0310 type tty_it0310.
    data ms_it0310 type ty_it0310.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_KIWI_STATUS IMPLEMENTATION.


  method check.
* KiwiSaver Status
    data: lt_all_it0310 type table of ty_it0310,
          lt_err_it0310 type table of ty_it0310,
          ls_it0310     type ty_it0310,
          ls_result     like line of rt_result.
    data: lv_chg_aedtm type aedtm.

* Set data read period
* Weekly payrolls: change on date = or > PCC period start date + 1 day
* Monthly payroll: changed on date > 14th of previous month
* (to allow for payroll being run ~14th of month)
    if ( mv_endda - mv_begda ) > 7.  " Monthly
      call function 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
        exporting
          i_date_old = mv_begda
        importing
          e_date_new = lv_chg_aedtm.
      lv_chg_aedtm+6(2) = '14'.
    else.                            " Weekly
      lv_chg_aedtm = mv_begda + 1.
    endif.

*All relevant employees with IT 0310 Kiwisaver Fund record
    select it0310~pernr, it0310~begda, it0310~fcode,  it0310~joind,
           it0310~cnhol, it0310~nekse, it0310~rescd, it0310~netex
      into corresponding fields of table @lt_all_it0310
      from pa0310 as it0310
      where it0310~pernr in @it_pernr_so
         and it0310~subty in @mt_subty
         and it0310~sprps = @if_hrpa_read_infotype=>unlocked
         and it0310~aedtm >= @lv_chg_aedtm
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0310~pernr
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
    order by it0310~pernr.

* Validate KiwiSaver Data
    loop at lt_all_it0310 into ls_it0310.
      " if P0310-JOIND is before P0310-BEGDA. If before then P0310-CNHOL must have 'X'
      " If P0310-CNHOL is Blank, Raise an Alert
      if ( ls_it0310-joind lt ls_it0310-begda ) and ( ls_it0310-cnhol eq abap_false ).
        move mc_errty_cnhol to ls_it0310-errty.
        append ls_it0310 to lt_err_it0310.
        " If P0310-NEKSE = 'X', then P0310-RESCD must have a value.
        " If P0310-RESCD is Blank, Raise an Alert
      elseif ( ls_it0310-nekse eq abap_true ) and ( ls_it0310-rescd is initial ).
        move mc_errty_rescd to ls_it0310-errty.
        append ls_it0310 to lt_err_it0310.
        " If P0310-RESCD = '09' then P0310-NETEX must have a value
        " If P0310-NETEX is blank, Raise an alert
      elseif ( ls_it0310-rescd eq mc_rescd_09 ) and ( ls_it0310-netex is initial ).
        move mc_errty_netex to ls_it0310-errty.
        append ls_it0310 to lt_err_it0310.
      endif.
    endloop.

    mt_it0310[] = lt_err_it0310[].
* Report all Error Employees
    sort lt_err_it0310.
    delete adjacent duplicates from lt_err_it0310 comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_err_it0310 into ls_it0310.
      ls_result-id = ls_it0310-pernr.
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
              when mc_errty_cnhol.
                lv_value_string = text-002.
              when mc_errty_rescd.
                lv_value_string = text-003.
              when mc_errty_netex.
                lv_value_string = text-004.
            endcase.

            me->add_record_to_sfo_tab(
                exporting
                  iv_itemid                   = mc_itemid_kiwistatus
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
          loop at mt_it0310 into ms_it0310
            where pernr = lv_pernr.

            lv_value_string = ms_it0310-errty.
            me->add_record_to_sfo_tab(
             exporting
               iv_itemid                   = mc_itemid_kiwistatus
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
