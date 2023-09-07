class zcl_m43_pa_kiwi_erpct definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.

  protected section.
    types:
      begin of ty_it0310,
        pernr type p0310-pernr,
        fcode type p0310-fcode,
        erpct type p0310-erpct.
    types: end of ty_it0310 .
    constants mc_itemid_kiwierpct type pyd_s_rdsfo_ext-itemid value 'KIWIERPCT' ##NO_TEXT.
    constants mc_std_kiwi_erpct type p0310-erpct value '3.00' ##NO_TEXT.
    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_KIWI_ERPCT IMPLEMENTATION.


  method check.
* KiwiSaver ER % is not equal to 3%
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
    select it0310~pernr, it0310~fcode, it0310~erpct
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

* Check if P0310-ERPCT is not equall to 3%.
* also Do not raise alert if P0310-ERPCT is 0%
* Raise an alert for Kiwisaver ER contribution
    loop at lt_all_it0310 into ls_it0310.
      check not ls_it0310-erpct is initial.

      " Check for percentage
      if ls_it0310-erpct <> mc_std_kiwi_erpct.
        append ls_it0310 to lt_err_it0310.
      endif.
    endloop.

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
    data:
      ls_err_ov       type ty_s_err_ov,
      lv_value_string type string.

    case iv_access_mode.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          move text-002 to lv_value_string.

          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_kiwierpct
              iv_text                     = |{ text-001 }|
              iv_value                    = lv_value_string
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab ).
          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

      when others.
        raise exception type cx_pyd_fnd.
    endcase.

  endmethod.
ENDCLASS.
