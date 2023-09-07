class ZCL_M43_PA_KIWI_CASUAL definition
  public
  inheriting from ZCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
protected section.

  types:
    begin of ty_it0310,
      pernr type p0310-pernr,
      begda type p0310-begda,
      fcode type p0310-fcode,
      joind type p0310-joind,
      persg type p0001-persg.
  types: end of ty_it0310 .

  constants mc_itemid_kiwicasual type pyd_s_rdsfo_ext-itemid value 'KIWICASUAL' ##NO_TEXT.
  constants mc_persg_casual type p0001-persg value 'C' ##NO_TEXT.

  methods check
      redefinition .
  methods err_ov_get_list
      redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_KIWI_CASUAL IMPLEMENTATION.


  method check.
* KiwiSaver: Employee is auto-enrolled as a casual employee
    data: lt_all_it0310 type table of ty_it0310,
          lt_err_it0310 type table of ty_it0310,
          ls_it0310     type ty_it0310,
          ls_result     like line of rt_result.
    data: lv_chg_aedtm type aedtm.
    data: lv_years type  p.

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

*All employees with changed Kiwisaver record in the selection period
    select it0310~pernr, it0310~begda, it0310~fcode,
           it0310~joind, it0001~persg
      into corresponding fields of table @lt_all_it0310
      from pa0310 as it0310
        inner join pa0001 as it0001
         on it0001~pernr = it0310~pernr
        and it0001~begda <= @mv_endda
        and it0001~endda >= @mv_begda
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
      " Check Employee Sub Group = C (P0001-PERSG)
      check ls_it0310-persg eq mc_persg_casual.

      " Check if P0310-JOIND has a date
      " If P0310-JOIND has a date, Raise an Alert
      if not ls_it0310-joind is initial.
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
              iv_itemid                   = mc_itemid_kiwicasual
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
