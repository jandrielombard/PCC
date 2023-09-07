class ZCL_M43_PA_KIWI_MISSING definition
  public
  inheriting from ZCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
protected section.

  constants MC_ITEMID_NZKIWI type PYD_S_RDSFO_EXT-ITEMID value 'NZKIWI' ##NO_TEXT.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_KIWI_MISSING IMPLEMENTATION.


  method check.
* Missing Kiwisaver Fund
    data: lt_pernr type table of ty_pernr.
    data: ls_result like line of rt_result.

    types: begin of ty_it0310,
             pernr type pernr_d,
             fcode type pa0310-fcode.
    types: end of ty_it0310.
    data: lt_all_it0310 type table of ty_it0310.
    data: lt_err_it0310 type table of ty_it0310.
    data: ls_it0310 type ty_it0310.

* Select All relevant employees for the Payroll Area
    select distinct it0000~pernr as dct_pernr
      into corresponding fields of table @lt_pernr
       from pa0000 as it0000
      inner join pa0001 as it0001 on it0000~pernr = it0001~pernr
      where it0000~pernr in @it_pernr_so
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
        and it0001~kostl in @mt_kostl.

*All relevant employees with IT 0310 Kiwisaver Fund record
    select it0310~pernr, it0310~fcode
      into corresponding fields of table @lt_all_it0310
      from pa0310 as it0310
      where it0310~pernr in @it_pernr_so
         and it0310~subty in @mt_subty
         and it0310~sprps = @if_hrpa_read_infotype=>unlocked
         and it0310~endda >= @mv_begda
         and it0310~begda <= @mv_endda
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

* Identify Employees with out Kiwisaver Fund
    loop at lt_all_it0310 into ls_it0310.
* Clear from the Employee main list
      delete lt_pernr where dct_pernr = ls_it0310-pernr.
    endloop.

* Report all Employees with missing Kiwisaver Fund record
    if not lt_pernr is initial.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_pernr into data(ls_pernr).
        ls_result-id = ls_pernr-dct_pernr.
        insert ls_result into table rt_result.
      endloop.
    endif.

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
          lv_value_string = text-002.
          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_nzkiwi
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
