class ZCL_M43_PA_IRD_MISSING definition
  public
  inheriting from ZCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_IRD type PYD_S_RDSFO_EXT-ITEMID value 'IRDMISSING' ##NO_TEXT.
  PROTECTED SECTION.

    METHODS check
        REDEFINITION .
    METHODS err_ov_get_list
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_M43_PA_IRD_MISSING IMPLEMENTATION.


  method check.
* To identify team members who have a missing IRD number, P0309-IRDNR
* Team members with tax code P0313-TAXCD 'ND' are excluded
    data: lt_pernr type table of ty_pernr.
    data: ls_result like line of rt_result.
    types: begin of ty_it0309,
             pernr type pernr_d,
             begda type p0309-begda,
             irdnr type p0309-irdnr,
             taxcd type p0313-taxcd.
    types: end of ty_it0309.
    data: lt_all_it0309 type table of ty_it0309.
    data: lt_err_it0309 type table of ty_it0309.
    data: ls_it0309 type ty_it0309.

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

* All relevant employees with it0309 details
    select it0309~pernr, it0309~irdnr, it0313~taxcd
        into corresponding fields of table @lt_all_it0309
        from pa0309 as it0309
       inner join pa0313 as it0313
         on it0313~pernr = it0309~pernr
        and it0313~begda <= @mv_endda
        and it0313~endda >= @mv_endda
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
                         and it0001~kostl in @mt_kostl )
      order by it0309~pernr.

* Check if P0309 exists. If it doesn't exist, check the tax code P0313-TAXCD is not 'ND'
* Check if P0309-IRDNR is blank. If blank, check the tax code P0313-TAXCD is not 'ND'
* Raise an alert for missing IRD number for employee
    loop at lt_all_it0309 into ls_it0309.
* Clear from the Employee main list
      if ls_it0309-irdnr is not initial and ls_it0309-taxcd <> 'ND'.
        delete lt_pernr where dct_pernr = ls_it0309-pernr.
      endif.
    endloop.

* Report all Employees with missing IRD Numuber
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
    data: ls_err_ov       type ty_s_err_ov,
          lv_value_string type string.

* Overview messages
    case iv_access_mode.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        loop at ct_err_ov into ls_err_ov.
          lv_value_string = text-001.
          me->add_record_to_sfo_tab(
              exporting
                iv_itemid                   = mc_itemid_ird
                iv_text                     = |{ text-002 }|
                iv_value                    = lv_value_string
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab ).

          lv_value_string = text-003.
          me->add_record_to_sfo_tab(
              exporting
                iv_itemid                   = mc_itemid_ird
                iv_text                     = ''
                iv_value                    = lv_value_string
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab ).

          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

        "Execute mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.
ENDCLASS.
