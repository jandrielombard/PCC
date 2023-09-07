class ZCL_M43_PA_TAX_ND definition
  public
  inheriting from ZCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
protected section.

  types:
    begin of ty_it0227,
             pernr type persno,
             taxfn type pa0227-taxfn.
    types: end of ty_it0227 .

  constants MC_ITEMID_NZTAXND type PYD_S_RDSFO_EXT-ITEMID value 'NZTAXND' ##NO_TEXT.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_TAX_ND IMPLEMENTATION.


  method check.
* Missing Tax Code
    data: lt_pernr type table of ty_pernr.
    data: ls_result like line of rt_result.

    types: begin of ty_it0313,
             pernr type pernr_d,
             taxcd type pa0313-taxcd.
    types: end of ty_it0313.
    data: lt_all_it0313 type table of ty_it0313.
    data: lt_err_it0313 type table of ty_it0313.
    data: ls_it0313 type ty_it0313.

*All relevant employees with IT 0313 records
    select it0313~pernr, it0313~taxcd
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

* Identify Employees with tax code ND
    loop at lt_all_it0313 into ls_it0313.
      " Check for blanck tax code
      if ls_it0313-taxcd eq 'ND'.
        append ls_it0313 to lt_err_it0313.
      endif.
    endloop.

* Add all Employees with ND tax code
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
    data:
      ls_err_ov       type ty_s_err_ov,
      lv_value_string type string.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        lv_value_string = text-002.
        loop at ct_err_ov into ls_err_ov.
          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_nztaxnd
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
