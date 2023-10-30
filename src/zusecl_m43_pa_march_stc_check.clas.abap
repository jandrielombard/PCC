class ZUSECL_M43_PA_MARCH_STC_CHECK definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_IT0313 type PYD_S_RDSFO_EXT-ITEMID value 'IT0007' ##NO_TEXT.
  constants MC_TAXCD_STC type PNZ_TAXCD value 'STC' ##NO_TEXT.
protected section.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZUSECL_M43_PA_MARCH_STC_CHECK IMPLEMENTATION.


  method CHECK.
* check Upcoming Special Tax Rate expiry (March) on IT0313
    data: lt_it     type sorted table of pernr_d with unique key table_line,
          ls_result like line of rt_result.
    data: lv_pabrp type pabrp.

* Rule processed only if Payroll period is 12 for monthly (payroll area N3)
* and greater than or equal to 49 for weekly employees (payroll areas N1 and N0)
    lv_pabrp = mv_payroll_period+4(2).
    if ( mv_endda - mv_begda ) > 7.  " Monthly
      check lv_pabrp eq 12.
    else.                            " Weekly
      check lv_pabrp ge 49.
    endif.

* Read Employee list
    select distinct it0313~pernr
      into table @lt_it
      from pa0313 as it0313
      where it0313~pernr in @it_pernr_so        and
            it0313~sprps = @if_hrpa_read_infotype=>unlocked and
            it0313~begda <= @mv_endda           and
            it0313~endda >= @mv_begda           and
            it0313~taxcd eq @mc_taxcd_stc       and
            exists ( select 1
              from pa0000 as it00
                inner join pa0001 as it01 on
                  it00~pernr = it01~pernr
              where it00~pernr = it0313~pernr       and
                    it00~begda <= @mv_endda           and
                    it00~endda >= @mv_begda           and
                    it01~begda <= @mv_endda           and
                    it01~endda >= @mv_begda           and
                    it00~stat2 in @mt_stat2           and
                    it00~sprps = @if_hrpa_read_infotype=>unlocked and
                    it01~sprps = @if_hrpa_read_infotype=>unlocked and
                    it01~abkrs in @mt_payroll_areas   and
                    it01~bukrs in @mt_bukrs           and
                    it01~werks in @mt_werks           and
                    it01~btrtl in @mt_btrtl           and
                    it01~persg in @mt_persg           and
                    it01~persk in @mt_persk           and
                    it01~kostl in @mt_kostl
            )
    order by it0313~pernr.

    if lt_it is not initial.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_it into data(ls_it).
        ls_result-id = ls_it.
        insert ls_result into table rt_result.
      endloop.
    endif.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Display error message
    data:
      ls_err_ov       type ty_s_err_ov,
      lv_value_string type string.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        message i071 into lv_value_string.

        loop at ct_err_ov into ls_err_ov.
          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_it0313
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
