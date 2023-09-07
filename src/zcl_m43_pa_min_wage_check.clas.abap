class zcl_m43_pa_min_wage_check definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.
  protected section.

    constants mc_itemid_minwage type pyd_s_rdsfo_ext-itemid value 'MINWAGE' ##NO_TEXT.
    constants mc_konst_zmin type t511p-konst value 'ZMIN' ##NO_TEXT.
    constants mc_hourly_rate_wt type lgart value '1108' ##NO_TEXT.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M43_PA_MIN_WAGE_CHECK IMPLEMENTATION.


  method check.
* Minimum wage rate check
    data: lt_pernr type table of ty_pernr.
    data: lt_it0008 type table of p0008,
          ls_it0008 type p0008,
          ls_result like line of rt_result.
    data: lv_chg_aedtm type aedtm.
    data: lt_it0008_lgart type zcl_m99_pcc_chk_utilities=>tty_it0008_lgart.
    data: ls_it0008_lgart type zcl_m99_pcc_chk_utilities=>ty_it0008_lgart.
    data: lv_wage_amt    type pad_amt7s,
          lv_zmin_amount type t511p-betrg.

* Read Minimum Wage amount
    select single betrg into lv_zmin_amount
      from t511p
     where molga = mc_molga_nz and konst = mc_konst_zmin
       and endda >= mv_endda and begda <= mv_endda.

* All IT 0008 records for the selection
    select it08~pernr, it08~subty, it08~endda, it08~begda, it08~waers,
           it08~lga01, it08~bet01,  it08~lga02, it08~bet02,
           it08~lga03, it08~bet03,  it08~lga04, it08~bet04,
           it08~lga05, it08~bet05,  it08~lga06, it08~bet06,
           it08~lga07, it08~bet07,  it08~lga08, it08~bet08,
           it08~lga09, it08~bet09,  it08~lga10, it08~bet10,
           it08~lga11, it08~bet11,  it08~lga12, it08~bet12,
           it08~lga13, it08~bet13,  it08~lga14, it08~bet14,
           it08~lga15, it08~bet15,  it08~lga16, it08~bet16,
           it08~lga17, it08~bet17,  it08~lga18, it08~bet18,
           it08~lga19, it08~bet19,  it08~lga20, it08~bet20
      into corresponding fields of table @lt_it0008
      from pa0008 as it08
      where it08~pernr in @it_pernr_so
         and it08~subty in @mt_subty
         and it08~sprps = @if_hrpa_read_infotype=>unlocked
         and it08~begda <= @mv_endda
         and it08~endda >= @mv_begda
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it08~pernr
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
    order by it08~pernr.

* Check if amount of wage type 1108 is less than constant ZMIN in Table T511P.
* Raise an alert when hourly rate less than minimum wage
    loop at lt_it0008 into ls_it0008.
      " Read IT0008 Wage Types
      call method zcl_m99_pcc_chk_utilities=>read_it0008_wagetype_dtls
        exporting
          is_p0008        = ls_it0008
        importing
          et_it0008_lgart = lt_it0008_lgart.

      delete lt_it0008_lgart where lgart <> mc_hourly_rate_wt.
      read table lt_it0008_lgart into ls_it0008_lgart index 1.
      if sy-subrc eq 0.
        " Check if amount less than min wage
        if ls_it0008_lgart-betrg < lv_zmin_amount.
          append ls_it0008-pernr to lt_pernr.
        endif.
      else.
        append ls_it0008-pernr to lt_pernr.
      endif.
    endloop.

* Report all Error Employees
    sort lt_pernr.
    delete adjacent duplicates from lt_pernr comparing dct_pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_pernr into data(ls_pernr).
      ls_result-id = ls_pernr-dct_pernr.
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
              iv_itemid                   = mc_itemid_minwage
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
