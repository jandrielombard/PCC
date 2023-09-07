class zcl_m99_pa_valid_tfn definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.

  protected section.

    types: begin of ty_it0227,
             pernr type persno,
             taxfn type pa0227-taxfn.
    types: end of ty_it0227.
    constants mc_itemid_tfn type pyd_s_rdsfo_ext-itemid value 'INVALIDTFN' ##NO_TEXT.
    constants mc_28_days type i value '28' ##NO_TEXT.

    data: lt_pernr type table of ty_pernr.
    data: lt_chg_it0227 type table of ty_it0227.
    data: lt_err_it0227 type table of ty_it0227.
    data: ls_it0227 type ty_it0227.

    data mt_filter_zterf type /iwbep/t_cod_select_options .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_VALID_TFN IMPLEMENTATION.


  method check.
* check time management status on IT07
    data: lv_validation_date type datum.
    data:  ls_result like line of rt_result.
    types: begin of ty_it0227,
             pernr type persno,
             aedtm type aedtm,
             taxfn type pa0227-taxfn.
    types: end of ty_it0227.

    data: lt_pernr type table of ty_pernr.
    data: lt_chg_it0227 type table of ty_it0227.
    data: lt_err_it0227 type table of ty_it0227.
    data: ls_it0227 type ty_it0227.

    data: l_status     type i,
          lv_hire_date type datum,
          lv_msg       type ref to if_hrpa_message_handler.

* Select All relevant employees for the Payroll Area
    select it0000~pernr as dct_pernr
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

*All relevant employees IT 0227 records
    select it0227~pernr, it0227~taxfn, it0227~aedtm
      into corresponding fields of table @lt_chg_it0227
      from pa0227 as it0227
      where it0227~pernr in @it_pernr_so
         and it0227~sprps = @if_hrpa_read_infotype=>unlocked
         and it0227~uname in @mt_uname
         and it0227~endda >= @mv_begda
         and it0227~begda <= @mv_endda
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0227~pernr
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
    order by it0227~pernr.

* Set the date for change date comparison
    lv_validation_date = mv_begda - 40.

* Add all Employees with Invalid TFN Number
    loop at lt_chg_it0227 into ls_it0227.
* Clear from the Employee main list
      delete lt_pernr where dct_pernr = ls_it0227-pernr.

* Change date is with in last 40 days
      check ls_it0227-aedtm ge lv_validation_date.

      "Get EE hire date
      clear: lv_hire_date.
      call function 'HR_ECM_GET_HIRE_DATE'
        exporting
          pernr           = ls_it0227-pernr
          message_handler = lv_msg
        importing
          hire_date       = lv_hire_date.

      clear: l_status.
      call function 'HRPY_AU_VALIDATE_TFN'
        exporting
          taxfn  = ls_it0227-taxfn
        importing
          status = l_status.

      if l_status eq '1'.
        append ls_it0227 to lt_err_it0227.
      elseif l_status eq '0'.
        check ( sy-datum - lv_hire_date ) > mc_28_days
        and ( ls_it0227-taxfn = '000000000' or
              ls_it0227-taxfn = '111111111' or
              ls_it0227-taxfn = '333333333' or
              ls_it0227-taxfn = '444444444' or
              ls_it0227-taxfn = '987654321' ).

        append ls_it0227 to lt_err_it0227.
      endif.
    endloop.

* Add all Employees with incorrect TFN number
    sort lt_err_it0227.
    delete adjacent duplicates from lt_err_it0227 comparing pernr.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_err_it0227 into ls_it0227.
      ls_result-id = ls_it0227-pernr.
      append ls_result to rt_result.
    endloop.

    if lt_pernr is not initial.
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

        message i037 into lv_value_string.

        loop at ct_err_ov into ls_err_ov.
          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_tfn
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


  method get_specifc_custmizing.
* Get parameters specific for this validation rule
    try.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.
ENDCLASS.
