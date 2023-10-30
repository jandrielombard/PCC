class ZUSECL_M99_PA_IT2006_W_IT0014 definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.

    types:
      begin of ty_it2006_dtls,
        begda   type p2006-begda,
        ktart   type p2006-ktart,
        balance type p2006-anzhl.
    types: end of ty_it2006_dtls .
    types:
      begin of ty_it2006,
        pernr   type pernr_d,
        begda   type p2006-begda,
        ktart   type p2006-ktart,
        balance type p2006-anzhl.
    types:end of ty_it2006 .
    types:
      tty_it2006 type table of ty_it2006 .
    types:
      begin of ty_it0014,
        pernr type pernr_d,
        subty type subty,
        begda type begda,
        endda type endda,
        betrg type maxbt,
        waers type waers.
    types: end of ty_it0014 .
    types:
      tty_it0014 type table of ty_it0014 .

    data mt_it0014 type tty_it0014 .
    data ms_it0014 type ty_it0014 .
    data mt_it2006 type tty_it2006 .
    data ms_it2006 type ty_it2006 .
    data ms_it2006_dtls type ty_it2006_dtls .
    constants mc_infty_2006 type infty value '2006' ##NO_TEXT.
    constants mc_itemid_plwrerror type pyd_itemid value 'PLWRERROR' ##NO_TEXT.
    constants mc_filter_ktart type pyd_par_type value 'Z99_KTART' ##NO_TEXT.
    constants mc_filter_subty_01 type pyd_par_type value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
    data mt_filter_ktart type /iwbep/t_cod_select_options .
    data mt_filter_subty_01 type /iwbep/t_cod_select_options .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT2006_W_IT0014 IMPLEMENTATION.


  method CHECK.
* Purchase Leave without Weekly Rate
    data: lt_pernr type table of ty_pernr.
    data: lt_it0014 type table of ty_it0014.
    data: lt_it2006 type table of ty_it2006.
    data: ls_result  type ty_s_result.

* Fetch Employees Purchase leave balance in the period
    select it2006~pernr, it2006~ktart, sum( it2006~anzhl - it2006~kverb ) as balance
      into corresponding fields of table @lt_it2006 from pa2006 as it2006
     where it2006~pernr in @it_pernr_so
       and it2006~subty in @mt_filter_ktart
       and it2006~begda <= @mv_endda
       and it2006~endda >= @mv_begda
        and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it2006~pernr
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
      group by it2006~pernr, it2006~ktart.

* All relevant employees with purcahse leave balance
    if not lt_it2006[] is initial.
      delete lt_it2006 where not balance > 0.

      select it0014~pernr, it0014~subty, it0014~begda, it0014~endda, it0014~betrg, it0014~waers
        into corresponding fields of table @lt_it0014 from pa0014 as it0014
        for all entries in @lt_it2006
          where pernr = @lt_it2006-pernr
            and it0014~subty in @mt_filter_subty_01
            and it0014~sprps = @if_hrpa_read_infotype=>unlocked
            and it0014~begda <= @mv_endda
            and it0014~endda >= @mv_begda.
    endif.
*
    sort lt_it0014 by pernr subty begda.
    loop at lt_it2006 into data(ls_it2006).
      read table lt_it0014 into data(ls_it0014)
       with key pernr = ls_it2006-pernr binary search.
      if sy-subrc <> 0.
        append ls_it2006-pernr to lt_pernr.

* Collect All Current records for Reporting
        move-corresponding ls_it2006 to ms_it2006.
        append ms_it2006 to mt_it2006.
      endif.
    endloop.

* Build Results table
    if not lt_pernr is initial.
      sort lt_pernr by dct_pernr.
      delete adjacent duplicates from lt_pernr comparing dct_pernr.

      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_pernr into data(ls_pernr).
        ls_result-id = ls_pernr-dct_pernr.
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

        lv_value_string = text-002.
        loop at ct_err_ov into ls_err_ov.
          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_plwrerror
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


  method GET_SPECIFC_CUSTMIZING.
* Read Custom Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.

    try.
* Read Quota types
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_ktart
          changing
            ct_parameter_tab = mt_filter_ktart.

* Read IT 0014 Wage types
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_subty_01
          changing
            ct_parameter_tab = mt_filter_subty_01.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
