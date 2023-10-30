class ZUSECL_M99_PA_EMAIL_CHECK definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
  protected section.
    constants mc_itemid_email type pyd_s_rdsfo_ext-itemid value 'EMAILADDR' ##NO_TEXT.
    types: begin of ty_email_data,
             pernr      type pernr_d,
             usrid_long type pa0105-usrid_long.
    types: end of ty_email_data.
    types: tty_email_data type table of ty_email_data.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_EMAIL_CHECK IMPLEMENTATION.


  method check.
* Bank Details: Check for Duplicate Bank Account
    data: lt_email_data type tty_email_data.
    data: ls_email_data type ty_email_data.
    data: lt_pernr type table of ty_pernr.
    data: ls_result like line of rt_result.

* All relevant employees IT 0105 records
    select it0105~pernr, it0105~usrid_long
      into corresponding fields of table @lt_email_data
      from pa0105 as it0105
      where it0105~pernr in @it_pernr_so
         and it0105~subty in @mt_subty
         and it0105~sprps = @if_hrpa_read_infotype=>unlocked
         and it0105~endda >= @mv_begda
         and it0105~begda <= @mv_endda
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0105~pernr
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
    order by it0105~pernr.

    loop at it_pernr_so assigning field-symbol(<person>).
      read table lt_email_data into ls_email_data with key pernr = <person>-low.

* Validate Email data
      if sy-subrc = 0.
        if ls_email_data-usrid_long na '@' or ls_email_data-usrid_long na '.'.
          append ls_email_data-pernr to lt_pernr.
        endif.
      else.  "No email address exists
        append <person>-low to lt_pernr.
      endif.
    endloop.

* Report Result
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
    data: ls_err_ov       type ty_s_err_ov,
          lv_value_string type string.

* Overview messages
    case iv_access_mode.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        loop at ct_err_ov into ls_err_ov.
          lv_value_string = text-001.
          me->add_record_to_sfo_tab(
              exporting
                iv_itemid                   = mc_itemid_email
                iv_text                     = |{ text-002 }|
                iv_value                    = lv_value_string
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab ).

          lv_value_string = text-003.
          me->add_record_to_sfo_tab(
              exporting
                iv_itemid                   = mc_itemid_email
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
