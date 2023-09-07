class zcl_m99_pa_it0011_w_it0015 definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.
  protected section.

    types:
      begin of ty_it0011_dtls,
        subty type subty,
        begda type begda.
    types: end of ty_it0011_dtls .
    types:
      begin of ty_it0011,
        pernr type pernr_d,
        subty type subty,
        begda type begda,
        stext type sbttx.
    types:end of ty_it0011 .
    types:
      tty_it0011 type table of ty_it0011 .
    types:
      begin of ty_it0015,
        pernr type pernr_d,
        subty type subty,
        begda type begda,
        endda type endda,
        betrg type maxbt,
        waers type waers.
    types: end of ty_it0015 .
    types:
      tty_it0015 type table of ty_it0015 .

    data mt_it0015 type tty_it0015 .
    data ms_it0015 type ty_it0015 .
    data mt_it0011 type tty_it0011 .
    data ms_it0011 type ty_it0011 .
    data ms_it0011_dtls type ty_it0011_dtls .
    constants mc_infty_0011 type infty value '0011' ##NO_TEXT.
    constants mc_itemid_paymenterr type pyd_itemid value 'PAYMENTERR' ##NO_TEXT.
    constants mc_filter_subty_01 type pyd_par_type value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_filter_subty_02 type pyd_par_type value 'Z99_FILTER_SUBTY_02' ##NO_TEXT.
    data mt_filter_subty_01 type /iwbep/t_cod_select_options .
    data mt_filter_subty_02 type /iwbep/t_cod_select_options .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_IT0011_W_IT0015 IMPLEMENTATION.


  method check.
* For P0011 record starting in current period, a corresponding P0015 should exist .
    data: lt_pernr type table of ty_pernr.
    data: lt_it0015 type table of ty_it0015.
    data: lt_it0011 type table of ty_it0011.
    data: ls_result  type ty_s_result.

* Fetch Employees Whose Birthday occurs in the period dates
    select it0011~pernr, it0011~subty, it0011~begda
      into corresponding fields of table @lt_it0011 from pa0011 as it0011
     where it0011~pernr in @it_pernr_so
       and it0011~subty in @mt_filter_subty_01
       and it0011~begda <= @mv_endda
       and it0011~begda >= @mv_begda
        and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0011~pernr
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
      order by it0011~pernr.

* All relevant employees with GPPL Payment Records
    if not lt_it0011[] is initial.
      select it0015~pernr, it0015~subty, it0015~begda, it0015~endda, it0015~betrg, it0015~waers
        into corresponding fields of table @lt_it0015 from pa0015 as it0015
        for all entries in @lt_it0011
          where pernr = @lt_it0011-pernr
            and it0015~subty in @mt_filter_subty_02
            and it0015~sprps = @if_hrpa_read_infotype=>unlocked
            and it0015~begda = @lt_it0011-begda.
    endif.
*
    sort lt_it0015 by pernr subty begda.
    loop at lt_it0011 into data(ls_it0011).
      read table lt_it0015 into data(ls_it0015)
       with key pernr = ls_it0011-pernr
                begda = ls_it0011-begda binary search.
      if sy-subrc <> 0.
        append ls_it0011-pernr to lt_pernr.
* Collect All Current records for Reporting
        move-corresponding ls_it0011 to ms_it0011.
        append ms_it0011 to mt_it0011.
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


  method err_ov_get_list.
* Method for Overview list display
    data: ls_err_ov type ty_s_err_ov,
          ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
          lv_modif  type abap_bool,
          lv_pernr  type p_pernr,
          lv_value  type pyd_item_value.

    data: lv_text type pyd_name.
    data: lv_it_begda(10) type c.

    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_it_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

* Populate SFO tab for Display
        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_it0011_dtls.

            move-corresponding ms_it0011_dtls to ms_it0011.
            move lv_pernr to ms_it0011-pernr.

* Read IT 0011 Subtype text
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_infty_0011
                subty               = ms_it0011-subty
                persnr              = ms_it0011-pernr
                begda               = ms_it0011-begda
                endda               = ms_it0011-begda
                molga               = mc_molga_nz
              importing
                stext               = ms_it0011-stext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.
            if sy-subrc <> 0.
* Implement suitable error handling here
            endif.

            concatenate ms_it0011-stext  '(' ms_it0011-subty ')'
              into data(lv_subty_text) separated by space.
            message i096(zhrpy_pcc_msg)
              with lv_subty_text into lv_text.

            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_paymenterr
                iv_text                     = |{ text-001 }|
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

            write: ms_it0011-begda to lv_it_begda dd/mm/yyyy.
            lv_text = lv_it_begda.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_paymenterr
                iv_text                     = |{ text-002 }|
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

* Populate SFO tab for Table Save
          loop at mt_it0011 into ms_it0011
            where pernr = lv_pernr.
            move-corresponding ms_it0011 to ms_it0011_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_it0011_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_paymenterr.
            ls_sfo-value  = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_specifc_custmizing.
* Read Custom Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.

    try.
* Read IT 0011 Subtypes
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_subty_01
          changing
            ct_parameter_tab = mt_filter_subty_01.

* Read IT 0015 Wage types
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_subty_02
          changing
            ct_parameter_tab = mt_filter_subty_02.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
