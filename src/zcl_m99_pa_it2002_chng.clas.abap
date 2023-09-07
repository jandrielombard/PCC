class zcl_m99_pa_it2002_chng definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.
  protected section.

    types:
      begin of ty_it2002_dtls,
        subty type subty,
        begda type begda,
        endda type endda,
        uname type uname,
        stdaz type enanz.
    types: end of ty_it2002_dtls .
    types:
      begin of ty_it2002,
        pernr  type pernr_d,
        subty  type subty,
        begda  type begda,
        endda  type endda,
        uname  type uname,
        stdaz  type enanz,
        attext type ltext.
    types: end of ty_it2002 .
    types:
      tty_it2002 type table of ty_it2002 .

    data mt_it2002 type tty_it2002 .
    data ms_it2002 type ty_it2002 .
    data ms_it2002_dtls type ty_it2002_dtls .
    constants mc_itemid_atenderr type pyd_itemid value 'ATENDERR' ##NO_TEXT.
    constants mc_infty_2002 type infty value '2002' ##NO_TEXT.
    constants mc_num_high_01 type pyd_par_type value 'Z99_NUM_HIGH_01' ##NO_TEXT.
    constants mc_num_low_01 type pyd_par_type value 'Z99_NUM_LOW_01' ##NO_TEXT.
    data mv_num_high_01 type pranz .
    data mv_num_low_01 type pranz .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_IT2002_CHNG IMPLEMENTATION.


  method check.
* Read Check data
    data: ls_result  type   ty_s_result.
    data: lt_it2002 type tty_it2002.

* All relevant employees with it2002 Records
    select it2002~pernr, it2002~subty, it2002~begda, it2002~endda,
           it2002~uname, it2002~stdaz
      into corresponding fields of table @lt_it2002 from pa2002 as it2002
        where it2002~pernr in @it_pernr_so
          and it2002~subty in @mt_subty
          and it2002~sprps in @mt_sprps
          and it2002~uname in @mt_uname
*          and it2002~aedtm >= @mv_begda
*          and it2002~aedtm <= @mv_endda_plus1
          and it2002~aedtm >= @mv_change_begda
          and exists ( select 1
                          from pa0000 as it0000 inner join pa0001 as it0001 on
                               it0000~pernr = it0001~pernr
                         where it0000~pernr = it2002~pernr
                           and it0000~begda <= @mv_endda
                           and it0000~endda >= @mv_begda
                           and it0000~stat2 in @mt_stat2
                           and it0000~sprps = ' '
                           and it0001~begda <= @mv_endda
                           and it0001~endda >= @mv_begda
                           and it0001~sprps = ' '
                           and it0001~abkrs in @mt_payroll_areas
                           and it0001~bukrs in @mt_bukrs
                           and it0001~werks in @mt_werks
                           and it0001~btrtl in @mt_btrtl
                           and it0001~persg in @mt_persg
                           and it0001~persk in @mt_persk
                           and it0001~kostl in @mt_kostl ).

* Apply Amount Filter
    sort lt_it2002. delete adjacent duplicates from lt_it2002.
    if not mv_num_high_01 is initial.
      delete lt_it2002 where not stdaz between mv_num_low_01 and mv_num_high_01.
    endif.

* Collect data for Overview
    append lines of lt_it2002 to mt_it2002.

* Build results table
    sort lt_it2002 by pernr.
    delete adjacent duplicates from lt_it2002 comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it2002 into data(ls_it2002).
      ls_result-id = ls_it2002-pernr.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Method for Overview list display
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr,
      lv_value  type pyd_item_value.

    data: lv_text type pyd_name.
    data: lv_at_begda(10) type c.

    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_at_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
* Populate SFO Tab
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters                      "MOD001++
          exporting                                     "MOD001++
            it_par         = it_par                     "MOD001++
            io_res_context = io_res_context.            "MOD001++
* Populate IT 2002 details for Display
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
                p_struct2 = ms_it2002_dtls.

            move-corresponding ms_it2002_dtls to ms_it2002.
            move lv_pernr to ms_it2002-pernr.
* Read IT2002 Subtype text
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_infty_2002
                subty               = ms_it2002-subty
                persnr              = ms_it2002-pernr
                begda               = ms_it2002-begda
                endda               = ms_it2002-endda
*               molga               = mc_molga_au         "MOD001--
                molga               = mv_molga            "MOD001++
              importing
                stext               = ms_it2002-attext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.
            if sy-subrc <> 0.
* Implement suitable error handling here
            endif.

            write: ms_it2002-begda to lv_at_begda dd/mm/yyyy.
            message i010(zhrpy_pcc_msg)
              with ms_it2002-attext ms_it2002-subty ms_it2002-stdaz ms_it2002-uname
              into lv_text.

            lv_at_date_txt = lv_at_begda.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_atenderr
                iv_text                     = lv_at_date_txt
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
* Populate IT 2002 details into table
          loop at mt_it2002 into ms_it2002
            where pernr = lv_pernr.

            move-corresponding ms_it2002 to ms_it2002_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_it2002_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_atenderr.
            ls_sfo-value  = lv_char_value.
            ls_sfo-datum  = ms_it2002-begda.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_specifc_custmizing.
* Read Check Specific Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.
    data ls_par       type if_pyd_fnd_types=>ty_s_resp.

    try.
* Read Hours Low
        me->mv_num_low_01 = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_low_01
                                                                    it_par = me->get_context( )->mt_par ).

* Read Hours High
        me->mv_num_high_01 = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_high_01
                                                                    it_par = me->get_context( )->mt_par ).

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
