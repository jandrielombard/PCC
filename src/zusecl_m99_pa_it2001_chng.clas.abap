class ZUSECL_M99_PA_IT2001_CHNG definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.

    types:
      begin of ty_it2001_dtls,
        subty type subty,
        begda type begda,
        endda type endda,
        uname type uname,
        stdaz type enanz.
    types: end of ty_it2001_dtls .
    types:
      begin of ty_it2001,
        pernr  type pernr_d,
        subty  type subty,
        begda  type begda,
        endda  type endda,
        uname  type uname,
        stdaz  type enanz,
        abtext type ltext.
    types: end of ty_it2001 .
    types:
      tty_it2001 type table of ty_it2001 .

    data mt_it2001 type tty_it2001 .
    data ms_it2001 type ty_it2001 .
    data ms_it2001_dtls type ty_it2001_dtls .
    constants mc_itemid_absenerr type pyd_itemid value 'ABSENERR' ##NO_TEXT.
    constants mc_infty_2001 type infty value '2001' ##NO_TEXT.
    constants mc_num_high_01 type pyd_par_type value 'Z99_NUM_HIGH_01' ##NO_TEXT.
    constants mc_num_low_01 type pyd_par_type value 'Z99_NUM_LOW_01' ##NO_TEXT.
    data mv_num_high_01 type pranz .
    data mv_num_low_01 type pranz .
    data mv_num_high_fl type boolean .
    data mv_num_low_fl type boolean .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT2001_CHNG IMPLEMENTATION.


  method CHECK.
    data: lt_it2001     type tty_it2001,
          lt_tmp_it2001 type tty_it2001.

* Read Check data
    data:     ls_result  type   ty_s_result.
* All relevant employees with it2001 Records
    select it2001~pernr, it2001~subty, it2001~begda, it2001~endda,
           it2001~uname, it2001~stdaz
      into corresponding fields of table @lt_it2001 from pa2001 as it2001
        where it2001~pernr in @it_pernr_so
          and it2001~subty in @mt_subty
          and it2001~sprps in @mt_sprps
          and it2001~uname in @mt_uname
*          and it2001~aedtm >= @mv_begda
*          and it2001~aedtm <= @mv_endda_plus1
          and it2001~aedtm >= @mv_change_begda
          and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it2001~pernr
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

    sort lt_it2001. delete adjacent duplicates from lt_it2001.
    if mv_num_low_fl eq abap_true.
      delete lt_it2001 where stdaz lt mv_num_low_01.
    endif.
    if mv_num_high_fl eq abap_true.
      delete lt_it2001 where stdaz gt mv_num_high_01.
    endif.

* Collect Results for Overview
    append lines of lt_it2001 to mt_it2001.

* Build Result Table
    lt_tmp_it2001 = lt_it2001.
    sort lt_tmp_it2001 by pernr.
    delete adjacent duplicates from lt_tmp_it2001 comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_tmp_it2001 into data(ls_it2001).
      ls_result-id = ls_it2001-pernr.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.
  endmethod.


  method ERR_OV_GET_LIST.
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
      lv_text   type pyd_name.

    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_ab_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
    data: lv_ab_begda(10) type c.

* Populate SFO Tab
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

* Transform IT 2001 details for Display
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
                p_struct2 = ms_it2001_dtls.

            move-corresponding ms_it2001_dtls to ms_it2001.

* Read IT2002 Subtype text
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_infty_2001
                subty               = ms_it2001-subty
                persnr              = lv_pernr
                begda               = ms_it2001-begda
                endda               = ms_it2001-endda
*               molga               = mc_molga_au       "MOD001--
                molga               = mv_molga          "MOD001++
              importing
                stext               = ms_it2001-abtext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.
            if sy-subrc <> 0.
* Implement suitable error handling here
            endif.

            write: ms_it2001-begda to lv_ab_begda dd/mm/yyyy.
            message i010(zhrpy_pcc_msg)
              with ms_it2001-abtext ms_it2001-subty ms_it2001-stdaz ms_it2001-uname
              into lv_text.

            lv_ab_date_txt = lv_ab_begda.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_absenerr
                iv_text                     = lv_ab_date_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.
* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

* Save IT 2001 details into table
          loop at mt_it2001 into ms_it2001
            where pernr = lv_pernr.
            move-corresponding ms_it2001 to ms_it2001_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_it2001_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_absenerr.
            ls_sfo-datum  = ms_it2001-begda.
            ls_sfo-value  = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Read Check Specific Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.
    data: ls_par   type if_pyd_fnd_types=>ty_s_resp,
          lv_count type i.
    try.
* Read Hours Low
*        me->mv_num_low_01 = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_low_01
*                                                                    it_par = me->get_context( )->mt_par ).
        clear: lv_count, mv_num_low_fl.
        loop at me->get_context( )->mt_par into ls_par where par_type = me->mc_num_low_01.
          if ls_par-kind = 'P' or ( ls_par-kind = 'S' and ls_par-sign = 'I' and ls_par-opti = 'EQ' ).
            add 1 to lv_count.
            me->mv_num_low_01 = ls_par-low.
          endif.
          exit.
        endloop.
        if lv_count <> 0.
          mv_num_low_fl = abap_true.
        endif.

* Read Hours High
*        me->mv_num_high_01 = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_high_01
*                                                                    it_par = me->get_context( )->mt_par ).
        clear: lv_count, mv_num_high_fl.
        loop at me->get_context( )->mt_par into ls_par where par_type = me->mc_num_high_01.
          if ls_par-kind = 'P' or ( ls_par-kind = 'S' and ls_par-sign = 'I' and ls_par-opti = 'EQ' ).
            add 1 to lv_count.
            me->mv_num_high_01 = ls_par-low.
          endif.
          exit.
        endloop.
        if lv_count <> 0.
          mv_num_high_fl = abap_true.
        endif.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
