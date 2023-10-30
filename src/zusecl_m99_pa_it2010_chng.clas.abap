class ZUSECL_M99_PA_IT2010_CHNG definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.

    types:
      begin of ty_it2010_dtls,
        subty type subty,
        begda type begda,
        endda type endda,
        seqnr type seqnr,
        uname type uname,
        anzhl type enanz,
        zeinh type pt_zeinh,
        betrg type pad_amt7s,
        waers type waers.
    types: end of ty_it2010_dtls .
    types:
      begin of ty_it2010,
        pernr  type pernr_d,
        subty  type subty,
        begda  type begda,
        endda  type endda,
        seqnr  type seqnr,
        uname  type uname,
        anzhl  type enanz,
        zeinh  type pt_zeinh,
        betrg  type pad_amt7s,
        waers  type waers,
        altext type ltext,
        etext  type einhtxt.
    types: end of ty_it2010 .
    types:
      tty_it2010 type table of ty_it2010 .

    data mt_it2010 type tty_it2010 .
    data ms_it2010 type ty_it2010 .
    data ms_it2010_dtls type ty_it2010_dtls .
    constants mc_itemid_allowerr type pyd_itemid value 'ALLOWERR' ##NO_TEXT.
    constants mc_infty_2010 type infty value '2010' ##NO_TEXT.
    data mc_num_high_01 type pyd_par_type value 'Z99_NUM_HIGH_01' ##NO_TEXT.
    data mc_num_low_01 type pyd_par_type value 'Z99_NUM_LOW_01' ##NO_TEXT.
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



CLASS ZUSECL_M99_PA_IT2010_CHNG IMPLEMENTATION.


  method CHECK.
* Read Check data
    data: ls_result type ty_s_result.
    data: lt_it2010 type tty_it2010.

* All relevant employees with IT2010 Records
    select it2010~pernr, it2010~subty, it2010~begda, it2010~endda, it2010~seqnr,
           it2010~uname, it2010~anzhl, it2010~zeinh, it2010~betrg, it2010~waers
      into corresponding fields of table @mt_it2010 from pa2010 as it2010
        where it2010~pernr in @it_pernr_so
          and it2010~subty in @mt_subty
          and it2010~sprps in @mt_sprps
          and it2010~uname in @mt_uname
*          and it2010~aedtm >= @mv_begda
*          and it2010~aedtm <= @mv_endda_plus1
          and it2010~aedtm >= @mv_change_begda
          and exists ( select 1
                          from pa0000 as it0000 inner join pa0001 as it0001 on
                               it0000~pernr = it0001~pernr
                         where it0000~pernr = it2010~pernr
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

    sort mt_it2010. delete adjacent duplicates from mt_it2010.
    if not mv_num_high_01 is initial.
      delete mt_it2010 where not anzhl between mv_num_low_01 and mv_num_high_01.
    endif.
    sort mt_it2010 by pernr begda seqnr subty.

* Prepare to build Result table
    lt_it2010 = mt_it2010.
    sort lt_it2010 by pernr.
    delete adjacent duplicates from lt_it2010 comparing pernr.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it2010 into data(ls_it2010).
      ls_result-id = ls_it2010-pernr.
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
      lv_value  type pyd_item_value.

    data lv_text type pyd_name.
    data lv_al_begda(10) type c.

    data lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data lv_al_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
    data lv_etext type t538t-etext.

* Populate SFO Tab
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters                      "MOD001++
          exporting                                     "MOD001++
            it_par         = it_par                     "MOD001++
            io_res_context = io_res_context.            "MOD001++
* Transform IT 2010 details for Display
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
                p_struct2 = ms_it2010_dtls.

            move-corresponding ms_it2010_dtls to ms_it2010.
            move lv_pernr to ms_it2010-pernr.
* Read IT2010 Subtype text
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_infty_2010
                subty               = ms_it2010-subty
                persnr              = ms_it2010-pernr
                begda               = ms_it2010-begda
                endda               = ms_it2010-endda
*               molga               = mc_molga_au           "MOD001--
                molga               = mv_molga              "MOD001++
              importing
                stext               = ms_it2010-altext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.
            if sy-subrc <> 0.
* Implement suitable error handling here
            endif.

            write: ms_it2010-begda to lv_al_begda dd/mm/yyyy.
            if not ms_it2010-betrg is initial.
              write ms_it2010-betrg to lv_char_value currency ms_it2010-waers left-justified.
              lv_text = lv_char_value && | | && ms_it2010-waers.
              message i013(zhrpy_pcc_msg)
              with ms_it2010-altext ms_it2010-subty lv_text ms_it2010-uname
              into lv_text.
            else.
              select single etext from  t538t into lv_etext
               where  sprsl  = sy-langu and    zeinh  = ms_it2010-zeinh.
              lv_text = ms_it2010-anzhl && | | && lv_etext.
              message i010(zhrpy_pcc_msg)
                with ms_it2010-altext ms_it2010-subty lv_text ms_it2010-uname
                into lv_text.
            endif.

            lv_al_date_txt = lv_al_begda.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_allowerr
                iv_text                     = lv_al_date_txt
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
          lv_pernr = ls_err_ov-id.

* Save IT 2010 details into table
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          loop at mt_it2010 into ms_it2010
            where pernr = lv_pernr.

            move-corresponding ms_it2010 to ms_it2010_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_it2010_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_allowerr.
            ls_sfo-value  = lv_char_value.
            ls_sfo-datum  = ms_it2010-begda.
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
