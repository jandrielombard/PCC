class ZUSECL_M99_PA_IT_SUBTYPE_CHNG definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.

    types:
      begin of ty_infty_dtls,
        subty type subty,
        begda type begda,
        endda type endda,
        uname type uname.
    types: end of ty_infty_dtls .
    types:
      begin of ty_infty,
        pernr type pernr_d,
        subty type subty,
        begda type begda,
        endda type endda,
        uname type uname,
        text  type ltext.
    types: end of ty_infty .
    types:
      tty_infty type table of ty_infty .

    constants mc_itemid_subtychng type pyd_itemid value 'SUBTYCHNG'.
    constants mc_check_begda type pyd_par_type value 'Z99_CHECK_BEGDA' ##NO_TEXT.
    data mv_check_begda type xfeld.
    data mt_infty type tty_infty .
    data ms_infty type ty_infty .
    data ms_infty_dtls type ty_infty_dtls .


    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT_SUBTYPE_CHNG IMPLEMENTATION.


  method CHECK.
*
    types: begin of ty_infty_data,
             pernr type pernr_d,
             subty type subty,
             begda type begda,
             endda type endda,
             uname type uname.
    types: end of ty_infty_data.
    data: lt_infty type standard table of ty_infty_data with non-unique key table_line.
    data: lv_table type tablename.
    data: ls_result  type   ty_s_result.

* Determine DB Table for infotype
    select single dbtab into lv_table from  t777d
           where  infty  = mv_infty.

* All relevant employees with it2001 Records
    select itinfty~pernr, itinfty~subty, itinfty~begda, itinfty~endda, itinfty~uname
      into corresponding fields of table @lt_infty from (lv_table) as itinfty
        where itinfty~subty in @mt_subty
          and itinfty~sprps in @mt_sprps
          and itinfty~uname in @mt_uname
*          and itinfty~aedtm >= @mv_begda
*          and itinfty~aedtm <= @mv_endda_plus1
          and itinfty~aedtm >= @mv_change_begda
          and exists ( select 1
                          from pa0000 as it0000 inner join pa0001 as it0001 on
                               it0000~pernr = it0001~pernr
                         where it0000~pernr = itinfty~pernr
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

* Where Z99_CHECK_BEGDA parameter set then
* Select where BEGDA of record is not equal to start date of PCC pay period
    if mv_check_begda eq abap_true.
      delete lt_infty where begda eq mv_begda.
    endif.

    sort lt_infty. delete adjacent duplicates from lt_infty.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_infty into data(ls_infty).
      ls_result-id = ls_infty-pernr.
      insert ls_result into table rt_result.

* Collect data for Overview List
      move-corresponding ls_infty to ms_infty.
      append ms_infty to mt_infty.
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

    data: lv_text type pyd_name.
    data: lv_it_begda(10) type c.
    data: ls_t582s  type  t582s.

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

        call function 'HR_T582S_READ'
          exporting
            sprsl           = sy-langu
            infty           = mv_infty
            itbld           = ' '
          importing
            t582s           = ls_t582s
          exceptions
            entry_not_found = 1
            others          = 2.
        concatenate ls_t582s-itext '(' mv_infty ')'
              into data(lv_infty_text).

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
                p_struct2 = ms_infty_dtls.

            move-corresponding ms_infty_dtls to ms_infty.
* Read IT2002 Subtype text
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mv_infty
                subty               = ms_infty-subty
                persnr              = lv_pernr
                begda               = ms_infty-begda
                endda               = ms_infty-endda
*               molga               = mc_molga_au           "MOD001--
                molga               = mv_molga              "MOD001++
              importing
                stext               = ms_infty-text
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.
            if sy-subrc <> 0.
* Implement suitable error handling here
            endif.

            concatenate ms_infty-text  '(' ms_infty-subty ')'
              into data(lv_subty_text) separated by space.
            write: ms_infty-begda to lv_it_begda dd/mm/yyyy.
            message i012(zhrpy_pcc_msg)
              with lv_infty_text lv_subty_text into lv_text.

            lv_it_date_txt = lv_it_begda.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_subtychng
                iv_text                     = lv_it_date_txt
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
          loop at mt_infty into ms_infty
            where pernr = lv_pernr.
            move-corresponding ms_infty to ms_infty_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_infty_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_subtychng.
            ls_sfo-datum  = ms_infty-begda.
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

* Read Custom Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.

    try.
* Read Z99_CHEK_BEGDA
        me->mv_check_begda = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_check_begda
                                                                    it_par = me->get_context( )->mt_par ).

      catch cx_pyd_fnd into data(lo_exception).
    endtry.


  endmethod.
ENDCLASS.
