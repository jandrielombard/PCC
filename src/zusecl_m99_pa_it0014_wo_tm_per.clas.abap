class ZUSECL_M99_PA_IT0014_WO_TM_PER definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.

    types:
      begin of ty_allow_data,
        pernr       type pernr_d,
        infty       type infty,
        subty       type subty,
        begda       type begda,
        endda       type endda,
        trfar       type trfar,
        trfgb        type trfgb,
        trfar_trfgb type char4,
        anzhl       type enanz.
    types: end of ty_allow_data .
    types:
      tty_allow_data type table of ty_allow_data .
    types:
      begin of ty_time_data,
        pernr type pernr_d,
        infty type infty,
        subty type subty,
        begda type begda,
        endda type endda,
        anzhl type enanz.
    types: end of ty_time_data .
    types:
      tty_time_data type table of ty_time_data .
    types:
      begin of ty_period_data,
        pernr type hrpayco_pernr_char,
        subty type subty,
        begda type begda,
        endda type endda,
        anzhl type enanz.
    types: end of ty_period_data .
    types:
      tty_period_data type table of ty_period_data .
    types:
      begin of ty_allowerr_dtls,
        subty type subty,
        begda type begda,
        endda type endda,
        error type char1,
      end of ty_allowerr_dtls .
    types:
      begin of ty_it0014_wo_time,
        pernr type pernr_d,
        subty type subty,
        begda type begda,
        endda type endda,
        error type char1,
      end of ty_it0014_wo_time .
    types:
      tty_it0014_wo_time type table of ty_it0014_wo_time .

    constants mc_itemid_allowerr type pyd_itemid value 'ALLOWERR' ##NO_TEXT.
    constants mc_it2001 type infty value '2001' ##NO_TEXT.
    constants mc_it2002 type infty value '2002' ##NO_TEXT.
    constants mc_it0014 type infty value '0014' ##NO_TEXT.
    constants mc_filter_subty_01 type pyd_par_type value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_filter_subty_02 type pyd_par_type value 'Z99_FILTER_SUBTY_02' ##NO_TEXT.
    constants mc_exc_filter_subty_01 type pyd_par_type value 'Z99_EXC_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_exc_filter_subty_02 type pyd_par_type value 'Z99_EXC_FILTER_SUBTY_02' ##NO_TEXT.
*    constants mc_z99_lgart_ind type pyd_par_type value 'Z99_LGART_IND' ##NO_TEXT.
*    constants mc_condition_lgart type pyd_par_type value 'Z99_CONDITION_LGART' ##NO_TEXT.
*    constants mc_comparison_operator type pyd_par_type value 'Z99_COMPARISON_OPERATOR' ##NO_TEXT.
    constants mc_filter_trfar type pyd_par_type value 'Z99_TRFAR' ##NO_TEXT.
    constants mc_exc_filter_trfar type pyd_par_type value 'Z99_EXC_TRFAR' ##NO_TEXT.
    constants mc_filter_trfgb type pyd_par_type value 'Z99_TRFGB' ##NO_TEXT.
    constants mc_exc_filter_trfgb type pyd_par_type value 'Z99_EXC_TRFGB' ##NO_TEXT.
    constants mc_filter_trfar_trfgb type pyd_par_type value 'Z99_TRFAR_TRFGB' ##NO_TEXT.
    constants mc_missing type pyd_itemid value 'MISSING' ##NO_TEXT.
    constants mc_mismatch type pyd_itemid value 'MISMATCH' ##NO_TEXT.
    data mt_filter_subty_01 type /iwbep/t_cod_select_options .
    data mt_filter_subty_02 type /iwbep/t_cod_select_options .
    data mt_trfar type /iwbep/t_cod_select_options .
    data mt_trfgb type /iwbep/t_cod_select_options .
    data mt_trfar_trfgb type /iwbep/t_cod_select_options .
    data mt_condition_lgart type /iwbep/t_cod_select_options .
    data mv_comparison_operator type zhrau_de_checkop .
    data mt_it0014_wo_time type tty_it0014_wo_time .
    data ms_it0014_wo_time type ty_it0014_wo_time .
    data ms_allowerr_dtls type ty_allowerr_dtls .
    data mv_z99_lgart_ind type boolean .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT0014_WO_TM_PER IMPLEMENTATION.


  method CHECK.
* All Relevant IT 2010 Wage types
    data: lt_all_subty  type /iwbep/t_cod_select_options,
          lt_it0014     type standard table of ty_allow_data,
          lt_it2001     type standard table of ty_time_data,
          lt_it2002     type standard table of ty_time_data,

          lt_allow_data type tty_period_data,
          lt_time_data  type tty_period_data,
          ls_allow_data type ty_period_data,
          ls_time_data  type ty_period_data.

    data: lt_it0014_wo_time type tty_it0014_wo_time,
          ls_it0014_wo_time type ty_it0014_wo_time.
    data: ls_result  type   ty_s_result.

* All relevant employees with IT2001 Records
    select it0014~pernr, it0014~subty, it0014~begda, it0014~endda,
            it0014~betrg as anzhl, it0008~trfar, it0008~trfgb, it0008~trfar && it0008~trfgb as trfar_trfgb
      into corresponding fields of table @lt_it0014
      from pa0014 as it0014 inner join pa0008 as it0008 on
        it0014~pernr = it0008~pernr
       where it0014~pernr in @it_pernr_so
         and it0014~subty in @mt_filter_subty_01
         and it0014~sprps = ' '
         and it0014~uname in @mt_uname
         and it0014~begda <= @mv_endda
         and it0014~endda >= @mv_begda
         and it0008~begda <= @mv_endda
         and it0008~endda >= @mv_begda
         and it0008~trfar in @mt_trfar
         and it0008~trfgb in @mt_trfgb
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0014~pernr
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
*      group by it0014~pernr, it0014~begda, it0014~endda, trfar_trfgb .

* Filter the data by Z99_TRFAR_TRFGB
    if not mt_trfar_trfgb is initial.
      delete lt_it0014 where trfar_trfgb not in mt_trfar_trfgb.
    endif.

* Read IT 2001 and IT 2002
    if not lt_it0014 is initial.
* Prepare Subtype List
      refresh lt_all_subty.
      append lines of mt_filter_subty_02 to lt_all_subty.
                                                            "IT 2001
      refresh: lt_it2001.
      select it2001~pernr as pernr, it2001~subty as subty,
             it2001~begda as begda, it2001~endda as endda, it2001~stdaz as anzhl
         into corresponding fields of table @lt_it2001 from pa2001 as it2001
          for all entries in @lt_it0014
       where it2001~pernr = @lt_it0014-pernr
         and it2001~subty in @lt_all_subty
         and it2001~sprps = ' '
         and it2001~endda >= @mv_begda
         and it2001~begda <= @mv_endda.

* Prepare IT 2001 records for the check
      sort lt_it2001 by pernr begda endda subty.
      loop at lt_it2001 into data(ls_it2001).
        if ls_it2001-subty in mt_filter_subty_02.
          move-corresponding ls_it2001 to ls_time_data.
          collect ls_time_data into lt_time_data.
        endif.
      endloop.
                                                            "IT 2002
      refresh: lt_it2002.
      select it2002~pernr as pernr, it2002~subty as subty,
             it2002~begda as begda, it2002~endda as endda, it2002~stdaz as anzhl
         into corresponding fields of table @lt_it2002 from pa2002 as it2002
          for all entries in @lt_it0014
       where it2002~pernr = @lt_it0014-pernr
         and it2002~subty in @lt_all_subty
         and it2002~sprps = ' '
         and it2002~endda >= @mv_begda
         and it2002~begda <= @mv_endda.

* Prepare IT 2002 records for the check
      sort lt_it2002 by pernr begda endda subty.
      loop at lt_it2002 into data(ls_it2002).
        if ls_it2002-subty in mt_filter_subty_02.
          move-corresponding ls_it2002 to ls_time_data.
          collect ls_time_data into lt_time_data.
        endif.
      endloop.
      refresh: lt_it2001, lt_it2002.

      sort lt_time_data by pernr.
    endif.

* Prepare Final IT 2010 records for the check
    sort lt_it0014 by pernr.
    loop at lt_it0014 into data(ls_it0014).
      move-corresponding ls_it0014 to ls_allow_data.
      collect ls_allow_data into lt_allow_data.
    endloop.
    refresh: lt_it0014.

* Prepare data for result
    loop at lt_allow_data into ls_allow_data.
      read table lt_time_data into ls_time_data
             with key pernr = ls_allow_data-pernr binary search.
      if sy-subrc ne 0.
* Collect data for Overview List
        clear: ls_it0014_wo_time.
        move: ls_allow_data-pernr to ls_it0014_wo_time-pernr,
              mc_missing          to ls_it0014_wo_time-error,
              ls_allow_data-subty to ls_it0014_wo_time-subty,
              ls_allow_data-begda to ls_it0014_wo_time-begda,
              ls_allow_data-endda to ls_it0014_wo_time-endda.
        append ls_it0014_wo_time to lt_it0014_wo_time.
      endif.
    endloop.

* Collect the data for Overview
    append lines of lt_it0014_wo_time to mt_it0014_wo_time.
    sort mt_it0014_wo_time by pernr error.
    delete adjacent duplicates from mt_it0014_wo_time comparing all fields.

* Delete Duplicate Records for Employee for result preparation
    sort lt_it0014_wo_time by pernr ascending.
    delete adjacent duplicates from lt_it0014_wo_time comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it0014_wo_time into ls_it0014_wo_time.
* it0014 Exists on the day with out IT2001 and IT2002
      ls_result-id = ls_it0014_wo_time-pernr.
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

    data: lv_text type pyd_name.

    data: lv_char_value type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_msg_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    data: lv_allowance_text type pyd_name,
          lv_stext          type  sbttx.

    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters
        get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).
        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

* Allowance Error Message
          clear ls_err_ov-sfo_tab.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_allowerr_dtls.

            move-corresponding ms_allowerr_dtls to ms_it0014_wo_time.
            move lv_pernr to ms_it0014_wo_time-pernr.

            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_it0014
                subty               = ms_it0014_wo_time-subty
                persnr              = ms_it0014_wo_time-pernr
                begda               = ms_it0014_wo_time-begda
                endda               = ms_it0014_wo_time-endda
*               molga               = mc_molga_au              "MOD001--
                molga               = mv_molga                 "MOD001++
              importing
                stext               = lv_stext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.

            concatenate lv_stext '(' ms_it0014_wo_time-subty ')' into lv_allowance_text.
            message i036(zhrpy_pcc_msg) with lv_allowance_text into lv_text.
            lv_msg_txt = text-001.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_allowerr
                iv_text                     = lv_msg_txt
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

          loop at mt_it0014_wo_time into ms_it0014_wo_time
            where pernr = lv_pernr.

            move-corresponding ms_it0014_wo_time to ms_allowerr_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_allowerr_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_allowerr.
            ls_sfo-value = lv_char_value.
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
    data: lt_param_so type /iwbep/t_cod_select_options,
          ls_par      type if_pyd_fnd_types=>ty_s_resp.

    try.
* Read IT 0014 Subtypes
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_subty_01
            iv_exc_par_type  = me->mc_exc_filter_subty_01
          changing
            ct_parameter_tab = mt_filter_subty_01.

* Read IT 2001 Subtypes
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_subty_02
            iv_exc_par_type  = me->mc_exc_filter_subty_02
          changing
            ct_parameter_tab = mt_filter_subty_02.

* Condition Wage types
*        loop at mo_context->mt_par into ls_par where par_type = me->mc_condition_lgart.
*          clear lt_param_so.
*          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
*          append lines of lt_param_so to mt_condition_lgart.
*        endloop.

* Comparison Oparator
*        mv_comparison_operator =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_comparison_operator
*                                                                        it_par      = mo_context->mt_par ).
*        if mv_comparison_operator is initial.
*          move 'EQ' to mv_comparison_operator.
*        endif.
* TRFAR
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_trfar
            iv_exc_par_type  = me->mc_exc_filter_trfar
          changing
            ct_parameter_tab = mt_trfar.
* TRFGB
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_trfgb
            iv_exc_par_type  = me->mc_exc_filter_trfgb
          changing
            ct_parameter_tab = mt_trfgb.
* TRFAR_TRFGB
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_trfar_trfgb
          changing
            ct_parameter_tab = mt_trfar_trfgb.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
