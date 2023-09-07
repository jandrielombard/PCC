class zcl_m99_pa_it2010_wo_time_per definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.
  protected section.

    types:
      begin of ty_time_data,
        pernr type pernr_d,
        infty type infty,
        subty type subty,
        begda type begda,
        endda type endda,
        seqnr type seqnr,
        anzhl type enanz.
    types: end of ty_time_data .
    types:
      tty_time_data type table of ty_time_data .
    types:
      begin of ty_period_data,
        pernr type hrpayco_pernr_char,
        anzhl type enanz.
    types: end of ty_period_data .
    types:
      tty_period_data type table of ty_period_data .
    types:
      begin of ty_allowerr_dtls,
        error       type char1,
        allow_anzhl type enanz,
        time_anzhl  type enanz,
      end of ty_allowerr_dtls .
    types:
      begin of ty_it2010_wo_time,
        pernr       type pernr_d,
        error       type char1,
        allow_anzhl type enanz,
        time_anzhl  type enanz,
      end of ty_it2010_wo_time .
    types:
      tty_it2010_wo_time type table of ty_it2010_wo_time .

    constants mc_itemid_allowerr type pyd_itemid value 'ALLOWERR' ##NO_TEXT.
    constants mc_it2001 type infty value '2001' ##NO_TEXT.
    constants mc_it2002 type infty value '2002' ##NO_TEXT.
    constants mc_it2010 type infty value '2010' ##NO_TEXT.
    constants mc_filter_subty_01 type pyd_par_type value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_filter_subty_02 type pyd_par_type value 'Z99_FILTER_SUBTY_02' ##NO_TEXT.
    constants mc_exc_filter_subty_01 type pyd_par_type value 'Z99_EXC_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_exc_filter_subty_02 type pyd_par_type value 'Z99_EXC_FILTER_SUBTY_02' ##NO_TEXT.
    constants mc_z99_lgart_ind type pyd_par_type value 'Z99_LGART_IND' ##NO_TEXT.
    constants mc_condition_lgart type pyd_par_type value 'Z99_CONDITION_LGART' ##NO_TEXT.
    constants mc_comparison_operator type pyd_par_type value 'Z99_COMPARISON_OPERATOR' ##NO_TEXT.
    constants mc_missing type pyd_itemid value 'MISSING' ##NO_TEXT.
    constants mc_mismatch type pyd_itemid value 'MISMATCH' ##NO_TEXT.
    data mt_filter_subty_01 type /iwbep/t_cod_select_options .
    data mt_filter_subty_02 type /iwbep/t_cod_select_options .
    data mt_condition_lgart type /iwbep/t_cod_select_options .
    data mv_comparison_operator type zhrau_de_checkop .
    data mt_it2010_wo_time type tty_it2010_wo_time .
    data ms_it2010_wo_time type ty_it2010_wo_time .
    data ms_allowerr_dtls type ty_allowerr_dtls .
    data mv_z99_lgart_ind type boolean .

    methods read_time_data
      importing
        !iv_pernr           type pernr_d
        !iv_begda           type begda
        !iv_endda           type endda
      exporting
        value(et_time_data) type tty_time_data .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_IT2010_WO_TIME_PER IMPLEMENTATION.


  method check.
* All Relevant IT 2010 Wage types
    data: lt_all_subty  type /iwbep/t_cod_select_options,
          lt_it2010     type standard table of ty_time_data with non-unique key table_line,
          lt_it2001     type standard table of ty_time_data with non-unique key table_line,
          lt_it2002     type standard table of ty_time_data with non-unique key table_line,
          lt_cond_data  type standard table of ty_time_data with non-unique key table_line,

          lt_allow_data type tty_period_data,
          lt_time_data  type tty_period_data,
          ls_allow_data type ty_period_data,
          ls_time_data  type ty_period_data.

    data: lt_it2010_wo_time type tty_it2010_wo_time,
          ls_it2010_wo_time type ty_it2010_wo_time.
    data: lv_comparison type boolean.
    data: ls_result  type   ty_s_result.

* All relevant employees with IT2010 Records
    select it2010~pernr, sum( it2010~anzhl ) as anzhl
      into corresponding fields of table @lt_it2010 from pa2010 as it2010
       where it2010~pernr in @it_pernr_so
         and it2010~subty in @mt_filter_subty_01
         and it2010~sprps = ' '
         and it2010~uname in @mt_uname
         and it2010~begda <= @mv_endda
         and it2010~endda >= @mv_begda
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
                         and it0001~kostl in @mt_kostl )
      group by it2010~pernr.

* Read IT 2001 and IT 2002
    if not lt_it2010 is initial.
* Prepare Subtype List
      refresh lt_all_subty.
      append lines of mt_condition_lgart to lt_all_subty.
      append lines of mt_filter_subty_02 to lt_all_subty.
                                                            "IT 2001
      refresh: lt_it2001.
      select it2001~pernr as pernr, it2001~subty as subty,
             it2001~begda as begda, it2001~endda as endda, it2001~seqnr as seqnr, it2001~stdaz as anzhl
         into corresponding fields of table @lt_it2001 from pa2001 as it2001
          for all entries in @lt_it2010
       where it2001~pernr = @lt_it2010-pernr
         and it2001~subty in @lt_all_subty
         and it2001~sprps = ' '
         and it2001~endda >= @mv_begda
         and it2001~begda <= @mv_endda.

* Prepare IT 2001 records for the check
      sort lt_it2001 by pernr begda endda subty.
      loop at lt_it2001 into data(ls_it2001).
        if ls_it2001-subty in mt_condition_lgart.
          append ls_it2001 to lt_cond_data.
        endif.
        if ls_it2001-subty in mt_filter_subty_02.
          move-corresponding ls_it2001 to ls_time_data.
          collect ls_time_data into lt_time_data.
        endif.
      endloop.
                                                            "IT 2002
      refresh: lt_it2002.
      select it2002~pernr as pernr, it2002~subty as subty,
             it2002~begda as begda, it2002~endda as endda, it2002~seqnr as seqnr, it2002~stdaz as anzhl
         into corresponding fields of table @lt_it2002 from pa2002 as it2002
          for all entries in @lt_it2010
       where it2002~pernr = @lt_it2010-pernr
         and it2002~subty in @lt_all_subty
         and it2002~sprps = ' '
         and it2002~endda >= @mv_begda
         and it2002~begda <= @mv_endda.

* Prepare IT 2001 records for the check
      sort lt_it2002 by pernr begda endda subty.
      loop at lt_it2002 into data(ls_it2002).
        if ls_it2002-subty in mt_condition_lgart.
          append ls_it2002 to lt_cond_data.
        endif.
        if ls_it2002-subty in mt_filter_subty_02.
          move-corresponding ls_it2002 to ls_time_data.
          collect ls_time_data into lt_time_data.
        endif.
      endloop.
      refresh: lt_it2001, lt_it2002.

      sort lt_time_data by pernr.
    endif.

* Prepare Final IT 2010 records for the check
    sort lt_it2010 by pernr.
    loop at lt_it2010 into data(ls_it2010).
      move-corresponding ls_it2010 to ls_allow_data.
      collect ls_allow_data into lt_allow_data.
    endloop.
    refresh: lt_it2010.

* Prepare data for result
    sort lt_cond_data by pernr subty begda endda.
    delete adjacent duplicates from lt_cond_data comparing pernr.

    loop at lt_cond_data into data(ls_cond_data).
      loop at lt_allow_data into ls_allow_data
          where pernr = ls_cond_data-pernr.
        read table lt_time_data into ls_time_data
               with key pernr = ls_allow_data-pernr binary search.
        if sy-subrc ne 0.
* Collect data for Overview List
          clear: ls_it2010_wo_time.
          move: ls_allow_data-pernr to ls_it2010_wo_time-pernr,
                mc_missing          to ls_it2010_wo_time-error,
                ls_allow_data-anzhl to ls_it2010_wo_time-allow_anzhl.
          append ls_it2010_wo_time to lt_it2010_wo_time.
        else.
          clear lv_comparison.
          call method zcl_m99_pcc_chk_fp4_base=>dynamic_check_operation
            exporting
              iv_opcode = mv_comparison_operator
              iv_var01  = ls_allow_data-anzhl
              iv_var02  = ls_time_data-anzhl
            receiving
              rv_result = lv_comparison.

          if lv_comparison eq abap_true.
            clear: ls_it2010_wo_time.
            move: ls_allow_data-pernr to ls_it2010_wo_time-pernr,
                  mc_mismatch         to ls_it2010_wo_time-error,
                  ls_allow_data-anzhl to ls_it2010_wo_time-allow_anzhl,
                  ls_time_data-anzhl  to ls_it2010_wo_time-time_anzhl.
            append ls_it2010_wo_time to lt_it2010_wo_time.
          endif.
        endif.

      endloop.
    endloop.

* Collect the data for Overview
    append lines of lt_it2010_wo_time to mt_it2010_wo_time.
    sort mt_it2010_wo_time by pernr error.
    delete adjacent duplicates from mt_it2010_wo_time comparing all fields.

* Delete Duplicate Records for Employee for result preparation
    sort lt_it2010_wo_time by pernr ascending.
    delete adjacent duplicates from lt_it2010_wo_time comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it2010_wo_time into ls_it2010_wo_time.
* IT2010 Exists on the day with out IT2001 and IT2002
      ls_result-id = ls_it2010_wo_time-pernr.
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
    data: lv_al_begda(10) type c,
          lv_ab_begda(10) type c,
          lv_ab_endda(10) type c.

    data: lv_char_value type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_al_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    data lt_time_data type standard table of ty_time_data.
    data: lv_timedata_anzhl type enanz,
          lv_timedata_text  type pyd_name,
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

* Generic Message
          clear ls_err_ov-sfo_tab.
          lv_al_date_txt = text-001.
          lv_text = text-002.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_allowerr
              iv_text                     = lv_al_date_txt
              iv_value                    = lv_text
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_allowerr_dtls.

            move-corresponding ms_allowerr_dtls to ms_it2010_wo_time.
            move lv_pernr to ms_it2010_wo_time-pernr.

* Sum Of Allowance Hours
            clear ls_err_ov-sfo_tab.
            lv_al_date_txt = text-005.
            lv_text = ms_it2010_wo_time-allow_anzhl.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_allowerr
                iv_text                     = lv_al_date_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Sum Of Time data Hours
            lv_al_date_txt = text-006.
            lv_text = ms_it2010_wo_time-time_anzhl.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_allowerr
                iv_text                     = lv_al_date_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Read Employee Allowance and Time data for the period
            refresh: lt_time_data.
            call method me->read_time_data
              exporting
                iv_pernr     = lv_pernr
                iv_begda     = mv_begda
                iv_endda     = mv_endda
              importing
                et_time_data = lt_time_data.

            loop at lt_time_data into data(ls_time_data).
              call function 'HR_GET_SUBTYPE_TEXT'
                exporting
                  infty               = ls_time_data-infty
                  subty               = ls_time_data-subty
                  persnr              = lv_pernr
                  begda               = ls_time_data-begda
                  endda               = ls_time_data-endda
*                 molga               = mc_molga_au           "MOD001--
                  molga               = mv_molga              "MOD001++
                importing
                  stext               = lv_stext
                exceptions
                  infty_not_found     = 1
                  subty_not_found     = 2
                  infty_not_supported = 3
                  others              = 4.

              write: ls_time_data-begda to lv_al_begda dd/mm/yyyy.

              concatenate lv_stext '(' ls_time_data-subty ')' into lv_timedata_text.
              message i015(zhrpy_pcc_msg)
                 with ls_time_data-infty lv_timedata_text ls_time_data-anzhl into lv_text.

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
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_it2010_wo_time into ms_it2010_wo_time
            where pernr = lv_pernr.

            move-corresponding ms_it2010_wo_time to ms_allowerr_dtls.
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


  method get_specifc_custmizing.
* Read Check Specific Parameters
    data: lt_param_so type /iwbep/t_cod_select_options,
          ls_par      type if_pyd_fnd_types=>ty_s_resp.

    try.
* Read IT 2010 Subtypes
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
        loop at mo_context->mt_par into ls_par where par_type = me->mc_condition_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_condition_lgart.
        endloop.

* Comparison Oparator
        mv_comparison_operator =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_comparison_operator
                                                                        it_par      = mo_context->mt_par ).
        if mv_comparison_operator is initial.
          move 'EQ' to mv_comparison_operator.
        endif.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.


  method read_time_data.
* Read Time Data from IT 2001, 2002 and 2010
    data: lt_all_subty  type /iwbep/t_cod_select_options.
    data: lt_it2010    type standard table of ty_time_data with non-unique key table_line,
          lt_it2001    type standard table of ty_time_data with non-unique key table_line,
          lt_it2002    type standard table of ty_time_data with non-unique key table_line,
          lt_time_data type standard table of ty_time_data with non-unique key table_line.
    data: lv_tabix type sy-tabix.

    refresh: lt_it2010.
    select it2010~pernr as pernr, it2010~subty as subty,
           it2010~begda as begda, it2010~endda as endda, it2010~seqnr as seqnr, it2010~anzhl as anzhl
       into corresponding fields of table @lt_it2010 from pa2010 as it2010
     where it2010~pernr = @iv_pernr
       and it2010~subty in @mt_filter_subty_01
       and it2010~sprps = ' '
       and it2010~endda >= @iv_begda
       and it2010~begda <= @iv_endda.
    loop at lt_it2010 into data(ls_it2010).
      lv_tabix = sy-tabix.
      move mc_it2010 to ls_it2010-infty.
      modify lt_it2010 index lv_tabix from ls_it2010 transporting infty.
    endloop.

* Subtype for Time Data
    refresh lt_all_subty.
    append lines of mt_condition_lgart to lt_all_subty.
    append lines of mt_filter_subty_02 to lt_all_subty.
                                                            "IT 2001
    refresh: lt_it2001.
    select it2001~pernr as pernr, it2001~subty as subty,
           it2001~begda as begda, it2001~endda as endda, it2001~seqnr as seqnr, it2001~stdaz as anzhl
       into corresponding fields of table @lt_it2001 from pa2001 as it2001
     where it2001~pernr = @iv_pernr
       and it2001~subty in @lt_all_subty
       and it2001~sprps = ' '
       and it2001~endda >= @iv_begda
       and it2001~begda <= @iv_endda.
    loop at lt_it2001 into data(ls_it2001).
      lv_tabix = sy-tabix.
      move mc_it2001 to ls_it2001-infty.
      modify lt_it2001 index lv_tabix from ls_it2001 transporting infty.
    endloop.

                                                            "IT 2002
    refresh: lt_it2002.
    select it2002~pernr as pernr, it2002~subty as subty,
           it2002~begda as begda, it2002~endda as endda, it2002~seqnr as seqnr, it2002~stdaz as anzhl
       into corresponding fields of table @lt_it2002 from pa2002 as it2002
     where it2002~pernr = @iv_pernr
       and it2002~subty in @lt_all_subty
       and it2002~sprps = ' '
       and it2002~endda >= @iv_begda
       and it2002~begda <= @iv_endda.
    loop at lt_it2002 into data(ls_it2002).
      lv_tabix = sy-tabix.
      move mc_it2002 to ls_it2002-infty.
      modify lt_it2002 index lv_tabix from ls_it2002 transporting infty.
    endloop.

    append lines of lt_it2010 to lt_time_data.
    append lines of lt_it2001 to lt_time_data.
    append lines of lt_it2002 to lt_time_data.
    refresh: lt_it2010, lt_it2001, lt_it2002.

    sort lt_time_data by pernr begda endda subty.
    et_time_data[] = lt_time_data[].

  endmethod.
ENDCLASS.
