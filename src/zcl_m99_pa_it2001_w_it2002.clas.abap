class zcl_m99_pa_it2001_w_it2002 definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.
  protected section.
    types: begin of ty_attenderr_dtl,
             ab_awart type lgart,
             ab_begda type begda,
             ab_endda type endda,
             at_awart type awart,
             at_begda type begda,
             at_endda type endda,
           end of ty_attenderr_dtl.
    types: begin of ty_it2001_w_it2002,
             pernr    type pernr_d,
             ab_text  type abwtxt,
             ab_awart type lgart,
             ab_begda type begda,
             ab_endda type endda,
             at_text  type abwtxt,
             at_awart type awart,
             at_begda type begda,
             at_endda type endda,
           end of ty_it2001_w_it2002.
    types tty_it2001_w_it2002 type table of ty_it2001_w_it2002.
    constants mc_itemid_attenderr type pyd_itemid value 'ATTENDERR'.
    constants mc_filter_subty_01 type pyd_par_type value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_filter_subty_02 type pyd_par_type value 'Z99_FILTER_SUBTY_02' ##NO_TEXT.
    constants mc_exc_filter_subty_01 type pyd_par_type value 'Z99_EXC_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_exc_filter_subty_02 type pyd_par_type value 'Z99_EXC_FILTER_SUBTY_02' ##NO_TEXT.
    constants mc_z99_anzhl type pyd_par_type value 'Z99_ANZHL' ##NO_TEXT.
    data mt_filter_subty_01 type /iwbep/t_cod_select_options .
    data mt_filter_subty_02 type /iwbep/t_cod_select_options .
    data mt_it2001_w_it2002 type tty_it2001_w_it2002.
    data ms_it2001_w_it2002 type ty_it2001_w_it2002.
    data ms_attenderr_dtl type ty_attenderr_dtl.
    data mv_z99_anzhl type pranz.

    methods check
        redefinition .
    methods get_specifc_custmizing
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_IT2001_W_IT2002 IMPLEMENTATION.


  method check.
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |27-FEB-2022 |1130848  |NZ validation logic using    |CFAK902802   *
*    |            |         |Z99_ANZHL                    |             *
*-----------------------------------------------------------------------*
* Read Check data
    types: begin of ty_it2001_data,
             pernr    type pernr_d,
             ab_awart type awart,
             ab_begda type begda,
             ab_endda type endda.
    types: end of ty_it2001_data.
    data: lt_it2001 type standard table of ty_it2001_data with non-unique key table_line.
    data: lt_it2001_01 type standard table of ty_it2001_data with non-unique key table_line.

    types: begin of ty_it2002_data,
             pernr    type pernr_d,
             at_awart type awart,
             at_begda type begda,
             at_endda type endda.
    types: end of ty_it2002_data.
    data: lt_it2002 type standard table of ty_it2002_data with non-unique key table_line.
    data: lt_it2002_01 type standard table of ty_it2002_data with non-unique key table_line.
    data: lt_it2001_w_it2002 type tty_it2001_w_it2002.
    data: ls_it2001_w_it2002 type ty_it2001_w_it2002.

    data: lv_index type sy-tabix,
          lv_subrc type sy-subrc.

    data: ls_result  type   ty_s_result.

* Use Changed IT2002 Approach to get retro changes as well
* All relevant employees with it2002 Records and IT2002 records
    select it2002~pernr,
           it2002~awart as at_awart, it2002~begda as at_begda, it2002~endda as at_endda
       into table @lt_it2002_01 from pa2002 as it2002
         where it2002~pernr in @it_pernr_so
          and  it2002~subty in @mt_filter_subty_02
           and it2002~sprps = ' '
           and it2002~uname in @mt_uname
*           and it2002~aedtm >= @mv_begda
*           and it2002~aedtm <= @mv_endda_plus1
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

* All relevant employees with IT2001 Records
    if not lt_it2002_01 is initial.
      select it2001~pernr,
             it2001~awart as ab_awart, it2001~begda as ab_begda, it2001~endda as ab_endda
        into corresponding fields of table @lt_it2001_01
        from pa2001 as it2001
        for all entries in @lt_it2002_01
       where it2001~pernr = @lt_it2002_01-pernr      and
             it2001~subty in @mt_filter_subty_01  and
             it2001~sprps in @mt_sprps            and
             it2001~endda >= @lt_it2002_01-at_begda  and
             it2001~begda <= @lt_it2002_01-at_endda.
    endif.
* Combine Both lists
    append lines of lt_it2001_01 to lt_it2001.
    append lines of lt_it2002_01 to lt_it2002.

    sort lt_it2001 by pernr ab_begda.
    delete adjacent duplicates from lt_it2001.

    sort lt_it2002 by pernr at_begda at_awart.
    delete adjacent duplicates from lt_it2002.

* Build the error list
    loop at lt_it2001 into data(ls_it2001).

      if not mv_z99_anzhl is initial.
        ls_it2001-ab_begda = ls_it2001-ab_begda + mv_z99_anzhl.

        check not  ls_it2001-ab_begda  > ls_it2001-ab_endda.
      endif.
* Index Looping for better performance
      clear: lv_index, lv_subrc.
      read table lt_it2002 into data(ls_it2002)
             with key pernr = ls_it2001-pernr.
      lv_index = sy-tabix.
      lv_subrc = sy-subrc.

      while lv_subrc eq 0.
* Check IT 2002 data
        if ls_it2002-at_begda <= ls_it2001-ab_endda and
           ls_it2002-at_endda >= ls_it2001-ab_begda.

* Collect data for Overview List
          move: ls_it2001-pernr to ls_it2001_w_it2002-pernr,
                ls_it2001-ab_awart to ls_it2001_w_it2002-ab_awart,
                ls_it2001-ab_begda to ls_it2001_w_it2002-ab_begda,
                ls_it2001-ab_endda to ls_it2001_w_it2002-ab_endda,
                ls_it2002-at_awart to ls_it2001_w_it2002-at_awart,
                ls_it2002-at_begda to ls_it2001_w_it2002-at_begda,
                ls_it2002-at_endda to ls_it2001_w_it2002-at_endda.
          append ls_it2001_w_it2002 to lt_it2001_w_it2002.
        endif.
* Check next record
        lv_index = lv_index + 1.
        read table lt_it2002 into ls_it2002  index lv_index.
        lv_subrc = sy-subrc.

        if not ls_it2002-pernr = ls_it2001-pernr.
* Exit the loop.
          lv_subrc = 99.
        endif.
      endwhile.

    endloop.

* Collect data for the Overview
    append lines of lt_it2001_w_it2002 to mt_it2001_w_it2002.

* Build result table
* Delete Duplicate Records for Employee for result preparation
    sort lt_it2001_w_it2002 by pernr ascending.
    delete adjacent duplicates from lt_it2001_w_it2002 comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it2001_w_it2002 into ls_it2001_w_it2002.
* IT2010 Exists on the day with out IT2001 and IT2002
      ls_result-id = ls_it2001_w_it2002-pernr.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.
  endmethod.


  method err_ov_get_list.
* Method for Overview list display
    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr,
      lv_value  type pyd_item_value.

    data: lv_text type pyd_name.
    data: lv_at_begda(10) type c,
          lv_ab_begda(10) type c,
          lv_ab_endda(10) type c.

    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_at_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

* Prepare SFO Data for Display
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_attenderr_dtl.

            move-corresponding ms_attenderr_dtl to ms_it2001_w_it2002.
* Read IT2001 Subtype text
            call method cl_pt_arq_customizing=>get_subtype_description
              exporting
                im_attabs_type   = ms_it2001_w_it2002-ab_awart
                im_pernr         = lv_pernr
                im_date          = ms_it2001_w_it2002-ab_begda
              importing
                ex_name          = ms_it2001_w_it2002-ab_text
              exceptions
                it0001_not_found = 1
                others           = 2.

* Read IT2001 Suibtype text
            call method cl_pt_arq_customizing=>get_subtype_description
              exporting
                im_attabs_type   = ms_it2001_w_it2002-at_awart
                im_pernr         = lv_pernr
                im_date          = ms_it2001_w_it2002-at_begda
              importing
                ex_name          = ms_it2001_w_it2002-at_text
              exceptions
                it0001_not_found = 1
                others           = 2.

            concatenate ms_it2001_w_it2002-ab_text '(' ms_it2001_w_it2002-ab_awart ')'
             into data(lv_ab_text).
            concatenate ms_it2001_w_it2002-at_text '(' ms_it2001_w_it2002-at_awart ')'
             into data(lv_at_text).
            write: ms_it2001_w_it2002-ab_begda to lv_ab_begda dd/mm/yyyy,
                   ms_it2001_w_it2002-ab_endda to lv_ab_endda dd/mm/yyyy,
                   ms_it2001_w_it2002-at_begda to lv_at_begda dd/mm/yyyy.

            concatenate lv_ab_begda mc_hyphen lv_ab_endda
                   into lv_text separated by space.

            message i007(zhrpy_pcc_msg)
             with lv_at_text lv_ab_text lv_text into lv_text.

            lv_at_date_txt = lv_at_begda.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_attenderr
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
* Populate SFO Tab for table
          loop at mt_it2001_w_it2002 into ms_it2001_w_it2002
            where pernr = lv_pernr.

            move-corresponding ms_it2001_w_it2002 to ms_attenderr_dtl.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_attenderr_dtl
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_attenderr.
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
    data lt_param_so  type /iwbep/t_cod_select_options.

    try.
* Read IT 2001 Subtypes
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_subty_01
            iv_exc_par_type  = me->mc_exc_filter_subty_01
          changing
            ct_parameter_tab = mt_filter_subty_01.

* Read IT 2002 Subtypes
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_subty_02
            iv_exc_par_type  = me->mc_exc_filter_subty_02
          changing
            ct_parameter_tab = mt_filter_subty_02.

* Read Z99_ANZHL for Absence Off set
        mv_z99_anzhl = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_z99_anzhl
                                                             it_par = me->get_context( )->mt_par ).
      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
