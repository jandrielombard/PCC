class ZUSECL_M99_PA_IT0007_WWEEK definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_IT07 type PYD_S_RDSFO_EXT-ITEMID value 'IT0007' ##NO_TEXT.
  constants MC_PARAM_WWEEKS type PYD_PAR_TYPE value 'Z99_WWEEKS' ##NO_TEXT.
  constants MC_PARAM_EXC_WWEEKS type PYD_PAR_TYPE value 'Z99_EXC_WWEEKS' ##NO_TEXT.
  constants MC_PARAM_PERSA_TRFAR type PYD_PAR_TYPE value 'Z99_PERSA_TRFAR' ##NO_TEXT.
  constants MC_PARAM_EXC_PERSA_TRFAR type PYD_PAR_TYPE value 'Z99_EXC_PERSA_TRFAR' ##NO_TEXT.
  protected section.

    types:
      begin of ty_output_for_db,
        begda type pa0007-begda,
        wweek type pa0007-wweek,
      end of ty_output_for_db .
    types:
      begin of ty_output,
        pernr type pa0007-pernr.
        include type ty_output_for_db.
      types: end of ty_output .
    types:
      tty_output type standard table of ty_output .
    types:
      begin of ty_working_week_text,
        sprsl type t559b-sprsl,
        wweek type t559b-wweek,
        wwtxt type t559b-wwtxt,
      end of ty_working_week_text .
    types:
      tty_working_week_text type sorted table of ty_working_week_text with unique key sprsl wweek .

    data mt_filter_wweeks type /iwbep/t_cod_select_options .
    data mt_filter_persa_trfar type /iwbep/t_cod_select_options .
    data mt_output type tty_output .
    data mt_working_week_text type tty_working_week_text .

    methods get_working_week_text
      importing
        !iv_working_week            type t559b-wweek
        !iv_language                type t559b-sprsl default sy-langu
      returning
        value(rv_working_week_text) type t559b-wwtxt .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT0007_WWEEK IMPLEMENTATION.


  method CHECK.
*Checks working week on IT7
    data: lt_it                type tty_output,
          ls_result            like line of rt_result,
          lv_dynamic_condition type string.
*   Combination of Personnel Area & Pay Scale Area needs to be used together
*   so 'AND' is used to combine values from each combination
    loop at mt_filter_persa_trfar into data(ls_filter_persa_trfar) where low is not initial.
      if sy-tabix = 1.
        lv_dynamic_condition = '('.
        if ls_filter_persa_trfar-sign = if_dmf_constants_c=>gc_range_sign_exclusive.
          lv_dynamic_condition = |{ lv_dynamic_condition } NOT|.
        endif.
      else.
        lv_dynamic_condition = |{ lv_dynamic_condition } OR|.
        if ls_filter_persa_trfar-sign = if_dmf_constants_c=>gc_range_sign_exclusive.
          lv_dynamic_condition = |{ lv_dynamic_condition } NOT|.
        endif.
      endif.
      lv_dynamic_condition = |{ lv_dynamic_condition }| &&
          | ( IT01~WERKS = '{ ls_filter_persa_trfar-low(4) }'| &&
          | AND IT08~TRFAR = '{ ls_filter_persa_trfar-low+4(2) }' )|.
    endloop.
    if sy-subrc = 0.
      lv_dynamic_condition = |{ lv_dynamic_condition } )|.
    endif.

    select distinct it07~pernr, it07~begda, it07~wweek
      into table @lt_it
      from pa0007 as it07
      where it07~pernr in @it_pernr_so                    and
            it07~sprps = @if_hrpa_read_infotype=>unlocked and
*            it07~aedtm <= @mv_endda_plus1                 and
*            it07~aedtm >= @mv_begda                       and
            it07~aedtm >= @mv_change_begda                and
            it07~wweek in @mt_filter_wweeks               and
            exists ( select 1
              from pa0000 as it00
                inner join pa0001 as it01 on
                  it00~pernr = it01~pernr
              where it00~pernr = it07~pernr    and
                (
                  (
                    ( it07~begda < @mv_begda   and it07~endda < @mv_begda ) or
                    ( it07~endda > @mv_endda   and it07~begda > @mv_endda )
                  ) or
                  (
                    it00~begda <= it07~endda   and
                    it00~endda >= it07~begda   and
                    it01~begda <= it07~endda   and
                    it01~endda >= it07~begda
                  )
                ) and
                it00~begda <= @mv_endda                       and
                it00~endda >= @mv_begda                       and
                it01~begda <= @mv_endda                       and
                it01~endda >= @mv_begda                       and
                it00~stat2 in @mt_stat2                       and
                it00~sprps = @if_hrpa_read_infotype=>unlocked and
                it01~sprps = @if_hrpa_read_infotype=>unlocked and
                it01~abkrs in @mt_payroll_areas               and
                it01~bukrs in @mt_bukrs                       and
                it01~werks in @mt_werks                       and
                it01~btrtl in @mt_btrtl                       and
                it01~persg in @mt_persg                       and
                it01~persk in @mt_persk                       and
                it01~kostl in @mt_kostl
            ) and exists ( select 1
              from pa0001 as it01 inner join pa0008 as it08 on
                it01~pernr = it08~pernr and
                it08~begda <= it01~endda and
                it08~endda >= it01~begda
              where it01~pernr = it07~pernr and
                    it01~sprps = @if_hrpa_read_infotype=>unlocked and
                    it08~sprps = @if_hrpa_read_infotype=>unlocked and
                    it01~begda <= it07~endda and
                    it01~endda >= it07~begda and
                    it08~begda <= it07~endda and
                    it08~endda >= it07~begda and
                    (lv_dynamic_condition)
            )
    order by it07~pernr.
* Store the data for Overview List
    mt_output[] = lt_it[].

* Populate Result table
    if lt_it is not initial.
      delete adjacent duplicates from lt_it comparing pernr.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_it into data(ls_it).
        ls_result-id = ls_it-pernr.
        insert ls_result into table rt_result.
      endloop.
    endif.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Display error message
    data:
      ls_err_ov            type ty_s_err_ov,
      ls_sfo               type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr             type p_pernr,
      lv_text              type text120,
      lv_value             type char060,
      lt_sfo_tab_temp      like ls_err_ov-sfo_tab,
      lv_value_string      type string,
      ls_output_for_db     type ty_output_for_db,
      lv_working_week_text type t559b-wwtxt.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        loop at ct_err_ov into ls_err_ov.
          clear: lt_sfo_tab_temp.
          "move SFO_TAB records to temporary table
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.
          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            clear: ls_output_for_db, lv_value, lv_value_string, lv_working_week_text.
            if ls_sfo_tab_temp-itemid = mc_itemid_it07.
              lv_value = ls_sfo_tab_temp-value.
              call method me->copy_structure_to_other
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ls_output_for_db.

              lv_working_week_text = get_working_week_text( ls_output_for_db-wweek ).

              write: ls_output_for_db-begda to lv_text.
              message i090 with ls_output_for_db-wweek lv_working_week_text into lv_value_string.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_it07
                  iv_text                     = |{ lv_text }|
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.
            endif.
          endloop.
          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.
          loop at mt_output into data(ls_output) where pernr = lv_pernr.
            clear: lv_value, ls_output_for_db.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_it07.
            ls_output_for_db = corresponding #( ls_output ).

            call method me->copy_structure_to_other
              exporting
                p_struct1 = ls_output_for_db
              changing
                p_struct2 = lv_value.

            ls_sfo-value = lv_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.
          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Get parameters specific for this validation rule

    try.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_param_wweeks
            iv_exc_par_type  = me->mc_param_exc_wweeks
          changing
            ct_parameter_tab = mt_filter_wweeks.

        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_param_persa_trfar
            iv_exc_par_type  = me->mc_param_exc_persa_trfar
          changing
            ct_parameter_tab = mt_filter_persa_trfar.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method GET_WORKING_WEEK_TEXT.
    read table mt_working_week_text into data(ls_t559b) with table key sprsl = iv_language wweek = iv_working_week.
    if sy-subrc = 0.
      rv_working_week_text = ls_t559b-wwtxt.
    else.
      clear: ls_t559b.
      select single sprsl wweek wwtxt into ls_t559b
        from t559b
        where sprsl = iv_language
          and wweek = iv_working_week.
      if sy-subrc = 0.
        insert ls_t559b into table mt_working_week_text.
        rv_working_week_text = ls_t559b-wwtxt.
      endif.
    endif.
  endmethod.
ENDCLASS.
