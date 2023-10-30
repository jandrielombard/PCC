class ZUSECL_M99_PA_IT0001_W_IT2006 definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_IT00_WITH_IT2006 type PYD_S_RDSFO_EXT-ITEMID value 'IT00_2006' ##NO_TEXT.
  constants MC_PARAM_MASSN type PYD_PAR_TYPE value 'Z99_MASSN' ##NO_TEXT.
  constants MC_PARAM_KTART type PYD_PAR_TYPE value 'Z99_KTART' ##NO_TEXT.
  constants MC_QUOTA_TYPE_ABSENCE type T556R-QTYPE value 'A' ##NO_TEXT.
  protected section.

    types:
      begin of ty_output_compact,
        begda type pa0000-begda,
        ktart type pa2006-ktart,
        anzhl type pa2006-anzhl,
      end of ty_output_compact,
      begin of ty_output,
        pernr type pa0000-pernr.
        include type ty_output_compact.
      types: end of ty_output .
    types:
      tty_output type standard table of ty_output with non-unique key pernr ktart .

    data mr_filter_massn type /iwbep/t_cod_select_options .
    data mr_filter_ktart type /iwbep/t_cod_select_options .
    data mt_output type tty_output .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT0001_W_IT2006 IMPLEMENTATION.


  method CHECK.
* check if team member still has balance in IT2006
    data: ls_result like line of rt_result.
    data: lt_output type  tty_output.

    select distinct it00~pernr, it00~begda, it2006~ktart, it2006~anzhl - it2006~kverb as anzhl
      into corresponding fields of table @lt_output
      from pa0000 as it00 left outer join pa2006 as it2006 on
        it00~pernr = it2006~pernr and
        it00~begda <= it2006~endda and
        it00~endda >= it2006~begda
      where it00~pernr   in @it_pernr_so     and
            it00~sprps   = @if_hrpa_read_infotype=>unlocked and
*            it00~aedtm   <= @mv_endda_plus1  and
*            it00~aedtm   >= @mv_begda        and
            it00~aedtm   >= @mv_change_begda and
            it00~stat2   in @mt_stat2        and
            it00~massn   in @mr_filter_massn and
            it2006~ktart in @mr_filter_ktart and
*            it2006~anzhl > 0                 and
            exists (
              select 1
              from pa0001 as it01
              where it01~pernr = it00~pernr  and
                    (
                      "Scenario: retro or future dated infotype record (outside pay period).
                      "because last modified date is used, this means that old/future record
                      "can be included as long as the last modified date
                      "is in current period. Thus, it01~begda <= it00~endda &
                      "it01~endda >= it00~begda cannot be used because
                      "period of IT00 does not intersect with current period.
                      (
*                        it00~aedtm between @mv_begda and @mv_endda_plus1 and
                         it00~aedtm >= @mv_change_begda and
                        (
                          ( it00~begda < @mv_begda   and it00~endda < @mv_begda ) or
                          ( it00~endda > @mv_endda   and it00~begda > @mv_endda )
                        )
                      )
                      or
                      (
                        "Scenario: infotype record within pay period.
                        "last modified date is used and IT00 period intersects with
                        "current period. it01~begda <= it00~endda &
                        "it01~endda >= it00~begda can be used to get more accurate
                        "result if there is a split in current period
*                        it00~aedtm between @mv_begda and @mv_endda_plus1 and
                        it00~aedtm >= @mv_change_begda   and
                        it00~begda <= @mv_endda        and
                        it00~endda >= @mv_begda        and
                        it01~begda <= it00~endda       and
                        it01~endda >= it00~begda
                      )
                    )                                  and
                    it01~begda <= @mv_endda            and
                    it01~endda >= @mv_begda            and
                    it01~sprps = @if_hrpa_read_infotype=>unlocked and
                    it01~abkrs in @mt_payroll_areas    and
                    it01~bukrs in @mt_bukrs            and
                    it01~werks in @mt_werks            and
                    it01~btrtl in @mt_btrtl            and
                    it01~persg in @mt_persg            and
                    it01~persk in @mt_persk            and
                    it01~kostl in @mt_kostl
              ).
    if lt_output is not initial.
* Collect Data for Overview
      delete lt_output where anzhl eq 0.
      append lines of lt_output to mt_output.

      sort lt_output. delete adjacent duplicates from lt_output comparing pernr.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_output into data(ls_output).
        ls_result-id = ls_output-pernr.
        insert ls_result into table rt_result.
      endloop.
    endif.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Display error message
    data:
      ls_err_ov         type ty_s_err_ov,
      ls_sfo            type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr          type p_pernr,
      lv_text           type text120,
      lv_value          type char060,
      lt_sfo_tab_temp   like ls_err_ov-sfo_tab,
      lv_value_string   type string,
      ls_output_compact type ty_output_compact,
      ls_t556b          type t556b.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          "move SFO_TAB records to temporary table
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.
          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            clear: ls_output_compact, lv_value, lv_value_string, ls_t556b.
            if ls_sfo_tab_temp-itemid = mc_itemid_it00_with_it2006.
              lv_value = ls_sfo_tab_temp-value.
              call method me->copy_structure_to_other
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ls_output_compact.

              call function 'HR_GET_QUOTA_CUSTOMIZING'
                exporting
                  pernum           = lv_pernr
                  quota_id         = ls_output_compact-ktart
                  quota_type       = mc_quota_type_absence
                importing
                  w556b            = ls_t556b
                exceptions
                  error_occurred   = 1
                  wrong_quota_type = 2
                  parameter_error  = 3
                  others           = 4.

              write: ls_output_compact-begda to lv_text.
              message i060 with ls_t556b-ktext ls_output_compact-ktart ls_output_compact-anzhl into lv_value_string.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_it00_with_it2006
                  iv_text                     = |{ lv_text }|
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.

            else.
              insert ls_sfo_tab_temp into table ls_err_ov-sfo_tab.
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
            clear: lv_value, ls_output_compact.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_it00_with_it2006.
            ls_output_compact = corresponding #( ls_output ).

            call method me->copy_structure_to_other
              exporting
                p_struct1 = ls_output_compact
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
            iv_inc_par_type  = me->mc_param_ktart
          changing
            ct_parameter_tab = mr_filter_ktart.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_param_massn
          changing
            ct_parameter_tab = mr_filter_massn.
      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.
ENDCLASS.
