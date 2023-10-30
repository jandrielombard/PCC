class ZUSECL_M99_PA_IT8_NEW definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_FILTER_LGART_01 type PYD_PAR_TYPE value 'Z99_FILTER_LGART_01' ##NO_TEXT.
  constants MC_ITEMID_IT08_PERIOD type PYD_S_RDSFO_EXT-ITEMID value 'IT08PERIOD' ##NO_TEXT.
PROTECTED SECTION.

  TYPES:
    BEGIN OF ty_it08_comp_wt_compact,
      begda TYPE pa0008-begda,
      endda TYPE pa0008-endda,
    END OF ty_it08_comp_wt_compact,
    BEGIN OF ty_it08_comp_wt,
      pernr TYPE pa0008-pernr.
      INCLUDE TYPE ty_it08_comp_wt_compact.
    TYPES: END OF ty_it08_comp_wt .
  TYPES:
    tty_it08_comp_wt TYPE STANDARD TABLE OF ty_it08_comp_wt .
  TYPES:
    BEGIN OF ty_t549a,
      abkrs TYPE t549a-abkrs,
      permo TYPE t549a-permo,
    END OF ty_t549a .
  TYPES:
    tty_t549a TYPE HASHED TABLE OF ty_t549a WITH UNIQUE KEY abkrs .
  TYPES:
    BEGIN OF ty_lgart,
      lgart     TYPE pa0008-lga01,
      begda_min TYPE pa0008-begda,
      endda_max TYPE pa0008-endda,
    END OF ty_lgart .
  TYPES:
    tty_lgart TYPE HASHED TABLE OF ty_lgart WITH UNIQUE KEY lgart .
  TYPES:
    BEGIN OF ty_data.
      INCLUDE TYPE zusecl_m99_pa_it0008_comp_wt=>ty_it08.
      TYPES:  begda_01 TYPE pa0001-begda,
      endda_01 TYPE pa0001-endda,
      abkrs_01 TYPE pa0001-abkrs,
    END OF ty_data .

  DATA mt_filter_lgart_01 TYPE /iwbep/t_cod_select_options .
  DATA mt_t549a TYPE tty_t549a .
  DATA mt_output TYPE tty_it08_comp_wt .

  METHODS adjust_pay_period_by_status
    IMPORTING
      !iv_pernr                TYPE pernr_d
      !iv_pay_period_begda     TYPE begda
      !iv_pay_period_endda     TYPE endda
    EXPORTING
      !ev_pay_period_begda_new TYPE begda
      !ev_pay_period_endda_new TYPE endda
      !ev_is_error             TYPE abap_bool .
  METHODS get_wt_to_check
    IMPORTING
      !is_data             TYPE ty_data
      !it_filter_lgart     TYPE /iwbep/t_cod_select_options
    RETURNING
      VALUE(rt_wage_types) TYPE zusecl_m99_pa_it0008_comp_wt=>tty_lgart .
  METHODS get_it08_in_period_and_check
    IMPORTING
      !iv_pernr              TYPE pernr_d
      !iv_pay_period_begda   TYPE begda
      !iv_pay_period_endda   TYPE endda
      !it_wage_type_to_check TYPE zusecl_m99_pa_it0008_comp_wt=>tty_lgart
    RETURNING
      VALUE(rv_is_error)     TYPE abap_bool .
  METHODS get_pay_period_begda_endda
    IMPORTING
      !iv_abkrs        TYPE abkrs
      !iv_it07_begda   TYPE begda
      !iv_it07_endda   TYPE endda
    EXPORTING
      !ev_period_begda TYPE begda
      !ev_period_endda TYPE endda .

  METHODS check
      REDEFINITION .
  METHODS err_ov_get_list
      REDEFINITION .
  METHODS get_specifc_custmizing
      REDEFINITION .
private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT8_NEW IMPLEMENTATION.


  METHOD ADJUST_PAY_PERIOD_BY_STATUS.
* adjust pay period to period when the team member is still active.
    TYPES: BEGIN OF ty_it00_stat,
             begda TYPE pa0000-begda,
             endda TYPE pa0000-endda,
             stat2 TYPE pa0000-stat2,
           END OF ty_it00_stat.
    DATA: lt_it00_stat            TYPE SORTED TABLE OF ty_it00_stat WITH UNIQUE KEY begda endda,
          ls_it00_stat            LIKE LINE OF lt_it00_stat,
          ls_it00_stat_next_hire  LIKE LINE OF lt_it00_stat,
          lv_pay_period_begda_new LIKE iv_pay_period_begda,
          lv_pay_period_endda_new LIKE iv_pay_period_endda,
          lv_tabix                TYPE sy-tabix.
    CLEAR: ev_pay_period_begda_new,
           ev_pay_period_endda_new,
           ev_is_error.

*   check whether employee is active on iv_pay_period_begda and iv_pay_period_endda.
    SELECT endda begda stat2
      INTO CORRESPONDING FIELDS OF TABLE lt_it00_stat
      FROM pa0000
      WHERE pernr = iv_pernr
        AND endda >= iv_pay_period_begda
    ORDER BY endda begda.
    IF sy-subrc = 0.
      LOOP AT lt_it00_stat INTO ls_it00_stat WHERE
        begda <= iv_pay_period_begda AND
        endda >= iv_pay_period_begda.
        IF ls_it00_stat-stat2 = mc_stat2_active.
          ev_pay_period_begda_new = iv_pay_period_begda.
        ELSE.
          "employee is NOT active at beginning of the pay period.
          "So find the hire date
          LOOP AT lt_it00_stat INTO ls_it00_stat_next_hire WHERE
            begda >= ls_it00_stat-endda AND
            endda >= ls_it00_stat-endda AND
            stat2 = mc_stat2_active.
            ev_pay_period_begda_new = ls_it00_stat_next_hire-begda.
            EXIT.
          ENDLOOP.
          IF ev_pay_period_begda_new IS INITIAL.
            ev_is_error = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        READ TABLE lt_it00_stat INTO ls_it00_stat WITH KEY stat2 = mc_stat2_active.
        IF sy-subrc = 0.
          ev_pay_period_begda_new = ls_it00_stat-begda.
        ENDIF.
      ENDIF.

      LOOP AT lt_it00_stat INTO ls_it00_stat WHERE
        begda <= iv_pay_period_endda AND
        endda >= iv_pay_period_endda.
        lv_tabix = sy-tabix.
        IF ls_it00_stat-stat2 = mc_stat2_active.
          ev_pay_period_endda_new = iv_pay_period_endda.
        ELSE.
          "employee is NOT active at end of the pay period.
          "So find the closest active period
          lv_tabix = lv_tabix - 1.
          IF lv_tabix <= 0.
            ev_is_error = abap_true.
            EXIT.
          ENDIF.
          DO lv_tabix TIMES.
            READ TABLE lt_it00_stat INTO ls_it00_stat_next_hire INDEX lv_tabix.
            IF ls_it00_stat_next_hire-stat2 = mc_stat2_active.
              ev_pay_period_endda_new = ls_it00_stat_next_hire-endda.
              EXIT.
            ENDIF.
            lv_tabix = lv_tabix - 1.
            IF lv_tabix <= 0.
              ev_is_error = abap_true.
              EXIT.
            ENDIF.
          ENDDO.
        ENDIF.
        EXIT.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  method CHECK.

    data: ls_output       like line of mt_output,
          lt_data         type sorted table of ty_data
            with unique key pernr subty objps endda begda seqnr begda_01 endda_01 abkrs_01,
          ls_result       like line of rt_result,
          lt_t549q        type standard table of t549q,
          lv_is_error     type abap_bool,
          ls_period_begda type d,
          ls_period_endda type d,
          ls_it08_only    type zusecl_m99_pa_it0008_comp_wt=>ty_it08,
          lt_wt_to_check  type zusecl_m99_pa_it0008_comp_wt=>tty_lgart.

    select it08~pernr, it08~subty, it08~objps,
            it08~endda, it08~begda,  it08~seqnr,
            it08~lga01, it08~bet01, it08~ein01, it08~lga02, it08~bet02, it08~ein02,
            it08~lga03, it08~bet03, it08~ein03, it08~lga04, it08~bet04, it08~ein04,
            it08~lga05, it08~bet05, it08~ein05, it08~lga06, it08~bet06, it08~ein06,
            it08~lga07, it08~bet07, it08~ein07, it08~lga08, it08~bet08, it08~ein08,
            it08~lga09, it08~bet09, it08~ein09, it08~lga10, it08~bet10, it08~ein10,
            it08~lga11, it08~bet11, it08~ein11, it08~lga12, it08~bet12, it08~ein12,
            it08~lga13, it08~bet13, it08~ein13, it08~lga14, it08~bet14, it08~ein14,
            it08~lga15, it08~bet15, it08~ein15, it08~lga16, it08~bet16, it08~ein16,
            it08~lga17, it08~bet17, it08~ein17, it08~lga18, it08~bet18, it08~ein18,
            it08~lga19, it08~bet19, it08~ein19, it08~lga20, it08~bet20, it08~ein20,
            it08~lga21, it08~bet21, it08~ein21, it08~lga22, it08~bet22, it08~ein22,
            it08~lga23, it08~bet23, it08~ein23, it08~lga24, it08~bet24, it08~ein24,
            it08~lga25, it08~bet25, it08~ein25, it08~lga26, it08~bet26, it08~ein26,
            it08~lga27, it08~bet27, it08~ein27, it08~lga28, it08~bet28, it08~ein28,
            it08~lga29, it08~bet29, it08~ein29, it08~lga30, it08~bet30, it08~ein30,
            it08~lga31, it08~bet31, it08~ein31, it08~lga32, it08~bet32, it08~ein32,
            it08~lga33, it08~bet33, it08~ein33, it08~lga34, it08~bet34, it08~ein34,
            it08~lga35, it08~bet35, it08~ein35, it08~lga36, it08~bet36, it08~ein36,
            it08~lga37, it08~bet37, it08~ein37, it08~lga38, it08~bet38, it08~ein38,
            it08~lga39, it08~bet39, it08~ein39, it08~lga40, it08~bet40, it08~ein40,
            it01~begda as begda_01, it01~endda as endda_01, it01~abkrs as abkrs_01
          into corresponding fields of table @lt_data
          from pa0008 as it08 left outer join pa0001 as it01 on
            it08~pernr = it01~pernr and
            it08~begda <= it01~endda and
            it08~endda >= it01~begda
          where it08~pernr in @it_pernr_so    and
*                it08~aedtm >= @mv_begda       AND
*                it08~aedtm <= @mv_endda_plus1 AND
                it08~aedtm >= @mv_change_begda and
                it08~sprps = @if_hrpa_read_infotype=>unlocked and
                ( it08~lga01 in @mt_filter_lgart_01 or it08~lga02 in @mt_filter_lgart_01 or
                  it08~lga03 in @mt_filter_lgart_01 or it08~lga04 in @mt_filter_lgart_01 or
                  it08~lga05 in @mt_filter_lgart_01 or it08~lga06 in @mt_filter_lgart_01 or
                  it08~lga07 in @mt_filter_lgart_01 or it08~lga08 in @mt_filter_lgart_01 or
                  it08~lga09 in @mt_filter_lgart_01 or it08~lga10 in @mt_filter_lgart_01 or
                  it08~lga11 in @mt_filter_lgart_01 or it08~lga12 in @mt_filter_lgart_01 or
                  it08~lga13 in @mt_filter_lgart_01 or it08~lga14 in @mt_filter_lgart_01 or
                  it08~lga15 in @mt_filter_lgart_01 or it08~lga16 in @mt_filter_lgart_01 or
                  it08~lga17 in @mt_filter_lgart_01 or it08~lga18 in @mt_filter_lgart_01 or
                  it08~lga19 in @mt_filter_lgart_01 or it08~lga20 in @mt_filter_lgart_01 or
                  it08~lga21 in @mt_filter_lgart_01 or it08~lga22 in @mt_filter_lgart_01 or
                  it08~lga23 in @mt_filter_lgart_01 or it08~lga24 in @mt_filter_lgart_01 or
                  it08~lga25 in @mt_filter_lgart_01 or it08~lga26 in @mt_filter_lgart_01 or
                  it08~lga27 in @mt_filter_lgart_01 or it08~lga28 in @mt_filter_lgart_01 or
                  it08~lga29 in @mt_filter_lgart_01 or it08~lga30 in @mt_filter_lgart_01 or
                  it08~lga31 in @mt_filter_lgart_01 or it08~lga32 in @mt_filter_lgart_01 or
                  it08~lga33 in @mt_filter_lgart_01 or it08~lga34 in @mt_filter_lgart_01 or
                  it08~lga35 in @mt_filter_lgart_01 or it08~lga36 in @mt_filter_lgart_01 or
                  it08~lga37 in @mt_filter_lgart_01 or it08~lga38 in @mt_filter_lgart_01 or
                  it08~lga39 in @mt_filter_lgart_01 or it08~lga40 in @mt_filter_lgart_01 ) and
                exists ( select 1
                        from pa0000 as it00 inner join pa0001 as it01 on
                          it00~pernr = it01~pernr
                        where it00~pernr = it08~pernr            and
                              (
                                "Scenario: retro or future dated infotype record (outside pay period).
                                "because last modified date is used, this means that old/future record
                                "can be included as long as the last modified date
                                "is in current period. Thus, it00~begda <= it08~endda &
                                "it00~endda >= it08~begda cannot be used because
                                "period of IT08 does not intersect with current period.
                                (
*                                  it08~aedtm between @mv_begda and @mv_endda_plus1 and
                                   it08~aedtm >= @mv_change_begda and

                                  (
                                    ( it08~begda < @mv_begda   and it08~endda < @mv_begda ) or
                                    ( it08~endda > @mv_endda   and it08~begda > @mv_endda )
                                  )
                                )
                                or
                                (
                                  "Scenario: infotype record within pay period.
                                  "last modified date is used and IT08 period intersects with
                                  "current period. it00~begda <= it08~endda &
                                  "it00~endda >= it08~begda can be used to get more accurate
                                  "result if there is a split in current period
*                                  it08~aedtm between @mv_begda and @mv_endda_plus1 and
                                  it08~aedtm >= @mv_change_begda and
                                  it08~begda <= @mv_endda      and
                                  it08~endda >= @mv_begda      and
                                  it00~begda <= it08~endda     and
                                  it00~endda >= it08~begda     and
                                  it01~begda <= it08~endda     and
                                  it01~endda >= it08~begda
                                )
                              )                                  and
                              it00~begda <= @mv_endda            and
                              it00~endda >= @mv_begda            and
                              it01~begda <= @mv_endda            and
                              it01~endda >= @mv_begda            and
                              it00~stat2 in @mt_stat2            and
                              it00~sprps = @if_hrpa_read_infotype=>unlocked and
                              it01~sprps = @if_hrpa_read_infotype=>unlocked and
                              it01~abkrs in @mt_payroll_areas    and
                              it01~bukrs in @mt_bukrs            and
                              it01~werks in @mt_werks            and
                              it01~btrtl in @mt_btrtl            and
                              it01~persg in @mt_persg            and
                              it01~persk in @mt_persk            and
                              it01~kostl in @mt_kostl )
    order by it08~pernr, it08~subty, it08~objps,
             it08~endda, it08~begda, it08~seqnr,
             it01~endda, it01~begda.

    loop at lt_data into data(ls_data).
      clear: lv_is_error,
             ls_period_begda,
             ls_period_endda,
             ls_it08_only,
             lt_wt_to_check.

      lt_wt_to_check = get_wt_to_check(
        exporting
          is_data = ls_data
          it_filter_lgart = mt_filter_lgart_01  ).

*     IT08 record might span multiple payroll periods so
*     get earliest start date of 1st period
*     get latest end date of last period
      get_pay_period_begda_endda(
        exporting
          iv_abkrs      = ls_data-abkrs_01
          iv_it07_begda = ls_data-begda
          iv_it07_endda = ls_data-endda
        importing
          ev_period_begda = ls_period_begda
          ev_period_endda = ls_period_endda ).

*     we need to get all IT08 within the pay periods because
*     if there is a split within the period and only the
*     first split has been modified, at this moment, 2nd split
*     is not yet obtained and same WT could exist in the 2nd split.
      lv_is_error = get_it08_in_period_and_check(
        iv_pernr = ls_data-pernr
        iv_pay_period_begda = ls_period_begda
        iv_pay_period_endda = ls_period_endda
        it_wage_type_to_check = lt_wt_to_check ).

      if lv_is_error = abap_true.
        "generate error.
        read table rt_result transporting no fields with key
          par_type = if_pyd_cont_types=>gcs_par_type-pernr
          id = ls_data-pernr.
        if sy-subrc <> 0.
          clear: ls_result.
          ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
          ls_result-id = ls_data-pernr.
          insert ls_result into table rt_result.
        endif.

        read table mt_output transporting no fields with key
          pernr = ls_data-pernr
          begda = ls_data-begda
          endda = ls_data-endda.
        if sy-subrc <> 0.
          clear: ls_output.
          ls_output-pernr = ls_data-pernr.
          ls_output-begda = ls_data-begda.
          ls_output-endda = ls_data-endda.
          insert ls_output into table mt_output.
          write:/ ls_data-pernr, ls_data-begda, ls_data-endda.
        endif.
      endif.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.

    data:
      ls_err_ov         type ty_s_err_ov,
      ls_sfo            type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr          type p_pernr,
      lv_value          type char060,
      ls_output_compact type ty_it08_comp_wt_compact,
      lt_sfo_tab_temp   like ls_err_ov-sfo_tab,
      lv_value_string   type string.


    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.
        loop at ct_err_ov into ls_err_ov.
          "move SFO_TAB records to temporary table
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.
          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            clear: ls_output_compact, lv_value, lv_value_string.
            if ls_sfo_tab_temp-itemid = mc_itemid_it08_period.
              lv_value = ls_sfo_tab_temp-value.
              call method me->copy_structure_to_other
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ls_output_compact.

              message i045 with ls_output_compact-begda ls_output_compact-endda into lv_value_string.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_it08_period
                  iv_text                     = |{ text-001 }|
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
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_output into data(ls_output) where pernr = lv_pernr.
            clear: lv_value.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_it08_period.
            move-corresponding ls_output to ls_output_compact.

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


  METHOD GET_IT08_IN_PERIOD_AND_CHECK.

*check if wage types in param IT_WAGE_TYPE_TO_CHECK have to start
*at start of pay period (lv_pay_period_begda_new)
*and end after end of pay period (lv_pay_period_endda_new)

    DATA: lt_data TYPE SORTED TABLE OF zusecl_m99_pa_it0008_comp_wt=>ty_it08 WITH UNIQUE KEY pernr subty objps endda begda seqnr,
          ls_data LIKE LINE OF lt_data.
    DATA: lv_is_gap_found         TYPE abap_bool,
          lv_is_error             TYPE abap_bool,
          lv_field_wt_name        TYPE string,
          lv_field_no(2)          TYPE n,
          lv_next_begda           TYPE d,
          lt_lgart                TYPE tty_lgart,
          ls_lgart                LIKE LINE OF lt_lgart,
          lr_wage_type_to_check   TYPE RANGE OF pa0008-lga01,
          lv_pay_period_begda_new LIKE iv_pay_period_begda,
          lv_pay_period_endda_new LIKE iv_pay_period_endda.
    FIELD-SYMBOLS: <fs_wt> TYPE pa0008-lga01.

    IF iv_pay_period_endda < iv_pay_period_begda.
      rv_is_error = abap_true.
      RETURN.
    ENDIF.

*   if the employee is hired mid pay period, this method will adjust
*   the start date to the hiring date. The same scenario if
*   the employee has become inactive, the end date is adjusted
*   to the end of the active period
    adjust_pay_period_by_status(
      EXPORTING
        iv_pernr = iv_pernr
        iv_pay_period_begda = iv_pay_period_begda
        iv_pay_period_endda = iv_pay_period_endda
      IMPORTING
        ev_pay_period_begda_new = lv_pay_period_begda_new
        ev_pay_period_endda_new = lv_pay_period_endda_new
        ev_is_error = rv_is_error ).

    IF rv_is_error = abap_true.
      RETURN.
    ENDIF.

    lr_wage_type_to_check = VALUE #( FOR ls_wage_type IN it_wage_type_to_check
                                      ( sign = if_dmf_constants_c=>gc_range_sign_inclusive
                                        option = if_dmf_constants_c=>gc_range_option_equal
                                        low = ls_wage_type ) ).

    SELECT it08~pernr, it08~subty, it08~objps,
           it08~endda, it08~begda,  it08~seqnr,
           it08~lga01, it08~bet01, it08~ein01, it08~lga02, it08~bet02, it08~ein02,
           it08~lga03, it08~bet03, it08~ein03, it08~lga04, it08~bet04, it08~ein04,
           it08~lga05, it08~bet05, it08~ein05, it08~lga06, it08~bet06, it08~ein06,
           it08~lga07, it08~bet07, it08~ein07, it08~lga08, it08~bet08, it08~ein08,
           it08~lga09, it08~bet09, it08~ein09, it08~lga10, it08~bet10, it08~ein10,
           it08~lga11, it08~bet11, it08~ein11, it08~lga12, it08~bet12, it08~ein12,
           it08~lga13, it08~bet13, it08~ein13, it08~lga14, it08~bet14, it08~ein14,
           it08~lga15, it08~bet15, it08~ein15, it08~lga16, it08~bet16, it08~ein16,
           it08~lga17, it08~bet17, it08~ein17, it08~lga18, it08~bet18, it08~ein18,
           it08~lga19, it08~bet19, it08~ein19, it08~lga20, it08~bet20, it08~ein20,
           it08~lga21, it08~bet21, it08~ein21, it08~lga22, it08~bet22, it08~ein22,
           it08~lga23, it08~bet23, it08~ein23, it08~lga24, it08~bet24, it08~ein24,
           it08~lga25, it08~bet25, it08~ein25, it08~lga26, it08~bet26, it08~ein26,
           it08~lga27, it08~bet27, it08~ein27, it08~lga28, it08~bet28, it08~ein28,
           it08~lga29, it08~bet29, it08~ein29, it08~lga30, it08~bet30, it08~ein30,
           it08~lga31, it08~bet31, it08~ein31, it08~lga32, it08~bet32, it08~ein32,
           it08~lga33, it08~bet33, it08~ein33, it08~lga34, it08~bet34, it08~ein34,
           it08~lga35, it08~bet35, it08~ein35, it08~lga36, it08~bet36, it08~ein36,
           it08~lga37, it08~bet37, it08~ein37, it08~lga38, it08~bet38, it08~ein38,
           it08~lga39, it08~bet39, it08~ein39, it08~lga40, it08~bet40, it08~ein40
         INTO CORRESPONDING FIELDS OF TABLE @lt_data
         FROM pa0008 AS it08
         WHERE it08~pernr = @iv_pernr AND
               it08~sprps = @if_hrpa_read_infotype=>unlocked AND
               it08~begda <= @lv_pay_period_endda_new AND
               it08~endda >= @lv_pay_period_begda_new AND
               ( it08~lga01 IN @lr_wage_type_to_check OR it08~lga02 IN @lr_wage_type_to_check OR
                 it08~lga03 IN @lr_wage_type_to_check OR it08~lga04 IN @lr_wage_type_to_check OR
                 it08~lga05 IN @lr_wage_type_to_check OR it08~lga06 IN @lr_wage_type_to_check OR
                 it08~lga07 IN @lr_wage_type_to_check OR it08~lga08 IN @lr_wage_type_to_check OR
                 it08~lga09 IN @lr_wage_type_to_check OR it08~lga10 IN @lr_wage_type_to_check OR
                 it08~lga11 IN @lr_wage_type_to_check OR it08~lga12 IN @lr_wage_type_to_check OR
                 it08~lga13 IN @lr_wage_type_to_check OR it08~lga14 IN @lr_wage_type_to_check OR
                 it08~lga15 IN @lr_wage_type_to_check OR it08~lga16 IN @lr_wage_type_to_check OR
                 it08~lga17 IN @lr_wage_type_to_check OR it08~lga18 IN @lr_wage_type_to_check OR
                 it08~lga19 IN @lr_wage_type_to_check OR it08~lga20 IN @lr_wage_type_to_check OR
                 it08~lga21 IN @lr_wage_type_to_check OR it08~lga22 IN @lr_wage_type_to_check OR
                 it08~lga23 IN @lr_wage_type_to_check OR it08~lga24 IN @lr_wage_type_to_check OR
                 it08~lga25 IN @lr_wage_type_to_check OR it08~lga26 IN @lr_wage_type_to_check OR
                 it08~lga27 IN @lr_wage_type_to_check OR it08~lga28 IN @lr_wage_type_to_check OR
                 it08~lga29 IN @lr_wage_type_to_check OR it08~lga30 IN @lr_wage_type_to_check OR
                 it08~lga31 IN @lr_wage_type_to_check OR it08~lga32 IN @lr_wage_type_to_check OR
                 it08~lga33 IN @lr_wage_type_to_check OR it08~lga34 IN @lr_wage_type_to_check OR
                 it08~lga35 IN @lr_wage_type_to_check OR it08~lga36 IN @lr_wage_type_to_check OR
                 it08~lga37 IN @lr_wage_type_to_check OR it08~lga38 IN @lr_wage_type_to_check OR
                 it08~lga39 IN @lr_wage_type_to_check OR it08~lga40 IN @lr_wage_type_to_check ) AND
              "for this query, we only care that the employee is active
              "We don't care if the employee has different IT0001 (payroll area, etc) as long
              "as the WT exists
              EXISTS ( SELECT 1
                        FROM pa0000 AS it00
                        WHERE it00~pernr = it08~pernr      AND
                              it00~begda <= it08~endda     AND
                              it00~endda >= it08~begda     AND
                              it00~stat2 IN @mt_stat2      AND
                              it00~sprps = @if_hrpa_read_infotype=>unlocked )
         ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      LOOP AT lt_data INTO ls_data.
        DO zusecl_m99_pa_it0008_comp_wt=>mc_number_of_wagetypes TIMES.
          lv_field_no = sy-index.
          CONCATENATE zusecl_m99_pa_it0008_comp_wt=>mc_field_prefix_wage_type lv_field_no INTO lv_field_wt_name.
          ASSIGN COMPONENT lv_field_wt_name OF STRUCTURE ls_data TO <fs_wt>.
          IF <fs_wt> IS ASSIGNED.
            IF <fs_wt> IS INITIAL.
              "if there is no wage type in current sy-index, remaining fields will not be populated.
              "Thus, there is no need to search further
              EXIT.
            ELSEIF <fs_wt> IN lr_wage_type_to_check.
              READ TABLE lt_lgart INTO ls_lgart WITH TABLE KEY lgart = <fs_wt>.
              IF sy-subrc = 0.

                IF ls_lgart-endda_max < if_hrpa_read_infotype=>high_date.
                  "prerequisite: make sure that LT_Data is sorted by PERNR ENDDA BEGDA
                  "and there is no overlap (which is the case for IT0008)
                  "check if there is a gap
                  CLEAR: lv_next_begda.
                  lv_next_begda = ls_lgart-endda_max + 1.
                  IF lv_next_begda < ls_data-begda.
                    lv_is_gap_found = abap_true.
                    EXIT.
                  ENDIF.

*                 IF ls_lgart-begda_min > ls_data-begda.
*                   ls_lgart-begda_min = ls_data-begda.
*                 ENDIF.
                  IF ls_lgart-endda_max < ls_data-endda.
                    ls_lgart-endda_max = ls_data-endda.
                  ENDIF.
                  MODIFY TABLE lt_lgart FROM ls_lgart TRANSPORTING endda_max.
                ENDIF.
              ELSE.
                CLEAR: ls_lgart.
                ls_lgart-lgart = <fs_wt>.
                ls_lgart-begda_min = ls_data-begda.
                ls_lgart-endda_max = ls_data-endda.
                INSERT ls_lgart INTO TABLE lt_lgart.
              ENDIF.
            ENDIF.
          ENDIF.
          IF lv_is_gap_found = abap_true.
            EXIT.
          ENDIF.
        ENDDO.

        IF lv_is_gap_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      "compare with pay period
      IF lv_is_gap_found = abap_false.
        LOOP AT lt_lgart INTO ls_lgart.
          IF ls_lgart-begda_min > lv_pay_period_begda_new OR
            ls_lgart-endda_max < lv_pay_period_endda_new.
            lv_is_error = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lv_is_gap_found = abap_true OR
        lv_is_error = abap_true.
        "generate error.
        rv_is_error = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD GET_PAY_PERIOD_BEGDA_ENDDA.

*Get pay periods for a payroll area that covers IV_IT07_BEGDA and IV_IT07_ENDDA

    TYPES: BEGIN OF ty_t549q,
             begda TYPE t549q-begda,
             endda TYPE t549q-endda,
           END OF ty_t549q.
    DATA: lt_t549q TYPE STANDARD TABLE OF ty_t549q,
          ls_t549q LIKE LINE OF lt_t549q.
    CLEAR: ev_period_begda, ev_period_endda.
    READ TABLE mt_t549a INTO DATA(ls_t549a) WITH TABLE KEY abkrs = iv_abkrs.
    IF sy-subrc <> 0.
      CLEAR: ls_t549a.
      ls_t549a-abkrs = iv_abkrs.
      SELECT SINGLE permo INTO ls_t549a-permo
        FROM t549a
        WHERE abkrs = iv_abkrs.
      IF sy-subrc = 0.
        INSERT ls_t549a INTO TABLE mt_t549a.
      ENDIF.
    ENDIF.
    IF ls_t549a IS NOT INITIAL.
      SELECT begda endda INTO CORRESPONDING FIELDS OF TABLE lt_t549q
        FROM t549q
        WHERE permo = ls_t549a-permo AND
          begda <= iv_it07_endda     AND
          endda >= iv_it07_begda.
      IF sy-subrc = 0.
        LOOP AT lt_t549q INTO ls_t549q.
          IF ev_period_begda IS INITIAL
            OR ev_period_begda > ls_t549q-begda.
            ev_period_begda = ls_t549q-begda.
          ENDIF.
          IF ev_period_endda IS INITIAL
            OR ev_period_endda < ls_t549q-endda.
            ev_period_endda = ls_t549q-endda.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD GET_SPECIFC_CUSTMIZING.
    TRY.
        CALL METHOD me->read_range_parameter
          EXPORTING
            iv_inc_par_type  = me->mc_filter_lgart_01
          CHANGING
            ct_parameter_tab = mt_filter_lgart_01.

      CATCH cx_pyd_fnd INTO DATA(lo_exception).
    ENDTRY.
  ENDMETHOD.


  METHOD GET_WT_TO_CHECK.
    DATA: ls_it08_only TYPE zusecl_m99_pa_it0008_comp_wt=>ty_it08.
    ls_it08_only = CORRESPONDING #( is_data ).
    zusecl_m99_pa_it0008_comp_wt=>sum_of_wt(
      EXPORTING
        is_it08   = ls_it08_only
        it_filter = it_filter_lgart
      IMPORTING
        et_lgart  = rt_wage_types ).

  ENDMETHOD.
ENDCLASS.
