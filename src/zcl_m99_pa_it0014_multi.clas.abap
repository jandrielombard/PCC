class ZCL_M99_PA_IT0014_MULTI definition
  public
  inheriting from ZCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_SUBTY type PYD_S_RDSFO_EXT-ITEMID value 'SUBTY' ##NO_TEXT.
  constants MC_PARAM_LGART type PYD_PAR_TYPE value 'Z99_LGART' ##NO_TEXT.

  class-methods IS_VALID_OPTION
    importing
      !IV_OPTION type OPTION
    returning
      value(RV_VALID) type ABAP_BOOL .
protected section.

  types:
    BEGIN OF ty_data_for_db,
             subty TYPE subty,
             begda TYPE begda,
           END OF ty_data_for_db .
  types:
    BEGIN OF ty_data,
             pernr TYPE pernr_d.
             INCLUDE TYPE ty_data_for_db.
           TYPES: END OF ty_data .
  types:
    tty_data TYPE SORTED TABLE OF ty_data WITH UNIQUE KEY pernr subty begda .

  data MT_OUTPUT type TTY_DATA .
  data MT_FILTER_LGART type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_LGART_TEXT type ZCL_M99_PA_IT0008_COMP_WT=>TTY_LGART_TEXT .

  methods GET_LGART_TEXT
    importing
      !IV_LGART type LGART
    returning
      value(RV_TEXT) type T512T-LGTXT .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_M99_PA_IT0014_MULTI IMPLEMENTATION.


  method check.
*check if there are multiple IT14 for specified wage types
*at the end of pay period. The comparison is between wage types
*specified as parameters (not for the same wage type)
    data: lt_output  type  tty_data.

    select distinct it14~pernr, it14~subty, it14~begda
      into table @lt_output
      from pa0014 as it14
      where it14~pernr in @it_pernr_so                    and
            it14~subty in @mt_filter_lgart                and
            it14~sprps = @if_hrpa_read_infotype=>unlocked and
            it14~begda <= @mv_endda                       and
            it14~endda >= @mv_endda                       and
            exists ( select 1
              from pa0000 as it00
                inner join pa0001 as it01 on
                  it00~pernr = it01~pernr
              where it00~pernr = it14~pernr                   and
                it00~begda <= it14~endda                      and
                it00~endda >= it14~begda                      and
                it01~begda <= it14~endda                      and
                it01~endda >= it14~begda                      and
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
            ) and
            exists ( select 1
                     from pa0014 as it14_2
                     where it14_2~pernr = it14~pernr                       and
                           it14_2~subty <> it14~subty                      and
                           it14_2~subty in @mt_filter_lgart                and
                           it14_2~sprps = @if_hrpa_read_infotype=>unlocked and
                           it14_2~begda <= @mv_endda                       and
                           it14_2~endda >= @mv_endda
            ).
    if lt_output is not initial.
* Collect data for Overview
      append lines of lt_output to mt_output.

      loop at lt_output into data(ls_output).
        insert value #(
                par_type = if_pyd_cont_types=>gcs_par_type-pernr
                id = ls_output-pernr ) into table rt_result.
      endloop.
    endif.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  METHOD err_ov_get_list.
* Display error message
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    DATA:
      ls_err_ov        TYPE ty_s_err_ov,
      ls_sfo           TYPE cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr         TYPE p_pernr,
      lv_text          TYPE text120,
      lv_value         TYPE char060,
      ls_output_for_db TYPE ty_data_for_db,
      lt_sfo_tab_temp  LIKE ls_err_ov-sfo_tab,
      lv_value_string  TYPE string,
      lv_lgart_text    TYPE t512t-lgtxt.

    CASE iv_access_mode.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters                                        "MOD001++
        get_parameters( it_par         = it_par                "MOD001++
                        io_res_context = io_res_context ).     "MOD001++
        LOOP AT ct_err_ov INTO ls_err_ov.
          "move SFO_TAB records to temporary table
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          CLEAR: ls_err_ov-sfo_tab.
          LOOP AT lt_sfo_tab_temp INTO DATA(ls_sfo_tab_temp).
            IF ls_sfo_tab_temp-itemid = mc_itemid_subty.
              CLEAR: ls_output_for_db, lv_value, lv_value_string.
              lv_value = ls_sfo_tab_temp-value.
              CALL METHOD me->copy_structure_to_other
                EXPORTING
                  p_struct1 = lv_value
                CHANGING
                  p_struct2 = ls_output_for_db.

              WRITE ls_output_for_db-begda TO lv_text.
              lv_lgart_text = get_lgart_text( ls_output_for_db-subty ).
              MESSAGE i080 WITH lv_lgart_text INTO lv_value_string.
              CALL METHOD me->add_record_to_sfo_tab
                EXPORTING
                  iv_itemid                   = mc_itemid_subty
                  iv_text                     = |{ lv_text }|
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                CHANGING
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.
            ELSE.
              insert ls_sfo_tab_temp into table ls_err_ov-sfo_tab.
            ENDIF.
          ENDLOOP.

          MODIFY ct_err_ov FROM ls_err_ov TRANSPORTING sfo_tab.
        ENDLOOP.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        LOOP AT ct_err_ov INTO ls_err_ov.
          CLEAR ls_err_ov-sfo_tab.
          CLEAR ls_sfo.
          lv_pernr = ls_err_ov-id.
          LOOP AT mt_output INTO DATA(ls_output) WHERE pernr = lv_pernr.
            CLEAR: lv_value.
            ADD 1 TO ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_subty.
            ls_output_for_db = CORRESPONDING #( ls_output ).

            CALL METHOD me->copy_structure_to_other
              EXPORTING
                p_struct1 = ls_output_for_db
              CHANGING
                p_struct2 = lv_value.

            ls_sfo-value = lv_value.
            INSERT ls_sfo INTO TABLE ls_err_ov-sfo_tab.
          ENDLOOP.
          MODIFY ct_err_ov FROM ls_err_ov.
        ENDLOOP.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_pyd_fnd.

    ENDCASE.

  ENDMETHOD.


  method get_lgart_text.
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    read table mt_lgart_text into data(ls_lgart_text) with table key lgart = iv_lgart.
    if sy-subrc = 0.
      rv_text = ls_lgart_text-lgart_text.
    else.
      call function 'HR_GET_LGART_TEXT'
        exporting
          p_lgart          = iv_lgart
*         p_molga          = mc_molga_au           "MOD001--
          p_molga          = mv_molga              "MOD001++
        importing
          p_longtext       = rv_text
        exceptions
          no_entry_in_512t = 1
          others           = 2.
      clear: ls_lgart_text.
      ls_lgart_text-lgart = iv_lgart.
      ls_lgart_text-lgart_text = rv_text.
      insert ls_lgart_text into table mt_lgart_text.
    endif.
  endmethod.


  METHOD get_specifc_custmizing.
* Get parameters specific for this validation rule

    TRY.
        CALL METHOD me->read_range_parameter
          EXPORTING
            iv_inc_par_type  = me->mc_param_lgart
          CHANGING
            ct_parameter_tab = mt_filter_lgart.
      CATCH cx_pyd_fnd INTO DATA(lo_exception).
    ENDTRY.
  ENDMETHOD.


  METHOD is_valid_option.
    rv_valid =  xsdbool(
      iv_option = if_dmf_constants_c=>gc_range_option_between OR
      iv_option = if_dmf_constants_c=>gc_range_option_equal OR
      iv_option = if_dmf_constants_c=>gc_range_option_cp OR
      iv_option = if_dmf_constants_c=>gc_range_option_ge OR
      iv_option = if_dmf_constants_c=>gc_range_option_le OR
      iv_option = if_dmf_constants_c=>gc_range_option_gt OR
      iv_option = if_dmf_constants_c=>gc_range_option_lt OR
      iv_option = if_dmf_constants_c=>gc_range_option_not_equal ).
  ENDMETHOD.
ENDCLASS.
