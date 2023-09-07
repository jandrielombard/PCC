CLASS zcl_m99_pa_term_w_active DEFINITION
  PUBLIC
  INHERITING FROM zcl_m99_pcc_chk_fp4_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS mc_itemid_it701 TYPE pyd_s_rdsfo_ext-itemid VALUE 'IT0701' ##NO_TEXT.
    CONSTANTS mc_param_massn TYPE pyd_par_type VALUE 'Z99_MASSN' ##NO_TEXT.

  PROTECTED SECTION.

    DATA mt_filter_massn TYPE /iwbep/t_cod_select_options .

    METHODS check
        REDEFINITION .
    METHODS err_ov_get_list
        REDEFINITION .
    METHODS get_specifc_custmizing
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_M99_PA_TERM_W_ACTIVE IMPLEMENTATION.


  METHOD check.

*check if there is IT0701 record there should be a corresponding
*IT00 record on next day with specified action reason

    TYPES: BEGIN OF lty_it,
             pernr TYPE pernr_d,
             begda TYPE begda,
           END OF lty_it,
           BEGIN OF lty_it_2.
             INCLUDE TYPE lty_it.
             TYPES: endda TYPE endda,
           END OF lty_it_2.
    DATA: lt_it701  TYPE STANDARD TABLE OF lty_it WITH NON-UNIQUE KEY pernr,
          ls_it701  LIKE LINE OF lt_it701,
          lt_it00   TYPE STANDARD TABLE OF lty_it_2 WITH NON-UNIQUE KEY pernr,
          ls_result LIKE LINE OF rt_result.

    "Do not use MV_BEGDA and MV_ENDDA to filter IT07
    "because we want to validate all IT701 records
    "for the employee.
    SELECT DISTINCT it701~pernr, it701~begda
      INTO TABLE @lt_it701
      FROM pa0701 AS it701
      WHERE it701~pernr IN @it_pernr_so                    AND
            it701~sprps = @if_hrpa_read_infotype=>unlocked AND
*            it701~begda <= @mv_endda           AND
*            it701~endda >= @mv_begda           AND
            EXISTS ( SELECT 1
              FROM pa0000 AS it00
                INNER JOIN pa0001 AS it01 ON
                  it00~pernr = it01~pernr
              WHERE it00~pernr = it701~pernr       AND
*                it00~begda <= it701~endda          AND
*                it00~endda >= it701~begda          AND
*                it01~begda <= it701~endda          AND
*                it01~endda >= it701~begda          AND`
                it00~begda <= @mv_endda            AND
                it00~endda >= @mv_begda            AND
                it01~begda <= @mv_endda            AND
                it01~endda >= @mv_begda            AND
                it00~stat2 IN @mt_stat2            AND
                it00~sprps = @if_hrpa_read_infotype=>unlocked AND
                it01~sprps = @if_hrpa_read_infotype=>unlocked AND
                it01~abkrs IN @mt_payroll_areas    AND
                it01~bukrs IN @mt_bukrs            AND
                it01~werks IN @mt_werks            AND
                it01~btrtl IN @mt_btrtl            AND
                it01~persg IN @mt_persg            AND
                it01~persk IN @mt_persk            AND
                it01~kostl IN @mt_kostl
            ).
    IF lt_it701 IS NOT INITIAL.
      LOOP AT lt_it701 ASSIGNING FIELD-SYMBOL(<component>).
        IF <component>-begda < if_hrpa_read_infotype=>high_date.
          <component>-begda = <component>-begda + 1.
        ENDIF.
      ENDLOOP.
      SELECT DISTINCT it00~pernr, it00~begda, it00~endda
        INTO CORRESPONDING FIELDS OF TABLE @lt_it00
        FROM pa0000 AS it00
        FOR ALL ENTRIES IN @lt_it701
        WHERE it00~pernr = @lt_it701-pernr  AND
              it00~begda <= @lt_it701-begda AND
              it00~endda >= @lt_it701-begda AND
              it00~massn IN @mt_filter_massn.

      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      LOOP AT lt_it701 INTO ls_it701.
        LOOP AT lt_it00 TRANSPORTING NO FIELDS WHERE
          pernr = ls_it701-pernr  AND
          begda <= ls_it701-begda AND
          endda >= ls_it701-begda.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          ls_result-id = ls_it701-pernr.
          INSERT ls_result INTO TABLE rt_result.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " register event when transaction complete we need clear cache
    SET HANDLER handle_init_buffers FOR mo_fnd_factory->mo_transaction.

  ENDMETHOD.


  METHOD err_ov_get_list.
* Display error message
    DATA:
      ls_err_ov       TYPE ty_s_err_ov,
      lv_value_string TYPE string.

    CASE iv_access_mode.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        MESSAGE i065 INTO lv_value_string.

        LOOP AT ct_err_ov INTO ls_err_ov.
          me->add_record_to_sfo_tab(
            EXPORTING
              iv_itemid                   = mc_itemid_it701
              iv_text                     = |{ text-001 }|
              iv_value                    = lv_value_string
              iv_text_for_1st_record_only = abap_true
            CHANGING
              ct_sfo_tab                  = ls_err_ov-sfo_tab ).
          MODIFY ct_err_ov FROM ls_err_ov TRANSPORTING sfo_tab.
        ENDLOOP.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.


      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_pyd_fnd.

    ENDCASE.

  ENDMETHOD.


  METHOD get_specifc_custmizing.
* Get parameters specific for this validation rule

    TRY.
        CALL METHOD me->read_range_parameter
          EXPORTING
            iv_inc_par_type  = me->mc_param_massn
          CHANGING
            ct_parameter_tab = mt_filter_massn.

      CATCH cx_pyd_fnd INTO DATA(lo_exception).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
