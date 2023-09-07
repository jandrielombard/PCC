CLASS zcl_m99_pa_it0007_status DEFINITION
  PUBLIC
  INHERITING FROM zcl_m99_pcc_chk_fp4_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS mc_itemid_it07 TYPE pyd_s_rdsfo_ext-itemid VALUE 'IT0007' ##NO_TEXT.
    CONSTANTS mc_param_zterf TYPE pyd_par_type VALUE 'Z99_FILTER_ZTERF' ##NO_TEXT.
    CONSTANTS mc_exc_zterf TYPE pyd_par_type VALUE 'Z99_EXC_FILTER_ZTERF' ##NO_TEXT.
  PROTECTED SECTION.

    DATA mt_filter_zterf TYPE /iwbep/t_cod_select_options .

    METHODS check
        REDEFINITION .
    METHODS err_ov_get_list
        REDEFINITION .
    METHODS get_specifc_custmizing
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_M99_PA_IT0007_STATUS IMPLEMENTATION.


  METHOD check.
* check time management status on IT07
    DATA: lt_it     TYPE SORTED TABLE OF pernr_d WITH UNIQUE KEY table_line,
          ls_result LIKE LINE OF rt_result.

    SELECT DISTINCT it07~pernr
      INTO TABLE @lt_it
      FROM pa0007 AS it07
      WHERE it07~pernr IN @it_pernr_so        AND
            it07~sprps = @if_hrpa_read_infotype=>unlocked AND
            it07~begda <= @mv_endda           AND
            it07~endda >= @mv_begda           AND
            it07~zterf IN @mt_filter_zterf    AND
            EXISTS ( SELECT 1
              FROM pa0000 AS it00
                INNER JOIN pa0001 AS it01 ON
                  it00~pernr = it01~pernr
              WHERE it00~pernr = it07~pernr       AND
                it00~begda <= it07~endda          AND
                it00~endda >= it07~begda          AND
                it01~begda <= it07~endda          AND
                it01~endda >= it07~begda          AND
                it00~begda <= @mv_endda           AND
                it00~endda >= @mv_begda           AND
                it01~begda <= @mv_endda           AND
                it01~endda >= @mv_begda           AND
                it00~stat2 IN @mt_stat2           AND
                it00~sprps = @if_hrpa_read_infotype=>unlocked AND
                it01~sprps = @if_hrpa_read_infotype=>unlocked AND
                it01~abkrs IN @mt_payroll_areas   AND
                it01~bukrs IN @mt_bukrs           AND
                it01~werks IN @mt_werks           AND
                it01~btrtl IN @mt_btrtl           AND
                it01~persg IN @mt_persg           AND
                it01~persk IN @mt_persk           AND
                it01~kostl IN @mt_kostl
            )
    ORDER BY it07~pernr.
    IF lt_it IS NOT INITIAL.

      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      LOOP AT lt_it INTO DATA(ls_it).
        ls_result-id = ls_it.
        INSERT ls_result INTO TABLE rt_result.
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

        MESSAGE i070 INTO lv_value_string.

        LOOP AT ct_err_ov INTO ls_err_ov.
          me->add_record_to_sfo_tab(
            EXPORTING
              iv_itemid                   = mc_itemid_it07
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
            iv_inc_par_type  = me->mc_param_zterf
            iv_exc_par_type  = me->mc_exc_zterf
          CHANGING
            ct_parameter_tab = mt_filter_zterf.

      CATCH cx_pyd_fnd INTO DATA(lo_exception).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
