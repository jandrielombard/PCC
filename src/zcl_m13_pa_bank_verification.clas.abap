CLASS zcl_m13_pa_bank_verification DEFINITION
  PUBLIC
  INHERITING FROM zcl_m99_pcc_chk_fp4_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS mc_currency_au TYPE pa0009-waers VALUE 'AUD' ##NO_TEXT.
    CONSTANTS mc_country_au TYPE pa0009-banks VALUE 'AU' ##NO_TEXT.
    CONSTANTS mc_itemid_bank_verification TYPE pyd_s_rdsfo_ext-itemid VALUE 'CURR_CNTRY' ##NO_TEXT.
  PROTECTED SECTION.

    METHODS check
        REDEFINITION .
    METHODS err_ov_get_list
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_M13_PA_BANK_VERIFICATION IMPLEMENTATION.


  METHOD check.
* Check if currency is not AUD or country is not AU in IT0009
    DATA: lt_it9   TYPE STANDARD TABLE OF pernr_d WITH NON-UNIQUE KEY table_line.
    DATA: ls_result LIKE LINE OF rt_result.

* All relevant employees with invalid IT09
    SELECT DISTINCT it00~pernr INTO TABLE lt_it9 FROM pa0000 AS it00
        INNER JOIN pa0001 AS it01 ON it00~pernr = it01~pernr
        INNER JOIN pa0009 AS it09 ON it09~pernr = it00~pernr
        WHERE it00~pernr in it_pernr_so       AND
              it01~begda <= mv_endda          AND
              it01~endda >= mv_begda          AND
              it00~begda <= mv_endda          AND
              it00~endda >= mv_begda          AND
              it00~stat2 IN mt_stat2          AND
              it00~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~abkrs IN mt_payroll_areas  AND
              it09~begda <= mv_endda          AND
              it09~endda >= mv_begda          AND
              it09~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~bukrs IN mt_bukrs          AND
              it01~werks IN mt_werks          AND
              it01~persg IN mt_persg          AND
              it01~persk IN mt_persk          AND
              ( it09~waers <> mc_currency_au OR it09~banks <> mc_country_au ) AND

              "This is to make sure that there is no valid record during the pay period
              NOT EXISTS ( SELECT *
                            FROM pa0009 AS it09_2
                            WHERE it09_2~pernr = it09~pernr      AND
                                  it09_2~subty = it09~subty      AND
                                  it09_2~objps = it09~objps      AND
                                  it09_2~sprps = it09~sprps      AND
                                  it09_2~begda <= mv_endda       AND
                                  it09_2~endda >= mv_begda       AND
                                  it09_2~waers = mc_currency_au  AND
                                  it09_2~banks = mc_country_au ).

    IF lt_it9 IS NOT INITIAL.
      RETURN.
    ENDIF.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.

    LOOP AT lt_it9 INTO ls_result-id.
      INSERT ls_result INTO TABLE rt_result.
    ENDLOOP.

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
        lv_value_string = text-001.
        REPLACE FIRST OCCURRENCE OF '&1' IN lv_value_string WITH mc_currency_au.
        REPLACE FIRST OCCURRENCE OF '&2' IN lv_value_string WITH mc_country_au.

        LOOP AT ct_err_ov INTO ls_err_ov.
          me->add_record_to_sfo_tab(
              EXPORTING
                iv_itemid                   = mc_itemid_bank_verification
                iv_text                     = |{ text-002 }|
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
ENDCLASS.
