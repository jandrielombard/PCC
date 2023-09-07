class ZCL_M99_PA_MAIN_ADDRESS definition
  public
  inheriting from ZCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_SUBTY type PYD_S_RDSFO_EXT-ITEMID value 'SUBTY' ##NO_TEXT.
protected section.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_M99_PA_MAIN_ADDRESS IMPLEMENTATION.


  METHOD CHECK.

    DATA: lt_it0   TYPE STANDARD TABLE OF pernr_d WITH NON-UNIQUE KEY table_line,
          lt_it6   TYPE STANDARD TABLE OF pernr_d WITH NON-UNIQUE KEY table_line,
          lv_size0 TYPE i,
          lv_size6 TYPE i.

    DATA: ls_result  TYPE   ty_s_result.


* All relevant employees
    SELECT it00~pernr INTO TABLE lt_it0 FROM pa0000 AS it00
        INNER JOIN pa0001 AS it01 ON it00~pernr = it01~pernr
        WHERE it01~pernr IN it_pernr_so                    AND
              it01~begda <= mv_endda                       AND
              it01~endda >= mv_begda                       AND
              it00~begda <= mv_endda                       AND
              it00~endda >= mv_begda                       AND
              it00~stat2 IN mt_stat2                       AND
              it00~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~abkrs IN mt_payroll_areas               AND
              it01~bukrs IN mt_bukrs                       AND
              it01~werks IN mt_werks                       AND
              it01~persg IN mt_persg                       AND
              it01~persk IN mt_persk.


* All relevant employees with IT06
    SELECT it00~pernr INTO TABLE lt_it6 FROM pa0000 AS it00
      INNER JOIN pa0001 AS it01 ON it00~pernr = it01~pernr
      INNER JOIN pa0006 AS it06 ON it06~pernr = it00~pernr
        WHERE it01~pernr IN it_pernr_so                    AND
              it01~begda <= mv_endda                       AND
              it01~endda >= mv_begda                       AND
              it00~begda <= mv_endda                       AND
              it00~endda >= mv_begda                       AND
              it00~stat2 IN mt_stat2                       AND
              it00~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~abkrs IN mt_payroll_areas               AND
              it06~begda <= mv_endda                       AND
              it06~endda >= mv_begda                       AND
              it06~subty = mv_subty                        AND
              it06~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~bukrs IN mt_bukrs                       AND
              it01~werks IN mt_werks                       AND
              it01~persg IN mt_persg                       AND
              it01~persk IN mt_persk.


    SORT lt_it0. DELETE ADJACENT DUPLICATES FROM lt_it0.
    SORT lt_it6. DELETE ADJACENT DUPLICATES FROM lt_it6.

    DESCRIBE TABLE lt_it0 LINES lv_size0.
    DESCRIBE TABLE lt_it6 LINES lv_size6.

    IF lv_size0 = lv_size6.
      RETURN.
    ENDIF.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.

    LOOP AT lt_it6 INTO DATA(lv_pernr).
      DELETE TABLE lt_it0 FROM lv_pernr.
    ENDLOOP.

    LOOP AT lt_it0 INTO ls_result-id.
      INSERT ls_result INTO TABLE rt_result.
    ENDLOOP.

    " register event when transaction complete we need clear cache
    SET HANDLER handle_init_buffers FOR mo_fnd_factory->mo_transaction.

  ENDMETHOD.


  METHOD ERR_OV_GET_LIST.

    DATA:
      ls_err_ov TYPE ty_s_err_ov,
      ls_sfo    TYPE cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  TYPE abap_bool,
      lv_pernr  TYPE p_pernr,
      lv_value  TYPE pyd_item_value.


    lv_value = text-001.

    CASE iv_access_mode.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        LOOP AT ct_err_ov INTO ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
          IF ls_err_ov-sfo_tab IS INITIAL.
            CLEAR ls_err_ov-sfo_tab.
            CLEAR ls_sfo.

            ADD 1 TO ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_subty.
            ls_sfo-value  = lv_value.
            ls_sfo-text = text-002.
            INSERT ls_sfo INTO TABLE ls_err_ov-sfo_tab.
            MODIFY ct_err_ov FROM ls_err_ov.

          ELSE.
            LOOP AT ls_err_ov-sfo_tab INTO ls_sfo.
              CASE ls_sfo-itemid.
                WHEN mc_itemid_subty.
                  ls_sfo-text = text-002 .
                  MODIFY ls_err_ov-sfo_tab FROM ls_sfo TRANSPORTING text.
                  lv_modif = abap_true.
                WHEN OTHERS.
                  "nothing
              ENDCASE.
            ENDLOOP.
            IF lv_modif = abap_true.
              MODIFY ct_err_ov FROM ls_err_ov.
            ENDIF.
          ENDIF.

        ENDLOOP.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        LOOP AT ct_err_ov INTO ls_err_ov.
          CLEAR ls_err_ov-sfo_tab.
          CLEAR ls_sfo.
          lv_pernr = ls_err_ov-id.

          ADD 1 TO ls_sfo-row_id.
          ls_sfo-itemid = mc_itemid_subty.
          ls_sfo-value  = lv_value.
          INSERT ls_sfo INTO TABLE ls_err_ov-sfo_tab.
          MODIFY ct_err_ov FROM ls_err_ov.
        ENDLOOP.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_pyd_fnd.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
