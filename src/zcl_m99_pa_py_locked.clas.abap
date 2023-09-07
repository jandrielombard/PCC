class ZCL_M99_PA_PY_LOCKED definition
  public
  inheriting from ZCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
protected section.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
private section.

  constants MC_ABRSP type PYD_PAR_TYPE value 'Z99_ABRSP' ##NO_TEXT.
  data MV_ABRSP type ABRSP .
  constants MC_ABRSP_DEFAULT_VALUE type ABRSP value 'X' ##NO_TEXT.
  constants MC_ITEMID type PYD_S_RDSFO_EXT-ITEMID value 'RETRO' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_M99_PA_PY_LOCKED IMPLEMENTATION.


  METHOD CHECK.

    DATA lt_pernr   TYPE STANDARD TABLE OF pernr_d WITH NON-UNIQUE KEY table_line.
    DATA ls_result  TYPE                   ty_s_result.

    SELECT DISTINCT it00~pernr INTO TABLE lt_pernr FROM pa0000 AS it00
        INNER JOIN pa0001 AS it01 ON it00~pernr = it01~pernr
        INNER JOIN pa0003 AS it03 ON it00~pernr = it03~pernr
        WHERE it03~pernr IN it_pernr_so                    AND
              it03~begda <= mv_endda                       AND
              it03~endda >= mv_begda                       AND
              it00~begda <= mv_endda                       AND
              it00~endda >= mv_begda                       AND
              it01~begda <= mv_endda                       AND
              it01~abkrs IN mt_payroll_areas               AND
              it01~sprps = if_hrpa_read_infotype=>unlocked AND
              it01~endda >= mv_begda                       AND
              it00~stat2  IN mt_stat2                      AND
              it00~sprps = if_hrpa_read_infotype=>unlocked AND
              it03~sprps = if_hrpa_read_infotype=>unlocked AND
              it03~abrsp = mv_abrsp                        AND
              it01~bukrs IN mt_bukrs                       AND
              it01~werks IN mt_werks                       AND
              it01~btrtl IN mt_btrtl                       AND
              it01~persg IN mt_persg                       AND
              it01~persk IN mt_persk                       AND
              it01~kostl IN mt_kostl
      ORDER BY it00~pernr.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.

    LOOP AT lt_pernr INTO ls_result-id.
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
      ls_reso   TYPE if_pyd_fnd_types=>ty_s_reso,
      lv_abkrs  TYPE abkrs,
      lv_land1  TYPE land1,
      lv_land2  TYPE land1,
      lv_werk   TYPE persa,
      lv_pernr  TYPE p_pernr.

    CASE iv_access_mode.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        LOOP AT ct_err_ov INTO ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
          IF ls_err_ov-sfo_tab IS INITIAL.
            CLEAR ls_err_ov-sfo_tab.
            CLEAR ls_sfo.

            ADD 1 TO ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid.
            ls_sfo-value  = text-001.
            ls_sfo-text = text-002.
            INSERT ls_sfo INTO TABLE ls_err_ov-sfo_tab.
            MODIFY ct_err_ov FROM ls_err_ov.

          ELSE.
            LOOP AT ls_err_ov-sfo_tab INTO ls_sfo.
              CASE ls_sfo-itemid.
                WHEN mc_itemid.
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
        CALL METHOD get_parameters
          EXPORTING
            it_par         = it_par
            io_res_context = io_res_context.

        LOOP AT ct_err_ov INTO ls_err_ov.
          CLEAR ls_err_ov-sfo_tab.
          CLEAR ls_sfo.
          lv_pernr = ls_err_ov-id.

          ADD 1 TO ls_sfo-row_id.
          ls_sfo-itemid = mc_itemid.
          ls_sfo-value  = text-001.
          INSERT ls_sfo INTO TABLE ls_err_ov-sfo_tab.
          MODIFY ct_err_ov FROM ls_err_ov.
        ENDLOOP.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_pyd_fnd.

    ENDCASE.

  ENDMETHOD.


  METHOD get_specifc_custmizing.

    TRY .
        IF line_exists( mo_context->mt_par[ par_type = me->mc_abrsp ] ).
          mv_abrsp =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_abrsp
                                                            it_par      = mo_context->mt_par ).
        ELSE.
          mv_abrsp = mc_abrsp_default_value.
        ENDIF.
      CATCH cx_pyd_fnd INTO DATA(lo_exception).

    ENDTRY.


  ENDMETHOD.
ENDCLASS.
