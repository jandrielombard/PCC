class ZUSECL_M99_PA_IT0001_WO_IT0008 definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_FIELD_PREFIX_NUMBER type NAME_FELD value 'ANZ' ##NO_TEXT.
  constants MC_ITEMID_IT01_WITHOUT_IT08 type PYD_S_RDSFO_EXT-ITEMID value 'IT1_WO_IT8' ##NO_TEXT.
  constants MC_FILTER_LGART_01 type PYD_PAR_TYPE value 'Z99_FILTER_LGART_01' ##NO_TEXT.
  constants MC_FILTER_LGART_02 type PYD_PAR_TYPE value 'Z99_FILTER_LGART_02' ##NO_TEXT.
protected section.

  types:
    BEGIN OF ty_data,
      pernr    TYPE pa0008-pernr,
      endda    TYPE pa0008-endda,
      begda    TYPE pa0008-begda,
    END OF ty_data .
  types:
    tty_data TYPE STANDARD TABLE OF ty_data .
  types:
    BEGIN OF ty_it1_wo_it8,
      pernr TYPE pa0001-pernr,
      begda TYPE pa0001-begda,
      endda TYPE pa0001-endda,
    END OF ty_it1_wo_it8 .
  types:
    tty_it1_wo_it8 TYPE SORTED TABLE OF ty_it1_wo_it8 with unique key pernr begda endda .

  data MT_IT1_WO_IT8 type TTY_IT1_WO_IT8 .
  data MT_FILTER_LGART_01 type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_FILTER_LGART_02 type /IWBEP/T_COD_SELECT_OPTIONS .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
PRIVATE SECTION.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT0001_WO_IT0008 IMPLEMENTATION.


  METHOD CHECK.

    DATA: lt_data       TYPE tty_data,
          ls_it1_wo_it8 LIKE LINE OF mt_it1_wo_it8,
          ls_result     LIKE LINE OF rt_result.

    CHECK me->mv_lgart IS NOT INITIAL.

    SELECT DISTINCT it08~pernr, it08~endda, it08~begda
      INTO CORRESPONDING FIELDS OF TABLE @lt_data
      FROM pa0001 AS it01 LEFT OUTER JOIN pa0008 AS it08 ON
        it01~pernr = it08~pernr AND
        it01~endda >= it08~begda AND
        it01~begda <= it08~endda
    WHERE it01~pernr in @it_pernr_so                    AND
          it01~endda >= @mv_begda                       AND
          it01~begda <= @mv_endda                       AND
          it01~sprps = @if_hrpa_read_infotype=>unlocked AND
          it01~abkrs IN @mt_payroll_areas               AND
          it01~bukrs IN @mt_bukrs                       AND
          it01~werks IN @mt_werks                       AND
          it01~btrtl IN @mt_btrtl                       AND
          it01~persg IN @mt_persg                       AND
          it01~persk IN @mt_persk                       AND
          it01~kostl IN @mt_kostl                       AND
          it08~lga01 <> @mv_lgart AND it08~lga02 <> @mv_lgart AND
          it08~lga03 <> @mv_lgart AND it08~lga04 <> @mv_lgart AND
          it08~lga05 <> @mv_lgart AND it08~lga06 <> @mv_lgart AND
          it08~lga07 <> @mv_lgart AND it08~lga08 <> @mv_lgart AND
          it08~lga09 <> @mv_lgart AND it08~lga10 <> @mv_lgart AND
          it08~lga11 <> @mv_lgart AND it08~lga12 <> @mv_lgart AND
          it08~lga13 <> @mv_lgart AND it08~lga14 <> @mv_lgart AND
          it08~lga15 <> @mv_lgart AND it08~lga16 <> @mv_lgart AND
          it08~lga17 <> @mv_lgart AND it08~lga18 <> @mv_lgart AND
          it08~lga19 <> @mv_lgart AND it08~lga20 <> @mv_lgart AND
          it08~lga21 <> @mv_lgart AND it08~lga22 <> @mv_lgart AND
          it08~lga23 <> @mv_lgart AND it08~lga24 <> @mv_lgart AND
          it08~lga25 <> @mv_lgart AND it08~lga26 <> @mv_lgart AND
          it08~lga27 <> @mv_lgart AND it08~lga28 <> @mv_lgart AND
          it08~lga29 <> @mv_lgart AND it08~lga30 <> @mv_lgart AND
          it08~lga31 <> @mv_lgart AND it08~lga32 <> @mv_lgart AND
          it08~lga33 <> @mv_lgart AND it08~lga34 <> @mv_lgart AND
          it08~lga35 <> @mv_lgart AND it08~lga36 <> @mv_lgart AND
          it08~lga37 <> @mv_lgart AND it08~lga38 <> @mv_lgart AND
          it08~lga39 <> @mv_lgart AND it08~lga40 <> @mv_lgart AND
          EXISTS ( SELECT 1
                   FROM pa0000 AS it00
                   WHERE it00~pernr = it01~pernr    AND
                         it00~endda >= it01~begda   AND
                         it00~begda <= it01~endda   AND
                         it00~endda >= @mv_begda    AND
                         it00~begda <= @mv_endda    AND
                         it00~stat2 IN @mt_stat2    AND
                         it00~sprps = @if_hrpa_read_infotype=>unlocked ).
    IF lt_data IS NOT INITIAL.
      LOOP AT lt_data INTO DATA(ls_data).
        CLEAR: ls_it1_wo_it8.
        ls_it1_wo_it8-pernr = ls_data-pernr.
        ls_it1_wo_it8-begda = ls_data-begda.
        ls_it1_wo_it8-endda = ls_data-endda.
        "if there is duplicate, sy-subrc returns 8 but we can ignore it
        INSERT ls_it1_wo_it8 INTO TABLE mt_it1_wo_it8.

        CLEAR: ls_result.
        ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
        ls_result-id = ls_it1_wo_it8-pernr.
        INSERT ls_result INTO TABLE rt_result.
      ENDLOOP.
    ENDIF.

    " register event when transaction complete we need clear cache
    SET HANDLER handle_init_buffers FOR mo_fnd_factory->mo_transaction.

  ENDMETHOD.


  METHOD ERR_OV_GET_LIST.

    DATA:
      ls_err_ov     TYPE ty_s_err_ov,
      ls_sfo        TYPE cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif      TYPE abap_bool,
      lv_pernr      TYPE p_pernr,
      lv_value      TYPE pyd_s_rdsfo_ext-value,
      ls_it1_wo_it8 LIKE LINE OF mt_it1_wo_it8.


    CASE iv_access_mode.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        CALL METHOD get_parameters
          EXPORTING
            it_par         = it_par
            io_res_context = io_res_context.

        LOOP AT ct_err_ov INTO ls_err_ov.
          CLEAR ls_err_ov-sfo_tab.
          CLEAR ls_sfo.
          lv_pernr = ls_err_ov-id.

          LOOP AT mt_it1_wo_it8 INTO ls_it1_wo_it8
            WHERE pernr = lv_pernr.

            MESSAGE e035 WITH
              ls_it1_wo_it8-begda
              ls_it1_wo_it8-endda
              mv_lgart INTO lv_value.


            ADD 1 TO ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_it01_without_it08.
            ls_sfo-text   = text-001.
            ls_sfo-value  = lv_value.
            INSERT ls_sfo INTO TABLE ls_err_ov-sfo_tab.
          ENDLOOP.

          MODIFY ct_err_ov FROM ls_err_ov.
        ENDLOOP.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_pyd_fnd.

    ENDCASE.

  ENDMETHOD.


  METHOD GET_SPECIFC_CUSTMIZING.

    TRY.
        me->read_range_parameter(
          EXPORTING
            iv_inc_par_type  = me->mc_filter_lgart_01
          CHANGING
            ct_parameter_tab = mt_filter_lgart_01 ).

        me->read_range_parameter(
          EXPORTING
            iv_inc_par_type  = me->mc_filter_lgart_02
          CHANGING
            ct_parameter_tab = mt_filter_lgart_02 ).

      CATCH cx_pyd_fnd INTO DATA(lo_exception).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
