class ZCL_PYC_KPI_EE_LOCKED definition
  public
  inheriting from ZCL_PYC_KPI_NUM_BASE
  create public

  global friends CL_PYD_FND_AUX .

public section.

  constants:
    BEGIN OF mc_lock_indicator,
                 yes TYPE abrsp VALUE 'X' ##NO_TEXT,
                 no  TYPE abrsp VALUE space ##NO_TEXT,
               END OF mc_lock_indicator .
protected section.

  methods HEADER_GET
    redefinition .
  methods NUMERIC_INFO_CALCULATE
    redefinition .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PYC_KPI_EE_LOCKED IMPLEMENTATION.


  METHOD header_get.

    ev_text = text-001.
    CLEAR es_var1.

  ENDMETHOD.


  METHOD numeric_info_calculate.

* get number of team members locked for payroll

    DATA:
      lt_abkrs_so TYPE /iwbep/t_cod_select_options,
      ls_t549q    TYPE ty_pay_period_dates.

    CLEAR ev_int4.

    get_parameters(
      EXPORTING
        io_res_context   = io_res_context
      IMPORTING
        et_abkrs_so = lt_abkrs_so
        es_pay_period_dates = ls_t549q ).

    "use lock indicator from PA0003-ABRSP
    IF lt_abkrs_so IS NOT INITIAL AND ls_t549q IS NOT INITIAL.
      SELECT COUNT( DISTINCT it00~pernr ) INTO @ev_int4 FROM pa0000 AS it00
          INNER JOIN pa0001 AS it01 ON it00~pernr = it01~pernr
          INNER JOIN pa0003 AS it03 ON it00~pernr = it03~pernr
          WHERE it03~begda <= @ls_t549q-endda                   AND
                it03~endda >= @ls_t549q-begda                   AND
                it00~begda <= @ls_t549q-endda                   AND
                it00~endda >= @ls_t549q-begda                   AND
                it01~begda <= @ls_t549q-endda                   AND
                it01~abkrs IN @lt_abkrs_so                      AND
                it01~sprps = @if_hrpa_read_infotype=>unlocked   AND
                it01~endda >= @ls_t549q-begda                   AND
                it00~stat2 = @cl_eain_constants=>c_pernr_active AND
                it00~sprps = @if_hrpa_read_infotype=>unlocked   AND
                it03~sprps = @if_hrpa_read_infotype=>unlocked   AND
                it03~abrsp = @mc_lock_indicator-yes.
    ENDIF.

    CLEAR ev_kpi_color. "take default
    ev_kpi_trend = cl_pyc_rt_facade=>gcs_kpi_trend-neutral.
    CLEAR ev_kpi_style. "-> take default
    CLEAR ev_p_type. "let framework decide
    CLEAR: ev_maxbt, ev_waers. "to be on the safe side

  ENDMETHOD.
ENDCLASS.
