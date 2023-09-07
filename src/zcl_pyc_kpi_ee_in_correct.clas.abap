class ZCL_PYC_KPI_EE_IN_CORRECT definition
  public
  inheriting from ZCL_PYC_KPI_NUM_BASE
  create public

  global friends CL_PYD_FND_AUX .

public section.
protected section.

  methods HEADER_GET
    redefinition .
  methods NUMERIC_INFO_CALCULATE
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_PYC_KPI_EE_IN_CORRECT IMPLEMENTATION.


  METHOD HEADER_GET.

    ev_text = text-001.
    CLEAR es_var1.

  ENDMETHOD.


  METHOD numeric_info_calculate.

* get number of team members in correction
    CLEAR ev_int4.

    DATA:
      lt_abkrs_so TYPE /iwbep/t_cod_select_options,
      ls_t549q    TYPE ty_pay_period_dates.

    get_parameters(
      EXPORTING
        io_res_context   = io_res_context
      IMPORTING
        et_abkrs_so = lt_abkrs_so
        es_pay_period_dates = ls_t549q ).

    "get number of active employees in current payroll period
    IF lt_abkrs_so IS NOT INITIAL AND ls_t549q IS NOT INITIAL.
      SELECT COUNT( DISTINCT mcw~pernr ) INTO @ev_int4 FROM m_premw AS mcw
        INNER JOIN pa0003 AS it03 ON mcw~pernr = it03~pernr
        WHERE mcw~koabr  =  @zcl_pyc_kpi_ee_locked=>mc_lock_indicator-yes AND
              it03~abrdt <= @ls_t549q-endda                               AND
              it03~sprps =  @if_hrpa_read_infotype=>unlocked              AND
              EXISTS ( SELECT 1
                         FROM pa0000 AS it0000 INNER JOIN pa0001 AS it0001 ON
                              it0000~pernr = it0001~pernr
                        WHERE it0000~pernr = mcw~pernr                          AND
                              it0000~begda <= @ls_t549q-endda                   AND
                              it0000~endda >= @ls_t549q-begda                   AND
                              it0000~stat2 = @cl_eain_constants=>c_pernr_active AND
                              it0000~sprps = @if_hrpa_read_infotype=>unlocked   AND
                              it0001~begda <= @ls_t549q-endda                   AND
                              it0001~endda >= @ls_t549q-begda                   AND
                              it0001~sprps = @if_hrpa_read_infotype=>unlocked   AND
                              it0001~abkrs IN @lt_abkrs_so ).
    ENDIF.

    CLEAR ev_kpi_color. "take default
    ev_kpi_trend = cl_pyc_rt_facade=>gcs_kpi_trend-neutral.
    CLEAR ev_kpi_style. "-> take default
    CLEAR ev_p_type. "let framework decide
    CLEAR: ev_maxbt, ev_waers. "to be on the safe side

  ENDMETHOD.
ENDCLASS.
