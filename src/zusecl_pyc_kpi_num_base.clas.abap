class ZUSECL_PYC_KPI_NUM_BASE definition
  public
  inheriting from CL_PYC_KPI_NUMERIC_BASE
  create public

  global friends CL_PYD_FND_AUX .

public section.
protected section.

  types:
    BEGIN OF ty_pay_period_dates,
           begda TYPE t549q-begda,
           endda TYPE t549q-endda,
         END OF ty_pay_period_dates .

  methods GET_PARAMETERS
    importing
      !IO_RES_CONTEXT type ref to IF_PYD_RES_CONTEXT
    exporting
      !ET_ABKRS_SO type /IWBEP/T_COD_SELECT_OPTIONS
      !ES_PAY_PERIOD_DATES type TY_PAY_PERIOD_DATES
    raising
      CX_PYD_FND .
private section.
ENDCLASS.



CLASS ZUSECL_PYC_KPI_NUM_BASE IMPLEMENTATION.


  METHOD GET_PARAMETERS.

* Get parameters commonly used for numerical analytic charts

    DATA: ls_resp          LIKE LINE OF io_res_context->mt_par,
          lv_period        TYPE fpper,
          lv_leading_abkrs TYPE abkrs,
          lv_permo         TYPE t549a-permo.
    CLEAR: et_abkrs_so, es_pay_period_dates.
    LOOP  AT io_res_context->mt_par INTO ls_resp
      WHERE par_type = if_pyd_cont_types=>gcs_par_type-abkrs.
      IF lv_leading_abkrs IS INITIAL.
        lv_leading_abkrs = ls_resp-low.
      ENDIF.
      CALL METHOD cl_pyd_fnd_aux=>append_so_fixed_value(
        EXPORTING
          iv_value = ls_resp-low
        CHANGING
          ct_so    = et_abkrs_so ).
    ENDLOOP.

    READ TABLE io_res_context->mt_par INTO ls_resp
      WITH KEY par_type = if_pyd_cont_types=>gcs_par_type-period.
    IF sy-subrc = 0.
      lv_period = ls_resp-low.
    ENDIF.

    IF lv_period IS NOT INITIAL AND lv_leading_abkrs IS NOT INITIAL.
      SELECT SINGLE permo INTO lv_permo FROM t549a WHERE abkrs = lv_leading_abkrs.
      IF sy-subrc = 0.
        "table t549q is a pooled table and it cannot be used in a JOIN.
        "Thus, keep query to table t549q as a separate query.
        SELECT SINGLE begda endda INTO CORRESPONDING FIELDS OF es_pay_period_dates
          FROM t549q
          WHERE permo = lv_permo       AND
                pabrj = lv_period(4)   AND
                pabrp = lv_period+4(2).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
