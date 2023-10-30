METHOD get_job_names_to_change .
  CONSTANTS: lc_check_job_base_name   TYPE string VALUE 'Execute Check Instances',
             lc_kpi_job_base_name     TYPE string VALUE 'Execute KPI Instances',
             lc_object_class          TYPE rs38m-programm VALUE 'CL_PYC_STT_EXECUTE_POLICIES',
             lc_par_type_payroll_area TYPE pyd_par_type VALUE 'ABKRS',
             lc_key_check             TYPE textpoolky VALUE '006',
             lc_key_kpi               TYPE textpoolky VALUE '007'.
  DATA: lv_check_job_base_name TYPE string,
        lt_text                TYPE STANDARD TABLE OF textpool.
  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname           = lc_object_class
      action               = swbm_c_op_display
    TABLES
      tpool                = lt_text
    EXCEPTIONS
      object_not_found     = 1
      permission_failure   = 2
      invalid_program_type = 3
      error_occured        = 4
      action_cancelled     = 5
      OTHERS               = 6.
  IF sy-subrc = 0.
    LOOP AT lt_text INTO DATA(ls_text) WHERE key = lc_key_check OR key = lc_key_kpi.
      APPEND VALUE #( sign = if_dmf_constants_c=>gc_range_sign_inclusive
        option = if_dmf_constants_c=>gc_range_option_equal
        low = ls_text-entry ) TO rt_job_base_names.
    ENDLOOP.
  ENDIF.

  IF rt_job_base_names IS INITIAL.
    rt_job_base_names = value #(
      ( sign = if_dmf_constants_c=>gc_range_sign_inclusive
        option = if_dmf_constants_c=>gc_range_option_equal
        low = lc_check_job_base_name )
      ( sign = if_dmf_constants_c=>gc_range_sign_inclusive
        option = if_dmf_constants_c=>gc_range_option_equal
        low = lc_kpi_job_base_name ) ).
  ENDIF.
ENDMETHOD.
