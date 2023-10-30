METHOD get_rule_summary .
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |17-08-2021 |1130843|get rule codes from parameters in |PTX-2616      |CFAK900511        *
*                         variable MV_SEL_PARAMS_AFTER_SPLIT.                                 *
*                         Please note that the variable                                       *
*                         is a custom one and needs to be                                     *
*                         populated in method SPLIT_SELECTION                                 *
*---------------------------------------------------------------------------------------------*
  CONSTANTS: lc_instance         TYPE rsscr_name VALUE 'SO_INST',
             lc_par_company_code TYPE pyd_par_type VALUE 'ABKRS'.
  IF iv_row_id IS NOT SUPPLIED.
    IF lines( mv_sel_params_after_split ) > 1.
      rv_code_summary = iv_job_base_name.
      EXIT.
    ENDIF.
  ENDIF.
  LOOP AT mv_sel_params_after_split INTO DATA(ls_split).
    IF iv_row_id IS SUPPLIED AND iv_row_id IS NOT INITIAL.
      CHECK ls_split-row_id = iv_row_id.
    ENDIF.

    "make sure that there is only one record otherwise exit
    DATA(lv_lines) = REDUCE i( INIT x = 0 FOR ls_sel_params IN ls_split-sel_params
                     WHERE ( selname = lc_instance ) NEXT x = x + 1 ).
    IF lv_lines > 1.
      rv_code_summary = iv_job_base_name.
      EXIT.
    ENDIF.

    LOOP AT ls_split-sel_params INTO DATA(ls_param) WHERE selname = lc_instance.
      rv_code_summary = get_rule_code( ls_param-low ).
      EXIT.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.
