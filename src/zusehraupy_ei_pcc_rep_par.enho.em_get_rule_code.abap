METHOD get_rule_code .
* This method is to extract parts of the name of the payroll data source type
* to construct job name
  CONSTANTS: lc_underscore       TYPE c VALUE '_',
             lc_pattern_kpi      TYPE string VALUE '*KPI*',
             lc_pattern_spinifex TYPE string VALUE '*SPINIFEX*',
             lc_pattern_check    TYPE string VALUE 'V*',
             lc_regex_kpi        TYPE string VALUE 'KPI.*',
             lc_regex_spinifex   TYPE string VALUE 'SPINIFEX_CHK_.{3}',
*             lc_regex_check_type TYPE string VALUE '(V[0-9]{4})_M[0-9]{2}_(.*)',
             lc_regex_check_type TYPE string VALUE '(V[0-9]{4})_ZUSE_(.*)',
*             lc_regex_check      TYPE string VALUE '(V[0-9]{4})_M[0-9]{2}_(.{0,13})'.
             lc_regex_check      TYPE string VALUE '(V[0-9]{4})_ZUSE_(.{0,13})'.
  DATA: lo_matcher       TYPE REF TO cl_abap_matcher,
        lv_success       TYPE abap_bool,
        lv_match_result  TYPE match_result,
        lv_regex_pattern TYPE string,
        lv_pyd_type      TYPE pyd_type,
        lv_text          TYPE string.
  TRY.
      lv_pyd_type = get_data_source_type( CONV pyd_instid( iv_rule ) ).

      IF iv_rule CP lc_pattern_kpi OR lv_pyd_type CP lc_pattern_kpi.
        lv_regex_pattern = lc_regex_kpi.
      ELSEIF lv_pyd_type CP lc_pattern_spinifex.
        lv_regex_pattern = lc_regex_spinifex.
      ELSEIF lv_pyd_type CP lc_pattern_check.
        IF lv_pyd_type IS NOT INITIAL.
          lv_regex_pattern = lc_regex_check_type.
        ELSE.
          lv_regex_pattern = lc_regex_check.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.

      IF lv_pyd_type IS NOT INITIAL.
        lv_text = lv_pyd_type.
      ELSE.
        lv_text = iv_rule.
      ENDIF.

      lo_matcher = cl_abap_matcher=>create(
        pattern     = lv_regex_pattern
        ignore_case = abap_true
        text        = lv_text ).
      IF lo_matcher IS BOUND.
        IF lo_matcher->find_next( ) = abap_true.

          lv_match_result = lo_matcher->get_match( ).

          IF lv_match_result-submatches IS NOT INITIAL.
            LOOP AT lv_match_result-submatches INTO DATA(ls_submatch).
              IF rv_rule_code IS INITIAL.
                rv_rule_code = lv_text+ls_submatch-offset(ls_submatch-length).
              ELSE.
                rv_rule_code = |{ rv_rule_code }{ lc_underscore }| &&
                  |{ lv_text+ls_submatch-offset(ls_submatch-length) }|.
              ENDIF.
            ENDLOOP.
            IF sy-subrc = 0.
              SHIFT rv_rule_code RIGHT DELETING TRAILING lc_underscore.
              SHIFT rv_rule_code LEFT DELETING LEADING space.
            ENDIF.
          ELSE.
            rv_rule_code = lv_text+lv_match_result-offset(lv_match_result-length).
          ENDIF.
        ENDIF.
      ENDIF.
    CATCH cx_sy_regex cx_sy_matcher.
  ENDTRY.
ENDMETHOD.
