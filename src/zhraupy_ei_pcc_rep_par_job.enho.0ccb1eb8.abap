"Name: \TY:CL_PYC_BPC_REPORT_PARALLEL\ME:GET_JOB_NAME\SE:END\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_REP_PAR_JOB.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |18-10-2021 |1130848|change background job name for    |PTX-3763      |CFAK900994        *
*                         Process Step Jobs.                                                  *
*---------------------------------------------------------------------------------------------*
* Add Process template name for all Process Step Job Names
* In case of Parallel Processing replace first 2 Char with Process template name
  if rv_result+0(5) CO lc_numbers and rv_result+5(1) CO lc_parallel_id_separator.
    lv_result_temp = |{ mv_process_name(4) }{ lc_parallel_id_separator }| &&
                     |{ rv_result+2 }|.
  else.
* In case of standard process step prefix the name with Process template name
    REPLACE ALL OCCURRENCES OF '_00' IN rv_result WITH '_'.
    lv_result_temp = |{ mv_process_name(4) }{ lc_parallel_id_separator }| &&
                     |{ rv_result }|.
  endif.
* Adjust the name to max length
  DESCRIBE FIELD rv_result LENGTH lv_max_length IN CHARACTER MODE.
  IF strlen( lv_result_temp ) > lv_max_length.
    rv_result = lv_result_temp(lv_max_length).
  ELSE.
    rv_result = lv_result_temp.
  ENDIF.
ENDENHANCEMENT.
