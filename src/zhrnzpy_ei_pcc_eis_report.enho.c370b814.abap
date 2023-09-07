"Name: \PR:HNZLPDS0\FO:DISPLAY_OUTPUT\SE:END\EI
ENHANCEMENT 0 ZHRNZPY_EI_PCC_EIS_REPORT.
*-----------------------------------------------------------------------*
* Description   : This Enhancement to standard SAP program is to        *
*                 display both Exceptions report and Error log Report   *
*                 during the batch execution                            *
*                 This will alow the PCC execution to present both the  *
*                 outputs to payroll administrators                     *
*                                                                       *
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |09-FEB-2023 |1130848  |Initial Implementation       |CFAK902801   *
*-----------------------------------------------------------------------*
* Report exceptions and Errors for PCC Execution
  data: lv_pcc_lines type i.
  if sy-batch eq abap_true.
    "Exception report
    DESCRIBE TABLE gt_report2 LINES lv_pcc_lines.
    IF NOT lv_pcc_lines IS INITIAL.
      PERFORM create_exception_report USING '&EXC'.
    ELSE.
      MESSAGE i021 WITH text-028.
    ENDIF.

    " Error Report
    DESCRIBE TABLE gt_error LINES lv_pcc_lines.
    IF lv_pcc_lines IS NOT INITIAL.
      PERFORM display_errors USING '&ERR'.
    ELSE.
      MESSAGE i021 WITH text-042.
    ENDIF.
  endif.
*
ENDENHANCEMENT.
