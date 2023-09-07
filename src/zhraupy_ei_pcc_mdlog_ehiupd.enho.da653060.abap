"Name: \TY:CL_PYC_MONI_STEP_STATUS\IN:IF_PYC_MONI_STEP_STATUS\ME:SET_STARTED\SE:END\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_MDLOG_EHIUPD.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |26-08-2021 |1130848|Event Handler Table Update Using  |PTX-3763      |CFAK900994        *
*    |           |       |Master Data Changes Log           |              |                  *
*---------------------------------------------------------------------------------------------*
* UpDate event handler table for all changes happened during the Payroll Processing
  try.
    call method zcl_m99_pcc_mdlog_ehiupdate=>submit_mdlog_ehiupdate
      exporting
        iv_pypi_id = iv_pypi_id.
   catch cx_pyc_cont .
  endtry.

ENDENHANCEMENT.
