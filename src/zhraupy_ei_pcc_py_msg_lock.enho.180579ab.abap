"Name: \TY:CL_PYC_RT_PAYROLL_MSG\ME:INSERT_MSG_WITH_DIFF_PARA\SE:END\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_PY_MSG_LOCK.
*----------------------------------------------------------------------------------------------*
* CHANGE HISTORY
*----------------------------------------------------------------------------------------------*
*Mod | Date      | User ID|Description                       |Change Label  |Workbench Request *
*----------------------------------------------------------------------------------------------*
*001 |29-07-2022 | 1130848| Lock Object for PYC_D_PY_MSG     | PTX-3763     |CFAK902076        *
*----------------------------------------------------------------------------------------------*
* Unlock Employee Records
  call method zcl_m99_pcc_utilities=>dequeue_py_msg
    exporting
      iv_pernr    = ls_ei_msgtab-pernr
      iv_run_type = iv_run_type.
ENDENHANCEMENT.
