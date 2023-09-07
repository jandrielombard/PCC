"Name: \TY:CL_PYC_RT_PAYROLL_MSG\ME:INSERT_MSG_WITH_DIFF_PARA\SE:BEGIN\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_PY_MSG_LOCK.
*----------------------------------------------------------------------------------------------*
* CHANGE HISTORY
*----------------------------------------------------------------------------------------------*
*Mod | Date      | User ID|Description                       |Change Label  |Workbench Request *
*----------------------------------------------------------------------------------------------*
*001 |29-07-2022 | 1130848| Lock Object for PYC_D_PY_MSG     | PTX-3763     |CFAK902076        *
*----------------------------------------------------------------------------------------------*
* Lock only Employee Records to avoid dead lock between multiple PCC jobs
   data: lv_ei_line type i,
        ls_ei_msgtab type pymsg.
* Read Employee Number from MSG table
  describe table it_msgtab lines lv_ei_line.
  read table it_msgtab into ls_ei_msgtab index lv_ei_line.
* Lock Employee Records
  call method zcl_m99_pcc_utilities=>enqueue_py_msg
    exporting
      iv_pernr    = ls_ei_msgtab-pernr
      iv_run_type = iv_run_type.
ENDENHANCEMENT.
