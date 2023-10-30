"Name: \TY:CL_PYC_BPC_REPORT_PARALLEL\ME:SPLIT_SELECTION\SE:END\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_REP_PAR_SPLIT.
*>>> Start of PTX-3763 - PCC Implementation - Load balancing for Validations
* Rearrange the above instances according to the Load category for the better performance
CALL METHOD zusecl_m99_pcc_chk_utilities=>ADJUST_SPLIT_SELECTION
  EXPORTING
    iv_split_param    = lv_split_param
    iv_split_interval = lv_split_interval
  CHANGING
    ct_sel_params     = rt_sel_params.
*<<< of PTX-3763 - PCC Implementation - Load balancing for Validations
ENDENHANCEMENT.
