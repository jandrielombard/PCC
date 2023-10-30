"Name: \PR:PYC_SUPPORT_SKIP_PROCESS_STEP\FO:PREPARE_PROCESS_DATA\SE:END\EI
ENHANCEMENT 0 ZUSEHRAUPY_EI_PCC_SKP_PROC_STP.
*----------------------------------------------------------------------------------------------*
* CHANGE HISTORY
*----------------------------------------------------------------------------------------------*
*Mod | Date      | User ID|Description                       |Change Label  |Workbench Request *
*----------------------------------------------------------------------------------------------*
*001 |08-07-2022 | 1130848| Status chg to suite SKIP ProStep | PTX-3763     |CFAK901974        *
*----------------------------------------------------------------------------------------------*
* Enhance Error Status for Confilicting " Execution and Error Status "
  call method zusecl_m99_pcc_utilities=>set_status_for_skip_pro_step
    changing
      ct_steps_tab = gt_steps_tab.

ENDENHANCEMENT.
