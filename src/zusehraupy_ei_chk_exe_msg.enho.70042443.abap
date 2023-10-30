"Name: \TY:CL_PYD_INST_RT\IN:IF_PYD_INST_RT\ME:INSTANCE_EXECUTE\SE:BEGIN\EI
ENHANCEMENT 0 ZUSEHRAUPY_EI_CHK_EXE_MSG.
*>>> Start of PTX-3763 - PCC Implementation
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |14-09-2021 |1130848|Check Start and End Messages      |PTX-3763      |CFAK900819        *
*---------------------------------------------------------------------------------------------*
* Report Check Execution Start time
  GET TIME.
  MESSAGE s008(ZHRPY_PCC_MSG) WITH iv_id sy-datum sy-uzeit.
*<<< End of PTX-3763 - PCC Implementation
ENDENHANCEMENT.
