"Name: \TY:CL_PYC_EVENT_HANDLER_BASE\IN:IF_PYC_EVENT_HANDLER\ME:EVENT_HANDLER_ITEM_GET_LIST\SE:END\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_EHEMPL_CONTROL.
*>>> Start of PTX-3763 - PCC Implementation - Event Handler Employee List Control
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |21-11-2022 |1130848|Event Handler List Control        |PTX-3763      |CFAK902560        *
*    |           |       |Initial Implementation            |              |                  *
*---------------------------------------------------------------------------------------------*
call method zcl_m99_pcc_utilities=>adjust_event_handler_item_list
  changing
    ct_ehi = rt_ehi.
*<<< End of PTX-3763 - PCC Implementation - Event Handler Employee List Control
ENDENHANCEMENT.
