report zhraurepy_del_dct_data.
*-----------------------------------------------------------------------*
* Program Name  : ZHRAUREPY_DEL_DCT_DATA
* Title         : Delete Test Payroll Results from Declustered Tables
* Create Date   : 28.10.2021
* Release       : ECC 6.0
* Author        : 1130848
*-----------------------------------------------------------------------*
* Description   : This is a copy of standard SAP Decluster data deletion
* program rpcdct_del_dct_data and you can use this program to delete
* test payroll results only. Not productive payroll results, not the
* data in cluster tables PCLx and you can run the report bofore running
* payroll driver in Payroll Monitoring and Test payroll processes to
* speed up the payroll driver execution.
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Mod | Date      | User ID  |Description                    |Change Req *
*-----------------------------------------------------------------------*
*001 |28-10-2021 | 1130848  |PTX-3763 Initial creation      |CFAK901075 *
*-----------------------------------------------------------------------*
include zhraurepy_del_dct_data_def.
*INCLUDE rpcdct_del_dct_data_definition.           "Data Definition

include zhraurepy_del_dct_data_screen.
*INCLUDE rpcdct_del_dct_data_screen.               "Selection Screen

include zhraurepy_del_dct_data_main.
*INCLUDE rpcdct_del_dct_data_main.                 "Main Program

include zhraurepy_del_dct_data_forms.
*INCLUDE rpcdct_del_dct_data_forms.                "Subroutines
