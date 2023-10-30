report zusehraurepy_del_dct_data.
*-----------------------------------------------------------------------*
* Program Name  : ZUSEHRAUREPY_DEL_DCT_DATA
* Title         : Delete Test Payroll Results from Declustered Tables
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

include zusehraurepy_del_dct_data_def.
*INCLUDE rpcdct_del_dct_data_definition.           "Data Definition

include zusehraurepy_del_dct_data_scrn.
*INCLUDE rpcdct_del_dct_data_screen.               "Selection Screen

include zusehraurepy_del_dct_data_main.
*INCLUDE rpcdct_del_dct_data_main.                 "Main Program

include zusehraurepy_del_dct_data_form.
*INCLUDE rpcdct_del_dct_data_forms.                "Subroutines
