*-----------------------------------------------------------------------*
* Program Name  : ZUSEREPY_PCC_MD_CHANGE_ASSGN                          *
* Title         : Update table PYC_D_EE_MDCA for employees who need     *
*                 master data changes via PA30                          *
*-----------------------------------------------------------------------*
* Description   : PCC: Unable to maintain employee data when payroll    *
*                 control record is released. This creates a workaround *
*                 to still update master data                           *
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
report zuserepy_pcc_md_change_assgn.

tables pernr.

*data: lt_pernr type table of PYC_D_EE_MDCA.


initialization.


start-of-selection.

*get pernr.
  data(lt_pernr) =   value PYC_D_EE_MDCA( ).

end-of-selection.
