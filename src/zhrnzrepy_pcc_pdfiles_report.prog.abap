*&----------------------------------------------------------------------*
*& Report           : ZHRAUREPY_PCC_AUDIT_FILEDL00                      *
*& Tiltle           : PCC NZ Pay Day Reports Exception Files Download   *
*& Transactio Code  : ZHRPY_M99_PDFILES                                 *
*& Create Date      : 06 February 2023                                  *
*& Release          : ECC 6.0                                           *
*& Author           : Satya Aluru                                       *
*&                                                                      *
*&  This custom program has been created to report NZ Pay Day reportsd  *
*&  exception files from logical file path HR_NZ_EMS_FILENAME           *
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |06-Feb-20223 |1130848  |Initial creation             |CFAK902801  *
*-----------------------------------------------------------------------*
report zhrnzrepy_pcc_pdfiles_report line-size 132.

include: zhrnzrepy_pcc_pdfiles_rep01,     " Data Definition and Sel Screen
         zhrnzrepy_pcc_pdfiles_rep02,     " Main Routines
         zhrnzrepy_pcc_pdfiles_rep03,     " Inbound Functions
         zhrnzrepy_pcc_pdfiles_rep04.     " Outbound Functions

************************************************************************
