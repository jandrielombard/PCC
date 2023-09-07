*&---------------------------------------------------------------------*
*& Report           : ZHRAUREPY_PCC_AUDIT_FILEDL00                     *
*& Tiltle           : PCC Audit Trail - Files Download                 *
*& Transactio Code  : ZHRPY_M99_AUDIT_FILES                            *
*& Create Date      : 21 December 2022                                 *
*& Release          : ECC 6.0                                          *
*& Author           : Satya Aluru                                      *
*&                                                                     *
*&  This custom program has been created to download PCC Audit Trail   *
*&  files from logical file path PCC_AL_AH                             *
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |22-DEC-2022 |1130848  |Initial creation             |CFAK902680   *
*-----------------------------------------------------------------------*
report zhraurepy_pcc_audit_filedl00 line-size 132.

include: zhraurepy_pcc_audit_filedl01,  " Data Definition and Sel Screen
         zhraurepy_pcc_audit_filedl02,  " Main Routines
         zhraurepy_pcc_audit_filedl03,  " Inbound Functions
         zhraurepy_pcc_audit_filedl04.  " Outbound Functions

************************************************************************
