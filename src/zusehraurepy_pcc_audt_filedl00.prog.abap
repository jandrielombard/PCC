*&---------------------------------------------------------------------*
*& Report           : ZUSEHRAUREPY_PCC_AUDT_FILEDL00                   *
*& Tiltle           : PCC Audit Trail - Files Download                 *
*& Create Date      : 21 December 2022                                 *
*& Release          : ECC 6.0                                          *
*&                                                                     *
*&  This custom program has been created to download PCC Audit Trail   *
*&  files from logical file path PCC_AL_AH                             *
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
report zusehraurepy_pcc_audt_filedl00 line-size 132.

include: zusehraurepy_pcc_audt_filedl01,  " Data Definition and Sel Screen
         zusehraurepy_pcc_audt_filedl02,  " Main Routines
         zusehraurepy_pcc_audt_filedl03,  " Inbound Functions
         zusehraurepy_pcc_audt_filedl04.  " Outbound Functions

************************************************************************
