"Name: \PR:HNZLEMPD0\FO:DOWNLOAD_EXCEP_FILE_BG\SE:BEGIN\EI
ENHANCEMENT 0 ZUSEHRNZPY_EI_PCC_EDS_REPORT.
*-----------------------------------------------------------------------*
* Description   : This Enhancement to standard SAP program is to        *
*                 display Error log Report during the batch execution   *
*                 This will alow the PCC execution to present both the  *
*                 outputs to payroll administrators                     *
*                                                                       *
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |23-AUG-2023 |1130848  |Initial Implementation       |CFAK903752   *
*-----------------------------------------------------------------------*
  check sy-batch = 'X' AND ch_fdown = 'X'.
ENDENHANCEMENT.
