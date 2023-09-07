*&---------------------------------------------------------------------*
*& Report           : ZHRAUREPY_WT_ATTRIBUTES_REP00                    *
*& Tiltle           : Wagetype Attributes Report                       *
*& Transactio Code  :                                                  *
*& Create Date      : 17 Aug 2021                                      *
*& Release          : ECC 6.0                                          *
*& Author           : Satya Aluru                                      *
*&                                                                     *
*& Custom utility program similer to standard wage type utilization    *
*& report,  reports selected wage types attributes in various  user    *
*& friendly formats                                                    *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*&---------------------------------------------------------------------*
*& DATE        User    Description                          TR Number  *
*&---------------------------------------------------------------------*
*&                                                                     *
*&---------------------------------------------------------------------*
report zhraurepy_wt_attributes_rep00 message-id 54  line-size 132.

include : zhraurepy_wt_attributes_rep01,
          zhraurepy_wt_attributes_rep02,
          zhraurepy_wt_attributes_rep03,
          zhraurepy_wt_attributes_rep04.
*
***********************************
* AT SELECTION SCREEN VALIDATION
***********************************

at selection-screen.
  select single * from t500l into gs_t500l where molga = modlgart.
  if sy-subrc ne 0.
    message e587(zazk_msg).
  endif.

  intca = gs_t500l-intca.

***********************************
* START OF SELECTION
***********************************

start-of-selection.

  select single * from t005t where spras = sy-langu
               and land1 = intca.
  if sy-subrc ne 0.
    message e587(zazk_msg).
  else.
    move t005t-landx to molgat.
  endif.

  perform main.

**************************************
* END OF SELECTION
**************************************

end-of-selection.
*Report the list.

*   IF DETAIL = 'X'.
  if matrix = ' ' .

    if withowt ='X'.
      sort itab by lgart srtseq col1.
      perform display_report tables itab.
    elseif withwt = 'X'.
      sort itab by lgart srtseq col1.
      itab1[] = itab[].
      delete adjacent duplicates from itab1 comparing lgart lgtxt.
      perform display_report tables itab1.
    else.
      delete itab2 where molga is initial.
      perform display_report tables itab2.
    endif.
  else.
    perform display_report tables <exceltab>.
  endif.

  include :  zhraurepy_wt_attributes_rep05.
