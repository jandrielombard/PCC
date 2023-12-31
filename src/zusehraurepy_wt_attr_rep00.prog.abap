*&---------------------------------------------------------------------*
*& Report           : ZUSEHRAUREPY_WT_ATTR_REP00                       *
*& Tiltle           : Wagetype Attributes Report                       *
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
report zusehraurepy_wt_attr_rep00 message-id 54  line-size 132.

include : zusehraurepy_wt_attr_rep01,
          zusehraurepy_wt_attr_rep02,
          zusehraurepy_wt_attr_rep03,
          zusehraurepy_wt_attr_rep04.
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

  include :  zusehraurepy_wt_attr_rep05.
