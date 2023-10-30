*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_DEL_DCT_DATA_SCREEN
*&---------------------------------------------------------------------*


selection-screen begin of block cluster with frame title text-001.
parameters:
  p_pclx type t77dct_reg-pcltabname default 'PCL2'.

parameters:
  p_relid type t77dct_reg-relid obligatory.

selection-screen end of block cluster.

selection-screen begin of block others with frame title text-002.



parameters:
  p_sim type hrdct_sim as checkbox default abap_false.
parameters:
  p_tpy type hrdct_tpy as checkbox default abap_true modif id tpy.
*>>> Start of WOW Specific Enhancements
* PARAMETERS:
*    p_tpy TYPE hrdct_tpy AS CHECKBOX DEFAULT abap_false.
*  PARAMETERS:
*    p_pdt TYPE hrdct_pdt AS CHECKBOX DEFAULT abap_false.
*  SELECT-OPTIONS p_date FOR gv_date NO-EXTENSION.
parameters:
  p_pdt type hrdct_pdt default abap_false no-display.
select-options p_date for gv_date no-extension no-display.
*<<< End of WOW Specific Enhancements
parameters:
  p_log type hrdct_dtlog as checkbox default abap_false.
selection-screen end of block others.
