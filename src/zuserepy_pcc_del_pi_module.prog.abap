*&---------------------------------------------------------------------*
*&  Include           PYC_SUPPORT_DEL_PI_MODULE
*&---------------------------------------------------------------------*
*>>> Start of MOD001--
***"PBO MODULES
***MODULE pbo100 OUTPUT.
***  SET PF-STATUS 'MAIN'.
***  SET TITLEBAR 'TITLE'.
***
***  g_application->show_docking_controls( ).
***ENDMODULE.
***
***"PAI MODULES
***MODULE pai100 INPUT.
***  CASE g_ok_code.
***    WHEN 'E' OR 'ENDE' OR 'ECAN'.
***      SET SCREEN 0. LEAVE SCREEN.
***    WHEN OTHERS.
***  ENDCASE.
***
***ENDMODULE.
*<<< End of MOD001--

*>>> Start of MOD001++
form top_of_page.
* Header Record for Process Instances
  data: lv_msg type string.

  write:/1 sy-title .
  if p_test eq abap_true.
    message i000(zhrpy_pcc_msg) with text-010 into lv_msg.
  else.
    message i000(zhrpy_pcc_msg) with text-011 into lv_msg.
  endif.
  write:/1 lv_msg.
  write:/.

endform.
*<<< End of MOD001++
