*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_DEL_DCT_DATA_MAIN
*&---------------------------------------------------------------------*
at selection-screen output.
  perform date_option_check.

start-of-selection.

  perform initialization.
  perform get_hrdct_tables using gt_hrdct_d_tables.

get pernr.

  perform get_del_pclkey_tab using gt_pclkeys.
  perform get_pernr_so changing pernr_so.

*>>> Start of WOW Specific Code
*  IF gv_count < 20.
* Read number of Employees for job from TVARVC tavle
  select single low from tvarvc
   into @gv_dct_var
  where name eq @gc_tvarvc-dct_del
    and type eq @gc_tvarvc-type_p.
  if sy-subrc eq 0.
    gv_emp_num = gv_dct_var.
  else.
    gv_emp_num = 500.
  endif.
  if gv_count < gv_emp_num.
*<<< End Of WOW Specific Code
    gv_count = gv_count + 1.
  else.

    perform del_data.
    perform del_tpy_rgdir.
    perform del_pcl2_xt.
    if p_sim = abap_false.
      commit work.
*>>> Start of WOW Specific Code
    else.
      rollback work.
    endif.
*<<< End Of WOW Specific Code
    gv_count = 0.
    clear gt_pclkeys.
    clear pernr_so.
  endif.
  gv_pernr_count = gv_pernr_count + 1.

end-of-selection.

  perform del_data.
  perform del_tpy_rgdir.
  perform del_pcl2_xt.

  if p_sim = abap_false.
    commit work.
*>>> Start of WOW Specific Code
  else.
    rollback work.
  endif.
*<<< End Of WOW Specific Code

  perform log_display.
