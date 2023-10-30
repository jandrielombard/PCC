report zuserepy_pcc_del_completed_pi.
*-----------------------------------------------------------------------*
* Program Name  : ZUSEREPY_PCC_DEL_COMPLETED_PI                         *
* Title         : Delete Completed Process Instances                    *
*-----------------------------------------------------------------------*
* Description   : Custom Program developed using standard SAP program   *
*                 PYC_SUPPORT_DEL_COMPLETED_PI to make the program      *
*                 compatable for background execution                   *
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
INCLUDE ZUSEREPY_PCC_DEL_PI_CLS.
*include zhraurepy_pcc_del_pi_cls.    "class
INCLUDE ZUSEREPY_PCC_DEL_PI_DS.
*include zhraurepy_pcc_del_pi_ds.     "data declaration and screen
INCLUDE ZUSEREPY_PCC_DEL_PI_MODULE.
*include zhraurepy_pcc_del_pi_module. "modules (pbo & pai)

start-of-selection.
*>>> Start of MOD001++
* Slect All existing Process ID's for * selection
  if s_pid-low eq '*'.
    select id into table gt_proc_id from pyc_d_pyp.
    loop at gt_proc_id into data(gs_proc_id).
      try.
          call method cl_pyd_fnd_aux=>append_so_fixed_value
            exporting
              iv_value = gs_proc_id
            changing
              ct_so    = gt_proc_id_so.
        catch cx_pyd_fnd .
      endtry.
    endloop.
    move-corresponding gt_proc_id_so[] to gt_pyp_id[].
  else.
    move-corresponding s_pid[] to gt_pyp_id[].
  endif.
*<<< End of MOD001++
  create object g_application
    exporting
*     it_proc_id_so = value #( ( sign = 'I' option = 'EQ' low = p_pid ) )
      "MOD001--
      it_proc_id_so = gt_pyp_id                 "MOD001++
      iv_endda      = p_endda.

end-of-selection.
  g_application->check_report_auth( ).

  "screen for display alv grid
***  call screen 100.                            "MOD001--
  if p_test eq abap_false.                       "MOD001++
    g_application->delete_proc_inst( ).          "MOD001++
  endif.                                         "MOD001++
  g_application->display_process_instances( ).   "MOD001++
