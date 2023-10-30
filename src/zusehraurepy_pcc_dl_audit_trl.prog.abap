report zusehraurepy_pcc_dl_audit_trl.
*-----------------------------------------------------------------------*
* Program Name  : ZUSEHRAUREPY_PCC_DL_AUDIT_TRAIL                       *
* Title         : Download Audit Trail for Process ID's                 *
*-----------------------------------------------------------------------*
* Description   : Wrapper program to execute standard SAP program       *
*                 PYC_SUPPORT_DL_AUDIT_TRAIL in automated way, so that  *
*                 Download of Audit Trail for PCC Payroll Process       *
*                 Instances can be scheduled as a background job        *
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
include zusehraurepy_pcc_dl_audit_tr01. " Application Class
include zusehraurepy_pcc_dl_audit_tr02. " Selection

start-of-selection.
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

* Read Process Instancess
  create object g_application
    exporting
      it_proc_id_so = gt_pyp_id
      iv_begda      = p_begda
      iv_endda      = p_endda
      iv_variant    = p_vari.

end-of-selection.
* Execute PYC_SUPPORT_DL_AUDIT_TRAIL for selected process instances
  g_application->check_report_auth( ).
  g_application->download_proc_inst( ).
