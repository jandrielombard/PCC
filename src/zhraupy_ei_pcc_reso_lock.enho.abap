class lcl_zhraupy_ei_pcc_reso_lock definition deferred.
class cl_pyd_result_rt definition local friends lcl_zhraupy_ei_pcc_reso_lock.
class lcl_zhraupy_ei_pcc_reso_lock definition.
  public section.
    class-data obj type ref to lcl_zhraupy_ei_pcc_reso_lock. "#EC NEEDED
    data core_object type ref to cl_pyd_result_rt .         "#EC NEEDED
 INTERFACES  IOW_ZHRAUPY_EI_PCC_RESO_LOCK.
    methods:
      constructor importing core_object
                              type ref to cl_pyd_result_rt optional.
endclass.
class lcl_zhraupy_ei_pcc_reso_lock implementation.
  method constructor.
    me->core_object = core_object.
  endmethod.

  method iow_zhraupy_ei_pcc_reso_lock~result_object_lock_by_reskey.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods RESULT_OBJECT_LOCK_BY_RESKEY
*"  importing
*"    !IS_RES_KEY type PYD_S_RES_KEY
*"  raising
*"    CX_PYD_FND .
*"------------------------------------------------------------------------*

    call function 'ENQUEUE_EPYD_D_RESO'
      exporting
*>>> Start of WOW Specific Code
        mode_pyd_d_reso = 'V'
*<<< End of WOW Specific Code
*       MANDT           = SY-MANDT
        instid          = is_res_key-instid
        par_hash        = is_res_key-par_hash
*       par_type        =
*       id              =
*       X_INSTID        = ' '
*       X_PAR_HASH      = ' '
*       X_PAR_TYPE      = ' '
*       X_ID            = ' '
*       _SCOPE          = '2'
*>>> Start of WOW Specific Code
        _wait           = abap_true
*<<< End of WOW Specific Code
*       _COLLECT        = ' '
      exceptions
        foreign_lock    = 1
        system_failure  = 2
        others          = 3.

    if sy-subrc <> 0.
      raise exception type cx_pyd_fnd exporting textid = cx_pyd_fnd=>reso_no_lock.
    endif.

  endmethod.
ENDCLASS.
