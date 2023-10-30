CLASS lcl_ZUSEHRAUPY_EI_PCC_REP_PAR DEFINITION DEFERRED.
CLASS cl_pyc_bpc_report_parallel DEFINITION LOCAL FRIENDS lcl_ZUSEHRAUPY_EI_PCC_REP_PAR.
CLASS lcl_ZUSEHRAUPY_EI_PCC_REP_PAR DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_ZUSEHRAUPY_EI_PCC_REP_PAR.  "#EC NEEDED
    DATA core_object TYPE REF TO cl_pyc_bpc_report_parallel . "#EC NEEDED
 INTERFACES  IPO_ZUSEHRAUPY_EI_PCC_REP_PAR.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO cl_pyc_bpc_report_parallel OPTIONAL.
ENDCLASS.
CLASS lcl_ZUSEHRAUPY_EI_PCC_REP_PAR IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD ipo_ZUSEHRAUPY_EI_PCC_REP_PAR~split_selection.
*"------------------------------------------------------------------------*
*" Declaration of POST-method, do not insert any comments here please!
*"
*"methods SPLIT_SELECTION
*"  importing
*"    !IT_SEL_PARAMS type RSPARAMS_TT
*"    !IO_RES_CONTEXT type ref to IF_PYD_RES_CONTEXT optional
*"  changing
*"    value(RT_SEL_PARAMS) type CL_PYC_BPC_REPORT_PARALLEL=>TY_T_SEL_PARAMS
*"  raising
*"    CX_PYC_CONT . "#EC CI_VALPAR
*"------------------------------------------------------------------------*
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |17-10-2021 |1130848|Read Process Name for Jobname Chg |PTX-3763      |CFAK901328        *
*---------------------------------------------------------------------------------------------*
    DATA: lv_process_country_grouping TYPE pyc_d_pyp-molga.
    core_object->mv_sel_params_after_split = rt_sel_params.
    IF io_res_context IS BOUND.
      READ TABLE io_res_context->mt_par INTO DATA(ls_par)
        WITH KEY par_type = cl_pyc_dt_proc_processes=>mc_pyp_proc.
      IF sy-subrc = 0.
        SELECT SINGLE name INTO
          @core_object->mv_process_name
          FROM pyc_d_pypt
          WHERE id = @ls_par-low.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
