class lcl_zhraupy_ei_pcc_pnp_pernr definition deferred.
class cl_pyc_bpc_selection_pnp_pernr definition local friends lcl_zhraupy_ei_pcc_pnp_pernr.
class lcl_zhraupy_ei_pcc_pnp_pernr definition.
  public section.
    class-data obj type ref to lcl_zhraupy_ei_pcc_pnp_pernr. "#EC NEEDED
    data core_object type ref to cl_pyc_bpc_selection_pnp_pernr . "#EC NEEDED
 INTERFACES  IOW_ZHRAUPY_EI_PCC_PNP_PERNR.
    methods:
      constructor importing core_object
                              type ref to cl_pyc_bpc_selection_pnp_pernr optional.
endclass.
class lcl_zhraupy_ei_pcc_pnp_pernr implementation.
  method constructor.
    me->core_object = core_object.
  endmethod.

  method iow_zhraupy_ei_pcc_pnp_pernr~get_specific_params.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods GET_SPECIFIC_PARAMS
*"  importing
*"    !IO_CONTEXT type ref to IF_PYD_RES_CONTEXT
*"    !IV_SHADOW_ID type PYD_SHADOW_ID
*"    !IV_RPT_CHAIN_ID type PYC_RPT_CHAIN_ID optional
*"    !IV_RPT_NAME type SYREPID optional
*"    !IV_VAR_NAME type VARIANT optional
*"    !IV_RPT_IS_PNPCE type BOOLE_D optional
*"  exporting
*"    !ET_SEL_PARAMS type RSPARAMS_TT
*"  raising
*"    CX_PYC_CONT .
*"------------------------------------------------------------------------*
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |28-MAR-2023 |1130848  |Changes to Employee        |CFAK903066   *
*    |            |         |Selection SQL              |             *
*---------------------------------------------------------------------*
    data: lv_selname      type char8,
          lv_begda        type datum,
          lv_endda        type datum,
          lv_abkrs        type abkrs,
          lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_permo        type permo,
          ls_par          type pyd_s_resp,
          lv_pa0001_pernr type pernr_d,
          ls_sel_params   type rsparams.

    data: lt_abkrs  type hrpy_tt_abkrs,
          ls_abkrsr type abkrs_cd.
*>>> Start of WOW Specific Code Change
*    data: lt_pernr type if_pyc_process_prog=>t_pernr,
*          ls_pernr type if_pyc_process_prog=>s_pernr.
    types:
      begin of line_pernr,
        pernr type p_pernr,
        abkrs type abkrs,
      end of line_pernr.
    data: lt_pernr type standard table of line_pernr,
          ls_pernr type line_pernr.
    constants gcv_par_selname type  pyd_par_type  value 'PYP_SELNAME'.
*<<< End of WOW Specific Code Change

    constants: ini_pernr type pernr value '00000000'.

    loop at io_context->mt_par into ls_par.

      case ls_par-par_type.
        when 'ABKRS'.
          lv_abkrs = ls_par-low.
          if lv_abkrs is not initial.
            move-corresponding ls_par to ls_abkrsr.
            move ls_par-opti to ls_abkrsr-option.
            if ls_abkrsr-sign is initial.
              move 'I'  to ls_abkrsr-sign.
              move 'EQ' to ls_abkrsr-option.
            endif.
            append ls_abkrsr to lt_abkrs.
          endif.

        when 'PERIOD'.
          lv_pabrj = ls_par-low(4).
          lv_pabrp = ls_par-low+4(2).

        when others.

      endcase.

    endloop.

*Get PNP field
    try.
        lv_selname = cl_pyd_fnd_aux=>get_resp_fixed_value(
                     iv_par_type = gcv_par_selname
                     it_par      = io_context->mt_par ).
      catch cx_pyd_fnd.
        clear lv_selname.
    endtry.

*get begda&endda from pay period
    clear ls_sel_params.
    select single permo into lv_permo from t549a
                  where abkrs in lt_abkrs.

    select single begda endda into (lv_begda, lv_endda) from t549q
        where permo = lv_permo
        and pabrj = lv_pabrj
        and pabrp = lv_pabrp.

*    SELECT i1~pernr
*       INTO CORRESPONDING FIELDS OF TABLE lt_pernr
*       FROM pa0001 AS i1
*       INNER JOIN pa0003 AS i3  ON i1~pernr = i3~pernr
*      WHERE i1~abkrs IN lt_abkrs
*        AND i1~endda >= lv_endda
*        AND i1~begda <= lv_endda
*        AND i3~abrdt <= lv_endda.

    " Note2269425, for case employee changing payroll area during the middle of
    " payroll period, considering ABRDT in PA0003 will lead to problem
    " should let payroll driver to cope with it
*>>> Start of WOW Specific Code Changes
*    SELECT i1~pernr
*      INTO CORRESPONDING FIELDS OF TABLE lt_pernr
*      FROM pa0001 AS i1
*        WHERE i1~abkrs IN lt_abkrs
*          AND i1~endda >= lv_begda
*          AND i1~begda <= lv_endda.
    select i1~pernr, i1~abkrs
        into corresponding fields of table @lt_pernr
        from pa0001 as i1
          where i1~endda >= @lv_begda
            and i1~begda <= @lv_endda.
    sort lt_pernr by abkrs pernr.
    delete lt_pernr where not abkrs in lt_abkrs.
*<<< End of WOW specific Code Changes
    " Note2344286, remove the potiential duplicate pernr here
    sort lt_pernr by pernr.
    delete adjacent duplicates from lt_pernr.

    loop at lt_pernr into ls_pernr.
      move ls_pernr-pernr to ls_sel_params-low.
      move 'I'            to ls_sel_params-sign.
      move 'EQ'           to ls_sel_params-option.
      move ini_pernr      to ls_sel_params-high.
      if lv_selname is not initial.
        ls_sel_params-selname = lv_selname .
      else.
        ls_sel_params-selname = 'PNPINDEX' .
      endif.
      append ls_sel_params to et_sel_params.
    endloop.

    sort et_sel_params.

  endmethod.
ENDCLASS.
