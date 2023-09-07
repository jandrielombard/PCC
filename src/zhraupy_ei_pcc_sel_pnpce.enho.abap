class lcl_zhraupy_ei_pcc_sel_pnpce definition deferred.
class cl_pyc_bpc_selection_pnpce_rep definition local friends lcl_zhraupy_ei_pcc_sel_pnpce.
class lcl_zhraupy_ei_pcc_sel_pnpce definition.
  public section.
    class-data obj type ref to lcl_zhraupy_ei_pcc_sel_pnpce. "#EC NEEDED
    data core_object type ref to cl_pyc_bpc_selection_pnpce_rep . "#EC NEEDED
 INTERFACES  IOW_ZHRAUPY_EI_PCC_SEL_PNPCE.
    methods:
      constructor importing core_object
                              type ref to cl_pyc_bpc_selection_pnpce_rep optional.
endclass.
class lcl_zhraupy_ei_pcc_sel_pnpce implementation.
  method constructor.
    me->core_object = core_object.
  endmethod.

  method iow_zhraupy_ei_pcc_sel_pnpce~get_specific_params.
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
    constants: ini_pernr type pernr value '00000000'.
    data: ls_sel_params type rsparams.

    types:
      begin of line_pernr,
        pernr type p_pernr,
        abkrs type abkrs,
      end of line_pernr.
    data: lt_pernrs type standard table of line_pernr,
          ls_pernr  type line_pernr.

    data: begin of log_mem_key,
            uname type syuname,
            datum type sydatum,
          end of log_mem_key.

    log_mem_key-uname = sy-uname.
    log_mem_key-datum = sy-datum.

    call method core_object->get_leading_time_selection(
      exporting
        it_par        = io_context->mt_par " Result Parameter List
      importing
        et_sel_params = data(lt_sel_params) ). " rsparams Table
    read table lt_sel_params into ls_sel_params with key selname = 'PNPTIMED'.
    delete lt_sel_params index sy-tabix.
    clear ls_sel_params .
    ls_sel_params-selname = 'PNPTIMR9'.
    ls_sel_params-kind = 'P' .
    ls_sel_params-low = '' .
    ls_sel_params-option = 'EQ' .
    ls_sel_params-sign = 'I' .
    append ls_sel_params to lt_sel_params .
    clear ls_sel_params .
    ls_sel_params-selname = 'PNPTIMRA'.
    ls_sel_params-kind = 'P' .
    ls_sel_params-low = 'X' .
    ls_sel_params-option = 'EQ' .
    ls_sel_params-sign = 'I' .
    append ls_sel_params to lt_sel_params .

*>>> Start of WOW Specific Code Changes
* Replace report selection with IT 0001 selection
*    SUBMIT pyc_select_pernr_via_pnpreport EXPORTING LIST TO MEMORY
*                        AND RETURN
**                        USING SELECTION-SET ' ' "No variant
*                        WITH SELECTION-TABLE lt_sel_params
*                        WITH plgmemky EQ log_mem_key.

*    IMPORT lt_pernrs TO lt_pernrs FROM MEMORY ID log_mem_key.

    data: lv_begda type datum,
          lv_endda type datum,
          lv_abkrs type abkrs,
          lv_pabrj type pabrj,
          lv_pabrp type pabrp,
          lv_permo type permo,
          ls_par   type pyd_s_resp.

    data: lt_abkrs  type hrpy_tt_abkrs,
          ls_abkrsr type abkrs_cd.

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

* get begda & endda from pay period
    clear ls_sel_params.
    select single permo into lv_permo from t549a
                  where abkrs in lt_abkrs.

    select single begda endda into (lv_begda, lv_endda) from t549q
        where permo = lv_permo
        and pabrj = lv_pabrj
        and pabrp = lv_pabrp.

    select i1~pernr, i1~abkrs
        into corresponding fields of table @lt_pernrs
        from pa0001 as i1
          where i1~endda >= @lv_begda
            and i1~begda <= @lv_endda.
    sort lt_pernrs by abkrs pernr.
    delete lt_pernrs where not abkrs in lt_abkrs.

    sort lt_pernrs by pernr.
    delete adjacent duplicates from lt_pernrs.
*<<< End of WOW Specific Code Change

    loop at lt_pernrs into ls_pernr.
      move ls_pernr-pernr to ls_sel_params-low.
      move 'I'            to ls_sel_params-sign.
      move 'EQ'           to ls_sel_params-option.
      move ini_pernr      to ls_sel_params-high.
      ls_sel_params-selname = 'PNPINDEX'.
      append ls_sel_params to et_sel_params.
    endloop.

    sort et_sel_params.
  endmethod.
ENDCLASS.
