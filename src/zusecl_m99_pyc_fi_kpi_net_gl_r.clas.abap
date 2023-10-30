class ZUSECL_M99_PYC_FI_KPI_NET_GL_R definition
  public
  inheriting from CL_PYC_KPI_CHART_BASE
  final
  create protected

  global friends CL_PYD_FND_AUX .

public section.
protected section.

  methods CHART_INFO_CALCULATE
    redefinition .
  methods CHART_ITEM_TEXT_GET
    redefinition .
  methods FOOTER_GET
    redefinition .
  methods HEADER_GET
    redefinition .
private section.
ENDCLASS.



CLASS ZUSECL_M99_PYC_FI_KPI_NET_GL_R IMPLEMENTATION.


  method CHART_INFO_CALCULATE.

    data:
      lv_rt_amount      type maxbt,
      lv_ppoix_amount   type maxbt,
      lv_ppopx_amount   type maxbt,
      lv_posting_amount type maxbt,
      lv_currency       type waers,
      ls_kpi_chart_item type ty_s_kpi_chart_item,
      ls_par            type if_pyd_fnd_types=>ty_s_resp,
      lt_lgart_so       type /iwbep/t_cod_select_options,
      lo_cx_fnd         type ref to cx_pyd_fnd,
      lo_cx_frw         type ref to cx_pyc_frw,
      lv_abkrs          type                   abkrs,
      lv_period         type                   fpper,
      lv_gl_account     type hkont,
      lv_pyp_proc_inst  type tvarv_val,
      lv_lgart          type lgart.

    data: lv_id type pyd_roid.

    try.
        lv_abkrs = cl_pyd_fnd_aux=>get_resp_fixed_value(
                     iv_par_type = 'ABKRS'
                     it_par      = io_res_context->mt_par ).
      catch cx_pyd_fnd.
    endtry.

    try.
        lv_period = cl_pyd_fnd_aux=>get_resp_fixed_value(
                     iv_par_type = 'PERIOD'
                     it_par      = io_res_context->mt_par ).
      catch cx_pyd_fnd.
    endtry.
    "Redundant under process context, but leaving in case customer is not using process context
    try.
        lv_gl_account = cl_pyd_fnd_aux=>get_resp_fixed_value(
                     iv_par_type = 'YK_SMB_GL_ACCOUNT'
                     it_par      = io_res_context->mt_par ).
      catch cx_pyd_fnd.
    endtry.
    "End redundant

    try.
        lv_pyp_proc_inst = cl_pyd_fnd_aux=>get_resp_fixed_value(
                     iv_par_type = 'PYP_PROC_INST'
                     it_par      = io_res_context->mt_par ).
      catch cx_pyd_fnd.
    endtry.


    clear et_kpi_chart_item.
    "Redundant under process context, but leaving in case customer is not using process context
    " Get WT
    loop at io_res_context->mt_par into ls_par
      where par_type = 'LGART'.
      call method cl_pyd_fnd_aux=>append_so_fixed_value(
        exporting
          iv_value = ls_par-low
        changing
          ct_so    = lt_lgart_so ).
    endloop.
    "End redundant


    if lv_lgart is initial.
      lv_lgart = '/559'.
    endif.

    if lv_gl_account is initial.
      lv_gl_account = '000061000'.
    endif.


*>>> Start of PTX-3763 PCC Implementation Code change
*>>> Start of MOD--
    "Get total amount from declustered payroll results
*    SELECT
*          SUM( CASE p2rx_eval_period~srtza WHEN 'P' THEN betrg * -1
*                        ELSE betrg END ) AS betrg
*            INTO @lv_rt_amount
*                     FROM p2rx_rt INNER JOIN p2rx_eval_period
*                                   ON p2rx_eval_period~dct_pernr EQ p2rx_rt~dct_pernr
*                                  AND p2rx_eval_period~dct_seqnr EQ p2rx_rt~dct_seqnr
*                        INNER JOIN p2rx_wpbp_index
*                                   ON p2rx_wpbp_index~dct_pernr EQ p2rx_rt~dct_pernr
*                                  AND p2rx_wpbp_index~dct_seqnr EQ p2rx_rt~dct_seqnr
*                                  AND p2rx_wpbp_index~rt_apznr EQ p2rx_rt~apznr
*                        INNER JOIN p2rx_wpbp
*                                   ON p2rx_wpbp~dct_pernr EQ p2rx_rt~dct_pernr
*                                  AND p2rx_wpbp~dct_seqnr EQ p2rx_rt~dct_seqnr
*                                  AND p2rx_wpbp~apznr EQ p2rx_wpbp_index~wpbp_apznr
*           WHERE p2rx_eval_period~abkrs = @lv_abkrs AND
*                 p2rx_eval_period~inper = @lv_period AND
*                 p2rx_rt~lgart = @lv_lgart.
*<<< End of MOD--
*>>> Start of MOD++
    select sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                     else betrg end ) as betrg
            into @lv_rt_amount
            from p2rx_eval_period inner join p2rx_rt
              on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
             and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
           where p2rx_eval_period~abkrs = @lv_abkrs and
                 p2rx_eval_period~inper = @lv_period and
                 p2rx_rt~lgart = @lv_lgart
             and exists ( select 1
                         from p2rx_wpbp_index
                               inner join p2rx_wpbp
                                  on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                                 and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                                 and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                         where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                           and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                           and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr )
           %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.
*End f MOD++
*<<< End of of PTX-3763 PCC Implementation Code change
    call function 'RP_GET_CURRENCY'
      exporting
        molga  = io_res_context->ms_inst-molga
*       TRFAR  =
*       TRFGB  =
*       TRFKZ  =
        begda  = sy-datum
        endda  = sy-datum
      importing
        waers  = lv_currency
*       VALID_BEGDA                         =
*       VALID_ENDDA                         =
*       RETURN =
*       CURRENCY_SOURCE                     =
*       TABLES
*       CURRENCY_TABLE                      =
      exceptions
*       MOLGA_NOT_IN_T001P                  = 1
*       NO_ENTRY_FOUND_IN_TABLE_T001        = 2
*       NO_ENTRY_FOUND_IN_TABLE_T500P       = 3
*       NO_ENTRY_FOUND_IN_TABLE_T500C       = 4
        others = 5.
    if sy-subrc <> 0.
      raise exception type cx_pyd_fnd.
    endif.


    "FICO
    " Find posting information from prior step
    " This code is limited to one posting ID.  If you run in parallel in order to create multiple posting documents you'll need to adjust the code
    select distinct o~id into lv_id up to 1 rows
      from pyd_d_reso as o
      inner join pyd_d_resp as p on p~instid = o~instid and p~par_hash = o~par_hash
      inner join pyd_d_res as r on r~instid = o~instid and r~par_hash = o~par_hash
      where p~par_type = 'PYP_PROC_INST'
        and p~low      = lv_pyp_proc_inst
        and r~dstype   = 'PYP_V2_SIMULATE_POSTING'
        and o~par_type = 'PYP_STS_POST_RUN_ID'.
    endselect.

*>>> Start of PTX-3763 PCC Implementation Code change
*>>> Start of MOD--
*    "First, get retro PPOPX items
*
*    select  sum( ppoix~betrg ) as betrg
*       into @lv_ppopx_amount
*     from  ppdix
*            inner join ppdhd
*            on  ppdhd~runid = ppdix~runid
*            and  ppdhd~docnum = ppdix~docnum
*            inner join ppdit
*            "on  PPDIT~DOCNUM = PPDHD~DOCNUM
*            on  ppdit~docnum = ppdix~docnum
*            and ppdit~doclin = ppdix~doclin
*            inner join ppopx
*            on ppopx~runid = ppdix~runid
*            and ppopx~tslin = ppdix~linum
*            inner join pcalac
*            on pcalac~pernr = ppopx~pernr
*            and pcalac~seqno = ppopx~seqno
*            "and pcalac~runid = ppopx~runid
*            inner join ppoix
*            on ppoix~runid = pcalac~runid
*            and ppoix~seqno = ppopx~seqno
*            and ppoix~postnum = ppopx~postnum
*            and ppoix~pernr = ppopx~pernr
*            "inner join PPOPX
*            "on  PPOPX~PERNR = PPOIX~PERNR
*            "and PPOPX~RUNID = PPOIX~RUNID
*            "and PPOPX~SEQNO = PPOIX~SEQNO )
*          where ppdix~runid = @lv_id and ppdit~hkont = @lv_gl_account and pcalac~srtza = 'A' and pcalac~type = 'PP'.
*
*
** The get current period PPOIX items
*    select  sum( ppoix~betrg ) as betrg
*       into @lv_ppoix_amount
*     from  ppdix
*            inner join ppdhd
*            on  ppdhd~runid = ppdix~runid
*            and  ppdhd~docnum = ppdix~docnum
*            inner join ppdit
*            "on  PPDIT~DOCNUM = PPDHD~DOCNUM
*            on  ppdit~docnum = ppdix~docnum
*            and ppdit~doclin = ppdix~doclin
*            "and pcalac~runid = ppopx~runid
*            inner join ppoix
*             on ppoix~runid = ppdix~runid
*            and ppoix~tslin = ppdix~linum
*            "inner join PPOPX
*            "on  PPOPX~PERNR = PPOIX~PERNR
*            "and PPOPX~RUNID = PPOIX~RUNID
*            "and PPOPX~SEQNO = PPOIX~SEQNO )
*          where ppdix~runid = @lv_id and ppdit~hkont = @lv_gl_account.
*
*    lv_posting_amount = lv_ppoix_amount - lv_ppopx_amount.
*<<< End of MOD--
*>>> Start of MOD++
* For a better performance and accurate results
* Call program RPCIP_DOCUMENT_ANALYZE to read GL Amount for the wage type
* RunID
    data: lt_runid type /iwbep/t_cod_select_options,
          lt_hkont type /iwbep/t_cod_select_options.
    data: lv_variant type  tcats-variant.

    data: lr_runid type range of ppdix-runid.
    data: lr_hkont type range of ppdit-hkont.
    data: lrs_runid like line of lr_runid.
    data: lrs_hkont like line of lr_hkont.

    field-symbols: <lt_glamt_data> type any table.
    data lr_glamt_data             type ref to data.

    call method cl_pyd_fnd_aux=>append_so_fixed_value(
      exporting
        iv_value = lv_id
      changing
        ct_so    = lt_runid ).
    loop at lt_runid into data(ls_runid).
      move-corresponding ls_runid to lrs_runid.
      append lrs_runid to lr_runid.
    endloop.
* GL Account
    call method cl_pyd_fnd_aux=>append_so_fixed_value(
      exporting
        iv_value = lv_gl_account
      changing
        ct_so    = lt_hkont ).
    loop at lt_hkont into data(ls_hkont).
      move-corresponding ls_hkont to lrs_hkont.
      append lrs_hkont to lr_hkont.
    endloop.
    "Initialize Run time Environment
    cl_salv_bs_runtime_info=>set(
      exporting display  = abap_false
                metadata = abap_false
                data     = abap_true ).

* Submit RPCIP_DOCUMENT_ANALYSE for GL Amounts
    submit rpcip_document_analyse
      with p_type = 'PP'
      with p_runid in lt_runid
      with p_hkont in lt_hkont
      with p_both = abap_true
      with p_pdeta = abap_true
      with p_parall = abap_true
      with p_jobs  = 10
      with p_objnr = 20
     and return.

    "Read Report Output
    try.
        cl_salv_bs_runtime_info=>get_data_ref(
          importing r_data = lr_glamt_data ).
        assign lr_glamt_data->* to <lt_glamt_data>.
      catch cx_salv_bs_sc_runtime_info.
        message e006(zhrpy_pcc_msg).
    endtry.

    cl_salv_bs_runtime_info=>clear_all( ).

    "Collect the Error list
    loop at <lt_glamt_data> assigning field-symbol(<ls_glamt_data>).
      assign component 'BETRG' of structure <ls_glamt_data> to field-symbol(<lv_betrg>).
      add <lv_betrg> to lv_posting_amount.
    endloop.

*<<< End of MOD++
*<<< End of of PTX-3763 PCC Implementation Code change

    lv_posting_amount = abs( lv_posting_amount ).

    ls_kpi_chart_item-p_type = cl_pyc_aux=>gcs_p_type-amount.

    if lv_posting_amount = lv_rt_amount.
      ls_kpi_chart_item-color  = cl_pyc_rt_facade=>gcs_kpi_color-good.
    else.
      ls_kpi_chart_item-color  = cl_pyc_rt_facade=>gcs_kpi_color-error.
    endif.

    ls_kpi_chart_item-index = '1'.
    ls_kpi_chart_item-maxbt =  lv_rt_amount.
    ls_kpi_chart_item-waers = lv_currency.
    insert ls_kpi_chart_item into table et_kpi_chart_item.

    ls_kpi_chart_item-index = '2'.
    ls_kpi_chart_item-maxbt = lv_posting_amount.
    ls_kpi_chart_item-waers = lv_currency.
    insert ls_kpi_chart_item into table et_kpi_chart_item.

  endmethod.


  METHOD CHART_ITEM_TEXT_GET.
    "This is only an example implementation. For productive
    "use, this has to be copied and adjusted to the
    "business needs.

    CASE iv_chart_index.
      WHEN '1'.
        rv_text = text-004.
      WHEN '2'.
        rv_text = text-005.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD FOOTER_GET.
    DATA:
      lv_rt_amount      TYPE maxbt,
      lv_posting_amount TYPE maxbt,
      lv_percent        TYPE i,
      lv_kpi_trend      TYPE pyc_kpi_trend,
      lv_gl_account     TYPE hkont,
      lo_cx_fnd         TYPE REF TO cx_pyd_fnd,
      lo_cx_frw         TYPE REF TO cx_pyc_frw.

    TRY.
        lv_gl_account = cl_pyd_fnd_aux=>get_resp_fixed_value(
                     iv_par_type = 'YK_SMB_GL_ACCOUNT'
                     it_par      = io_res_context->mt_par ).
      CATCH cx_pyd_fnd.
    ENDTRY.

    SHIFT lv_gl_account LEFT DELETING LEADING '0'.

    CONCATENATE 'GL account ' lv_gl_account INTO ev_text SEPARATED BY space.


  ENDMETHOD.


  method HEADER_GET.

    ev_text = text-000.
    CLEAR es_var1.
  endmethod.
ENDCLASS.
