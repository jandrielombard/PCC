class zcl_m99_py_wt_ddntk_betrg definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.

    methods read_check_data
      importing
        !it_pernr type hr99s_pernr_range .
  protected section.

    types:
      begin of ty_data_result,
        dct_pernr type p_pernr,
        fpper     type fpper,
        lgart     type lgart,
        betrg     type betrg,
      end of ty_data_result .
    types:
      tty_data_result type table of ty_data_result .
    types:
      begin of ty_data_details,
        lgart type lgart,
        betrg type betrg,
      end of ty_data_details .
    types:
      tty_data_details type table of ty_data_details .

    constants mc_itemid_amterr type pyd_itemid value 'AMTERR' ##NO_TEXT.
    constants mc_lgart_r type pyd_par_type value 'Z99_LGART' ##NO_TEXT.
    constants mc_lgart_ind type pyd_par_type value 'Z99_LGART_IND' ##NO_TEXT.
    constants mc_betrg_high type pyd_par_type value 'Z99_AMT_HIGH' ##NO_TEXT.
    constants mc_betrg_low type pyd_par_type value 'Z99_AMT_LOW' ##NO_TEXT.
    data mt_lgart_betrg type /iwbep/t_cod_select_options .
    data ms_lgart type /iwbep/s_cod_select_option .
    data mv_compare_betrg_low type betrg .
    data mv_compare_betrg_high type betrg .
    data mv_lgart_ind type char1 .
    data mt_bat_data_result type tty_data_result .
    data mt_all_data_result type tty_data_result .
    data ms_all_data_result type ty_data_result .
    data ms_data_dtls type ty_data_details .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PY_WT_DDNTK_BETRG IMPLEMENTATION.


  method check.
* Performing the actual check
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lt_all_data_result type tty_data_result,
          ls_data_result     type ty_data_result,
          ls_result          type ty_s_result.
    data: lt_pernr type tty_pernr.
    data: lv_abs_objname  type abap_abstypename.
    data: lv_objname  type seoclsname.

    refresh mt_objdata.
* Read Employee Numbers
    if mv_tpy_res is not initial.  "Test Payroll
      select hrdct_tpy_rgdir~dct_pernr as dct_pernr
        into corresponding fields of table @lt_pernr
        from hrdct_tpy_rgdir
       where hrdct_tpy_rgdir~dct_pernr in @it_pernr_so
         and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
         and hrdct_tpy_rgdir~inper = @mv_payroll_period
        %_hints adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")'.

    else.                          "Production Payroll
      select p2rx_eval_period~dct_pernr as dct_pernr
        into corresponding fields of table @lt_pernr
        from p2rx_eval_period
       where p2rx_eval_period~dct_pernr in @it_pernr_so
         and p2rx_eval_period~abkrs in @mt_payroll_areas
         and p2rx_eval_period~inper = @mv_payroll_period
        %_hints adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")'.
    endif.

    sort lt_pernr by dct_pernr.
    delete adjacent duplicates from lt_pernr.

    lv_abs_objname = cl_abap_classdescr=>get_class_name( me ).
    split lv_abs_objname at mc_equal into data(lv_clstext) lv_objname.

* Perform Parallel Processing
    call method me->multithread_process
      exporting
        iv_clsname = lv_objname
        it_pernr   = lt_pernr.

* Collect the data
    data: lo_cloned_chkobj type ref to zcl_m99_py_wt_ddntk_betrg.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.
          append lines of lo_cloned_chkobj->mt_bat_data_result to lt_all_data_result.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref. "MOD001++
      endtry.

    endloop.

* Collect data for Overview
    append lines of lt_all_data_result to me->mt_all_data_result.

* Build results table
    sort lt_all_data_result by dct_pernr ascending.
    delete adjacent duplicates from lt_all_data_result comparing dct_pernr.

    loop at lt_all_data_result into ls_data_result.
      ls_result-id = ls_data_result-dct_pernr.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      append ls_result to rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Populating Overview screen
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    data:
      ls_err_ov     type ty_s_err_ov,
      ls_sfo        type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif      type abap_bool,
      lv_pernr      type p_pernr,
      lv_value      type pyd_item_value,
      lv_text       type pyd_name,
      lv_char_value type char060.

    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_al_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
    data: lv_longtext  type  t512t-lgtxt,
          lv_shorttext type  t512t-kztxt.

    case iv_access_mode.
        "Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters
        get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).

        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

          clear ls_err_ov-sfo_tab.  clear ls_sfo.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_data_dtls.

            clear: lv_text.
            call function 'HR_GET_LGART_TEXT'
              exporting
                p_lgart          = ms_data_dtls-lgart
*               p_molga          = mc_molga_au                "MOD001--
                p_molga          = mv_molga                    "MOD001++
              importing
                p_longtext       = lv_longtext
                p_shorttext      = lv_shorttext
              exceptions
                no_entry_in_512t = 1
                others           = 2.
            if sy-subrc <> 0.
* Implement suitable error handling here
            endif.
            lv_value = |{ ms_data_dtls-lgart } { lv_longtext }|.
            message i000(zhrpy_pcc_msg) with lv_value ms_data_dtls-betrg into lv_value.
            clear lv_text.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_amterr
                iv_text                     = lv_text
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

        "Execution Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_all_data_result into ms_all_data_result
                where dct_pernr = lv_pernr.

            move-corresponding ms_all_data_result to ms_data_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_data_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_amterr.
            ls_sfo-value = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_specifc_custmizing.
* Populating custom paramters for the validation

    data: lt_param_so type /iwbep/t_cod_select_options,
          ls_par      type if_pyd_fnd_types=>ty_s_resp,
          lt_wt       type table of t512w,
          lv_index    type i.

    try .

* Wage Types for BETRG
        loop at mo_context->mt_par into ls_par where par_type = me->mc_lgart_r.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low(4)   ).
          append lines of lt_param_so to mt_lgart_betrg.
        endloop.


* Amount Low Values
        mv_compare_betrg_low =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_betrg_low
                                                                      it_par      = mo_context->mt_par ).

* Amount High Values
        mv_compare_betrg_high =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_betrg_high
                                                                       it_par      = mo_context->mt_par ).

* Individual WT indicator
        mv_lgart_ind =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_lgart_ind
                                                              it_par      = mo_context->mt_par ).

* Build SWT Wage type list
        append lines of mt_lgart_betrg to mt_swt_lgart.
        mv_swt_exc_retro = abap_true.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.


  method read_check_data.
* Called in RFC to perform data selection and populate result validation table
    data: lt_data_result       type standard table of ty_data_result,
          lt_data_result_2     type standard table of ty_data_result,
          lt_data_result_lgart type standard table of ty_data_result,
          ls_data_result       type ty_data_result.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.

      "LGART Total current period
      select p2rx_ddntk~dct_pernr, hrdct_tpy_rgdir~fpper, p2rx_ddntk~lgart, sum( p2rx_ddntk~betrg ) as betrg
              into corresponding fields of table @lt_data_result
                       from hrdct_tpy_rgdir inner join p2rx_ddntk
                         on hrdct_tpy_rgdir~dct_pernr eq p2rx_ddntk~dct_pernr
                        and hrdct_tpy_rgdir~dct_seqnr eq p2rx_ddntk~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr and
                   hrdct_tpy_rgdir~abkrs in @mt_payroll_areas and
                   hrdct_tpy_rgdir~fpper = @mv_payroll_period and
                   hrdct_tpy_rgdir~inper = @mv_payroll_period and
                   p2rx_ddntk~lgart in @mt_lgart_betrg
               and exists ( select 1
                           from p2rx_wpbp
                           where p2rx_wpbp~dct_pernr eq p2rx_ddntk~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_ddntk~dct_seqnr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_ddntk~dct_pernr, hrdct_tpy_rgdir~fpper, p2rx_ddntk~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")'.

      append lines of lt_data_result to lt_data_result_2.
      sort lt_data_result_2 by dct_pernr ascending.
      clear lt_data_result.
      loop at lt_data_result_2 into data(ls_data_result_2).
        collect ls_data_result_2 into lt_data_result.
      endloop.


    else. "Production Payroll

      "LGART Total inc retro
      select p2rx_ddntk~dct_pernr, p2rx_eval_period~fpper, p2rx_ddntk~lgart, sum( p2rx_ddntk~betrg ) as betrg
              into corresponding fields of table @lt_data_result
                       from p2rx_eval_period inner join p2rx_ddntk
                                     on p2rx_eval_period~dct_pernr eq p2rx_ddntk~dct_pernr
                                    and p2rx_eval_period~dct_seqnr eq p2rx_ddntk~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr and
                   p2rx_eval_period~abkrs in @mt_payroll_areas and
                   p2rx_eval_period~inper = @mv_payroll_period and
                   p2rx_ddntk~lgart in @mt_lgart_betrg
               and exists ( select 1
                           from p2rx_wpbp
                           where p2rx_wpbp~dct_pernr eq p2rx_ddntk~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_ddntk~dct_seqnr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_ddntk~dct_pernr, p2rx_eval_period~fpper, p2rx_ddntk~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' .
    endif.

    sort lt_data_result by dct_pernr fpper lgart.
    delete adjacent duplicates from lt_data_result.
    delete lt_data_result where fpper <> mv_payroll_period.

* Rebuild the list with out WT details
    lt_data_result_2[] = lt_data_result[].
    lt_data_result_lgart[] = lt_data_result[].

    refresh lt_data_result.
    loop at lt_data_result_2 into data(ls_betrg_data_2).
      if not mv_lgart_ind eq mc_abap_yes.
        clear: ls_betrg_data_2-lgart.
      endif.
      collect ls_betrg_data_2 into lt_data_result.
    endloop.

* Collect Results according to criteria
    loop at lt_data_result into ls_data_result.
      check ls_data_result-betrg between mv_compare_betrg_low and mv_compare_betrg_high.
      append ls_data_result to mt_bat_data_result.
    endloop.

  endmethod.
ENDCLASS.
