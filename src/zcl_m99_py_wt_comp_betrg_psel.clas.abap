class zcl_m99_py_wt_comp_betrg_psel definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.

    types:
      begin of ty_betrg_data,
        dct_pernr type p_pernr,
        fpper     type fpper,
        lgart     type lgart,
        betrg     type maxbt,
        waers     type waers,
      end of ty_betrg_data .
    types:
      tty_betrg_data type table of ty_betrg_data .

    methods read_check_data
      importing
        !it_pernr type hr99s_pernr_range .
  protected section.

    types:
      begin of ty_betrg_dtls,
        fpper type fpper,
        lgart type lgart,
        betrg type maxbt,
        waers type waers,
      end of ty_betrg_dtls .

    constants mc_z99_lgart type pyd_par_type value 'Z99_LGART' ##NO_TEXT.
    constants mc_z99_exc_lgart type pyd_par_type value 'Z99_EXC_LGART' ##NO_TEXT.
    constants mc_lgart_ind type pyd_par_type value 'Z99_LGART_IND' ##NO_TEXT.
    constants mc_amt_high type pyd_par_type value 'Z99_AMT_HIGH' ##NO_TEXT.
    constants mc_amt_low type pyd_par_type value 'Z99_AMT_LOW' ##NO_TEXT.
    constants mc_z99_stat2 type pyd_par_type value 'Z99_STAT2' ##NO_TEXT.
    constants mc_proc_class type pyd_par_type value 'Z99_PROC_CLASS' ##NO_TEXT.
    constants mc_eval_class type pyd_par_type value 'Z99_EVAL_CLASS' ##NO_TEXT.
    constants mc_itemid_amterr type pyd_itemid value 'AMTERR' ##NO_TEXT.
    data mt_lgart type /iwbep/t_cod_select_options .
    data ms_lgart type /iwbep/s_cod_select_option .
    data mt_eval_class type /iwbep/t_cod_select_options .
    data mt_proc_class type /iwbep/t_cod_select_options .
    data mv_proc_class type char4 .
    data mv_eval_class type char4 .
    data mv_lgart_ind type boolean .
    data mv_amt_high type betrg .
    data mv_amt_low type betrg .
    data mv_z99_stat2 type stat2 .
    data mt_bat_betrg_data type standard table of ty_betrg_data .
    data mt_all_betrg_data type standard table of ty_betrg_data .
    data ms_betrg_data type ty_betrg_data .
    data ms_betrg_dtls type ty_betrg_dtls .
    data gv_rfc_jobs type i value 30 ##NO_TEXT.
    data gv_rfc_snd_jobs type i value 1 ##NO_TEXT.
    data gv_rfc_rcv_jobs type i value 1 ##NO_TEXT.
    data gv_rfc_excp_flag type boolean .
    data gv_rfc_parallel_process type xfeld .
    data gv_rfc_msg type boolean value space ##NO_TEXT.
    data gv_rfc_group type rzlli_apcl value 'parallel' ##NO_TEXT.
    data gv_rfc_implemented type boolean .
    data g_max_pbt_wps type i .
    data g_free_pbt_wps type i .
    data l_return type bapireturn1 .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PY_WT_COMP_BETRG_PSEL IMPLEMENTATION.


  method check.
* Compare Wage type Values (BETRG) to High or Low values
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lt_all_betrg_data type standard table of ty_betrg_data,
          ls_betrg_data     type ty_betrg_data.

    data: lt_pernr type tty_pernr.
    data: ls_pernr type ty_pernr.

    data: ls_result       type ty_s_result.
    data: lv_curr type waers.
    data lv_retcd type sy-subrc.
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
* Perform Parallel Processing
    lv_abs_objname = cl_abap_classdescr=>get_class_name( me ).
    split lv_abs_objname at mc_equal into data(lv_clstext) lv_objname.
    call method me->multithread_process
      exporting
        iv_clsname = lv_objname
        it_pernr   = lt_pernr.

* Collect the data
    data: lo_cloned_chkobj type ref to zcl_m99_py_wt_comp_betrg_psel.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.
          append lines of lo_cloned_chkobj->mt_bat_betrg_data to lt_all_betrg_data.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref. "MOD001++
      endtry.
    endloop.

* Collect Data for Overview
    append lines of lt_all_betrg_data to me->mt_all_betrg_data.

* Prepare the error list
    sort lt_all_betrg_data by dct_pernr ascending.
    delete adjacent duplicates from lt_all_betrg_data comparing dct_pernr.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_all_betrg_data into ls_betrg_data.
      ls_result-id = ls_betrg_data-dct_pernr.
      append ls_result to rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Method for Overview list display
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
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters
        get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).
        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_betrg_dtls.

            clear: lv_text.
            if ms_betrg_dtls-lgart is initial.
              lv_text = text-001.
              message i021(zhrpy_pcc_msg)
                 with ms_betrg_dtls-fpper+4(2) ms_betrg_dtls-fpper+0(4) into lv_text.
            else.
              call function 'HR_GET_LGART_TEXT'
                exporting
                  p_lgart          = ms_betrg_dtls-lgart
*                 p_molga          = mc_molga_au        "MOD001--
                  p_molga          = mv_molga            "MOD001++
                importing
                  p_longtext       = lv_longtext
                  p_shorttext      = lv_shorttext
                exceptions
                  no_entry_in_512t = 1
                  others           = 2.
              if sy-subrc <> 0.
* Implement suitable error handling here
              endif.
              message i022(zhrpy_pcc_msg)
                 with lv_longtext ms_betrg_dtls-fpper+4(2) ms_betrg_dtls-fpper+0(4) into lv_text.
            endif.
            write ms_betrg_dtls-betrg to lv_char_value currency ms_betrg_dtls-waers left-justified.
            lv_value = lv_char_value.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_amterr
                iv_text                     = lv_text
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

* Comparison High Value
          lv_text = text-002.
          write mv_amt_high to lv_char_value left-justified.
          lv_value = lv_char_value.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_amterr
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Comparison Low Value
          lv_text = text-003.
          write mv_amt_low to lv_char_value left-justified.
          lv_value = lv_char_value.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_amterr
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.
* Action
          lv_text = text-005.
          lv_value = text-006.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_amterr
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_all_betrg_data into ms_betrg_data
            where dct_pernr = lv_pernr.

            move-corresponding ms_betrg_data to ms_betrg_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_betrg_dtls
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
* Read Check Specific Parameters
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
*002 |06-APR-2023 |1130848  |Accept Multiple Eval / Proc  |CFAK903066   *
*-----------------------------------------------------------------------*
*003 |26-JUL-2023 |1130848  |Accept Exclude Wage types    |CFAK903612   *
*-----------------------------------------------------------------------*
    data lt_param_so  type /iwbep/t_cod_select_options.
    data ls_par       type if_pyd_fnd_types=>ty_s_resp.
    data: lt_wt    type table of t512w,
          lv_index type i.
    data: lv_molga type molga.                     "MOD001++

    try .
        lv_molga = mo_context->ms_inst-molga.               "MOD0001++
*>>> Start of MOD003--
* Wage Types from Z99_LGART
*        refresh: mt_lgart.
*        loop at mo_context->mt_par into ls_par where par_type = me->mc_z99_lgart.
*          clear lt_param_so.
*          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
*          append lines of lt_param_so to mt_lgart.
*        endloop.
*<<< End of MOD003--
*>>> Start of MOD003++
* Wage Types from Z99_LGART and Z99_EXC_LGART
        refresh: mt_lgart.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_z99_lgart
            iv_exc_par_type  = me->mc_z99_exc_lgart
          changing
            ct_parameter_tab = mt_lgart.
*<<< End of MOD003++
* Processing Class
*        mv_proc_class =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_proc_class     "MOD002--
*                                                               it_par      = mo_context->mt_par ). "MOD002--
        loop at mo_context->mt_par into ls_par where par_type = me->mc_proc_class.                  "MOD002++
          clear lt_param_so.                                                                        "MOD002++
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).              "MOD002++
          append lines of lt_param_so to mt_proc_class.                                             "MOD002++
        endloop.                                                                                    "MOD002++

* Read WT using Processing Class
*        if not mv_proc_class is initial.                         "MOD002++
        if not mt_proc_class is initial.                          "MOD002++
          loop at mt_proc_class into data(ms_proc_class).         "MOD002++
            mv_proc_class = ms_proc_class-low.                    "MOD002++

            lv_index = mv_proc_class(2) - 1.
            select lgart vklas into corresponding fields of table lt_wt
*            from t512w where molga = mc_molga_au        "MOD001--
              from t512w where molga = lv_molga            "MOD001++
                           and endda >= mv_begda
                           and begda <= mv_endda.

            loop at lt_wt into data(ls_wt1) where vklas+lv_index(1) = mv_proc_class+2(1).
              clear lt_param_so.
              lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_wt1-lgart ).
              append lines of lt_param_so to mt_lgart.
            endloop.
          endloop.                                                 "MOD002++
        endif.

* Evaluation Class
*        mv_eval_class =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_eval_class     "MOD002--
*                                                               it_par      = mo_context->mt_par ). "MOD002--
        loop at mo_context->mt_par into ls_par where par_type = me->mc_eval_class.                  "MOD002++
          clear lt_param_so.                                                                        "MOD002++
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).              "MOD002++
          append lines of lt_param_so to mt_eval_class.                                             "MOD002++
        endloop.                                                                                    "MOD002++

* Read WT Using Evaluation Class
*        if mv_eval_class is not initial.                      "MOD002--
        if not mt_eval_class is initial.                       "MOD002++
          loop at mt_eval_class into data(ms_eval_class).      "MOD002++
            mv_eval_class = ms_eval_class-low.                 "MOD002++

            lv_index = 2 * mv_eval_class(2) - 2.
            select lgart aklas into corresponding fields of table lt_wt
*            from t512w where molga = mc_molga_au          "MOD001--
              from t512w where molga = lv_molga            "MOD001++
                           and endda >= mv_begda
                           and begda <= mv_endda.
            loop at lt_wt into data(ls_wt) where aklas+lv_index(2) = mv_eval_class+2(2).
              clear lt_param_so.
              lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_wt-lgart ).
              append lines of lt_param_so to mt_lgart.
            endloop.
          endloop.                                             "MOD002++
        endif.

* Wage type Indicator
        mv_lgart_ind =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_lgart_ind
                                                                   it_par = mo_context->mt_par ).

* Amount Low Value
        mv_amt_low =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_amt_low
                                                              it_par    = mo_context->mt_par ).

* Amount High Value
        mv_amt_high =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_amt_high
                                                               it_par    = mo_context->mt_par ).
* Employee Status
        mv_z99_stat2 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_z99_stat2
                                                                it_par    = mo_context->mt_par ).
* Build SWT Wage type list
        refresh: mt_swt_lgart.
        append lines of mt_lgart to mt_swt_lgart.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.


  method read_check_data.
* Called in RFC to perform data selection and populate result validation table

    data: lt_betrg_data   type standard table of ty_betrg_data,
          lt_betrg_data_2 type standard table of ty_betrg_data.

    data: lv_curr type waers.
    data  lv_retcd type sy-subrc.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.
      "LGART Total current period
      refresh: lt_betrg_data.
      select p2rx_rt~dct_pernr as dct_pernr, hrdct_tpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1
                          else betrg end ) as betrg
              into corresponding fields of table @lt_betrg_data
                       from hrdct_tpy_rgdir inner join p2rx_rt
                        on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                       and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~fpper = @mv_payroll_period
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and p2rx_rt~lgart in @mt_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                             and ( p2rx_wpbp~stat2 in @mt_stat2 and p2rx_wpbp~endda >= @mv_endda )
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_rt~dct_pernr, hrdct_tpy_rgdir~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      delete lt_betrg_data where lgart not in mt_lgart.

*      "LGART Total from retros
      refresh: lt_betrg_data_2.
*      select p2rx_rt~dct_pernr as dct_pernr, hrpy_rgdir~fpper as fpper, p2rx_rt~lgart as lgart,
*            sum( case hrpy_rgdir~srtza when 'A' then betrg * -1
*                          else 0 end ) as betrg
*              into corresponding fields of table @lt_betrg_data_2
*                from hrdct_tpy_rgdir inner join hrpy_rgdir
*                      on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
*                      and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
*                    inner join p2rx_rt
*                      on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
*                      and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
*             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
*               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
*               and hrdct_tpy_rgdir~fpper = @mv_payroll_period
*               and hrdct_tpy_rgdir~inper = @mv_payroll_period
*               and p2rx_rt~lgart in @mt_lgart
*               and exists ( select 1
*                           from p2rx_wpbp_index
*                           inner join p2rx_wpbp
*                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
*                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
*                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
*                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
*                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
*                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
*                             and ( p2rx_wpbp~stat2 = @mc_stat2_active and p2rx_wpbp~endda >= @mv_endda )
*                             and p2rx_wpbp~bukrs in @mt_bukrs
*                             and p2rx_wpbp~werks in @mt_werks
*                             and p2rx_wpbp~btrtl in @mt_btrtl
*                             and p2rx_wpbp~persg in @mt_persg
*                             and p2rx_wpbp~persk in @mt_persk
*                             and p2rx_wpbp~kostl in @mt_kostl )
*             group by p2rx_rt~dct_pernr, hrpy_rgdir~fpper, p2rx_rt~lgart
*             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.
*
*      delete lt_betrg_data_2 where lgart not in mt_lgart.

      append lines of lt_betrg_data to lt_betrg_data_2.
      sort lt_betrg_data_2 by dct_pernr  fpper lgart.

      refresh lt_betrg_data.
      loop at lt_betrg_data_2 into data(ls_betrg_data).
        collect ls_betrg_data into lt_betrg_data.
      endloop.

    else. "Production Payroll

      "LGART Total Inc retro
      refresh: lt_betrg_data.
      select p2rx_rt~dct_pernr as dct_pernr, p2rx_eval_period~fpper as fpper,
             p2rx_rt~lgart as lgart,
            sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                          else betrg end ) as betrg
              into corresponding fields of table @lt_betrg_data
              from p2rx_eval_period inner join p2rx_rt
                on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
               and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr
               and p2rx_eval_period~abkrs in @mt_payroll_areas
               and p2rx_eval_period~inper = @mv_payroll_period
               and p2rx_rt~lgart in @mt_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                                 inner join p2rx_wpbp
                                    on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                                   and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                                   and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                             and ( p2rx_wpbp~stat2 in @mt_stat2 and p2rx_wpbp~endda >= @mv_endda )
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_rt~dct_pernr, p2rx_eval_period~fpper, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      delete lt_betrg_data where lgart not in mt_lgart.
    endif.

    sort lt_betrg_data by dct_pernr fpper lgart.
    delete lt_betrg_data where betrg eq 0.

* Rebuild the list with out WT details
    lt_betrg_data_2 = lt_betrg_data.

    refresh lt_betrg_data.
    loop at lt_betrg_data_2 into data(ls_betrg_data_2).
      if not mv_lgart_ind eq mc_abap_yes.
        clear: ls_betrg_data_2-lgart.
      endif.
      collect ls_betrg_data_2 into lt_betrg_data.
    endloop.

* Check the Amount and Report the Error
    loop at lt_betrg_data into ls_betrg_data.
      check ls_betrg_data-betrg between mv_amt_low and mv_amt_high.

      append ls_betrg_data to mt_bat_betrg_data.
    endloop.

  endmethod.
ENDCLASS.
