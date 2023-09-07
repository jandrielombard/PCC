class zcl_m99_py_wt_comp_anzhl definition
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
      begin of ty_anzhl_data,
        dct_pernr type p_pernr,
        fpper     type fpper,
        fpend     type fpend,
        lgart     type lgart,
        anzhl     type pranz,
        wostd     type wostd,
        mostd     type mostd,
      end of ty_anzhl_data .
    types:
      tty_anzhl_data type table of ty_anzhl_data .
    types:
      begin of ty_anzhl_dtls,
        fpper type fpper,
        lgart type lgart,
        anzhl type pranz,
        wostd type wostd,
      end of ty_anzhl_dtls .

    constants mc_wweeks type pyd_par_type value 'Z99_WWEEKS' ##NO_TEXT.
    constants mc_z99_lgart type pyd_par_type value 'Z99_LGART' ##NO_TEXT.
    constants mc_z99_condition_lgart type pyd_par_type value 'Z99_CONDITION_LGART' ##NO_TEXT.
    constants mc_lgart_ind type pyd_par_type value 'Z99_LGART_IND' ##NO_TEXT.
    constants mc_num_high type pyd_par_type value 'Z99_NUM_HIGH' ##NO_TEXT.
    constants mc_num_low type pyd_par_type value 'Z99_NUM_LOW' ##NO_TEXT.
    constants mc_num_wostd type pyd_par_type value 'Z99_NUM_WOSTD' ##NO_TEXT.
    constants mc_num_mostd type pyd_par_type value 'Z99_NUM_MOSTD' ##NO_TEXT.
    constants mc_proc_class type pyd_par_type value 'Z99_PROC_CLASS' ##NO_TEXT.
    constants mc_eval_class type pyd_par_type value 'Z99_EVAL_CLASS' ##NO_TEXT.
    constants mc_condition_part_per type pyd_par_type value 'Z99_CONDITION_PART_PER' ##NO_TEXT.
    constants mc_itemid_numerr type pyd_itemid value 'NUMERR' ##NO_TEXT.
    data mt_wweeks type /iwbep/t_cod_select_options .
    data ms_wweeks type /iwbep/s_cod_select_option .
    data mt_lgart type /iwbep/t_cod_select_options .
    data ms_lgart type /iwbep/s_cod_select_option .
    data mt_cond_lgart type /iwbep/t_cod_select_options .
    data ms_cond_lgart type /iwbep/s_cod_select_option .
    data mt_eval_class type /iwbep/t_cod_select_options .
    data mt_proc_class type /iwbep/t_cod_select_options .
    data mv_proc_class type char4 .
    data mv_eval_class type char4 .
    data mv_lgart_ind type boolean .
    data mv_num_high type anzhl .
    data mv_num_low type anzhl .
    data mv_num_wostd type char10 .
    data mv_num_mostd type char10 .
    data mv_wostd_mult_num type wostd .
    data mv_wostd_comp_oparator type zhrau_de_checkop .
    data mv_mostd_mult_num type mostd .
    data mv_mostd_comp_oparator type zhrau_de_checkop .
    data mv_condition_part_per type boolean .
    data mt_bat_anzhl_data type standard table of ty_anzhl_data .
    data mt_all_anzhl_data type standard table of ty_anzhl_data .
    data ms_anzhl_data type ty_anzhl_data .
    data ms_anzhl_dtls type ty_anzhl_data .
    data mv_comp_wostd type p0007-wostd .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PY_WT_COMP_ANZHL IMPLEMENTATION.


  method check.
* Read Payroll Data using Parallel Processing and Built the Result table
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lt_anzhl_data type standard table of ty_anzhl_data,
          ls_anzhl_data type ty_anzhl_data.

    data: lt_pernr type tty_pernr.
    data: ls_pernr type ty_pernr.

    data: ls_result type ty_s_result.
    data: lv_curr   type waers.
    data  lv_retcd  type sy-subrc.
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
    data: lo_cloned_chkobj type ref to zcl_m99_py_wt_comp_anzhl.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.
          append lines of lo_cloned_chkobj->mt_bat_anzhl_data to lt_anzhl_data.
*        catch cx_pyd_fnd.                                                 "MOD001--
        catch cx_pyd_fnd into data(lo_cx_ref).                             "MOD001++
          raise exception type cx_pyd_fnd exporting previous = lo_cx_ref. "MOD001++
      endtry.

    endloop.

* Collect data for Overview
    append lines of lt_anzhl_data to me->mt_all_anzhl_data.

* Build Result Table
    sort lt_anzhl_data by dct_pernr ascending.
    delete adjacent duplicates from lt_anzhl_data comparing dct_pernr.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_anzhl_data into ls_anzhl_data.
      ls_result-id = ls_anzhl_data-dct_pernr.
      append ls_result to rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* * Method for Overview list display
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
                p_struct2 = ms_anzhl_dtls.

            clear: lv_text.
            if ms_anzhl_dtls-lgart is initial.
              lv_text = text-001.
              message i020(zhrpy_pcc_msg)
                 with ms_anzhl_dtls-fpper+4(2) ms_anzhl_dtls-fpper+0(4) into lv_text.
            else.
              call function 'HR_GET_LGART_TEXT'
                exporting
                  p_lgart          = ms_anzhl_dtls-lgart
                  p_molga          = io_res_context->ms_inst-molga
                importing
                  p_longtext       = lv_longtext
                  p_shorttext      = lv_shorttext
                exceptions
                  no_entry_in_512t = 1
                  others           = 2.
              if sy-subrc <> 0.
* Implement suitable error handling here
              endif.
              message i019(zhrpy_pcc_msg)
                 with lv_longtext ms_anzhl_dtls-fpper+4(2) ms_anzhl_dtls-fpper+0(4) into lv_text.
            endif.
            write ms_anzhl_dtls-anzhl to lv_char_value left-justified.
            lv_value = lv_char_value.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_numerr
                iv_text                     = lv_text
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

* Comparison High Value
          lv_text = text-002.
          if mv_num_wostd is initial and mv_num_mostd is initial.
            write mv_num_high to lv_char_value left-justified.
          else.
            if not mv_num_wostd is initial.
              write ms_anzhl_dtls-wostd  to lv_char_value left-justified.
            endif.
            if not mv_num_mostd is initial.
              write ms_anzhl_dtls-mostd  to lv_char_value left-justified.
            endif.
          endif.
          lv_value = lv_char_value.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_numerr
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Comparison Low Value
          lv_text = text-003.
          if mv_num_wostd is initial and mv_num_mostd is initial.
            write mv_num_low to lv_char_value left-justified.
          else.
            if not mv_num_wostd is initial.
              write ms_anzhl_dtls-wostd  to lv_char_value left-justified.
            endif.
            if not mv_num_mostd is initial.
              write ms_anzhl_dtls-mostd  to lv_char_value left-justified.
            endif.
          endif.
          lv_value = lv_char_value.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_numerr
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
              iv_itemid                   = mc_itemid_numerr
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

          loop at mt_all_anzhl_data into ms_anzhl_data
            where dct_pernr = lv_pernr.

            move-corresponding ms_anzhl_data to ms_anzhl_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_anzhl_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_numerr.
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
* Read Check specific Parameters
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
*002 |06-APR-2023 |1130848  |Accept Multiple Eval / Proc  |CFAK903066   *
*-----------------------------------------------------------------------*
    data lt_param_so  type /iwbep/t_cod_select_options.
    data ls_par       type if_pyd_fnd_types=>ty_s_resp.
    data: lt_wt    type table of t512w,
          lv_index type i.
    data: lv_molga type molga.

    try .
        lv_molga = mo_context->ms_inst-molga.        "MOD001++
* Working weeks
        loop at mo_context->mt_par into ls_par where par_type = me->mc_wweeks.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_wweeks.
        endloop.

* Wage Types from Z99_LGART
        refresh: mt_lgart.
        loop at mo_context->mt_par into ls_par where par_type = me->mc_z99_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_lgart.
        endloop.

* Processing Class
*        mv_eval_class =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_eval_class     "MOD002--
*                                                               it_par      = mo_context->mt_par ). "MOD002--
        loop at mo_context->mt_par into ls_par where par_type = me->mc_proc_class.                  "MOD002++
          clear lt_param_so.                                                                        "MOD002++
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).              "MOD002++
          append lines of lt_param_so to mt_proc_class.                                             "MOD002++
        endloop.                                                                                    "MOD002++

* Read WT using Processing Class
*        if not mv_proc_class is initial.                      "MOD002--
        if not mt_proc_class is initial.                       "MOD002++
          loop at mt_proc_class into data(ms_proc_class).      "MOD002++
            mv_proc_class = ms_proc_class-low.                 "MOD002++

            lv_index = mv_proc_class(2) - 1.
            select lgart vklas into corresponding fields of table lt_wt
*            from t512w where molga = mc_molga_au           "MOD001--
              from t512w where molga = lv_molga             "MOD001++
                           and endda >= mv_begda
                           and begda <= mv_endda.

            loop at lt_wt into data(ls_wt1) where vklas+lv_index(1) = mv_proc_class+2(1).
              clear lt_param_so.
              lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_wt1-lgart ).
              append lines of lt_param_so to mt_lgart.
            endloop.
          endloop.                                               "MOD002++
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
*            from t512w where molga = mc_molga_au           "MOD001--
              from t512w where molga = lv_molga             "MOD001++
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

* Number Low Value
        mv_num_low =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_low
                                                              it_par    = mo_context->mt_par ).

* Number High Value
        mv_num_high =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_high
                                                               it_par    = mo_context->mt_par ).
* Working Weeks
        mv_num_wostd =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_wostd
                                                                it_par    = mo_context->mt_par ).
        mv_wostd_comp_oparator = mv_num_wostd+0(2).
        mv_wostd_mult_num = mv_num_wostd+2.
* Working Months
        mv_num_mostd =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_mostd
                                                                it_par    = mo_context->mt_par ).
        mv_mostd_comp_oparator = mv_num_mostd+0(2).
        mv_mostd_mult_num = mv_num_mostd+2.
* Entries and Leavers Exclusion Flag

        mv_condition_part_per =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_condition_part_per
                                                                       it_par    = mo_context->mt_par ).

* Condition Wage Types from Z99_CONDITION_LGART
        refresh: mt_cond_lgart.
        loop at mo_context->mt_par into ls_par where par_type = me->mc_z99_condition_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_cond_lgart.
        endloop.

* Build SWT Wage type list
        refresh: mt_swt_lgart.
        append lines of mt_lgart to mt_swt_lgart.
        append lines of mt_cond_lgart to mt_swt_lgart.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.


  method read_check_data.
* Read Payroll Data and perform data validation
    data: lt_anzhl_data   type standard table of ty_anzhl_data,
          lt_anzhl_data_2 type standard table of ty_anzhl_data.
    data: lt_all_lgart type /iwbep/t_cod_select_options.
    data: lt_pernr type hr99s_pernr_range.
    data: ls_pernr type sel_pernr.

    data: lv_curr type waers.
    data: lt_p0007            type table of p0007,
          ls_p0007            type p0007,
          lv_comp_wostd       type p0007-wostd,
          lv_wostd_comparison type boolean,
          lv_mostd_comparison type boolean.

    data  lv_retcd type sy-subrc.
    data: lv_entry_date   type begda,
          lv_leaving_date type endda.

    append lines of mt_lgart to lt_all_lgart.
    append lines of mt_cond_lgart to lt_all_lgart.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.

      "LGART Total current period
      refresh: lt_anzhl_data.
      select hrdct_tpy_rgdir~dct_pernr as dct_pernr, hrdct_tpy_rgdir~fpper as fpper,
             hrdct_tpy_rgdir~fpend as fpend, p2rx_rt~lgart as lgart,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then anzhl * -1
                          else anzhl end ) as anzhl
              into corresponding fields of table @lt_anzhl_data
                       from hrdct_tpy_rgdir inner join p2rx_rt
                        on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                       and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and p2rx_rt~lgart in @lt_all_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by hrdct_tpy_rgdir~dct_pernr, hrdct_tpy_rgdir~fpper, hrdct_tpy_rgdir~fpend, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      "LGART Total for retros
      refresh: lt_anzhl_data_2.
      select hrdct_tpy_rgdir~dct_pernr as dct_pernr, hrpy_rgdir~fpper as fpper,
            hrpy_rgdir~fpend as fpend, p2rx_rt~lgart as lgart,
            sum( case hrpy_rgdir~srtza when 'A' then anzhl * -1
                          else 0 end ) as anzhl
              into corresponding fields of table @lt_anzhl_data_2
                          from hrdct_tpy_rgdir inner join hrpy_rgdir
                                 on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
                                and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
                           inner join p2rx_rt
                                on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
                               and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and p2rx_rt~lgart in @lt_all_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by hrdct_tpy_rgdir~dct_pernr, hrpy_rgdir~fpper, hrpy_rgdir~fpend, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

      append lines of lt_anzhl_data to lt_anzhl_data_2.
      sort lt_anzhl_data_2 by dct_pernr  fpper lgart.

      refresh lt_anzhl_data.
      loop at lt_anzhl_data_2 into data(ls_anzhl_data).
        collect ls_anzhl_data into lt_anzhl_data.
      endloop.

    else. "Production Payroll

      "LGART Total Inc retro
      refresh: lt_anzhl_data.
      select p2rx_rt~dct_pernr as dct_pernr, p2rx_eval_period~fpper as fpper,
             p2rx_eval_period~fpend as fpend, p2rx_rt~lgart as lgart,
            sum( case p2rx_eval_period~srtza when 'P' then anzhl * -1
                          else anzhl end ) as anzhl
              into corresponding fields of table @lt_anzhl_data
                       from p2rx_eval_period inner join p2rx_rt
                                     on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                                    and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr
               and p2rx_eval_period~abkrs in @mt_payroll_areas
               and p2rx_eval_period~inper = @mv_payroll_period
               and p2rx_rt~lgart in @lt_all_lgart
               and exists ( select 1
                           from p2rx_wpbp_index
                                 inner join p2rx_wpbp
                                    on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                                   and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                                   and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
                             and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             group by p2rx_rt~dct_pernr, p2rx_eval_period~fpper, p2rx_eval_period~fpend, p2rx_rt~lgart
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

    endif.

    sort lt_anzhl_data by dct_pernr fpper lgart.
    delete adjacent duplicates from lt_anzhl_data.
    delete lt_anzhl_data where fpper ne mv_payroll_period.

* Check for Condition Wage type
    ls_pernr-sign = 'I'. ls_pernr-option = 'EQ'.
    loop at lt_anzhl_data into ls_anzhl_data
        where fpper = mv_payroll_period
          and lgart in mt_cond_lgart.
      ls_pernr-low = ls_pernr-high = ls_anzhl_data-dct_pernr.
      collect ls_pernr into lt_pernr.
    endloop.
    if not lt_pernr is initial.
      delete lt_anzhl_data where dct_pernr not in lt_pernr.
    else.
      clear lt_anzhl_data.
    endif.

* Rebuild the list with out WT details
    lt_anzhl_data_2 = lt_anzhl_data.

    refresh lt_anzhl_data.
    loop at lt_anzhl_data_2 into data(ls_anzhl_data_2)
       where lgart in mt_lgart.
      if not mv_lgart_ind eq mc_abap_yes.
        clear: ls_anzhl_data_2-lgart.
      endif.
      collect ls_anzhl_data_2 into lt_anzhl_data.
    endloop.

    loop at lt_anzhl_data into ls_anzhl_data.
* Check if Z99_NUM_WOSTD / Z99_NUM_MOSTD parameter present, if it is not then use the HIGH & LOW values.
      if mv_num_wostd is initial and mv_num_mostd is initial.
        check ls_anzhl_data-anzhl between mv_num_low and mv_num_high.
      else.
*Flag to reference field WOSTD/MOSTD for the NUM value. The number value in the parameter is the multiplier for field WOSTD
*i.e.IT7 weekly working hours field (WOSTD) multiplied by the value held in the parameter.
*    IT7 Monthly working hours field (MOSTD) multiplied by the value held in the parameter.
        refresh: lt_p0007.
* Read infotype 0007
        call function 'HR_READ_INFOTYPE'
          exporting
            pernr           = ls_anzhl_data-dct_pernr
            infty           = '0007'
            begda           = ls_anzhl_data-fpend
            endda           = ls_anzhl_data-fpend
          importing
            subrc           = lv_retcd
          tables
            infty_tab       = lt_p0007
          exceptions
            infty_not_found = 1.
        if not lt_p0007 is initial.
          clear: ls_p0007.
          read table lt_p0007 into ls_p0007 index 1.
* Check Working Weeks
          check ls_p0007-wweek in mt_wweeks.
* Weekly Working hours
          if not mv_num_wostd is initial.
            ls_anzhl_data-wostd = ls_p0007-wostd * mv_wostd_mult_num.
            clear lv_wostd_comparison.
            call method zcl_m99_pcc_chk_fp4_base=>dynamic_check_operation
              exporting
                iv_opcode = mv_wostd_comp_oparator
                iv_var01  = ls_anzhl_data-anzhl
                iv_var02  = ls_anzhl_data-wostd
              receiving
                rv_result = lv_wostd_comparison.

            check lv_wostd_comparison eq abap_true.
          endif.
* Monthly Hours
          if not mv_num_mostd is initial.
            ls_anzhl_data-mostd = ls_p0007-mostd * mv_mostd_mult_num.
            clear lv_mostd_comparison.
            call method zcl_m99_pcc_chk_fp4_base=>dynamic_check_operation
              exporting
                iv_opcode = mv_mostd_comp_oparator
                iv_var01  = ls_anzhl_data-anzhl
                iv_var02  = ls_anzhl_data-mostd
              receiving
                rv_result = lv_mostd_comparison.

            check lv_mostd_comparison eq abap_true.
          endif.
        endif.
      endif.
* When CONDITION_PART_PER set exclude Entries and Leavers from the List
      if mv_condition_part_per eq abap_true.
        "Entry Date
        clear: lv_entry_date.
        call function 'HR_ENTRY_DATE'
          exporting
            persnr               = ls_anzhl_data-dct_pernr
          importing
            entrydate            = lv_entry_date
          exceptions
            entry_date_not_found = 1
            pernr_not_assigned   = 2
            others               = 3.
        check lv_entry_date not between mv_begda and mv_endda.

        "Leaveing Date
        clear: lv_leaving_date.
        call function 'HR_LEAVING_DATE'
          exporting
            persnr                 = ls_anzhl_data-dct_pernr
          importing
            leavingdate            = lv_leaving_date
          exceptions
            leaving_date_not_found = 1
            pernr_not_assigned     = 2
            others                 = 3.
        if lv_leaving_date eq '00000000'.
          move mc_high_date to lv_leaving_date.
        endif.

        check lv_leaving_date not between mv_begda and mv_endda.
      endif.

      move-corresponding ls_anzhl_data to ms_anzhl_data.
      append ms_anzhl_data to mt_bat_anzhl_data.
    endloop.

  endmethod.
ENDCLASS.
