class ZUSECL_M99_PA_ADV_ABSENCE definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
  protected section.

    types:
      begin of ty_it2001,
        pernr type p2001-pernr,
        subty type p2001-subty,
        begda type p2001-begda,
        endda type p2001-endda,
        aedtm type p2001-aedtm,
        stext type sbttx.
    types: end of ty_it2001 .
    types:
      tty_it2001 type table of ty_it2001 .
    types:
      begin of ty_it2001_dtls,
        subty type p2001-subty,
        begda type p2001-begda,
        endda type p2001-endda,
        aedtm type p2001-aedtm.
    types: end of ty_it2001_dtls .
    types:
      begin of ty_advtab,
        srtfd type pcl2-srtfd,
        srtf2 type pcl2-srtf2,
        aedtm type pcl2-aedtm.
    types: end of ty_advtab .
    types:
      begin of ty_payroll_result,
        dct_pernr type p_pernr,
        inper     type iperi,
        fpper     type fpper,
        lgart     type lgart,
        betrg     type maxbt,
      end of ty_payroll_result .
    types:
      tty_payroll_result type table of ty_payroll_result .

    constants mc_offset_days type pyd_par_type value 'Z99_OFFSET_DAYS' ##NO_TEXT.
    constants mc_itemid_advpaywb type pyd_s_rdsfo_ext-itemid value 'ADVPAYWB' ##NO_TEXT.
    constants mc_infty_2001 type infty value '2001' ##NO_TEXT.
    constants mc_z99_lgart type pyd_par_type value 'Z99_LGART' ##NO_TEXT.
    constants mc_amt_low type pyd_par_type value 'Z99_AMT_LOW' ##NO_TEXT.
    constants mc_amt_high type pyd_par_type value 'Z99_AMT_HIGH' ##NO_TEXT.
    data mv_relid type relid .
    data mv_offset_days type numc3 .
    data mt_allow_lgart type /iwbep/t_cod_select_options .
    data ms_allow_lgart type /iwbep/s_cod_select_option .
    data mv_amt_low type pranz .
    data mv_amt_high type pranz .
    data mt_it2001 type tty_it2001 .
    data ms_it2001 type ty_it2001 .
    data ms_it2001_dtls type ty_it2001_dtls .
    data mv_prev_payroll_period type iperi .

    methods read_pp_payroll_result
      importing
        !iv_pernr          type pernr-pernr
      exporting
        !et_payroll_result type tty_payroll_result .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_ADV_ABSENCE IMPLEMENTATION.


  method CHECK.
* Termination in Current Period. Process Termination workbench.
    data: ls_result like line of rt_result.
    data: lv_check_begda type begda.

    data: lt_all_it2001 type table of ty_it2001.
    data: lt_wberr_it2001 type table of ty_it2001.
    data: lt_err_it2001 type table of ty_it2001.
    data: ls_it2001 type ty_it2001.
    data: lo_payroll_area type ref to cl_hr_payroll_area,
          lv_abkrs        type t549a-abkrs,
          lv_period_begda type begda,
          lv_period_endda type endda.

    data: lv_srtfd_var type pcl2-srtfd.
    data: lt_advtab type table of ty_advtab.
    data: ls_advtab type ty_advtab.
    data: lt_pernr_so type /iwbep/t_cod_select_options.
    data: lt_pp_payroll_result type tty_payroll_result.

* Set Date for Selection
    lv_check_begda = sy-datum + mv_offset_days.

*All Employees with P2001-BEGDA is 7 days or less from current date.
    select it2001~pernr, it2001~subty,
           it2001~begda, it2001~endda, it2001~aedtm
      into corresponding fields of table @lt_all_it2001
      from pa2001 as it2001
      where it2001~pernr in @it_pernr_so
         and it2001~subty in @mt_subty
         and it2001~sprps = @if_hrpa_read_infotype=>unlocked
         and it2001~begda between @sy-datum and @lv_check_begda
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it2001~pernr
                         and it0000~begda <= @mv_endda
                         and it0000~endda >= @mv_begda
                         and it0000~stat2 in @mt_stat2
                         and it0000~sprps = @if_hrpa_read_infotype=>unlocked
                         and it0001~begda <= @mv_endda
                         and it0001~endda >= @mv_begda
                         and it0001~sprps = @if_hrpa_read_infotype=>unlocked
                         and it0001~abkrs in @mt_payroll_areas
                         and it0001~bukrs in @mt_bukrs
                         and it0001~werks in @mt_werks
                         and it0001~btrtl in @mt_btrtl
                         and it0001~persg in @mt_persg
                         and it0001~persk in @mt_persk
                         and it0001~kostl in @mt_kostl )
    order by it2001~pernr.

* Read N1 cluster for all the leave employees
    if not lt_all_it2001 is initial.
      "Initialize
      read table mt_payroll_areas into data(ls_payroll_areas) index 1.
      lv_abkrs = ls_payroll_areas-low.
      call method cl_hr_payroll_area=>get_instance
        exporting
          imp_area = lv_abkrs
        receiving
          ret_area = lo_payroll_area.

      loop at lt_all_it2001 into ls_it2001.
        try.
            clear: lv_period_begda, lv_period_endda.
            call method lo_payroll_area->determine_periode_begda_endda
              exporting
                imp_date  = ls_it2001-begda
              importing
                exp_begda = lv_period_begda
                exp_endda = lv_period_endda.
          catch cx_hrpy_payroll_area .
        endtry.

        concatenate ls_it2001-pernr lv_period_begda
                    lv_period_endda lv_period_endda into lv_srtfd_var.
        select single advpcl2~srtfd, advpcl2~srtf2, advpcl2~aedtm
          into corresponding fields of @ls_advtab
          from pcl2 as advpcl2 where relid = @mv_relid and srtfd = @lv_srtfd_var.
        if sy-subrc <> 0.
          append ls_it2001 to lt_wberr_it2001.
        endif.
      endloop.
* Error Employees
      sort lt_wberr_it2001.
      delete adjacent duplicates from lt_wberr_it2001 comparing pernr.

* Allowances to be paid along with advance leave - Validation
      if not mt_allow_lgart[] is initial.
        clear lt_pp_payroll_result.
        loop at lt_wberr_it2001 into ls_it2001.
          call method me->read_pp_payroll_result
            exporting
              iv_pernr          = ls_it2001-pernr
            importing
              et_payroll_result = lt_pp_payroll_result.
          if not lt_pp_payroll_result is initial.
            append ls_it2001 to lt_err_it2001.
          endif.
        endloop.
      else.
        lt_err_it2001[] = lt_wberr_it2001[].
      endif.

      mt_it2001[] = lt_err_it2001[].
    endif.

* Report all Error Employees
    sort lt_err_it2001.
    delete adjacent duplicates from lt_err_it2001 comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_err_it2001 into ls_it2001.
      ls_result-id = ls_it2001-pernr.
      append ls_result to rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Method for Overview list display
    data: ls_err_ov type ty_s_err_ov,
          ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
          lv_modif  type abap_bool,
          lv_pernr  type p_pernr,
          lv_value  type pyd_item_value.

    data: lv_value_string type pyd_name.
    data: lv_it_begda(10) type c.
    data: lv_it_endda(10) type c.

    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_it_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

* Populate SFO tab for Display
        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          " Reason
          if not mt_allow_lgart is initial.
            lv_value_string = text-006.
          else.
            lv_value_string = text-002.
          endif.
          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_advpaywb
              iv_text                     = |{ text-001 }|
              iv_value                    = lv_value_string
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab ).

          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_it2001_dtls.

            move-corresponding ms_it2001_dtls to ms_it2001.
            move lv_pernr to ms_it2001-pernr.

* Read IT 0011 Subtype text
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_infty_2001
                subty               = ms_it2001-subty
                persnr              = ms_it2001-pernr
                begda               = ms_it2001-begda
                endda               = ms_it2001-begda
                molga               = mc_molga_nz
              importing
                stext               = ms_it2001-stext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.
            if sy-subrc <> 0.
* Implement suitable error handling here
            endif.

            concatenate ms_it2001-stext  '(' ms_it2001-subty ')'
              into data(lv_subty_text) separated by space.
            write: ms_it2001-begda to lv_it_begda dd/mm/yyyy.
            write: ms_it2001-endda to lv_it_endda dd/mm/yyyy.
            message i097(zhrpy_pcc_msg)
              with lv_subty_text lv_it_begda  lv_it_endda into lv_value_string.

            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_advpaywb
                iv_text                     = |{ text-003 }|
                iv_value                    = lv_value_string
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

          endloop.

          " Message
          clear: lv_value_string.
          case mv_molga.
            when mc_molga_au.
              lv_value_string = text-004.
            when mc_molga_nz.
              lv_value_string = text-005.
          endcase.
          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_advpaywb
              iv_text                     = ''
              iv_value                    = lv_value_string
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab ).

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

* Populate SFO tab for Table Save
          loop at mt_it2001 into ms_it2001
            where pernr = lv_pernr.
            move-corresponding ms_it2001 to ms_it2001_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_it2001_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_advpaywb.
            ls_sfo-value  = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.

    data lt_param_so  type /iwbep/t_cod_select_options.
    data ls_par       type if_pyd_fnd_types=>ty_s_resp.
    data: lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_vabrj        type vabrj,
          lv_vabrp        type vabrp,
          lv_abkrs        type abkrs,
          lv_begda        type begda,
          lv_endda        type endda,
          lo_payroll_area type ref to cl_hr_payroll_area.

* Advance Payments RELID for the country
    case mv_molga.
      when mc_molga_au.
        mv_relid = 'Q3'.
      when mc_molga_nz.
        mv_relid = 'N1'.
    endcase.

* Offst Days
    try.
        mv_offset_days =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_offset_days
                                                                it_par     = mo_context->mt_par ).

* Include Condition Wage type
        loop at mo_context->mt_par into ls_par where par_type = me->mc_z99_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_allow_lgart.
        endloop.

* Amount Low Values
        mv_amt_low =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_amt_low
                                                            it_par      = mo_context->mt_par ).
* Amout High Values
        mv_amt_high =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_amt_high
                                                             it_par      = mo_context->mt_par ).

* Prev Payroll Period
        lv_abkrs = mv_payroll_area.
        lo_payroll_area = cl_hr_payroll_area=>get_instance( imp_area = lv_abkrs ).
        lv_pabrj = mv_payroll_period(4).
        lv_pabrp = mv_payroll_period+4(2).

        lo_payroll_area->get_period_info(
          exporting
            imp_pabrj = lv_pabrj
            imp_pabrp = lv_pabrp
          importing
            exp_vabrj = lv_vabrj
            exp_vabrp = lv_vabrp
            exp_begda = lv_begda
            exp_endda = lv_endda ).

        "previous payroll period, used by to calc wage type differences"
        mv_prev_payroll_period = lv_vabrj && lv_vabrp.
      catch cx_pyd_fnd into data(lo_exception).
    endtry.


  endmethod.


  method READ_PP_PAYROLL_RESULT.
* Read Previous Payroll Data
    data: lt_payroll_result   type standard table of ty_payroll_result,
          lt_payroll_result_2 type standard table of ty_payroll_result.
    data: lt_pernr type table of ty_it2001.

    data: lv_curr type waers.
    "LGART Total Inc retro
    refresh: lt_payroll_result.
    select p2rx_rt~dct_pernr as dct_pernr, p2rx_eval_period~inper as inper,
           p2rx_eval_period~fpper as fpper, p2rx_rt~lgart as lgart,
           sum( case p2rx_eval_period~srtza
                when 'P' then betrg * -1
                else betrg end ) as betrg
      into corresponding fields of table @lt_payroll_result
      from p2rx_eval_period
      inner join p2rx_rt
         on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
        and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
      where p2rx_eval_period~dct_pernr eq @iv_pernr
        and p2rx_eval_period~fpper = @mv_prev_payroll_period
        and p2rx_eval_period~inper = @mv_prev_payroll_period
        and p2rx_rt~lgart in @mt_allow_lgart
      group by p2rx_rt~dct_pernr, p2rx_eval_period~inper, p2rx_eval_period~fpper, p2rx_rt~lgart
      %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.

    sort lt_payroll_result by dct_pernr inper fpper lgart.
    delete adjacent duplicates from lt_payroll_result.

    delete lt_payroll_result where fpper <> mv_prev_payroll_period.
    et_payroll_result = lt_payroll_result.

  endmethod.
ENDCLASS.
