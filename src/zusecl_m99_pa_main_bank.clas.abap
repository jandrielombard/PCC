class ZUSECL_M99_PA_MAIN_BANK definition
  public
  inheriting from ZuseCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_SUBTY type PYD_S_RDSFO_EXT-ITEMID value 'SUBTY' ##NO_TEXT.
  protected section.

    constants mc_z99_condition_it0041_01 type pyd_par_type value 'Z99_CONDITION_IT0041_01' ##NO_TEXT.
    constants mc_datetype_hiredate type p0041-dar01 value '01' ##NO_TEXT.

    data mv_z99_condition_it0041_01 type boolean.

    methods get_hiredate_from_it0041
      importing
        !iv_pernr           type pernr-pernr
      returning
        value(rv_hire_date) type datum .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_MAIN_BANK IMPLEMENTATION.


  method CHECK.
* Validate Employee Data
    data: lt_it0       type standard table of pernr_d with non-unique key table_line,
          lt_it9       type standard table of pernr_d with non-unique key table_line,
          lv_size0     type i,
          lv_size9     type i,
          lv_pernr     type pernr-pernr,
          lv_hire_date type datum.

    data: ls_result  type   ty_s_result.
* All relevant employees
    select it00~pernr into table lt_it0 from pa0000 as it00
        inner join pa0001 as it01 on it00~pernr = it01~pernr
        where it01~pernr in it_pernr_so                    and
              it01~begda <= mv_endda                       and
              it01~endda >= mv_begda                       and
              it00~begda <= mv_endda                       and
              it00~endda >= mv_begda                       and
              it00~stat2 in mt_stat2                       and
              it00~sprps = if_hrpa_read_infotype=>unlocked and
              it01~sprps = if_hrpa_read_infotype=>unlocked and
              it01~abkrs in mt_payroll_areas               and
              it01~bukrs in mt_bukrs                       and
              it01~werks in mt_werks                       and
              it01~persg in mt_persg                       and
              it01~persk in mt_persk.

* All relevant employees with IT09
    select it00~pernr into table lt_it9 from pa0000 as it00
        inner join pa0001 as it01 on it00~pernr = it01~pernr
        inner join pa0009 as it09 on it09~pernr = it00~pernr
        where it01~pernr in it_pernr_so                    and
              it01~begda <= mv_endda                       and
              it01~endda >= mv_begda                       and
              it00~begda <= mv_endda                       and
              it00~endda >= mv_begda                       and
              it00~stat2 in mt_stat2                       and
              it00~sprps = if_hrpa_read_infotype=>unlocked and
              it01~sprps = if_hrpa_read_infotype=>unlocked and
              it01~abkrs in mt_payroll_areas               and
              it09~begda <= mv_endda                       and
              it09~endda >= mv_begda                       and
              it09~subty = mv_subty                        and
              it09~sprps = if_hrpa_read_infotype=>unlocked and
              it01~bukrs in mt_bukrs                       and
              it01~werks in mt_werks                       and
              it01~persg in mt_persg                       and
              it01~persk in mt_persk.

    sort lt_it0. delete adjacent duplicates from lt_it0.
    sort lt_it9. delete adjacent duplicates from lt_it9.

    describe table lt_it0 lines lv_size0.
    describe table lt_it9 lines lv_size9.

    if lv_size0 = lv_size9.
      return.
    endif.
    loop at lt_it9 into lv_pernr.
      delete table lt_it0 from lv_pernr.
    endloop.

* Additional Check for
    if mv_z99_condition_it0041_01 eq mc_abap_yes.
      loop at lt_it0 into lv_pernr.
        call method me->get_hiredate_from_it0041
          exporting
            iv_pernr     = lv_pernr
          receiving
            rv_hire_date = lv_hire_date.
        if lv_hire_date gt mv_endda.
          delete table lt_it0 from lv_pernr.
        endif.
      endloop.
    endif.

* Build Result Table
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it0 into ls_result-id.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.

    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr,
      lv_value  type pyd_item_value.


    lv_value = text-001.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
          if ls_err_ov-sfo_tab is initial.
            clear ls_err_ov-sfo_tab.
            clear ls_sfo.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_subty.
            ls_sfo-value  = lv_value.
            ls_sfo-text = text-002.
            insert ls_sfo into table ls_err_ov-sfo_tab.
            modify ct_err_ov from ls_err_ov.

          else.
            loop at ls_err_ov-sfo_tab into ls_sfo.
              case ls_sfo-itemid.
                when mc_itemid_subty.
                  ls_sfo-text = text-002 .
                  modify ls_err_ov-sfo_tab from ls_sfo transporting text.
                  lv_modif = abap_true.
                when others.
                  "nothing
              endcase.
            endloop.
            if lv_modif = abap_true.
              modify ct_err_ov from ls_err_ov.
            endif.
          endif.

        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          add 1 to ls_sfo-row_id.
          ls_sfo-itemid = mc_itemid_subty.
          ls_sfo-value  = lv_value.
          insert ls_sfo into table ls_err_ov-sfo_tab.
          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_HIREDATE_FROM_IT0041.
*
    data: lt_0041 type table of p0041,
          ls_0041 type p0041,
          lv_dar  type p0041-dar01,
          lv_dat  type p0041-dat01.

    rv_hire_date = mc_high_date.
    call function 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr           = iv_pernr
        infty           = '0041'
        begda           = mv_endda
        endda           = mv_endda
        bypass_buffer   = 'X'
      tables
        infty_tab       = lt_0041
      exceptions
        infty_not_found = 1
        others          = 2.

    if sy-subrc <> 0 or lt_0041 is initial.
      exit.
    endif.
* only one entry possible
    read table lt_0041 into ls_0041 index 1.
    do 12 times varying lv_dar from ls_0041-dar01
                               next ls_0041-dar02
                varying lv_dat from ls_0041-dat01
                               next ls_0041-dat02.
      if lv_dar = mc_datetype_hiredate.
        move lv_dat to rv_hire_date.
        exit.
      endif.
    enddo.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.

    data: lt_param_so type /iwbep/t_cod_select_options,
          ls_par      type if_pyd_fnd_types=>ty_s_resp.

    try .
* Hire Date Check
        mv_z99_condition_it0041_01 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_z99_condition_it0041_01
                                                                            it_par      = mo_context->mt_par ).
      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.
ENDCLASS.
