class ZUSECL_M43_PA_AL_ANN_MOVE definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
protected section.

  types:
    begin of ty_anzhl_data,
        dct_pernr type p_pernr,
        lgart     type lgart,
        anzhl     type pranz,
      end of ty_anzhl_data .
  types:
    tty_anzhl_data type table of ty_anzhl_data .
  types TY_IT0041 type P0041 .
  types:
    tty_it0041 type table of p0041 .
  types:
    begin of ty_it0041_chg,
             pernr   type p0041-pernr,
             changed type boolean,
           end of ty_it0041_chg .
  types:
    tty_it0041_chg type table of ty_it0041_chg .

  constants MC_ITEMID_ANNDTMOVE type PYD_S_RDSFO_EXT-ITEMID value 'ANNDTMOVE' ##NO_TEXT.
  constants MC_Z99_LGART type PYD_PAR_TYPE value 'Z99_LGART' ##NO_TEXT.
  data MV_PREV_PAYROLL_PERIOD type IPERI .
  data MV_PREV_PERIOD_ENDDA type ENDDA .
  data MT_Z99_LGART type /IWBEP/T_COD_SELECT_OPTIONS .
  data MS_Z99_LGART type /IWBEP/S_COD_SELECT_OPTION .
  constants MC_DATETYPE_Z3 type P0041-DAR01 value 'Z3' ##NO_TEXT.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
private section.

  methods GET_IT0041_CHANGED_DATA
    importing
      !IT_ANZHL_DATA type TTY_ANZHL_DATA
    exporting
      !ET_IT0041_CHANGED type TTY_PERNR .
ENDCLASS.



CLASS ZUSECL_M43_PA_AL_ANN_MOVE IMPLEMENTATION.


  method CHECK.
* Monitoring moving of the Annual Leave date in EC from ECP wage type
    data: lt_pernr type table of ty_pernr.
    data: ls_result like line of rt_result.

    data: lt_anzhl_data type standard table of ty_anzhl_data.
    data: ls_anzhl_data type ty_anzhl_data.
    data: lt_it0041 type tty_it0041.
    data: lt_it0041_chg type tty_pernr.

* Select All relevant employees for the Payroll Area
    "LGART Total Inc retro
    refresh: lt_anzhl_data.
    select p2rx_rt~dct_pernr as dct_pernr, p2rx_rt~lgart as lgart,
          sum( case p2rx_eval_period~srtza when 'P' then anzhl * -1
                        else anzhl end ) as anzhl
            into corresponding fields of table @lt_anzhl_data
                     from p2rx_eval_period inner join p2rx_rt
                                   on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                                  and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
           where p2rx_eval_period~dct_pernr in @it_pernr_so
             and p2rx_eval_period~abkrs in @mt_payroll_areas
             and p2rx_eval_period~inper = @mv_prev_payroll_period
             and p2rx_rt~lgart in @mt_z99_lgart
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

* All relevant employees with IT0041 change
    if not lt_anzhl_data[] is initial.
      call method me->get_it0041_changed_data
        exporting
          it_anzhl_data     = lt_anzhl_data
        importing
          et_it0041_changed = lt_it0041_chg.
    endif.

* If WT 9ALD is generated in previous period,
* check if P0041 is created with a new record from start of current pay period
* Raise an alert to update Annual Leave Anniversary date
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_anzhl_data into ls_anzhl_data.
      read table lt_it0041_chg into data(ls_it0041_chg)
       with key dct_pernr =  ls_anzhl_data-dct_pernr binary search.
      if sy-subrc <> 0.
* Build Results table
        ls_result-id = ls_anzhl_data-dct_pernr.
        insert ls_result into table rt_result.
      endif.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Display error message
    data:
      ls_err_ov       type ty_s_err_ov,
      lv_value_string type string.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          lv_value_string = text-002.
          me->add_record_to_sfo_tab(
            exporting
              iv_itemid                   = mc_itemid_anndtmove
              iv_text                     = |{ text-001 }|
              iv_value                    = lv_value_string
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab ).
          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_IT0041_CHANGED_DATA.
* check if P0041 is created with a new record from start of current pay period
    data: lt_cp_it0041 type tty_it0041,
          lt_pp_it0041 type tty_it0041.
    data: ls_cp_it0041 type ty_it0041,
          ls_pp_it0041 type ty_it0041.
    data: lv_dar type p0041-dar01,
          lv_dat type p0041-dat01.
    data: lv_cp_z3_date type datum,
          lv_pp_z3_date type datum.

* read IT 0041 data for the relevant Employees
    check not it_anzhl_data[] is initial.
    "Current Period Records
    select * into corresponding fields of table @lt_cp_it0041
          from pa0041 as it0041
          for all entries in @it_anzhl_data
            where pernr = @it_anzhl_data-dct_pernr
              and it0041~sprps = ' '
              and it0041~begda <= @mv_begda
              and it0041~endda >= @mv_begda.
    sort lt_cp_it0041 by pernr.

    "Prev Period IT0041 Records
    select * into corresponding fields of table @lt_pp_it0041
      from pa0041 as it0041
      for all entries in @it_anzhl_data
        where pernr = @it_anzhl_data-dct_pernr
          and it0041~sprps = ' '
          and it0041~begda <= @mv_prev_period_endda
          and it0041~endda >= @mv_prev_period_endda.
    sort lt_pp_it0041 by pernr.

    loop at it_anzhl_data into data(ls_anzhl_data).
* Read Current Period IT 0041 Record
      clear: ls_cp_it0041.
      read table lt_cp_it0041 into ls_cp_it0041
        with key pernr =  ls_anzhl_data-dct_pernr binary search.
* Read Prev Period IT 0041 Record
      clear: ls_pp_it0041.
      read table lt_pp_it0041 into ls_pp_it0041
       with key pernr =  ls_anzhl_data-dct_pernr binary search.

* Check the bigin date
      check ls_cp_it0041-begda = mv_begda.
* Check Z3 date is changed or not
* Read Current Period Z3 date
      do 12 times varying lv_dar from ls_cp_it0041-dar01
                                 next ls_cp_it0041-dar02
                  varying lv_dat from ls_cp_it0041-dat01
                                 next ls_cp_it0041-dat02.
        if lv_dar = mc_datetype_z3.
          move lv_dat to lv_cp_z3_date.
          exit.
        endif.
      enddo.

* Read Prev Period Period Z3 date
      do 12 times varying lv_dar from ls_pp_it0041-dar01
                                  next ls_pp_it0041-dar02
                   varying lv_dat from ls_pp_it0041-dat01
                                  next ls_pp_it0041-dat02.
        if lv_dar = mc_datetype_z3.
          move lv_dat to lv_pp_z3_date.
          exit.
        endif.
      enddo.

* Collect Employees whose Z3 date not same.
      if lv_cp_z3_date <> lv_pp_z3_date.
        append ls_cp_it0041-pernr to et_it0041_changed.
      endif.
    endloop.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* read check specific parameters
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

    try .
* Wage Types Selection
        refresh:  mt_z99_lgart.
        loop at mo_context->mt_par into ls_par where par_type = me->mc_z99_lgart.
          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
          append lines of lt_param_so to mt_z99_lgart.
        endloop.

* Previous Payroll Period
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
        mv_prev_period_endda = mv_begda - 1.
      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.
ENDCLASS.
