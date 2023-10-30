class ZUSECL_M13_IT0001_IT0008_FLDS definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
protected section.

  types:
    begin of ty_data,
        pernr       type pernr_d,
        bukrs       type bukrs,
        werks       type werks,
        btrtl       type btrtl,
        persg       type persg,
        persk       type persk,
        kostl       type kostl,
        trfar	      type trfar,
        trfgb	      type trfgb,
        trfar_trfgb type char4,
      end of ty_data .
  types:
    tty_data type standard table of ty_data .
  types:
    begin of ty_it1a8_dtls,
        bukrs       type bukrs,
        werks       type werks,
        btrtl       type btrtl,
        persg       type persg,
        persk       type persk,
        kostl       type kostl,
        trfar	      type trfar,
        trfgb	      type trfgb,
        trfar_trfgb type char4,
      end of ty_it1a8_dtls .

  constants MC_ITEMID_IT1A8_ERR type PYD_S_RDSFO_EXT-ITEMID value 'IT1A8ERR' ##NO_TEXT.
  constants MC_TRFAR type PYD_PAR_TYPE value 'Z99_TRFAR' ##NO_TEXT.
  constants MC_EXC_TRFAR type PYD_PAR_TYPE value 'Z99_EXC_TRFAR' ##NO_TEXT.
  constants MC_TRFGB type PYD_PAR_TYPE value 'Z99_TRFGB' ##NO_TEXT.
  constants MC_EXC_TRFGB type PYD_PAR_TYPE value 'Z99_EXC_TRFGB' ##NO_TEXT.
  constants MC_TRFAR_TRFGB type PYD_PAR_TYPE value 'Z99_TRFAR_TRFGB' ##NO_TEXT.
  constants MC_FIELDNAME type PYD_PAR_TYPE value 'Z99_FIELDNAME' ##NO_TEXT.
  constants MC_FLD_BUKRS type FIELDNAME value 'BUKRS' ##NO_TEXT.
  constants MC_FLD_WERKS type FIELDNAME value 'WERKS' ##NO_TEXT.
  constants MC_FLD_BTRTL type FIELDNAME value 'BTRTL' ##NO_TEXT.
  constants MC_FLD_PERSG type FIELDNAME value 'PERSG' ##NO_TEXT.
  constants MC_FLD_PERSK type FIELDNAME value 'PERSK' ##NO_TEXT.
  constants MC_FLD_KOSTL type FIELDNAME value 'KOSTL' ##NO_TEXT.
  constants MC_FLD_TRFAR type FIELDNAME value 'TRFAR' ##NO_TEXT.
  constants MC_FLD_TRFGB type FIELDNAME value 'TRFGB' ##NO_TEXT.
  constants MC_FLD_TRFAR_TRFGB type FIELDNAME value 'TRFAR_TRFGB' ##NO_TEXT.
  data MT_IT1A8_ERR type TTY_DATA .
  data MT_TRFAR type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_TRFGB type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_TRFAR_TRFGB type /IWBEP/T_COD_SELECT_OPTIONS .
  data MV_FIELDNAME type FIELDNAME .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M13_IT0001_IT0008_FLDS IMPLEMENTATION.


  method CHECK.

    data: lt_data      type tty_data,
          ls_it1a8_err like line of mt_it1a8_err,
          ls_result    like line of rt_result.

* Read Infotype 0001 and 0008 Data
    select distinct it08~pernr, it01~endda, it01~begda, it01~bukrs,
                    it01~werks, it01~btrtl, it01~persg, it01~persk,
                    it08~trfar, it08~trfgb, it08~trfar && it08~trfgb as trfar_trfgb
      into corresponding fields of table @lt_data
      from pa0001 as it01 inner join pa0008 as it08 on
        it01~pernr = it08~pernr and
        it01~endda >= it08~begda and
        it01~begda <= it08~endda
    where it01~pernr in @it_pernr_so
      and    it01~endda >= @mv_begda
      and    it01~begda <= @mv_endda
      and    it01~sprps = @if_hrpa_read_infotype=>unlocked
      and    it01~abkrs in @mt_payroll_areas
      and    it01~bukrs in @mt_bukrs
      and    it01~werks in @mt_werks
      and    it01~btrtl in @mt_btrtl
      and    it01~persg in @mt_persg
      and    it01~persk in @mt_persk
      and    it01~kostl in @mt_kostl
      and    it08~trfar in @mt_trfar
      and    it08~trfgb in @mt_trfgb
      and exists ( select 1
                   from pa0000 as it00
                   where it00~pernr = it01~pernr    and
                         it00~endda >= it01~begda   and
                         it00~begda <= it01~endda   and
                         it00~endda >= @mv_begda    and
                         it00~begda <= @mv_endda    and
                         it00~stat2 in @mt_stat2    and
                         it00~sprps = @if_hrpa_read_infotype=>unlocked ).

* Filter the data by Z99_TRFAR_TRFGB
    if not mt_trfar_trfgb is initial.
      delete lt_data where trfar_trfgb not in mt_trfar_trfgb.
    endif.

* Populate Results Table
    if not lt_data is initial.
      delete adjacent duplicates from lt_data comparing pernr.
      loop at lt_data into data(ls_data).
        clear: ls_it1a8_err.
        move-corresponding ls_data to ls_it1a8_err.
        append ls_it1a8_err to mt_it1a8_err.

        clear: ls_result.
        ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
        ls_result-id = ls_it1a8_err-pernr.
        insert ls_result into table rt_result.
      endloop.
    endif.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.

    data:
      ls_err_ov    type ty_s_err_ov,
      ls_sfo       type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif     type abap_bool,
      lv_pernr     type p_pernr,
      lv_value     type pyd_s_rdsfo_ext-value,
      ls_it1a8_err like line of mt_it1a8_err.


    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_it1a8_err into ls_it1a8_err
            where pernr = lv_pernr.

            case mv_fieldname.
              when mc_fld_bukrs.
                message e025 into lv_value.
              when mc_fld_werks.
                message e026 into lv_value.
              when mc_fld_btrtl.
                message e027 into lv_value.
              when mc_fld_persg.
                message e028 into lv_value.
              when mc_fld_persk.
                message e029 into lv_value.
              when mc_fld_kostl.
                message e031 into lv_value.
              when mc_fld_trfar.
                message e032 into lv_value.
              when mc_fld_trfgb.
                message e033 into lv_value.
              when mc_fld_trfar_trfgb.
                message e034 into lv_value.
            endcase.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_it1a8_err.
            ls_sfo-text   = text-001.
            ls_sfo-value  = lv_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.

    try.
* Pay scale type
        refresh mt_trfar.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_trfar
            iv_exc_par_type  = me->mc_exc_trfar
          changing
            ct_parameter_tab = mt_trfar.
* Pay Scale Area
        refresh  mt_trfgb.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_trfgb
            iv_exc_par_type  = me->mc_exc_trfgb
          changing
            ct_parameter_tab = mt_trfgb.
* Pay scale type and Pay Scale Area Combination
        refresh mt_trfar_trfgb.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_trfar_trfgb
          changing
            ct_parameter_tab = mt_trfar_trfgb.

* Validation Fieldname
        mv_fieldname =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_fieldname
                                                              it_par      = mo_context->mt_par ).

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.
ENDCLASS.
