class ZUSECL_M99_PA_PDC_ERRORS definition
  public
  inheriting from ZuseCL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.
    data mv_kobde type kobde .
    data mc_kobde type pyd_par_type value 'Z99_KOBDE' ##NO_TEXT.
    data mc_itemid_rdc type pyd_itemid value 'RDC'.
    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.


ENDCLASS.



CLASS ZUSECL_M99_PA_PDC_ERRORS IMPLEMENTATION.


  method CHECK.
* Employee with Payroll Corrections Flag
    data: lt_pernr      type standard table of pernr_d with non-unique key table_line.
    data: ls_result     type                   ty_s_result.

    select it00~pernr into table lt_pernr from pa0000 as it00
        inner join pa0001 as it01 on it00~pernr = it01~pernr
        inner join pa0003 as it03 on it00~pernr = it03~pernr
        where it03~pernr in it_pernr_so     and
              it03~begda <= mv_endda        and
              it03~endda >= mv_begda        and
              it00~begda <= mv_endda        and
              it00~endda >= mv_begda        and
              it00~stat2 in mt_stat2        and
              it00~sprps = ' '              and
              it01~begda <= mv_endda        and
              it01~endda >= mv_begda        and
              it01~abkrs in mt_payroll_areas  and
              it03~sprps = ' '              and
              it03~kobde =  mv_kobde        and
              it01~bukrs in mt_bukrs        and
              it01~werks in mt_werks        and
              it01~persg in mt_persg        and
              it01~persk in mt_persk.

    sort lt_pernr. delete adjacent duplicates from lt_pernr.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.

    loop at lt_pernr into ls_result-id.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Method for Overview list display
    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
          if ls_err_ov-sfo_tab is initial.
            clear ls_err_ov-sfo_tab.
            clear ls_sfo.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_rdc.
            ls_sfo-value  = text-001.
            ls_sfo-text = text-002.
            insert ls_sfo into table ls_err_ov-sfo_tab.
            modify ct_err_ov from ls_err_ov.

          else.
            loop at ls_err_ov-sfo_tab into ls_sfo.
              case ls_sfo-itemid.
                when mc_itemid_rdc.
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
          ls_sfo-itemid = 'RDC'.
          ls_sfo-value  = text-001.
          insert ls_sfo into table ls_err_ov-sfo_tab.
          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.
* Read Check Specific Parameters
    try .
        mv_kobde =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_kobde
                                                          it_par      = mo_context->mt_par ).
      catch cx_pyd_fnd into data(lo_exception).

    endtry.

  endmethod.
ENDCLASS.
