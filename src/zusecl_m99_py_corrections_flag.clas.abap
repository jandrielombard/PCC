class ZUSECL_M99_PY_CORRECTIONS_FLAG definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
protected section.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
private section.

  data MV_KOBDE type KOBDE .
  data MC_KOBDE type PYD_PAR_TYPE value 'Z99_KOBDE' ##NO_TEXT.
ENDCLASS.



CLASS ZUSECL_M99_PY_CORRECTIONS_FLAG IMPLEMENTATION.


  method CHECK.
* Employee with Payroll Corrections Flag
    data: lt_pernr      type standard table of pernr_d with non-unique key table_line.
    data: ls_result     type                   ty_s_result.

    select mcw~pernr into table @lt_pernr from m_premw as mcw
      inner join pa0003 as it03 on mcw~pernr = it03~pernr
      where mcw~pernr in @it_pernr_so
        and mcw~koabr eq 'X'
        and it03~abrdt <= @mv_endda
        and it03~sprps = ' '
        and exists ( select 1
                       from pa0000 as it0000 inner join pa0001 as it0001 on
                            it0000~pernr = it0001~pernr
                      where it0000~pernr = mcw~pernr
                        and it0000~begda <= @mv_endda
                        and it0000~endda >= @mv_begda
                        and it0000~stat2 in @mt_stat2
                        and it0000~sprps = ' '
                        and it0001~begda <= @mv_endda
                        and it0001~endda >= @mv_begda
                        and it0001~sprps = ' '
                        and it0001~abkrs in @mt_payroll_areas
                        and it0001~bukrs in @mt_bukrs
                        and it0001~werks in @mt_werks
                        and it0001~btrtl in @mt_btrtl
                        and it0001~persg in @mt_persg
                        and it0001~persk in @mt_persk
                        and it0001~kostl in @mt_kostl ).

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
            ls_sfo-itemid = 'MCW'.
            ls_sfo-value  = text-001.
            ls_sfo-text = text-002.
            insert ls_sfo into table ls_err_ov-sfo_tab.
            modify ct_err_ov from ls_err_ov.

          else.
            loop at ls_err_ov-sfo_tab into ls_sfo.
              case ls_sfo-itemid.
                when 'MCW'.
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
          ls_sfo-itemid = 'MCW'.
          ls_sfo-value  = text-001.
          insert ls_sfo into table ls_err_ov-sfo_tab.
          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.
ENDCLASS.
