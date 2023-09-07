class ZCL_M99_PA_PERM2CASUAL_CONV definition
  public
  inheriting from ZCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_PER2CASERR type PYD_S_RDSFO_EXT-ITEMID value 'PER2CASERR' ##NO_TEXT.
  constants MC_CONDITION_P2C_STATUS type PYD_PAR_TYPE value 'Z99_CONDITION_P2C_STATUS' ##NO_TEXT.
  protected section.

    types:
      begin of ty_per2cas_dtls ,
        zlwd    type zhrau_ta_per2cas-zlwd,
        zstatus type zhrau_ta_per2cas-zstatus,
      end of ty_per2cas_dtls.
    types:
      begin of ty_per2cas,
        pernr type zhrau_ta_per2cas-pernr.
        include type ty_per2cas_dtls.
      types: end of ty_per2cas .
    types:
      tty_per2cas type standard table of ty_per2cas.

    data mt_filter_status type /iwbep/t_cod_select_options .
    data mt_per2cas type tty_per2cas .
    data ms_per2cas type ty_per2cas.
    data ms_per2cas_dtls type ty_per2cas_dtls.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PA_PERM2CASUAL_CONV IMPLEMENTATION.


  method check.
* check time management status on IT07
    data: lt_per2cas type tty_per2cas,
          ls_result  like line of rt_result.

    select tb_per2cas~pernr, tb_per2cas~zlwd, tb_per2cas~zstatus
      into corresponding fields of table @lt_per2cas
      from zhrau_ta_per2cas as tb_per2cas
      where tb_per2cas~pernr in @it_pernr_so
        and tb_per2cas~zstatus in @mt_filter_status
      and exists ( select 1
                     from pa0000 as it00
                    inner join pa0001 as it01 on
                          it00~pernr = it01~pernr
                    where it00~pernr = tb_per2cas~pernr  and
                          it00~begda <= @mv_endda  and
                          it00~endda >= @mv_begda  and
                          it00~sprps = @if_hrpa_read_infotype=>unlocked and
                          it00~stat2 in @mt_stat2  and
                          it01~begda <= @mv_endda  and
                          it01~endda >= @mv_begda  and
                          it01~sprps = @if_hrpa_read_infotype=>unlocked and
                          it01~abkrs in @mt_payroll_areas and
                          it01~bukrs in @mt_bukrs  and
                          it01~werks in @mt_werks  and
                          it01~btrtl in @mt_btrtl  and
                          it01~persg in @mt_persg  and
                          it01~persk in @mt_persk  and
                          it01~kostl in @mt_kostl
                    )
    order by tb_per2cas~pernr.

    if lt_per2cas is not initial.
      sort lt_per2cas by pernr zlwd.
      mt_per2cas[] = lt_per2cas[].

      delete adjacent duplicates from lt_per2cas comparing pernr.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_per2cas into data(ls_per2cas).
        ls_result-id = ls_per2cas-pernr.
        insert ls_result into table rt_result.
      endloop.
    endif.

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
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr,
      lv_value  type pyd_item_value.
    data: lv_text type pyd_name.

    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
    data: lv_it_begda(10) type c.
    data: lv_threshold_vlaue type ltext,
          lv_exc_sum         type ltext.
    data: lt_dd07v_tab type table of dd07v.

* Populate SFO tab
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

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
                p_struct2 = ms_per2cas_dtls.

            move-corresponding ms_per2cas_dtls to ms_per2cas.
            ms_per2cas-pernr = lv_pernr.

* Message 1
            lv_txt = text-001.
            lv_text = text-002.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_per2caserr
                iv_text                     = lv_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Message 2
            lv_txt = text-003.
            clear: lv_text.
            call function 'DD_DOMVALUES_GET'
              exporting
                domname        = 'ZSTATUS'
                text           = abap_true
                langu          = sy-langu
              tables
                dd07v_tab      = lt_dd07v_tab
              exceptions
                wrong_textflag = 1
                others         = 2.
            loop at lt_dd07v_tab into data(ls_dd07v_tab)
              where domvalue_l = ms_per2cas-zstatus.
              lv_text = ls_dd07v_tab-ddtext.
              exit.
            endloop.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_per2caserr
                iv_text                     = lv_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
* Message 3.
            lv_txt = text-004.
            write: ms_per2cas-zlwd to lv_it_begda dd/mm/yyyy.
            lv_text = lv_it_begda.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_per2caserr
                iv_text                     = lv_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

* Populate PER2CAS details to DB
          loop at mt_per2cas into ms_per2cas
            where pernr = lv_pernr.

            move-corresponding ms_per2cas to ms_per2cas_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_per2cas_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_per2caserr.
            ls_sfo-value  = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_specifc_custmizing.
* Get parameters specific for this validation rule
    try.
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_condition_p2c_status
          changing
            ct_parameter_tab = mt_filter_status.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.
ENDCLASS.
