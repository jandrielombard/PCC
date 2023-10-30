class ZUSECL_M43_PA_TERM_WB definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
protected section.

  types:
    begin of ty_it0000,
        pernr type p0000-pernr,
        begda type p0000-begda,
        massn type p0000-massn,
        massg type p0000-massg,
        mgtxt type t530t-mgtxt,
        aedtm type p0000-aedtm.
    types: end of ty_it0000 .
  types:
    tty_it0000 type table of ty_it0000 .
  types:
    begin of ty_it0000_dtls,
        begda type p0000-begda,
        massn type p0000-massn,
        massg type p0000-massg,
        aedtm type p0000-aedtm.
    types: end of ty_it0000_dtls .
  types:
    begin of ty_it0416,
        pernr type persno,
        subty type p0416-subty,
        begda type p0416-begda,
        aedtm type p0416-aedtm.
    types: end of ty_it0416 .

  constants MC_ITEMID_EXETERMWB type PYD_S_RDSFO_EXT-ITEMID value 'EXETERMWB' ##NO_TEXT.
  constants MC_MASSN_TERMINATION type P0000-MASSN value 'ZT' ##NO_TEXT.
  constants MC_IT0416_TWB_SUBTY type P0416-SUBTY value '4301' ##NO_TEXT.
  data MT_IT0000 type TTY_IT0000 .
  data MS_IT0000 type TY_IT0000 .
  data MS_IT0000_DTLS type TY_IT0000_DTLS .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M43_PA_TERM_WB IMPLEMENTATION.


  method CHECK.
* Termination in Current Period. Process Termination workbench.
    data: ls_result like line of rt_result.
    data: lt_all_it0000 type table of ty_it0000.
    data: lt_err_it0000 type table of ty_it0000.

    data: ls_it0000 type ty_it0000.
    data: lt_it0416 type table of ty_it0416.
    data: ls_it0416 type ty_it0416.
    data: lv_term_date type datum.

*All terminated employees in the current period
    select it0~pernr, it0~begda, it0~massn, it0~massg, it0~aedtm
      into corresponding fields of table @lt_all_it0000
      from pa0000 as it0
      where it0~pernr in @it_pernr_so
         and it0~sprps = @if_hrpa_read_infotype=>unlocked
         and it0~endda >= @mv_begda
         and it0~begda <= @mv_endda
         and it0~massn = @mc_massn_termination
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it0~pernr
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
    order by it0~pernr.

* Read IT 0416 for all terminated Employees
    if not lt_all_it0000[] is initial.
      select it0416~pernr, it0416~subty, it0416~begda, it0416~aedtm
        into corresponding fields of table @lt_it0416
        from pa0416 as it0416
        for all entries in @lt_all_it0000
          where pernr = @lt_all_it0000-pernr
            and it0416~subty = @mc_it0416_twb_subty
            and it0416~sprps = @if_hrpa_read_infotype=>unlocked.
    endif.

* Check if P0000-MASSN is ZT in current period
* Check if P2013 subtype '60' or P0416 subtype '4301' record does not exist on P0000-MASSN = 'ZT' start date.
* Raise an alert for processing termination workbench
    loop at lt_all_it0000 into ls_it0000.
      lv_term_date = ls_it0000-begda - 1.
      read table lt_it0416 into ls_it0416
           with key pernr = ls_it0000-pernr
                    begda = lv_term_date.
      if sy-subrc eq 0.
        " Check  change date is before IT0416 date
        if ls_it0000-aedtm gt ls_it0416-aedtm.
          append ls_it0000 to lt_err_it0000.
        endif.
      else.
        append ls_it0000 to lt_err_it0000.
      endif.
    endloop.
*
    mt_it0000[] = lt_err_it0000[].

* Report all Error Employees
    sort lt_err_it0000.
    delete adjacent duplicates from lt_err_it0000 comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_err_it0000 into ls_it0000.
      ls_result-id = ls_it0000-pernr.
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

    data: lv_text type pyd_name.
    data: lv_date type begda.
    data: lv_term_date(10) type c.
    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_term_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.
    data: lv_massg_text type t530t-mgtxt.

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
          lv_value = text-004.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_exetermwb
              iv_text                     = |{ text-001 }|
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_it0000_dtls.

            move-corresponding ms_it0000_dtls to ms_it0000.
            move lv_pernr to ms_it0000-pernr.

* Read Termination Reason text
            if ms_it0000-massn is not initial and
               ms_it0000-massg is not initial.
              select single mgtxt into ms_it0000-mgtxt
                from t530t
               where sprsl  = sy-langu
                 and massn  = ms_it0000-massn
                 and massg  = ms_it0000-massg.
              concatenate ms_it0000-mgtxt  '(' ms_it0000-massg ')'
                into lv_massg_text separated by space.
            endif.

            lv_date = ms_it0000-begda - 1.
            write: lv_date to lv_term_date dd/mm/yyyy.

            " Termibation Date
            lv_value = lv_term_date.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_exetermwb
                iv_text                     = |{ text-002 }|
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

            " termination Reason
            lv_value = lv_massg_text.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_exetermwb
                iv_text                     = |{ text-003 }|
                iv_value                    = lv_value
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

* Populate SFO tab for Table Save
          loop at mt_it0000 into ms_it0000
              where pernr = lv_pernr.

            move-corresponding ms_it0000 to ms_it0000_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_it0000_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_exetermwb.
            ls_sfo-value  = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.
ENDCLASS.
