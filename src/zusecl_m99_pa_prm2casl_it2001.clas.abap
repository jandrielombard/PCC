class ZUSECL_M99_PA_PRM2CASL_IT2001 definition
  public
  inheriting from ZuseCL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_PER2CASERR type PYD_S_RDSFO_EXT-ITEMID value 'PER2CASERR' ##NO_TEXT.
  constants MC_CONDITION_P2C_STATUS type PYD_PAR_TYPE value 'Z99_CONDITION_P2C_STATUS' ##NO_TEXT.
  constants MC_INFTY_2001 type INFTY value '2001' ##NO_TEXT.
  protected section.

    types:
      begin of ty_absence_dtls ,
        zlwd     type zuse_ta_per2cas-zlwd,
        zstatus  type zuse_ta_per2cas-zstatus,
        awart    type awart,
        ab_begda type begda,
        ab_endda type endda,
        seqnr    type seqnr,
      end of ty_absence_dtls.
    types:
      begin of ty_absence,
        pernr    type zuse_ta_per2cas-pernr,
        zlwd     type zuse_ta_per2cas-zlwd,
        zstatus  type zuse_ta_per2cas-zstatus,
        awart    type awart,
        ab_begda type begda,
        ab_endda type endda,
        seqnr    type seqnr,
        abtext   type ltext,
      end of ty_absence.
    types:
     tty_absence type standard table of ty_absence.
    types:
      begin of ty_per2cas,
        pernr   type zuse_ta_per2cas-pernr,
        zlwd    type zuse_ta_per2cas-zlwd,
        zstatus type zuse_ta_per2cas-zstatus.
    types:  end of ty_per2cas.
    types:
      tty_per2cas type standard table of ty_per2cas.
    types:
      begin of ty_it2001,
        pernr    type pernr_d,
        awart    type awart,
        ab_begda type begda,
        ab_endda type endda,
        seqnr    type seqnr,
      end of ty_it2001 .

    data mt_filter_status type /iwbep/t_cod_select_options .
    data mt_absence type tty_absence .
    data ms_absence type ty_absence.
    data ms_absence_dtls type ty_absence_dtls.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_PRM2CASL_IT2001 IMPLEMENTATION.


  method CHECK.
* check time management status on IT07
    data: lt_per2cas type tty_per2cas,
          ls_per2cas type ty_per2cas,
          lt_it2001  type table of ty_it2001,
          ls_it2001  type ty_it2001,
          ls_result  like line of rt_result.
    data: lv_index type sy-tabix.

    select tb_per2cas~pernr, tb_per2cas~zlwd, tb_per2cas~zstatus
      into corresponding fields of table @lt_per2cas
      from zuse_ta_per2cas as tb_per2cas
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
      delete adjacent duplicates from lt_per2cas comparing all fields.
      " read futuer Absences
      select it2001~pernr as pernr, it2001~awart as awart,
        it2001~begda as ab_begda, it2001~endda as ab_endda, it2001~seqnr as seqnr
        into corresponding fields of table @lt_it2001
        from pa2001 as it2001
         for all entries in @lt_per2cas
      where it2001~pernr = @lt_per2cas-pernr
        and it2001~subty in @mt_subty
        and it2001~sprps in @mt_sprps
        and it2001~begda > @lt_per2cas-zlwd.
      sort lt_it2001 by pernr ab_begda ab_endda awart seqnr.
      delete adjacent duplicates from lt_it2001 comparing all fields.

      " Collect Data for Overview Display
      loop at lt_per2cas into ls_per2cas.
        read table lt_it2001 transporting no fields
          with key pernr = ls_per2cas-pernr binary search.
        if sy-subrc eq 0.
          lv_index = sy-tabix.
          loop at lt_it2001 from lv_index into ls_it2001.
            if not ls_it2001-pernr eq ls_per2cas-pernr .
              exit.
            endif.

            if ls_it2001-ab_begda > ls_per2cas-zlwd.
              move-corresponding ls_it2001 to ms_absence.
              move: ls_per2cas-zlwd to ms_absence-zlwd,
                    ls_per2cas-zstatus to ms_absence-zstatus.
              append ms_absence to mt_absence.
            endif.

          endloop.
        endif.
      endloop.

      " Prepare Result
      delete adjacent duplicates from lt_it2001 comparing pernr.
      ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      loop at lt_it2001 into ls_it2001.
        ls_result-id = ls_it2001-pernr.
        insert ls_result into table rt_result.
      endloop.
    endif.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Method for Overview list display
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
    data: lv_ab_begda(10) type c,
          lv_ab_endda(10) type c.
    data: lv_threshold_vlaue type ltext,
          lv_exc_sum         type ltext.
    data: lt_dd07v_tab type table of dd07v.
    data: lv_prev_zlwd    type zuse_ta_per2cas-zlwd,
          lv_prev_zstatus type zuse_ta_per2cas-zstatus.

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

* Generic Message
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

          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_absence_dtls.

            move-corresponding ms_absence_dtls to ms_absence.
            ms_absence-pernr = lv_pernr.

            if not ( ms_absence-zlwd eq lv_prev_zlwd and
                     ms_absence-zstatus eq lv_prev_zstatus ).
* Transfer Satus
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
                where domvalue_l = ms_absence-zstatus.
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
* Tarnsfer Date
              lv_txt = text-004.
              write: ms_absence-zlwd to lv_it_begda dd/mm/yyyy.
              lv_text = lv_it_begda.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_per2caserr
                  iv_text                     = lv_txt
                  iv_value                    = lv_text
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.

              lv_prev_zlwd = ms_absence-zlwd.
              lv_prev_zstatus = ms_absence-zstatus.
            endif.

* Future Absence Details
            lv_txt = text-005.
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_infty_2001
                subty               = ms_absence-awart
                persnr              = ms_absence-pernr
                begda               = ms_absence-ab_begda
                endda               = ms_absence-ab_begda
                molga               = mv_molga
              importing
                stext               = ms_absence-abtext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.

            concatenate ms_absence-abtext '(' ms_absence-awart ')'
             into ms_absence-abtext.
            message i047(zhrpy_pcc_msg) with ms_absence-abtext
                    ms_absence-ab_begda ms_absence-ab_endda into lv_text.
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
          loop at mt_absence into ms_absence
            where pernr = lv_pernr.

            move-corresponding ms_absence to ms_absence_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_absence_dtls
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


  method GET_SPECIFC_CUSTMIZING.
* Get parameters specific for this validation rule
    try.
* Permanent to Casual transfer Status
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_condition_p2c_status
          changing
            ct_parameter_tab = mt_filter_status.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.
ENDCLASS.
