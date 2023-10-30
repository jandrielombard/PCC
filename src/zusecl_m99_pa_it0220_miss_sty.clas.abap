class ZUSECL_M99_PA_IT0220_MISS_STY definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
  protected section.

    constants mc_itemid_subty type pyd_s_rdsfo_ext-itemid value 'SUBTY' ##NO_TEXT.
    constants mc_filter_subty_01 type pyd_par_type value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_exc_filter_subty_01 type pyd_par_type value 'Z99_EXC_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_z13_medcl type pyd_par_type value 'Z13_MEDCL' ##NO_TEXT.

    types:
      begin of ty_output_db,
        begda type pa0000-begda,
      end of ty_output_db,
      begin of ty_output,
        pernr type pa0000-pernr.
        include type ty_output_db.
      types: end of ty_output .
    types:
      tty_output type standard table of ty_output with non-unique key pernr begda .
*
    types: begin of ty_it0220,
             pernr type pa0220-pernr,
             begda type pa0220-begda,
             medcl type pa0220-medcl,
             conam type pa0220-conam.
    types: end of ty_it0220 .
    types:
      tty_it0220 type standard table of ty_it0220  .

    data mt_filter_subty_01 type /iwbep/t_cod_select_options .
    data mt_medcl type /iwbep/t_cod_select_options .
    data mt_output type tty_output .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT0220_MISS_STY IMPLEMENTATION.


  method CHECK.
*check if any team member does not have certain IT0220 record
    data: lt_output type  tty_output,
          ls_output type  ty_output.
    data: lt_it0220 type  tty_it0220,
          ls_it0220 type  ty_it0220.
    data: lv_exclude type boolean.
    data: lt_exclude_pernr type hr99s_pernr_range,
          ls_exclude_pernr type sel_pernr.

    select distinct it00~pernr, it00~begda
      into corresponding fields of table @lt_output
      from pa0000 as it00
      where it00~pernr in @it_pernr_so                    and
            it00~sprps = @if_hrpa_read_infotype=>unlocked and
            it00~begda <= @mv_endda                       and
            it00~endda >= @mv_begda                       and
            it00~stat2 in @mt_stat2                       and
            exists (
              select 1
              from pa0001 as it01
              where it01~pernr = it00~pernr                       and
                    it01~begda <= it00~endda                      and
                    it01~endda >= it00~begda                      and
                    it01~begda <= @mv_endda                       and
                    it01~endda >= @mv_begda                       and
                    it01~sprps = @if_hrpa_read_infotype=>unlocked and
                    it01~abkrs in @mt_payroll_areas               and
                    it01~bukrs in @mt_bukrs                       and
                    it01~werks in @mt_werks                       and
                    it01~btrtl in @mt_btrtl                       and
                    it01~persg in @mt_persg                       and
                    it01~persk in @mt_persk                       and
                    it01~kostl in @mt_kostl
              ) and
            not exists (
              select 1
              from pa0220 as it220
              where it220~pernr = it00~pernr                       and
                    it220~sprps = @if_hrpa_read_infotype=>unlocked and
                    it220~begda <= it00~endda                      and
                    it220~endda >= it00~begda                      and
                    it220~begda <= @mv_endda                       and
                    it220~endda >= @mv_begda                       and
                    it220~subty in @mt_filter_subty_01
              ).

* Where missing subtype found, then also check
* If an active IT0220 record exists,
* - if no raise alert
* - if yes then check value of field P0220-MEDCL
* ==> if = S2, then exclude employee
* ==> if <> S2 then raise alert
    if not lt_output[] is initial.
      select it0220~pernr, it0220~medcl, it0220~conam
        into corresponding fields of table @lt_it0220
              from pa0220 as it0220
        for all entries in @lt_output
       where it0220~pernr = @lt_output-pernr
         and it0220~sprps = @if_hrpa_read_infotype=>unlocked
         and it0220~begda <= @mv_endda
         and it0220~endda >= @mv_begda.
    endif.

* Delete selected Employees from Intial list
    ls_exclude_pernr-sign = 'I'. ls_exclude_pernr-option = 'EQ'.
    loop at lt_it0220 into ls_it0220.
      delete lt_output where pernr = ls_it0220-pernr.

* Check the Existence of Superannuation Medical Classification
      clear lv_exclude.
      if ls_it0220-medcl in mt_medcl.
        lv_exclude = abap_true.
      else.
        if ls_it0220-conam > 0.
* After checking Z13_MEDCL then check if P0220-CONAM has a value (> 0)
* ==> if P0220-CONAM  has value, then exclude employee
          lv_exclude = abap_true.
        endif.
      endif.

      if lv_exclude eq abap_true.
        move ls_it0220-pernr to ls_exclude_pernr-low.
        collect ls_exclude_pernr into lt_exclude_pernr.
      endif.
    endloop.

** Check the Existence of Superannuation Medical Classification
*    delete lt_it0220 where medcl in mt_medcl.
** After checking Z13_MEDCL then check if P0220-CONAM has a value (> 0)
** ==> if P0220-CONAM  has value, then exclude employee
*    delete lt_it0220 where conam > 0.
    delete lt_it0220 where pernr in lt_exclude_pernr.

    delete adjacent duplicates from lt_it0220 comparing pernr.
* Collect data for Overview
* Delete selected Employees from Intial list
    loop at lt_it0220 into ls_it0220.
      move-corresponding ls_it0220 to ls_output.
      append ls_output to lt_output.
    endloop.
    append lines of lt_output to mt_output.

* Build result table
    loop at lt_output into ls_output.
      insert value #(
          par_type = if_pyd_cont_types=>gcs_par_type-pernr
          id = ls_output-pernr )
        into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method ERR_OV_GET_LIST.
* Display error message
    data:
      ls_err_ov       type ty_s_err_ov,
      ls_sfo          type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr        type p_pernr,
      lv_text         type text120,
      lv_value        type char060,
      lt_sfo_tab_temp like ls_err_ov-sfo_tab,
      lv_value_string type string,
      ls_output_db    type ty_output_db.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        message i075(zhrpy_pcc_msg) into lv_value_string.

        loop at ct_err_ov into ls_err_ov.
          "move SFO_TAB records to temporary table
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.
          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            clear: ls_output_db, lv_value.
            if ls_sfo_tab_temp-itemid = mc_itemid_subty.
              lv_value = ls_sfo_tab_temp-value.
              call method me->copy_structure_to_other
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ls_output_db.

              lv_text = text-001.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_subty
                  iv_text                     = |{ lv_text }|
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.
            else.
              insert ls_sfo_tab_temp into table ls_err_ov-sfo_tab.
            endif.
          endloop.
          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.
          loop at mt_output into data(ls_output) where pernr = lv_pernr.
            clear: lv_value, ls_output_db.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_subty.
            ls_output_db = corresponding #( ls_output ).

            call method me->copy_structure_to_other
              exporting
                p_struct1 = ls_output_db
              changing
                p_struct2 = lv_value.

            ls_sfo-value = lv_value.
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
        loop at mo_context->mt_par into data(ls_par) where
          par_type = mc_filter_subty_01 or
          par_type = mc_exc_filter_subty_01.
          append value #(
            sign = switch #( ls_par-par_type
                    when mc_filter_subty_01 then if_dmf_constants_c=>gc_range_sign_inclusive
                    else if_dmf_constants_c=>gc_range_sign_exclusive )
            option = if_dmf_constants_c=>gc_range_option_cp
            low = ls_par-low ) to mt_filter_subty_01.
        endloop.

* Read Medical Class
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_z13_medcl
          changing
            ct_parameter_tab = mt_medcl.
    endtry.
  endmethod.
ENDCLASS.
