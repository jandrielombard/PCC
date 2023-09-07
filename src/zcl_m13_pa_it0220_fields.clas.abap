class zcl_m13_pa_it0220_fields definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  create public .

  public section.

    types:
      begin of ty_it41,
        pernr type pa0041-pernr,
        begda type pa0041-begda,
        endda type pa0041-endda.
        include type ps0041.
      types: end of ty_it41 .

    constants mc_itemid_it0220_fields type pyd_s_rdsfo_ext-itemid value 'IT0220' ##NO_TEXT.
    constants mc_filter_subty_01 type pyd_par_type value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_exc_filter_subty_01 type pyd_par_type value 'Z99_EXC_FILTER_SUBTY_01' ##NO_TEXT.
    constants mc_filter_dpcon_low type pyd_par_type value 'Z13_DPCON_LOW' ##NO_TEXT.
    constants mc_filter_dpcon_high type pyd_par_type value 'Z13_DPCON_HIGH' ##NO_TEXT.
    constants mc_exc_filter_dpcon_low type pyd_par_type value 'Z13_EXC_DPCON_LOW' ##NO_TEXT.
    constants mc_exc_filter_dpcon_high type pyd_par_type value 'Z13_EXC_DPCON_HIGH' ##NO_TEXT.
    constants mc_filter_conam_low type pyd_par_type value 'Z13_CONAM_LOW' ##NO_TEXT.
    constants mc_filter_conam_high type pyd_par_type value 'Z13_CONAM_HIGH' ##NO_TEXT.
    constants mc_filter_emp_influence type pyd_par_type value 'Z13_EEINF' ##NO_TEXT.
    constants mc_filter_exclusion_flag type pyd_par_type value 'Z13_EXCLF' ##NO_TEXT.
    constants mc_filter_defined_benefits type pyd_par_type value 'Z13_MEDCL' ##NO_TEXT.
    constants mc_filter_trfar type pyd_par_type value 'Z99_TRFAR' ##NO_TEXT.
    constants mc_exc_filter_trfar type pyd_par_type value 'Z99_EXC_TRFAR' ##NO_TEXT.
    constants mc_filter_trfgb type pyd_par_type value 'Z99_TRFGB' ##NO_TEXT.
    constants mc_exc_filter_trfgb type pyd_par_type value 'Z99_EXC_TRFGB' ##NO_TEXT.
    constants mc_filter_trfar_trfgb type pyd_par_type value 'Z99_TRFAR_TRFGB' ##NO_TEXT.
    constants mc_fkber type pyd_par_type value 'Z99_FKBER' ##NO_TEXT.
    constants mc_exc_fkber type pyd_par_type value 'Z99_EXC_FKBER' ##NO_TEXT.
    constants mc_infty_super type infty value '0220' ##NO_TEXT.

protected section.

  types:
    begin of ty_output_db,
        subty       type pa0220-subty,
        objps       type pa0220-objps,
        endda       type pa0220-endda,
        begda       type pa0220-begda,
        seqnr       type pa0220-seqnr,
        dpcon       type pa0220-dpcon,
        conam       type pa0220-conam,
        exclf       type pa0220-exclf,
        eeinf       type pa0220-eeinf,
        medcl       type pa0220-medcl,
        trfar_trfgb type char4,
      end of ty_output_db .
  types:
    begin of ty_output,
        pernr type pa0220-pernr.
        include type ty_output_db.
      types: end of ty_output .
  types:
    tty_output type table of ty_output .
  types:
    tyr_dpcon type range of pa0220-dpcon .
  types:
    tyr_conam type range of pa0220-conam .

  data MT_FILTER_SUBTY_01 type /IWBEP/T_COD_SELECT_OPTIONS .
  data MR_FILTER_DPCON type TYR_DPCON .
  data MR_FILTER_CONAM type TYR_CONAM .
  data MR_FILTER_EEINF type /IWBEP/T_COD_SELECT_OPTIONS .
  data MR_FILTER_EXCLF type /IWBEP/T_COD_SELECT_OPTIONS .
  data MR_FILTER_MEDCL type /IWBEP/T_COD_SELECT_OPTIONS .
  data MR_FILTER_TRFAR type /IWBEP/T_COD_SELECT_OPTIONS .
  data MR_FILTER_TRFGB type /IWBEP/T_COD_SELECT_OPTIONS .
  data MR_FILTER_TRFAR_TRFGB type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_OUTPUT type TTY_OUTPUT .
  data MO_INFTY_ACCESS type ref to CL_HRPA_PLAIN_INFOTYPE_ACCESS .
  constants MC_YES type BOOLEAN value 'Y' ##NO_TEXT.
  constants MC_NO type BOOLEAN value 'N' ##NO_TEXT.
  data MT_FKBER type /IWBEP/T_COD_SELECT_OPTIONS .

  methods GET_DEFAULT_CONTRIBUTION
    importing
      !IS_PSKEY type PSKEY
    returning
      value(RV_DPCON) type P0220-DPCON .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M13_PA_IT0220_FIELDS IMPLEMENTATION.


  method check.
* read check data
    data: lt_output type tty_output.
    data: ls_p0220 type p0220.
    data: lv_index type sy-tabix.

*Check certain combination of IT220 fields is correct or not
    select distinct it220~pernr, it220~subty, it220~objps,
      it220~endda, it220~begda, it220~seqnr,
      it220~dpcon, it220~conam, it220~exclf,
      it220~eeinf, it220~medcl, it08~trfar && it08~trfgb as trfar_trfgb
      into corresponding fields of table @lt_output
      from pa0220 as it220
      inner join pa0008 as it08
         on it08~pernr = it220~pernr
        and it08~begda <= @mv_endda
        and it08~endda >= @mv_begda
      where it220~pernr in @it_pernr_so
        and   it220~sprps = @if_hrpa_read_infotype=>unlocked
        and    it220~begda <= @mv_endda
        and    it220~endda >= @mv_begda
        and    it220~subty in @mt_filter_subty_01
*>>> Donot Apply IT 0220 selections directly, data not stored in Infotype table
*            it220~conam in @mr_filter_conam    and
*            it220~dpcon in @mr_filter_dpcon    and
*            it220~eeinf in @mr_filter_eeinf    and
*            it220~exclf in @mr_filter_exclf    and
*            it220~medcl in @mr_filter_medcl    and
*<<< Donot Apply IT 0220 selections directly, data not stored in Infotype table
        and it08~begda <= @mv_endda
        and it08~endda >= @mv_begda
        and it08~trfar in @mr_filter_trfar
        and it08~trfgb in @mr_filter_trfgb
        and exists ( select 1
                       from pa0000 as it00
                        inner join pa0001 as it01
                           on it00~pernr = it01~pernr
                        where it00~pernr = it220~pernr
                          and it00~begda <= @mv_endda
                          and it00~endda >= @mv_begda
                          and it00~sprps = @if_hrpa_read_infotype=>unlocked
                          and it00~stat2 in @mt_stat2
                          and it01~begda <= @mv_endda
                          and it01~endda >= @mv_begda
                          and it01~sprps = @if_hrpa_read_infotype=>unlocked
                          and it01~bukrs in @mt_bukrs
                          and it01~werks in @mt_werks
                          and it01~btrtl in @mt_btrtl
                          and it01~persg in @mt_persg
                          and it01~persk in @mt_persk
                          and it01~abkrs in @mt_payroll_areas
                          and it01~kostl in @mt_kostl
                          and it01~fkber in @mt_fkber
            )
      order by it220~pernr, it220~subty, it220~objps,
        it220~endda, it220~begda, it220~seqnr.

* Filter the data by Z99_TRFAR_TRFGB
    if not mr_filter_trfar_trfgb is initial.
      delete lt_output where trfar_trfgb not in mr_filter_trfar_trfgb.
    endif.

    if lt_output is not initial.
* Read IT 0220 details
      loop at lt_output assigning field-symbol(<lsf_output>).
        lv_index = sy-tabix.

        if <lsf_output>-dpcon is initial and <lsf_output>-conam is initial.
          <lsf_output>-dpcon = me->get_default_contribution(
            exporting
              is_pskey = value pskey(
                            pernr = <lsf_output>-pernr
                            infty = mc_infty_super
                            subty = <lsf_output>-subty
                            objps = <lsf_output>-objps
                            sprps = if_hrpa_read_infotype=>unlocked
                            endda = <lsf_output>-endda
                            begda = <lsf_output>-begda
                            seqnr = <lsf_output>-seqnr ) ).
        endif.

        modify lt_output from <lsf_output> index lv_index transporting dpcon.
      endloop.

* Apply IT 0220 filters
      delete lt_output
       where not ( conam in mr_filter_conam and
                   dpcon in mr_filter_dpcon and
                   eeinf in mr_filter_eeinf and
                   exclf in mr_filter_exclf and
                   medcl in mr_filter_medcl ).

* Collect data for Overview
      mt_output = lt_output.

* Prepare Result
      delete adjacent duplicates from lt_output comparing pernr.
      loop at lt_output into data(ls_output).
        insert value #(
                  par_type = if_pyd_cont_types=>gcs_par_type-pernr
                  id = ls_output-pernr )
                into table rt_result.
      endloop.
    endif.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Display error message
    data:
      ls_err_ov        like line of ct_err_ov,
      ls_sfo           type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr         type p_pernr,
      lv_text          type text120,
      lv_value         type char060,
      lt_sfo_tab_temp  like ls_err_ov-sfo_tab,
      lv_value_string  type string,
      ls_output_for_db type ty_output_db.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          "move SFO_TAB records to temporary table
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.
          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            clear: ls_output_for_db, lv_value, lv_value_string.
            if ls_sfo_tab_temp-itemid = mc_itemid_it0220_fields.
              lv_value = ls_sfo_tab_temp-value.
              call method me->copy_structure_to_other
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ls_output_for_db.

              do 7 times.
                clear: lv_text, lv_value_string.
                case sy-index.
                  when 1.
                    lv_text = text-001.
                    message i055 into lv_value_string.
                  when 2.
                    lv_text = text-002.
                    message i056 with ls_output_for_db-begda ls_output_for_db-endda into lv_value_string.
                  when 3.
                    lv_text = text-003.
                    lv_value_string = ls_output_for_db-dpcon.
                  when 4.
                    lv_text = text-004.
                    lv_value_string = ls_output_for_db-conam.
                  when 5.
                    lv_text = text-005.
                    lv_value_string = ls_output_for_db-eeinf.
                  when 6.
                    lv_text = text-006.
                    lv_value_string = ls_output_for_db-exclf.
                  when 7.
                    lv_text = text-007.
                    lv_value_string = ls_output_for_db-medcl.
                endcase.

                call method me->add_record_to_sfo_tab
                  exporting
                    iv_itemid                   = mc_itemid_it0220_fields
                    iv_text                     = |{ lv_text }|
                    iv_value                    = lv_value_string
                    iv_text_for_1st_record_only = abap_true
                  changing
                    ct_sfo_tab                  = ls_err_ov-sfo_tab.
              enddo.
            else.
              insert ls_sfo_tab_temp into table ls_err_ov-sfo_tab.
            endif.
          endloop.
          if sy-subrc <> 0.
            lv_text = text-001.
            message i055 into lv_value_string.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_it0220_fields
                iv_text                     = |{ lv_text }|
                iv_value                    = lv_value_string
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endif.
          modify ct_err_ov from ls_err_ov transporting sfo_tab.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_output into data(ls_output) where pernr = lv_pernr.
            clear: lv_value, ls_output_for_db.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_it0220_fields.
            ls_output_for_db = corresponding #( ls_output ).

            call method me->copy_structure_to_other
              exporting
                p_struct1 = ls_output_for_db
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


  method get_default_contribution.
    data: lo_masterdata_bl        type ref to if_hrpa_masterdata_bl,
          lo_hrpa_message_handler type ref to cl_hrpa_message_list,
          ls_p0220                type p0220,
          lv_is_ok                type abap_bool.
    create object lo_hrpa_message_handler.

    try.
        if mo_infty_access is not bound.
          cl_hrpa_masterdata_factory=>get_business_logic(
            importing
              business_logic = lo_masterdata_bl ).
          create object mo_infty_access
            exporting
              masterdata_bl = lo_masterdata_bl.
        endif.

        if mo_infty_access is not bound.
          exit.
        endif.

        mo_infty_access->if_hrpa_plain_infotype_access~get_initial_record(
          exporting
            tclas           = cl_hrpa_tclas=>tclas_employee
            pskey           = is_pskey
            itbld           = space
            massn           = space
            massg           = space
            no_auth_check   = abap_true
            message_handler = lo_hrpa_message_handler
          importing
            pnnnn           = ls_p0220
            is_ok           = lv_is_ok ).
        if lv_is_ok = abap_true.
          rv_dpcon = ls_p0220-dpcon.
        endif.

      catch cx_hrpa_violated_assertion.
    endtry.

  endmethod.


  method get_specifc_custmizing.
* Get parameters specific for this validation rule
    data: ls_filter_dpcon like line of mr_filter_dpcon,
          ls_filter_conam like line of mr_filter_conam,
          ls_filter_eeinf like line of mr_filter_eeinf,
          ls_filter_exclf like line of mr_filter_exclf,
          ls_filter_medcl like line of mr_filter_medcl.
    try.
        loop at mo_context->mt_par into data(ls_par) where
          par_type = mc_filter_subty_01.
          append value #( sign = if_dmf_constants_c=>gc_range_sign_inclusive
            option = if_dmf_constants_c=>gc_range_option_cp
            low = ls_par-low ) to mt_filter_subty_01.
        endloop.

        loop at mo_context->mt_par into ls_par where
          par_type = mc_exc_filter_subty_01.
          append value #( sign = if_dmf_constants_c=>gc_range_sign_exclusive
            option = if_dmf_constants_c=>gc_range_option_cp
            low = ls_par-low ) to mt_filter_subty_01.
        endloop.
* Include DPCON
        clear: ls_filter_dpcon.
        ls_filter_dpcon-low = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_filter_dpcon_low
                                                                    it_par = me->get_context( )->mt_par ).
        ls_filter_dpcon-high = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_filter_dpcon_high
                                                                    it_par = me->get_context( )->mt_par ).
        if ls_filter_dpcon-low is not initial and ls_filter_dpcon-high is not initial.
          ls_filter_dpcon-sign = if_dmf_constants_c=>gc_range_sign_inclusive.
          ls_filter_dpcon-option = if_dmf_constants_c=>gc_range_option_between.
        elseif ls_filter_dpcon-low is not initial.
          ls_filter_dpcon-sign = if_dmf_constants_c=>gc_range_sign_inclusive.
          ls_filter_dpcon-option = if_dmf_constants_c=>gc_range_option_ge.
        elseif ls_filter_dpcon-high is not initial.
          ls_filter_dpcon-sign = if_dmf_constants_c=>gc_range_sign_inclusive.
          ls_filter_dpcon-option = if_dmf_constants_c=>gc_range_option_le.
          ls_filter_dpcon-low = ls_filter_dpcon-high.
          clear: ls_filter_dpcon-high.
        endif.

        if ls_filter_dpcon is not initial.
          append ls_filter_dpcon to mr_filter_dpcon.
        endif.

* Exclude DPCON
        clear: ls_filter_dpcon.
        ls_filter_dpcon-low = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_exc_filter_dpcon_low
                                                                          it_par = me->get_context( )->mt_par ).
        ls_filter_dpcon-high = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_exc_filter_dpcon_high
                                                                    it_par = me->get_context( )->mt_par ).
        if ls_filter_dpcon-low is not initial and ls_filter_dpcon-high is not initial.
          ls_filter_dpcon-sign = if_dmf_constants_c=>gc_range_sign_exclusive.
          ls_filter_dpcon-option = if_dmf_constants_c=>gc_range_option_between.
        elseif ls_filter_dpcon-low is not initial.
          ls_filter_dpcon-sign = if_dmf_constants_c=>gc_range_sign_exclusive.
          ls_filter_dpcon-option = if_dmf_constants_c=>gc_range_option_le.
        elseif ls_filter_dpcon-high is not initial.
          ls_filter_dpcon-sign = if_dmf_constants_c=>gc_range_sign_exclusive.
          ls_filter_dpcon-option = if_dmf_constants_c=>gc_range_option_ge.
          ls_filter_dpcon-low = ls_filter_dpcon-high.
          clear: ls_filter_dpcon-high.
        endif.

        if ls_filter_dpcon is not initial.
          append ls_filter_dpcon to mr_filter_dpcon.
        endif.


        ls_filter_conam-low = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_filter_conam_low
                                                                    it_par = me->get_context( )->mt_par ).
        ls_filter_conam-high = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_filter_conam_high
                                                                    it_par = me->get_context( )->mt_par ).
        if ls_filter_conam-low is not initial and ls_filter_conam-high is not initial.
          ls_filter_conam-sign = if_dmf_constants_c=>gc_range_sign_inclusive.
          ls_filter_conam-option = if_dmf_constants_c=>gc_range_option_between.
        elseif ls_filter_conam-low is not initial.
          ls_filter_conam-sign = if_dmf_constants_c=>gc_range_sign_inclusive.
          ls_filter_conam-option = if_dmf_constants_c=>gc_range_option_ge.
        elseif ls_filter_conam-high is not initial.
          ls_filter_conam-sign = if_dmf_constants_c=>gc_range_sign_inclusive.
          ls_filter_conam-option = if_dmf_constants_c=>gc_range_option_le.
          ls_filter_conam-low = ls_filter_conam-high.
          clear: ls_filter_conam-high.
        endif.

        if ls_filter_conam is not initial.
          append ls_filter_conam to mr_filter_conam.
        endif.

        refresh mr_filter_eeinf.
        loop at mo_context->mt_par into ls_par where
          par_type = me->mc_filter_emp_influence.
          case ls_par-low.
            when me->mc_yes.
              ls_par-low = if_dmf_constants_c=>gc_yes.
            when me->mc_no.
              ls_par-low = if_dmf_constants_c=>gc_no.
          endcase.
          append value #( sign = if_dmf_constants_c=>gc_range_sign_inclusive
            option = if_dmf_constants_c=>gc_range_option_equal
            low = ls_par-low ) to mr_filter_eeinf.
        endloop.

        refresh mr_filter_exclf.
        loop at mo_context->mt_par into ls_par
          where par_type = me->mc_filter_exclusion_flag.
          case ls_par-low.
            when me->mc_yes.
              ls_par-low = if_dmf_constants_c=>gc_yes.
            when me->mc_no.
              ls_par-low = if_dmf_constants_c=>gc_no.
          endcase.
          append value #( sign = if_dmf_constants_c=>gc_range_sign_inclusive
            option = if_dmf_constants_c=>gc_range_option_equal
            low = ls_par-low ) to mr_filter_exclf.
        endloop.

        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_defined_benefits
          changing
            ct_parameter_tab = mr_filter_medcl.

* Read Pay Structure Parameters
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_trfar
            iv_exc_par_type  = me->mc_exc_filter_trfar
          changing
            ct_parameter_tab = mr_filter_trfar.

        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_trfgb
            iv_exc_par_type  = me->mc_exc_filter_trfgb
          changing
            ct_parameter_tab = mr_filter_trfgb.

        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_trfar_trfgb
          changing
            ct_parameter_tab = mr_filter_trfar_trfgb.

* Read Functional Area
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_fkber
            iv_exc_par_type  = me->mc_exc_fkber
          changing
            ct_parameter_tab = mt_fkber.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.
  endmethod.
ENDCLASS.
