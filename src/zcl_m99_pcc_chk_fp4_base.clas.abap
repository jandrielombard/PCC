class zcl_m99_pcc_chk_fp4_base definition
  public
  inheriting from cl_pyd_chk_fp4_base
  abstract
  create public .

  public section.

    interfaces if_serializable_object .

    constants mc_abap_yes type boolean value 'Y' ##NO_TEXT.
    constants mc_abap_no type boolean value 'N' ##NO_TEXT.
    constants mc_no_payroll_result type char30 value 'SBP_PCC_NO_PAYROLL_RESULT' ##NO_TEXT.
    constants mc_tech_error type char30 value 'SBP_PCC_TECHNICAL_ERROR' ##NO_TEXT.

    methods handle_init_buffers
        for event init_buffers of if_pyd_transaction .
    methods return_info
      importing
        !p_task type clike .
    class-methods dynamic_check_operation
      importing
        !iv_opcode       type zhrau_de_checkop
        !iv_var01        type any
        !iv_var02        type any
      returning
        value(rv_result) type boolean .

    methods if_pyd_ty_rt~execute
        redefinition .
    methods if_pyd_ty_rt~result_details_get
        redefinition .
protected section.

  types:
    begin of ty_pernr,
      dct_pernr type pernr_d.
  types: end of ty_pernr .
  types:
    tty_pernr type table of ty_pernr .
  types:
    begin of ty_objdata,
      tasknm    type char32,
      objectxml type zhrpy_de_pcc_objdata,
      status    type zhrpyde_pcc_taskstatus,
      msg       type string.
  types: end of ty_objdata .
  types:
    tty_objdata type table of ty_objdata .
  types:
    begin of ty_s_period_info,
      begda type datum,
      endda type datum,
    end of ty_s_period_info .
  types:
    begin of ty_s_rt_with_text,
      groupid    type pyd_cont_groupid,
      group_name type pyd_name,
      pernr      type p_pernr,
      lgart      type lgart,
      abnum      type p13_abnum,
      brnum      type p13_brnum,
      wflag      type wflag,
      betrg      type maxbt,
      anzhl      type anzhl,
      betpe      type betpe,
      lgtxt      type lgtxt,
      fpper      type fpper,
      zeinh      type pt_zeinh,
      amt_curr   type waers,
      rte_curr   type waers,
      exclude    type p13_exclude,
    end of ty_s_rt_with_text .
  types:
    ty_t_rt_with_text type standard table of ty_s_rt_with_text .
  types:
    begin of ty_s_rt_char_betpe,
      groupid    type pyd_cont_groupid,
      group_name type pyd_name,
      pernr      type p_pernr,
      lgart      type lgart,
      abnum      type p13_abnum,
      brnum      type p13_brnum,
      wflag      type wflag,
      betrg      type maxbt,
      anzhl      type anzhl,
      betpe      type betpe_char,
      lgtxt      type lgtxt,
      fpper      type fpper,
      zeinh      type pt_zeinh,
      amt_curr   type waers,
      rte_curr   type waers,
      exclude    type p13_exclude,
    end of ty_s_rt_char_betpe .
  types:
    ty_t_rt_char_betpe type standard table of ty_s_rt_char_betpe .
  types:
    begin of ty_s_bukrs_curr,
      bukrs type bukrs,
      waers type waers,
    end of ty_s_bukrs_curr .
  types:
    ty_t_bukrs_curr  type standard table of ty_s_bukrs_curr with non-unique key bukrs .

  data mt_objdata type tty_objdata .
  data ms_objdata type ty_objdata .
  constants:
    begin of gcs_groupid,
      org_data type pyd_cont_groupid value 'ORG_DATA',
      pay_data type pyd_cont_groupid value 'PAY_DATA',
    end of gcs_groupid .
  constants mc_param_abkrs type pyd_par_type value 'ABKRS' ##NO_TEXT.
  constants mc_param_period type pyd_par_type value 'PERIOD' ##NO_TEXT.
  constants mc_param_tpy_res type pyd_par_type value 'TPY_RES' ##NO_TEXT.
  constants mc_pernr type pyd_par_type value 'PERNR' ##NO_TEXT.
  constants mc_stat2 type pyd_par_type value 'Z99_STAT2' ##NO_TEXT.
  constants mc_bukrs type pyd_par_type value 'BUKRS' ##NO_TEXT.
  constants mc_persa type pyd_par_type value 'PERSA' ##NO_TEXT.
  constants mc_btrtl type pyd_par_type value 'BTRTL' ##NO_TEXT.
  constants mc_persg type pyd_par_type value 'PERSG' ##NO_TEXT.
  constants mc_persk type pyd_par_type value 'PERSK' ##NO_TEXT.
  constants mc_kostl type pyd_par_type value 'KOSTL' ##NO_TEXT.
  constants mc_user_id type pyd_par_type value 'Z99_USER_ID' ##NO_TEXT.
  constants mc_sprps type pyd_par_type value 'Z99_SPRPS' ##NO_TEXT.
  constants mc_infty type pyd_par_type value 'Z99_INFTY' ##NO_TEXT.
  constants mc_subty type pyd_par_type value 'Z99_SUBTY' ##NO_TEXT.
  constants mc_lgart type pyd_par_type value 'LGART' ##NO_TEXT.
  constants mc_exc_stat2 type pyd_par_type value 'Z99_EXC_STAT2' ##NO_TEXT.
  constants mc_exc_bukrs type pyd_par_type value 'Z99_EXC_BUKRS' ##NO_TEXT.
  constants mc_exc_persa type pyd_par_type value 'Z99_EXC_PERSA' ##NO_TEXT.
  constants mc_exc_btrtl type pyd_par_type value 'Z99_EXC_BTRTL' ##NO_TEXT.
  constants mc_exc_persg type pyd_par_type value 'Z99_EXC_PERSG' ##NO_TEXT.
  constants mc_exc_persk type pyd_par_type value 'Z99_EXC_PERSK' ##NO_TEXT.
  constants mc_exc_kostl type pyd_par_type value 'Z99_EXC_KOSTL' ##NO_TEXT.
  constants mc_exc_user_id type pyd_par_type value 'Z99_EXC_USER_ID' ##NO_TEXT.
  constants mc_exc_sprps type pyd_par_type value 'Z99_EXC_SPRPS' ##NO_TEXT.
  constants mc_exc_infty type pyd_par_type value 'Z99_EXC_INFTY' ##NO_TEXT.
  constants mc_exc_subty type pyd_par_type value 'Z99_EXC_SUBTY' ##NO_TEXT.
  constants mc_exc_lgart type pyd_par_type value 'Z99_EXC_LGART' ##NO_TEXT.
  constants mc_hyphen type pyd_name value ' - ' ##NO_TEXT.
  constants mc_equal type pyd_name value '=' ##NO_TEXT.
  constants mc_text_reason type pyd_name value 'Reason' ##NO_TEXT.
  constants mc_high_date type datum value '99991231' ##NO_TEXT.
  constants:
    begin of mc_response_missing,
      msgid type symsgid value 'ZHRPY_PCC_MSG',
      msgno type symsgno value '091',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of mc_response_missing .
  constants:
    begin of mc_data_read_error,
      msgid type symsgid value 'ZHRPY_PCC_MSG',
      msgno type symsgno value '092',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of mc_data_read_error .
  constants:
    begin of gcs_taskstatus,
      new       type zhrpyde_pcc_taskstatus value 'NEW',
      completed type zhrpyde_pcc_taskstatus value 'COMPL',
      errored   type zhrpyde_pcc_taskstatus value 'ERROR',
    end of gcs_taskstatus.
  data mo_context type ref to if_pyd_res_context .
  data mv_payroll_area type abkrs .
  data mv_payroll_period type iperi .
  data mv_tpy_res type hrdct_is_tpy .
  data mv_begda type datum .
  data mv_endda type datum .
  data mv_endda_plus1 type datum .
  data mv_change_begda type datum .
  data mt_stat2 type /iwbep/t_cod_select_options .
  data mc_stat2_active type stat2 value '3' ##NO_TEXT.
  data mt_bukrs type /iwbep/t_cod_select_options .
  data mt_werks type /iwbep/t_cod_select_options .
  data mt_btrtl type /iwbep/t_cod_select_options .
  data mt_persg type /iwbep/t_cod_select_options .
  data mt_persk type /iwbep/t_cod_select_options .
  data mt_abkrs type /iwbep/t_cod_select_options .
  data mt_kostl type /iwbep/t_cod_select_options .
  data mt_uname type /iwbep/t_cod_select_options .
  data mt_sprps type /iwbep/t_cod_select_options .
  data mv_infty type infty .
  data mv_subty type subty .
  data mt_subty type /iwbep/t_cod_select_options .
  data mv_lgart type lgart .
  data mt_zpersgk type /iwbep/t_cod_select_options .
  data mc_zpersgk type pyd_par_type value 'YK_PERSGK' ##NO_TEXT.
  data mt_exec_parameters type if_pyd_fnd_types=>ty_t_resp .
  data mv_curr type waers .
  data mt_enriched_rt type ty_t_rt_with_text .
  data mc_molga_au type molga value '13' ##NO_TEXT.
  data mc_molga_nz type molga value '43' ##NO_TEXT.
  data mt_payroll_areas type /iwbep/t_cod_select_options .
  data mt_bukrs_curr type ty_t_bukrs_curr .
  data mt_tcurx type trty_tcurx .
  data mt_swt_lgart type /iwbep/t_cod_select_options .
  data mv_swt_exc_retro type boolean .
  data mv_rfc_jobs type i value 10 ##NO_TEXT.
  data mv_rfc_snd_jobs type i value 1 ##NO_TEXT.
  data mv_rfc_rcv_jobs type i value 1 ##NO_TEXT.
  data mv_rfc_excp_flag type boolean .
  data mv_rfc_parallel_process type xfeld .
  data mv_rfc_msg type boolean value space ##NO_TEXT.
  data mv_rfc_group type rzlli_apcl value 'PCC' ##NO_TEXT.
  data mv_rfc_implemented type boolean .
  data mv_task_num type numc4 .

  data:
    begin of mv_task_name,
      name(22) type c,
      num      type numc4.
  data:end of mv_task_name .
  data mv_debug type xfeld .
  data mv_molga type molga .

  methods get_context
    returning
      value(ro_result) type ref to if_pyd_res_context .
  methods set_context
    importing
      !io_context type ref to if_pyd_res_context .
  methods get_tpy_res
    returning
      value(rv_result) type hrdct_is_tpy .
  methods get_payroll_area
    returning
      value(rv_result) type abkrs .
  methods get_payroll_period
    returning
      value(rv_result) type iperi .
  methods get_payroll_period_info
    returning
      value(rs_result) type ty_s_period_info .
  methods get_employee_status .
  methods get_infty_and_subty .
  methods get_specifc_custmizing
    importing
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context .
  methods is_off_cycle_run
    importing
      !io_res_context  type ref to if_pyd_res_context
    returning
      value(rv_result) type boole_d .
  methods get_pernr_parameters
    importing
      !it_par_addl     type if_pyd_ty_rt=>ty_t_result_key
    returning
      value(rt_result) type /iwbep/t_cod_select_options .
  methods get_parameters
    importing
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context .
  methods get_persg_and_persk .
  methods check
        abstract
    importing
      !is_ty           type pyd_d_ty
      !is_inst         type ty_s_inst
      !it_par          type if_pyd_fnd_types=>ty_t_resp
      !io_res_context  type ref to if_pyd_res_context
      !it_par_addl     type if_pyd_ty_rt=>ty_t_result_key
      !it_pernr_so     type /iwbep/t_cod_select_options
    returning
      value(rt_result) type ty_t_result
    raising
      cx_pyd_fnd .
  methods sap_sfo_get
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode .
  methods sap_t_get
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode .
  methods sap_spo_get
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode .
  methods get_generic_sfo
    returning
      value(rt_result) type cl_pyd_rd_dto_sfo=>ty_t_rd .
  methods adjust_sfo_texts
    changing
      !rt_result type cl_pyd_rd_dto_sfo=>ty_t_rd .
  methods convert_to_date
    importing
      !iv_date         type datum
    returning
      value(rv_result) type char10 .
  methods get_bukrs .
  methods get_lgart .
  methods yk_emp_info
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode
    raising
      cx_pyd_fnd .
  methods yk_wage_type_report
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode
    exporting
      !et_rt          type ty_t_rt_with_text
    raising
      cx_pyd_fnd .
  methods yk_audit_report
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode
    raising
      cx_pyd_fnd .
  methods yk_payslip
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode
    raising
      cx_pyd_fnd .
  methods yk_payroll_log
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode
    raising
      cx_pyd_fnd .
  methods yk_cats_info
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode
    raising
      cx_pyd_fnd .
  methods filter_result_on_emp_status
    changing
      !ct_result type ty_t_result .
  methods get_payroll_areas .
  methods get_currency
    importing
      !iv_molga      type molga
      !iv_endda      type datum
    returning
      value(rv_curr) type waers .
  methods get_currency_by_pernr
    importing
      !iv_pernr      type p_pernr
      !iv_endda      type datum
    returning
      value(rv_curr) type waers .
  methods convert_maxbt
    importing
      !iv_currency type tcurc-waers
      !iv_amount   type maxbt
    exporting
      !ev_maxbt    type maxbt
    raising
      cx_sql_exception .
  methods get_standard_parameters .
  methods read_range_parameter
    importing
      !iv_inc_par_type  type pyd_par_type optional
      !iv_exc_par_type  type pyd_par_type optional
    changing
      !ct_parameter_tab type /iwbep/t_cod_select_options .
  methods set_so_exc_fixed_value
    importing
      !iv_value    type any
    returning
      value(rt_so) type /iwbep/t_cod_select_options .
  methods read_all_relevant_employees
    exporting
      !rt_pernr_range type hr99s_pernr_range .
  methods read_emp_orginfo
    importing
      !iv_pernr      type pernr_d
    exporting
      !ev_persa      type werks
      !ev_persa_text type pbtxt
      !ev_btrtl      type btrtl
      !ev_btrtl_text type btrtx
    raising
      cx_pyd_fnd .
  methods add_record_to_sfo_tab
    importing
      !iv_itemid                   type cl_pyd_rd_dto_sfo=>ty_s_rd-itemid
      !iv_text                     type cl_pyd_rd_dto_sfo=>ty_s_rd-text
      !iv_value                    type string
      !iv_text_for_1st_record_only type abap_bool default abap_true
    changing
      !ct_sfo_tab                  type cl_pyd_rd_dto_sfo=>ty_t_rd .
  methods copy_structure_to_other
    importing
      !p_struct1 type any
    changing
      !p_struct2 type any .
  methods sap_swt
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode
    exporting
      !et_rt          type ty_t_rt_with_text
    raising
      cx_pyd_fnd .
  methods multithread_process
    importing
      !iv_clsname type seoclsname
      !it_pernr   type tty_pernr .
  methods submit_isolated_task
    importing
      !iv_guid_22           type guid_22
      !iv_clsname           type seoclsname
      !it_pernr_range       type hr99s_pernr_range
    changing
      !iv_serialized_object type zhrpy_de_pcc_objdata .
  methods convert_objdata
    importing
      !is_objdata       type ty_objdata
    changing
      !io_cloned_object type any
    raising
      cx_pyd_fnd .
  methods write_result_for_check_test
    importing
      !it_result type ty_t_result .
  methods get_rca_tpy_payslip
    importing
      !iv_pernr        type persno
    returning
      value(rt_stream) type cl_pyc_rd_dto_stream=>ty_t_rd .
  methods get_rca_prod_payslip
    importing
      !iv_pernr        type persno
      !iv_per          type char1
    returning
      value(rt_result) type cl_pyc_rd_dto_stream=>ty_t_rd .
  methods get_rca_fallback_form
    importing
      !iv_form         type fpname
    returning
      value(rt_result) type cl_pyc_rd_dto_stream=>ty_t_rd .
  methods z99_payslip
    importing
      !is_rd          type if_pyd_fnd_types=>ty_s_rd
      !it_par         type if_pyd_fnd_types=>ty_t_resp
      !io_res_context type ref to if_pyd_res_context
      !iv_access_mode type pyd_rdt_data_access_mode
    raising
      cx_pyd_fnd .
  methods check_avail_server .

  methods err_kv_get_list
      redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PCC_CHK_FP4_BASE IMPLEMENTATION.


  method add_record_to_sfo_tab.
    constants: lc_max_length type i value 60.
    data: ls_sfo_tab    like line of ct_sfo_tab,
          lv_row_id     like ls_sfo_tab-row_id,
          lt_components type standard table of swastrtab.
    if ct_sfo_tab is not initial.
      "Get maximum row id if there are existing records already
      lv_row_id = reduce #( init max_row_id = 0
                            for <m> in ct_sfo_tab
                            next max_row_id = cond #( when <m>-row_id > max_row_id then <m>-row_id else max_row_id ) ).
      add 1 to lv_row_id.
      ls_sfo_tab-row_id = lv_row_id.
      ls_sfo_tab-itemid = iv_itemid.
      ls_sfo_tab-text   = space.
      ls_sfo_tab-value  = space.
      insert ls_sfo_tab into table ct_sfo_tab.
    endif.

    if strlen( iv_value ) > lc_max_length.
      call function 'SWA_STRING_SPLIT'
        exporting
          input_string                 = iv_value
          max_component_length         = lc_max_length
          terminating_separators       = space
        tables
          string_components            = lt_components
        exceptions
          max_component_length_invalid = 1
          others                       = 2.
      if sy-subrc <> 0.
        add 1 to lv_row_id.
        ls_sfo_tab-row_id = lv_row_id.
        ls_sfo_tab-itemid = iv_itemid.
        ls_sfo_tab-text   = iv_text.
        ls_sfo_tab-value  = iv_value(lc_max_length).
        insert ls_sfo_tab into table ct_sfo_tab.
      else.
        loop at lt_components into data(ls_components).
          clear: ls_sfo_tab.
          if iv_text_for_1st_record_only = abap_true and sy-tabix = 1.
            ls_sfo_tab-text   = iv_text.
          endif.
          add 1 to lv_row_id.
          ls_sfo_tab-row_id = lv_row_id.
          ls_sfo_tab-itemid = iv_itemid.
          ls_sfo_tab-value  = ls_components-str.
          insert ls_sfo_tab into table ct_sfo_tab.
        endloop.
      endif.

    else.
      add 1 to lv_row_id.
      ls_sfo_tab-row_id = lv_row_id.
      ls_sfo_tab-itemid = iv_itemid.
      ls_sfo_tab-text   = iv_text.
      ls_sfo_tab-value  = iv_value.
      insert ls_sfo_tab into table ct_sfo_tab.
    endif.
  endmethod.


  method adjust_sfo_texts.
*   To be Redifined.

  endmethod.


  method check_avail_server.
* Check max available dialog processes and then
* Set the number of dialog processors for the parallel processing.
    data: lv_group_name     type rzllitab-classname,
          lv_group_type_pbt type rzllitab-grouptype value 'S',
          lt_servers        type table of rzlliapsrv,
          lv_lines          type i.

* Check server availibility
    lv_group_name = mv_rfc_group.
    call function 'SMLG_GET_DEFINED_SERVERS'
      exporting
        grouptype          = lv_group_type_pbt
        groupname          = lv_group_name
      tables
        instances          = lt_servers[]
      exceptions
        invalid_group_type = 2
        no_instances_found = 3
        others             = 99.

    if sy-subrc eq 0.
      describe table lt_servers lines lv_lines.
      if lv_lines eq 1.
        move 2 to mv_rfc_jobs.
      endif.
    endif.

  endmethod.


  method convert_maxbt.
    data: lv_shift type i,
          ls_tcurx type tcurx.
    clear: lv_shift.


    read table mt_tcurx into ls_tcurx
               with key currkey = iv_currency.
    case sy-subrc.
      when 0.
        lv_shift = ls_tcurx-currdec - 2.
      when others.
        select single * from tcurx into ls_tcurx where currkey = iv_currency.
        case sy-subrc.
          when 0.
            lv_shift = ls_tcurx-currdec - 2.
*          wa_tcurx = ls_tcurx.
            append ls_tcurx to mt_tcurx.
          when 4.
            clear: ls_tcurx.
            ls_tcurx-currkey = iv_currency.
            ls_tcurx-currdec = 2.
            append ls_tcurx to mt_tcurx.
          when others.
            raise exception type cx_sql_exception.
*   Fehler beim Lesen der Datenbanktabelle &1
        endcase.
    endcase.

    ev_maxbt = iv_amount * ( 10 ** lv_shift ).

  endmethod.


  method convert_objdata.

    data: ls_textid type scx_t100key.

    if not is_objdata-msg is initial.
      ls_textid = mc_data_read_error.
      ls_textid-attr1 = is_objdata-tasknm.
      raise exception type cx_pyd_fnd
        exporting
          textid = ls_textid.
    endif.
    if ms_objdata-objectxml is initial.
      ls_textid = mc_response_missing.
      ls_textid-attr1 = is_objdata-tasknm.
      raise exception type cx_pyd_fnd
        exporting
          textid = ls_textid.
    else.
      call transformation id_indent source xml is_objdata-objectxml result checkobject = io_cloned_object.
    endif.

  endmethod.


  method convert_to_date.
    call function 'CONVERSION_EXIT_GDATE_OUTPUT'
      exporting
        input  = iv_date
      importing
        output = rv_result.
  endmethod.


  method copy_structure_to_other.

* Copy content of a structure to another structure
    call function 'HR_99S_COPY_STRUC1_STRUC2'
      exporting
        p_struct1 = p_struct1
      importing
        p_struct2 = p_struct2.

  endmethod.


  method dynamic_check_operation.
* Perform Dynamic operation between two variables.
    rv_result = abap_false.
    case iv_opcode .
      when 'EQ'.
        check iv_var01 eq iv_var02 .
      when 'NE'.
        check iv_var01 ne iv_var02 .
      when 'LT'.
        check iv_var01 lt iv_var02.
      when 'GT'.
        check iv_var01 gt iv_var02 .
      when 'LE'.
        check iv_var01 le iv_var02.
      when 'GE'.
        check iv_var01 ge iv_var02 .
      when others.
        exit.
    endcase.
    rv_result = abap_true.

  endmethod.


  method err_kv_get_list.
* To be redefined as per Check Requirement, in case more details are needed.
* By default, this method will show the following info:
*   1) Personal Area Text and
*   2) Personal Sub Area Text

    data lv_pernr type pernr_d.
    data lv_persa type werks.
    data lv_btrtl type btrtl.
    data lv_persa_text type t500p-name1.
    data lv_btrtl_text type t001p-btext.

    me->get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
* Populate Employee Personal Area and Sub Area details
        loop at ct_err_kv into data(ls_err_kv).
          lv_pernr = ls_err_kv-id.

          try.
              call method me->read_emp_orginfo
                exporting
                  iv_pernr      = lv_pernr
                importing
                  ev_persa      = lv_persa
                  ev_persa_text = lv_persa_text
                  ev_btrtl      = lv_btrtl
                  ev_btrtl_text = lv_btrtl_text.
            catch cx_pyd_fnd .
          endtry.

          ls_err_kv-category = gcs_kv_cat-other.  "Possible values are OTH and NUM
          concatenate lv_persa '/' lv_btrtl into ls_err_kv-value separated by space.
          concatenate lv_persa_text '/' lv_btrtl_text into ls_err_kv-text separated by space.
          modify ct_err_kv from ls_err_kv transporting text category value.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method filter_result_on_emp_status.

****** Method created with note 2684325 **********************

    data ls_result              type  ty_s_result.
    data lt_pernr_so            type  /iwbep/t_cod_select_options.
    data lt_pernrs_so            type  /iwbep/t_cod_select_options.

    data lt_pernr   type standard table of pernr_d with non-unique key table_line.
    data ls_pernr like line of lt_pernr.

    check mt_stat2 is not initial.

    loop at ct_result into ls_result where par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      lt_pernr_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_result-id ).
      append lines of lt_pernr_so to lt_pernrs_so.
    endloop.


    select it00~pernr into table lt_pernr from pa0000 as it00 where it00~stat2 in mt_stat2 and it00~pernr in lt_pernrs_so.

    loop at ct_result into ls_result where par_type = if_pyd_cont_types=>gcs_par_type-pernr.
      read table lt_pernr with key table_line = ls_result-id transporting no fields. "meaning the pernr does not have the right Emp. status
      if sy-subrc ne 0.
        delete ct_result. "Remove unwanted pernrs
      endif.
    endloop.


  endmethod.


  method get_bukrs.

    data lt_bukrs_so   type /iwbep/t_cod_select_options.

    clear mt_bukrs[].
    loop at mo_context->mt_par into data(ls_par) where par_type = me->mc_bukrs.
      clear lt_bukrs_so.
      lt_bukrs_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
      append lines of lt_bukrs_so to mt_bukrs.
    endloop.

  endmethod.


  method get_context.
    ro_result = me->mo_context.
  endmethod.


  method get_currency.

    select single waers from t500w as curr
    inner join t500l as country on curr~land1 = country~intca
    into  rv_curr
    where country~molga = iv_molga
    and curr~begda le iv_endda
    and curr~endda ge iv_endda.                        "#EC CI_BUFFJOIN

  endmethod.


  method get_currency_by_pernr.
    data lv_bukrs   type bukrs.
    data ls_bukrs   type ty_s_bukrs_curr.

    select single bukrs from pa0001 into lv_bukrs
     where pernr = iv_pernr
       and begda le iv_endda
       and endda ge iv_endda.

    read table mt_bukrs_curr into ls_bukrs with key bukrs = lv_bukrs.
    if sy-subrc = 0.
      rv_curr = ls_bukrs-waers.
    else.
      select single bukrs waers from t001 into ls_bukrs
       where bukrs = lv_bukrs.
      if sy-subrc = 0.
        rv_curr = ls_bukrs-waers.
        insert ls_bukrs into table mt_bukrs_curr.
      endif.
    endif.
  endmethod.


  method get_employee_status.

    data lt_stat2_so type /iwbep/t_cod_select_options.

    loop at mo_context->mt_par into data(ls_par) where par_type = me->mc_stat2.
      clear lt_stat2_so.
      lt_stat2_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
      append lines of lt_stat2_so to mt_stat2.
    endloop.
    if sy-subrc <> 0.
      mt_stat2 = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = me->mc_stat2_active ).
    endif.

  endmethod.


  method get_generic_sfo.
    data: ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
          lv_row_id type pyd_rowid.

    lv_row_id = 0.
    add 1 to lv_row_id.
    ls_sfo-row_id   = lv_row_id.
    ls_sfo-value    = mv_payroll_area.
    ls_sfo-text     = text-003.
    append ls_sfo to rt_result.

    add 1 to lv_row_id.
    ls_sfo-row_id   = lv_row_id.
    ls_sfo-value    = mv_payroll_period.
    ls_sfo-text     = text-007.
    append ls_sfo to rt_result.

    add 1 to lv_row_id.
    ls_sfo-row_id   = lv_row_id.
    ls_sfo-value    = me->convert_to_date( mv_begda ).
    ls_sfo-text     = text-008.
    append ls_sfo to rt_result.

    add 1 to lv_row_id.
    ls_sfo-row_id   = lv_row_id.
    ls_sfo-value    = me->convert_to_date( mv_endda ).
    ls_sfo-text     = text-009.
    append ls_sfo to rt_result.

  endmethod.


  method get_infty_and_subty.
    data lt_zsubty_so    type /iwbep/t_cod_select_options.

    try .
        mv_infty =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_infty
                                                          it_par      = mo_context->mt_par ).

        mv_subty =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_subty
                                                          it_par      = mo_context->mt_par ).

      catch cx_pyd_fnd into data(lo_exception).

    endtry.

    clear mt_subty[].

    loop at mo_context->mt_par into data(ls_par) where par_type = me->mc_subty.
      clear lt_zsubty_so.
      lt_zsubty_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
      append lines of lt_zsubty_so to mt_subty.
    endloop.
    if sy-subrc <> 0.
      return.
    endif.

    sort mt_subty by low option sign.
    delete adjacent duplicates from mt_subty.


  endmethod.


  method get_lgart.

    try.
        "Get the current run mode of the process (Test/production).
        me->mv_lgart = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_lgart
                                                             it_par      = me->get_context( )->mt_par ).
      catch cx_root.
        return.
    endtry.


  endmethod.


  method get_parameters.

    if me->mo_context is initial.
      me->set_context( io_res_context ).
    endif.

*   HRDCT: Is a Payroll Test result?
    me->get_tpy_res( ).

*   Get Payroll Area value
    me->get_payroll_area( ). "note: 2789066: this call is obsolete but keep it for backward compatibility

    me->get_payroll_areas( ). "note: 2789066: fetch multiple payroll areas from the context

*   Get Period value
    me->get_payroll_period( ).
    me->get_payroll_period_info( ).

**   Get Employee Status value - If not customized STAT2 = 3 by default (ACTIVE)
*    me->get_employee_status( ).
*
*****Srini Comment: Infty,s ubty are not standard parameter types as per
**   Get Infotype
*    me->get_infty_and_subty( ).
*
**   Get PERSG and PERSK
*    me->get_persg_and_persk( ).
*
**   Get BUKRS
*    me->get_bukrs( ).
*>>> Start of WOW PCC Development
*   Get Standard Parameters
    me->get_standard_parameters( ).
*<<< End of WOW PCC Development

*   Get LGART
    me->get_lgart( ).

*   Get other customizing - To Be Redefined as per customer need
    me->get_specifc_custmizing( it_par         = it_par
                                io_res_context = io_res_context  ).

  endmethod.


  method get_payroll_area.

    if me->mv_payroll_area is not initial.
      rv_result = me->mv_payroll_area.
      return.
    endif.

    try.
        me->mv_payroll_area = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_param_abkrs
                                                                    it_par      = me->get_context( )->mt_par ).
      catch cx_root.
        "In case of error just returns empty
        return.
    endtry.

    rv_result = me->mv_payroll_area.

  endmethod.


  method get_payroll_areas.


    data    lt_abkrs_so     type /iwbep/t_cod_select_options.
    clear mt_payroll_areas.
    loop at mo_context->mt_par into data(ls_par) where par_type = me->mc_param_abkrs.
      clear lt_abkrs_so.
      lt_abkrs_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
      append lines of lt_abkrs_so to mt_payroll_areas.
    endloop.

    delete adjacent duplicates from mt_payroll_areas.


  endmethod.


  method get_payroll_period.

*    IF me->mv_payroll_period IS NOT INITIAL.
*      rv_result = me->mv_payroll_period.
*      RETURN.
*    ENDIF.
*Note 2789066: Always read from the context

    try.
        me->mv_payroll_period = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_param_period
                                                                      it_par      = me->get_context( )->mt_par ).
      catch cx_root.
        "In case of error just returns empty
        return.
    endtry.

    rv_result = me->mv_payroll_period.

  endmethod.


  method get_payroll_period_info.

    data lv_pabrj  type pabrj.
    data lv_pabrp  type pabrp.

    data(lv_period) = me->get_payroll_period( ).
    lv_pabrj = lv_period(4).
    lv_pabrp = lv_period+4(2).

    cl_hr_payroll_area=>get_instance( imp_area = me->get_payroll_area( ) )->get_period_info(
      exporting imp_pabrj = lv_pabrj
                imp_pabrp = lv_pabrp
      importing exp_begda = data(lv_begda)
                exp_endda = data(lv_endda) ).

    rs_result-begda = mv_begda = lv_begda.
    rs_result-endda = mv_endda = lv_endda.

  endmethod.


  method get_pernr_parameters.
* populate PERNR type parameters
    if it_par_addl is not initial .
      "recheck
      loop at it_par_addl into data(ls_par_addl) where par_type = me->mc_pernr .
        try .
            call method cl_pyd_fnd_aux=>append_so_fixed_value(
              exporting
                iv_value = ls_par_addl-id
              changing
                ct_so    = rt_result ).
          catch cx_pyd_fnd.
            return.
        endtry.
      endloop.
    endif.
* Additionally, populate employee numbers from ZHRAUREPY_PCC_EXECUTE
    data: gt_pernr type pernr_tab,
          gs_pernr like line of gt_pernr.
    clear mv_debug. "global flag to indicate debugging mode - rollback work
    import gt_pernr = gt_pernr from memory id 'PCC_EXECUTE'.
    if sy-subrc = 0.
      loop at gt_pernr into gs_pernr.
        try .
            call method cl_pyd_fnd_aux=>append_so_fixed_value(
              exporting
                iv_value = gs_pernr
              changing
                ct_so    = rt_result ).
          catch cx_pyd_fnd.
            return.
        endtry.
      endloop.
      if sy-subrc = 0.
        mv_debug = abap_true.
      endif.
    endif.
  endmethod.


  method get_persg_and_persk.
    data lt_zpersgk_so   type /iwbep/t_cod_select_options.

    clear mt_zpersgk[].
    loop at mo_context->mt_par into data(ls_par) where par_type = me->mc_zpersgk.
      clear lt_zpersgk_so.
      lt_zpersgk_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
      append lines of lt_zpersgk_so to mt_zpersgk.
    endloop.
    sort mt_zpersgk by low option sign.
    delete adjacent duplicates from mt_zpersgk.

  endmethod.


  method get_rca_fallback_form.

    data:
      lo_fp_api     type ref to cx_fp_api,
      lt_stream     type cl_pyc_rd_dto_stream=>ty_t_rd,
      ls_stream     type cl_pyc_rd_dto_stream=>ty_s_rd,
      ls_outputpar  type sfpoutputparams,
      lv_fmname     type rs38l_fnam,
      ls_docparams  type sfpdocparams,
      ls_formoutput type fpformoutput.

* -------------------------------------------------------------------------
* Init
* -------------------------------------------------------------------------
    try.

        call function 'FP_FUNCTION_MODULE_NAME'
          exporting
            i_name     = iv_form
          importing
            e_funcname = lv_fmname.

      catch cx_fp_api into lo_fp_api.

    endtry.

    ls_outputpar-nodialog = abap_true.
    ls_outputpar-getpdf = abap_true.
    call function 'FP_JOB_OPEN'
      changing
        ie_outputparams = ls_outputpar.

    " Set language and country.
    ls_docparams-langu = sy-langu.
    ls_docparams-country = 'AU'.

    " Call generated function module (form).
    call function lv_fmname
      exporting
        /1bcdwb/docparams  = ls_docparams
      importing
        /1bcdwb/formoutput = ls_formoutput.

    " Close print job.
    call function 'FP_JOB_CLOSE'.
    ls_stream-mime_type = 'application/pdf'.

    ls_stream-row_id = '001'.
    ls_stream-value  = ls_formoutput-pdf.
    append ls_stream to lt_stream.

* -------------------------------------------------------------------------
* Stream Export
* -------------------------------------------------------------------------
    rt_result = lt_stream.

  endmethod.


  method get_rca_prod_payslip.

    data:
      lt_p0001               type table of p0001,
      ls_p0001               type p0001,
      lv_period              type iperi,
      lv_bondt               type bondt,
      lt_stream              type cl_pyc_rd_dto_stream=>ty_t_rd,
      ls_stream              type cl_pyc_rd_dto_stream=>ty_s_rd,
      lv_pernr               type p_pernr,
      ls_pmehf               type pmehf,
      ls_a_it_filtered_rgdir type line of h99_clst_t_rgdir,
      lv_molga               type molga,
      lv_relid               type relid_pcl2,
      lt_it_filtered_rgdir   type h99_clst_t_rgdir,
      wa_rgdir               like line of lt_it_filtered_rgdir,
      hrform_name            type hrf_name,
      pdf_size               type  i,
      pdf_doc                type xstring,
      oref                   type ref to cx_uuid_error,
      lo_payslip_helper      type ref to cl_hrxss_rem_helper.

* -------------------------------------------------------------------------
* Init
* -------------------------------------------------------------------------
    lv_pernr  = iv_pernr.

    create object lo_payslip_helper
      exporting
        iv_pernr = lv_pernr.

    lv_molga = lo_payslip_helper->get_country_code( ).

    " Get the cluster id for payroll results
    select single relid into lv_relid from t500l where molga = lv_molga.

    lt_it_filtered_rgdir = lo_payslip_helper->get_filtered_rgdir( ).

*    if ms_proc_ctx-bondt is not initial.
*      if iv_per = 'C'. " Current Period
*
*        lv_bondt = ms_proc_ctx-bondt.
*
*        sort lt_it_filtered_rgdir ascending by bondt.
*
*        loop at lt_it_filtered_rgdir into ls_a_it_filtered_rgdir
*          where bondt = lv_bondt.
*
*        endloop.
*
*      else. " Display the last productive on-cycle payslip available
*
*        sort lt_it_filtered_rgdir ascending by inper.
*        loop at lt_it_filtered_rgdir into ls_a_it_filtered_rgdir.
*        endloop.
*
*      endif.
*
*    else.
*    if iv_per = 'C'. " Current Period
    lv_period = mv_payroll_period.

*    else. " Previous Period
*      loop at ms_proc_ctx-time_list into data(ls_time_list) where id = 'PAYROLL_PERIOD_CURRENT-1'.
*        lv_period = ls_time_list-faper.
*      endloop.
*
*    endif.

    sort lt_it_filtered_rgdir ascending by paydt fpbeg.
    loop at lt_it_filtered_rgdir into ls_a_it_filtered_rgdir
      where inper = lv_period.
    endloop.

*    endif.

    wa_rgdir = ls_a_it_filtered_rgdir.

* -------------------------------------------------------------------------
*  No Payroll Data --> Fallback form
* -------------------------------------------------------------------------
    if wa_rgdir is initial. " No payroll Results
      rt_result = get_rca_fallback_form( exporting iv_form   = mc_no_payroll_result ).
      return.
    endif.
* -------------------------------------------------------------------------
* Get the Form Name thanks to the PA0001
* -------------------------------------------------------------------------
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr     = lv_pernr
        infty     = '0001'
      tables
        infty_tab = lt_p0001.

    loop at lt_p0001 into  ls_p0001
      where begda le wa_rgdir-ipend
        and endda ge wa_rgdir-ipend.
    endloop.

    move-corresponding ls_p0001 to ls_pmehf.
    move-corresponding wa_rgdir to ls_pmehf.

    ls_pmehf-rclas = 'CESS'.
    ls_pmehf-molga = lv_molga.
    ls_pmehf-uname = sy-uname.

    call function 'HR_FEATURE_BACKFIELD'
      exporting
        feature                     = 'HRFOR'
        struc_content               = ls_pmehf
        kind_of_error               = space
      importing
        back                        = hrform_name
      exceptions
        dummy                       = 1
        error_operation             = 2
        no_backvalue                = 3
        feature_not_generated       = 4
        invalid_sign_in_funid       = 5
        field_in_report_tab_in_pe03 = 6
        others                      = 7.

    if sy-subrc ne 0 and hrform_name is initial.
      hrform_name = 'ZHRAUWOW_PSWTS'.
    endif.

* -------------------------------------------------------------------------
* Start Payslip
* -------------------------------------------------------------------------

    lo_payslip_helper->get_payslip(
        exporting
          is_rgdir     = wa_rgdir
          iv_form_name = hrform_name
        importing
          ev_document = pdf_doc
          ev_doc_size = pdf_size
        exceptions
          ex_payslip_creation_failed = 1
        ).

    if sy-subrc is not initial.
      rt_result = get_rca_fallback_form( exporting iv_form   = mc_tech_error ).
      return.
    endif.

* -------------------------------------------------------------------------
* Everything is fine --> Let's prepare the stream
* -------------------------------------------------------------------------
    ls_stream-mime_type = 'application/pdf'.

    ls_stream-row_id = '001'.
    ls_stream-value  = pdf_doc.
    append ls_stream to lt_stream.

* -------------------------------------------------------------------------
* Stream Export
* -------------------------------------------------------------------------
    rt_result = lt_stream.

  endmethod.


  method get_rca_tpy_payslip.
* Payslip Using Test Payroll Results
    data:
      lt_evp        type h99_clst_t_rgdir,
      form_object   type ref to object,
      lt_rgdir      type h99_clst_t_rgdir,
      lv_len1       type i,
      lv_xpayslip   type xstring,
      ls_pay_result type paybe_result,
      lt_p0001      type  table of p0001,
      ls_stream     type cl_pyc_rd_dto_stream=>ty_s_rd,
      lt_stream     type cl_pyc_rd_dto_stream=>ty_t_rd.

* References declarations
    data:
      lo_tpy_manager type ref to cl_hrdct_tpy_manager,
      lo_pay_access  type ref to cl_hr_pay_access.

****Determine Payslip form ****
    data:
      lv_form_name      type hrf_name,
      lv_date_1         type dats,
      lv_date_2         type dats,
      ls_pmehf          type pmehf,
      ls_pme74          type pme74,
      lr_payslip_helper type ref to cl_hrxss_rem_helper, lv_molga type molga,
      ls_msg            type bapiret1,
      lv_msg_type       type bapi_mtype,
      lv_variant        type bapi7004-payslip_variant,
      lv_hrpclx_buff    type hrpclx_pclx_buffer_on.

    data: gt_buffer	    type pay99_t_tbuff,
          gt_buffer_dir	type pay99_t_buffer_dir.
    data lt_db_evp     type h99_clst_t_rgdir.
    data lt_evp_new     type h99_clst_t_rgdir.

* -------------------------------------------------------------------------
* Init
* -------------------------------------------------------------------------
    lo_tpy_manager    = cl_hrdct_tpy_manager=>get_instance( ).

* -------------------------------------------------------------------------
* Get the Form Name thanks to the PA0001
* -------------------------------------------------------------------------
*    if ms_proc_ctx-bondt is not initial .
*      lv_date_1 = ms_proc_ctx-bondt.
*      lv_date_2 = ms_proc_ctx-bondt.
*    else.
    lv_date_1 = mv_endda.
    lv_date_2 = mv_endda.
*    endif.

    call function 'HR_READ_INFOTYPE'
      exporting
        pernr     = iv_pernr
        infty     = '0001'
        begda     = lv_date_1
        endda     = lv_date_2
      tables
        infty_tab = lt_p0001.

    read table lt_p0001 index 1 into data(ls_p0001).

    data(lo_payroll_area) = cl_hr_payroll_area=>get_instance( imp_area = ls_p0001-abkrs ).

* -------------------------------------------------------------------------
* Read Test Payroll results
* -------------------------------------------------------------------------
    call method lo_tpy_manager->fill_tpy_evps(
      exporting
        inpty        = ' '
        inper        = mv_payroll_period
        iperm        = lo_payroll_area->permo
        iabkrs       = value #( ( sign = 'I' option = 'EQ' low = ls_p0001-abkrs ) )
        bondt        = '00000000'
        inpid        = ' '
        person_id    = conv #( iv_pernr )
        group        = value #( all_pernrs = value #( ( pernr = iv_pernr ) ) )
      importing
        all_rgdirs   = data(lt_all_rgdirs)    " Table that contains all RGDIRs of a group
        all_evps     = data(lt_evps)          " Table that contains all RGDIRs of a group
        eval_periods = data(lt_eval_periods)
                       ).

    lv_hrpclx_buff = cl_hrpclx_buffer=>is_switched_on( ).
    if lines( lt_eval_periods ) > 0.
      loop at lt_eval_periods into data(ls_eval_periods).
        loop at ls_eval_periods-evp into data(ls_evp).

          if ls_evp-seqnr ge 90000.
            "import payroll results per period.
            lo_tpy_manager->fill_tpy_buffer(
              exporting
                iv_pernr      = iv_pernr                 " Personnel Number
                is_evp        = ls_evp                        " Cluster Directory (For Export and Import of Payroll Results)
              importing
                et_buffer     = data(lt_buffer)             " Tbuff payroll buffer
                et_buffer_dir = data(lt_buffer_dir)         " Buffer Dir payroll buffer
                ev_clusterid  = data(lv_clusterid)          " Relation ID
            ).

            append lines of lt_buffer to gt_buffer.
          endif.

          append ls_evp to lt_evp.
          append ls_evp to lt_rgdir.

        endloop.

      endloop.

      append lines of lt_buffer_dir to gt_buffer_dir.

      create object lo_pay_access.
* Add Test Payroll Results to the Buffer
      if lv_hrpclx_buff = abap_true.
        try.
            cl_hrpclx_buffer=>get_instance( )->fill_from_table(
              iv_pclx_id       = 'P2'                       " PCL2 - payroll result
              it_pclx          = conv #( lt_buffer ) ).     " Tbuff payroll buffer
          catch cx_hrpclx_authorization. " Authorization Error When Accessing PCLx Tables
            exit. "no authorization, just skip.
        endtry.
      else.
        lo_pay_access->initialize_buffer_from_tbuff(
          exporting
            buffer     = gt_buffer
            buffer_dir = gt_buffer_dir    " Buffer Dir payroll buffer
            clusterid  = lv_clusterid     " Relation ID
        ).
      endif.

* Add Results from DB to the Buffer
      lt_db_evp[] = lt_evp.
      delete lt_db_evp where seqnr lt 90000.
      call function 'HR_IMPORT_BUFFER_FROM_PCLX'
        exporting
          employee_number   = iv_pernr
          cluster_id        = lv_clusterid
        tables
          rgdir             = lt_db_evp[]
        exceptions
          no_results        = 1
          no_read_authority = 2
          others            = 3.

    endif.
* -------------------------------------------------------------------------
*  No Payroll Data --> Fallback form
* -------------------------------------------------------------------------
    if lt_rgdir is initial. " No payroll Results

      rt_stream = get_rca_fallback_form( exporting iv_form   = mc_no_payroll_result ).
      return.

    endif.

* -------------------------------------------------------------------------
*  Init HRFORMS
* -------------------------------------------------------------------------
    move-corresponding ls_p0001 to ls_pmehf.
    loop at lt_rgdir into data(ls_rgdir).

    endloop.
    move-corresponding ls_rgdir to ls_pmehf.
    create object lr_payslip_helper
      exporting
        iv_pernr = iv_pernr.

    lv_molga = lr_payslip_helper->get_country_code( ).
    ls_pmehf-rclas = 'CESS'.
    ls_pmehf-molga = lv_molga.
    ls_pmehf-uname = sy-uname.

    call function 'HR_FEATURE_BACKFIELD'
      exporting
        feature                     = 'HRFOR'
        struc_content               = ls_pmehf
        kind_of_error               = space
      importing
        back                        = lv_form_name
*      CHANGING
*       STATUS                      =
      exceptions
        dummy                       = 1
        error_operation             = 2
        no_backvalue                = 3
        feature_not_generated       = 4
        invalid_sign_in_funid       = 5
        field_in_report_tab_in_pe03 = 6
        others                      = 7.

    if sy-subrc ne 0 and lv_form_name is initial.
      lv_form_name = 'ZHRAUWOW_PSWTS'.
    endif.

    if lv_form_name eq '$CEDT$'.
      " Use classic version of payslip program (RPCEDTx0)
      call function 'BAPI_GET_PAYSLIP_PDF'
        exporting
          employeenumber = iv_pernr
          sequencenumber = ls_rgdir-seqnr
          payslipvariant = space
          rgdirline      = ls_rgdir             "KVHN2332117
        importing
          return         = ls_msg
          payslip        = lv_xpayslip
          pdf_fsize      = lv_len1.

      if ls_msg-type is not initial.
        rt_stream = get_rca_fallback_form( exporting iv_form   = mc_tech_error ).
        return.

      endif..

    else.
* -------------------------------------------------------------------------
*  Call HRFORMS
* -------------------------------------------------------------------------
      move-corresponding ls_p0001 to ls_pme74.
      move-corresponding ls_rgdir to ls_pme74.
      ls_pme74-molga = lv_molga.

      call function 'HR_FEATURE_BACKFIELD'
        exporting
          feature       = 'EDTIN'
          struc_content = ls_pme74
        importing
          back          = lv_variant
        exceptions
          others        = 1.

      call function 'HRFORMS_CALL_INIT'
        exporting
          hrform_name = lv_form_name "hrform_name
          begin_date  = lv_date_1
          end_date    = lv_date_2
        importing
          form_object = form_object
        exceptions
          form_error  = 1
          others      = 2.

      lt_evp_new[] = lt_evp[].
      delete lt_evp_new[] where inper <> ls_rgdir-inper.
      call function 'HRFORMS_CALL_PERNR'
        exporting
          form_object        = form_object
          pernr              = iv_pernr
          result_list        = lt_evp_new
          call_evp           = 'X'
          molga              = lv_molga
          rgdir              = lt_rgdir
          buffer             = gt_buffer
          buffer_dir         = gt_buffer_dir
        exceptions
          form_error         = 1
          payroll_read_error = 2
          reject             = 3
          no_form            = 4
          others             = 5.

      " Get form as pdf stream
      call function 'HRFORMS_CALL_PDF'
        exporting
          form_object = form_object
        importing
          pdf_size    = lv_len1
          pdf_xstring = lv_xpayslip
        exceptions
          form_error  = 1
          others      = 2.

      if sy-subrc <> 0.
        rt_stream = get_rca_fallback_form( exporting iv_form   = mc_tech_error ).
        return.

      endif.
    endif.

* -------------------------------------------------------------------------
* Everything is fine --> Let's prepare the stream
* -------------------------------------------------------------------------
    ls_stream-value  = lv_xpayslip.
    ls_stream-mime_type = 'application/pdf'.
    ls_stream-row_id = '001'.
    append ls_stream to lt_stream.

* -------------------------------------------------------------------------
* Stream Export
* -------------------------------------------------------------------------
    rt_stream = lt_stream.

  endmethod.


  method get_specifc_custmizing.
*   TO BE REDEFINED IN CASE OF SPECIFIC CUSTOMIZING OTHER THAN:
*    - PY Area (ABKRS)
*    - PY Period
*    - TPY_RES
*    - Employee Status (STAT2)
*    - Infotype (INFTY)
*    - Subtype (SUBTY)

  endmethod.


  method get_standard_parameters.
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |27-FEB-2023 |1130848  |NZ Change Period Enhancments |CFAK902930   *
*-----------------------------------------------------------------------*
*002 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
* Read Standard Validation Parameters
    data: lv_tabix type sy-tabix,
          lv_kostl type kostl.
* * Note: tolerance at the end of the period is required
* e.g. end of period +1 day to allow for data entered in on the Monday
* for the PY period that ended on the Sunday.
    mv_endda_plus1 = mv_endda + 1.

* Date for Change Selection
    if ( mv_endda - mv_begda ) > 7.
      call function 'OIL_GET_PREV_MONTH'
        exporting
          i_date = mv_begda
        importing
          e_date = mv_change_begda.

      if mo_context->ms_inst-molga = mc_molga_nz.     "MOD001++
        mv_change_begda+6(2) = '14'.                  "MOD001++
      else.                                           "MOD001++
        mv_change_begda+6(2) = '20'.
      endif.                                          "MOD001++
    else.
      mv_change_begda = mv_begda + 1.
    endif.

* Read Employee Status selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_stat2
        iv_exc_par_type  = me->mc_exc_stat2
      changing
        ct_parameter_tab = mt_stat2.

    if mt_stat2 is initial.
      mt_stat2 = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = me->mc_stat2_active ).
    endif.

* Read Company Code selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_bukrs
        iv_exc_par_type  = me->mc_exc_bukrs
      changing
        ct_parameter_tab = mt_bukrs.

* Read Personal Area selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_persa
        iv_exc_par_type  = me->mc_exc_persa
      changing
        ct_parameter_tab = mt_werks.

* Read Personal Sub Area Selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_btrtl
        iv_exc_par_type  = me->mc_exc_btrtl
      changing
        ct_parameter_tab = mt_btrtl.

* Read Employee Group Selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_persg
        iv_exc_par_type  = me->mc_exc_persg
      changing
        ct_parameter_tab = mt_persg.

* Read Employee Subgroup Selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_persk
        iv_exc_par_type  = me->mc_exc_persk
      changing
        ct_parameter_tab = mt_persk.

* Read Infotype / Subtype Selection
    try .
        mv_infty =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_infty
                                                          it_par      = mo_context->mt_par ).
        mv_subty =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_subty
                                                          it_par      = mo_context->mt_par ).
      catch cx_pyd_fnd into data(lo_exception).
    endtry.

    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_subty
        iv_exc_par_type  = me->mc_exc_subty
      changing
        ct_parameter_tab = mt_subty.

* Read Cost Center Selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_kostl
        iv_exc_par_type  = me->mc_exc_kostl
      changing
        ct_parameter_tab = mt_kostl.

    loop at mt_kostl into data(ls_kostl).
      lv_tabix = sy-tabix.

      if not ls_kostl-low is initial.
        clear: lv_kostl.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = ls_kostl-low
          importing
            output = lv_kostl.
        if not lv_kostl is initial.
          move lv_kostl to ls_kostl-low.
        endif.
      endif.

      if not ls_kostl-high is initial.
        clear: lv_kostl.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = ls_kostl-high
          importing
            output = lv_kostl.
        if not lv_kostl is initial.
          move lv_kostl to ls_kostl-high.
        endif.
      endif.

      modify mt_kostl index lv_tabix from ls_kostl transporting low high.

    endloop.

* Read User Name Selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_user_id
        iv_exc_par_type  = me->mc_exc_user_id
      changing
        ct_parameter_tab = mt_uname.

* Read Lock indicator Selection
    call method me->read_range_parameter
      exporting
        iv_inc_par_type  = me->mc_sprps
        iv_exc_par_type  = me->mc_exc_sprps
      changing
        ct_parameter_tab = mt_sprps.

* Country Grouping                             "MOD002++
    mv_molga = mo_context->ms_inst-molga.      "MOD002++

  endmethod.


  method get_tpy_res.

*    IF me->mv_tpy_res IS NOT INITIAL.
*      rv_result = me->mv_tpy_res.
*    ENDIF.

    try.
        "Get the current run mode of the process (Test/production).
        me->mv_tpy_res = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_param_tpy_res
                                                               it_par      = me->get_context( )->mt_par ).
      catch cx_root.
        return.
    endtry.

    rv_result = me->mv_tpy_res.

  endmethod.


  method handle_init_buffers.

    " note 2164175, clear all buffered variables
    clear mt_abkrs.
    clear mv_lgart.
    clear mv_payroll_period.
    clear mv_begda.
    clear mv_endda.
    clear mv_tpy_res.
    clear mt_bukrs.
    clear mv_lgart.

    "Deregister
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction activation ' '.

  endmethod.


  method if_pyd_ty_rt~execute.
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903081   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data lt_sh_pernrs_so type /iwbep/t_cod_select_options.

*   Off-cycle does not support in this runtime class
    if is_off_cycle_run( io_res_context = io_res_context ) = abap_true.
      return.
    endif.

*   Get the list of PERNR's selected manually by the end-user (rule DEFINITION)
    data(lr_pernrs_so) = get_pernr_parameters( it_par_addl = it_par_addl ).

    super->execute( is_ty          = is_ty
                    is_inst        = is_inst
                    it_par         = it_par
                    io_res_context = io_res_context
                    it_par_addl    = it_par_addl ).

    get_parameters( it_par         = it_par
                    io_res_context = io_res_context ).

*>>> PTX-3763 Start of WOW Specific Code

*>>> Start of 1251/20 Jira #E2P-1794 Woolworths NZ ECP Phase 2 PCC Build 0
* Read Employees for New Zealand Payroll Areas
    if lr_pernrs_so is initial.
      call method zcl_m99_pcc_chk_utilities=>read_payroll_area_pernr
        exporting
          it_par      = io_res_context->mt_par
        changing
          ct_pernr_so = lr_pernrs_so.
    endif.
*<<< End of 1251/20 Jira #E2P-1794 Woolworths NZ ECP Phase 2 PCC Build 0

* CHECK method Call for Selected Employees
* Employee Range Table
    data: lt_pernr_range type	/iwbep/t_cod_select_options,
          ls_pernr_range like line of lt_pernr_range.
    data: lv_count type i.
    data: lt_result type ty_t_result.
    data: ls_result type ty_s_result.

* Process large number of Employee selection in batches
    if not lr_pernrs_so is initial.

      clear: lt_pernr_range, lv_count.
      loop at lr_pernrs_so into data(ls_pernr).
        move-corresponding ls_pernr to ls_pernr_range.
        append ls_pernr_range to lt_pernr_range.
        lv_count = lv_count + 1.

        if lv_count eq 2000.
          try .
              refresh lt_result.
              lt_result = me->check( is_ty          = is_ty
                                     is_inst        = is_inst
                                     it_par         = it_par
                                     io_res_context = io_res_context
                                     it_par_addl    = it_par_addl
                                     it_pernr_so    = lt_pernr_range ).
            catch cx_pyd_fnd into data(lo_mul_exception).
              raise exception type cx_pyd_fnd exporting previous = lo_mul_exception.  "MOD001++
          endtry.
          loop at lt_result into ls_result.
            read table rt_result transporting no fields
              with key par_type = ls_result-par_type id = ls_result-id.
            if sy-subrc ne 0.
              append value #( par_type = ls_result-par_type
                              id       = ls_result-id ) to rt_result.
            endif.
          endloop.

          clear: lt_pernr_range, lv_count.
        endif.

      endloop.

      if not lt_pernr_range is initial.
        try .
            refresh lt_result.
            lt_result = me->check( is_ty          = is_ty
                                   is_inst        = is_inst
                                   it_par         = it_par
                                   io_res_context = io_res_context
                                   it_par_addl    = it_par_addl
                                   it_pernr_so    = lt_pernr_range ).
          catch cx_pyd_fnd into data(lo_last_exception).
            raise exception type cx_pyd_fnd exporting previous = lo_last_exception.  "MOD001++
        endtry.
        loop at lt_result into ls_result.
          read table rt_result transporting no fields
            with key par_type = ls_result-par_type id = ls_result-id.
          if sy-subrc ne 0.
            append value #( par_type = ls_result-par_type
                            id       = ls_result-id ) to rt_result.
          endif.
        endloop.
      endif.

    else.
* Standard Check method call for all Employees
*<<< PTX-3763 End of WOW Specific Code
      try .
          rt_result = me->check( is_ty          = is_ty
                                 is_inst        = is_inst
                                 it_par         = it_par
                                 io_res_context = io_res_context
                                 it_par_addl    = it_par_addl
                                 it_pernr_so    = lr_pernrs_so ).
        catch cx_pyd_fnd into data(lo_exception).
          raise exception type cx_pyd_fnd exporting previous = lo_exception.    "MOD001++
      endtry.
*>>> PTX-3763 Start of WOW Specific Code
    endif.
*<<< PTX-3763 End of WOW Specific Code

*>>> PTX-3763 Special Enhancement for Check test Program
    "if we're debugging through PCC_EXECUTE - output and quit
    if mv_debug eq abap_true.
      rollback work.
      call method me->write_result_for_check_test
        exporting
          it_result = rt_result.
      "stop the further processing:
      raise exception type cx_pyd_fnd.

    endif.
*<<< PTX-3763 Special Enhancement for Check test program

  endmethod.


  method if_pyd_ty_rt~result_details_get.

    types:
    ty_icon(32) type c.

    data:
      lt_rt       type ty_t_rt_with_text,
      lt_list     type table of abaplist,
      lt_html     type table of w3html,
      ls_html     type w3html,
      lv_icon     type ty_icon,
      lt_icon     type standard table of ty_icon with non-unique default key,
      lv_document type string,
      lt_stream   type cl_pyc_rd_dto_stream=>ty_t_rd,
      ls_stream   type cl_pyc_rd_dto_stream=>ty_s_rd.

    "get parameters
    get_parameters( it_par         = it_par
                    io_res_context = io_res_context ).

    " In real world you no need implement so many RDTS Maybe just one is enough
    case is_rd-rd->mv_rd_type.
      when 'SAP_SFO' or 'SAP_GOV'.
        "Sample for display report result into screen
        " In this sample , we just simulate a spool first
        sap_sfo_get(
      exporting
        is_rd          = is_rd    " Result Detail Generic
        it_par         = it_par    " Result Parameter List
        io_res_context = io_res_context    " PYDS: Result Context
        iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
        ).

      when 'SAP_T'.
        "This just a sample to show how to display the text views
        sap_t_get(
       exporting
         is_rd          = is_rd    " Result Detail Generic
         it_par         = it_par    " Result Parameter List
         io_res_context = io_res_context    " PYDS: Result Context
         iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
         ).

      when 'SAP_SPO'.
        "Sample for display report result into screen
        " In this sample , we just simulate a spool first
        sap_spo_get(
        exporting
          is_rd          = is_rd    " Result Detail Generic
          it_par         = it_par    " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
          ).

      when 'SAP_PDF'.
        "Sample for display PDF file into screen
        "simulate the PDF file by spool file
*      sap_pdf_get(
*      EXPORTING
*        is_rd          = is_rd    " Result Detail Generic
*        it_par         = it_par    " Result Parameter List
*        io_res_context = io_res_context    " PYDS: Result Context
*        iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
*        ).
*>>> WOW Specific Code
* WOW Simple Wage Type Report
      when 'SAP_SWT'.
        sap_swt(
        exporting
          is_rd          = is_rd             " Result Detail Generic
          it_par         = it_par            " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
          ).
* WOW TPY/PROD Payslip Display based on the Context
      when 'Z99_PAYSLIP'.
        z99_payslip(
        exporting
          is_rd          = is_rd    " Result Detail Generic
          it_par         = it_par    " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
          ).
*<<< WOW Specific Code
      when 'YK_PAYSLIP'.
        yk_payslip(
        exporting
          is_rd          = is_rd    " Result Detail Generic
          it_par         = it_par    " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
          ).

      when 'YK_AUDIT_REPORT'.
        yk_audit_report(
        exporting
          is_rd          = is_rd    " Result Detail Generic
          it_par         = it_par    " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
          ).

      when 'YK_WAGE_TYPE_REPORT'.
        yk_wage_type_report(
        exporting
          is_rd          = is_rd    " Result Detail Generic
          it_par         = it_par    " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
          ).

      when 'YK_EMP_INFO'.
        yk_emp_info(
        exporting
          is_rd          = is_rd    " Result Detail Generic
          it_par         = it_par    " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
          ).

      when 'YK_PAYROLL_LOG'.
        yk_payroll_log(
        exporting
          is_rd          = is_rd    " Result Detail Generic
          it_par         = it_par    " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode

          ).

      when 'YK_CATS_INFO'.
        yk_cats_info(
        exporting
          is_rd          = is_rd    " Result Detail Generic
          it_par         = it_par    " Result Parameter List
          io_res_context = io_res_context    " PYDS: Result Context
          iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
          ).

      when others.

        call method super->result_details_get(
            is_rd          = is_rd
            it_par         = it_par
            io_res_context = io_res_context
            iv_access_mode = iv_access_mode ).

    endcase.


  endmethod.


  method is_off_cycle_run.
    data lv_pt_cat type pyc_proc_templ_cat.

    clear rv_result.

    if io_res_context is not initial.
      read table io_res_context->mt_par into data(ls_par)
                                        with key par_type = cl_pyc_pt_proc_templ_cat=>gc_par_type.
      if sy-subrc = 0.
        lv_pt_cat = ls_par-low.
        try .
            data(lo_pi_aux) = cl_pyc_proc_inst_aux=>get_instance( io_transaction = mo_fnd_factory->mo_transaction ).
            if lo_pi_aux->proc_inst_is_oc( iv_proc_templ_cat = lv_pt_cat ) eq abap_true.
              rv_result = abap_true.
            endif.
          catch cx_pyc_frw.
        endtry.
      endif.
    endif.
  endmethod.


  method multithread_process.
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
* Employee range table
    data: lt_pernr_range type hr99s_pernr_range,
          ls_pernr_range like line of lt_pernr_range.
    data: lv_count type i.
    data: lv_guid_22 type guid_22.

* Parallel Process using Multithread
    check not it_pernr is initial.

* Set number of dialog processes for the parallel processing
    call method me->check_avail_server.

* Serialize the object for RFC Call
    data lv_serialized_object type zhrpy_de_pcc_objdata.
    call transformation id_indent source checkobject = me result xml lv_serialized_object.
    call function 'GUID_CREATE'
      importing
        ev_guid_22 = lv_guid_22.

    clear: lt_pernr_range, lv_count.
    ls_pernr_range-sign = 'I'.
    ls_pernr_range-option = 'EQ'.

    loop at it_pernr into data(ls_pernr).
      ls_pernr_range-low = ls_pernr-dct_pernr.
      append ls_pernr_range to lt_pernr_range.
      lv_count = lv_count + 1.

      if lv_count eq 2000.
        call method me->submit_isolated_task
          exporting
            iv_guid_22           = lv_guid_22
            iv_clsname           = iv_clsname
            it_pernr_range       = lt_pernr_range
          changing
            iv_serialized_object = lv_serialized_object.
        clear: lt_pernr_range, lv_count.
      endif.

    endloop.

    if not lt_pernr_range is initial.
      call method me->submit_isolated_task
        exporting
          iv_guid_22           = lv_guid_22
          iv_clsname           = iv_clsname
          it_pernr_range       = lt_pernr_range
        changing
          iv_serialized_object = lv_serialized_object.
    endif.
* Wait until all jobs are finished
    wait for asynchronous tasks until mv_rfc_snd_jobs eq mv_rfc_rcv_jobs up to 7200 seconds.

* Report Parallel Processing Errors
    loop at mt_objdata into ms_objdata                                               "MOD001++
      where ( status eq gcs_taskstatus-errored or status eq gcs_taskstatus-new ).    "MOD001++
      message i000(zhrpy_pcc_msg) with text-022 ms_objdata-tasknm ms_objdata-msg ''. "MOD001++
    endloop.                                                                         "MOD001++

  endmethod.


  method read_all_relevant_employees.
* Standard method to retrive relevant employee list
    data: lt_it0   type hrasr00pernr_tab.
* All relevant employees
    select it00~pernr into table lt_it0 from pa0000 as it00
        inner join pa0001 as it01 on it00~pernr = it01~pernr
        where it00~begda <= mv_endda
          and it00~endda >= mv_begda
          and it00~stat2 in mt_stat2
          and it00~sprps = ' '
          and it01~begda <= mv_endda
          and it01~endda >= mv_begda
          and it01~sprps = ' '
          and it01~abkrs in mt_payroll_areas
          and it01~bukrs in mt_bukrs
          and it01~werks in mt_werks
          and it01~persg in mt_persg
          and it01~persk in mt_persk
          and it01~kostl in mt_kostl.

* Build PERNR Range Table
    data: lv_next_pernr type pernr_d.
    data: ls_pernr like line of rt_pernr_range.

    refresh: rt_pernr_range.
* create single line consecutive numbers
    clear: ls_pernr.
    ls_pernr-sign   = 'I'.
    ls_pernr-option = 'BT'.
    loop at lt_it0 into data(ls_it0).
      if ls_pernr-high is initial.
        ls_pernr-low    = ls_it0-pernr.
        ls_pernr-high   = ls_it0-pernr.
      else.
        lv_next_pernr = ls_pernr-high + 1.
        if ls_it0-pernr ne lv_next_pernr.
          append ls_pernr to rt_pernr_range.

          ls_pernr-low    = ls_it0-pernr.
          ls_pernr-high   = ls_it0-pernr.
        else.
          ls_pernr-high   = ls_it0-pernr.
        endif.
      endif.
    endloop.
* add last row
    if ls_pernr-low is not initial.
      append ls_pernr to rt_pernr_range.
    endif.

  endmethod.


  method read_emp_orginfo.
    data: lt_p0001 type standard table of p0001 with non-unique key pskey,
          ls_p0001 type                   p0001.

    "determine employee organizational data
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr     = iv_pernr
        infty     = '0001'
        begda     = mv_begda
        endda     = mv_endda
      tables
        infty_tab = lt_p0001
      exceptions
        others    = 2.
    if sy-subrc <> 0.
      clear lt_p0001.
    endif.
    sort lt_p0001 by endda descending.
    read table lt_p0001 into ls_p0001 index 1.
    if sy-subrc <> 0.
      clear ls_p0001.
    endif.

    "---------------------------------------------------------------------------------"
    "Personnel Area
    data:
      lt_persa_so type /iwbep/t_cod_select_options,
      lt_t500p    type cl_pyd_cfg=>ty_t_t500p,
      ls_t500p    type t500p.

    ev_persa = ls_p0001-werks.
    lt_persa_so = cl_pyd_fnd_aux=>set_so_fixed_value( ls_p0001-werks ).
    lt_t500p = cl_pyd_cfg=>t500p_get_list( it_persa_so = lt_persa_so ).
    read table lt_t500p into ls_t500p index 1.
    ev_persa_text = ls_t500p-name1.

    "---------------------------------------------------------------------------------"
    "Personnel Subarea
    ev_btrtl = ls_p0001-btrtl.
    select single btext into ev_btrtl_text
      from t001p where werks = ls_p0001-werks and btrtl = ls_p0001-btrtl.

  endmethod.


  method read_range_parameter.
* Read Parameter Selection
    data lt_param_so  type /iwbep/t_cod_select_options.

    clear ct_parameter_tab[].
* Include values
    if not iv_inc_par_type is initial.
      loop at mo_context->mt_par into data(ls_par) where par_type = iv_inc_par_type.
        clear lt_param_so.
        lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = ls_par-low   ).
        append lines of lt_param_so to ct_parameter_tab.
      endloop.
    endif.

* Exclude Values
    if not iv_exc_par_type is initial.
      loop at mo_context->mt_par into data(ls_exc_par) where par_type = iv_exc_par_type.
        clear lt_param_so.
        lt_param_so = me->set_so_exc_fixed_value( iv_value = ls_exc_par-low   ).
        append lines of lt_param_so to ct_parameter_tab.
      endloop.
    endif.

  endmethod.


  method return_info.
* Method to recive results from parallel processing task
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data lv_serialized_object type zhrpy_de_pcc_objdata.
    data lv_error_msg type string.
    data lv_status type zhrpyde_pcc_taskstatus.
    data: lv_index type sy-tabix.

    clear: mv_rfc_msg.
    receive results from function 'ZHRPYFG_PCC_PARALLEL_TASK'
     importing
       ev_serialized_object       = lv_serialized_object
       ev_error_msg               = lv_error_msg
    exceptions
      communication_failure = 1 message mv_rfc_msg
      system_failure        = 2 message mv_rfc_msg.

    if sy-subrc <> 0.
*      lv_error_msg = text-022.                      "MOD001--
      concatenate text-023 mv_rfc_msg                "MOD001++
             into lv_error_msg separated by space.   "MOD001++
      lv_status = gcs_taskstatus-errored.            "MOD001++
    else.                                            "MOD001++
      lv_status = gcs_taskstatus-completed.          "MOD001++
    endif.                                           "MOD001++
* Collect the result for deserialization
    loop at mt_objdata into ms_objdata where tasknm = p_task.
      lv_index = sy-tabix.

      move: lv_serialized_object to ms_objdata-objectxml,
            lv_error_msg to ms_objdata-msg,
            lv_status to ms_objdata-status.            "MOD001++
*      modify mt_objdata from ms_objdata index lv_index transporting objectxml msg. "MOD001--
      modify mt_objdata from ms_objdata                                             "MOD001++
        index lv_index transporting objectxml status msg.                           "MOD001++
    endloop.

    mv_rfc_rcv_jobs = mv_rfc_rcv_jobs + 1.
    mv_rfc_jobs = mv_rfc_jobs + 1.

  endmethod.


  method sap_sfo_get.
    data lt_sfo    type cl_pyd_rd_dto_sfo=>ty_t_rd.

    case iv_access_mode.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        if is_rd-rd->mv_rd_type = 'SAP_SFO'.
          try .
              call method is_rd-rd->get_data(
                importing
                  et_data = lt_sfo ).
              if lt_sfo is initial.
                lt_sfo = me->get_generic_sfo( ).
              endif.
            catch cx_pyd_fnd.
              lt_sfo = me->get_generic_sfo( ).
          endtry.

          " you can do text adjustment here if neccesary
          call method me->adjust_sfo_texts "To be redifined as per customer needs
            changing
              rt_result = lt_sfo.


        endif.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        if is_rd-rd->mv_rd_type = 'SAP_SFO'.
          call method get_parameters
            exporting
              it_par         = it_par
              io_res_context = io_res_context.

          try .
              call method is_rd-rd->get_data(
                importing
                  et_data = lt_sfo ).
              if lt_sfo is initial.
                lt_sfo = me->get_generic_sfo( ).
              endif.
            catch cx_pyd_fnd.
              lt_sfo = me->get_generic_sfo( ).
          endtry.

          " you can do text adjustment here if neccesary
          call method me->adjust_sfo_texts "To be redifined as per customer needs
            changing
              rt_result = lt_sfo.
        endif.
      when others.
*        RAISE EXCEPTION TYPE cx_pyd_fnd.

    endcase.

    try .
        call method is_rd-rd->set_data( lt_sfo ).
      catch cx_pyd_fnd.

    endtry.
  endmethod.


  method sap_spo_get.

*    TYPES:
*      ty_icon(32) TYPE c.
*
*    DATA:
*      lv_pernr    TYPE p_pernr,
*      lt_rt       TYPE ty_t_rt_with_text,
*      ls_rt       TYPE ty_s_rt_with_text,
*      lv_sum      TYPE maxbt,
*      lt_list     TYPE TABLE OF abaplist,
*      lt_html     TYPE TABLE OF w3html,
*      ls_html     TYPE w3html,
*      lv_icon     TYPE ty_icon,
*      lt_icon     TYPE STANDARD TABLE OF ty_icon WITH NON-UNIQUE DEFAULT KEY,
*      lv_document TYPE string,
*      lt_stream   TYPE cl_pyc_rd_dto_stream=>ty_t_rd,
*      ls_stream   TYPE cl_pyc_rd_dto_stream=>ty_s_rd.
*
*    CLEAR lt_rt .
*    lv_pernr  = is_rd-id.
*
*    wage_type_info_get(
*      EXPORTING
*        iv_pernr       = lv_pernr
*      IMPORTING
*        et_rt          = lt_rt ).
*
*    lv_sum = 0.
*    LOOP AT lt_rt INTO ls_rt WHERE lgart = mv_lgart.
*      lv_sum = ls_rt-betrg .
*    ENDLOOP.
*    "This code is just for demo , not for productive one .
*    SUBMIT pyc_chk_sample_fp4_spool
*      WITH p_betrg = lv_sum
*      WITH p_high  = mv_net
*      AND RETURN EXPORTING LIST TO MEMORY.               "#EC CI_SUBMIT
*
*    CALL FUNCTION 'LIST_FROM_MEMORY'
*      TABLES
*        listobject = lt_list
*      EXCEPTIONS
*        OTHERS     = 2.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    CALL FUNCTION 'WWW_HTML_FROM_LISTOBJECT'
*      TABLES
*        html       = lt_html
*        listobject = lt_list
*        listicons  = lt_icon.
*
*    LOOP AT lt_html INTO ls_html.
*      CONCATENATE lv_document ls_html INTO lv_document.
*    ENDLOOP.
*
*    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*      EXPORTING
*        text     = lv_document
*        mimetype = 'text/html'
*      IMPORTING
*        buffer   = ls_stream-value
*      EXCEPTIONS
*        OTHERS   = 2.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    ls_stream-mime_type = 'text/html'.
*
*    ls_stream-row_id = '001'.
*    APPEND ls_stream TO lt_stream.
*
*    CALL METHOD is_rd-rd->set_data( lt_stream ).
  endmethod.


  method sap_swt.

    types:
      begin of t_lgart,
        sign   type ddsign,
        option type ddoption,
        low    type persa,
        high   type persa,
      end of t_lgart.

    types:
      begin of ty_s_rd,
        row_id type pyd_rowid,
        fpper  type fpper,
        lgart  type lgart.
        include type pyd_s_rdswt_ext as ext.
      types:
        text   type pyd_name,
      end of ty_s_rd .

    data: lt_rt     type ty_t_rt_with_text,
          lt_rt_2   type ty_t_rt_with_text,
          ls_rt     type ty_s_rt_with_text,
          ls_swt    type cl_pyd_rd_dto_swt=>ty_s_rd,
          lt_swt    type cl_pyd_rd_dto_swt=>ty_t_rd,
          lv_row_id type pyd_rowid,
          lv_param  type string,
          lv_pernr  type p_pernr,
          lv_curr   type waers.
    data: lt_char_rt type ty_t_rt_char_betpe,
          ls_char_rt type ty_s_rt_char_betpe.

    data lty_s_rd type sorted table  of ty_s_rd with non-unique key lgart.
    data ls_output_rt like line of et_rt.
    data lv_currency type waers.
    data lv_bit type c length 1.
    data lt_t512w type table of t512w.
    data lt_lgart type range of lgart.
    data ls_lgart like line of lt_lgart.
    data lv_molga type molga.
    data mv_persa type persa.
    data row_id type i.
    data ls_par like line of mt_exec_parameters.
    data lt_sel_lgart type range of lgart.
    data ls_sel_lgart like line of lt_sel_lgart.

    field-symbols <ls_t512w> type t512w.

    lv_pernr  = is_rd-id.

    call function 'RH_PM_GET_MOLGA_FROM_PERNR'
      exporting
        pernr           = lv_pernr
        begda           = sy-datum
        endda           = sy-datum
      importing
        molga           = lv_molga
      exceptions
        nothing_found   = 1
        no_active_plvar = 2
        others          = 3.

    "get currency per company code
    call method me->get_currency_by_pernr
      exporting
        iv_pernr = lv_pernr
        iv_endda = mv_endda
      receiving
        rv_curr  = lv_curr.

*>>> WOW Specific Code
    "List of WTs for the Display
    call method get_parameters
      exporting
        it_par         = it_par
        io_res_context = io_res_context.

    if not mt_swt_lgart is initial.
      refresh: lt_sel_lgart.
      loop at mt_swt_lgart into data(ms_swt_lgart).
        move-corresponding ms_swt_lgart to ls_sel_lgart.
        collect ls_sel_lgart into lt_sel_lgart.
      endloop.
    else.
      "Work out what the list of WTs you want to show yourself
      clear ls_lgart.
      ls_lgart-low = '/101'.
      ls_lgart-high = '/101'.
      ls_lgart-option = 'BT'.
      ls_lgart-sign = 'I'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/106'.
      ls_lgart-high = '/106'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/110'.
      ls_lgart-high = '/110'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/116'.
      ls_lgart-high = '/116'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/142'.
      ls_lgart-high = '/142'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/172'.
      ls_lgart-high = '/172'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/559'.
      ls_lgart-high = '/559'.
      append ls_lgart to lt_sel_lgart.
      ls_lgart-low = '/401'.
      ls_lgart-high = '/4ZZ'.
      append ls_lgart to lt_sel_lgart.
    endif.
*<<< WOW Specific Code

    clear ls_lgart.

    if mv_tpy_res = 'X'. "Test Payroll results

      " WTs for IN period A results
*****Note 2955368 - include rte_curr field from P2RX_RT in query to correctly display the hourly rate upto 5 decimal places *******
*      SELECT hrdct_tpy_rgdir~fpper AS groupid, hrdct_tpy_rgdir~fpper AS group_name, p2rx_rt~lgart AS lgart, p2rx_rt~betpe AS betpe, p2rx_rt~anzhl AS anzhl, p2rx_rt~rte_curr AS rte_curr, t512t~lgtxt AS lgtxt,"p2rx_eval_period~fpper as fpper,
*****Note 2959827 - SUM ANZHL field to appropriately display cumulated hours in case there are splits generated in the wagetype *****
**** Note 2964127 - Incorrect Currency conversion *****
      select hrdct_tpy_rgdir~fpper as groupid, hrdct_tpy_rgdir~fpper as group_name,
             p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then anzhl * -1 else anzhl end ) as anzhl,
            p2rx_rt~amt_curr as amt_curr, p2rx_rt~rte_curr as rte_curr, t512t~lgtxt as lgtxt,"p2rx_eval_period~fpper as fpper,
            sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1 else betrg end ) as betrg
             into corresponding fields of table @lt_rt
                      from p2rx_rt inner join hrdct_tpy_rgdir
                                    on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                                   and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
                         inner join t512t
                                   on t512t~sprsl = @sy-langu
                                   and t512t~molga = @lv_molga
                                   and t512t~lgart = p2rx_rt~lgart
            where hrdct_tpy_rgdir~abkrs = @mv_payroll_area and
                  hrdct_tpy_rgdir~dct_pernr = @lv_pernr and
                  hrdct_tpy_rgdir~inper = @mv_payroll_period and
                  ( p2rx_rt~betrg <> 0 or p2rx_rt~anzhl <> 0 or p2rx_rt~betpe <> 0 ) and
                  p2rx_rt~lgart in @lt_sel_lgart
            group by hrdct_tpy_rgdir~fpper, p2rx_rt~lgart, p2rx_rt~betpe, p2rx_rt~anzhl, p2rx_rt~amt_curr, p2rx_rt~rte_curr, t512t~lgtxt . "#EC CI_BUFFJOIN

      " WTs for retro P results
*****Note 2955368 - include rte_curr field from P2RX_RT in query to correctly display the hourly rate upto 5 decimal places *******
*      SELECT hrdct_tpy_rgdir~fpper AS groupid, hrdct_tpy_rgdir~fpper AS group_name, p2rx_rt~lgart AS lgart, p2rx_rt~betpe AS betpe, p2rx_rt~anzhl AS anzhl, p2rx_rt~rte_curr AS rte_curr, t512t~lgtxt AS lgtxt,"p2rx_eval_period~fpper as fpper,
*****Note 2959827 - SUM ANZHL field to appropriately display cumulated hours in case there are splits generated in the wagetype *****
**** Note 2964127 - Incorrect Currency conversion *****
      select hrdct_tpy_rgdir~fpper as groupid, hrdct_tpy_rgdir~fpper as group_name,
             p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe,
             sum( case hrpy_rgdir~srtza when 'A' then anzhl * -1 else anzhl end ) as anzhl,
             p2rx_rt~amt_curr as amt_curr, p2rx_rt~rte_curr as rte_curr, t512t~lgtxt as lgtxt,"p2rx_eval_period~fpper as fpper,
             sum( case hrpy_rgdir~srtza when 'A' then betrg * -1 else 0 end ) as betrg
             into corresponding fields of table @lt_rt_2
                    from p2rx_rt inner join hrpy_rgdir
                                  on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
                                 and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
                       inner join hrdct_tpy_rgdir
                                  on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
                                 and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
                         inner join t512t
                                   on t512t~sprsl = @sy-langu
                                   and t512t~molga = @lv_molga
                                   and t512t~lgart = p2rx_rt~lgart
            where hrdct_tpy_rgdir~abkrs = @mv_payroll_area and
                  hrdct_tpy_rgdir~dct_pernr = @lv_pernr and
                  hrdct_tpy_rgdir~inper = @mv_payroll_period and
                  "hrpy_rgdir~fpper <> @mv_payroll_period and "in case of current period off-cycles
                  ( p2rx_rt~betrg <> 0 or p2rx_rt~anzhl <> 0 or p2rx_rt~betpe <> 0 ) and
                  p2rx_rt~lgart in @lt_sel_lgart
            group by hrdct_tpy_rgdir~fpper, p2rx_rt~lgart, p2rx_rt~betpe, p2rx_rt~anzhl, p2rx_rt~amt_curr, p2rx_rt~rte_curr, t512t~lgtxt . "#EC CI_BUFFJOIN

      append lines of lt_rt to lt_rt_2.
      sort lt_rt_2 ascending.

      refresh lt_char_rt.
      loop at lt_rt_2 into ls_rt.
        move-corresponding ls_rt to ls_char_rt.
        collect ls_char_rt into lt_char_rt.
      endloop.

    else.
*****Note 2955368 - include rte_curr field from P2RX_RT in query to correctly display the hourly rate upto 5 decimal places *******
*      SELECT hrdct_tpy_rgdir~fpper AS groupid, hrdct_tpy_rgdir~fpper AS group_name, p2rx_rt~lgart AS lgart, p2rx_rt~betpe AS betpe, p2rx_rt~anzhl AS anzhl, p2rx_rt~rte_curr AS rte_curr, t512t~lgtxt AS lgtxt,"p2rx_eval_period~fpper as fpper,
*****Note 2959827 - SUM ANZHL field to appropriately display cumulated hours in case there are splits generated in the wagetype *****
**** Note 2964127 - Incorrect Currency conversion *****
      select p2rx_eval_period~fpper as groupid, p2rx_eval_period~fpper as group_name, p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe, sum( case p2rx_eval_period~srtza when 'P' then anzhl * -1
                         else anzhl end ) as anzhl, p2rx_rt~amt_curr as amt_curr, p2rx_rt~rte_curr as rte_curr, t512t~lgtxt as lgtxt,"p2rx_eval_period~fpper as fpper,
     sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                   else betrg end ) as betrg
       into corresponding fields of table @lt_rt
                from p2rx_rt inner join p2rx_eval_period
                              on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
                   inner join t512t
                             on t512t~sprsl = @sy-langu
                             and t512t~molga = @lv_molga
                             and t512t~lgart = p2rx_rt~lgart
      where p2rx_eval_period~abkrs = @mv_payroll_area and
            p2rx_eval_period~dct_pernr = @lv_pernr and
            p2rx_eval_period~inper = @mv_payroll_period and
            ( p2rx_rt~betrg <> 0 or p2rx_rt~anzhl <> 0 or p2rx_rt~betpe <> 0 ) and
            p2rx_rt~lgart in @lt_sel_lgart
      group by p2rx_eval_period~fpper, p2rx_rt~lgart, p2rx_rt~betpe, p2rx_rt~anzhl, p2rx_rt~amt_curr, p2rx_rt~rte_curr, t512t~lgtxt . "#EC CI_BUFFJOIN

      refresh lt_char_rt.
      loop at lt_rt into ls_rt.
        move-corresponding ls_rt to ls_char_rt.
        collect ls_char_rt into lt_char_rt.
      endloop.
    endif.

    sort lt_char_rt by groupid descending lgart ascending.
* Exclude Retro Pay data
    if mv_swt_exc_retro = abap_true.
      delete lt_char_rt where groupid <> mv_payroll_period.
    endif.

    refresh et_rt.
    loop at lt_char_rt into ls_char_rt.
      move-corresponding ls_char_rt to ls_rt.
      append ls_rt to et_rt.

      ls_swt-group_name = 'FOR Period ' && '&' && ls_rt-group_name+4(2) && '/' && ls_rt-group_name(4).
      replace '&' with space into ls_swt-group_name.
      ls_swt-groupid = ls_rt-group_name.
      ls_swt-row_id = lv_row_id.
      ls_swt-itemid = ls_rt-lgart.
      ls_swt-text = ls_rt-lgtxt.
      ls_swt-amt  = ls_rt-betrg.
      ls_swt-num  = ls_rt-anzhl.
      ls_swt-rte  = ls_rt-betpe.
      if ls_rt-amt_curr is initial.
        ls_swt-amt_curr = lv_curr.
      else.
        ls_swt-amt_curr = ls_rt-amt_curr.   " Note 2964127 - Incorrect Currency conversion
      endif.
      if ls_rt-rte_curr is initial.
        ls_swt-rte_curr = lv_curr.
      else.
        ls_swt-rte_curr = ls_rt-rte_curr.   " Note 2955368 - include rte_curr field from P2RX_RT in query to correctly display the hourly rate upto 5 decimal places
      endif.
      add 1 to lv_row_id .
      append ls_swt to lt_swt.
    endloop.

    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_swt.
      catch cx_pyd_fnd .
    endtry.

*    CALL METHOD is_rd-rd->set_data( lt_swt ).

  endmethod.


  method sap_t_get.
* Populate SAP_T custom text for Validations

    data: lt_text   type table of zhrpy_pcc_vtext,
          ls_text   type zhrpy_pcc_vtext,
          lt_txt    type cl_pyd_rd_dto_t=>ty_t_rd,
          ls_txt    type cl_pyd_rd_dto_t=>ty_s_rd,
          lv_row_id type pyd_rowid,
          lv_abkrs  type abkrs,
          lv_pernr  type p_pernr.

    lv_row_id  = 0.

    "retrieve all the text lines for the current validation
    select * into table lt_text
      from zhrpy_pcc_vtext
      where validation = if_pyd_ty_rt~mv_type.

    if sy-subrc ne 0.
      "do old logic
      lv_pernr  = is_rd-id.
      lv_abkrs = mv_payroll_area.

      call method get_parameters
        exporting
          it_par         = it_par
          io_res_context = io_res_context.

      add 1 to lv_row_id .
      clear ls_txt .
      ls_txt-row_id = lv_row_id.
      ls_txt-text   = text-006 && ':' && lv_pernr.
      append ls_txt to lt_txt.

      add 1 to lv_row_id .
      clear ls_txt .
      ls_txt-row_id = lv_row_id.
      ls_txt-text   = text-003 && ':' && lv_abkrs.
      append ls_txt to lt_txt.

    else.
      "populate with custom text
      sort lt_text by seqnr ascending.

      loop at lt_text into ls_text.
        add 1 to lv_row_id .
        ls_txt-row_id = lv_row_id.
        concatenate ls_text-textline cl_abap_char_utilities=>cr_lf into ls_txt-text.
        append ls_txt to lt_txt.
      endloop.
    endif.

    "save the data
    try .
        call method is_rd-rd->set_data( lt_txt ).
      catch cx_pyd_fnd.

    endtry.
  endmethod.


  method set_context.
    me->mo_context = io_context.
  endmethod.


  method set_so_exc_fixed_value.

    data:
      ls_so type /iwbep/s_cod_select_option.

    ls_so-sign   = 'E'.
    ls_so-option = 'EQ'.
    ls_so-low    = iv_value.
    append ls_so to rt_so.

  endmethod.


  method submit_isolated_task.
* Submit Parallel Processing task
*------------------------------------------------------ --------------*
* CHANGE HISTORY                                                      *
*---------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                |Change Label *
*---------------------------------------------------------------------*
*001 |29-MAR-2023 |1130848  |Parallel Processing Error  |CFAK903066   *
*    |            |         |Reporting                  |             *
*---------------------------------------------------------------------*
    data: lv_task(32) type c.
    data: lv_message type bapi_msg.
    data: lv_status type zhrpyde_pcc_taskstatus.    "MOD001++

    do.
      clear mv_rfc_msg.
      wait until mv_rfc_jobs > 0.

      clear: lv_task.
      add 1 to mv_task_num.
      move iv_guid_22 to mv_task_name-name.
      move mv_task_num to mv_task_name-num.
      lv_task = mv_task_name.
      lv_status = gcs_taskstatus-new.                 "MOD001++

      call function 'ZHRPYFG_PCC_PARALLEL_TASK'
        starting new task lv_task
        destination in group mv_rfc_group
        calling return_info on end of task
        exporting
          io_check_objnm        = iv_clsname
          it_pernr              = it_pernr_range
          iv_delay              = '2'
          iv_serialized_object  = iv_serialized_object
        exceptions
          communication_failure = 1 message mv_rfc_msg
          system_failure        = 2 message mv_rfc_msg
          resource_failure      = 3.

      case sy-subrc.
        when 0.
          mv_rfc_snd_jobs = mv_rfc_snd_jobs + 1.
          mv_rfc_jobs = mv_rfc_jobs - 1.

*          clear: ms_objdata.                                "MOD001--
*          ms_objdata-tasknm = lv_task.                      "MOD001--
*          append ms_objdata to mt_objdata.                  "MOD001--
          exit.                  "Job processing finished
        when 1.                  "Communication failure
          case mv_rfc_excp_flag.
            when space.
              mv_rfc_excp_flag = '1'.
              "First attempt for Communication Failure handling
              wait until mv_rfc_rcv_jobs >= mv_rfc_snd_jobs
                             up to '0.5' seconds.
            when others.
              clear mv_rfc_excp_flag.  "Reset flag
              concatenate 'Communication Failure(RFC)'(001)
                          mv_rfc_msg
                     into lv_message separated by ':'.
              lv_status = gcs_taskstatus-errored.              "MOD001++
              exit.                  "Job processing finished
          endcase.
        when 2.                      "System failure
          concatenate 'System Failure(RFC)'(002) mv_rfc_msg
                 into lv_message separated by ':'.
          lv_status = gcs_taskstatus-errored.                   "MOD001++
          exit.
        when 3.                       "No resources available at present
          case mv_rfc_excp_flag.
            when space.
              mv_rfc_excp_flag = '1'.
              wait until mv_rfc_rcv_jobs >= mv_rfc_snd_jobs
                             up to '0.2' seconds.
            when '1'.
              mv_rfc_excp_flag = '2'.
              wait until mv_rfc_rcv_jobs >= mv_rfc_snd_jobs
                         up to '0.5' seconds.
            when '2'.
              mv_rfc_excp_flag = '3'.
              wait until mv_rfc_rcv_jobs >= mv_rfc_snd_jobs up to '1' seconds.
            when others.
              clear mv_rfc_excp_flag.
*              lv_message = 'Complex Application Error (RFC)'(003). "MOD001--
              concatenate 'Complex Application Error (RFC)'(003)    "MOD001++
                          mv_rfc_msg                                "MOD001++
                     into lv_message separated by ':'.              "MOD001++
              lv_status = gcs_taskstatus-errored.                   "MOD001++
              exit.
          endcase.
      endcase.
    enddo.

* Collect Task Details for Reconciliation
    if not lv_task is initial.                    "MOD001++
      clear: ms_objdata.                          "MOD001++
      ms_objdata-tasknm = lv_task.                "MOD001++
      ms_objdata-status = lv_status.              "MOD001++
      ms_objdata-msg = lv_message.                "MOD001++
      append ms_objdata to mt_objdata.            "MOD001++
    endif.                                        "MOD001++

  endmethod.


  method write_result_for_check_test.
* Write Result for Check test program
    if lines( it_result ) > 0.
      write /.
      format intensified on.
      write: 'Personel number', /.
      format intensified off.
      loop at it_result into data(ls_result).
        write: ls_result-id.
        new-line.
      endloop.
    else.
      write: 'No personnel number selected', /.
    endif.

  endmethod.


  method yk_audit_report.
    data: lt_rt             type ty_t_rt_with_text,
          ls_rt             type ty_s_rt_with_text,
          lt_txt            type cl_pyd_rd_dto_t=>ty_t_rd,
          ls_txt            type cl_pyd_rd_dto_t=>ty_s_rd,
          ls_par            type line of           if_pyd_fnd_types=>ty_t_resp,
          lv_abkrs          type                   abkrs, "Payroll Area
          lv_period(6),
          lv_row_id         type pyd_rowid,
          lv_param          type string,
          lv_name_text      type string,
          lv_pernr          type p_pernr,
          lv_size           type i,
          lo_payroll_area   type ref to cl_hr_payroll_area,
          lv_pabrj          type                   pabrj,
          lv_pabrp          type                   pabrp,
          lv_begda          type                   begda,
          lv_endda          type                   endda,
          lv_prev_period(6),
          lv_pernr_pcl4(9),
          lv_prev_pabrj     type pabrj,
          lv_prev_pabrp     type pabrp,
          lv_prev_begda     type datum,
          lv_prev_endda     type datum,
          lv_subrc          type sy-subrc,
          lv_datum          type string,
          lv_time           type string,
          ls_prev_period    type pc2paper,
          ls_t569u          type t569u,
          ls_pcl4           type pcl4.

    data: lt_pernr_tab   type range of p_pernr,
          infty_tab      type range of subty,
          doc_key_tab    type standard table of pldoc_key,
          ls_doc_key_tab like line of doc_key_tab,
          ls_pernr       like line of lt_pernr_tab.

    loop at it_par into ls_par.

      case ls_par-par_type.
        when 'ABKRS'.
          lv_abkrs = ls_par-low.
        when 'PERIOD'.
          lv_period = ls_par-low.
        when 'TPY_RES'.
          "l_is_test_payroll = ls_par-low.
      endcase.
    endloop.

    "Additional logic for Process Context
    if lv_abkrs is initial. "Payroll Area
      lv_abkrs = cl_pyd_fnd_aux=>get_resp_fixed_value(
      iv_par_type = 'ABKRS'
      it_par = io_res_context->mt_par ).
    endif.

    if lv_period is initial. "Pay Period
      lv_period = cl_pyd_fnd_aux=>get_resp_fixed_value(
      iv_par_type = 'PERIOD'
      it_par = io_res_context->mt_par ).
    endif.

    lv_pernr  = is_rd-id.

    lo_payroll_area = cl_hr_payroll_area=>get_instance( imp_area = lv_abkrs ).

    " Find date when payroll was exited from the payroll control record logs

    lv_pabrj = lv_period(4).
    lv_pabrp = lv_period+4(2).

    lo_payroll_area->get_period_info( exporting imp_pabrj = lv_pabrj
                                                imp_pabrp = lv_pabrp
                                      importing exp_begda = lv_begda
                                                exp_endda = lv_endda ).

    lv_prev_endda = lv_begda - 1.
    ls_prev_period = lo_payroll_area->get_periode_with_date( imp_date = lv_prev_endda ).
    lv_prev_pabrj = ls_prev_period-pabrj.
    lv_prev_pabrp = ls_prev_period-pabrp.
    lv_prev_period = ls_prev_period-pabrj && ls_prev_period-pabrp.
    lo_payroll_area->get_period_info( exporting imp_pabrj = lv_prev_pabrj
                                                imp_pabrp = lv_prev_pabrp
                                      importing exp_begda = lv_prev_begda
                                                exp_endda = lv_prev_endda ).

    select single * from t569u into ls_t569u where abkrs = lv_abkrs and state = '3' and vwsaz = '01' and pabrj = lv_prev_pabrj and pabrp = lv_prev_pabrp.

    constants comp_nine(20) type c value '09182736455463728190'.

    translate ls_t569u-aedat using comp_nine.
    translate ls_t569u-uzeit using comp_nine.


    "Find PCL4 records between last pay period exit date and today - need to filter on time stamp later
* 1. Fill range tables
    clear lt_pernr_tab.
    ls_pernr-sign   = 'I'.
    ls_pernr-option = 'EQ'.
    ls_pernr-low    = lv_pernr.
    append ls_pernr to lt_pernr_tab.

    call function 'HR_INFOTYPE_LOG_GET_LIST'
      exporting
        begda              = ls_t569u-aedat
        endda              = sy-datum
      importing
        subrc              = lv_subrc
      tables
        pernr_tab          = lt_pernr_tab
        infty_tab          = infty_tab
        infty_logg_key_tab = doc_key_tab.

    sort doc_key_tab by bdate btime descending infty ascending.

    loop at doc_key_tab into ls_doc_key_tab where bdate = ls_t569u-aedat and btime lt ls_t569u-uzeit.
      "remove items that were change on same day as payroll exited, but before payroll was complete
      delete doc_key_tab index sy-tabix.
    endloop.
    constants: c_tab type c value cl_abap_char_utilities=>horizontal_tab.

    describe table doc_key_tab lines lv_size.

    lv_row_id  = 0.
    if lv_size is initial.
      ls_txt-text   = 'No Master Data changes during pay period'.
      append ls_txt to lt_txt.
    else.
      ls_txt-text   = 'Infotype' && c_tab && 'Date' && c_tab && 'Time' && c_tab && 'User' .  "'This is the first line of the text.'.
      append ls_txt to lt_txt.
      loop at doc_key_tab into ls_doc_key_tab.
        add 1 to lv_row_id .
        ls_txt-row_id = lv_row_id .
        call function 'CONVERSION_EXIT_PDATE_OUTPUT'
          exporting
            input  = ls_doc_key_tab-bdate
          importing
            output = lv_datum.

        call function 'CONVERSION_EXIT_RSTIM_OUTPUT'
          exporting
            input  = ls_doc_key_tab-btime
          importing
            output = lv_time.

        select single adrp~name_text into lv_name_text
          from usr21 join adrp on usr21~persnumber = adrp~persnumber and
                                  adrp~date_from   = '00010101'      and
                                  adrp~nation      = ''
          where usr21~bname = ls_doc_key_tab-uname.

        ls_txt-text   = ls_doc_key_tab-infty && c_tab && lv_datum && c_tab && lv_time && c_tab && lv_name_text.
        append ls_txt to lt_txt.
      endloop.
    endif.

    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_txt.
      catch cx_pyd_fnd .
    endtry.

*    CALL METHOD is_rd-rd->set_data( lt_txt ).

  endmethod.


  method yk_cats_info.
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    types:
      begin of t_ty_data_result,
        pernr  type p_pernr,
        statu2 type pw_statu2,
        begda  type datum,
        endda  type datum,
        datum1 type datum,
        awart  type awart,
        stdaz  type abstd,
      end of t_ty_data_result.

    data:
      lt_gov         type                   cl_pyd_rd_dto_gov=>ty_t_rd,
      ls_gov         type                   cl_pyd_rd_dto_gov=>ty_s_rd,
      lv_pernr       type                   pernr_d,
      lv_rowid       type i,
      ls_par         type                   if_pyd_fnd_types=>ty_s_resp,
      lo_pt_abkrs    type ref to            if_pyd_par_type_rt,
      lo_pt_period   type ref to            if_pyd_par_type_rt,
      lv_abkrs       type                   abkrs,
      lv_period      type                   fpper,
      ls_par_val     type                   if_pyd_par_type_rt=>ty_s_par_val,
      lt_par_val     type                   if_pyd_par_type_rt=>ty_t_par_val,
      lv_begda       type                   begda,
      lv_endda       type                   endda,
      lt_data_result type standard table of t_ty_data_result,
      ls_data_result type t_ty_data_result.

    "temp! refactor coding into reuse class and config class later!

    "personnel number
    lv_pernr = is_rd-id.

    "get parameters
    get_parameters( it_par         = it_par
                    io_res_context = io_res_context ).

    "Find employees with CATS transfers that have not occurred from this pay period prior
    "Adjust to meet the logic and requirements for your business.  Your business may use ptex2010 as well
    select ptex2000~pernr, ptex2000~statu2, ptex2000~begda, ptex2000~endda, ptex2000~datum1, ptex2000~awart, ptex2000~stdaz
            into corresponding fields of table @lt_data_result
                     from ptex2000 inner join pa0001
                                   on pa0001~pernr eq ptex2000~pernr
           where pa0001~abkrs = @mv_payroll_area and
                 ptex2000~endda <= @mv_endda and
                 ( ptex2000~statu2 = ' ' or ptex2000~statu2 = '2' )  "Not transferred or error
           group by ptex2000~pernr, ptex2000~statu2, ptex2000~begda, ptex2000~endda, ptex2000~datum1, ptex2000~awart, ptex2000~stdaz.

    sort lt_data_result. delete adjacent duplicates from lt_data_result.

    lv_rowid = 0.

    loop at lt_data_result into ls_data_result where statu2 = '2'.
      lv_rowid = lv_rowid + 1.
      ls_gov-groupid    = 'NEW'.
*      select single atext into ls_gov-group_name from t554t where awart eq ls_data_result-awart and sprsl = sy-langu and moabw = mc_molga_au. "Change moab selection as required
      select single atext into ls_gov-group_name from t554t where awart eq ls_data_result-awart and sprsl = sy-langu and moabw = mv_molga. "Change moab selection as required
      ls_gov-row_id     = lv_rowid.
      ls_gov-text       = 'Timesheet hours'.
      ls_gov-value      = ls_data_result-stdaz.
      append ls_gov to lt_gov.
      lv_rowid = lv_rowid + 1.
      ls_gov-row_id     = lv_rowid.
      ls_gov-itemid     = 'NEW'.
      ls_gov-text       = 'Record entered on'.

      call function 'CONVERSION_EXIT_PDATE_OUTPUT'
        exporting
          input  = ls_data_result-datum1
        importing
          output = ls_gov-value.

      append ls_gov to lt_gov.
      lv_rowid = lv_rowid + 1.
      ls_gov-row_id     = lv_rowid.
      ls_gov-text       = 'Record date'.
      call function 'CONVERSION_EXIT_PDATE_OUTPUT'
        exporting
          input  = ls_data_result-begda
        importing
          output = ls_gov-value.

      append ls_gov to lt_gov.
    endloop.


    loop at lt_data_result into ls_data_result where statu2 = ' '.
      lv_rowid = lv_rowid + 1.
      ls_gov-groupid    = 'NEW'.
*      select single atext into ls_gov-group_name from t554t where awart eq ls_data_result-awart and sprsl = sy-langu and moabw = mc_molga_au. "Change moab selection as required
      select single atext into ls_gov-group_name from t554t where awart eq ls_data_result-awart and sprsl = sy-langu and moabw = mv_molga. "Change moab selection as required
      ls_gov-row_id     = lv_rowid.
      ls_gov-text       = 'Timesheet hours'.
      ls_gov-value      = ls_data_result-stdaz.
      append ls_gov to lt_gov.
      lv_rowid = lv_rowid + 1.
      ls_gov-row_id     = lv_rowid.
      ls_gov-itemid     = 'NEW'.
      ls_gov-text       = 'Record entered on'.

      call function 'CONVERSION_EXIT_PDATE_OUTPUT'
        exporting
          input  = ls_data_result-datum1
        importing
          output = ls_gov-value.

      append ls_gov to lt_gov.
      lv_rowid = lv_rowid + 1.
      ls_gov-row_id     = lv_rowid.
      ls_gov-text       = 'Record date'.
      call function 'CONVERSION_EXIT_PDATE_OUTPUT'
        exporting
          input  = ls_data_result-begda
        importing
          output = ls_gov-value.

      append ls_gov to lt_gov.
    endloop.


    "---------------------------------------------------------------------------------"
    "end processing
    sort lt_gov by row_id.

    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_gov.
      catch cx_pyd_fnd .
    endtry.

    call method is_rd-rd->set_data( lt_gov ).


  endmethod.


  method yk_emp_info.
    data:
      lt_gov        type                   cl_pyd_rd_dto_gov=>ty_t_rd,
      ls_gov        type                   cl_pyd_rd_dto_gov=>ty_s_rd,
      lv_pernr      type                   pernr_d,
      ls_par        type                   if_pyd_fnd_types=>ty_s_resp,
      lt_p0001      type standard table of p0001 with non-unique key pskey,
      ls_p0001      type                   p0001,
      lt_p0006      type standard table of p0006 with non-unique key pskey,
      ls_p0006      type                   p0006,
      lt_p0105      type standard table of p0105 with non-unique key pskey,
      ls_p0105      type                   p0105,
      lo_pt_abkrs   type ref to            if_pyd_par_type_rt,
      lo_pt_period  type ref to            if_pyd_par_type_rt,
      lv_abkrs      type                   abkrs,
      lv_period     type                   fpper,
      ls_par_val    type                   if_pyd_par_type_rt=>ty_s_par_val,
      lt_par_val    type                   if_pyd_par_type_rt=>ty_t_par_val,
      lt_abkrs_so   type                   /iwbep/t_cod_select_options,
      lt_permo_so   type                   /iwbep/t_cod_select_options,
      lt_pabrj_so   type                   /iwbep/t_cod_select_options,
      lt_pabrp_so   type                   /iwbep/t_cod_select_options,
      lt_t549a      type                   cl_pyd_cfg=>ty_t_t549a,
      ls_t549a      type                   cl_pyd_cfg=>ty_s_t549a,
      lt_t549q      type                   cl_pyd_cfg=>ty_t_t549q,
      ls_t549q      type                   cl_pyd_cfg=>ty_s_t549q,
      lv_begda      type                   begda,
      lv_endda      type                   endda,
      lv_btrtl_text type                   btrtx,
      lv_persg_text type                   pgtxt,
      lv_persk_text type                   pktxt,
      lv_kostl_text type                   ktext,
      lv_bukrs_text type                   butxt.

    "temp! refactor coding into reuse class and config class later!

    "personnel number
    lv_pernr = is_rd-id.

    "---------------------------------------------------------------------------------"
*    "determine payroll area
*    lv_abkrs = cl_pyd_fnd_aux=>get_resp_fixed_value(
*                 iv_par_type = if_pyd_cont_types=>gcs_par_type-abkrs
*                 it_par      = it_par ).
*    "Additional logic for Process Context
*    if mv_abkrs is initial. "Payroll Area
*      mv_abkrs = cl_pyd_fnd_aux=>get_resp_fixed_value(
*      iv_par_type = 'ABKRS'
*      it_par = io_res_context->mt_par ).
*    endif.
*
*    lo_pt_abkrs = mo_fnd_factory->get_par_type_rt( if_pyd_cont_types=>gcs_par_type-abkrs ).
*
*    CLEAR ls_gov.
*    CLEAR: ls_par_val, lt_par_val.
*    ls_par_val-value = lv_abkrs.
*    INSERT ls_par_val INTO TABLE lt_par_val.
*
*    lt_par_val = lo_pt_abkrs->values_enrich_name( lt_par_val ).
*
*    READ TABLE lt_par_val INTO ls_par_val INDEX 1.
*    IF sy-subrc <> 0.
*      CLEAR ls_par_val-name.
*    ENDIF.

    ls_gov-groupid    = gcs_groupid-pay_data.
    ls_gov-group_name = text-002.
    ls_gov-row_id     = '201'.
    ls_gov-itemid     = 'ABKRS'.
    ls_gov-text       = text-004.
    ls_gov-value      = mv_payroll_area.
*    REPLACE '&1' WITH lv_abkrs INTO ls_gov-value.
*    REPLACE '&2' WITH ls_par_val-name INTO ls_gov-value.
    append ls_gov to lt_gov.

*    "---------------------------------------------------------------------------------"
*    "determine period
*    lv_period = cl_pyd_fnd_aux=>get_resp_fixed_value(
*                  iv_par_type = if_pyd_cont_types=>gcs_par_type-period
*                  it_par      = it_par ).
*
*    "Additional logic for Process Context
*    if lv_period is initial. "Pay Period
*      lv_period = cl_pyd_fnd_aux=>get_resp_fixed_value(
*      iv_par_type = 'PERIOD'
*      it_par = io_res_context->mt_par ).
*    endif.
*    lo_pt_period = mo_fnd_factory->get_par_type_rt( if_pyd_cont_types=>gcs_par_type-period ).
*
*    CLEAR ls_gov.
*    CLEAR: ls_par_val, lt_par_val.
*    ls_par_val-value = lv_period.
*    INSERT ls_par_val INTO TABLE lt_par_val.
*
*    lt_par_val = lo_pt_period->values_enrich_name( lt_par_val ).
*
*    READ TABLE lt_par_val INTO ls_par_val INDEX 1.
*    IF sy-subrc <> 0.
*      CLEAR ls_par_val-name.
*    ENDIF.

    ls_gov-groupid    = gcs_groupid-pay_data.
    ls_gov-group_name = text-002.
    ls_gov-row_id     = '202'.
    ls_gov-itemid     = 'PERIOD'.
    ls_gov-text       = text-005.
    ls_gov-value      = mv_payroll_period.
    append ls_gov to lt_gov.

    "---------------------------------------------------------------------------------"
    "determine period dates
    clear ls_gov.
    ls_gov-groupid    = gcs_groupid-pay_data.
    ls_gov-group_name = text-002.

    lt_abkrs_so = cl_pyd_fnd_aux=>set_so_fixed_value( mv_payroll_area ).
    lt_t549a = cl_pyd_cfg=>t549a_get_list( it_abkrs_so = lt_abkrs_so ).
    read table lt_t549a into ls_t549a index 1.
    if sy-subrc = 0 and mv_payroll_period is not initial.
      lt_permo_so = cl_pyd_fnd_aux=>set_so_fixed_value( ls_t549a-permo ).
      lt_pabrj_so = cl_pyd_fnd_aux=>set_so_fixed_value( mv_payroll_period(4) ).
      lt_pabrp_so = cl_pyd_fnd_aux=>set_so_fixed_value( mv_payroll_period+4(2) ).
      lt_t549q = cl_pyd_cfg=>t549q_get_list(
                   it_permo_so = lt_permo_so
                   it_pabrj_so = lt_pabrj_so
                   it_pabrp_so = lt_pabrp_so ).
      read table lt_t549q into ls_t549q index 1.
      if sy-subrc = 0.
        lv_begda = ls_t549q-begda.
        lv_endda = ls_t549q-endda.
      endif.
    endif.

    ls_gov-row_id     = '203'.
    ls_gov-itemid     = 'BEGDA'.
    ls_gov-text       = text-010.
    ls_gov-datum      = lv_begda.
    append ls_gov to lt_gov.

    ls_gov-row_id     = '204'.
    ls_gov-itemid     = 'ENDDA'.
    ls_gov-text       = text-011.
    ls_gov-datum      = lv_endda.
    append ls_gov to lt_gov.

    "---------------------------------------------------------------------------------"
    "determine employee organizational data

    "call with authorization check on
    "if nothing returned -> just leave corresponding fields empty
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr     = lv_pernr
        infty     = '0001'
        begda     = lv_begda
        endda     = lv_endda
      tables
        infty_tab = lt_p0001
      exceptions
        others    = 2.
    if sy-subrc <> 0.
      clear lt_p0001.
    endif.

    sort lt_p0001 by endda descending.
    read table lt_p0001 into ls_p0001 index 1.
    if sy-subrc <> 0.
      clear ls_p0001.
    endif.

    clear ls_gov.

    ls_gov-groupid = gcs_groupid-org_data.
    ls_gov-group_name = text-001.

    "---------------------------------------------------------------------------------"
    "Employee ID (PERNR) and name
    ls_gov-row_id = '101'.
    ls_gov-itemid = 'PERNR'.
    ls_gov-text   = text-012.
    ls_gov-value  = text-013.
    replace '&1' with lv_pernr into ls_gov-value.
    replace '&2' with ls_p0001-ename into ls_gov-value.
    append ls_gov to lt_gov.

    "---------------------------------------------------------------------------------"
    "Personnel Area
    data:
      lt_persa_so type /iwbep/t_cod_select_options,
      lt_t500p    type cl_pyd_cfg=>ty_t_t500p,
      ls_t500p    type t500p.

    ls_gov-row_id = '102'.
    ls_gov-itemid = 'WERKS'.
    ls_gov-text   = text-014.
    ls_gov-value  = text-013.
    replace '&1' with ls_p0001-werks into ls_gov-value.
    lt_persa_so = cl_pyd_fnd_aux=>set_so_fixed_value( ls_p0001-werks ).
    lt_t500p = cl_pyd_cfg=>t500p_get_list( it_persa_so = lt_persa_so ).
    read table lt_t500p into ls_t500p index 1.
    "if not found, text remains initial
    replace '&2' with ls_t500p-name1 into ls_gov-value.
    append ls_gov to lt_gov.

    "---------------------------------------------------------------------------------"
    "Personnel Subarea
    ls_gov-row_id = '103'.
    ls_gov-itemid = 'BTRTL'.
    ls_gov-text   = text-015.
    ls_gov-value  = text-013.
    replace '&1' with ls_p0001-btrtl into ls_gov-value.
    select single btext into lv_btrtl_text
      from t001p where werks = ls_p0001-werks and btrtl = ls_p0001-btrtl.
    "if not found, lv_btrtl_text remains initial
    replace '&2' with lv_btrtl_text into ls_gov-value.
    append ls_gov to lt_gov.

    "---------------------------------------------------------------------------------"
    "Employee Group
    ls_gov-row_id = '104'.
    ls_gov-itemid = 'PERSG'.
    ls_gov-text   = text-016.
    ls_gov-value  = text-013.
    replace '&1' with ls_p0001-persg into ls_gov-value.
    select single ptext into lv_persg_text
      from t501t where sprsl = sy-langu and persg = ls_p0001-persg.
    "if not found, lv_persg_text remains initial
    replace '&2' with lv_persg_text into ls_gov-value.
    append ls_gov to lt_gov.

    "---------------------------------------------------------------------------------"
    "Employee Subgroup
    ls_gov-row_id = '105'.
    ls_gov-itemid = 'PERSK'.
    ls_gov-text   = text-017.
    ls_gov-value  = text-013.
    replace '&1' with ls_p0001-persk into ls_gov-value.
    select single ptext into lv_persk_text
      from t503t where sprsl = sy-langu and persk = ls_p0001-persk.
    "if not found, lv_persk_text remains initial
    replace '&2' with lv_persk_text into ls_gov-value.
    append ls_gov to lt_gov.

    "---------------------------------------------------------------------------------"
    "Company Code
    ls_gov-row_id = '106'.
    ls_gov-itemid = 'BUKRS'.
    ls_gov-text   = text-018.
    ls_gov-value  = text-013.
    replace '&1' with ls_p0001-bukrs into ls_gov-value.

    call function 'HRCA_COMPANYCODE_GETDETAIL'
      exporting
        companycode = ls_p0001-bukrs
        language    = sy-langu
      importing
        comp_name   = lv_bukrs_text
      exceptions
        others      = 2.
    if sy-subrc <> 0.
      "lv_bukrs_text remains empty
    endif.

    replace '&2' with lv_bukrs_text into ls_gov-value.
    append ls_gov to lt_gov.

    "---------------------------------------------------------------------------------"
    "Cost Center
    if not ls_p0001-kostl is initial.

      ls_gov-row_id = '107'.
      ls_gov-itemid = 'KOSTL'.
      ls_gov-text   = text-019.
      ls_gov-value  = text-013.
      replace '&1' with ls_p0001-kostl into ls_gov-value.

      "if KOKRS not filled, it can be determined using function model
      "HRCA_CONTROLLINGAREA_FIND. Not done yet.
      call function 'HR_READ_FOREIGN_OBJECT_TEXT'
        exporting
          otype           = 'K'
*         OBJID           =
          costcenter      = ls_p0001-kostl
          controllingarea = ls_p0001-kokrs
*         STATUS          = '1'
          begda           = ls_p0001-begda
          endda           = ls_p0001-endda
          langu           = sy-langu
        importing
          costcenter_name = lv_kostl_text
        exceptions
          others          = 5.
      if sy-subrc <> 0.
        clear lv_kostl_text.
      endif.

      replace '&2' with lv_kostl_text into ls_gov-value.
      append ls_gov to lt_gov.

    endif.

    "determine employee personal data

    "call with authorization check on
    "if nothing returned -> just leave corresponding fields empty
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr     = lv_pernr
        infty     = '0006'
        begda     = lv_begda
        endda     = lv_endda
      tables
        infty_tab = lt_p0006
      exceptions
        others    = 2.
    if sy-subrc <> 0.
      clear lt_p0001.
    endif.

    sort lt_p0006 by endda descending.
    loop at lt_p0006 into ls_p0006 where subty = '1'.
    endloop.
    if sy-subrc <> 0.
      clear ls_p0006.
    endif.

    "---------------------------------------------------------------------------------"
    "Telephone Number - update to the relevant field for your country
    ls_gov-row_id = '108'.
    ls_gov-itemid = 'TELNO'.
    ls_gov-text   = text-020.
    ls_gov-value  = ls_p0006-num01.

    append ls_gov to lt_gov.

    "Email
    "call with authorization check on
    "if nothing returned -> just leave corresponding fields empty
    call function 'HR_READ_INFOTYPE'
      exporting
        pernr     = lv_pernr
        infty     = '0105'
        begda     = lv_begda
        endda     = lv_endda
      tables
        infty_tab = lt_p0105
      exceptions
        others    = 2.
    if sy-subrc <> 0.
      clear lt_p0001.
    endif.

    sort lt_p0105 by endda descending.
    loop at lt_p0105 into ls_p0105 where subty = '0010'.
    endloop.
    if sy-subrc <> 0.
      clear ls_p0105.
    endif.

    "---------------------------------------------------------------------------------"
    "Telephone Number
    ls_gov-row_id = '109'.
    ls_gov-itemid = 'EMAIL'.
    ls_gov-text   = text-021.
    ls_gov-value  = ls_p0105-usrid_long.

    append ls_gov to lt_gov.



    "---------------------------------------------------------------------------------"
    "end processing
    sort lt_gov by row_id.
    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_gov.
      catch cx_pyd_fnd .
    endtry.

*    CALL METHOD is_rd-rd->set_data( lt_gov ).


  endmethod.


  method yk_payroll_log.
    types: begin of t_payroll_msg,
             iaper   type iperi,
             seqnr   type seu_level,
             as_text type char40,
             text1   type plog_txt,
           end of t_payroll_msg.

    data:
      lt_gov             type                   cl_pyd_rd_dto_gov=>ty_t_rd,
      ls_gov             type                   cl_pyd_rd_dto_gov=>ty_s_rd,
      lv_pernr           type                   pernr_d,
      ls_par             type                   if_pyd_fnd_types=>ty_s_resp,
      lt_p0001           type standard table of p0001 with non-unique key pskey,
      ls_p0001           type                   p0001,
      lt_p0006           type standard table of p0006 with non-unique key pskey,
      ls_p0006           type                   p0006,
      lt_p0105           type standard table of p0105 with non-unique key pskey,
      ls_p0105           type                   p0105,
      lo_pt_abkrs        type ref to            if_pyd_par_type_rt,
      lo_pt_period       type ref to            if_pyd_par_type_rt,
      lv_abkrs           type                   abkrs,
      lv_period          type                   fpper,
      ls_par_val         type                   if_pyd_par_type_rt=>ty_s_par_val,
      lt_par_val         type                   if_pyd_par_type_rt=>ty_t_par_val,
      lt_abkrs_so        type                   /iwbep/t_cod_select_options,
      lt_permo_so        type                   /iwbep/t_cod_select_options,
      lt_pabrj_so        type                   /iwbep/t_cod_select_options,
      lt_pabrp_so        type                   /iwbep/t_cod_select_options,
      lt_t549a           type                   cl_pyd_cfg=>ty_t_t549a,
      ls_t549a           type                   cl_pyd_cfg=>ty_s_t549a,
      lt_t549q           type                   cl_pyd_cfg=>ty_t_t549q,
      ls_t549q           type                   cl_pyd_cfg=>ty_s_t549q,
      lv_begda           type                   begda,
      lv_endda           type                   endda,
      lv_btrtl_text      type                   btrtx,
      lv_persg_text      type                   pgtxt,
      lv_persk_text      type                   pktxt,
      lv_kostl_text      type                   ktext,
      lv_bukrs_text      type                   butxt,
      lv_is_test_payroll type c.

    data: lt_pyc_d_py_msg type table of t_payroll_msg,
          ls_payroll_log  type t_payroll_msg,
          lt_lines        type table of text60,    "Note 2707759
          lv_seqnr        type pyd_rowid.          "Note 2707759

    "temp! refactor coding into reuse class and config class later!

    "personnel number
    lv_pernr = is_rd-id.

    select lt_pyc_d_py_msg~iaper as iaper, lt_pyc_d_py_msg~seqnr as seqnr,
      lt_pyc_d_py_msg~as_text as as_text, lt_pyc_d_py_msg~text1 as text1
      into table @lt_pyc_d_py_msg
      from pyc_d_py_msg as lt_pyc_d_py_msg
      where lt_pyc_d_py_msg~abkrs = @mv_payroll_area and
            lt_pyc_d_py_msg~iaper = @mv_payroll_period and
            lt_pyc_d_py_msg~pernr = @lv_pernr and
            lt_pyc_d_py_msg~test_res = @mv_tpy_res.

    clear ls_gov.
    clear: ls_par_val, lt_par_val.

    loop at lt_pyc_d_py_msg into ls_payroll_log.

      clear lt_lines.                                         "Note 2707759

      call function 'RKD_WORD_WRAP'                           "Note 2707759
        exporting
          textline  = ls_payroll_log-text1
          outputlen = 60
        tables
          out_lines = lt_lines.

      if ls_payroll_log-seqnr = '01'.                         "Note 2707759
        lv_seqnr = ls_payroll_log-seqnr.
      endif.

      loop at lt_lines assigning field-symbol(<lv_text1>).    "Note 2707759
        ls_gov-groupid    = '1'.
        ls_gov-group_name = 'Messages'.
        ls_gov-row_id     = lv_seqnr.                         "Note 2707759
*        ls_gov-row_id     = ls_payroll_log-seqnr.
        ls_gov-itemid     = 'ABKRS'.
        ls_gov-text       = ls_payroll_log-as_text.
        ls_gov-value      = <lv_text1>.                       "Note 2707759
*        ls_gov-value      = ls_payroll_log-text1.
        append ls_gov to lt_gov.
        lv_seqnr = lv_seqnr + 1.                              "Note 2707759
      endloop.
    endloop.


    "---------------------------------------------------------------------------------"
    "end processing
    sort lt_gov by row_id.
    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_gov.
      catch cx_pyd_fnd .
    endtry.
*    CALL METHOD is_rd-rd->set_data( lt_gov ).


  endmethod.


  method yk_payslip.


    data:
      lv_reident type rspoid,
      ls_params  type pri_params,
      lv_rqident type rspoid,
      lt_stream  type cl_pyc_rd_dto_stream=>ty_t_rd,
      ls_stream  type cl_pyc_rd_dto_stream=>ty_s_rd.

    data:
      lt_rt        type ty_t_rt_with_text,
      ls_rt        type ty_s_rt_with_text,
      lt_txt       type cl_pyd_rd_dto_t=>ty_t_rd,
      ls_txt       type cl_pyd_rd_dto_t=>ty_s_rd,
      lv_row_id    type pyd_rowid,
      lv_param     type string,
      lv_pernr     type p_pernr,
      lv_sum       type  maxbt,
      lv_uuid      type guid_16,
      lv_ls_nm     type string,
      lv_list_name type char12 value 'PCC_CHK_NM'.

    clear lt_rt .
    lv_pernr  = is_rd-id.

    call function 'GUID_CREATE'
      importing
        ev_guid_16 = lv_uuid.

    lv_ls_nm  =  lv_uuid .
    lv_list_name = lv_ls_nm+0(12).


    data: mo_payslip_helper   type ref to cl_hrxss_rem_helper,
          a_molga             type molga,
          a_relid             type relid_pcl2,
          a_it_filtered_rgdir type h99_clst_t_rgdir,
          wa_rgdir            like line of a_it_filtered_rgdir,
          hrform_name         type hrf_name,
          pdf_size            type  i,
          pdf_doc             type xstring.

    data: ls_a_it_filtered_rgdir type line of h99_clst_t_rgdir.

    "lv_pernr = '00002000'.
    create object mo_payslip_helper
      exporting
        iv_pernr = lv_pernr.


    a_molga = mo_payslip_helper->get_country_code( ).

* get cluster id for payroll results
    select single relid into a_relid from t500l where molga = a_molga.

    a_it_filtered_rgdir = mo_payslip_helper->get_filtered_rgdir( ).
    sort a_it_filtered_rgdir ascending by paydt fpbeg.                      "KVHN1527625


    loop at a_it_filtered_rgdir into ls_a_it_filtered_rgdir where inper eq mv_payroll_period.

    endloop.


    "  READ TABLE a_it_filtered_rgdir INTO wa_rgdir INDEX 1.
    wa_rgdir = ls_a_it_filtered_rgdir.
    if sy-subrc eq 0.
      hrform_name = 'SAP_PAYSLIP_US'.

      mo_payslip_helper->get_payslip(
          exporting
            is_rgdir     = wa_rgdir
            iv_form_name = hrform_name
          importing
            ev_document = pdf_doc
            ev_doc_size = pdf_size
          exceptions
            ex_payslip_creation_failed = 1
          ).

      ls_stream-mime_type = 'application/pdf'.

      ls_stream-row_id = '001'.
      ls_stream-value  = pdf_doc.
      append ls_stream to lt_stream.


      try.
          call method is_rd-rd->set_data
            exporting
              it_data = lt_stream.
        catch cx_pyd_fnd .
      endtry.

*      CALL METHOD is_rd-rd->set_data( lt_stream ).

*    else.
*
*
*      ls_txt-text = 'Employee does not yet have productive payroll results' .
*      ls_txt-row_id = '001'.
*      append ls_txt to lt_txt.
*
*    call method is_rd-rd->set_data( lt_txt ).

    endif.


  endmethod.


  method yk_wage_type_report.
    types:
      begin of t_lgart,
        sign   type ddsign,
        option type ddoption,
        low    type persa,
        high   type persa,
      end of t_lgart.

    types:
      begin of ty_s_rd,
        row_id type pyd_rowid,
        fpper  type fpper,
        lgart  type lgart.
        include type pyd_s_rdswt_ext as ext.
      types:
        text   type pyd_name,
      end of ty_s_rd .

    data: lt_rt     type ty_t_rt_with_text,
          lt_rt_2   type ty_t_rt_with_text,
          ls_rt     type ty_s_rt_with_text,
          ls_swt    type cl_pyd_rd_dto_swt=>ty_s_rd,
          lt_swt    type cl_pyd_rd_dto_swt=>ty_t_rd,
          lv_row_id type pyd_rowid,
          lv_param  type string,
          lv_pernr  type p_pernr,
          lv_curr   type waers.

    data lty_s_rd type sorted table  of ty_s_rd with non-unique key lgart.
    data ls_output_rt like line of et_rt.
    data lv_currency type waers.
    data lv_bit type c length 1.
    data lt_t512w type table of t512w.
    data lt_lgart type range of lgart.
    data ls_lgart like line of lt_lgart.
    data lv_molga type molga.
    data mv_persa type persa.
    data row_id type i.
    data ls_par like line of mt_exec_parameters.
    data lt_sel_lgart type range of lgart.

    field-symbols <ls_t512w> type t512w.

    lv_pernr  = is_rd-id.

    call function 'RH_PM_GET_MOLGA_FROM_PERNR'
      exporting
        pernr           = lv_pernr
        begda           = sy-datum
        endda           = sy-datum
      importing
        molga           = lv_molga
      exceptions
        nothing_found   = 1
        no_active_plvar = 2
        others          = 3.

    "get currency per company code
    call method me->get_currency_by_pernr
      exporting
        iv_pernr = lv_pernr
        iv_endda = mv_endda
      receiving
        rv_curr  = lv_curr.

    "Work out what the list of WTs you want to show yourself
    ls_lgart-low = '/101'.
    ls_lgart-high = '/101'.
    ls_lgart-option = 'BT'.
    ls_lgart-sign = 'I'.
    append ls_lgart to lt_sel_lgart.
    ls_lgart-low = '/106'.
    ls_lgart-high = '/106'.
    append ls_lgart to lt_sel_lgart.
    ls_lgart-low = '/110'.
    ls_lgart-high = '/110'.
    append ls_lgart to lt_sel_lgart.
    ls_lgart-low = '/116'.
    ls_lgart-high = '/116'.
    append ls_lgart to lt_sel_lgart.
    ls_lgart-low = '/142'.
    ls_lgart-high = '/142'.
    append ls_lgart to lt_sel_lgart.
    ls_lgart-low = '/172'.
    ls_lgart-high = '/172'.
    append ls_lgart to lt_sel_lgart.
    ls_lgart-low = '/559'.
    ls_lgart-high = '/559'.
    append ls_lgart to lt_sel_lgart.
    ls_lgart-low = '/401'.
    ls_lgart-high = '/4ZZ'.
    append ls_lgart to lt_sel_lgart.
    ls_lgart-low = '0000'.
    ls_lgart-high = 'ZZZZ'.
    append ls_lgart to lt_sel_lgart.

    clear ls_lgart.


    if mv_tpy_res = 'X'. "Test Payroll results

      " WTs for IN period A results
*****Note 2955368 - include rte_curr field from P2RX_RT in query to correctly display the hourly rate upto 5 decimal places *******
*      SELECT hrdct_tpy_rgdir~fpper AS groupid, hrdct_tpy_rgdir~fpper AS group_name, p2rx_rt~lgart AS lgart, p2rx_rt~betpe AS betpe, p2rx_rt~anzhl AS anzhl, p2rx_rt~rte_curr AS rte_curr, t512t~lgtxt AS lgtxt,"p2rx_eval_period~fpper as fpper,
*****Note 2959827 - SUM ANZHL field to appropriately display cumulated hours in case there are splits generated in the wagetype *****
**** Note 2964127 - Incorrect Currency conversion *****
      select hrdct_tpy_rgdir~fpper as groupid, hrdct_tpy_rgdir~fpper as group_name, p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe, sum( case hrdct_tpy_rgdir~srtza when 'P' then anzhl * -1
                         else anzhl end ) as anzhl, p2rx_rt~amt_curr as amt_curr, p2rx_rt~rte_curr as rte_curr, t512t~lgtxt as lgtxt,"p2rx_eval_period~fpper as fpper,
           sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1
                         else betrg end ) as betrg
             into corresponding fields of table @et_rt
                      from p2rx_rt inner join hrdct_tpy_rgdir
                                    on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr
                                   and hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
                         inner join t512t
                                   on t512t~sprsl = @sy-langu
                                   and t512t~molga = @lv_molga
                                   and t512t~lgart = p2rx_rt~lgart
            where hrdct_tpy_rgdir~abkrs = @mv_payroll_area and
                  hrdct_tpy_rgdir~dct_pernr = @lv_pernr and
                  hrdct_tpy_rgdir~inper = @mv_payroll_period and
                  ( p2rx_rt~betrg <> 0 or p2rx_rt~betpe <> 0 ) and
                  p2rx_rt~lgart in @lt_sel_lgart
            group by hrdct_tpy_rgdir~fpper, p2rx_rt~lgart, p2rx_rt~betpe, p2rx_rt~anzhl, p2rx_rt~amt_curr, p2rx_rt~rte_curr, t512t~lgtxt . "#EC CI_BUFFJOIN

      " WTs for retro P results
*****Note 2955368 - include rte_curr field from P2RX_RT in query to correctly display the hourly rate upto 5 decimal places *******
*      SELECT hrdct_tpy_rgdir~fpper AS groupid, hrdct_tpy_rgdir~fpper AS group_name, p2rx_rt~lgart AS lgart, p2rx_rt~betpe AS betpe, p2rx_rt~anzhl AS anzhl, p2rx_rt~rte_curr AS rte_curr, t512t~lgtxt AS lgtxt,"p2rx_eval_period~fpper as fpper,
*****Note 2959827 - SUM ANZHL field to appropriately display cumulated hours in case there are splits generated in the wagetype *****
**** Note 2964127 - Incorrect Currency conversion *****
      select hrdct_tpy_rgdir~fpper as groupid, hrdct_tpy_rgdir~fpper as group_name, p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe, sum( case hrdct_tpy_rgdir~srtza when 'P' then anzhl * -1
                         else anzhl end ) as anzhl, p2rx_rt~amt_curr as amt_curr, p2rx_rt~rte_curr as rte_curr, t512t~lgtxt as lgtxt,"p2rx_eval_period~fpper as fpper,
         sum( case hrpy_rgdir~srtza when 'A' then betrg * -1
                       else 0 end ) as betrg
             into corresponding fields of table @lt_rt_2
                    from p2rx_rt inner join hrpy_rgdir
                                  on hrpy_rgdir~pernr eq p2rx_rt~dct_pernr
                                 and hrpy_rgdir~seqnr eq p2rx_rt~dct_seqnr
                       inner join hrdct_tpy_rgdir
                                  on hrdct_tpy_rgdir~dct_pernr eq hrpy_rgdir~pernr
                                 and hrdct_tpy_rgdir~fpper eq hrpy_rgdir~fpper
                         inner join t512t
                                   on t512t~sprsl = @sy-langu
                                   and t512t~molga = @lv_molga
                                   and t512t~lgart = p2rx_rt~lgart
            where hrdct_tpy_rgdir~abkrs = @mv_payroll_area and
                  hrdct_tpy_rgdir~dct_pernr = @lv_pernr and
                  hrdct_tpy_rgdir~inper = @mv_payroll_period and
                  hrpy_rgdir~inper <> @mv_payroll_period and "in case of current period off-cycles
                  ( p2rx_rt~betrg <> 0 or p2rx_rt~betpe <> 0 ) and
                  p2rx_rt~lgart in @lt_sel_lgart
            group by hrdct_tpy_rgdir~fpper, p2rx_rt~lgart, p2rx_rt~betpe, p2rx_rt~anzhl, p2rx_rt~amt_curr, p2rx_rt~rte_curr, t512t~lgtxt . "#EC CI_BUFFJOIN

      append lines of et_rt to lt_rt_2.
      sort lt_rt_2 ascending.
      clear et_rt.
      loop at lt_rt_2 into ls_rt.
        collect ls_rt into et_rt.
      endloop.


    else.
*****Note 2955368 - include rte_curr field from P2RX_RT in query to correctly display the hourly rate upto 5 decimal places *******
*      SELECT hrdct_tpy_rgdir~fpper AS groupid, hrdct_tpy_rgdir~fpper AS group_name, p2rx_rt~lgart AS lgart, p2rx_rt~betpe AS betpe, p2rx_rt~anzhl AS anzhl, p2rx_rt~rte_curr AS rte_curr, t512t~lgtxt AS lgtxt,"p2rx_eval_period~fpper as fpper,
*****Note 2959827 - SUM ANZHL field to appropriately display cumulated hours in case there are splits generated in the wagetype *****
**** Note 2964127 - Incorrect Currency conversion *****
      select p2rx_eval_period~fpper as groupid, p2rx_eval_period~fpper as group_name, p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe, sum( case p2rx_eval_period~srtza when 'P' then anzhl * -1
                         else anzhl end ) as anzhl, p2rx_rt~amt_curr as amt_curr, p2rx_rt~rte_curr as rte_curr, t512t~lgtxt as lgtxt,"p2rx_eval_period~fpper as fpper,
     sum( case p2rx_eval_period~srtza when 'P' then betrg * -1
                   else betrg end ) as betrg
       into corresponding fields of table @et_rt
                from p2rx_rt inner join p2rx_eval_period
                              on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr
                             and p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
                   inner join t512t
                             on t512t~sprsl = @sy-langu
                             and t512t~molga = @lv_molga
                             and t512t~lgart = p2rx_rt~lgart
      where p2rx_eval_period~abkrs = @mv_payroll_area and
            p2rx_eval_period~dct_pernr = @lv_pernr and
            p2rx_eval_period~inper = @mv_payroll_period and
            ( p2rx_rt~betrg <> 0 or p2rx_rt~betpe <> 0 ) and
            p2rx_rt~lgart in @lt_sel_lgart
      group by p2rx_eval_period~fpper, p2rx_rt~lgart, p2rx_rt~betpe, p2rx_rt~anzhl, p2rx_rt~amt_curr, p2rx_rt~rte_curr, t512t~lgtxt . "#EC CI_BUFFJOIN

    endif.


    sort et_rt by groupid descending lgart ascending.



    loop at et_rt into ls_rt.
      ls_swt-group_name = 'FOR Period ' && '&' && ls_rt-group_name+4(2) && '/' && ls_rt-group_name(4).
      replace '&' with space into ls_swt-group_name.
      ls_swt-groupid = ls_rt-group_name.
      ls_swt-row_id = lv_row_id.
      ls_swt-itemid = ls_rt-lgart.
      ls_swt-text = ls_rt-lgtxt.
      ls_swt-amt  = ls_rt-betrg.
      ls_swt-num  = ls_rt-anzhl.
      ls_swt-rte  = ls_rt-betpe.
      if ls_rt-amt_curr is initial.
        ls_swt-amt_curr = lv_curr.
      else.
        ls_swt-amt_curr = ls_rt-amt_curr.   " Note 2964127 - Incorrect Currency conversion
      endif.
      if ls_rt-rte_curr is initial.
        ls_swt-rte_curr = lv_curr.
      else.
        ls_swt-rte_curr = ls_rt-rte_curr.   " Note 2955368 - include rte_curr field from P2RX_RT in query to correctly display the hourly rate upto 5 decimal places
      endif.
      add 1 to lv_row_id .
      append ls_swt to lt_swt.
    endloop.

    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_swt.
      catch cx_pyd_fnd .
    endtry.

*    CALL METHOD is_rd-rd->set_data( lt_swt ).

  endmethod.


  method z99_payslip.
* Display Payslip based on the context
    data: lt_stream type cl_pyc_rd_dto_stream=>ty_t_rd,
          ls_stream type cl_pyc_rd_dto_stream=>ty_s_rd.

    data: lv_pernr     type p_pernr.
* Employee Number
    lv_pernr  = is_rd-id.
    if mv_tpy_res eq abap_true.
* TPY Payslip
      lt_stream = get_rca_tpy_payslip( iv_pernr = lv_pernr ).
    else.
* Production Payslip for Current Period
      lt_stream = get_rca_prod_payslip( iv_pernr  = lv_pernr iv_per = 'C'   ).
    endif.
    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_stream.
      catch cx_pyd_fnd .
    endtry.

  endmethod.
ENDCLASS.
