class zusecl_sbp_pyd_chk_fp4_us_bas definition
  public
  inheriting from cl_pyd_chk_fp4_base
  abstract
  create public .

  public section.

    constants mc_molga type molga value '13' ##NO_TEXT.
    constants mc_param_abkrs type pyd_par_type value 'ABKRS' ##NO_TEXT.
    constants mc_param_period type pyd_par_type value 'PERIOD' ##NO_TEXT.
    constants mc_param_tpy_res type pyd_par_type value 'TPY_RES' ##NO_TEXT.
    constants mc_srtza_curr type srtza value 'A' ##NO_TEXT.
    constants mc_srtza_prev type srtza value 'P' ##NO_TEXT.

    class-methods set_select_option
      importing
        !iv_value  type any
        !iv_sign   type /iwbep/s_cod_select_option-sign
        !iv_option type /iwbep/s_cod_select_option-option
      changing
        !ch_so     type /iwbep/t_cod_select_options .
    methods handle_init_buffers
        for event init_buffers of if_pyd_transaction .

    methods if_pyd_ty_rt~execute
        redefinition .
    methods if_pyd_ty_rt~result_details_get
        redefinition .
  protected section.

    types:
      begin of ty_s_cats_2000_info,
        pernr type p_pernr,
        datum type datum,
        awart type awart,
        atext type abwtxt,
        stdaz type abstd,
        beguz type catsbeguz,
        enduz type catsenduz,
        plans type plans,
      end of ty_s_cats_2000_info .
    types:
      begin of ty_s_cats_2010_info,
        pernr type p_pernr,
        datum type datum,
        lgart type lgart,
        lgtxt type lgtxt,
        stdaz type enstd,
      end of ty_s_cats_2010_info .
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
        betrg      type maxbt,
        anzhl      type anzhl,
        betpe      type betpe,
        lgtxt      type lgtxt,
        fpper      type fpper,
        zeinh      type pt_zeinh,
        amt_curr   type waers,
        rte_curr   type waers,
      end of ty_s_rt_with_text .
    types:
      begin of ty_s_zl_with_text,
        pernr type p_pernr,
        datum type datum,
        lgart type lgart,
        anzhl type anzhl,
        lgtxt type lgtxt,
      end of ty_s_zl_with_text .
    types:
      ty_t_cats_2000_info type sorted table of ty_s_cats_2000_info with non-unique key pernr .
    types:
      ty_t_cats_2010_info type sorted table of ty_s_cats_2010_info with non-unique key pernr .
    types:
      ty_t_rt_with_text type standard table of ty_s_rt_with_text .
    types:
      ty_t_zl_with_text type standard table of ty_s_zl_with_text .

    constants:
      begin of gcs_groupid,
        org_data type pyd_cont_groupid value 'ORG_DATA',
        pay_data type pyd_cont_groupid value 'PAY_DATA',
      end of gcs_groupid .
    data mo_context type ref to if_pyd_res_context .
    data mt_abkrs type /iwbep/t_cod_select_options .
    data mt_abrsp type /iwbep/t_cod_select_options .
    data mt_ansvh type /iwbep/t_cod_select_options .
    data mt_btrtl type /iwbep/t_cod_select_options .
    data mt_bukrs type /iwbep/t_cod_select_options .
    data mt_enriched_cats_2000_info type ty_t_cats_2000_info .
    data mt_enriched_cats_2010_info type ty_t_cats_2010_info .
    data mt_enriched_rt type ty_t_rt_with_text .
    data mt_enriched_zl type ty_t_zl_with_text .
    data mt_exec_parameters type if_pyd_fnd_types=>ty_t_resp .
    data mt_infty type /iwbep/t_cod_select_options .
    data mt_infty2 type /iwbep/t_cod_select_options .
    data mt_kostl type /iwbep/t_cod_select_options .
    data mt_lgart_exclude_filter type /iwbep/t_cod_select_options .
    data mt_lgart_include_filter type /iwbep/t_cod_select_options .
    data mt_massn type /iwbep/t_cod_select_options .
    data mt_payroll_areas type /iwbep/t_cod_select_options .
    data mt_persg type /iwbep/t_cod_select_options .
    data mt_persk type /iwbep/t_cod_select_options .
    data mt_plans type /iwbep/t_cod_select_options .
    data mt_pwstat2 type /iwbep/t_cod_select_options .
    data mt_rzawe type /iwbep/t_cod_select_options .
    data mt_schkz type /iwbep/t_cod_select_options .
    data mt_stat2 type /iwbep/t_cod_select_options .
    data mt_state type /iwbep/t_cod_select_options .
    data mt_statu2 type /iwbep/t_cod_select_options .
    data mt_subty type /iwbep/t_cod_select_options .
    data mt_subty2 type /iwbep/t_cod_select_options .
    data mt_teilk type /iwbep/t_cod_select_options .
    data mt_trfar type /iwbep/t_cod_select_options .
    data mt_trfgb type /iwbep/t_cod_select_options .
    data mt_trfgr type /iwbep/t_cod_select_options .
    data mt_trfst type /iwbep/t_cod_select_options .
    data mt_vwsaz type /iwbep/t_cod_select_options .
    data mt_werks type /iwbep/t_cod_select_options .
    data mt_zterf type /iwbep/t_cod_select_options .
    data mv_begda type datum .
    data mv_curr type waers .
    data mv_delay_days type rvari_val_255 .
    data mv_endda type datum .
    data mv_infty type infty .
    data mv_infty2 type infty .
    data mv_lgart type lgart .
    data mv_payroll_area type abkrs .
    data mv_payroll_period type iperi .
    data mv_subty type subty .
    data mv_subty2 type subty .
    data mv_tpy_res type hrdct_is_tpy .
    data mt_orgeh type /iwbep/t_cod_select_options .

    methods adjust_sfo_texts
      changing
        !rt_result type cl_pyd_rd_dto_sfo=>ty_t_rd .
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
        value(rt_result) type ty_t_result .
    methods get_check_delay .
    methods get_context
      returning
        value(ro_result) type ref to if_pyd_res_context .
    methods get_custom_filter_scope .
    methods get_employee_scope_cust_filt
      importing
        !it_pernr_so type /iwbep/t_cod_select_options
      returning
        value(rt_so) type /iwbep/t_cod_select_options .
    methods get_generic_sfo
      returning
        value(rt_result) type cl_pyd_rd_dto_sfo=>ty_t_rd .
    methods get_parameters
      importing
        !it_par         type if_pyd_fnd_types=>ty_t_resp
        !io_res_context type ref to if_pyd_res_context .
    methods get_payroll_area
      returning
        value(rv_result) type abkrs .
    methods get_payroll_areas .
    methods get_payroll_period
      returning
        value(rv_result) type iperi .
    methods get_payroll_period_info
      returning
        value(rs_result) type ty_s_period_info .
    methods get_pernr_parameters
      importing
        !it_par_addl     type if_pyd_ty_rt=>ty_t_result_key
      returning
        value(rt_result) type /iwbep/t_cod_select_options .
    methods get_specifc_custmizing
      importing
        !it_par         type if_pyd_fnd_types=>ty_t_resp
        !io_res_context type ref to if_pyd_res_context .
    methods get_tpy_res
      returning
        value(rv_result) type hrdct_is_tpy .
    methods is_check_delayed
      returning
        value(rv_result) type boole_d .
    methods is_off_cycle_run
      importing
        !io_res_context  type ref to if_pyd_res_context
      returning
        value(rv_result) type boole_d .
    methods parse_exclude_entry
      importing
        !iv_entry type zhr_pcc_params-value .
    methods parse_include_entry
      importing
        !iv_entry type zhr_pcc_params-value .
    methods sap_pdf_get
      importing
        !is_rd          type if_pyd_fnd_types=>ty_s_rd
        !it_par         type if_pyd_fnd_types=>ty_t_resp
        !io_res_context type ref to if_pyd_res_context
        !iv_access_mode type pyd_rdt_data_access_mode
      raising
        cx_pyd_fnd .
    methods sap_sfo_get
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
    methods sap_swt_get
      importing
        !is_rd          type if_pyd_fnd_types=>ty_s_rd
        !it_par         type if_pyd_fnd_types=>ty_t_resp
        !io_res_context type ref to if_pyd_res_context
        !iv_access_mode type pyd_rdt_data_access_mode
      raising
        cx_pyd_fnd .
    methods sap_t_get
      importing
        !is_rd          type if_pyd_fnd_types=>ty_s_rd
        !it_par         type if_pyd_fnd_types=>ty_t_resp
        !io_res_context type ref to if_pyd_res_context
        !iv_access_mode type pyd_rdt_data_access_mode .
    methods select_cats_records
      importing
        !iv_pernr          type p_pernr
      exporting
        !et_cats_2000_info type ty_t_cats_2000_info
        !et_cats_2010_info type ty_t_cats_2010_info .
    methods set_context
      importing
        !io_context type ref to if_pyd_res_context .
    methods set_stat2_default .
    methods yk_audit_report
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
    methods yk_emp_info
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
    methods yk_payslip
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
    methods copy_structure_to_other
      importing
        !p_struct1 type any
      changing
        !p_struct2 type any .

    methods err_kv_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_SBP_PYD_CHK_FP4_US_BAS IMPLEMENTATION.


  method adjust_sfo_texts ##NEEDED.
*   To be Redifined.

  endmethod.


  method copy_structure_to_other.
* Copy Content of a Structure to another structure
    call function 'HR_99S_COPY_STRUC1_STRUC2'
      exporting
        p_struct1 = p_struct1
      importing
        p_struct2 = p_struct2.

  endmethod.


  method err_kv_get_list.
* To be redefined as per customer need, in case more details are needed.
* By default, this method will show the following info:
*   1) PY Area
*   2) PY Area description

    data lv_atext  type t549t-atext.

    me->get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).


    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        loop at ct_err_kv into data(ls_err_kv).
          select single atext into lv_atext from t549t
            where abkrs = me->mv_payroll_area
            and   sprsl = sy-langu.
          if sy-subrc = 0.
            ls_err_kv-text = lv_atext.
          endif.

          ls_err_kv-category = gcs_kv_cat-other.  "Possible values are OTH and NUM
          ls_err_kv-value = me->mv_payroll_area.
          modify ct_err_kv from ls_err_kv transporting text category value.
        endloop.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        loop at ct_err_kv into ls_err_kv.
          ls_err_kv-category = gcs_kv_cat-other.
          ls_err_kv-value    = me->mv_payroll_area.
          modify ct_err_kv from ls_err_kv transporting kv.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method get_check_delay.
    select single par_type from PYD_D_PTT  into mv_delay_days
      where name     eq 'Number of Delay Days'.
  endmethod.


  method get_context.
    ro_result = me->mo_context.
  endmethod.


  method get_custom_filter_scope.
* Read the values from the ZHR_PCC_PARAMS table
    select function, value from zhr_pcc_params into table @data(lt_params)
      where type       eq @if_pyd_ty_rt~mv_type
        and ( function eq 'INCLUDE' or function eq 'EXCLUDE' ).
* Prcoess each of the parameter table entries
    loop at lt_params reference into data(lr_params).
      case lr_params->function.
        when 'INCLUDE'.
          me->parse_include_entry( iv_entry = lr_params->value ).
        when 'EXCLUDE'.
          me->parse_exclude_entry( iv_entry = lr_params->value ).
      endcase.
    endloop.
  endmethod.


  method get_employee_scope_cust_filt.
* Local Data Declarations
    data: lt_so      type /iwbep/t_cod_select_options,
          lt_exclude type /iwbep/t_cod_select_options.
* Clear the return value
    clear rt_so.
* Determine the employee scope
    if mv_tpy_res eq abap_true.
      select distinct rgdir~dct_pernr as low into corresponding fields of table lt_so from hrdct_tpy_rgdir as rgdir ##TOO_MANY_ITAB_FIELDS
        inner join p2rx_rt   as rt    on rgdir~dct_pernr = rt~dct_pernr   and
                                         rgdir~dct_seqnr = rt~dct_seqnr
        inner join p2rx_wpbp as wpbp  on rgdir~dct_pernr = wpbp~dct_pernr and
                                         rgdir~dct_seqnr = wpbp~dct_seqnr
        inner join pa0000    as p0000 on rgdir~dct_pernr = p0000~pernr
        where wpbp~bukrs in mt_bukrs and
              wpbp~werks in mt_werks and
              wpbp~btrtl in mt_btrtl and
              wpbp~persg in mt_persg and
              wpbp~persk in mt_persk and
              wpbp~schkz in mt_schkz and
              wpbp~zterf in mt_zterf and
              wpbp~trfar in mt_trfar and
              wpbp~trfgb in mt_trfgb and
              wpbp~trfgr in mt_trfgr and
              wpbp~trfst in mt_trfst and
              wpbp~plans in mt_plans and
              wpbp~kostl in mt_kostl and
              wpbp~begda le mv_endda and
              wpbp~endda ge mv_endda and
              rgdir~dct_pernr in it_pernr_so   and
              rgdir~abkrs eq mv_payroll_area   and
              rgdir~srtza eq mc_srtza_curr     and
              rgdir~inper eq mv_payroll_period and
              rgdir~fpper eq mv_payroll_period and
              rgdir~ocrsn eq space             and
              rgdir~inocr eq space             and
              rt~lgart    in mt_lgart_include_filter and
              p0000~begda le mv_endda          and
              p0000~endda ge mv_begda          and
              p0000~stat2 in mt_stat2.

      if mt_lgart_exclude_filter is not initial.
        select distinct rgdir~dct_pernr as low into corresponding fields of table lt_exclude from hrdct_tpy_rgdir as rgdir ##TOO_MANY_ITAB_FIELDS
          inner join p2rx_rt   as rt    on rgdir~dct_pernr = rt~dct_pernr   and
                                           rgdir~dct_seqnr = rt~dct_seqnr
          inner join p2rx_wpbp as wpbp  on rgdir~dct_pernr = wpbp~dct_pernr and
                                           rgdir~dct_seqnr = wpbp~dct_seqnr
          inner join pa0000    as p0000 on rgdir~dct_pernr = p0000~pernr
          where wpbp~bukrs in mt_bukrs and
                wpbp~werks in mt_werks and
                wpbp~btrtl in mt_btrtl and
                wpbp~persg in mt_persg and
                wpbp~persk in mt_persk and
                wpbp~schkz in mt_schkz and
                wpbp~zterf in mt_zterf and
                wpbp~trfar in mt_trfar and
                wpbp~trfgb in mt_trfgb and
                wpbp~trfgr in mt_trfgr and
                wpbp~trfst in mt_trfst and
                wpbp~plans in mt_plans and
                wpbp~kostl in mt_kostl and
                wpbp~begda le mv_endda and
                wpbp~endda ge mv_endda and
                rgdir~dct_pernr in it_pernr_so   and
                rgdir~abkrs eq mv_payroll_area   and
                rgdir~srtza eq mc_srtza_curr     and
                rgdir~inper eq mv_payroll_period and
                rgdir~fpper eq mv_payroll_period and
                rgdir~ocrsn eq space             and
                rgdir~inocr eq space             and
                rt~lgart    in mt_lgart_exclude_filter and
                p0000~begda le mv_endda          and
                p0000~endda ge mv_begda          and
                p0000~stat2 in mt_stat2.
      endif.

    else.
      select distinct rgdir~pernr as low into corresponding fields of table lt_so from hrpy_rgdir as rgdir ##TOO_MANY_ITAB_FIELDS
        inner join p2rx_rt   as rt    on rgdir~pernr = rt~dct_pernr   and
                                         rgdir~seqnr = rt~dct_seqnr
        inner join p2rx_wpbp as wpbp  on rgdir~pernr = wpbp~dct_pernr and
                                         rgdir~seqnr = wpbp~dct_seqnr
        inner join pa0000    as p0000 on rgdir~pernr = p0000~pernr
        where wpbp~bukrs in mt_bukrs and
              wpbp~werks in mt_werks and
              wpbp~btrtl in mt_btrtl and
              wpbp~persg in mt_persg and
              wpbp~persk in mt_persk and
              wpbp~schkz in mt_schkz and
              wpbp~zterf in mt_zterf and
              wpbp~trfar in mt_trfar and
              wpbp~trfgb in mt_trfgb and
              wpbp~trfgr in mt_trfgr and
              wpbp~trfst in mt_trfst and
              wpbp~plans in mt_plans and
              wpbp~kostl in mt_kostl and
              wpbp~begda le mv_endda and
              wpbp~endda ge mv_endda and
              rgdir~pernr in it_pernr_so       and
              rgdir~abkrs eq mv_payroll_area   and
              rgdir~srtza eq mc_srtza_curr     and
              rgdir~inper eq mv_payroll_period and
              rgdir~fpper eq mv_payroll_period and
              rgdir~ocrsn eq space             and
              rgdir~inocr eq space             and
              rt~lgart    in mt_lgart_include_filter and
              p0000~begda le mv_endda          and
              p0000~endda ge mv_begda          and
              p0000~stat2 in mt_stat2.

      if mt_lgart_exclude_filter is not initial.
        select distinct rgdir~pernr as low into corresponding fields of table lt_exclude from hrpy_rgdir as rgdir ##TOO_MANY_ITAB_FIELDS
          inner join p2rx_rt   as rt    on rgdir~pernr = rt~dct_pernr   and
                                           rgdir~seqnr = rt~dct_seqnr
          inner join p2rx_wpbp as wpbp  on rgdir~pernr = wpbp~dct_pernr and
                                           rgdir~seqnr = wpbp~dct_seqnr
          inner join pa0000    as p0000 on rgdir~pernr = p0000~pernr
          where wpbp~bukrs in mt_bukrs and
                wpbp~werks in mt_werks and
                wpbp~btrtl in mt_btrtl and
                wpbp~persg in mt_persg and
                wpbp~persk in mt_persk and
                wpbp~schkz in mt_schkz and
                wpbp~zterf in mt_zterf and
                wpbp~trfar in mt_trfar and
                wpbp~trfgb in mt_trfgb and
                wpbp~trfgr in mt_trfgr and
                wpbp~trfst in mt_trfst and
                wpbp~plans in mt_plans and
                wpbp~kostl in mt_kostl and
                wpbp~begda le mv_endda and
                wpbp~endda ge mv_endda and
                rgdir~pernr in it_pernr_so       and
                rgdir~abkrs eq mv_payroll_area   and
                rgdir~srtza eq mc_srtza_curr     and
                rgdir~inper eq mv_payroll_period and
                rgdir~fpper eq mv_payroll_period and
                rgdir~ocrsn eq space             and
                rgdir~inocr eq space             and
                rt~lgart    in mt_lgart_exclude_filter and
                p0000~begda le mv_endda          and
                p0000~endda ge mv_begda          and
                p0000~stat2 in mt_stat2.
      endif.
    endif.
* Sort the include & exclude lists
    sort lt_exclude by low.
    sort lt_so by low.
* For each entry of inclusion for an employee
    loop at lt_so into data(ls_so).
* Ensure they should not be excluded
      read table lt_exclude transporting no fields with key low = ls_so-low binary search.
      if sy-subrc ne 0.
* And if not, add them to the list.
        append value #( sign = 'I' option = 'EQ' low = ls_so-low ) to rt_so.
      endif.
    endloop.
  endmethod.


  method get_generic_sfo.
* Local Data Declarations
    data: ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
          lv_row_id type pyd_rowid.
* Add 1 to the row id
    add 1 to lv_row_id.
* Set the row number, value and text
    ls_sfo-row_id   = lv_row_id.
    ls_sfo-value    = mv_payroll_area.
    ls_sfo-text     = text-003.
* Append to the result data table
    append ls_sfo to rt_result.
* Add 1 to the row id
    add 1 to lv_row_id.
* Set the row number, value and text
    ls_sfo-row_id   = lv_row_id.
    ls_sfo-value    = mv_payroll_period.
    ls_sfo-text     = text-007.
* Append to the result data table
    append ls_sfo to rt_result.
* Add 1 to the row id
    add 1 to lv_row_id.
* Set the row number, value and text
    ls_sfo-row_id   = lv_row_id.
    ls_sfo-value    = |{ mv_begda date = user }|.
    ls_sfo-text     = text-008.
* Append to the result data table
    append ls_sfo to rt_result.
* Add 1 to the row id
    add 1 to lv_row_id.
* Set the row number, value and text
    ls_sfo-row_id   = lv_row_id.
    ls_sfo-value    = |{ mv_endda date = user }|.
    ls_sfo-text     = text-009.
* Append to the result data table
    append ls_sfo to rt_result.
  endmethod.


  method get_parameters.
* Set the context if it is currently unknown
    if me->mo_context is initial.
      me->set_context( io_res_context ).
    endif.
*   HRDCT: Is a Payroll Test result?
    me->get_tpy_res( ).
*   Get Payroll Area value
    me->get_payroll_area( ).
    me->get_payroll_areas( ). "note: 2789066: fetch multiple payroll areas from the context
*   Get Period value
    me->get_payroll_period( ).
    me->get_payroll_period_info( ).
* Get the number of days to delay the check for
    me->get_check_delay( ).
* Populate the ranges based on custom config table and not context object
    me->get_custom_filter_scope( ).
* Set the employement status to active only if nothing is specified
    me->set_stat2_default( ).
*   Get other customizing - To Be Redefined as per customer need
    me->get_specifc_custmizing( it_par         = it_par
                                io_res_context = io_res_context  ).
  endmethod.


  method get_payroll_area.
* If the payroll area is stored globally, return that
    if me->mv_payroll_area is not initial.
      rv_result = me->mv_payroll_area.
      return.
    endif.
* Otherwise retrieve it from the context.
    try.
        me->mv_payroll_area = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_param_abkrs
                                                                    it_par      = me->get_context( )->mt_par ).
      catch cx_root ##CATCH_ALL.
        return.
    endtry.
* Set the return result value
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
* If the payroll period is stored globally, return that
    if me->mv_payroll_period is not initial.
      rv_result = me->mv_payroll_period.
      return.
    endif.
* Otherwise retrieve it from the context.
    try.
        me->mv_payroll_period = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_param_period
                                                                      it_par      = me->get_context( )->mt_par ).
      catch cx_root ##CATCH_ALL.
        return.
    endtry.
* Set the return result value
    rv_result = me->mv_payroll_period.
  endmethod.


  method get_payroll_period_info.
* Local Data Declarations
    data: lv_pabrj type pabrj,
          lv_pabrp type pabrp.
* Read the period
    data(lv_period) = me->get_payroll_period( ).
* Split the year from the period
    lv_pabrj = lv_period(4).
    lv_pabrp = lv_period+4(2).
* Get the dates for the period
    cl_hr_payroll_area=>get_instance( imp_area = me->get_payroll_area( ) )->get_period_info(
      exporting imp_pabrj = lv_pabrj
                imp_pabrp = lv_pabrp
      importing exp_begda = data(lv_begda)
                exp_endda = data(lv_endda) ).
* Set the dates
    rs_result-begda = mv_begda = lv_begda.
    rs_result-endda = mv_endda = lv_endda.
  endmethod.


  method get_pernr_parameters.
* If the object result is not empty
    if it_par_addl is not initial .
* Extract all the employees and add them to the result list
      loop at it_par_addl into data(ls_par_addl) where par_type = if_pyd_cont_types=>gcs_par_type-pernr.
        try.
            cl_pyd_fnd_aux=>append_so_fixed_value( exporting iv_value = ls_par_addl-id
                                                   changing  ct_so    = rt_result ).
          catch cx_pyd_fnd.
            return.
        endtry.
      endloop.
    endif.
  endmethod.


  method get_specifc_custmizing ##NEEDED.
* TO BE REDEFINED IN CASE OF SPECIFIC CUSTOMIZING OTHER THAN WHAT HAS ALREADY BE IMPORTED
  endmethod.


  method get_tpy_res.
* If the test payroll flag is stored globally, return that
    if me->mv_tpy_res is not initial.
      rv_result = me->mv_tpy_res.
    endif.
* Otherwise retrieve it from the context.
    try.
* Get the current run mode of the process (Test/production).
        me->mv_tpy_res = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_param_tpy_res
                                                               it_par      = me->get_context( )->mt_par ).
      catch cx_root ##CATCH_ALL.
        return.
    endtry.
* Set the return result value
    rv_result = me->mv_tpy_res.
  endmethod.


  method handle_init_buffers.
*According to note 2164175, clear all buffered variables
    clear mt_abkrs.
    clear mv_lgart.
    clear mv_payroll_period.
    clear mv_begda.
    clear mv_endda.
    clear mv_tpy_res.
    clear mt_bukrs.
    clear mv_lgart.
    clear mt_btrtl.
    clear mt_schkz.
    clear mt_zterf.
    clear mt_trfar.
    clear mt_trfgb.
    clear mt_trfgr.
    clear mt_trfst.
    clear mt_plans.
    clear mt_lgart_include_filter.
    clear mt_lgart_exclude_filter.
    clear mv_delay_days.
    clear mt_pwstat2.
* Deregister
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction activation ' '.
  endmethod.


  method if_pyd_ty_rt~execute.
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
* Some checks can be delayed
* Need to perform this here as to ensure parameter mv_begda & mv_delay_days is filled.
    if is_check_delayed( ) eq abap_true.
      return.
    endif.
* Call the method to perform the check
    try .
        rt_result = me->check( is_ty          = is_ty
                               is_inst        = is_inst
                               it_par         = it_par
                               io_res_context = io_res_context
                               it_par_addl    = it_par_addl
                               it_pernr_so    = lr_pernrs_so ).
      catch cx_pyd_fnd ##NO_HANDLER.
    endtry.
  endmethod.


  method if_pyd_ty_rt~result_details_get.
* get parameters
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
        "This just a sample to show how to display the pdf views
        sap_pdf_get(
       exporting
         is_rd          = is_rd    " Result Detail Generic
         it_par         = it_par    " Result Parameter List
         io_res_context = io_res_context    " PYDS: Result Context
         iv_access_mode = iv_access_mode    " Payroll Data Source Result Details Type Data Access Mode
         ).

      when 'SAP_SWT'.
* Call the Simple Wage Type output table population
        sap_swt_get( exporting is_rd          = is_rd    " Result Detail Generic
                               it_par         = it_par    " Result Parameter List
                               io_res_context = io_res_context    " PYDS: Result Context
                               iv_access_mode = iv_access_mode ).   " Payroll Data Source Result Details Type Data Access Mode

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


  method is_check_delayed.
* Local Data Declarations
    data: lv_days  type i,
          lv_datum type datum.
* Clear the result
    clear rv_result.
* If no delay is found, return
    if mv_delay_days is initial.
      return.
    endif.
* Otherwise add days to the beginning of the period
    lv_days  = mv_delay_days.
    lv_datum = mv_begda + lv_days.
* If todays date is prior to that, then delay the check
    if sy-datum lt lv_datum.
      rv_result = abap_true.
    endif.
  endmethod.


  method is_off_cycle_run.
* Local Data Declarations
    data lv_pt_cat type pyc_proc_templ_cat.
* Clear the return result
    clear rv_result.
* If we have context
    if io_res_context is not initial.
* Determine the processing template category
      read table io_res_context->mt_par into data(ls_par)
                                        with key par_type = cl_pyc_pt_proc_templ_cat=>gc_par_type.
* If found
      if sy-subrc eq 0.
* Set the category value
        lv_pt_cat = ls_par-low.
        try .
* Check if the category is for off cycle
            data(lo_pi_aux) = cl_pyc_proc_inst_aux=>get_instance( io_transaction = mo_fnd_factory->mo_transaction ).
            if lo_pi_aux->proc_inst_is_oc( iv_proc_templ_cat = lv_pt_cat ) eq abap_true.
* If it is, return true.
              rv_result = abap_true.
            endif.
          catch cx_pyc_frw ##NO_HANDLER.
        endtry.
      endif.
    endif.
  endmethod.


  method parse_exclude_entry.
* Local Data Declarations
    data: lv_sign   type /iwbep/s_cod_select_option-sign,
          lv_option type /iwbep/s_cod_select_option-option.
*Spilt apart the field name from the value
    data(lv_string) = iv_entry.
    split lv_string at '=' into data(lv_field) data(lv_value) in character mode.
* Set the sign to be equals
    lv_sign = 'E'. "'I'.
* Set the option as being either "Does not conatin Pattern" or "Not equal to"
    if lv_value ca '*'.
      lv_option = 'CP'. "'NP'.
    else.
      lv_option = 'EQ'. "'NE'.
    endif.
* Add the value to the field
    case lv_field.
      when 'STAT2'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_stat2.
      when 'BUKRS'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_bukrs.
      when 'PERSA'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_werks.
      when 'BTRTL'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_btrtl.
      when 'PERSG'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_persg.
      when 'PERSK'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_persk.
      when 'SCHKZ'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_schkz.
      when 'ZTERF'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_zterf.
      when 'TRFAR'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_trfar.
      when 'TRFGB'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_trfgb.
      when 'TRFGR'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_trfgr.
      when 'TRFST'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_trfst.
      when 'LGART'.
        if lv_value ca '*'.
          append value #( sign = 'I' option = 'CP' low = lv_value ) to mt_lgart_exclude_filter.
        else.
          append value #( sign = 'I' option = 'EQ' low = lv_value ) to mt_lgart_exclude_filter.
        endif.
      when 'PLANS'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_plans.
      when 'ABRSP'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_abrsp.
      when 'INFTY'.
        mv_infty = lv_value.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_infty.
      when 'SUBTY'.
        mv_subty = lv_value.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_subty.
      when 'INFTY2'.
        mv_infty2 = lv_value.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_infty2.
      when 'SUBTY2'.
        mv_subty2 = lv_value.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_subty2.
      when 'KOSTL'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_kostl.
      when 'RZAWE'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_rzawe.
      when 'TEILK'.
        append value #( sign = lv_sign option = 'EQ' low = lv_value ) to mt_teilk.
      when 'STATU2'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_statu2.
      when 'STATE'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_state.
      when 'VWSAZ'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_vwsaz.
      when 'MASSN'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_massn.
      when 'ANSVH'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_ansvh.
      when 'ORGEH'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_orgeh.
      when others.
    endcase.
  endmethod.


  method parse_include_entry.
* Local Data Declarations
    data: lv_sign   type /iwbep/s_cod_select_option-sign,
          lv_option type /iwbep/s_cod_select_option-option.
* Split the field name from the value
    data(lv_string) = iv_entry.
    split lv_string at '=' into data(lv_field) data(lv_value) in character mode.
* Set the entry to be include
    lv_sign = 'I'.
* Set the option as eith "Contains pattern" or "Equal to"
    if lv_value ca '*'.
      lv_option = 'CP'.
    else.
      lv_option = 'EQ'.
    endif.
* Add the value to the correct range
    case lv_field.
      when 'STAT2'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_stat2.
      when 'BUKRS'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_bukrs.
      when 'PERSA'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_werks.
      when 'BTRTL'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_btrtl.
      when 'PERSG'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_persg.
      when 'PERSK'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_persk.
      when 'SCHKZ'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_schkz.
      when 'ZTERF'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_zterf.
      when 'TRFAR'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_trfar.
      when 'TRFGB'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_trfgb.
      when 'TRFGR'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_trfgr.
      when 'TRFST'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_trfst.
      when 'LGART'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_lgart_include_filter.
      when 'PLANS'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_plans.
      when 'ABRSP'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_abrsp.
      when 'INFTY'.
        mv_infty = lv_value.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_infty.
      when 'SUBTY'.
        mv_subty = lv_value.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_subty.
      when 'INFTY2'.
        mv_infty2 = lv_value.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_infty2.
      when 'SUBTY2'.
        mv_subty2 = lv_value.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_subty2.
      when 'KOSTL'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_kostl.
      when 'RZAWE'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_rzawe.
      when 'TEILK'.
        append value #( sign = lv_sign option = 'NE' low = lv_value ) to mt_teilk.
      when 'STATU2'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_statu2.
      when 'STATE'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_state.
      when 'VWSAZ'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_vwsaz.
      when 'MASSN'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_massn.
      when 'ANSVH'.
        append value #( sign = lv_sign option = lv_option low = lv_value ) to mt_ansvh.
      when others.
    endcase.
  endmethod.


  method sap_pdf_get ##NEEDED.
  endmethod.


  method sap_sfo_get.
* Local Data Declarations
    data lt_sfo type cl_pyd_rd_dto_sfo=>ty_t_rd.

    case iv_access_mode.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        if is_rd-rd->mv_rd_type = 'SAP_SFO'.
          try .
* Get the data, if one cannot be found, or an error is rasied
* Get a default
              call method is_rd-rd->get_data(
                importing
                  et_data = lt_sfo ).
              if lt_sfo is initial.
                lt_sfo = me->get_generic_sfo( ).
              endif.
            catch cx_pyd_fnd.
              lt_sfo = me->get_generic_sfo( ).
          endtry.
* you can do text adjustment here if neccesary
          call method me->adjust_sfo_texts "To be redifined as per customer needs
            changing
              rt_result = lt_sfo.
        endif.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

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
      when others.
    endcase.

    try .
        call method is_rd-rd->set_data( lt_sfo ).
      catch cx_pyd_fnd ##NO_HANDLER.
    endtry.
  endmethod.


  method sap_spo_get ##NEEDED.

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


  method sap_swt_get ##NEEDED.
  endmethod.


  method sap_t_get.
* Local Data Declarations
    data: lt_txt    type cl_pyd_rd_dto_t=>ty_t_rd,
          ls_txt    type cl_pyd_rd_dto_t=>ty_s_rd,
          lv_row_id type pyd_rowid,
          lv_pernr  type p_pernr.
* Type cast the employee number
    lv_pernr  = is_rd-id.
* Get the parameters
    me->get_parameters( exporting it_par         = it_par
                                  io_res_context = io_res_context ).
* Add 1 to the row number
    add 1 to lv_row_id.
* Set the row number, and the text line
    ls_txt-row_id = lv_row_id.
    ls_txt-text   = text-006 && ':' && lv_pernr.
* Add it to the output table
    append ls_txt to lt_txt.
* Add 1 to the row number
    add 1 to lv_row_id.
* Set the row number, and the text line
    ls_txt-row_id = lv_row_id.
    ls_txt-text   = text-003 && ':' && mv_payroll_area.
* Add it to the output table
    append ls_txt to lt_txt.
* Add the text table to the result data type
    try .
        call method is_rd-rd->set_data( lt_txt ).
      catch cx_pyd_fnd ##NO_HANDLER.
    endtry.
  endmethod.


  method select_cats_records.
* Clear out the exporting variables
    clear: et_cats_2000_info, et_cats_2010_info.
* Get the entries from the global tables
    et_cats_2000_info = filter #( mt_enriched_cats_2000_info where pernr = iv_pernr ).
    et_cats_2010_info = filter #( mt_enriched_cats_2010_info where pernr = iv_pernr ).
* If results have been found, return
    if et_cats_2000_info is not initial or et_cats_2010_info is not initial.
      return.
    endif.
* Otherwise select again
* For Absences & Attendances
    select ptex2000~pernr, ptex2000~begda as datum, ptex2000~awart, t554t~atext,
           ptex2000~stdaz, ptex2000~beguz, ptex2000~enduz, ptex2000~plans into corresponding fields of table @et_cats_2000_info from ptex2000
      inner join pa0001 on pa0001~pernr eq ptex2000~pernr
      inner join pa0000 on pa0000~pernr eq ptex2000~pernr
      inner join t001p  on t001p~werks  eq pa0001~werks and "#EC CI_BUFFJOIN
                           t001p~btrtl  eq pa0001~btrtl
      inner join t554t  on t554t~awart  eq ptex2000~awart and
                           t554t~moabw  eq t001p~moabw    and
                           t554t~sprsl  eq @sy-langu
      where ptex2000~pernr  eq @iv_pernr         and
            ptex2000~endda  le @mv_endda         and
            ptex2000~statu3 in @mt_pwstat2       and
            pa0000~begda    le @mv_endda         and
            pa0000~endda    ge @mv_begda         and
            pa0000~stat2    in @mt_stat2         and
            pa0001~begda    le @mv_endda         and
            pa0001~endda    ge @mv_begda         and
            pa0001~bukrs    in @mt_bukrs         and
            pa0001~abkrs    in @mt_payroll_areas and
            pa0001~werks    in @mt_werks         and
            pa0001~persg    in @mt_persg         and
            pa0001~persk    in @mt_persk
      order by ptex2000~pernr.
* For Wage Types
    select ptex2010~pernr, ptex2010~begda as datum, ptex2010~lgart, t512t~lgtxt,
           ptex2010~stdaz into corresponding fields of table @et_cats_2010_info from ptex2010
      inner join pa0001 on pa0001~pernr eq ptex2010~pernr
      inner join pa0000 on pa0000~pernr eq ptex2010~pernr
      inner join t512t  on t512t~lgart  eq ptex2010~lgart and "#EC CI_BUFFJOIN
                           t512t~molga  eq @mc_molga      and
                           t512t~sprsl  eq @sy-langu
      where ptex2010~pernr  eq @iv_pernr         and
            ptex2010~begda  le @mv_endda         and
            ptex2010~statu3 in @mt_pwstat2       and
            pa0000~begda    le @mv_endda         and
            pa0000~endda    ge @mv_begda         and
            pa0000~stat2    in @mt_stat2         and
            pa0001~begda    le @mv_endda         and
            pa0001~endda    ge @mv_begda         and
            pa0001~bukrs    in @mt_bukrs         and
            pa0001~abkrs    in @mt_payroll_areas and
            pa0001~werks    in @mt_werks         and
            pa0001~persg    in @mt_persg         and
            pa0001~persk    in @mt_persk
      order by ptex2010~pernr.
  endmethod.


  method set_context.
    me->mo_context = io_context.
  endmethod.


  method set_select_option.
* Add the entry to the select options
    append value #( sign = iv_sign option = iv_option low = iv_value ) to ch_so.
  endmethod.


  method set_stat2_default.
* If the employee status range is empty, set it to default on active employees
    if mt_stat2 is initial.
      append value #( sign = 'I' option = 'EQ' low = zcl_constants=>c_stat2_3 ) to mt_stat2.
    endif.
  endmethod.


  method yk_audit_report.
    data: lt_txt          type cl_pyd_rd_dto_t=>ty_t_rd,
          ls_txt          type cl_pyd_rd_dto_t=>ty_s_rd,
          ls_par          type line of           if_pyd_fnd_types=>ty_t_resp,
          lv_abkrs        type                   abkrs, "Payroll Area
          lv_period(6),
          lv_row_id       type pyd_rowid,
          lv_name_text    type string,
          lv_pernr        type p_pernr,
          lv_size         type i,
          lo_payroll_area type ref to cl_hr_payroll_area,
          lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_begda        type begda,
          lv_endda        type endda ##NEEDED,
          lv_prev_pabrj   type pabrj,
          lv_prev_pabrp   type pabrp,
          lv_subrc        type sy-subrc ##NEEDED,
          lv_datum        type string,
          lv_time         type string,
          ls_prev_period  type pc2paper,
          lv_prev_endda   type datum,
          ls_t569u        type t569u.

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

    select * from t569u into ls_t569u up to 1 rows where abkrs = lv_abkrs and state = '3' and vwsaz = '01' and pabrj = lv_prev_pabrj and pabrp = lv_prev_pabrp. endselect.

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
      ls_txt-text   = |No Master Data changes during pay period|.
      append ls_txt to lt_txt.
    else.
      ls_txt-text   = |Infotype{ c_tab }Date{ c_tab }Time{ c_tab }User| .  "'This is the first line of the text.'.
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

        select adrp~name_text into lv_name_text
          up to 1 rows
          from usr21 join adrp on usr21~persnumber = adrp~persnumber and
                                  adrp~date_from   = '00010101'      and
                                  adrp~nation      = ''
          where usr21~bname = ls_doc_key_tab-uname.
        endselect.

        ls_txt-text   = ls_doc_key_tab-infty && c_tab && lv_datum && c_tab && lv_time && c_tab && lv_name_text.
        append ls_txt to lt_txt.
      endloop.
    endif.

    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_txt.
      catch cx_pyd_fnd ##NO_HANDLER.
    endtry.
  endmethod.


  method yk_cats_info.
* Local Data Declarations
    data:
      lt_gov            type cl_pyd_rd_dto_gov=>ty_t_rd,
      ls_gov            type cl_pyd_rd_dto_gov=>ty_s_rd,
      lt_cats_2000_info type ty_t_cats_2000_info,
      lt_cats_2010_info type ty_t_cats_2010_info,
      lv_pernr          type pernr_d,
      lv_rowid          type i,
      lv_groupid        type i.
* Type cast the employee number
    lv_pernr = is_rd-id.
* Select the CATS records for the employee
    me->select_cats_records( exporting iv_pernr          = lv_pernr
                             importing et_cats_2000_info = lt_cats_2000_info
                                       et_cats_2010_info = lt_cats_2010_info ).
* Output the 2001/2002 entries
    loop at lt_cats_2000_info into data(ls_2000_result).
      add 1 to: lv_rowid, lv_groupid.
      ls_gov-groupid    = lv_groupid.
      ls_gov-group_name = |Absence/Attendance Type { ls_2000_result-awart } ( { ls_2000_result-atext } ) on { ls_2000_result-datum date = user }|.
      ls_gov-row_id     = lv_rowid.
      ls_gov-itemid     = 'STDAZ'.
      ls_gov-text       = |Hours|.
      ls_gov-value      = ls_2000_result-stdaz.
      append ls_gov to lt_gov.
      if not ( ( ls_2000_result-beguz is initial or ls_2000_result-beguz eq '      ' ) and
               ( ls_2000_result-enduz is initial or ls_2000_result-enduz eq '      ' ) ).
        lv_rowid = lv_rowid + 1.
        ls_gov-row_id     = lv_rowid.
        ls_gov-itemid     = 'BEGUZ'.
        ls_gov-text       = |Begin Time|.
        ls_gov-value      = |{ ls_2000_result-beguz time = user }|.
        append ls_gov to lt_gov.
        lv_rowid = lv_rowid + 1.
        ls_gov-row_id     = lv_rowid.
        ls_gov-itemid     = 'ENDUZ'.
        ls_gov-text       = |End Time|.
        ls_gov-value      = |{ ls_2000_result-enduz time = user }|.
        append ls_gov to lt_gov.
      endif.
      if ls_2000_result-plans is not initial.
        lv_rowid = lv_rowid + 1.
        ls_gov-row_id     = lv_rowid.
        ls_gov-itemid     = 'PLANS'.
        ls_gov-text       = |Higher Grade Position|.
        ls_gov-value      = |{ ls_2000_result-plans }|.
        append ls_gov to lt_gov.
      endif.
    endloop.
* Output the 2010 entries
    loop at lt_cats_2010_info into data(ls_2010_result).
      add 1 to: lv_rowid, lv_groupid.
      ls_gov-groupid    = lv_groupid.
      ls_gov-group_name = |Wage Type { ls_2010_result-lgart } ( { ls_2010_result-lgtxt } ) on { ls_2010_result-datum date = user }|.
      ls_gov-row_id     = lv_rowid.
      ls_gov-itemid     = 'STDAZ'.
      ls_gov-text       = |Hours|.
      ls_gov-value      = ls_2010_result-stdaz.
      append ls_gov to lt_gov.
    endloop.
* Set the Data to the result type
    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_gov.
      catch cx_pyd_fnd ##NO_HANDLER.
    endtry.
  endmethod.


  method yk_emp_info.
    data:
      lt_gov        type                   cl_pyd_rd_dto_gov=>ty_t_rd,
      ls_gov        type                   cl_pyd_rd_dto_gov=>ty_s_rd,
      lv_pernr      type                   pernr_d,
      lt_p0001      type standard table of p0001 with non-unique key pskey,
      ls_p0001      type                   p0001,
      lt_p0006      type standard table of p0006 with non-unique key pskey,
      ls_p0006      type                   p0006,
      lt_p0105      type standard table of p0105 with non-unique key pskey,
      ls_p0105      type                   p0105,
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
        others      = 0.

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
    loop at lt_p0006 into ls_p0006 where subty = '0001'.
      exit.
    endloop.
    if sy-subrc <> 0.
      clear ls_p0006.
    endif.

    "---------------------------------------------------------------------------------"
    "Telephone Number - update to the relevant field for your country
    ls_gov-row_id = '108'.
    ls_gov-itemid = 'TELNO'.
    ls_gov-text   = text-020.
    ls_gov-value  = ls_p0006-telnr.

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
      exit.
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
      catch cx_pyd_fnd ##NO_HANDLER.
    endtry.
  endmethod.


  method yk_payroll_log.
* Local Data Declarations
    data: lt_gov   type cl_pyd_rd_dto_gov=>ty_t_rd,
          ls_gov   type cl_pyd_rd_dto_gov=>ty_s_rd,
          lv_pernr type pernr_d,
          lt_lines type table of text60,    "Note 2707759
          lv_seqnr type pyd_rowid.          "Note 2707759

    constants: lc_wordwrap type i value 60.
* Type cast the employee number
    lv_pernr = is_rd-id.
* Select the messages from the message table
    select iaper, seqnr, as_text, text1
      into table @data(lt_pyc_d_py_msg)
      from pyc_d_py_msg
      where abkrs    eq @mv_payroll_area
        and iaper    eq @mv_payroll_period
        and pernr    eq @lv_pernr
        and test_res eq @mv_tpy_res.

    loop at lt_pyc_d_py_msg into data(ls_payroll_log).
* Clear the table of lines
      clear lt_lines.                                         "Note 2707759
* Wordwrap the message at 60 characters
      call function 'RKD_WORD_WRAP'                           "Note 2707759
        exporting
          textline  = ls_payroll_log-text1
          outputlen = lc_wordwrap
        tables
          out_lines = lt_lines.
* Write out the sequence number
      if ls_payroll_log-seqnr = '01'.                         "Note 2707759
        lv_seqnr = ls_payroll_log-seqnr.
      endif.
* Loop at the table lines and output to the result data type
      loop at lt_lines assigning field-symbol(<lv_text1>).    "Note 2707759
        ls_gov-groupid    = '1'.
        ls_gov-group_name = |Messages|.
        ls_gov-row_id     = lv_seqnr.                         "Note 2707759
        ls_gov-itemid     = 'ABKRS'.
        ls_gov-text       = ls_payroll_log-as_text.
        ls_gov-value      = <lv_text1>.                       "Note 2707759
        append ls_gov to lt_gov.
        lv_seqnr = lv_seqnr + 1.                              "Note 2707759
      endloop.
    endloop.
* Sort the results, and set to the result data type
    sort lt_gov by row_id.
    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_gov.
      catch cx_pyd_fnd ##NO_HANDLER.
    endtry.
  endmethod.


  method yk_payslip.
* Local Data Declarations
    data: lo_payslip_helper type ref to cl_hrxss_rem_helper,
          lt_stream         type cl_pyc_rd_dto_stream=>ty_t_rd,
          ls_pmehf          type pmehf,
          ls_filtered_rgdir type pc261,
          lv_form           type hrf_name,
          lv_pdf_size       type i ##NEEDED,
          lv_pdf_doc        type xstring,
          lv_pernr          type p_pernr.
* Get hte employee number
    lv_pernr  = is_rd-id.
* Get the payslip helper
    create object lo_payslip_helper
      exporting
        iv_pernr = lv_pernr.
* Filter out the results
    data(lt_filtered_rgdir) = lo_payslip_helper->get_filtered_rgdir( ).
    delete lt_filtered_rgdir where iabkrs ne mv_payroll_area or inper ne mv_payroll_period or payty  ne ''.
    read table lt_filtered_rgdir into ls_filtered_rgdir index 1.
* If one is found
    if sy-subrc eq 0.
      ls_pmehf-rclas = 'CESS'.
      ls_pmehf-molga = lo_payslip_helper->get_country_code( ).
      ls_pmehf-uname = sy-uname.
* Get the form name for ESS Payslip
      try.
          cl_hrpa_feature=>get_value( exporting feature       = 'HRFOR'
                                                struc_content = ls_pmehf
                                      importing return_value  = lv_form ).
        catch cx_hrpa_violated_assertion ##NO_HANDLER.
      endtry.
* Get the output for the payslip
      lo_payslip_helper->get_payslip( exporting  is_rgdir                   = ls_filtered_rgdir
                                                 iv_form_name               = lv_form
                                      importing  ev_document                = lv_pdf_doc
                                                 ev_doc_size                = lv_pdf_size
                                      exceptions ex_payslip_creation_failed = 1 ).
* Set the output as a PDF document
      if sy-subrc eq 0.
        append value #( mime_type = |application/pdf| row_id = '001' value = lv_pdf_doc ) to lt_stream.
      endif.
* Add the output to the result detail type
      try.
          call method is_rd-rd->set_data
            exporting
              it_data = lt_stream.
        catch cx_pyd_fnd ##NO_HANDLER.
      endtry.
    endif.
  endmethod.


  method yk_wage_type_report.
* Local Data Declarations
    data: ls_swt    type cl_pyd_rd_dto_swt=>ty_s_rd,
          lt_swt    type cl_pyd_rd_dto_swt=>ty_t_rd,
          lv_row_id type pyd_rowid,
          lv_pernr  type p_pernr.
* Type cast the employee number
    lv_pernr  = is_rd-id.
* Populat the list of WTs you want to show
    data(lt_sel_lgart) = value /iwbep/t_cod_select_options( ( sign = 'I' option = 'EQ' low = '/101' )
                                                            ( sign = 'I' option = 'EQ' low = '/106' )
                                                            ( sign = 'I' option = 'EQ' low = '/110' )
                                                            ( sign = 'I' option = 'EQ' low = '/116' )
                                                            ( sign = 'I' option = 'EQ' low = '/142' )
                                                            ( sign = 'I' option = 'EQ' low = '/172' )
                                                            ( sign = 'I' option = 'EQ' low = '/559' )
                                                            ( sign = 'I' option = 'BT' low = '/401' high = '/4ZZ' )
                                                            ( sign = 'I' option = 'BT' low = '0000' high = 'ZZZZ' ) ).
* Select from either the test payroll results, or the actual paytoll results
    if mv_tpy_res = abap_true.
* WTs for IN period results
      select hrdct_tpy_rgdir~fpper as groupid, hrdct_tpy_rgdir~fpper as group_name, p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe, t512t~lgtxt as lgtxt,
        sum( case hrdct_tpy_rgdir~srtza when 'P' then p2rx_rt~anzhl * -1 else p2rx_rt~anzhl end ) as anzhl,
        sum( case hrdct_tpy_rgdir~srtza when 'P' then betrg * -1 else betrg end ) as betrg
        into corresponding fields of table @et_rt ##TOO_MANY_ITAB_FIELDS
        from p2rx_rt inner join hrdct_tpy_rgdir on hrdct_tpy_rgdir~dct_pernr eq p2rx_rt~dct_pernr and
                                                   hrdct_tpy_rgdir~dct_seqnr eq p2rx_rt~dct_seqnr
                     inner join t512t           on t512t~sprsl = @sy-langu and
                                                   t512t~molga = @mc_molga and
                                                   t512t~lgart = p2rx_rt~lgart
        where hrdct_tpy_rgdir~abkrs     eq @mv_payroll_area
          and hrdct_tpy_rgdir~dct_pernr eq @lv_pernr
          and hrdct_tpy_rgdir~inper     eq @mv_payroll_period
          and p2rx_rt~lgart             in @lt_sel_lgart
        group by hrdct_tpy_rgdir~fpper, p2rx_rt~lgart, p2rx_rt~betpe, t512t~lgtxt . "#EC CI_BUFFJOIN
    else.
      select p2rx_eval_period~fpper as groupid, p2rx_eval_period~fpper as group_name, p2rx_rt~lgart as lgart, p2rx_rt~betpe as betpe, t512t~lgtxt as lgtxt,
        sum( case p2rx_eval_period~srtza when 'P' then p2rx_rt~anzhl * -1 else p2rx_rt~anzhl end ) as anzhl,
        sum( case p2rx_eval_period~srtza when 'P' then betrg * -1 else betrg end ) as betrg
        into corresponding fields of table @et_rt ##TOO_MANY_ITAB_FIELDS
        from p2rx_rt inner join p2rx_eval_period on p2rx_eval_period~dct_pernr eq p2rx_rt~dct_pernr and
                                                    p2rx_eval_period~dct_seqnr eq p2rx_rt~dct_seqnr
                     inner join t512t            on t512t~sprsl = @sy-langu and
                                                    t512t~molga = @mc_molga and
                                                    t512t~lgart = p2rx_rt~lgart
        where p2rx_eval_period~abkrs     eq @mv_payroll_area
          and p2rx_eval_period~dct_pernr eq @lv_pernr
          and p2rx_eval_period~inper     eq @mv_payroll_period
          and p2rx_rt~lgart              in @lt_sel_lgart
        group by p2rx_eval_period~fpper, p2rx_rt~lgart, p2rx_rt~betpe, t512t~lgtxt . "#EC CI_BUFFJOIN
    endif.
* Remove any entries where both the number and the amount are equal to 0
    delete et_rt where betrg eq 0 and anzhl eq 0.
* Output the payroll results to the result data type
    loop at et_rt into data(ls_rt).
      ls_swt-group_name = |FOR Period { ls_rt-group_name+4(2) }/{ ls_rt-group_name(4) }|.
      ls_swt-groupid    = ls_rt-group_name.
      ls_swt-row_id     = lv_row_id.
      ls_swt-itemid     = ls_rt-lgart.
      ls_swt-text       = ls_rt-lgtxt.
      ls_swt-amt        = ls_rt-betrg.
      ls_swt-num        = ls_rt-anzhl.
      ls_swt-rte        = ls_rt-betpe.
      ls_swt-amt_curr   = mv_curr.
      add 1 to lv_row_id .
      append ls_swt to lt_swt.
    endloop.

    try.
        call method is_rd-rd->set_data
          exporting
            it_data = lt_swt.
      catch cx_pyd_fnd ##NO_HANDLER.
    endtry.
  endmethod.
ENDCLASS.
