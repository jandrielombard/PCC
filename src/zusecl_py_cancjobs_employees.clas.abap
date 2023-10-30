class ZUSECL_PY_CANCJOBS_EMPLOYEES definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.

  types:
    begin of ty_cancjob_dtls,
        jobname  type tbtcjob-jobname,
        strtdate type tbtcjob-strtdate,
        strttime type tbtcjob-strttime.
    types: end of ty_cancjob_dtls .
  protected section.

    data mc_itemid_cancjob type pyd_itemid value 'CANCJOB' ##NO_TEXT.
    data mt_cancjob_data type zusecl_m99_pcc_ehiupdate=>tty_emplist.
    data ms_cancjob_data type zusecl_m99_pcc_ehiupdate=>ty_emplist.
    data ms_cancjob_dtls type ty_cancjob_dtls.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.


ENDCLASS.



CLASS ZUSECL_PY_CANCJOBS_EMPLOYEES IMPLEMENTATION.


  method CHECK.
* Cancelled jobs Employees list
    data: lt_pernr      type table of pernr_d.
    data: ls_result     type ty_s_result.

    data lt_emp_so type hr99s_pernr_range.
    data lv_procid type pyc_d_pyp-id.
    field-symbols <lt_emp_data> type any table.
    data lo_emp_data            type ref to data.

* Get Process ID
    lv_procid =
     cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = cl_pyc_rt_facade=>gcs_par_type-proc
                                           it_par      = it_par ).
* Employee Selection
    move-corresponding it_pernr_so to lt_emp_so.

* Initialize
    cl_salv_bs_runtime_info=>set(
      exporting display  = abap_false
                metadata = abap_false
                data     = abap_true ).

* Submit Event Handler Update program for Employee LIst
    submit zhraurepy_pcc_ehiupd00
      with p_procid eq lv_procid
      with p_pccval eq abap_true
      with s_emp_so eq lt_emp_so
    and return.

* Collect the Employee LIst
    try.
        cl_salv_bs_runtime_info=>get_data_ref(
          importing r_data = lo_emp_data ).
        assign lo_emp_data->* to <lt_emp_data>.
      catch cx_salv_bs_sc_runtime_info.
        message e051(zhrpy_pcc_msg).
    endtry.

    cl_salv_bs_runtime_info=>clear_all( ).

    if <lt_emp_data> is assigned.
      move-corresponding <lt_emp_data> to mt_cancjob_data keeping target lines.
      loop at <lt_emp_data> assigning field-symbol(<ls_emp_data>).
        assign component if_pyd_cont_types=>gcs_par_type-pernr
           of structure <ls_emp_data> to field-symbol(<lv_pernr>).
        append <lv_pernr> to lt_pernr.
      endloop.
    endif.

* Build Result Table
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
      ls_err_ov       type ty_s_err_ov,
      ls_sfo          type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_pernr        type p_pernr,
      lv_value        type char060,
      lt_sfo_tab_temp like ls_err_ov-sfo_tab,
      lv_text         type cl_pyd_rd_dto_sfo=>ty_s_rd-text,
      lv_value_string type string.
    data lt_pay_runtime	type zusecl_m99_pcc_utilities=>tty_payrun_time.
    data ls_pay_runtime	type zusecl_m99_pcc_utilities=>ty_payrun_time.

    case iv_access_mode.
        "Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          " move Cancelled Job records to temporary table
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab_temp = ls_err_ov-sfo_tab.
          clear: ls_err_ov-sfo_tab.

          loop at lt_sfo_tab_temp into data(ls_sfo_tab_temp).
            if ls_sfo_tab_temp-itemid eq mc_itemid_cancjob.
              clear: ms_cancjob_dtls, lv_value.
              lv_value = ls_sfo_tab_temp-value.
              call method me->copy_structure_to_other
                exporting
                  p_struct1 = lv_value
                changing
                  p_struct2 = ms_cancjob_dtls.

              "Generic Message
              clear ls_err_ov-sfo_tab.
              lv_value_string = | { text-002 } { text-003 } |.
              lv_text = text-001.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_cancjob
                  iv_text                     = lv_text
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.

              " Job Name
              message i057(zhrpy_pcc_msg)
                 with ms_cancjob_dtls-jobname
                 into lv_value_string.
              lv_text = text-004.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_cancjob
                  iv_text                     = lv_text
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.

              " Job Details
              message i058(zhrpy_pcc_msg)
                 with ms_cancjob_dtls-strtdate
                      ms_cancjob_dtls-strttime
                 into lv_value_string.
              lv_text = text-006.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_cancjob
                  iv_text                     = lv_text
                  iv_value                    = lv_value_string
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.

              " Payroll Calculation Run time
              call method zusecl_m99_pcc_utilities=>get_pernr_payroll_runtime
                exporting
                  iv_pernr       = lv_pernr
                  io_res_context = io_res_context
                importing
                  et_pay_runtime = lt_pay_runtime.
              read table lt_pay_runtime into ls_pay_runtime index 1.

              if not ls_pay_runtime-rundt is initial.
                message i058(zusehrpy_pcc_msg)
                 with ls_pay_runtime-rundt ls_pay_runtime-runtm into lv_value_string.
              else.
                message i059(zusehrpy_pcc_msg) into lv_value_string.
              endif.
              lv_text = text-005.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_cancjob
                  iv_text                     = lv_text
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

        " Execution Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_cancjob_data into ms_cancjob_data
              where pernr = lv_pernr.

            clear: lv_value.
            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_cancjob.
            move-corresponding ms_cancjob_data to ms_cancjob_dtls.

            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_cancjob_dtls
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
ENDCLASS.
