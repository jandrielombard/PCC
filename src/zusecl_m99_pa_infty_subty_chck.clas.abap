class ZUSECL_M99_PA_INFTY_SUBTY_CHCK definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.

    types:
      begin of ty_infty_dtls,
        subty type subty,
        begda type begda,
        endda type endda,
        uname type uname.
    types: end of ty_infty_dtls .
    types:
      begin of ty_infty,
        pernr type pernr_d,
        subty type subty,
        begda type begda,
        endda type endda,
        uname type uname,
        text  type ltext.
    types: end of ty_infty .
    types:
      tty_infty type table of ty_infty .

    constants mc_check_exists type pyd_par_type value 'Z99_CHECK_EXISTS' ##NO_TEXT.
    constants mc_check_message type pyd_par_type value 'Z99_CHECK_MESSAGE' ##NO_TEXT.
    constants mc_itemid_itystychk type pyd_itemid value 'ITYSTYCHK'.
    data mv_check_exists type xfeld.
    data mt_check_message type /iwbep/t_cod_select_options .
    data mt_infty type tty_infty .
    data ms_infty type ty_infty .
    data ms_infty_dtls type ty_infty_dtls .


    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_INFTY_SUBTY_CHCK IMPLEMENTATION.


  method CHECK.
*
    types: begin of ty_infty_data,
             pernr type pernr_d,
             subty type subty,
             begda type begda,
             endda type endda,
             uname type uname.
    types: end of ty_infty_data.
    data: lt_infty type standard table of ty_infty_data.
    data: ls_infty type ty_infty_data.
    data: lt_all_pernr type tty_pernr.
    data: lv_table type tablename.
    data: ls_result  type   ty_s_result.

* Determine DB Table for infotype
    select single dbtab into lv_table from  t777d
           where  infty  = mv_infty.

* All relevant employees with it2001 Records
    select itinfty~pernr, itinfty~subty, itinfty~begda, itinfty~endda, itinfty~uname
      into corresponding fields of table @lt_infty from (lv_table) as itinfty
        where itinfty~pernr in @it_pernr_so
          and itinfty~subty in @mt_subty
          and itinfty~sprps in @mt_sprps
          and itinfty~uname in @mt_uname
          and itinfty~begda <= @mv_endda
          and itinfty~endda >= @mv_begda
          and exists ( select 1
                          from pa0000 as it0000 inner join pa0001 as it0001 on
                               it0000~pernr = it0001~pernr
                         where it0000~pernr = itinfty~pernr
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

    sort lt_infty. delete adjacent duplicates from lt_infty.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    if mv_check_exists eq abap_true.
      sort lt_infty. delete adjacent duplicates from lt_infty comparing pernr.
      loop at lt_infty into ls_infty.
        ls_result-id = ls_infty-pernr.
        insert ls_result into table rt_result.

* Collect data for Overview List
        move-corresponding ls_infty to ms_infty.
        append ms_infty to mt_infty.
      endloop.
    else.
      select it0000~pernr as dct_pernr
        into corresponding fields of table @lt_all_pernr
        from pa0000 as it0000 inner join pa0001 as it0001 on
             it0000~pernr = it0001~pernr
       where it0000~pernr in @it_pernr_so
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
         and it0001~kostl in @mt_kostl.
* Check the Non Existence of Record for the Employee
      sort lt_all_pernr by dct_pernr.
      delete adjacent duplicates from lt_all_pernr comparing dct_pernr.
      loop at lt_all_pernr into data(ls_pernr).
        read table lt_infty into ls_infty with key pernr = ls_pernr-dct_pernr.
        if sy-subrc <> 0.
          ls_result-id = ls_pernr-dct_pernr.
          insert ls_result into table rt_result.
        endif.
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
      lv_pernr  type p_pernr.

    data: lv_value type pyd_name.
    data: lv_char_value  type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_text type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at mt_check_message into data(ls_check_message).
          if lv_value is initial.
            move ls_check_message-low to lv_value.
          else.
            concatenate lv_value ls_check_message-low into lv_value separated by space.
          endif.
        endloop.

* Populate SFO tab for Display
        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

          clear ls_err_ov-sfo_tab.
          clear ls_sfo.

          lv_text = text-001.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_itystychk
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  method GET_SPECIFC_CUSTMIZING.

* Read Custom Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.

    try.
* Read Z99_CHEK_EXISTS
        me->mv_check_exists = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_check_exists
                                                                    it_par = me->get_context( )->mt_par ).
        case me->mv_check_exists.
          when 'Y'.
            mv_check_exists = abap_true.
          when others.
            mv_check_exists = abap_false.
        endcase.

* Read Message Text
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_check_message
          changing
            ct_parameter_tab = mt_check_message.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.


  endmethod.
ENDCLASS.
