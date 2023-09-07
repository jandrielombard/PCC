class zcl_m99_py_payroll_log definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.

    methods read_check_data
      importing
        !it_pernr type hr99s_pernr_range .
  protected section.

    types:
      begin of ty_payroll_msg,
        pernr   type pernr_d,
        seqnr   type pyd_seqnr,
        as_text type char40,
        text1   type plog_txt,
        tlevel  type seu_level,
      end of ty_payroll_msg .
    types:
      tty_payroll_msg type table of ty_payroll_msg .
    types:
      ty_t_msg_dtl type standard table of pyc_d_py_msg .
    types ty_p2rq_messages type p2rq_messages .
    types:
      tty_p2rq_messages type standard table of ty_p2rq_messages .

    constants mc_filter_msgty type pyd_par_type value 'Z99_FILTER_MSGTY' ##NO_TEXT.
    constants mc_filter_msgno type pyd_par_type value 'Z99_FILTER_MSGNO' ##NO_TEXT.
    constants mc_exc_filter_msgty type pyd_par_type value 'Z99_EXC_FILTER_MSGTY' ##NO_TEXT.
    constants mc_exc_filter_msgno type pyd_par_type value 'Z99_EXC_FILTER_MSGNO' ##NO_TEXT.
    constants mc_filter_message type pyd_par_type value 'Z99_FILTER_MESSAGE' ##NO_TEXT.
    constants mc_exc_filter_message type pyd_par_type value 'Z99_EXC_FILTER_MESSAGE' ##NO_TEXT.
    constants mc_filter_p2rq_msg type pyd_par_type value 'Z99_FILTER_P2RQ_MSG' ##NO_TEXT.
    constants mc_itemid_pymsg type pyd_itemid value 'PYMSG' ##NO_TEXT.
    constants mc_mclas_standard type pc23n-mclas value 'STANDARD' ##NO_TEXT.
    constants mc_msgno_0 type pc23n-msgno value '0' ##NO_TEXT.
    constants mc_tlevel_00 type seu_level value '00' ##NO_TEXT.
    constants mc_msg_separator type char2 value '->' ##NO_TEXT.
    constants mc_separator_slash type char1 value '/' ##NO_TEXT.
    data mt_filter_msgty type /iwbep/t_cod_select_options .
    data mt_filter_msgno type /iwbep/t_cod_select_options .
    data mt_filter_message type /iwbep/t_cod_select_options .
    data mt_payroll_msg type tty_payroll_msg .
    data ms_payroll_msg type ty_payroll_msg .
    data mt_filter_p2rq_msg type /iwbep/t_cod_select_options .
    data mt_all_p2rq_messages type tty_p2rq_messages .
    data mt_bat_p2rq_messages type tty_p2rq_messages .
    data mt_filter_p2rq_msgno type /iwbep/t_cod_select_options .
    data mt_filter_p2rq_mclas type /iwbep/t_cod_select_options .

    methods get_msg_detail
      importing
        !io_res_context type ref to if_pyd_res_context
        !iv_pernr       type p_pernr
      exporting
        !et_msg_detail  type ty_t_msg_dtl .
    methods get_p2rq_messages
      importing
        !is_ty                  type pyd_d_ty
        !is_inst                type ty_s_inst
        !it_par                 type if_pyd_fnd_types=>ty_t_resp
        !io_res_context         type ref to if_pyd_res_context
        !it_par_addl            type if_pyd_ty_rt=>ty_t_result_key
        !it_pernr_so            type /iwbep/t_cod_select_options
      exporting
        value(et_p2rq_messages) type tty_p2rq_messages .
    methods merge_pymsg_and_p2rq_msg
      importing
        !it_p2rq_messages type tty_p2rq_messages
      changing
        !ct_payroll_msg   type tty_payroll_msg .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
    methods get_specifc_custmizing
        redefinition .
    methods yk_payroll_log
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PY_PAYROLL_LOG IMPLEMENTATION.


  method check.
* Employees from Payroll Corrections (Match Code W) table
    data lt_payroll_msg   type tty_payroll_msg.
    data ls_payroll_msg   type ty_payroll_msg.
    data ls_result  type  ty_s_result.

* Read Messages from P2RQ_MESSAGES Table
    call method me->get_p2rq_messages
      exporting
        is_ty            = is_ty
        is_inst          = is_inst
        it_par           = it_par
        io_res_context   = io_res_context
        it_par_addl      = it_par_addl
        it_pernr_so      = it_pernr_so
      importing
        et_p2rq_messages = mt_all_p2rq_messages.

* Find messages in payroll log
    select tbl_py_msg~pernr, tbl_py_msg~seqnr as seqnr,
           tbl_py_msg~as_text as as_text, tbl_py_msg~text1 as text1, tbl_py_msg~tlevel
      into corresponding fields of table @lt_payroll_msg
     from pyc_d_py_msg as tbl_py_msg
       inner join pa0001 as it01 on tbl_py_msg~pernr = it01~pernr
     where tbl_py_msg~pernr in @it_pernr_so and
           tbl_py_msg~abkrs in @mt_payroll_areas and
           tbl_py_msg~iaper = @mv_payroll_period and
           tbl_py_msg~test_res = @mv_tpy_res and
           tbl_py_msg~msgty in @mt_filter_msgty and
           tbl_py_msg~msgno in @mt_filter_msgno and
           tbl_py_msg~message in @mt_filter_message and
           it01~bukrs in @mt_bukrs and
           it01~werks in @mt_werks and
           it01~persg in @mt_persg and
           it01~persk in @mt_persk.

* Append P2RQ_MESSAGES to the LT_PAYROLL_MESSAGES
    call method me->merge_pymsg_and_p2rq_msg
      exporting
        it_p2rq_messages = mt_all_p2rq_messages
      changing
        ct_payroll_msg   = lt_payroll_msg.

* Prepare final result
    sort lt_payroll_msg by pernr seqnr tlevel.
    delete adjacent duplicates from lt_payroll_msg.
    mt_payroll_msg[] = lt_payroll_msg[].

    delete adjacent duplicates from lt_payroll_msg comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_payroll_msg into ls_payroll_msg.
      ls_result-id = ls_payroll_msg-pernr.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.


  method err_ov_get_list.
* Method for Overview list display
    data:
      lv_text   type pyd_name,
      lv_value  type string,
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr.

    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.

    case iv_access_mode.

      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
          if ls_err_ov-sfo_tab is initial.
            clear ls_err_ov-sfo_tab.
            clear ls_sfo.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_pymsg.
            ls_sfo-value  = text-001.
            ls_sfo-text = text-002.
            insert ls_sfo into table ls_err_ov-sfo_tab.
            modify ct_err_ov from ls_err_ov.
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

          lv_text = text-002.
          clear: lv_value.
          loop at mt_payroll_msg into ms_payroll_msg
              where pernr = lv_pernr.

            if ms_payroll_msg-tlevel eq '00' and not lv_value is initial.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_pymsg
                  iv_text                     = lv_text
                  iv_value                    = lv_value
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.

              clear: lv_value.
            endif.

            if lv_value is initial.
              move ms_payroll_msg-text1 to lv_value.
            else.
              concatenate lv_value ms_payroll_msg-text1
                into lv_value separated by space.
            endif.
          endloop.

* Last Message
          if not lv_value is initial.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_pymsg
                iv_text                     = lv_text
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endif.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.


  endmethod.


  method get_msg_detail.

    clear et_msg_detail.
    select * into table et_msg_detail from pyc_d_py_msg " get PERNR list from msg table, only E type PERNR
        where iabkr    in mt_payroll_areas and
              iaper    = mv_payroll_period and
              inpty    = space and
              inpid    = space and
              message  = 'E' and
              pernr    = iv_pernr and
              test_res = mv_tpy_res.                   "#EC CI_NOFIRST.

  endmethod.


  method get_p2rq_messages.
* performing the actual check
    data: lt_all_p2rq_messages   type tty_p2rq_messages.
    data: lt_pernr type tty_pernr.
    data: lv_abs_objname  type abap_abstypename.
    data: lv_objname  type seoclsname.

    refresh mt_objdata.
* Read Employee Numbers
    if mv_tpy_res is not initial.  "Test Payroll
      select hrdct_tpy_rgdir~dct_pernr as dct_pernr
        into corresponding fields of table @lt_pernr
        from hrdct_tpy_rgdir
       where hrdct_tpy_rgdir~dct_pernr in @it_pernr_so
         and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
         and hrdct_tpy_rgdir~inper = @mv_payroll_period
        %_hints adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")'.

    else.                          "Production Payroll
      select p2rx_eval_period~dct_pernr as dct_pernr
        into corresponding fields of table @lt_pernr
        from p2rx_eval_period
       where p2rx_eval_period~dct_pernr in @it_pernr_so
         and p2rx_eval_period~abkrs in @mt_payroll_areas
         and p2rx_eval_period~inper = @mv_payroll_period
        %_hints adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")'.
    endif.

    sort lt_pernr by dct_pernr.
    delete adjacent duplicates from lt_pernr.

* Perform Parallel Processing
    lv_abs_objname = cl_abap_classdescr=>get_class_name( me ).
    split lv_abs_objname at mc_equal into data(lv_clstext) lv_objname.
    call method me->multithread_process
      exporting
        iv_clsname = lv_objname
        it_pernr   = lt_pernr.

* Collect the data
    data: lo_cloned_chkobj type ref to zcl_m99_py_payroll_log.
    loop at mt_objdata into ms_objdata.
      try.
          call method me->convert_objdata
            exporting
              is_objdata       = ms_objdata
            changing
              io_cloned_object = lo_cloned_chkobj.

          append lines of lo_cloned_chkobj->mt_bat_p2rq_messages to lt_all_p2rq_messages.
        catch cx_pyd_fnd.
      endtry.
    endloop.

* Return messages for final result process
    et_p2rq_messages[] = lt_all_p2rq_messages[].

  endmethod.


  method get_specifc_custmizing.
* Read Check Specific Parameters
    data lt_param_so  type /iwbep/t_cod_select_options.
    data: lv_mclas type string,
          lv_msgno type string.

    try.
* Read Message Type
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_msgty
            iv_exc_par_type  = me->mc_exc_filter_msgty
          changing
            ct_parameter_tab = mt_filter_msgty.

* Read Message Number
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_msgty
            iv_exc_par_type  = me->mc_exc_filter_msgty
          changing
            ct_parameter_tab = mt_filter_msgno.

* Read Message
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_message
            iv_exc_par_type  = me->mc_exc_filter_message
          changing
            ct_parameter_tab = mt_filter_message.

* Read P2RQ_MESSAGES Message Type
        call method me->read_range_parameter
          exporting
            iv_inc_par_type  = me->mc_filter_p2rq_msg
          changing
            ct_parameter_tab = mt_filter_p2rq_msg.

        loop at mt_filter_p2rq_msg into data(ls_filter_p2rq_msg).
          split ls_filter_p2rq_msg-low at mc_separator_slash into lv_mclas lv_msgno.

          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = lv_mclas ).
          append lines of lt_param_so to mt_filter_p2rq_mclas.

          clear lt_param_so.
          lt_param_so = cl_pyd_fnd_aux=>set_so_fixed_value( iv_value = lv_msgno ).
          append lines of lt_param_so to mt_filter_p2rq_msgno.
        endloop.

      catch cx_pyd_fnd into data(lo_exception).
    endtry.

  endmethod.


  method merge_pymsg_and_p2rq_msg.
* Merge PYC_PY_D_MSG and P2RQ_MESSAGES
    data: ls_excep type pc23n.
    data: lv_msg type pc29k-mstxt.
    data: lv_msgno type pc23n-msgno.
    data: ls_payroll_msg type ty_payroll_msg.
    data: lt_t100a type table of t100a.
* Read Messages table
    if not mt_filter_p2rq_mclas is initial.
      select arbgb stext from  t100a
        into corresponding fields of table lt_t100a
       where arbgb  in mt_filter_p2rq_mclas
         and masterlang  = sy-langu.
    endif.

* Transfer Valid P2RQ messages to the Payroll Messages table
    loop at it_p2rq_messages into data(ls_p2rq_messages).
      clear: ls_excep.
      if ls_p2rq_messages-mstxt ca mc_msg_separator.
        split ls_p2rq_messages-mstxt at mc_msg_separator into ls_excep-mclas ls_excep-msgno lv_msg.
      else.
        move: mc_mclas_standard      to ls_excep-mclas,
              mc_msgno_0             to ls_excep-msgno,
              ls_p2rq_messages-mstxt to lv_msg.
      endif.

      check ls_excep-mclas in mt_filter_p2rq_mclas.
      check ls_excep-msgno in mt_filter_p2rq_msgno.

      move: ls_p2rq_messages-dct_pernr to ls_payroll_msg-pernr,
            ls_p2rq_messages-dct_seqnr to ls_payroll_msg-seqnr,
            mc_tlevel_00               to ls_payroll_msg-tlevel.
      move: lv_msg to ls_payroll_msg-text1.
      read table lt_t100a into data(ls_t100a)
        with key arbgb = ls_excep-mclas.
      if sy-subrc eq 0.
        move ls_t100a-stext to ls_payroll_msg-as_text.
      endif.

      append ls_payroll_msg to ct_payroll_msg.
    endloop.

  endmethod.


  method read_check_data.

    data: lt_p2rq_messages type standard table of ty_p2rq_messages.

    "Check if a Test Payroll
    if mv_tpy_res is not initial.
      refresh: lt_p2rq_messages.
      select p2rq_messages~dct_pernr as dct_pernr, p2rq_messages~dct_seqnr as dct_seqnr,
             p2rq_messages~msgar as msgar, p2rq_messages~mstxt as mstxt
              into corresponding fields of table @lt_p2rq_messages
              from hrdct_tpy_rgdir inner join p2rq_messages
                on hrdct_tpy_rgdir~dct_pernr eq p2rq_messages~dct_pernr
               and hrdct_tpy_rgdir~dct_seqnr eq p2rq_messages~dct_seqnr
             where hrdct_tpy_rgdir~dct_pernr in @it_pernr
               and hrdct_tpy_rgdir~abkrs in @mt_payroll_areas
              " and hrdct_tpy_rgdir~fpper = @mv_payroll_period
               and hrdct_tpy_rgdir~inper = @mv_payroll_period
               and exists ( select 1
                           from p2rx_wpbp_index
                           inner join p2rx_wpbp
                              on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                             and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                             and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rq_messages~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rq_messages~dct_seqnr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
        %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("HRDCT_TPY_RGDIR~PCC")'.

    else. "Production Payroll
      refresh: lt_p2rq_messages.
      select p2rq_messages~dct_pernr as dct_pernr, p2rq_messages~dct_seqnr as dct_seqnr,
             p2rq_messages~msgar as msgar, p2rq_messages~mstxt as mstxt
              into corresponding fields of table @lt_p2rq_messages
                       from p2rx_eval_period inner join p2rq_messages
                         on p2rx_eval_period~dct_pernr eq p2rq_messages~dct_pernr
                        and p2rx_eval_period~dct_seqnr eq p2rq_messages~dct_seqnr
             where p2rx_eval_period~dct_pernr in @it_pernr
               and p2rx_eval_period~abkrs in @mt_payroll_areas
             "  and p2rx_eval_period~fpper = @mv_payroll_period
               and p2rx_eval_period~inper = @mv_payroll_period
               and exists ( select 1
                           from p2rx_wpbp_index
                                 inner join p2rx_wpbp
                                    on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
                                   and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
                                   and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
                           where p2rx_wpbp_index~dct_pernr eq p2rq_messages~dct_pernr
                             and p2rx_wpbp_index~dct_seqnr eq p2rq_messages~dct_seqnr
                             and p2rx_wpbp~bukrs in @mt_bukrs
                             and p2rx_wpbp~werks in @mt_werks
                             and p2rx_wpbp~btrtl in @mt_btrtl
                             and p2rx_wpbp~persg in @mt_persg
                             and p2rx_wpbp~persk in @mt_persk
                             and p2rx_wpbp~kostl in @mt_kostl )
             %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("P2RX_EVAL_PERIOD~PCC")'.
    endif.

    sort lt_p2rq_messages by dct_pernr.
    delete adjacent duplicates from lt_p2rq_messages comparing dct_pernr msgar mstxt.

    mt_bat_p2rq_messages[] = lt_p2rq_messages[].

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

    data: lt_pyc_d_py_msg type table of ty_payroll_msg,
          ls_payroll_log  type ty_payroll_msg,
          lt_lines        type table of text60,    "Note 2707759
          lv_seqnr        type pyd_rowid.          "Note 2707759

    "temp! refactor coding into reuse class and config class later!

    "personnel number
    lv_pernr = is_rd-id.
*>>> WOW Specific Code
    call method get_parameters
      exporting
        it_par         = it_par
        io_res_context = io_res_context.
*<<< WOW Specific Code

    select lt_pyc_d_py_msg~pernr as pernr, lt_pyc_d_py_msg~seqnr as seqnr,
      lt_pyc_d_py_msg~as_text as as_text, lt_pyc_d_py_msg~text1 as text1, lt_pyc_d_py_msg~tlevel as tlevel
      into table @lt_pyc_d_py_msg
      from pyc_d_py_msg as lt_pyc_d_py_msg
      where lt_pyc_d_py_msg~abkrs = @mv_payroll_area and
            lt_pyc_d_py_msg~iaper = @mv_payroll_period and
            lt_pyc_d_py_msg~pernr = @lv_pernr and
            lt_pyc_d_py_msg~test_res = @mv_tpy_res and

*>>> WOW Specific Code
            lt_pyc_d_py_msg~msgty in @mt_filter_msgty and
            lt_pyc_d_py_msg~msgno in @mt_filter_msgno and
            lt_pyc_d_py_msg~message in @mt_filter_message.

    data: lt_pernr type hr99s_pernr_range,
          ls_pernr like line of lt_pernr.
    data: lt_t_payroll_msg type tty_payroll_msg.

    ls_pernr = value #( sign =  if_fsbp_const_range=>sign_include
                        option = if_fsbp_const_range=>option_equal
                        low = lv_pernr ).
    append ls_pernr to lt_pernr.
    call method me->read_check_data
      exporting
        it_pernr = lt_pernr.

* Append P2RQ_MESSAGES to the LT_PAYROLL_MESSAGES
    call method me->merge_pymsg_and_p2rq_msg
      exporting
        it_p2rq_messages = mt_bat_p2rq_messages
      changing
        ct_payroll_msg   = lt_pyc_d_py_msg.
*<<< WOW Specific Code

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
ENDCLASS.
