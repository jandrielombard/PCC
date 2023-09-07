class zcl_m99_py_sim_posting_rej_ees definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.
  protected section.

    types:
      begin of ty_s_bpc_result,
        id     type pyc_obj_id,
        bpc_id type pyc_bpc_id,
      end of ty_s_bpc_result .
    types:
      ty_t_msg_dtl type standard table of pyc_d_py_msg .
    types:
      ty_t_bpc_result  type standard table of ty_s_bpc_result .
    types:
      ty_t_pernrs  type standard table of pernr_d with non-unique key table_line .

    constants mc_itemid_simposterr type pyd_itemid value 'SIMPOSTERR' ##NO_TEXT.
    constants mc_itemid_simpostmsg type pyd_itemid value 'SIMPOSTMSG' ##NO_TEXT.

    methods get_msg_detail
      importing
        !io_res_context type ref to if_pyd_res_context
        !iv_pernr       type p_pernr
      exporting
        !ev_msg_detail  type string .

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZCL_M99_PY_SIM_POSTING_REJ_EES IMPLEMENTATION.


  method check.
* Posting Simulation Errors
    constants: lc_pyp_proc_inst type pyd_par_type value 'PYP_PROC_INST',
               lc_pyd_shadow    type pyd_par_type value 'PYD_SHADOW',
               lc_pernr         type pyd_par_type value 'PERNR',
               lc_simpo         type pyc_obj_id value 'SIMPO'.
    data:
      lt_pernr        type standard table of pernr_d with non-unique key table_line,
      lt_rejo         type                   ty_t_bpc_result,
      lv_pernr        type                   pernr_d,
      ls_result       type                   ty_s_result,
      lv_pypid        type                   string,
      lo_cx_cont      type ref to cx_pyd_cont,
      lo_cx_fnd       type ref to cx_pyd_fnd,
      lt_shadow_id_so type /iwbep/t_cod_select_options,
      lt_shadow_or_so type /iwbep/t_cod_select_options.

    " Off-cycle does not support in this runtime class
    data:
      ls_par    like line of it_par,
      lv_pt_cat type pyc_proc_templ_cat,
      lo_pi_aux type ref to cl_pyc_proc_inst_aux.

    if io_res_context is not initial.
      read table io_res_context->mt_par into ls_par with key par_type = cl_pyc_pt_proc_templ_cat=>gc_par_type.
      if sy-subrc = 0.
        lv_pt_cat = ls_par-low.

        try .
            lo_pi_aux = cl_pyc_proc_inst_aux=>get_instance( io_transaction = mo_fnd_factory->mo_transaction ).
            if lo_pi_aux->proc_inst_is_oc( iv_proc_templ_cat = lv_pt_cat ) eq abap_true.
              return.
            endif.
          catch cx_pyc_frw.
        endtry.
      endif.
    endif.

    try.
        call method get_parameters  " get parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.
      catch cx_pyd_fnd into lo_cx_fnd.
*        raise exception lo_cx_fnd .
    endtry.

    loop at it_par into ls_par where par_type = lc_pyp_proc_inst.
      lv_pypid = ls_par-low.
    endloop.

    loop at it_par_addl into data(ls_par_addl) where par_type = lc_pyd_shadow.
      append value #( sign = 'I' option = 'EQ' low = ls_par_addl-id ) to lt_shadow_id_so.
    endloop.
    if sy-subrc <> 0. "normal execution from process management without shadow id
      append value #( sign = 'I' option = 'EQ' low = '02' ) to lt_shadow_or_so.
    endif.

*  select distinct rejo~rej_obj_id as id,rejo~bpc_id as bpc_id "#EC CI_NOFIRST
*      into table @lt_rejo
*      from pyc_d_bpc_rejo as rejo inner join pyc_d_bpc as bpc on rejo~bpc_id = bpc~id inner join pyc_d_bpc_rejom as rejom on rejo~bpc_id = rejom~bpc_id
*                                  left join  pyc_d_bpc_rpt as rpt on bpc~id = rpt~bpc_id and rejo~rpt_chain_id = rpt~rpt_chain_id
*      where rejo~rej_obj_type = @lc_pernr and
*            rpt~rejo_cat      = @lc_simpo and
*            bpc~pypi_id = @lv_pypid and
*            bpc~shadow_id in @lt_shadow_id_so and
*            bpc~shadow_origin in @lt_shadow_or_so.
* SQL Change to improve Performance
    select distinct rejo~rej_obj_id as id, rejo~bpc_id as bpc_id
      into table @lt_rejo
      from pyc_d_bpc as bpc
      inner join pyc_d_bpc_rejo as rejo
         on rejo~bpc_id = bpc~id
       left join  pyc_d_bpc_rpt as rpt
         on rpt~bpc_id = bpc~id
        and rpt~rpt_chain_id = rejo~rpt_chain_id
      where bpc~pypi_id = @lv_pypid
        and bpc~shadow_id in @lt_shadow_id_so
        and bpc~shadow_origin in @lt_shadow_or_so
        and rejo~rej_obj_type = @lc_pernr
        and rpt~rejo_cat      = @lc_simpo
        and exists ( select 1
                       from pyc_d_bpc_rejom
                      where pyc_d_bpc_rejom~bpc_id = rejo~bpc_id
                        and pyc_d_bpc_rejom~rpt_chain_id = rejo~rpt_chain_id
                        and pyc_d_bpc_rejom~rej_obj_type = rejo~rej_obj_type
                        and pyc_d_bpc_rejom~rej_obj_id  = rejo~rej_obj_id ) %_hints adabas 'ORDERED'.

    sort lt_rejo.
    delete adjacent duplicates from lt_rejo.

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_rejo into data(ls_rejo).
      ls_result-id = ls_rejo-id.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.
  endmethod.


  method err_ov_get_list.
* Method for Overview list display
    data: lv_abkrs  type abkrs,
          lv_bukrs  type bukrs,
          lv_pernr  type p_pernr,
          ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
          lv_text   type cl_pyd_rd_dto_sfo=>ty_s_rd-text,
          ls_err_ov type ty_s_err_ov.

    data:
      lt_msg_dtl type ty_t_msg_dtl,
      ls_msg_dtl type pyc_d_py_msg,
      lv_msg_dtl type string,
      lt_msg     type table of string.

    case iv_access_mode.
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        read table ct_err_ov into ls_err_ov index 1.
        lv_pernr = ls_err_ov-id.

        get_msg_detail(
          exporting
            io_res_context  = io_res_context  " Result Detail Generic
            iv_pernr        = lv_pernr
          importing
            ev_msg_detail = lv_msg_dtl         " get detail from pyc_d_py_msg
              ).

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.

          add 1 to ls_sfo-row_id.
          ls_sfo-itemid = mc_itemid_simposterr.
          ls_sfo-text   = text-001.
          ls_sfo-value  = text-002.
          insert ls_sfo into table ls_err_ov-sfo_tab.

          if not lv_msg_dtl is initial.
            lv_text = text-003.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_simpostmsg
                iv_text                     = lv_text
                iv_value                    = lv_msg_dtl
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endif.

          modify ct_err_ov from ls_err_ov.
        endloop.
      when others.

    endcase.
  endmethod.


  method get_msg_detail.
* Get Posting Simulation Error Message
    data: lt_msg_detail type table of pyc_d_bpc_rejom.

    select * into corresponding fields of table @lt_msg_detail from pyc_d_bpc_rejom as rejom
      inner join pyc_d_bpc_rejo as rejo on rejom~bpc_id = rejo~bpc_id and rejom~rej_obj_id = rejo~rej_obj_id
      inner join pyc_d_bpc_rpt as rpt on rejo~bpc_id = rpt~bpc_id and rejo~rpt_chain_id = rpt~rpt_chain_id
      where rejom~rej_obj_type = @if_pyd_cont_types=>gcs_par_type-pernr and
            rejom~msgty        = 'E' and
            rejom~rej_obj_id   = @iv_pernr.             "#EC CI_NOFIRST

    loop at lt_msg_detail assigning field-symbol(<fs_msg_detail>).
      message id <fs_msg_detail>-msgid  type 'I' number <fs_msg_detail>-msgno
        with <fs_msg_detail>-msgv1 <fs_msg_detail>-msgv2 <fs_msg_detail>-msgv3 <fs_msg_detail>-msgv4
        into ev_msg_detail.
    endloop.

  endmethod.
ENDCLASS.
