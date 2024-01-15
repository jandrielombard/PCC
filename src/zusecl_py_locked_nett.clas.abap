class ZUSECL_PY_LOCKED_NETT definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.
protected section.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
private section.

  constants MC_Z99_LGART type PYD_PAR_TYPE value 'Z99_LGART' ##NO_TEXT.
  constants MC_ABRSP type PYD_PAR_TYPE value 'Z99_ABRSP' ##NO_TEXT.
  data MV_ABRSP type ABRSP .
  constants MC_ABRSP_DEFAULT_VALUE type ABRSP value 'X' ##NO_TEXT.
  constants MC_ITEMID type PYD_S_RDSFO_EXT-ITEMID value 'RETRO' ##NO_TEXT.
  data MV_Z99_LGART type LGART .
ENDCLASS.



CLASS ZUSECL_PY_LOCKED_NETT IMPLEMENTATION.


 method check.

* Check if person is locked, and if they have pay results (nett pay)

   data lt_pernr   type standard table of pernr_d with non-unique key table_line.
   data ls_result  type                   ty_s_result.

   select distinct it00~pernr into table lt_pernr from pa0000 as it00
       inner join pa0001 as it01 on it00~pernr = it01~pernr
       inner join pa0003 as it03 on it00~pernr = it03~pernr
       where it03~pernr in it_pernr_so                    and
             it03~begda <= mv_endda                       and
             it03~endda >= mv_begda                       and
             it00~begda <= mv_endda                       and
             it00~endda >= mv_begda                       and
             it01~begda <= mv_endda                       and
             it01~abkrs in mt_payroll_areas               and
             it01~sprps = if_hrpa_read_infotype=>unlocked and
             it01~endda >= mv_begda                       and
             it00~stat2  in mt_stat2                      and
             it00~sprps = if_hrpa_read_infotype=>unlocked and
             it03~sprps = if_hrpa_read_infotype=>unlocked and
             it03~abrsp = mv_abrsp                        and
             it01~bukrs in mt_bukrs                       and
             it01~werks in mt_werks                       and
             it01~btrtl in mt_btrtl                       and
             it01~persg in mt_persg                       and
             it01~persk in mt_persk                       and
             it01~kostl in mt_kostl
     order by it00~pernr.


   select p2rx_rt~dct_pernr as dct_pernr, p2rx_rt~lgart as lgart
           into   table @data(lt_lgart_data)
                    from p2rx_versc inner join p2rx_rt
                                  on p2rx_versc~dct_pernr eq p2rx_rt~dct_pernr
                                 and p2rx_versc~dct_seqnr eq p2rx_rt~dct_seqnr
          where p2rx_versc~dct_pernr in @it_pernr_so
            and p2rx_versc~abkrs in @mt_payroll_areas
            and p2rx_versc~inper =  @mv_payroll_period  " @mv_prev_payroll_period
            and p2rx_rt~lgart = @mv_Z99_lgart
*             and exists ( select 1
*                         from p2rx_wpbp_index
*                               inner join p2rx_wpbp
*                                  on p2rx_wpbp~dct_pernr eq p2rx_wpbp_index~dct_pernr
*                                 and p2rx_wpbp~dct_seqnr eq p2rx_wpbp_index~dct_seqnr
*                                 and p2rx_wpbp~apznr     eq p2rx_wpbp_index~wpbp_apznr
*                         where p2rx_wpbp_index~dct_pernr eq p2rx_rt~dct_pernr
*                           and p2rx_wpbp_index~dct_seqnr eq p2rx_rt~dct_seqnr
*                           and p2rx_wpbp_index~rt_apznr  eq p2rx_rt~apznr )
          group by p2rx_rt~dct_pernr, p2rx_versc~fpper, p2rx_versc~fpend, p2rx_rt~lgart
          %_hints adabas 'ORDERED' adabas 'INDEXACCESS ("p2rx_versc~PCC")' adabas 'INDEXACCESS ("P2RX_RT~PCC")'.



   ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.

   loop at lt_pernr into ls_result-id.
    if line_exists( lt_lgart_data[ dct_pernr = ls_result-id ] ).  "If they have results and are locked, then produce error
      insert ls_result into table rt_result.
    endif.

   endloop.

   " register event when transaction complete we need clear cache
   set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

 endmethod.


  METHOD ERR_OV_GET_LIST.

    DATA:
      ls_err_ov TYPE ty_s_err_ov,
      ls_sfo    TYPE cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  TYPE abap_bool,
      ls_reso   TYPE if_pyd_fnd_types=>ty_s_reso,
      lv_abkrs  TYPE abkrs,
      lv_land1  TYPE land1,
      lv_land2  TYPE land1,
      lv_werk   TYPE persa,
      lv_pernr  TYPE p_pernr.

    CASE iv_access_mode.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        LOOP AT ct_err_ov INTO ls_err_ov.
          lv_modif = abap_false.
          lv_pernr = ls_err_ov-id.
          IF ls_err_ov-sfo_tab IS INITIAL.
            CLEAR ls_err_ov-sfo_tab.
            CLEAR ls_sfo.

            ADD 1 TO ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid.
            ls_sfo-value  = text-001.
            ls_sfo-text = text-002.
            INSERT ls_sfo INTO TABLE ls_err_ov-sfo_tab.
            MODIFY ct_err_ov FROM ls_err_ov.

          ELSE.
            LOOP AT ls_err_ov-sfo_tab INTO ls_sfo.
              CASE ls_sfo-itemid.
                WHEN mc_itemid.
                  ls_sfo-text = text-002 .
                  MODIFY ls_err_ov-sfo_tab FROM ls_sfo TRANSPORTING text.
                  lv_modif = abap_true.
                WHEN OTHERS.
                  "nothing
              ENDCASE.
            ENDLOOP.
            IF lv_modif = abap_true.
              MODIFY ct_err_ov FROM ls_err_ov.
            ENDIF.
          ENDIF.

        ENDLOOP.

      WHEN if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        CALL METHOD get_parameters
          EXPORTING
            it_par         = it_par
            io_res_context = io_res_context.

        LOOP AT ct_err_ov INTO ls_err_ov.
          CLEAR ls_err_ov-sfo_tab.
          CLEAR ls_sfo.
          lv_pernr = ls_err_ov-id.

          ADD 1 TO ls_sfo-row_id.
          ls_sfo-itemid = mc_itemid.
          ls_sfo-value  = text-001.
          INSERT ls_sfo INTO TABLE ls_err_ov-sfo_tab.
          MODIFY ct_err_ov FROM ls_err_ov.
        ENDLOOP.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_pyd_fnd.

    ENDCASE.

  ENDMETHOD.


  method get_specifc_custmizing.

    try .
        if line_exists( mo_context->mt_par[ par_type = me->mc_abrsp ] ).
          mv_abrsp =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_abrsp
                                                            it_par      = mo_context->mt_par ).
        else.
          mv_abrsp = mc_abrsp_default_value.
        endif.
        if line_exists( mo_context->mt_par[ par_type = me->mc_z99_lgart ] ).
          mv_z99_lgart =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_z99_lgart
                                                            it_par      = mo_context->mt_par ).
        endif.

      catch cx_pyd_fnd into data(lo_exception).

    endtry.


  endmethod.
ENDCLASS.
