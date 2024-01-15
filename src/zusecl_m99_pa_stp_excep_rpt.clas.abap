class ZUSECL_M99_PA_STP_EXCEP_RPT definition
  public
  inheriting from ZuseCL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
  protected section.

    types:
* IT 0006 STP Data
      begin of ty_it6_stpdata,
        pernr type p0006-pernr,
        subty type p0006-subty,
        stras type p0006-stras,
        state type p0006-state,
        pstlz type p0006-pstlz.

    types: end of ty_it6_stpdata .
    types:
      tty_it6_stpdata type table of ty_it6_stpdata .

    types:
      begin of ty_stpexc_rep,
        pernr type persno,
        errms type text80,
      end of ty_stpexc_rep .
    types:
      tty_stpexc_rep type table of ty_stpexc_rep .

    data mt_it6_stpdata type tty_it6_stpdata .
    data ms_it6_stpdata type ty_it6_stpdata .
    data mc_itemid_stpexc type pyd_itemid value 'STPEXC' ##NO_TEXT.
    data mt_stpexc_rep type tty_stpexc_rep .
    data ms_stpexc_rep type ty_stpexc_rep .
    constants c_0006 type tabname value 'PA0006' ##NO_TEXT.
    constants c_pstlz type fieldname value 'PSTLZ' ##NO_TEXT.

    constants c_nsw type regio value 'NSW' ##NO_TEXT.
    constants c_act type regio value 'ACT' ##NO_TEXT.
    constants c_vic type regio value 'VIC' ##NO_TEXT.
    constants c_sa type regio value 'SA' ##NO_TEXT.
    constants c_tas type regio value 'TAS' ##NO_TEXT.
    constants c_wa type regio value 'WA' ##NO_TEXT.
    constants c_nt type regio value 'NT' ##NO_TEXT.
    constants c_qld type regio value 'QLD' ##NO_TEXT.

    methods check
        redefinition .
    methods err_ov_get_list
        redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_STP_EXCEP_RPT IMPLEMENTATION.


  method check.
* STP Data Verification
* IT 0006 STP Data
    data: lt_it6_stpdata type table of ty_it6_stpdata,
          ls_it6_stpdata type ty_it6_stpdata.
    data: lv_pstlz_error type boolean.

* STP Error Details
    data lt_stpexc_rep type tty_stpexc_rep .
    data ls_stpexc_rep type ty_stpexc_rep .

    data: ls_result  type ty_s_result.

* Fetch Employees IT 0006 STP data
    select it0006~pernr, it0006~subty, it0006~stras, it0006~state, it0006~pstlz
      into corresponding fields of table @lt_it6_stpdata
      from pa0006 as it0006
      inner join pa0000 as it0000 on it0000~pernr = it0006~pernr
      inner join pa0001 as it0001 on it0001~pernr = it0006~pernr
     where it0006~pernr in @it_pernr_so
       and it0006~begda <= @mv_endda
       and it0006~endda >= @mv_begda
       and it0006~subty = @mv_subty   "permanent address
       and it0000~pernr = it0006~pernr
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
       and it0001~kostl in @mt_kostl .
* Delete duplicate records
    delete adjacent duplicates from lt_it6_stpdata comparing all fields.

    loop at it_pernr_so assigning field-symbol(<person>).
      clear: ls_stpexc_rep.
      move: <person>-low to ls_stpexc_rep-pernr.

* Identify Post code Errors in IT0006 fields
      read table lt_it6_stpdata into ls_it6_stpdata with key pernr = <person>-low.
      if sy-subrc = 0.
        clear: lv_pstlz_error.
* Check postcode
        case ls_it6_stpdata-state.
          when c_nsw or c_act.
            if ls_it6_stpdata-pstlz(1) <> '2'.
              lv_pstlz_error = abap_true.
            endif.
          when c_vic.
            if ls_it6_stpdata-pstlz(1) <> '3'.
              lv_pstlz_error = abap_true.
            endif.
          when c_qld.
            if ls_it6_stpdata-pstlz(1) <> '4'.
              lv_pstlz_error = abap_true.
            endif.
          when c_sa.
            if ls_it6_stpdata-pstlz(1) <> '5'.
              lv_pstlz_error = abap_true.
            endif.
          when c_wa.
            if ls_it6_stpdata-pstlz(1) <> '6'.
              lv_pstlz_error = abap_true.
            endif.
          when c_tas.
            if ls_it6_stpdata-pstlz(1) <> '7'.
              lv_pstlz_error = abap_true.
            endif.
          when c_nt.
            if ls_it6_stpdata-pstlz+0(2) <> '08' and ls_it6_stpdata-pstlz+0(2) <> '09'.
              lv_pstlz_error = abap_true.
            endif.
          when others.
            if ls_it6_stpdata-state is initial.
              message i024(zhrpy_pcc_msg) into ls_stpexc_rep-errms.
            endif.
        endcase.
        if lv_pstlz_error eq abap_true.
          message i023(zhrpy_pcc_msg)
                  with ls_it6_stpdata-state
                       ls_it6_stpdata-pstlz(1)
                       ls_it6_stpdata-subty
                  into ls_stpexc_rep-errms.
        endif.

        if not ls_stpexc_rep-errms is initial.
          append ls_stpexc_rep to lt_stpexc_rep.
        endif.

        if ls_it6_stpdata-stras is initial.   "check the street is provided
          message i052(zhrpy_pcc_msg) into ls_stpexc_rep-errms.
          append ls_stpexc_rep to lt_stpexc_rep.
        endif.
      else.   "no address found
        message i053(zhrpy_pcc_msg) into ls_stpexc_rep-errms.
        append ls_stpexc_rep to lt_stpexc_rep.
      endif.
    endloop.




    sort lt_stpexc_rep by pernr.
* Collect Data for Overview
    append lines of lt_stpexc_rep to mt_stpexc_rep.

* Build Results table
    loop at lt_stpexc_rep  into ls_stpexc_rep
         group by ( pernr = ls_stpexc_rep-pernr ) ascending
         without members
         assigning field-symbol(<group>).
      append value #(  par_type = if_pyd_cont_types=>gcs_par_type-pernr
                             id = <group>-pernr ) to rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.
  endmethod.


  method ERR_OV_GET_LIST.
* Method for Overview list display
* Method for Overview list display
    data: ls_err_ov type ty_s_err_ov,
          ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
          lv_modif  type abap_bool,
          lv_pernr  type p_pernr.

    data: ls_stpexc_rep type ty_stpexc_rep.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

* Populate SFO Tab
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.

        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

* Populate text into Value field for display
          clear: ls_err_ov-sfo_tab.
          clear ls_sfo.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_txt = ls_sfo_tab-text.

            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_stpexc
                iv_text                     = mc_text_reason
                iv_value                    = lv_txt
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
* Populate long Error message into text field for table save
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          loop at mt_stpexc_rep into ls_stpexc_rep
              where pernr eq lv_pernr.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_stpexc.
            ls_sfo-text   = ls_stpexc_rep-errms.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.
ENDCLASS.
