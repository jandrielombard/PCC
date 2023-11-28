class ZUSECL_PYC_BPC_SELECT_STP2 definition
  public
  inheriting from CL_PYC_BPC_SELECTION_PNP
  final
  create public .

public section.
protected section.

  constants:
    begin of gc_par_type,
        period type pyd_par_type value 'PERIOD',
        abkrs  type pyd_par_type value 'ABKRS',
      end of gc_par_type .

  methods GET_LEADING_TIME_SELECTION
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_PYC_BPC_SELECT_STP2 IMPLEMENTATION.


  method get_leading_time_selection.
*----------------------------------------------------------------------*
*Date      |User ID |Description              |Chg. Ref.|WkBh Req      *
*----------------------------------------------------------------------*
*01/03/2023|1106254 |Initial development      |STPS-187 |CFAK902866
*                    Copy of STP BPC selection           Revtrac - 1220
*                    class with addition of
*                    update DB para in testrun
*----------------------------------------------------------------------*

* Populate Selection Parameters
    constants: lc_abkrs        type pyd_par_type value 'ABKRS',
               lc_period       type pyd_par_type value 'PERIOD',
               lc_test_run     type pyd_par_type value 'Z99_TEST_RUN',
               lc_update_db    type pyd_par_type value 'Z99_UPDATE_DB',
               lc_sel_timed    type rsscr_name   value 'PNPTIMED',
               lc_par_pnppabrp type rsscr_name   value 'PNPPABRP',
               lc_par_pnppabrj type rsscr_name   value 'PNPPABRJ',
               lc_par_pnpxabkr type rsscr_name   value 'PNPXABKR',
               lc_par_pnpabkrs type rsscr_name   value 'PNPABKRS',
               lc_par_submit   type rsscr_name   value 'SUBMIT',
               lc_par_update   type rsscr_name   value 'UPDATE',
               lc_par_runty    type rsscr_name   value 'RUNTY',
               lc_par_upddb    type rsscr_name   value 'UPD',
               lc_par_noupd    type rsscr_name   value 'NOUPD',
               lc_x            type c            value 'X',
               lc_i            type tvarv_sign   value 'I',
               lc_p            type tvarv_sign   value 'P',
               lc_eq           type tvarv_opti   value 'EQ',
               lc_two          type c            value '2',
               lc_blank        type c            value ' ',
               lc_par_spool    type rsscr_name   value 'SPOOL',
               lc_excel        type rsscr_name   value 'DOWN_CHK',
               lc_pc           type rsscr_name   value 'PC_CHK',
               lc_server       type rsscr_name   value 'DOSF_CHK'.

    data:ls_par        type pyd_s_resp,
         ls_sel_params type line of rsparams_tt,
         lv_sel_name   type char8,
         lv_pabrj      type pabrj,
         lv_pabrp      type pabrp,
         lc_tst_flag   type c.

    clear et_sel_params.

*check PERIOD exist; if exist set to the other period
    read table it_par into ls_par with key par_type = gc_par_type-period.
    if sy-subrc = 0.
      clear ls_sel_params .
      ls_sel_params-selname = lc_sel_timed.
      ls_sel_params-kind    = lc_p.
      ls_sel_params-sign    = lc_i .
      ls_sel_params-option  = lc_eq .
      ls_sel_params-low     = lc_two .
      append ls_sel_params to et_sel_params .
    endif.


* Set ABKRS, Period Begda and Period Endda
    loop at it_par into ls_par.
      case ls_par-par_type.
        when lc_abkrs.
* Populate PNPXABKR
          lv_sel_name = lc_par_pnpxabkr.

          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
          ls_sel_params-selname = lv_sel_name.
          append ls_sel_params to et_sel_params  .

* Populate PNPABKRS
          lv_sel_name = lc_par_pnpabkrs.

          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
          ls_sel_params-selname = lv_sel_name.
          append ls_sel_params to et_sel_params  .

*if period exist, assign other period
        when lc_period.
          lv_pabrj = ls_par-low(4).
          lv_pabrp = ls_par-low+4(2).

          lv_sel_name = lc_par_pnppabrp.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low     = lv_pabrp.
          append ls_sel_params to et_sel_params .

          lv_sel_name = lc_par_pnppabrj.
          clear ls_sel_params .
          move-corresponding ls_par to ls_sel_params.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low     = lv_pabrj.
          append ls_sel_params to et_sel_params .

        when lc_test_run.
* Parameter Run Type
          lv_sel_name = lc_par_runty.
          clear ls_sel_params .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-kind    = lc_p.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
*Set lc_tst_flag if test indicator is 'X'
          ls_sel_params-low    = lc_tst_flag  = ls_par-low.
          append ls_sel_params to et_sel_params.

        when  lc_update_db.
*Parameter Test Run with Update/No UPD to DB for STP2
*Check if test indicator is set as 'X'
          if lc_tst_flag = lc_x.
*Check if UPD is set as 'X'
            if ls_par-low = lc_x.
              lv_sel_name = lc_par_upddb.
              clear ls_sel_params .
              ls_sel_params-selname = lv_sel_name.
              ls_sel_params-kind    = lc_p.
              ls_sel_params-sign    = lc_i .
              ls_sel_params-option  = lc_eq .
              ls_sel_params-low     = ls_par-low.
              append ls_sel_params to et_sel_params.
*Clear the noupd radiobutton if UPD is set to 'X'
              lv_sel_name = lc_par_noupd.
              clear ls_sel_params .
              ls_sel_params-selname = lv_sel_name.
              ls_sel_params-kind    = lc_p.
              ls_sel_params-sign    = lc_i .
              ls_sel_params-option  = lc_eq .
              ls_sel_params-low     = lc_blank.
              append ls_sel_params to et_sel_params.
            else.
              lv_sel_name = lc_par_noupd.
              clear ls_sel_params .
              ls_sel_params-selname = lv_sel_name.
              ls_sel_params-kind    = lc_p.
              ls_sel_params-sign    = lc_i .
              ls_sel_params-option  = lc_eq .
              ls_sel_params-low     = lc_x.
              append ls_sel_params to et_sel_params.
            endif.
            clear lc_tst_flag.
          endif.

        when others.
      endcase.
    endloop.

* Parameter Submit
    lv_sel_name = lc_par_submit.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind    = lc_p.
    ls_sel_params-sign    = lc_i .
    ls_sel_params-option  = lc_eq .
    ls_sel_params-low     = abap_true.
    append ls_sel_params to et_sel_params .

* Parameter Update
    lv_sel_name = lc_par_update.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind    = lc_p.
    ls_sel_params-sign    = lc_i .
    ls_sel_params-option  = lc_eq .
    ls_sel_params-low     = lc_blank.
    append ls_sel_params to et_sel_params .

* Parameter Spool Down load
    lv_sel_name = lc_par_spool.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind    = lc_p.
    ls_sel_params-sign    = lc_i .
    ls_sel_params-option  = lc_eq .
    ls_sel_params-low     = abap_true.
    append ls_sel_params to et_sel_params.

* Parameter App Server download
    lv_sel_name = lc_server.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind    = lc_p.
    ls_sel_params-sign    = lc_i .
    ls_sel_params-option  = lc_eq .
    ls_sel_params-low     = abap_true.
    append ls_sel_params to et_sel_params.

*   Remove pc download radiobutton
    lv_sel_name = lc_pc.
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-low     = abap_false.
    append ls_sel_params to et_sel_params.
* Parameter excel
    lv_sel_name = lc_excel.
    clear ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind    = lc_p.
    ls_sel_params-sign    = lc_i .
    ls_sel_params-option  = lc_eq .
    ls_sel_params-low     = abap_true.
    append ls_sel_params to et_sel_params.

    if lines( et_sel_params ) = 0 .
      raise exception type cx_pyc_cont
        exporting
          textid = cx_pyc_cont=>get_leading_time_selection_err.
    endif.

  endmethod.
ENDCLASS.
