class ZCL_PYC_BPC_SELECTION_STP2 definition
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



CLASS ZCL_PYC_BPC_SELECTION_STP2 IMPLEMENTATION.


  METHOD get_leading_time_selection.
*----------------------------------------------------------------------*
*Date      |User ID |Description              |Chg. Ref.|WkBh Req      *
*----------------------------------------------------------------------*
*01/03/2023|1106254 |Initial development      |STPS-187 |CFAK902866
*                    Copy of STP BPC selection           Revtrac - 1220
*                    class with addition of
*                    update DB para in testrun
*----------------------------------------------------------------------*

* Populate Selection Parameters
    CONSTANTS: lc_abkrs        TYPE pyd_par_type VALUE 'ABKRS',
               lc_period       TYPE pyd_par_type VALUE 'PERIOD',
               lc_test_run     TYPE pyd_par_type VALUE 'Z99_TEST_RUN',
               lc_update_db    TYPE pyd_par_type VALUE 'Z99_UPDATE_DB',
               lc_sel_timed    TYPE rsscr_name   VALUE 'PNPTIMED',
               lc_par_pnppabrp TYPE rsscr_name   VALUE 'PNPPABRP',
               lc_par_pnppabrj TYPE rsscr_name   VALUE 'PNPPABRJ',
               lc_par_pnpxabkr TYPE rsscr_name   VALUE 'PNPXABKR',
               lc_par_pnpabkrs TYPE rsscr_name   VALUE 'PNPABKRS',
               lc_par_submit   TYPE rsscr_name   VALUE 'SUBMIT',
               lc_par_update   TYPE rsscr_name   VALUE 'UPDATE',
               lc_par_runty    TYPE rsscr_name   VALUE 'RUNTY',
               lc_par_upddb    TYPE rsscr_name   VALUE 'UPD',
               lc_par_noupd    TYPE rsscr_name   VALUE 'NOUPD',
               lc_x            TYPE c            VALUE 'X',
               lc_i            TYPE tvarv_sign   VALUE 'I',
               lc_p            TYPE tvarv_sign   VALUE 'P',
               lc_eq           TYPE tvarv_opti   VALUE 'EQ',
               lc_two          TYPE c            VALUE '2',
               lc_blank        TYPE c            VALUE ' ',
               lc_par_spool    TYPE rsscr_name   VALUE 'SPOOL'.

    DATA:ls_par          TYPE pyd_s_resp,
         ls_sel_params   TYPE LINE OF rsparams_tt,
         lv_sel_name     TYPE char8,
         lv_pabrj        TYPE pabrj,
         lv_pabrp        TYPE pabrp,
         lc_tst_flag     TYPE c.

    CLEAR et_sel_params.

*check PERIOD exist; if exist set to the other period
    READ TABLE it_par INTO ls_par WITH KEY par_type = gc_par_type-period.
    IF sy-subrc = 0.
      CLEAR ls_sel_params .
      ls_sel_params-selname = lc_sel_timed.
      ls_sel_params-kind    = lc_p.
      ls_sel_params-sign    = lc_i .
      ls_sel_params-option  = lc_eq .
      ls_sel_params-low     = lc_two .
      APPEND ls_sel_params TO et_sel_params .
    ENDIF.


* Set ABKRS, Period Begda and Period Endda
    LOOP AT it_par INTO ls_par.
      CASE ls_par-par_type.
        WHEN lc_abkrs.
* Populate PNPXABKR
          lv_sel_name = lc_par_pnpxabkr.

          CLEAR ls_sel_params .
          MOVE-CORRESPONDING ls_par TO ls_sel_params.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
          ls_sel_params-selname = lv_sel_name.
          APPEND ls_sel_params TO et_sel_params  .

* Populate PNPABKRS
          lv_sel_name = lc_par_pnpabkrs.

          CLEAR ls_sel_params .
          MOVE-CORRESPONDING ls_par TO ls_sel_params.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
          ls_sel_params-selname = lv_sel_name.
          APPEND ls_sel_params TO et_sel_params  .

*if period exist, assign other period
        WHEN lc_period.
          lv_pabrj = ls_par-low(4).
          lv_pabrp = ls_par-low+4(2).

          lv_sel_name = lc_par_pnppabrp.
          CLEAR ls_sel_params .
          MOVE-CORRESPONDING ls_par TO ls_sel_params.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low     = lv_pabrp.
          APPEND ls_sel_params TO et_sel_params .

          lv_sel_name = lc_par_pnppabrj.
          CLEAR ls_sel_params .
          MOVE-CORRESPONDING ls_par TO ls_sel_params.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-low     = lv_pabrj.
          APPEND ls_sel_params TO et_sel_params .

        WHEN lc_test_run.
* Parameter Run Type
          lv_sel_name = lc_par_runty.
          CLEAR ls_sel_params .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-kind    = lc_p.
          ls_sel_params-sign    = lc_i .
          ls_sel_params-option  = lc_eq .
*Set lc_tst_flag if test indicator is 'X'
          ls_sel_params-low    = lc_tst_flag  = ls_par-low.
          APPEND ls_sel_params TO et_sel_params.

        WHEN  lc_update_db.
*Parameter Test Run with Update/No UPD to DB for STP2
*Check if test indicator is set as 'X'
          IF lc_tst_flag = lc_x.
*Check if UPD is set as 'X'
            IF ls_par-low = lc_x.
              lv_sel_name = lc_par_upddb.
              CLEAR ls_sel_params .
              ls_sel_params-selname = lv_sel_name.
              ls_sel_params-kind    = lc_p.
              ls_sel_params-sign    = lc_i .
              ls_sel_params-option  = lc_eq .
              ls_sel_params-low     = ls_par-low.
              APPEND ls_sel_params TO et_sel_params.
*Clear the noupd radiobutton if UPD is set to 'X'
              lv_sel_name = lc_par_noupd.
              CLEAR ls_sel_params .
              ls_sel_params-selname = lv_sel_name.
              ls_sel_params-kind    = lc_p.
              ls_sel_params-sign    = lc_i .
              ls_sel_params-option  = lc_eq .
              ls_sel_params-low     = lc_blank.
              APPEND ls_sel_params TO et_sel_params.
            ELSE.
              lv_sel_name = lc_par_noupd.
              CLEAR ls_sel_params .
              ls_sel_params-selname = lv_sel_name.
              ls_sel_params-kind    = lc_p.
              ls_sel_params-sign    = lc_i .
              ls_sel_params-option  = lc_eq .
              ls_sel_params-low     = lc_x.
              APPEND ls_sel_params TO et_sel_params.
            ENDIF.
            CLEAR lc_tst_flag.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

* Parameter Submit
    lv_sel_name = lc_par_submit.
    CLEAR ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind    = lc_p.
    ls_sel_params-sign    = lc_i .
    ls_sel_params-option  = lc_eq .
    ls_sel_params-low     = abap_true.
    APPEND ls_sel_params TO et_sel_params .

* Parameter Update
    lv_sel_name = lc_par_update.
    CLEAR ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind    = lc_p.
    ls_sel_params-sign    = lc_i .
    ls_sel_params-option  = lc_eq .
    ls_sel_params-low     = lc_blank.
    APPEND ls_sel_params TO et_sel_params .

* Parameter Spool Down load
    lv_sel_name = lc_par_spool.
    CLEAR ls_sel_params .
    ls_sel_params-selname = lv_sel_name.
    ls_sel_params-kind    = lc_p.
    ls_sel_params-sign    = lc_i .
    ls_sel_params-option  = lc_eq .
    ls_sel_params-low     = abap_true.
    APPEND ls_sel_params TO et_sel_params.

    IF lines( et_sel_params ) = 0 .
      RAISE EXCEPTION TYPE cx_pyc_cont
        EXPORTING
          textid = cx_pyc_cont=>get_leading_time_selection_err.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
