class ZUSECL_PYC_BPC_SELECT_OC_STP2 definition
  public
  inheriting from CL_PYC_BPC_SELECTION_PNP
  create public .

public section.

  constants GC_SUB_CLSNM type CLASSNAME value 'CL_PYC_BPC_SELECTION_OC_PNP_P' ##NO_TEXT.
protected section.

  methods GET_OTHER_PARAMS
    redefinition .
  methods CONVERT_DEV_SEL_OBJ_TO_PARAMS
    redefinition .
private section.
ENDCLASS.



CLASS ZUSECL_PYC_BPC_SELECT_OC_STP2 IMPLEMENTATION.


  METHOD CONVERT_DEV_SEL_OBJ_TO_PARAMS.
*----------------------------------------------------------------------*
*Date      |User ID |Description              |Chg. Ref.|WkBh Req      *
*----------------------------------------------------------------------*
*01/03/2023|1106254 |Initial development      |STPS-187 |CFAK902866
*                    Copy of STP BPC OC                  Revtrac - 1220
*                    selection class
*----------------------------------------------------------------------*
* Populate PNPINDEX selection

    DATA:ls_dev_sel_obj TYPE if_pyd_shadow_access=>ty_s_shadow_item,
         ls_sel_params  TYPE rsparams.

    CONSTANTS :lc_i            TYPE tvarv_sign VALUE 'I',
               lc_eq           TYPE tvarv_opti VALUE 'EQ',
               lc_p            TYPE tvarv_sign VALUE 'P',
               lc_sel_pnpindex TYPE rsscr_name VALUE 'PNPINDEX',
               lc_pernr        TYPE char5      VALUE 'PERNR'.

    LOOP AT it_dev_sel_obj INTO ls_dev_sel_obj WHERE par_type = lc_pernr.
      CLEAR ls_sel_params.
      MOVE lc_i  TO ls_sel_params-sign.
      MOVE lc_p  TO ls_sel_params-kind.
      MOVE lc_eq TO ls_sel_params-option.
      MOVE ls_dev_sel_obj-par_val TO ls_sel_params-low.
      ls_sel_params-selname = lc_sel_pnpindex .
      APPEND ls_sel_params TO et_sel_params.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_OTHER_PARAMS.
*----------------------------------------------------------------------*
*Date      |User ID |Description              |Chg. Ref.|WkBh Req      *
*----------------------------------------------------------------------*
*01/03/2023|1106254 |Initial development      |STPS-187 |CFAK902866
*                    Copy of STP BPC selection           Revtrac - 1220
*                    class with addition of
*                    update DB para in testrun
*----------------------------------------------------------------------*
* Populate Selection Parameters

    DATA:lv_proc_inst_id TYPE pyc_proc_inst_id,
         lo_sel_access   TYPE REF TO if_pyc_selection_oc_access,
         lt_selection    TYPE if_pyc_selection_oc_access=>ty_t_selection,
         ls_selection    TYPE if_pyc_selection_oc_access=>ty_s_selection,
         lt_par          TYPE if_pyd_fnd_types=>ty_t_resp,
         ls_sel_params   TYPE rsparams,
         lx_pyc_frw      TYPE REF TO cx_pyc_frw,
         lx_pyd_fnd      TYPE REF TO cx_pyd_fnd,
         lv_sel_name     TYPE char8,
         lc_tst_flag     TYPE c.

    CONSTANTS: lc_sel_bondt    TYPE rsscr_name   VALUE 'PNPBONDT',
               lc_sel_payty    TYPE rsscr_name   VALUE 'PNPPAYTY',
               lc_sel_payid    TYPE rsscr_name   VALUE 'PNPPAYID',
               lc_sel_pnpxabkr TYPE rsscr_name   VALUE 'PNPXABKR',
               lc_sel_pnptimed TYPE rsscr_name   VALUE 'PNPTIMED',
               lc_sel_pnptimr9 TYPE rsscr_name   VALUE 'PNPTIMR9',
               lc_sel_pnptimra TYPE rsscr_name   VALUE 'PNPTIMRA',
               lc_sel_pnppabrj TYPE rsscr_name   VALUE 'PNPPABRJ',
               lc_sel_pnppabrp TYPE rsscr_name   VALUE 'PNPPABRP',
               lc_sel_pnpindex TYPE rsscr_name   VALUE 'PNPINDEX',
               lc_test_run     TYPE pyd_par_type VALUE 'Z99_TEST_RUN',
               lc_update_db    TYPE pyd_par_type VALUE 'Z99_UPDATE_DB',
               lc_par_submit   TYPE rsscr_name   VALUE 'SUBMIT',
               lc_par_update   TYPE rsscr_name   VALUE 'UPDATE',
               lc_par_runty    TYPE rsscr_name   VALUE 'RUNTY',
               lc_par_upddb    TYPE rsscr_name   VALUE 'UPD',
               lc_par_noupd    TYPE rsscr_name   VALUE 'NOUPD',
               lc_x            TYPE c            VALUE 'X',
               lc_two          TYPE c            VALUE '2',
               lc_three        TYPE c            VALUE '3',
               lc_blank        TYPE c            VALUE ' ',
               lc_i            TYPE tvarv_sign   VALUE 'I',
               lc_eq           TYPE tvarv_opti   VALUE 'EQ',
               lc_p            TYPE tvarv_sign   VALUE 'P',
               lc_s            TYPE tvarv_sign   VALUE 'S',
               lc_par_spool    TYPE rsscr_name   VALUE 'SPOOL'.

    TRY .
        lv_proc_inst_id = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = cl_pyc_rt_facade=>gcs_par_type-proc_inst
                                                                it_par      = io_context->mt_par ).

        lo_sel_access = cl_pyc_selection_oc_access=>get_instance( ).
        lt_selection = lo_sel_access->selection_list_get( iv_proc_inst_id = lv_proc_inst_id ).
        lt_par  = io_context->mt_par.

        "If no selection object is found, raise exception
        IF lt_selection IS INITIAL.
          RAISE EXCEPTION TYPE cx_pyc_cont
            EXPORTING
              textid = cx_pyc_cont=>oc_empty_selection.
        ENDIF.

        " Inject parameters from selection package infomation
        READ TABLE lt_selection INTO ls_selection WITH KEY package_id = iv_rpt_chain_id.
        IF sy-subrc = 0.
          IF ls_selection-pabrj IS INITIAL.
            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_pnptimed .
            ls_sel_params-kind    = lc_p.
            ls_sel_params-low     = lc_three.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.

            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_bondt .
            ls_sel_params-low     = ls_selection-bondt.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.

            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_payty .
            ls_sel_params-low     = ls_selection-payty.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.

            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_payid .
            ls_sel_params-low     = ls_selection-payid.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.

            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_pnpxabkr .
            ls_sel_params-low     = ls_selection-abkrs.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.

          ELSE.
            " double check mandatory info to be used for selection parameters
            IF ls_selection-pabrj IS INITIAL
              OR ls_selection-pabrp IS INITIAL
              OR ls_selection-abkrs IS INITIAL.
              RAISE EXCEPTION TYPE cx_pyc_cont. " David, tbd, message content
            ENDIF.

            " fill leading ABKRS and Period
            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_pnpxabkr .
            ls_sel_params-low     = ls_selection-abkrs.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.

            " fill leading period to set it as other period, tbd, need to verify PNP or PNPCE
            IF iv_rpt_is_pnpce EQ abap_true.
              CLEAR ls_sel_params.
              ls_sel_params-selname = lc_sel_pnptimed .
              ls_sel_params-kind    = lc_s.
              ls_sel_params-low     = lc_two.
              ls_sel_params-sign    = lc_i.
              ls_sel_params-option  = lc_eq.
              APPEND ls_sel_params TO et_sel_params.
            ELSE.
              CLEAR ls_sel_params.
              ls_sel_params-selname = lc_sel_pnptimr9 .
              ls_sel_params-kind    = lc_p.
              ls_sel_params-low     = lc_blank.
              ls_sel_params-sign    = lc_i.
              ls_sel_params-option  = lc_eq.
              APPEND ls_sel_params TO et_sel_params.

              CLEAR ls_sel_params.
              ls_sel_params-selname = lc_sel_pnptimra .
              ls_sel_params-kind    = lc_p.
              ls_sel_params-low     = lc_x.
              ls_sel_params-sign    = lc_i.
              ls_sel_params-option  = lc_eq.
              APPEND ls_sel_params TO et_sel_params.
            ENDIF.

            " fill leading time selection
            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_pnppabrj .
            ls_sel_params-low     = ls_selection-pabrj.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.

            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_pnppabrp .
            ls_sel_params-low     = ls_selection-pabrp.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.
          ENDIF.

* Common Parameters
* Personal Number
          LOOP AT lt_selection INTO ls_selection WHERE package_id = iv_rpt_chain_id.
            CLEAR ls_sel_params.
            ls_sel_params-selname = lc_sel_pnpindex .
            ls_sel_params-low     = ls_selection-pernr.
            ls_sel_params-sign    = lc_i.
            ls_sel_params-option  = lc_eq.
            APPEND ls_sel_params TO et_sel_params.
          ENDLOOP.

* Parameter Submit
          lv_sel_name = lc_par_submit.
          CLEAR ls_sel_params .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-kind    = lc_p.
          ls_sel_params-sign    = lc_i.
          ls_sel_params-option  = lc_eq.
          ls_sel_params-low = abap_true.
          APPEND ls_sel_params TO et_sel_params .

* Parameter Update
          lv_sel_name = lc_par_update.
          CLEAR ls_sel_params .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-kind    = lc_p .
          ls_sel_params-sign    = lc_i.
          ls_sel_params-option  = lc_eq.
          ls_sel_params-low     = lc_blank.
          APPEND ls_sel_params TO et_sel_params .

* Set Run type
          LOOP AT lt_par INTO DATA(ls_par).
            CASE ls_par-par_type.
              WHEN lc_test_run.
* Parameter Run Type
                lv_sel_name = lc_par_runty.
                CLEAR : ls_sel_params , lc_tst_flag.
                ls_sel_params-selname = lv_sel_name.
                ls_sel_params-kind    = lc_p .
                ls_sel_params-sign    = lc_i.
                ls_sel_params-option  = lc_eq.
*Set lc_tst_flag if test indicator is 'X'
                ls_sel_params-low     = lc_tst_flag = ls_par-low.
                APPEND ls_sel_params TO et_sel_params.

*Test run with Update Database
              WHEN  lc_update_db.
* Check if parameter Test Run is set
                IF lc_tst_flag = lc_x.
*Check if parameter update DB is set
                  IF ls_par-low = lc_x.
                    lv_sel_name = lc_par_upddb.
                    CLEAR ls_sel_params .
                    ls_sel_params-selname = lv_sel_name.
                    ls_sel_params-kind    = lc_p .
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
*Set the no update DB radiobutton for test run
                    lv_sel_name = lc_par_noupd.
                    CLEAR ls_sel_params .
                    ls_sel_params-selname = lv_sel_name.
                    ls_sel_params-kind    = lc_p .
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

* Parameter Spool Download
          lv_sel_name = lc_par_spool.
          CLEAR ls_sel_params .
          ls_sel_params-selname = lv_sel_name.
          ls_sel_params-kind    = lc_p.
          ls_sel_params-sign    = lc_i.
          ls_sel_params-option  = lc_eq.
          ls_sel_params-low     = abap_true.
          APPEND ls_sel_params TO et_sel_params.
        ENDIF.

      CATCH cx_pyc_frw INTO lx_pyc_frw.
        RAISE EXCEPTION TYPE cx_pyc_cont EXPORTING previous = lx_pyc_frw.
      CATCH cx_pyd_fnd INTO lx_pyd_fnd.
        RAISE EXCEPTION TYPE cx_pyc_cont EXPORTING previous = lx_pyd_fnd.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
