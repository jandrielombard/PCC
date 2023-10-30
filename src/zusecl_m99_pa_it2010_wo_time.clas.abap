class ZUSECL_M99_PA_IT2010_WO_TIME definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
protected section.

  types:
    begin of ty_time_data,
        pernr type pernr_d,
        infty type infty,
        subty type subty,
        begda type begda,
        endda type endda,
        seqnr type seqnr,
        anzhl type enanz.
    types: end of ty_time_data .
  types:
    tty_time_data type table of ty_time_data .
  types:
    begin of ty_allowerr_dtls,
        subty type subty,
        begda type begda,
        endda type endda,
        error type pyd_itemid,
      end of ty_allowerr_dtls .
  types:
    begin of ty_it2010_wo_time,
        pernr type pernr_d,
        subty type subty,
        begda type begda,
        endda type endda,
        error type pyd_itemid,
      end of ty_it2010_wo_time .
  types:
    tty_it2010_wo_time type table of ty_it2010_wo_time .

  constants MC_ITEMID_ALLOWERR type PYD_ITEMID value 'ALLOWERR' ##NO_TEXT.
  constants MC_INFTY_2010 type INFTY value '2010' ##NO_TEXT.
  constants MC_OVERVIEW_SUBTY type PYD_PAR_TYPE value 'Z99_OVERVIEW_SUBTYPE' ##NO_TEXT.
  constants MC_FILTER_SUBTY_01 type PYD_PAR_TYPE value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
  constants MC_FILTER_SUBTY_02 type PYD_PAR_TYPE value 'Z99_FILTER_SUBTY_02' ##NO_TEXT.
  constants MC_EXC_FILTER_SUBTY_01 type PYD_PAR_TYPE value 'Z99_EXC_FILTER_SUBTY_01' ##NO_TEXT.
  constants MC_EXC_FILTER_SUBTY_02 type PYD_PAR_TYPE value 'Z99_EXC_FILTER_SUBTY_02' ##NO_TEXT.
  constants MC_Z99_LGART_IND type PYD_PAR_TYPE value 'Z99_LGART_IND' ##NO_TEXT.
  constants MC_COMPARISON_OPERATOR type PYD_PAR_TYPE value 'Z99_COMPARISON_OPERATOR' ##NO_TEXT.
  constants MC_MISSING type PYD_ITEMID value 'MISSING' ##NO_TEXT.
  constants MC_MISMATCH type PYD_ITEMID value 'MISMATCH' ##NO_TEXT.
  constants MC_IT2001 type INFTY value '2001' ##NO_TEXT.
  constants MC_IT2002 type INFTY value '2002' ##NO_TEXT.
  constants MC_IT2010 type INFTY value '2010' ##NO_TEXT.
  data MT_OVERVIEW_SUBTYPE type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_FILTER_SUBTY_01 type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_FILTER_SUBTY_02 type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_IT2010_WO_TIME type TTY_IT2010_WO_TIME .
  data MS_IT2010_WO_TIME type TY_IT2010_WO_TIME .
  data MS_ALLOWERR_DTLS type TY_ALLOWERR_DTLS .
  data:
    mt_emp_time_data type standard table of ty_time_data .
  data MV_Z99_LGART_IND type BOOLEAN .
  data MV_COMPARISON_OPERATOR type ZHRAU_DE_CHECKOP .

  methods READ_TIME_DATA
    importing
      !IV_PERNR type PERNR_D
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    exporting
      value(ET_TIME_DATA) type TTY_TIME_DATA .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_IT2010_WO_TIME IMPLEMENTATION.


  method CHECK.
* All Relevant IT 2010 Wage types
    data: lt_it2010     type standard table of ty_time_data with non-unique key table_line,
          lt_it2001     type standard table of ty_time_data with non-unique key table_line,
          lt_it2002     type standard table of ty_time_data with non-unique key table_line,
          lt_allow_data type tty_time_data,
          lt_time_data  type tty_time_data.

    data: lt_it2010_wo_time type tty_it2010_wo_time,
          ls_it2010_wo_time type ty_it2010_wo_time.
    data: lv_comparison type boolean.
    data: ls_result  type   ty_s_result.

* All relevant employees with IT2001 Records
    select it2010~pernr, it2010~subty, it2010~begda, it2010~endda, sum( it2010~anzhl ) as anzhl
      into corresponding fields of table @lt_it2010 from pa2010 as it2010
       where it2010~pernr in @it_pernr_so
         and it2010~subty in @mt_filter_subty_01
         and it2010~sprps = ' '
         and it2010~uname in @mt_uname
*         and it2010~aedtm >= @mv_begda
*         and it2010~aedtm <= @mv_endda_plus1
         and it2010~aedtm >= @mv_change_begda
         and exists ( select 1
                        from pa0000 as it0000 inner join pa0001 as it0001 on
                             it0000~pernr = it0001~pernr
                       where it0000~pernr = it2010~pernr
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
                         and it0001~kostl in @mt_kostl )
      group by it2010~pernr, it2010~subty, it2010~begda, it2010~endda.

* Prepare Final IT 2010 records for the check
    sort lt_it2010 by pernr begda endda subty.
    loop at lt_it2010 into data(ls_it2010).
      if not mv_z99_lgart_ind eq mc_abap_yes.
        clear: ls_it2010-subty.
      endif.

      collect ls_it2010 into lt_allow_data.
    endloop.

* Read IT 2001 and IT 2002
    if not lt_it2010 is initial.
                                                            "IT 2001
      refresh: lt_it2001.
      select it2001~pernr as pernr, it2001~subty as subty,
             it2001~begda as begda, it2001~endda as endda, it2001~seqnr as seqnr, it2001~stdaz as anzhl
         into corresponding fields of table @lt_it2001 from pa2001 as it2001
          for all entries in @lt_allow_data
       where it2001~pernr = @lt_allow_data-pernr
         and it2001~subty in @mt_filter_subty_02
         and it2001~sprps = ' '
         and it2001~endda >= @lt_allow_data-begda
         and it2001~begda <= @lt_allow_data-endda.

* Prepare IT 2001 records for the check
      sort lt_it2001 by pernr begda endda subty.
      loop at lt_it2001 into data(ls_it2001).
        clear: ls_it2001-subty, ls_it2001-seqnr.
        collect ls_it2001 into lt_time_data.
      endloop.
                                                            "IT 2002
      refresh: lt_it2002.
      select it2002~pernr as pernr, it2002~subty as subty,
             it2002~begda as begda, it2002~endda as endda, it2002~seqnr as seqnr, it2002~stdaz as anzhl
         into corresponding fields of table @lt_it2002 from pa2002 as it2002
          for all entries in @lt_it2010
       where it2002~pernr = @lt_it2010-pernr
         and it2002~subty in @mt_filter_subty_02
         and it2002~sprps = ' '
         and it2002~endda >= @lt_it2010-begda
         and it2002~begda <= @lt_it2010-endda.

* Prepare IT 2001 records for the check
      sort lt_it2002 by pernr begda endda subty.
      loop at lt_it2002 into data(ls_it2002).
        clear: ls_it2002-subty, ls_it2002-seqnr.
        collect ls_it2002 into lt_time_data.
      endloop.
      refresh: lt_it2001, lt_it2002.

      sort lt_time_data by pernr begda endda.
    endif.

* Prepare data for result
    loop at lt_allow_data into data(ls_allow_data).
      read table lt_time_data into data(ls_time_data)
             with key pernr = ls_allow_data-pernr
                      begda = ls_allow_data-begda
                      endda = ls_allow_data-endda binary search.
      if sy-subrc ne 0.
* Collect data for Overview List
        clear: ls_it2010_wo_time.
        move: ls_allow_data-pernr to ls_it2010_wo_time-pernr,
              ls_allow_data-begda to ls_it2010_wo_time-begda,
              ls_allow_data-endda to ls_it2010_wo_time-endda,
              mc_missing          to ls_it2010_wo_time-error.
        append ls_it2010_wo_time to lt_it2010_wo_time.
      else.
        clear lv_comparison.
        call method zusecl_m99_pcc_chk_fp4_base=>dynamic_check_operation
          exporting
            iv_opcode = mv_comparison_operator
            iv_var01  = ls_allow_data-anzhl
            iv_var02  = ls_time_data-anzhl
          receiving
            rv_result = lv_comparison.

        if lv_comparison eq abap_true.
          clear: ls_it2010_wo_time.
          move: ls_allow_data-pernr to ls_it2010_wo_time-pernr,
                ls_allow_data-begda to ls_it2010_wo_time-begda,
                ls_allow_data-endda to ls_it2010_wo_time-endda,
                mc_mismatch         to ls_it2010_wo_time-error.
          append ls_it2010_wo_time to lt_it2010_wo_time.
        endif.
      endif.

    endloop.

* Collect the data for Overview
    append lines of lt_it2010_wo_time to mt_it2010_wo_time.
    sort mt_it2010_wo_time by pernr begda endda error.
    delete adjacent duplicates from mt_it2010_wo_time comparing all fields.

* Delete Duplicate Records for Employee for result preparation
    sort lt_it2010_wo_time by pernr ascending.
    delete adjacent duplicates from lt_it2010_wo_time comparing pernr.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it2010_wo_time into ls_it2010_wo_time.
* IT2010 Exists on the day with out IT2001 and IT2002
      ls_result-id = ls_it2010_wo_time-pernr.
      insert ls_result into table rt_result.
    endloop.

    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.
  endmethod.


  method ERR_OV_GET_LIST.
* Method for Overview list display
*-----------------------------------------------------------------------*
* CHANGE HISTORY                                                        *
*-----------------------------------------------------------------------*
*Chg |Date        |User ID  |Description                  |Change Label *
*-----------------------------------------------------------------------*
*001 |02-FEB-2023 |1130848  |MOLGA Enhancments            |CFAK902930   *
*-----------------------------------------------------------------------*
    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr,
      lv_value  type pyd_item_value.

    data: lv_text type pyd_name.
    data: lv_al_begda(10) type c,
          lv_ab_begda(10) type c,
          lv_ab_endda(10) type c.

    data: lv_char_value type char060.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_al_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    data lt_time_data type standard table of ty_time_data.
    data: lv_timedata_anzhl type enanz,
          lv_timedata_text  type pyd_name,
          lv_stext          type  sbttx.

    constants : lc_highlighter type char2 value '**'.

    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        "get parameters
        get_parameters( it_par         = it_par
                        io_res_context = io_res_context ).
        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

* Generic Message
          clear ls_err_ov-sfo_tab.
          lv_al_date_txt = text-001.
          lv_text = text-002.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_allowerr
              iv_text                     = lv_al_date_txt
              iv_value                    = lv_text
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.

          loop at lt_sfo_tab into ls_sfo_tab.
            lv_char_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_char_value
              changing
                p_struct2 = ms_allowerr_dtls.

            move-corresponding ms_allowerr_dtls to ms_it2010_wo_time.
            move lv_pernr to ms_it2010_wo_time-pernr.

            refresh: lt_time_data.
            call method me->read_time_data
              exporting
                iv_pernr     = lv_pernr
                iv_begda     = ms_it2010_wo_time-begda
                iv_endda     = ms_it2010_wo_time-endda
              importing
                et_time_data = lt_time_data.

            loop at lt_time_data into data(ls_time_data).
              call function 'HR_GET_SUBTYPE_TEXT'
                exporting
                  infty               = ls_time_data-infty
                  subty               = ls_time_data-subty
                  persnr              = lv_pernr
                  begda               = ls_time_data-begda
                  endda               = ls_time_data-endda
*                 molga               = mc_molga_au           "MOD001--
                  molga               = mv_molga              "MOD001++
                importing
                  stext               = lv_stext
                exceptions
                  infty_not_found     = 1
                  subty_not_found     = 2
                  infty_not_supported = 3
                  others              = 4.

              write: ls_time_data-begda to lv_al_begda dd/mm/yyyy.

              if ls_time_data-subty in mt_overview_subtype.
                concatenate lc_highlighter lv_stext '(' ls_time_data-subty ')' into lv_timedata_text.
              else.
                concatenate lv_stext '(' ls_time_data-subty ')' into lv_timedata_text.
              endif.
              message i015(zhrpy_pcc_msg)
                   with ls_time_data-infty lv_timedata_text ls_time_data-anzhl into lv_text.

              lv_al_date_txt = lv_al_begda.
              call method me->add_record_to_sfo_tab
                exporting
                  iv_itemid                   = mc_itemid_allowerr
                  iv_text                     = lv_al_date_txt
                  iv_value                    = lv_text
                  iv_text_for_1st_record_only = abap_true
                changing
                  ct_sfo_tab                  = ls_err_ov-sfo_tab.
            endloop.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_it2010_wo_time into ms_it2010_wo_time
            where pernr = lv_pernr.

            move-corresponding ms_it2010_wo_time to ms_allowerr_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_allowerr_dtls
              changing
                p_struct2 = lv_char_value.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_allowerr.
            ls_sfo-value = lv_char_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.


  METHOD GET_SPECIFC_CUSTMIZING.
* Read Check Specific Parameters
    DATA lt_param_so  TYPE /iwbep/t_cod_select_options.

    TRY.
* Read IT 2010 Subtypes
        CALL METHOD me->read_range_parameter
          EXPORTING
            iv_inc_par_type  = me->mc_filter_subty_01
            iv_exc_par_type  = me->mc_exc_filter_subty_01
          CHANGING
            ct_parameter_tab = mt_filter_subty_01.

* Read IT 2001 Subtypes
        CALL METHOD me->read_range_parameter
          EXPORTING
            iv_inc_par_type  = me->mc_filter_subty_02
            iv_exc_par_type  = me->mc_exc_filter_subty_02
          CHANGING
            ct_parameter_tab = mt_filter_subty_02.

*Read Overview Subtypes
        CALL METHOD me->read_range_parameter
          EXPORTING
            iv_inc_par_type  = me->mc_overview_subty
          CHANGING
            ct_parameter_tab = mt_overview_subtype.

* Wage type Indicator
        mv_z99_lgart_ind =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_z99_lgart_ind
                                                                  it_par = mo_context->mt_par ).

* Comparison Oparator
        mv_comparison_operator =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_comparison_operator
                                                                        it_par      = mo_context->mt_par ).
        IF mv_comparison_operator IS INITIAL.
          MOVE 'GT' TO mv_comparison_operator.
        ENDIF.

      CATCH cx_pyd_fnd INTO DATA(lo_exception).
    ENDTRY.

  ENDMETHOD.


  METHOD READ_TIME_DATA.
* Read Time Data from IT 2001, 2002 and 2010
    DATA: lt_it2010    TYPE STANDARD TABLE OF ty_time_data WITH NON-UNIQUE KEY table_line,
          lt_it2001    TYPE STANDARD TABLE OF ty_time_data WITH NON-UNIQUE KEY table_line,
          lt_it2002    TYPE STANDARD TABLE OF ty_time_data WITH NON-UNIQUE KEY table_line,
          lt_time_data TYPE STANDARD TABLE OF ty_time_data WITH NON-UNIQUE KEY table_line.

    DATA: lt_filter_subty_02_overview TYPE /iwbep/t_cod_select_options.

    DATA:  lv_tabix TYPE sy-tabix.

    APPEND LINES OF  : mt_overview_subtype        TO lt_filter_subty_02_overview,
                       mt_filter_subty_02         TO lt_filter_subty_02_overview.

    REFRESH: lt_it2010.
    SELECT it2010~pernr AS pernr, it2010~subty AS subty,
           it2010~begda AS begda, it2010~endda AS endda, it2010~seqnr AS seqnr, it2010~anzhl AS anzhl
       INTO CORRESPONDING FIELDS OF TABLE @lt_it2010 FROM pa2010 AS it2010
     WHERE it2010~pernr = @iv_pernr
       AND it2010~subty IN @mt_filter_subty_01
       AND it2010~sprps = ' '
       AND it2010~endda >= @iv_begda
       AND it2010~begda <= @iv_endda.
    LOOP AT lt_it2010 INTO DATA(ls_it2010).
      lv_tabix = sy-tabix.
      MOVE mc_it2010 TO ls_it2010-infty.
      MODIFY lt_it2010 INDEX lv_tabix FROM ls_it2010 TRANSPORTING infty.
    ENDLOOP.
                                                            "IT 2001
    REFRESH: lt_it2001.
    SELECT it2001~pernr AS pernr, it2001~subty AS subty,
           it2001~begda AS begda, it2001~endda AS endda, it2001~seqnr AS seqnr, it2001~stdaz AS anzhl
       INTO CORRESPONDING FIELDS OF TABLE @lt_it2001 FROM pa2001 AS it2001
     WHERE it2001~pernr = @iv_pernr
*       AND it2001~subty IN @mt_filter_subty_02
        AND it2001~subty IN @lt_filter_subty_02_overview
       AND it2001~sprps = ' '
       AND it2001~endda >= @iv_begda
       AND it2001~begda <= @iv_endda.
    LOOP AT lt_it2001 INTO DATA(ls_it2001).
      lv_tabix = sy-tabix.
      MOVE mc_it2001 TO ls_it2001-infty.
      MODIFY lt_it2001 INDEX lv_tabix FROM ls_it2001 TRANSPORTING infty.
    ENDLOOP.

                                                            "IT 2002
    REFRESH: lt_it2002.
    SELECT it2002~pernr AS pernr, it2002~subty AS subty,
           it2002~begda AS begda, it2002~endda AS endda, it2002~seqnr AS seqnr, it2002~stdaz AS anzhl
       INTO CORRESPONDING FIELDS OF TABLE @lt_it2002 FROM pa2002 AS it2002
     WHERE it2002~pernr = @iv_pernr
*       AND it2002~subty IN @mt_filter_subty_02
       AND it2002~subty IN @lt_filter_subty_02_overview
       AND it2002~sprps = ' '
       AND it2002~endda >= @iv_begda
       AND it2002~begda <= @iv_endda.
    LOOP AT lt_it2002 INTO DATA(ls_it2002).
      lv_tabix = sy-tabix.
      MOVE mc_it2002 TO ls_it2002-infty.
      MODIFY lt_it2002 INDEX lv_tabix FROM ls_it2002 TRANSPORTING infty.
    ENDLOOP.

    APPEND LINES OF lt_it2010 TO lt_time_data.
    APPEND LINES OF lt_it2001 TO lt_time_data.
    APPEND LINES OF lt_it2002 TO lt_time_data.
    REFRESH: lt_it2010, lt_it2001, lt_it2002.

    SORT lt_time_data BY pernr begda endda subty.
    et_time_data[] = lt_time_data[].

  ENDMETHOD.
ENDCLASS.
