class zcl_m99_pa_it2010_w_it2001 definition
  public
  inheriting from zcl_m99_pcc_chk_fp4_base
  final
  create public .

  public section.
protected section.

  types:
    begin of ty_it2010,
      pernr type pernr_d,
      lgart type lgart,
      begda type begda,
      endda type endda,
      anzhl type anzhl,
    end of ty_it2010 .
  types:
    begin of ty_it2001,
      pernr    type pernr_d,
      awart    type awart,
      ab_begda type begda,
      ab_endda type endda,
      seqnr    type seqnr,
      stdaz    type abstd,
      alldf    type alldf,
    end of ty_it2001 .
  types:
    begin of ty_allowerr_dtl,
      lgart    type lgart,
      begda    type begda,
      anzhl    type anzhl,
      awart    type awart,
      ab_begda type begda,
      stdaz    type abstd,
    end of ty_allowerr_dtl .
  types:
    begin of ty_it2010_w_it2001,
      pernr    type pernr_d,
      altext   type ltext,
      abtext   type ltext,
      lgart    type lgart,
      begda    type begda,
      endda    type endda,
      anzhl    type anzhl,
      awart    type awart,
      ab_begda type begda,
      ab_endda type endda,
      stdaz    type abstd,
    end of ty_it2010_w_it2001 .
  types:
    tty_it2010_w_it2001 type table of ty_it2010_w_it2001 .

  constants MC_INFTY_2010 type INFTY value '2010' ##NO_TEXT.
  constants MC_INFTY_2001 type INFTY value '2001' ##NO_TEXT.
  constants MC_ITEMID_ALLOWERR type PYD_ITEMID value 'ALLOWERR' ##NO_TEXT.
  constants MC_FILTER_SUBTY_01 type PYD_PAR_TYPE value 'Z99_FILTER_SUBTY_01' ##NO_TEXT.
  constants MC_FILTER_SUBTY_02 type PYD_PAR_TYPE value 'Z99_FILTER_SUBTY_02' ##NO_TEXT.
  constants MC_EXC_FILTER_SUBTY_01 type PYD_PAR_TYPE value 'Z99_EXC_FILTER_SUBTY_01' ##NO_TEXT.
  constants MC_EXC_FILTER_SUBTY_02 type PYD_PAR_TYPE value 'Z99_EXC_FILTER_SUBTY_02' ##NO_TEXT.
  constants MC_FULLDAY_IND type PYD_PAR_TYPE value 'Z99_FULLDAY_IND' ##NO_TEXT.
  constants MC_NUM_HIGH_02 type PYD_PAR_TYPE value 'Z99_NUM_HIGH_02' ##NO_TEXT.
  constants MC_NUM_LOW_02 type PYD_PAR_TYPE value 'Z99_NUM_LOW_02' ##NO_TEXT.
  data MT_FILTER_SUBTY_01 type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_FILTER_SUBTY_02 type /IWBEP/T_COD_SELECT_OPTIONS .
  data MT_IT2010_W_IT2001 type TTY_IT2010_W_IT2001 .
  data MS_IT2010_W_IT2001 type TY_IT2010_W_IT2001 .
  data MS_ALLOWERR_DTL type TY_ALLOWERR_DTL .
  data MV_FULLDAY_IND type BOOLEAN .
  data MV_NUM_LOW_02 type PRANZ .
  data MV_NUM_HIGH_02 type PRANZ .

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  methods GET_SPECIFC_CUSTMIZING
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_M99_PA_IT2010_W_IT2001 IMPLEMENTATION.


  METHOD check.
* All relavant IT2010 Records with out corresponding IT2001
    DATA: lt_it2010 TYPE TABLE OF ty_it2010,
          ls_it2010 TYPE ty_it2010.
    DATA: lt_it2001 TYPE TABLE OF ty_it2001,
          ls_it2001 TYPE ty_it2001.
    DATA: lt_it2010_w_it2001 TYPE tty_it2010_w_it2001,
          ls_it2010_w_it2001 TYPE ty_it2010_w_it2001.
    DATA: lv_retcd TYPE sy-subrc,
          lv_index TYPE sy-tabix,
          lv_subrc TYPE sy-subrc.

    DATA: ls_result TYPE ty_s_result.

* All relevant employees with IT2010 Records and IT2001 records
    SELECT it2010~pernr AS pernr, it2010~lgart AS lgart,
           it2010~begda AS begda, it2010~endda AS endda, it2010~anzhl AS anzhl
       INTO CORRESPONDING FIELDS OF TABLE @lt_it2010 FROM pa2010 AS it2010
         WHERE it2010~pernr IN @it_pernr_so
          AND  it2010~subty IN @mt_filter_subty_01
           AND it2010~uname IN @mt_uname
           AND it2010~sprps = ' '
*           and it2010~aedtm >= @mv_begda
*           and it2010~aedtm <= @mv_endda_plus1
           AND it2010~aedtm >= @mv_change_begda
           AND EXISTS ( SELECT 1
                          FROM pa0000 AS it0000 INNER JOIN pa0001 AS it0001 ON
                               it0000~pernr = it0001~pernr
                         WHERE it0000~pernr = it2010~pernr
                           AND it0000~begda <= @mv_endda
                           AND it0000~endda >= @mv_begda
                           AND it0000~stat2 IN @mt_stat2
                           AND it0000~sprps = ' '
                           AND it0001~begda <= @mv_endda
                           AND it0001~endda >= @mv_begda
                           AND it0001~sprps = ' '
                           AND it0001~abkrs IN @mt_payroll_areas
                           AND it0001~bukrs IN @mt_bukrs
                           AND it0001~werks IN @mt_werks
                           AND it0001~btrtl IN @mt_btrtl
                           AND it0001~persg IN @mt_persg
                           AND it0001~persk IN @mt_persk
                           AND it0001~kostl IN @mt_kostl ).

    SORT lt_it2010 BY pernr begda endda lgart.

* Read Corresponding IT 2001 records for the selection
* All relevant employees with IT2010 Records and IT2001 records
    IF NOT lt_it2010 IS INITIAL.
      SELECT it2001~pernr AS pernr, it2001~awart AS awart,
        it2001~begda AS ab_begda, it2001~endda AS ab_endda, it2001~seqnr AS seqnr,
        it2001~stdaz AS stdaz, it2001~alldf AS alldf
        INTO CORRESPONDING FIELDS OF TABLE @lt_it2001 FROM pa2001 AS it2001
         FOR ALL ENTRIES IN @lt_it2010
      WHERE it2001~pernr = @lt_it2010-pernr
        AND it2001~subty IN @mt_filter_subty_02
        AND it2001~sprps IN @mt_sprps
        AND it2001~endda >= @lt_it2010-begda
        AND it2001~begda <= @lt_it2010-endda.

      IF mv_fullday_ind EQ mc_abap_yes.
        DELETE lt_it2001 WHERE alldf NE abap_true.
      ELSEIF  mv_fullday_ind EQ mc_abap_no.
        DELETE lt_it2001 WHERE alldf EQ abap_true.
      ELSE.
*            <Do Nothing >
      ENDIF.
*Delete IT2001 data where Absence hours which is not within the Paramter ranges
      DELETE lt_it2001 WHERE stdaz NOT BETWEEN mv_num_low_02 AND mv_num_high_02.

      SORT lt_it2001 BY pernr ab_begda ab_endda awart seqnr.
    ENDIF.

* Prepare data for result
    LOOP AT lt_it2010 INTO ls_it2010.
* Index Looping for better performance
      CLEAR: lv_index, lv_subrc.
      READ TABLE lt_it2001 INTO ls_it2001
            WITH KEY pernr = ls_it2010-pernr BINARY SEARCH.
      lv_index = sy-tabix.
      lv_subrc = sy-subrc.

      WHILE lv_subrc EQ 0.
* Check IT 2001 data
        IF ls_it2001-ab_begda <= ls_it2010-endda AND
           ls_it2001-ab_endda >= ls_it2010-begda.
* IT 20001 Exists on the day
* Collect data for Overview List
          MOVE: ls_it2010-pernr TO ls_it2010_w_it2001-pernr,
                ls_it2010-lgart TO ls_it2010_w_it2001-lgart,
                ls_it2010-begda TO ls_it2010_w_it2001-begda,
                ls_it2010-endda TO ls_it2010_w_it2001-endda,
                ls_it2010-anzhl TO ls_it2010_w_it2001-anzhl,
                ls_it2001-awart TO ls_it2010_w_it2001-awart,
                ls_it2001-ab_begda TO ls_it2010_w_it2001-ab_begda,
                ls_it2001-ab_endda TO ls_it2010_w_it2001-ab_endda,
                ls_it2001-stdaz TO ls_it2010_w_it2001-stdaz.
          APPEND ls_it2010_w_it2001 TO lt_it2010_w_it2001.
        ENDIF.
* Check next record
        lv_index = lv_index + 1.
        READ TABLE lt_it2001 INTO ls_it2001  INDEX lv_index.
        lv_subrc = sy-subrc.

        IF NOT ls_it2001-pernr = ls_it2010-pernr.
* Exit the loop.
          lv_subrc = 99.
        ENDIF.
      ENDWHILE.

    ENDLOOP.

* Collect data for Overview
    APPEND LINES OF lt_it2010_w_it2001 TO mt_it2010_w_it2001.

* Delete Duplicate Records for Employee for result preparation
    SORT lt_it2010_w_it2001 BY pernr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_it2010_w_it2001 COMPARING pernr.
* Populate Result Table
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    LOOP AT lt_it2010_w_it2001 INTO ls_it2010_w_it2001.
* IT 2001 Exists on the day
      ls_result-id = ls_it2010_w_it2001-pernr.
      INSERT ls_result INTO TABLE rt_result.
    ENDLOOP.

    " register event when transaction complete we need clear cache
    SET HANDLER handle_init_buffers FOR mo_fnd_factory->mo_transaction.

  ENDMETHOD.


  method err_ov_get_list.
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
      lv_value  type char060.

    data: lv_text type string.
    data: lv_al_begda(10) type c,
          lv_ab_begda(10) type c,
          lv_ab_endda(10) type c.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_al_date_txt type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    data: lv_return type bapireturn1.

* Populate SFO Tab
    case iv_access_mode.

* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

          clear ls_err_ov-sfo_tab.
          clear ls_sfo.

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
            lv_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_value
              changing
                p_struct2 = ms_allowerr_dtl.

            move-corresponding ms_allowerr_dtl to ms_it2010_w_it2001.
            move lv_pernr to ms_it2010_w_it2001-pernr.

            write: ms_it2010_w_it2001-begda to lv_al_begda dd/mm/yyyy.
            lv_al_date_txt = lv_al_begda.
* Read IT2010 Subtype text
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_infty_2010
                subty               = ms_it2010_w_it2001-lgart
                persnr              = ms_it2010_w_it2001-pernr
                begda               = ms_it2010_w_it2001-begda
                endda               = ms_it2010_w_it2001-begda
*               molga               = mc_molga_au           "MOD001--
                molga               = mv_molga              "MOD001++
              importing
                stext               = ms_it2010_w_it2001-altext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.

* Read IT2001 Suibtype text
            call function 'HR_GET_SUBTYPE_TEXT'
              exporting
                infty               = mc_infty_2001
                subty               = ms_it2010_w_it2001-awart
                persnr              = ms_it2010_w_it2001-pernr
                begda               = ms_it2010_w_it2001-ab_begda
                endda               = ms_it2010_w_it2001-ab_begda
*               molga               = mc_molga_au           "MOD001--
                molga               = mv_molga              "MOD001++
              importing
                stext               = ms_it2010_w_it2001-abtext
              exceptions
                infty_not_found     = 1
                subty_not_found     = 2
                infty_not_supported = 3
                others              = 4.

* Build error text IT2010
            concatenate ms_it2010_w_it2001-altext '(' ms_it2010_w_it2001-lgart ')'
             into ms_it2010_w_it2001-altext.
            concatenate ms_it2010_w_it2001-abtext '(' ms_it2010_w_it2001-awart ')'
             into ms_it2010_w_it2001-abtext.

* Build error text IT2010
            message i007(zhrpy_pcc_msg)
             with ms_it2010_w_it2001-altext ms_it2010_w_it2001-anzhl into lv_text.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_allowerr
                iv_text                     = lv_al_date_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.

* Build error text IT2001
            message i007(zhrpy_pcc_msg)
              with ms_it2010_w_it2001-abtext ms_it2010_w_it2001-stdaz into lv_text.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_allowerr
                iv_text                     = lv_al_date_txt
                iv_value                    = lv_text
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.

          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_it2010_w_it2001 into ms_it2010_w_it2001
            where pernr = lv_pernr.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_allowerr.
            move-corresponding ms_it2010_w_it2001 to ms_allowerr_dtl.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_allowerr_dtl
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


  METHOD get_specifc_custmizing.
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

* Fullday Indicator
        mv_fullday_ind =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_fullday_ind
                                                                  it_par = mo_context->mt_par ).

* UNPAID leave Num High
        mv_num_high_02 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_high_02
                                                                  it_par = mo_context->mt_par ).

* UNPAID leave Num Low
        mv_num_low_02 =  cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = me->mc_num_low_02
                                                                  it_par = mo_context->mt_par ).

      CATCH cx_pyd_fnd INTO DATA(lo_exception).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
