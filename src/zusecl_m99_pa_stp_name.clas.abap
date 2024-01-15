class ZUSECL_M99_PA_STP_NAME definition
  public
  inheriting from ZuseCL_M99_PCC_CHK_FP4_BASE
  final
  create public .

public section.
protected section.

  types:
* IT 0002 STP Data
    begin of ty_it2_stpdata,
        pernr type p0002-pernr,
        nachn type p0002-nachn,
        vorna type p0002-vorna,
        midnm type p0002-midnm.
    types: end of ty_it2_stpdata .
  types:
    tty_it2_stpdata type table of ty_it2_stpdata .
  types:
* IT 0006 STP Data
    begin of ty_it6_stpdata,
        pernr type p0002-pernr,
        stras type p0006-stras,
        locat type p0006-locat,
        ort01 type p0006-ort01.
    types: end of ty_it6_stpdata .
  types:
    tty_it6_stpdata type table of ty_it6_stpdata .
  types:
* stp error details
    begin of ty_stperr_dtls,
        pernr     type p0002-pernr,
        tabname   type tabname,
        fieldname type fieldname.
    types: end of ty_stperr_dtls .
  types:
    tty_stperr_dtls type table of ty_stperr_dtls .
  types:
* stp error field details
    begin of ty_errfield_dtls,
        tabname   type tabname,
        fieldname type fieldname.
    types: end of ty_errfield_dtls .
  types:
    tty_errfield_dtls type table of ty_errfield_dtls .

  data MT_IT2_STPDATA type TTY_IT2_STPDATA .
  data MS_IT2_STPDATA type TY_IT2_STPDATA .
  data MT_IT6_STPDATA type TTY_IT6_STPDATA .
  data MS_IT6_STPDATA type TY_IT6_STPDATA .
  data MT_STPERR_DTLS type TTY_STPERR_DTLS .
  data MS_STPERR_DTLS type TY_STPERR_DTLS .
  data MS_ERRFIELD_DTLS type TY_ERRFIELD_DTLS .
  constants MC_IT2CHAR_PATTERN type STRING value '([0-9a-zA-Z \.,\?\(\)\{\}:;''\|\-_=\\/@#$%\*=&"])*' ##NO_TEXT.
  constants MC_IT6CHAR_PATTERN type STRING value '([0-9a-zA-Z \.,''\(\)\''\/\-\#\&])*' ##NO_TEXT.
  constants MC_ITEMID_CHARERR type PYD_ITEMID value 'CHARERR' ##NO_TEXT.
  constants C_0002 type TABNAME value 'PA0002' ##NO_TEXT.
  constants C_0006 type TABNAME value 'PA0006' ##NO_TEXT.
  constants C_NACHN type FIELDNAME value 'NACHN' ##NO_TEXT.
  constants C_VORNA type FIELDNAME value 'VORNA' ##NO_TEXT.
  constants C_MIDNM type FIELDNAME value 'MIDNM' ##NO_TEXT.
  constants C_STRAS type FIELDNAME value 'STRAS' ##NO_TEXT.
  constants C_LOCAT type FIELDNAME value 'LOCAT' ##NO_TEXT.
  constants C_ORT01 type FIELDNAME value 'ORT01' ##NO_TEXT.

  methods CHECK
    redefinition .
  methods ERR_OV_GET_LIST
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_M99_PA_STP_NAME IMPLEMENTATION.


  method check.
* STP Data with Special Characters
    data: lo_regex   type ref to cl_abap_regex,
          lo_matcher type ref to cl_abap_matcher.
    data lv_pat       type string.

* IT 0002 STP Data
    data: lt_it2_stpdata type table of ty_it2_stpdata,
          ls_it2_stpdata type ty_it2_stpdata.
* IT 0006 STP Data
    data: lt_it6_stpdata type table of ty_it6_stpdata,
          ls_it6_stpdata type ty_it6_stpdata.
* STP Error Details
    data lt_stperr_dtls type tty_stperr_dtls .
    data ls_stperr_dtls type ty_stperr_dtls .

    data: ls_result  type ty_s_result.

* Fetch Employees IT 0002 STP data
    select it0002~pernr, it0002~nachn, it0002~vorna, it0002~midnm
      into corresponding fields of table @lt_it2_stpdata
      from pa0002 as it0002
      inner join pa0000 as it0000 on it0000~pernr = it0002~pernr
      inner join pa0001 as it0001 on it0001~pernr = it0002~pernr
     where it0002~pernr in @it_pernr_so
       and it0002~begda <= @mv_endda
       and it0002~endda >= @mv_begda
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

* Fetch Employees IT 0006 STP data
    select it0006~pernr, it0006~stras, it0006~locat, it0006~ort01
      into corresponding fields of table @lt_it6_stpdata
      from pa0006 as it0006
      inner join pa0000 as it0000 on it0000~pernr = it0006~pernr
      inner join pa0001 as it0001 on it0001~pernr = it0006~pernr
     where it0006~pernr in @it_pernr_so
       and it0006~begda <= @mv_endda
       and it0006~endda >= @mv_begda
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


* Identify Special Chrectors in IT0002 fields
* Create Pattren
    lv_pat = mc_it2char_pattern .
    create object lo_regex
      exporting
        pattern     = lv_pat
        ignore_case = abap_false.
    loop at it_pernr_so assigning field-symbol(<person>).
      read table lt_it2_stpdata into ls_it2_stpdata with key pernr = <person>-low.  "check if person has an infotype 0002
      if sy-subrc = 0.
         write <person>-low.
*      loop at lt_it2_stpdata into ls_it2_stpdata.
        clear: ls_stperr_dtls.
        move: ls_it2_stpdata-pernr to ls_stperr_dtls-pernr,
              c_0002 to ls_stperr_dtls-tabname.
        lo_matcher = lo_regex->create_matcher( text =  ls_it2_stpdata-nachn ).
        if lo_matcher->match( ) is initial.
          move c_nachn to ls_stperr_dtls-fieldname.
          append ls_stperr_dtls to lt_stperr_dtls.
        endif.
        lo_matcher = lo_regex->create_matcher( text =  ls_it2_stpdata-vorna ).
        if lo_matcher->match( ) is initial.
          move c_vorna to ls_stperr_dtls-fieldname.
          append ls_stperr_dtls to lt_stperr_dtls.
        endif.
        lo_matcher = lo_regex->create_matcher( text =  ls_it2_stpdata-midnm ).
        if lo_matcher->match( ) is initial.
          move c_midnm to ls_stperr_dtls-fieldname.
          append ls_stperr_dtls to lt_stperr_dtls.
        endif.
      else.
         append <person>-low to lt_stperr_dtls.
         write <person>-low.
      endif.
    endloop.

* Identify Special Chrectors in IT0006 fields
* Create Pattren
    lv_pat = mc_it6char_pattern .
    free lo_regex.
    create object lo_regex
      exporting
        pattern     = lv_pat
        ignore_case = abap_false.
    loop at lt_it6_stpdata into ls_it6_stpdata.
      clear: ls_stperr_dtls.
      move: ls_it6_stpdata-pernr to ls_stperr_dtls-pernr,
            c_0006 to ls_stperr_dtls-tabname.
      lo_matcher = lo_regex->create_matcher( text =  ls_it6_stpdata-stras ).
      if lo_matcher->match( ) is initial.
        move c_stras to ls_stperr_dtls-fieldname.
        append ls_stperr_dtls to lt_stperr_dtls.
      endif.
      lo_matcher = lo_regex->create_matcher( text =  ls_it6_stpdata-locat ).
      if lo_matcher->match( ) is initial.
        move c_locat to ls_stperr_dtls-fieldname.
        append ls_stperr_dtls to lt_stperr_dtls.
      endif.
      lo_matcher = lo_regex->create_matcher( text =  ls_it6_stpdata-ort01 ).
      if lo_matcher->match( ) is initial.
        move c_ort01 to ls_stperr_dtls-fieldname.
        append ls_stperr_dtls to lt_stperr_dtls.
      endif.
    endloop.

    sort lt_stperr_dtls by pernr tabname.
* Collect Data for Overview
    append lines of lt_stperr_dtls to mt_stperr_dtls.

* Build Results table
    loop at lt_stperr_dtls  into ls_stperr_dtls
         group by ( pernr = ls_stperr_dtls-pernr ) ascending
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
    data:
      ls_err_ov type ty_s_err_ov,
      ls_sfo    type cl_pyd_rd_dto_sfo=>ty_s_rd,
      lv_modif  type abap_bool,
      lv_pernr  type p_pernr.

    data: lv_sfo_value type char060,
          lv_value     type string.
    data: lv_al_begda(10) type c,
          lv_ab_begda(10) type c,
          lv_ab_endda(10) type c.
    data: lt_sfo_tab type cl_pyd_rd_dto_sfo=>ty_t_rd,
          ls_sfo_tab type cl_pyd_rd_dto_sfo=>ty_s_rd.
    data: lv_text type cl_pyd_rd_dto_sfo=>ty_s_rd-text.

    data: lv_return type bapireturn1.
    field-symbols: <struc> type any,
                   <comp>  type any.
    data: lt_fieldinfos type table of dfies,
          ls_fieldinfos type dfies.
    data: lt_p0002 type table of p0002,
          ls_p0002 type p0002,
          lt_p0006 type table of p0006,
          ls_p0006 type p0006.

* Populate SFO tab
    case iv_access_mode.
* Read Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-read.
        call method get_parameters
          exporting
            it_par         = it_par
            io_res_context = io_res_context.

        loop at ct_err_ov into ls_err_ov.
          clear: lt_p0002, lt_p0006, ls_p0002, ls_p0006.

          lv_pernr = ls_err_ov-id.
          lt_sfo_tab = ls_err_ov-sfo_tab.

          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          loop at lt_sfo_tab into ls_sfo_tab.
            lv_sfo_value = ls_sfo_tab-value.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = lv_sfo_value
              changing
                p_struct2 = ms_errfield_dtls.
* Read FIELD text
            call function 'DDIF_FIELDINFO_GET'
              exporting
                tabname   = ms_errfield_dtls-tabname
                fieldname = ms_errfield_dtls-fieldname
              tables
                dfies_tab = lt_fieldinfos
              exceptions
                others    = 0.
            read table lt_fieldinfos into ls_fieldinfos index 1.
            lv_text = ls_fieldinfos-fieldtext.

            case ms_errfield_dtls-tabname.
              when c_0002.
                if not ls_p0002-pernr eq lv_pernr.
                  call function 'HR_READ_INFOTYPE'
                    exporting
                      pernr           = lv_pernr
                      infty           = '0002'
                      begda           = mv_begda
                      endda           = mv_endda
                      bypass_buffer   = 'X'
                    tables
                      infty_tab       = lt_p0002
                    exceptions
                      infty_not_found = 1
                      invalid_input   = 2
                      others          = 3.
                  if sy-subrc <> 0.
* Implement suitable error handling here
                  endif.
                  read table lt_p0002 into ls_p0002 index 1.
                endif.
                assign ls_p0002 to <struc>.
              when c_0006.
                if not ls_p0006-pernr eq lv_pernr.
                  call function 'HR_READ_INFOTYPE'
                    exporting
                      pernr           = lv_pernr
                      infty           = '0006'
                      begda           = mv_begda
                      endda           = mv_endda
                      bypass_buffer   = 'X'
                    tables
                      infty_tab       = lt_p0006
                    exceptions
                      infty_not_found = 1
                      invalid_input   = 2
                      others          = 3.
                  if sy-subrc <> 0.
* Implement suitable error handling here
                  endif.
                  read table lt_p0006 into ls_p0006 index 1.
                endif.
                assign ls_p0006 to <struc>.
            endcase.
            assign component ms_errfield_dtls-fieldname
                   of structure <struc> to <comp>.
            move <comp> to lv_value.
            call method me->add_record_to_sfo_tab
              exporting
                iv_itemid                   = mc_itemid_charerr
                iv_text                     = lv_text
                iv_value                    = lv_value
                iv_text_for_1st_record_only = abap_true
              changing
                ct_sfo_tab                  = ls_err_ov-sfo_tab.
          endloop.
* Action
          lv_text = text-001.
          lv_value = text-002.
          call method me->add_record_to_sfo_tab
            exporting
              iv_itemid                   = mc_itemid_charerr
              iv_text                     = lv_text
              iv_value                    = lv_value
              iv_text_for_1st_record_only = abap_true
            changing
              ct_sfo_tab                  = ls_err_ov-sfo_tab.
          modify ct_err_ov from ls_err_ov.
        endloop.

* Execute Mode
      when if_pyd_fnd_types=>gcs_rdt_data_access_mode-execute.

        loop at ct_err_ov into ls_err_ov.
          clear ls_err_ov-sfo_tab.
          clear ls_sfo.
          lv_pernr = ls_err_ov-id.

          loop at mt_stperr_dtls into ms_stperr_dtls
            where pernr = lv_pernr.

            add 1 to ls_sfo-row_id.
            ls_sfo-itemid = mc_itemid_charerr.
            move-corresponding ms_stperr_dtls to ms_errfield_dtls.
            call method me->copy_structure_to_other
              exporting
                p_struct1 = ms_errfield_dtls
              changing
                p_struct2 = lv_sfo_value.

            ls_sfo-value = lv_sfo_value.
            insert ls_sfo into table ls_err_ov-sfo_tab.
          endloop.
* Add Reason
          modify ct_err_ov from ls_err_ov.
        endloop.

      when others.
        raise exception type cx_pyd_fnd.

    endcase.

  endmethod.
ENDCLASS.
