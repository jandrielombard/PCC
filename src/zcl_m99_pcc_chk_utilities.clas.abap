class ZCL_M99_PCC_CHK_UTILITIES definition
  public
  final
  create public .

public section.

  types:
    begin of ty_it0008_lgart,
        seqnr(3),
        lgart    type p0008-lga01,
        betrg    type p0008-bet01,
        anzhl    type p0008-anz01,
        ein      type p0008-ein01,
        opken    type p0008-opk01,
        indbw    type p0008-ind01,
        waers    type p0008-waers.
    types: end of ty_it0008_lgart .
  types:
    tty_it0008_lgart type table of ty_it0008_lgart .

  class-methods ADJUST_SPLIT_SELECTION
    importing
      !IV_SPLIT_PARAM type RSSCR_NAME
      !IV_SPLIT_INTERVAL type I
    changing
      value(CT_SEL_PARAMS) type CL_PYC_BPC_REPORT_PARALLEL=>TY_T_SEL_PARAMS .
  class-methods READ_IT0008_WAGETYPE_DTLS
    importing
      !IS_P0008 type P0008
    exporting
      !ET_IT0008_LGART type TTY_IT0008_LGART .
  class-methods READ_PAYROLL_AREA_PERNR
    importing
      !IT_PAR type IF_PYD_FND_TYPES=>TY_T_RESP
    changing
      !CT_PERNR_SO type /IWBEP/T_COD_SELECT_OPTIONS .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_M99_PCC_CHK_UTILITIES IMPLEMENTATION.


  method adjust_split_selection.
* Adjust input selection split according to the load category configuration
    constants gc_instances type rsscr_name value 'SO_INST'.

    types: begin of ty_cat,
             id       type pyd_instid,
             category type zhrau_de_vcat,
           end of ty_cat.

    data: lt_cat   type table of ty_cat,
          ls_cat   type ty_cat,
          lv_index type i,
          lv_total type i.
    data: lt_params             type rsparams_tt,
          ls_params             type rsparams,
          ls_sel                type cl_pyc_bpc_report_parallel=>ty_s_sel_params,
          ls_sel_sample         type cl_pyc_bpc_report_parallel=>ty_s_sel_params,
          ls_extreme_sel_params type cl_pyc_bpc_report_parallel=>ty_s_sel_params,
          lt_extreme_sel_params type cl_pyc_bpc_report_parallel=>ty_t_sel_params.

    data: lt_low     type rsparams_tt,
          lt_medium  type rsparams_tt,
          lt_high    type rsparams_tt,
          lt_extreme type rsparams_tt,
          lt_all     type rsparams_tt.
    data  lv_lines type i.

    field-symbols: <fs_sel> type cl_pyc_bpc_report_parallel=>ty_s_sel_params.

* Only for Validations
    check iv_split_param eq gc_instances.

    lv_total = lines( ct_sel_params ).
    check lv_total > 0.

    "collect validation instances for further sorting
    loop at ct_sel_params assigning <fs_sel>.
      loop at <fs_sel>-sel_params into ls_params where selname = gc_instances.
        append ls_params to lt_params.
      endloop.
      "clear the SO_INST entries to be added back later
      delete <fs_sel>-sel_params where selname = gc_instances.
    endloop.

    "get the configuration
    select b~id a~category into table lt_cat
      from zhrpy_pcc_vcat as a
      inner join pyd_d_inst as b
      on a~validation = b~type.

    "do the sorting
    loop at  lt_params into ls_params.
      read table lt_cat into ls_cat with key id = ls_params-low.
      if sy-subrc = 0.
        case ls_cat-category.
          when '1'. "medium
            append ls_params to lt_medium.
          when '2'. "high
            append ls_params to lt_high.
          when '3'. "extreme
            append ls_params to lt_extreme.
          when others. "low
            append ls_params to lt_low.
        endcase.
      else.
        append ls_params to lt_low.
      endif.
    endloop.
* Keep Sample Sel for Extreme
    clear ls_sel_sample.
    read table ct_sel_params into ls_sel_sample index 1.

    "combine rest of instances into one table sorted by load category high to low
    clear lt_all.
    append lines of lt_high    to lt_all.
    append lines of lt_medium  to lt_all.
    append lines of lt_low     to lt_all.

* populate the data back distributed evenly:
    describe table lt_all lines lv_lines.
    if lv_lines eq 0.
      " When All Validations are tranfered to Extreme option clear the Existing sel parameters line
      loop at ct_sel_params assigning <fs_sel>.
        delete ct_sel_params .
      endloop.
    else.
      clear lv_index.
      loop at lt_all into ls_params.
        lv_index = lv_index + 1.
        if lv_index > lv_total.
          lv_index = 1.
        endif.
        read table ct_sel_params assigning <fs_sel> index lv_index.
        if sy-subrc = 0.
          "one entry per each row of ct_sel_params, then repeat
          append ls_params to <fs_sel>-sel_params.
        endif.
      endloop.
    endif.

    "Create new lines for extreme jobs
    if not lt_extreme is initial.
      clear lv_index.
      read table ct_sel_params into ls_sel index 1.
      if sy-subrc eq 0.
        lv_index = lv_total.
      endif.

      loop at lt_extreme into ls_params.
        lv_index = lv_index + 1.
        ls_extreme_sel_params = ls_sel_sample.
        ls_extreme_sel_params-row_id = lv_index.
        " Append Check instance for the split
        append ls_params to ls_extreme_sel_params-sel_params.
        " Create new split
        append ls_extreme_sel_params to lt_extreme_sel_params.
      endloop.

      " Finally add new split selection for Extreme validations
      append lines of lt_extreme_sel_params to ct_sel_params.
    endif.

  endmethod.


  method read_it0008_wagetype_dtls.
* read IT 0008 Wage types details
    data: ls_p0008 type p0008.
    data: ls_it0008_lgart type ty_it0008_lgart.
    data: begin of str_p0008,
            lgart type p0008-lga01,
            betrg type p0008-bet01,
            anzhl type p0008-anz01,
            ein   type p0008-ein01,
            opken type p0008-opk01,
          end of str_p0008.
    data: str_indbw type p0008-ind01.
    data: number_of_lgart_in_p0008 type i value 40.
*
    refresh et_it0008_lgart.
    move-corresponding is_p0008 to ls_p0008.
    do number_of_lgart_in_p0008 times
       varying str_p0008-lgart from ls_p0008-lga01
                               next ls_p0008-lga02
       varying str_p0008-betrg from ls_p0008-bet01
                               next ls_p0008-bet02
       varying str_p0008-anzhl from ls_p0008-anz01
                               next ls_p0008-anz02
       varying str_p0008-ein   from ls_p0008-ein01
                               next ls_p0008-ein02
       varying str_p0008-opken from ls_p0008-opk01
                               next ls_p0008-opk02
       varying str_indbw       from ls_p0008-ind01
                               next ls_p0008-ind02.
      if str_p0008-lgart ne space.
        clear ls_it0008_lgart.
        ls_it0008_lgart-waers = ls_p0008-waers.
        move-corresponding str_p0008 to ls_it0008_lgart.
        move str_indbw to ls_it0008_lgart-indbw.
        append ls_it0008_lgart to et_it0008_lgart.
      endif.
    enddo.

  endmethod.


  method read_payroll_area_pernr.
* Read Employee Numbers for the Payroll Area
    constants: mc_abkrs  type pyd_par_type value 'ABKRS',
               mc_period type pyd_par_type value 'PERIOD'.
    data: ls_par          type pyd_s_resp.
    data: lv_payroll_area type tvarv_val,
          lv_pabrj        type pabrj,
          lv_pabrp        type pabrp,
          lv_vabrj        type vabrj,
          lv_vabrp        type vabrp,
          lv_abkrs        type abkrs,
          lv_begda        type begda,
          lv_endda        type endda,
          lo_payroll_area type ref to cl_hr_payroll_area.

    data: lt_pernr type if_pyc_process_prog=>t_pernr,
          ls_pernr type if_pyc_process_prog=>s_pernr.
    data: lr_pernr     type hr99s_pernr_range,
          lr_sel_pernr type sel_pernr.
    data: l_prev_pernr type pernr-pernr,
          l_next_pernr type pernr-pernr.

* Read Payroll Area and Period dates from parameters
    loop at it_par into ls_par.
      case ls_par-par_type.
        when  mc_abkrs.
          lv_payroll_area = ls_par-low.
        when mc_period.
          lv_pabrj = ls_par-low(4).
          lv_pabrp = ls_par-low+4(2).
      endcase.
    endloop.

* Only for New Zealand Payroll Area's
    check lv_payroll_area+0(1) = 'N'.

* Payroll Period
    lv_abkrs = lv_payroll_area.
    lo_payroll_area = cl_hr_payroll_area=>get_instance( imp_area = lv_abkrs ).
    lo_payroll_area->get_period_info(
      exporting
        imp_pabrj = lv_pabrj
        imp_pabrp = lv_pabrp
      importing
        exp_vabrj = lv_vabrj
        exp_vabrp = lv_vabrp
        exp_begda = lv_begda
        exp_endda = lv_endda ).

* Read Payroll Area Employees
    select it0001~pernr
      into corresponding fields of table lt_pernr
      from pa0001 as it0001
        where it0001~abkrs eq lv_abkrs
          and it0001~endda >= lv_begda
          and it0001~begda <= lv_endda.

    "remove the potiential duplicate pernr
    sort lt_pernr by pernr.
    delete adjacent duplicates from lt_pernr.

* Build Pernr Range Table
    refresh: lr_pernr.
    lr_sel_pernr-sign   = 'I'.
    lr_sel_pernr-option = 'BT'.
    loop at lt_pernr into ls_pernr .
      if lr_sel_pernr-high is initial.
        lr_sel_pernr-low    = ls_pernr-pernr.
        lr_sel_pernr-high   = ls_pernr-pernr.
      else.
        l_next_pernr = lr_sel_pernr-high + 1.
        if ls_pernr-pernr ne l_next_pernr.
          append lr_sel_pernr to lr_pernr.

          lr_sel_pernr-low    = ls_pernr-pernr.
          lr_sel_pernr-high   = ls_pernr-pernr.
        else.
          lr_sel_pernr-high   = ls_pernr-pernr.
        endif.
      endif.
    endloop.
* Add last row to range
    if lr_sel_pernr-low is not initial.
      append lr_sel_pernr to lr_pernr.
    endif.

    move-corresponding lr_pernr[] to ct_pernr_so[].

  endmethod.
ENDCLASS.
