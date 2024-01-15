class ZUSECL_PA_INFOTYPE_COMBINATION definition
  public
  inheriting from ZUSECL_M99_PCC_CHK_FP4_BASE
  create public .

public section.

  constants MC_ITEMID_SUBTY type PYD_S_RDSFO_EXT-ITEMID value 'SUBTY' ##NO_TEXT.
protected section.

  constants MC_Z99_CONDITION_IT0041_01 type PYD_PAR_TYPE value 'Z99_CONDITION_IT0041_01' ##NO_TEXT.
  constants MC_DATETYPE_HIREDATE type P0041-DAR01 value '01' ##NO_TEXT.
  data MV_Z99_CONDITION_IT0041_01 type BOOLEAN .

  methods CHECK
    redefinition .
  private section.
ENDCLASS.



CLASS ZUSECL_PA_INFOTYPE_COMBINATION IMPLEMENTATION.


  method check.
* Validate Employee Data
    types: begin of ty_check,
             pernr type pernr_d,
             inf1  type ddflag,
             inf2  type ddflag,
           end of ty_check.

    data: lt_it0       type standard table of ty_check with non-unique key table_line,
          lv_size0     type i,
          lv_pernr     type pernr-pernr,
          lv_hire_date type datum,
          lv_tabname   type tabname,
          lv_subty     type subty.

    data: ls_result  type   ty_s_result.
* All relevant employees
    select it00~pernr into table lt_it0 from pa0000 as it00
        inner join pa0001 as it01 on it00~pernr = it01~pernr
        where it01~pernr in it_pernr_so                    and
              it01~begda <= mv_endda                       and
              it01~endda >= mv_begda                       and
              it00~begda <= mv_endda                       and
              it00~endda >= mv_begda                       and
              it00~stat2 in mt_stat2                       and
              it00~sprps = if_hrpa_read_infotype=>unlocked and
              it01~sprps = if_hrpa_read_infotype=>unlocked and
              it01~abkrs in mt_payroll_areas               and
              it01~bukrs in mt_bukrs                       and
              it01~werks in mt_werks                       and
              it01~persg in mt_persg                       and
              it01~persk in mt_persk.

    data: lt_where    type table of edpline,
          lt_sel_list type table of edpline,
          dref        type ref to data,
          itab_type   type ref to cl_abap_tabledescr,
          struct_type type ref to cl_abap_structdescr,
          comp_tab    type cl_abap_structdescr=>component_table.

    field-symbols : <lt_outtab>  type any table,
                    <lt_outtab1> type zpakey_tab,
                    <lt_outtab2> type zpakey_tab,
                    <l_fld>      type any.


    append 'PERNR' to lt_sel_list.

    data lt_table1 type table of pakey.
    data lt_table2 type table of pakey.

    "we need to dynamically read the two infotypes
    do 2 times.
      if sy-index = '1'.
        lv_tabname = |PA{ mv_infty }|.
        lv_subty =  mv_subty.
      else.
        lv_tabname = |PA{ mv_infty2 }|.
        lv_subty =  mv_subty2.
      endif.

      struct_type ?= cl_abap_typedescr=>describe_by_name( lv_tabname ).
      comp_tab = struct_type->get_components( ).
      struct_type = cl_abap_structdescr=>create( comp_tab ).
      itab_type   = cl_abap_tabledescr=>create( struct_type ).

      "creation of the data for the selection
      create data dref type handle itab_type.
      assign dref->* to <lt_outtab>.


      "dynamic selection for first infotype.
      clear <lt_outtab>.
      select  (lt_sel_list)
             from     (lv_tabname)
             into corresponding fields of table <lt_outtab>
        for all entries in lt_it0
             where    pernr = lt_it0-pernr
             and      subty = lv_subty
             and      begda le mv_endda
             and      endda ge mv_begda.
      check sy-subrc = 0.
      if sy-index = 1.

        loop at <lt_outtab> assigning field-symbol(<tab>).
          append corresponding #( <tab> ) to lt_table1.
        endloop.
      else.
        loop at <lt_outtab> assigning <tab>.
          append corresponding #( <tab> ) to lt_table2.
        endloop.
      endif.
    enddo.


*   Get all the entries from table 1 and all the entries from table two and add it to one combined table.
    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_table1 assigning field-symbol(<out>).
      read table lt_it0 with key pernr = <out>-pernr assigning field-symbol(<it1>).
      if sy-subrc = 0.
        <it1>-inf1 = 'X'.
      endif.
    endloop.

    loop at lt_table2 assigning <out>.
      read table lt_it0 with key pernr = <out>-pernr assigning field-symbol(<it2>)..
      if sy-subrc = 0.
        <it2>-inf2 = 'X'.
      endif.
    endloop.

    "we dont care about employees that have both infotypes, OR neither
    delete lt_it0 where inf1 = 'X' and inf2 = 'X'.
    delete lt_it0 where inf1 = ' ' and inf2 = ' '.

    "we are now left with employees that have at least one infotype/subtype missing

    ls_result-par_type = if_pyd_cont_types=>gcs_par_type-pernr.
    loop at lt_it0 assigning field-symbol(<it0>).
      ls_result-id = <it0>-pernr.
      append  ls_result to rt_result.
    endloop.
    " register event when transaction complete we need clear cache
    set handler handle_init_buffers for mo_fnd_factory->mo_transaction.

  endmethod.
ENDCLASS.
