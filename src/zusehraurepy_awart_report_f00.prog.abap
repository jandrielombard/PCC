*&---------------------------------------------------------------------*
*&  Include           ZZ_AWART_REPORT_F00
*&---------------------------------------------------------------------*


types: begin of ty_s_alvc,
         persk      type persk,
         zztimeid   type zhrau_de_time,
         zztimetext type zhrau_de_timetext,
         endda      type endda,
         begda      type begda,
         awart      type awart,
         atext      type abwtxt,
         perskt     type pktxt,
       end of ty_s_alvc.

data: g_container        type scrfname value 'ALV_LIST_CONT1',
      ok_code            like sy-ucomm,
      grid1              type ref to cl_gui_alv_grid,
      g_custom_container type ref to cl_gui_custom_container,
      gs_layout          type lvc_s_layo,
      gt_fieldcatalog    type lvc_t_fcat,
      gs_fieldcatalog    type lvc_s_fcat.

data:
*  gt_catsid  type standard table of zcatstimeid,
*  gs_catsid  type zcatstimeid,
*  gt_catsidt type standard table of zcatstimeidtext,
*  gs_catsidt type zcatstimeidtext,
  gt_t512t   type standard table of t512t,
  gs_t512t   type t512t,
  gt_t503t   type standard table of t503t,
  gs_t503t   type t503t,
  list_alvc  type table of ty_s_alvc,
  gs_alvc    type ty_s_alvc.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form prepare_data .
  select * from t554s into table gt_t554s
    where moabw = '13'
      and subty in so_awart
      and begda <= pa_key
      and endda >= pa_key.

  select * from t554t into table gt_t554t
    where sprsl = sy-langu
      and moabw = '13'
      and awart in so_awart.

  sort: gt_t554s, gt_t554t.

  select * from t503t into table gt_t503t
    where sprsl = sy-langu.
  sort gt_t503t by persk.

  perform timeid.

endform.

*&---------------------------------------------------------------------*
*&      Form  CATSID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form timeid .
*  select * from zcatstimeid into table gt_catsid
*    where begda <= pa_key
*      and endda >= pa_key
*      and awart in so_awart.
*
*  delete gt_catsid where awart = ''.
*
*  select * from zcatstimeidtext into table gt_catsidt
*    where spras = sy-langu
*      and endda >= pa_key.
*  sort gt_catsidt.
*  delete adjacent duplicates from gt_catsidt comparing spras persk zztimeid.
endform.

*&---------------------------------------------------------------------*
*&      Form  WRITE_TIMEID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_timeid .

  perform get_timeid_alv.
  perform set_fieldcat_timeid.
  if not grid1 is initial.
    call method grid1->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = gt_fieldcatalog.
    call method grid1->set_gridtitle
      exporting
        i_gridtitle = text-020.
  endif.
  call screen 100.

endform.

*&---------------------------------------------------------------------*
*&      Form  GET_CATSID_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_timeid_alv .

*  loop at gt_catsid into gs_catsid.
*    clear: gs_alvc, gs_catsidt.
*    move-corresponding gs_catsid to gs_alvc.
*    read table gt_catsidt with key
*    persk = gs_alvc-persk
*    zztimeid = gs_alvc-zztimeid into gs_catsidt binary search.
*    if sy-subrc = 0.
*      gs_alvc-zztimetext = gs_catsidt-zztimetext.
*    endif.
*    read table gt_t554t into gs_t554t with key awart = gs_alvc-awart binary search.
*    if sy-subrc = 0.
*      gs_alvc-atext = gs_t554t-atext.
*    endif.
*    read table gt_t503t into gs_t503t with key persk = gs_alvc-persk binary search.
*    if sy-subrc = 0.
*      gs_alvc-perskt = gs_t503t-ptext.
*    endif.
*    append gs_alvc to list_alvc.
*  endloop.
*
*  sort list_alvc by awart zztimeid persk.
endform.

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_timeID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_timeid .
  gs_layout-grid_title = text-020.
  clear gt_fieldcatalog.
  clear gs_fieldcatalog.
  clear gs_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'AWART'.
  gs_fieldcatalog-tabname = 'LIST_ALVC'.
  gs_fieldcatalog-rollname = 'AWART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'ATEXT'.
  gs_fieldcatalog-tabname = 'LIST_ALVC'.
  gs_fieldcatalog-rollname = 'ABWTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'ZZTIMEID'.
  gs_fieldcatalog-tabname = 'LIST_ALVC'.
  gs_fieldcatalog-reptext = 'TIMEID'.
  gs_fieldcatalog-outputlen = 6.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'ZZTIMETEXT'.
  gs_fieldcatalog-tabname = 'LIST_ALVC'.
  gs_fieldcatalog-reptext = 'Time ID Text'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'PERSK'.
  gs_fieldcatalog-tabname = 'LIST_ALVC'.
  gs_fieldcatalog-reptext = 'ESG'.
  gs_fieldcatalog-outputlen = 3.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'PERSKT'.
  gs_fieldcatalog-tabname = 'LIST_ALVC'.
  gs_fieldcatalog-reptext = 'Employee Subgroup'.
  gs_fieldcatalog-outputlen = 20.
  append gs_fieldcatalog to gt_fieldcatalog.

endform.

*&---------------------------------------------------------------------*
*&      Form  CYCLE_MATCHUP_AWART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cycle_matchup_awart using value(p_single).
  types: begin of ty_alv_op,
           ccycl type ccycl,
           abart type abrar,
           lgart type lgart,
           vargt type vrarg,
           seqno type seqln,
           mode  type c,
           opt1  type char10,
           opt2  type char10,
           opt3  type char10,
           opt4  type char10,
           opt5  type char10,
           opt6  type char10,
         end of ty_alv_op,
         ty_t_alv_op type table of ty_alv_op.

  data: lt_t52c5    type standard table of t52c5,
        lt_t52ba    type standard table of t52ba,
        lt_alv_op   type ty_t_alv_op,
        lw_alv_op   type ty_alv_op,
        lw_t52ba    type t52ba,
        lw_t52c5    type t52c5,
        lt_r_selopt type table of selopt,
        lw_r_selopt type selopt,
        l_string    type string,
        l_tabix     type sytabix.

  data: lo_table      type ref to cl_salv_table,
        lo_header     type ref to cl_salv_form_layout_grid,
        lo_h_flow     type ref to cl_salv_form_layout_flow,
        lo_columns    type ref to cl_salv_columns,
        lo_column     type ref to cl_salv_column_table,
        lo_functions  type ref to cl_salv_functions_list,
        lo_selections type ref to cl_salv_selections,
        lo_salv_msg   type ref to cx_salv_msg,
        lo_sorts      type ref to cl_salv_sorts,
        lo_layout     type ref to cl_salv_layout,
        l_text        type txt255,
        lw_key        type salv_s_layout_key.

  data:
    absence type awart.

  clear absence.
  lw_r_selopt-sign   = 'I'.
  lw_r_selopt-option = 'CP'.

  if p_single = 'X'.
    l_tabix = sy-curow - 2.
    read table gt_t554s index l_tabix into gs_t554s.
    move gs_t554s-subty to absence.
    if absence is initial.
      write: 'Please select the Absence type and click on this button.'.
      return.
    endif.
    l_string = '*' && absence && '*'.
    lw_r_selopt-low    = l_string.
    append lw_r_selopt to lt_r_selopt.
  else.
    loop at gt_t554s into gs_t554s.
      move gs_t554s-subty to absence.
      l_string = '*' && absence && '*'.
      lw_r_selopt-low    = l_string.
      append lw_r_selopt to lt_r_selopt.
    endloop.
  endif.

  select * from t52c5
           into table lt_t52c5
           where vargt in lt_r_selopt
              or vinfo in lt_r_selopt.
  if sy-subrc = 0.
    refresh lt_r_selopt[].
    loop at lt_t52c5 into lw_t52c5.
      clear lw_r_selopt-low.
      lw_r_selopt-low    = lw_t52c5-ccycl.
      append lw_r_selopt to lt_r_selopt.
    endloop.
    sort lt_r_selopt by low.
    delete adjacent duplicates from lt_r_selopt comparing low.
    select * into table lt_t52ba
             from t52ba
             where potyp = 'CYCL'
             and   ponam in lt_r_selopt
             and   pattr = 'CNT'
            and    pwert  = '*' .
    if sy-subrc = 0.
      refresh lt_r_selopt[].
      clear lw_r_selopt-low.
      loop at lt_t52ba into lw_t52ba.
        clear lw_r_selopt-low.
        lw_r_selopt-low    = lw_t52ba-ponam.
        append lw_r_selopt to lt_r_selopt.
      endloop.
      delete lt_t52c5 where ccycl not in lt_r_selopt.
      loop at lt_t52c5 into lw_t52c5.
        write: lw_t52c5-ccycl to lw_alv_op-ccycl,
               lw_t52c5-abart to lw_alv_op-abart,
               lw_t52c5-lgart to lw_alv_op-lgart,
               lw_t52c5-vargt to lw_alv_op-vargt,
               lw_t52c5-seqno to lw_alv_op-seqno,
               lw_t52c5-vinfo(1) to lw_alv_op-mode,
               lw_t52c5-vinfo+1(10) to lw_alv_op-opt1,
               lw_t52c5-vinfo+12(10) to lw_alv_op-opt2,
               lw_t52c5-vinfo+23(10) to lw_alv_op-opt3,
               lw_t52c5-vinfo+34(10) to lw_alv_op-opt4,
               lw_t52c5-vinfo+45(10) to lw_alv_op-opt5,
               lw_t52c5-vinfo+56(10) to lw_alv_op-opt6.
        append lw_alv_op to lt_alv_op.
      endloop.
    else.
      write: 'The Absence Type is not used in any Rules for this country.'.
    endif.
  else.
    write: text-011.
  endif.

  if lt_alv_op[] is not initial.
*   Header object
    create object lo_header.
*   Get New Instance for ALV Table Object
    try.
        cl_salv_table=>factory(
        exporting
          list_display   = if_salv_c_bool_sap=>false
        importing
          r_salv_table = lo_table
        changing
          t_table      = lt_alv_op ).
      catch cx_salv_msg into lo_salv_msg.
        message e999 with lo_salv_msg->msgv1
        lo_salv_msg->msgv2
        lo_salv_msg->msgv3
        lo_salv_msg->msgv4.
    endtry.

*   Get the coloumns from the table
    lo_columns = lo_table->get_columns( ).
*   Optimise the coloumns width
    lo_columns->set_optimize( ).
*   Functions
    lo_functions = lo_table->get_functions( ).
    lo_functions->set_all( value = abap_true ).
*   Heading for the report
    l_text = 'Rules using the specified Absence types'.
    lo_h_flow = lo_header->create_flow( row = 1  column = 1 ).
    lo_h_flow->create_text( text = l_text ).

*   Selections
    lo_selections = lo_table->get_selections( ).
    lo_selections->set_selection_mode(
    cl_salv_selections=>if_salv_c_selection_mode~row_column ).
*   Set the column headings on the output
    perform set_column_names
            using lo_columns: 'CCYCL' 'Rule'
                              changing lo_column,
                              'ABART' 'Employee subgroup grouping'
                              changing lo_column,
                              'LGART' 'Wage Type'
                              changing lo_column,
                              'VARGT' 'Variable Key'
                              changing lo_column,
                              'SEQNO' 'Next Line'
                              changing lo_column,
                              'MODE' 'Mode'
                              changing lo_column,
                              'OPT1' 'Operation'
                              changing lo_column,
                              'OPT2' 'Operation'
                              changing lo_column,
                              'OPT3' 'Operation'
                              changing lo_column,
                              'OPT4' 'Operation'
                              changing lo_column,
                              'OPT5' 'Operation'
                              changing lo_column,
                              'OPT6' 'Operation'
                              changing lo_column.

    call method lo_table->get_layout
      receiving
        value = lo_layout.
    lw_key-report = sy-repid.
    lo_layout->set_key( lw_key ).
*   Set Layout selected on screen
    lo_layout->set_default( abap_true ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   Set the top of list using the header for Online.
    lo_table->set_top_of_list( lo_header ).
    lo_table->display( ).
  endif.

endform.                    " CYCLE_MATCHUP

form set_column_names using io_columns type ref to cl_salv_columns
                            i_colname  type lvc_fname
                            i_coltext  type scrtext_l
                   changing eo_column type ref to cl_salv_column_table.

  data: l_scrtext_m type scrtext_m,
        l_scrtext_s type scrtext_s.

  l_scrtext_m = i_coltext.
  l_scrtext_s = i_coltext.

  try.
      eo_column ?= io_columns->get_column( i_colname ).
      eo_column->set_long_text( i_coltext ).
      eo_column->set_medium_text( l_scrtext_m ).
      eo_column->set_short_text( l_scrtext_s ).
    catch cx_salv_not_found.
      message e999(zh) with 'No column found with the given entry'.
  endtry.

endform.                    " SET_COLUMN_NAMES

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pbo output.
  set pf-status '0100'.

  if g_custom_container is initial.
    create object g_custom_container
      exporting
        container_name = g_container.
    create object grid1
      exporting
        i_parent = g_custom_container.

    call method grid1->set_table_for_first_display
      exporting
        is_layout       = gs_layout
      changing
        it_outtab       = list_alvc
        it_fieldcatalog = gt_fieldcatalog.
  else.
    call method grid1->refresh_table_display.
  endif.
  call method cl_gui_control=>set_focus exporting control = grid1.
endmodule.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai input.
  case ok_code.
    when 'EXIT' or 'BACK' or 'CANC'.
      set screen 0. leave screen.
    when others.
  endcase.
  clear ok_code.
endmodule.
