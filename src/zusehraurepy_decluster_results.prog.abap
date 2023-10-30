*-----------------------------------------------------------------------*
* Program Name  : ZUSEHRAUREPY_DECLUSTER_RESULTS
* Title         : ALV report for decluster tables content
*-----------------------------------------------------------------------*
* Description   : Based on custom config retrieve decluster tables content
*                 for selected employee / period
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Date       | User ID      |Description                   |Change Label *
*-----------------------------------------------------------------------*
*           |              |                              |             *
*           |              |                              |             *
*-----------------------------------------------------------------------*
report zusehraurepy_decluster_results.

nodes: pernr.

selection-screen begin of block a1 with frame title text-001.
parameters:
p_test as checkbox default 'X'. "Use test payroll results or live one
selection-screen end of block a1.

class lcl_handle_events definition deferred.


data: gr_events type ref to lcl_handle_events.
data: gv_count TYPE i.
data: gr_table   type ref to cl_salv_table.
data: gr_details type ref to cl_salv_table.

data: gt_rgdir type table of hrdct_tpy_rgdir with header line.

data: begin of gt_outtab occurs 0,
        pernr  type p_pernr,
        ename  type emnam,
        seqnr  type CDSEQ,
        fpper  type faper,
        inper  type iperi,
        tname type tabname.
data: end of gt_outtab.

data: begin of gt_pernr occurs 0,
        pernr  type p_pernr,
        ename  type emnam.
data: end of gt_pernr.

data: gt_config type table of zuse_pcc_tables with header line.

class lcl_handle_events definition.
  public section.
    methods:
      on_double_click for event double_click of cl_salv_events_table
        importing row column.
endclass.

class lcl_handle_events implementation.

  method on_double_click.
    "call details ALV when user double-clicks a row
    perform show_table_info using row column.
  endmethod.

endclass.

"make PERNR field mandatory

at selection-screen output.
  loop at screen.
    if screen-name cs 'PNPPERNR'.
      screen-required = 1.
      screen-active = 1.
      modify screen.
    endif.
  endloop.

at selection-screen on pnppernr. "check total employees count entered
  if lines( pnppernr[] ) > 20.
    message 'Personnel Number count exeeds limit of 20 - please reduce!' type 'E'.
  endif.

start-of-selection.

  refresh: gt_rgdir, gt_pernr, gt_outtab, gt_config.

  CLEAR gv_count.

  "fetch the config
  select * into table gt_config from zuse_pcc_tables.

get pernr.

  gv_count = gv_count + 1.

  "only limit records count to 20 (in case range was entered at selscreen)
  IF gv_count > 20.
    REJECT.
  ENDIF.

  gt_pernr-pernr = pernr-pernr.
  gt_pernr-ename = pernr-ename.
  APPEND gt_pernr.

end-of-selection.

  if gt_pernr[] is not initial.

    "populate rgdir table for further processing
    if p_test is not initial.
      select dct_pernr dct_seqnr inper fpper
        into corresponding fields of table gt_rgdir
        from hrdct_tpy_rgdir
        for all entries in gt_pernr
        where dct_pernr = gt_pernr-pernr and
              abkrs = pnpxabkr and
              ipend BETWEEN pn-begda AND pn-endda.
    else.
      select dct_pernr dct_seqnr inper fpper
        into corresponding fields of table gt_rgdir
        from p2rx_eval_period
        for all entries in gt_pernr
        where dct_pernr = gt_pernr-pernr and
              abkrs = pnpxabkr and
              ipend BETWEEN pn-begda AND pn-endda.
    endif.

    "construct the output table:
    LOOP AT gt_pernr.
      LOOP AT gt_rgdir where dct_pernr = gt_pernr-pernr.
        LOOP AT gt_config.
          gt_outtab-pernr = gt_pernr-pernr.
          gt_outtab-ename = gt_pernr-ename.
          gt_outtab-seqnr = gt_rgdir-dct_seqnr.
          gt_outtab-inper = gt_rgdir-inper.
          gt_outtab-fpper = gt_rgdir-fpper.
          gt_outtab-tname = gt_config-table_name.
          append gt_outtab.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    "show the result
    perform show_alv.

  else.

    write: / 'No data!'.

  endif.



form show_table_info using i_row    type i
                           i_column type lvc_fname.
  "get clicked row details
  read table gt_outtab index i_row.

  check sy-subrc = 0.

  "show the selected table
  perform show_details_alv using gt_outtab-pernr
                                 gt_outtab-seqnr
                                 gt_outtab-fpper
                                 gt_outtab-tname.

endform.

form show_alv.
  "main grid with EE number, name, list of tables

  data: lr_columns type ref to cl_salv_columns,
        lr_column  type ref to cl_salv_column_table,
        lr_events  type ref to cl_salv_events_table.
  data: lr_display_settings type ref to cl_salv_display_settings,
        l_title             type lvc_title,
        lr_functions        type ref to cl_salv_functions_list.
  data: lr_sort    type ref to cl_salv_sorts.


  try.
      cl_salv_table=>factory(
        importing
          r_salv_table = gr_table
        changing
          t_table      = gt_outtab[] ).
    catch cx_salv_msg.
  endtry.

  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( abap_true ).

  lr_events = gr_table->get_event( ).

  create object gr_events.

  set handler gr_events->on_double_click for lr_events.

  lr_sort = gr_table->get_sorts( ).

  try.
      call method lr_sort->add_sort
        exporting
          columnname = 'PERNR'.
      call method lr_sort->add_sort
        exporting
          columnname = 'ENAME'.
      call method lr_sort->add_sort
        exporting
          columnname = 'SEQNR'.
      call method lr_sort->add_sort
        exporting
          columnname = 'FPPER'.
      call method lr_sort->add_sort
        exporting
          columnname = 'INPER'.
    catch cx_salv_not_found .
    catch cx_salv_existing .
    catch cx_salv_data_error .
  endtry.

  l_title = text-002.
  lr_display_settings = gr_table->get_display_settings( ).
  lr_display_settings->set_list_header( l_title ).

  gr_table->display( ).

endform.

form show_details_alv using p_pernr type pernr_d
                            p_seqnr type cdseq
                            p_fpper type faper
                            p_table type tabname.
  "dynamic details grid

  data: lr_columns type ref to cl_salv_columns,
        lr_column  type ref to cl_salv_column_table,
        lr_events  type ref to cl_salv_events_table.
  data: lr_display_settings type ref to cl_salv_display_settings,
        l_title             type lvc_title,
        lt_rgdir            type table of hrdct_tpy_rgdir with header line,
        lr_functions        type ref to cl_salv_functions_list.
  data: lr_sort    type ref to cl_salv_sorts.
  data: lr_table   type REF TO cl_abap_tabledescr,
        lt_keys    TYPE abap_table_keydescr_tab,
        lv_OK.


  data: tabref type ref to data.
  field-symbols : <tab> type any table,
                  <fld> type any.

  create data tabref type table of (p_table).

  check sy-subrc = 0.

  assign tabref->* to <tab>.

  check sy-subrc = 0.

  "populate the output
*  "READ TABLE gt_rgdir WITH KEY dct_pernr = p_pernr.
*  lt_rgdir[] = gt_rgdir[].
*  delete lt_rgdir where dct_pernr <> p_pernr.
*  if lt_rgdir[] is not initial.
*    select * into table <tab>
*      from (p_table)
*      for all entries in lt_rgdir
*      where dct_pernr = lt_rgdir-dct_pernr and
*            dct_seqnr = lt_rgdir-dct_seqnr.
*  endif.

    "check if seqnr field exists
    lr_table ?= cl_abap_typedescr=>describe_by_data( <tab> ).
    lt_keys = lr_table->get_keys( ).

    CLEAR lv_ok.
    LOOP AT lt_keys INTO DATA(ls_key).
      READ TABLE ls_key-components WITH KEY name = 'DCT_SEQNR' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_ok = 'X'.
        exit.
      ENDIF.
    ENDLOOP.


    IF lv_ok is not initial.
      "ok to proceed
    select * into table <tab>
      from (p_table)
      where dct_pernr = p_pernr and
            dct_seqnr = p_seqnr.
    ELSE.
      "access by period instead
     select * into table <tab>
      from (p_table)
      where dct_pernr = p_pernr and
            dct_pabrj = p_fpper(4) and
            dct_pabrp = p_fpper+4(2).
    ENDIF.

  try.
      cl_salv_table=>factory(
        importing
          r_salv_table = gr_details
        changing
          t_table      = <tab> ).
    catch cx_salv_msg.
  endtry.

  lr_columns = gr_details->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  lr_functions = gr_details->get_functions( ).
  lr_functions->set_all( abap_true ).

  lr_sort = gr_details->get_sorts( ).

  try.
      call method lr_sort->add_sort
        exporting
          columnname = 'DCT_SEQNR'.
    catch cx_salv_not_found .
    catch cx_salv_existing .
    catch cx_salv_data_error .
  endtry.

  l_title = p_table.
  lr_display_settings = gr_details->get_display_settings( ).
  lr_display_settings->set_list_header( l_title ).

  gr_details->display( ).

endform.
