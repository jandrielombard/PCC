*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_WT_ATTRIBUTES_REP05
*&---------------------------------------------------------------------*
***********************************************************************
* INCLUDE:       ZHRAUREPY_WT_ATTRIBUTES_REP05
* TITLE:         General Include for Report Display                   *
* AUTHOR:
* DATE:                                                               *
* R/3 RELEASE:  SAP R/3 Enterprise
***********************************************************************

FORM display_report TABLES gt_outtab.

  DATA: gr_table TYPE REF TO cl_salv_table.

*... Create Instance
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = gt_outtab[].
    CATCH cx_salv_msg .
      CLEAR gt_outtab[].
  ENDTRY.

* Set PF-STATUS
  DATA: lv_fcode(4) TYPE c VALUE 'APPL'.
  TRY.
*      gr_table->set_screen_status(
*      pfstatus      =  'STANDARD'
*      report        =  sy-repid
*      set_functions = gr_table->c_functions_all ).
      DATA(lr_funct) = gr_table->get_functions( ).
      lr_funct->set_all( abap_true ).

    CATCH cx_salv_object_not_found.
      CLEAR gr_table.
  ENDTRY.
*---- set the top-of-page
  DATA: lr_content TYPE REF TO cl_salv_form_element.

  PERFORM create_form_content_tol_alv CHANGING lr_content.
  gr_table->set_top_of_list( lr_content ).

*-------Set title setting
*  DATA: LR_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
*        L_TITLE TYPE LVC_TITLE.
*
*  L_TITLE = P_TITLE.
*  LR_DISPLAY_SETTINGS = GR_TABLE->GET_DISPLAY_SETTINGS( ).
*  LR_DISPLAY_SETTINGS->SET_LIST_HEADER( L_TITLE ).

**... SET SORT
*  PERFORM SET_SORT_ALV USING GR_TABLE.

*... Report Structure
  PERFORM load_report_structure USING gr_table.

*... Display Table
  gr_table->display( ).

ENDFORM.                    "DISPLAY_REPORT
*&--------------------------------------------------------------------*
*&      Form  set_sort_alv
*&--------------------------------------------------------------------*
*      This form routine is for sorting specific columns
*---------------------------------------------------------------------*
FORM set_sort_alv USING gr_table TYPE REF TO cl_salv_table.

  DATA:
  lr_sorts TYPE REF TO cl_salv_sorts.   "sort information

  lr_sorts = gr_table->get_sorts( ).

  lr_sorts->clear( ).

*  IF DETAIL = 'X'.
  IF matrix = ' '.
    IF withowt = 'X'.

      TRY.
          lr_sorts->add_sort(
            columnname = 'LGART'
            position   = 1
            subtotal  = abap_true
            sequence   = if_salv_c_sort=>sort_up ).
        CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
          CLEAR lr_sorts.                              "#EC NO_HANDLER.
      ENDTRY.

      TRY.
          lr_sorts->add_sort(
            columnname = 'LGTXT'
            position   = 2
            subtotal  = abap_true
            sequence   = if_salv_c_sort=>sort_up ).
        CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
          CLEAR lr_sorts.                              "#EC NO_HANDLER.
      ENDTRY.
      TRY.
          lr_sorts->add_sort(
            columnname = 'SRTSEQ'
            position   = 3
            subtotal  = abap_true
            sequence   = if_salv_c_sort=>sort_up ).
        CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
          CLEAR lr_sorts.                              "#EC NO_HANDLER.
      ENDTRY.

      TRY.
          lr_sorts->add_sort(
            columnname = 'CLASS'
            position   = 4
            subtotal  = abap_true
            sequence   = if_salv_c_sort=>sort_up ).
        CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
          CLEAR lr_sorts.                              "#EC NO_HANDLER.
      ENDTRY.

      TRY.
          lr_sorts->add_sort(
            columnname = 'COL1'
            position   = 5
            subtotal  = abap_true
            sequence   = if_salv_c_sort=>sort_up ).
        CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
          CLEAR lr_sorts.                              "#EC NO_HANDLER.
      ENDTRY.

    ENDIF.
  ENDIF.

ENDFORM.                    "set_sort_alv
*&---------------------------------------------------------------------*
*&      Form  LOAD_REPORT_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM load_report_structure USING gr_table TYPE REF TO cl_salv_table.
* Set technical columns

  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column.

  DATA: l_fname    TYPE lvc_fname,
        l_medtext  TYPE scrtext_m,
        l_longtext TYPE scrtext_l,
        l_tooltip  TYPE lvc_tip.

  lr_columns = gr_table->get_columns( ).

*  IF DETAIL = 'X'.
  IF matrix = ' '.


    TRY.
        lr_column = lr_columns->get_column( 'LGART' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( 'Wage Type ' ).
        lr_column->set_long_text( 'Wage Type ' ).
        lr_column->set_long_text( 'Wage Type' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.
    TRY.
        lr_column = lr_columns->get_column( 'LGTXT' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( 'Description' ).
        lr_column->set_long_text( 'Description' ).
*        LR_COLUMN->SET_LONG_TEXT( 'Description' ).
        lr_column->set_output_length( 25 ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'SRTSEQ' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( 'Sort Seq' ).
        lr_column->set_long_text( 'Sort Seq' ).
        lr_column->set_long_text( 'Sort Seq' ).
        lr_column->set_visible( abap_off ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.
    TRY.
        lr_column = lr_columns->get_column( 'CLASS' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( 'Class' ).
        lr_column->set_long_text( 'Class' ).
        lr_column->set_long_text( 'Class' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.
    TRY.
        lr_column = lr_columns->get_column( 'COL1' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( 'Number' ).
        lr_column->set_long_text( 'Number' ).
        lr_column->set_output_length( 4 ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.
    TRY.
        lr_column = lr_columns->get_column( 'COL2' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( 'Class Text' ).
        lr_column->set_long_text( 'Class Text' ).
        lr_column->set_output_length( 40 ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.
    TRY.
        lr_column = lr_columns->get_column( 'COL3' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( 'Class Value' ).
        lr_column->set_long_text( 'Class Value' ).
        lr_column->set_output_length( 4 ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.
    TRY.
        lr_column = lr_columns->get_column( 'COL4' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( '' ).
        lr_column->set_long_text( 'Value Text' ).
        lr_column->set_output_length( 40 ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.
  ENDIF.
* Matrix listing.
  IF matrix = 'X'.
    TRY.
        lr_column = lr_columns->get_column( 'DESC' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_medium_text( 'Class Description.' ).
        lr_column->set_long_text( 'Class Description.' ).
        lr_column->set_output_length( 40 ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
        CLEAR lr_column.
    ENDTRY.
*
* add data columns from wage type table ( all the same )
    LOOP AT wt_tab.
      fs1-num = wt_tab-colno.
      ASSIGN fs1 TO <col>.
*
*      L_MEDTEXT = L_LONGTEXT = WT_TAB-LGART.
      CONCATENATE wt_tab-lgart wt_tab-lgtxt INTO l_longtext.
      CONCATENATE wt_tab-lgart wt_tab-lgtxt INTO l_medtext.

      l_fname = <col>.

      TRY.
          lr_column = lr_columns->get_column( l_fname ).
          lr_column->set_short_text( ' ' ).
          lr_column->set_medium_text( l_medtext ).
          lr_column->set_long_text( l_longtext ).
          lr_column->set_output_length( 4 ).
          IF NOT wt_tab-lgtxt IS INITIAL.
            l_tooltip = wt_tab-lgtxt.
            lr_column->set_tooltip( l_tooltip ).
          ENDIF.
        CATCH cx_salv_not_found.                        "#EC NO_HANDLER
          CLEAR lr_column.
      ENDTRY.

    ENDLOOP.
*
    DATA: l_color_column TYPE lvc_fname.
    MOVE 'COLINFO' TO l_color_column.
    TRY.
        lr_columns->set_color_column( l_color_column ).
      CATCH cx_salv_data_error .
        CLEAR l_color_column.
    ENDTRY.
*
  ENDIF.

*
ENDFORM.                    " LOAD_REPORT_STRUCTURE*&---------------------------------------------------------------------*
*
