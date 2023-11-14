*&---------------------------------------------------------------------*
*& Include          LSPOXUTS
*&---------------------------------------------------------------------*

CLASS lcl_fugr_spox_unittest DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS getspoolashtml FOR TESTING.

    METHODS returnspooljob FOR TESTING.

    METHODS return_spooljob_dat FOR TESTING.

    METHODS getspoolid RETURNING VALUE(r_rq) TYPE rspoid.

    METHODS unitassert.

ENDCLASS.

CLASS lcl_fugr_spox_unittest IMPLEMENTATION.

  METHOD getspoolashtml.

    DATA l_rq TYPE rspoid.
    DATA i_zip TYPE xstring.

    l_rq = getspoolid( ).

    CHECK l_rq <> 0.

    CALL FUNCTION 'RSPO_RETURN_SPOOLJOB_HTML'
      EXPORTING
        rqident              = l_rq
*       FIRST_LINE           = 1
*       LAST_LINE            =
*       PAGES                =
        compressed           = abap_true
      IMPORTING
*       HTML_FILES           =
        e_zip                = i_zip
      EXCEPTIONS
        no_such_job          = 1
        not_abap_list        = 2
        job_contains_no_data = 3
        selection_empty      = 4
        no_permission        = 5
        can_not_access       = 6
        read_error           = 7
        conversion_error     = 8
        cannot_compress      = 9
        OTHERS               = 10.
    IF sy-subrc = 8 OR sy-subrc = 9 OR ( sy-subrc = 0 AND xstrlen( i_zip ) = 0 ).
      unitassert( ).
    ENDIF.

  ENDMETHOD.

  METHOD returnspooljob.

    DATA l_rq TYPE rspoid.
    DATA lt_buff TYPE soli_tab.

    l_rq = getspoolid( ).

    CHECK l_rq <> 0.

    CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
      EXPORTING
        rqident              = l_rq
*       FIRST_LINE           = 1
*       LAST_LINE            =
*       DESIRED_TYPE         =
*     IMPORTING
*       REAL_TYPE            =
*       SP_LANG              =
      TABLES
        buffer               = lt_buff
*       BUFFER_PDF           =
      EXCEPTIONS
        no_such_job          = 1
        job_contains_no_data = 1
        selection_empty      = 2
        no_permission        = 1
        can_not_access       = 2
        read_error           = 2
        type_no_match        = 1
        OTHERS               = 2.
    IF sy-subrc = 2 OR ( sy-subrc = 0 AND lines( lt_buff ) = 0 ).
      unitassert( ).
    ENDIF.

  ENDMETHOD.

  METHOD return_spooljob_dat.

    DATA l_rq TYPE rspoid.
    DATA et_dat TYPE list_string_table.

    l_rq = getspoolid( ).

    CHECK l_rq <> 0.

    CALL FUNCTION 'RSPO_RETURN_SPOOLJOB_DAT'
      EXPORTING
        rqident              = l_rq
        pages                = 'X'
      IMPORTING
        buffer_dat           = et_dat
      EXCEPTIONS
        no_such_job          = 1
        not_abap_list        = 1
        job_contains_no_data = 1
        selection_empty      = 2
        no_permission        = 1
        can_not_access       = 2
        read_error           = 2
        OTHERS               = 2.
    IF sy-subrc = 2 OR ( sy-subrc = 0 AND et_dat IS INITIAL ).
      unitassert( ).
    ENDIF.

  ENDMETHOD.


  METHOD getspoolid.

    SELECT rqident FROM tsp01 INTO r_rq WHERE rqdoctype = 'LIST' AND rqwriter = 0 AND rqapprule > 0
      AND rqpaper <> 'X_SPOOLERR'.
      EXIT.
    ENDSELECT.

  ENDMETHOD.


  METHOD unitassert.

    DATA l_errmsg TYPE string.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_errmsg.
    cl_aunit_assert=>fail( msg = l_errmsg ).

  ENDMETHOD.

ENDCLASS.
