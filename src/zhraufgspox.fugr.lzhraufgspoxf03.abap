*----------------------------------------------------------------------*
*   INCLUDE LSPOXF03                                                   *
*----------------------------------------------------------------------*

FORM handle_call_error.
  DATA: msg(100).

  IF sy-subrc NE 0.
    IF sy-msgno IS INITIAL.
      CASE sy-subrc.
        WHEN 1. msg = 'AREA oder OP fehlt'(001).
        WHEN 2. msg = 'Bereich nicht bekannt'(002).
        WHEN 3. msg = 'Operation in Bereich nicht bekannt'(003).
        WHEN 4. msg = 'Fehler in Parametersatz der Operation'(004).
        WHEN 5. msg = 'Fehler im Wert eines Operartionsparameters'(005).
        WHEN OTHERS.
          msg = 'Fehler in Operation: '(000).
          msg+89(10) = sy-subrc.
          CONDENSE msg.
          MESSAGE s549 WITH msg RAISING operation_failed.
      ENDCASE.
      MESSAGE e549 WITH msg RAISING call_error.
    ELSE.
      IF sy-subrc > 5.
        route_exception operation_failed.
      ELSE.
        route_exception call_error.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                               " HANDLE_CALL_ERROR

*---------------------------------------------------------------------*
*       FORM end_call                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  rc                                                            *
*---------------------------------------------------------------------*
FORM end_call CHANGING rc.
  IF sy-subrc NE 0.
    IF rc IS INITIAL.
      rc = '1'.
    ENDIF.
  ENDIF.
ENDFORM.                    "end_call

*&---------------------------------------------------------------------*
*&      Form  check_call_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->MSG        text
*      -->RC         text
*----------------------------------------------------------------------*
FORM check_call_error USING msg rc.

  DATA: tracelevel TYPE i.
  tracelevel = 1.

  IF sy-subrc NE 0.
    PERFORM write1_trace1 USING tracelevel 'called by %s\n' caller.
    CLEAR caller.
    IF sy-msgid IS INITIAL.
      MESSAGE e003(spoc) WITH msg ' ' rc ' '
      RAISING operation_failed.                         "#EC *
    ELSE.
      route_exception operation_failed.
    ENDIF.
  ENDIF.
ENDFORM.                    "check_call_error
