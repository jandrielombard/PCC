*----------------------------------------------------------------------*
***INCLUDE Hilfsroutinen zu offenem Spoolauftrag
*----------------------------------------------------------------------*


FORM set_handle USING handle.
  READ TABLE spool_handles INDEX handle.
  IF sy-subrc <> 0 OR spool_handles-active IS INITIAL.
    MESSAGE e463 RAISING handle_not_valid.
  ENDIF.
ENDFORM.                    "set_handle

*---------------------------------------------------------------------*
*       FORM get_request2                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  rqident                                                       *
*  -->  rq                                                            *
*---------------------------------------------------------------------*
FORM get_request2 CHANGING rqident rq LIKE tsp01.
  IF rq IS INITIAL.
    PERFORM get_request USING rqident CHANGING rq.
  ELSE.
    rqident = rq-rqident.
  ENDIF.
ENDFORM.                    "get_request2

*&---------------------------------------------------------------------*
*&      Form  check_comp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RQ         text
*----------------------------------------------------------------------*
FORM check_comp USING rq LIKE tsp01.
  IF rq-rqdoctype <> 'COMP'.
    MESSAGE e462 WITH rq-rqident RAISING no_comp_job.
  ENDIF.
ENDFORM.                    "check_comp

*---------------------------------------------------------------------*
*       FORM get_request                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  rqident                                                       *
*  -->  rq                                                            *
*---------------------------------------------------------------------*
FORM get_request USING rqident CHANGING rq LIKE tsp01.
  SELECT SINGLE * FROM tsp01 INTO rq WHERE rqident = rqident.
  IF sy-subrc <> 0.
    MESSAGE e126 WITH rqident RAISING no_such_job.
  ENDIF.
ENDFORM.                    "get_request

*&---------------------------------------------------------------------*
*&      Form  get_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PJIDENT    text
*      -->PJNUMBER   text
*      -->PJ         text
*----------------------------------------------------------------------*
FORM get_job USING pjident pjnumber CHANGING pj LIKE tsp02.
  SELECT SINGLE * FROM tsp02 INTO pj WHERE pjident = pjident
                                     AND   pjnummer = pjnumber.
  IF sy-subrc <> 0.
    MESSAGE e306 RAISING no_such_job.
  ENDIF.
ENDFORM.                    "get_job

*---------------------------------------------------------------------*
*       FORM get_incomplete_request                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  rqident                                                       *
*  -->  rq                                                            *
*---------------------------------------------------------------------*
FORM get_incomplete_request USING rqident CHANGING rq LIKE tsp01.
  PERFORM get_request USING rqident CHANGING rq.

  IF rq-rqfinal = 'C'.
    MESSAGE e461 WITH rqident RAISING job_final.
  ENDIF.
ENDFORM.                    "get_incomplete_request

*&---------------------------------------------------------------------*
*&      Form  build_title
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RQ         text
*      -->TITLE      text
*----------------------------------------------------------------------*
FORM build_title USING rq LIKE tsp01 CHANGING title.
  DATA: num(12).
  num = rq-rqident.
  CONCATENATE rq-rq0name rq-rq1name rq-rq2name num
                                    INTO title SEPARATED BY space.
ENDFORM.                    "build_title

*&---------------------------------------------------------------------*
*&      Form  get_expiration
*&---------------------------------------------------------------------*
*       note 591430
*----------------------------------------------------------------------*
FORM get_expiration  USING    deltime
                     CHANGING document_data STRUCTURE sodocchgi1.

  DATA: exp_value.
  CALL FUNCTION 'RSPO_OPTION_GET'
    EXPORTING
      name  = spopt_so_no_expire
    IMPORTING
      value = exp_value.

  IF exp_value IS INITIAL.
    DATA: expdat LIKE sy-datum.
    MOVE deltime TO expdat.
    IF expdat LE sy-datum.
      document_data-expiry_dat = sy-datum + 10.
      document_data-obj_expdat = sy-datum + 10.
    ELSE.
      document_data-expiry_dat = expdat.
      document_data-obj_expdat = expdat.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_expiration

*&---------------------------------------------------------------------*
*&      Form  move_attributes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM move_attributes  USING    tsp01attr STRUCTURE tsp01
                      CHANGING spoolattr STRUCTURE bapixmspoolid.

  MOVE tsp01attr-rqident TO spoolattr-spoolid.
  MOVE tsp01attr-rqclient TO spoolattr-client.
  MOVE tsp01attr-rq0name TO spoolattr-name.
  MOVE tsp01attr-rq1name TO spoolattr-suffix1.
  MOVE tsp01attr-rq2name TO spoolattr-suffix2.
  MOVE tsp01attr-rqowner TO spoolattr-owner.
  MOVE tsp01attr-rqfinal TO spoolattr-final.
  MOVE tsp01attr-rqcretime TO spoolattr-crtime.
  MOVE tsp01attr-rqdeltime TO spoolattr-dltime.
  MOVE tsp01attr-rqapprule TO spoolattr-spopages.
  MOVE tsp01attr-rq1dispo TO spoolattr-printtime.
  MOVE tsp01attr-rq2dispo TO spoolattr-delafterprint.
  MOVE tsp01attr-rqdest TO spoolattr-device.
  MOVE tsp01attr-rqcopies TO spoolattr-copies.
  MOVE tsp01attr-rqprio TO spoolattr-priority.
  MOVE tsp01attr-rqpaper TO spoolattr-spoformat.
  MOVE tsp01attr-rqpjreq TO spoolattr-pjtotal.
  MOVE tsp01attr-rqpjdone TO spoolattr-pjdone.
  MOVE tsp01attr-rqpjserr TO spoolattr-pjproblem.
  MOVE tsp01attr-rqpjherr TO spoolattr-pjerror.
  MOVE tsp01attr-rqwriter TO spoolattr-writer.
  MOVE tsp01attr-rqerror TO spoolattr-sperror.
  MOVE tsp01attr-rqo1name TO spoolattr-temsename.
  MOVE tsp01attr-rqo1part TO spoolattr-temsepart.
  MOVE tsp01attr-rqo1clie TO spoolattr-temseclient.
  MOVE tsp01attr-rqtitle TO spoolattr-title.
  MOVE tsp01attr-rqsaptitle TO spoolattr-sapcover.
  MOVE tsp01attr-rqunxtitle TO spoolattr-oscover.
  MOVE tsp01attr-rqreceiver TO spoolattr-receiver.
  MOVE tsp01attr-rqdivision TO spoolattr-division.
  MOVE tsp01attr-rqauth TO spoolattr-authority.
  MOVE tsp01attr-rqmodtime TO spoolattr-modtime.
  MOVE tsp01attr-rqdoctype TO spoolattr-doctyp.
  MOVE tsp01attr-rqposname TO spoolattr-osname.

ENDFORM.                    " move_attributes

*&---------------------------------------------------------------------*
*&      Form  CHECK_DEBUGGER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_debugger .

  DATA: debug_on TYPE tspoptions-value,
    n TYPE i.

  IF NOT sy-batch IS INITIAL.
    CALL FUNCTION 'RSPO_OPTION_GET'
      EXPORTING
        name  = spopt_spool_debug
      IMPORTING
        value = debug_on.
    IF NOT debug_on IS INITIAL.
      WHILE n IS INITIAL.
      ENDWHILE.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_DEBUGGER

*&---------------------------------------------------------------------*
*&      Form  SET_MAIL_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MAIL_TITLE  text
*      <--P_DOCUMENT_DATA_OBJ_DESCR  text
*----------------------------------------------------------------------*
FORM set_mail_title  CHANGING mail_title
                              document_data-obj_descr.

  DATA: callstack TYPE sys_callst,
  caller TYPE sys_calls,
  use_spool_title,
  max_level TYPE i VALUE 3.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level    = max_level
    IMPORTING
      et_callstack = callstack.

  READ TABLE callstack INDEX max_level INTO caller.

  IF caller-progname = 'RSBTCRTE' OR
     caller-progname = 'SAPLSPOOL_SP01R' OR
     caller-progname = 'BTC_SEND_REMAINING_SPOOLISTS'.

    CALL FUNCTION 'RSPO_OPTION_GET'
      EXPORTING
        name  = spopt_use_spool_title
      IMPORTING
        value = use_spool_title.

    IF use_spool_title IS NOT INITIAL AND tsp01-rqtitle IS NOT INITIAL.
      mail_title = tsp01-rqtitle.
    ENDIF.
  ENDIF.

  IF mail_title IS INITIAL.
    document_data-obj_descr = sy-title.
  ELSE.
    document_data-obj_descr = mail_title.
  ENDIF.

  CONDENSE document_data-obj_descr.

ENDFORM.                    " SET_MAIL_TITLE
*&---------------------------------------------------------------------*
*&      Form  FILL_OBJECT_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OBJECT_HEADER  text
*----------------------------------------------------------------------*
FORM fill_object_header  TABLES   object_header STRUCTURE solisti1.

  DATA: wa_header TYPE solisti1.

  IF tsp01-rqdoctype = 'OTF'.
    wa_header-line = c_sapconnect_sapscript.
  ELSEIF tsp01-rqdoctype = 'SMART'.
    wa_header-line = c_sapconnect_smart_forms.
  ENDIF.
  APPEND wa_header TO object_header.
  CONCATENATE 'TDDEST' '=' tsp01-rqdest INTO wa_header-line.
  APPEND wa_header TO object_header.
  CONCATENATE 'TDDATASET' '=' tsp01-rq0name INTO wa_header-line.
  APPEND wa_header TO object_header.
  CONCATENATE 'TDSUFFIX1' '=' tsp01-rq0name INTO wa_header-line.
  APPEND wa_header TO object_header.
  CONCATENATE 'TDTITLE' '=' tsp01-rqtitle INTO wa_header-line.
  APPEND wa_header TO object_header.

ENDFORM.                    " FILL_OBJECT_HEADER
*&---------------------------------------------------------------------*
*&      Form  CREATE_PACKING_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PLIST  text
*      -->P_SO_ALI  text
*      -->P_DOCUMENT_DATA  text
*----------------------------------------------------------------------*
FORM create_packing_list  TABLES   plist STRUCTURE sopcklsti1
                                   so_ali STRUCTURE soli
                                   object_header STRUCTURE solisti1
                          USING    document_data STRUCTURE sodocchgi1
                                   real_type.

  DATA: num_lines TYPE i.
  DATA: line_size TYPE i.

  REFRESH plist.
  CLEAR plist.
  plist-transf_bin = 'X'.
  plist-head_start = 0.
  plist-head_num = 0.
  plist-body_start = 0.
  plist-body_num = 0.
  plist-doc_type = 'RAW'.
  plist-obj_descr = document_data-obj_descr.
  APPEND plist.
  plist-transf_bin = 'X'.
  DESCRIBE TABLE object_header LINES num_lines.
  plist-head_start = 1.
  plist-head_num = num_lines.
  plist-body_start = 1.
  DESCRIBE TABLE so_ali LINES plist-body_num.
  plist-doc_type = real_type.
* Insert note 690074 to calculate document size
  CLASS cl_abap_char_utilities DEFINITION LOAD.
  IF cl_abap_char_utilities=>charsize > 1. " system is Unicode
    line_size = 510.
  ELSE.
    line_size = 255.
  ENDIF.
  READ TABLE so_ali INDEX plist-body_num.
  plist-doc_size = ( plist-body_num - 1 ) * line_size
                   + STRLEN( so_ali ).
  APPEND plist.

ENDFORM.                    " CREATE_PACKING_LIST

*&---------------------------------------------------------------------*
*&      Form  WRITE_CALLSTACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_callstack USING p_function TYPE RS38L_FNAM.

  DATA: trace_on TYPE tspoptions-VALUE.
  DATA: callstack TYPE abap_callstack.
  DATA: callstack_line TYPE abap_callstack_line.
  DATA: blockname(20).
  DATA: uname TYPE sy-uname.
  DATA: tcode TYPE sy-tcode.
  DATA: rest TYPE string.

  get_option 'WRITE_CALLSTACK' trace_on.

  IF trace_on = abap_true.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack = callstack.

    uname = sy-uname.
    tcode = sy-tcode.

    PERFORM write_trace USING 'Callstack %s %s %s\n'        "#EC NOTEXT
          p_function
          uname
          tcode.

    LOOP AT callstack INTO callstack_line.

      blockname = callstack_line-blockname.
      CONDENSE: blockname,
      callstack_line-mainprogram,
      callstack_line-INCLUDE.

      SPLIT callstack_line-mainprogram AT '=' INTO
      callstack_line-mainprogram rest.

      IF callstack_line-mainprogram <> 'SAPLSPOX'.
        PERFORM write_trace USING 'Callstack %s %s %s\n'     "#EC NOTEXT
              callstack_line-mainprogram
              callstack_line-INCLUDE
              blockname.
      ENDIF.
    ENDLOOP.


  ENDIF.

ENDFORM.                    " WRITE_CALLSTACK
