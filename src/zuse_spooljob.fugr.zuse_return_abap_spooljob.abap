FUNCTION ZUSE_RETURN_ABAP_SPOOLJOB.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(RQIDENT) LIKE  TSP01-RQIDENT
*"     VALUE(FIRST_LINE) TYPE  I DEFAULT 1
*"     VALUE(LAST_LINE) TYPE  I OPTIONAL
*"     VALUE(PAGES) TYPE  C OPTIONAL
*"  TABLES
*"      BUFFER
*"  EXCEPTIONS
*"      NO_SUCH_JOB
*"      NOT_ABAP_LIST
*"      JOB_CONTAINS_NO_DATA
*"      SELECTION_EMPTY
*"      NO_PERMISSION
*"      CAN_NOT_ACCESS
*"      READ_ERROR
*"----------------------------------------------------------------------
  DATA: mem_tab LIKE abaplist OCCURS 10.
  DATA: data_is_otf TYPE c.
  DATA: subrc TYPE sy-subrc.
  DATA: c_subrc(10).
  DATA: rqid TYPE rqident.
  DATA: doctype TYPE rspodoctyp.
  DATA  l_msg TYPE symsg.

  CLEAR: subrc, c_subrc.
  rqid = rqident.

  CALL FUNCTION 'RSPO_CHECK_JOB_ID_PERMISSION'
    EXPORTING
      rqident       = rqident
      access        = 'DISP'
    EXCEPTIONS
      no_such_job   = 1
      no_permission = 2.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    c_subrc = subrc.
    CONDENSE c_subrc.
    PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
                              'RSPO_CHECK_JOB_ID_PERMISSION' "#EC NOTEXT
                              'Exception:'                  "#EC NOTEXT
                              c_subrc.
    CASE subrc.
      WHEN 1. RAISE no_such_job.
      WHEN 2. RAISE no_permission.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'RSPO_GET_TYPE_SPOOLJOB'
    EXPORTING
      rqident        = rqident
    IMPORTING
      is_otf         = data_is_otf
      doctype        = doctype
    EXCEPTIONS
      can_not_access = 1.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    c_subrc = subrc.
    CONDENSE c_subrc.
    PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
                              'RSPO_GET_TYPE_SPOOLJOB'      "#EC NOTEXT
                              'Exception:'                  "#EC NOTEXT
                              c_subrc.
    CASE subrc.
      WHEN 1. RAISE can_not_access.
    ENDCASE.
  ENDIF.

  IF last_line NE 0 AND last_line < first_line.
    RAISE selection_empty.
  ENDIF.

  IF data_is_otf = 'X' OR doctype <> 'LIST'.
    RAISE not_abap_list.
  ELSE.
    SUBMIT rspolist EXPORTING LIST TO MEMORY AND RETURN
                    WITH rqident = rqident
                    WITH first = first_line
                    WITH last = last_line
                    WITH pages = pages.
    IMPORT msg = l_msg FROM MEMORY ID 'RSPOLIST'.
    IF l_msg IS NOT INITIAL.
      FREE MEMORY ID 'RSPOLIST'.
      MESSAGE ID l_msg-msgid TYPE c_type_error NUMBER l_msg-msgno
       WITH l_msg-msgv1 l_msg-msgv2 l_msg-msgv3 l_msg-msgv4 RAISING read_error.
    ENDIF.


    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = mem_tab
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      c_subrc = sy-subrc.
      CONDENSE c_subrc.
      PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
                                'LIST_FROM_MEMORY'          "#EC NOTEXT
                                'Exception:'                "#EC NOTEXT
                                c_subrc.
      FREE MEMORY ID 'LAST_PAGE'.
      FREE MEMORY ID 'CURRENT_SPOOLID'.
      RAISE read_error.
    ENDIF.

**    CALL FUNCTION 'LIST_TO_ASCI'
***        EXPORTING
***             LIST_INDEX         = -1
**      TABLES
**        listasci           = buffer
**        listobject         = mem_tab
**      EXCEPTIONS
**        empty_list         = 1
**        list_index_invalid = 2
**        OTHERS             = 3.
**    IF sy-subrc <> 0.
**      subrc = sy-subrc.
**      IF subrc = 1.
**        IF first_line <> 1 OR NOT last_line IS INITIAL.
**          PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
**                                    'LIST_TO_ASCI'          "#EC NOTEXT
**                                    'Exception:'            "#EC NOTEXT
**                                    'SELECTION_EMPTY'.      "#EC NOTEXT
**          RAISE selection_empty.
**        ELSE.
**          PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
**                                     'LIST_TO_ASCI'         "#EC NOTEXT
**                                     'Exception:'           "#EC NOTEXT
**                                     'JOB_CONTAINS_NO_DATA'. "#EC NOTEXT
**          RAISE job_contains_no_data.
**        ENDIF.
**      ELSE.
**        PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
**                                    'LIST_TO_ASCI'          "#EC NOTEXT
**                                    'Exception:'            "#EC NOTEXT
**                                    'READ_ERROR'.           "#EC NOTEXT
**        RAISE read_error.
**      ENDIF.
**    ENDIF.
**
**    CALL FUNCTION 'LIST_FREE_MEMORY'
**      TABLES
**        listobject = mem_tab
**      EXCEPTIONS
**        OTHERS     = 1.
**    IF sy-subrc <> 0.
**      FREE MEMORY ID 'LAST_PAGE'.
**      FREE MEMORY ID 'CURRENT_SPOOLID'.
**      RAISE read_error.
**    ENDIF.
   ENDIF.

ENDFUNCTION.
