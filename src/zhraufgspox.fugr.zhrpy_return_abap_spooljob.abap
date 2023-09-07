function zhrpy_return_abap_spooljob.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(RQIDENT) LIKE  TSP01-RQIDENT
*"     VALUE(FIRST_LINE) TYPE  I DEFAULT 1
*"     VALUE(LAST_LINE) TYPE  I OPTIONAL
*"     VALUE(PAGES) TYPE  C OPTIONAL
*"  EXPORTING
*"     REFERENCE(LIST_OBJECT) TYPE  TABLE_ABAPLIST
*"  EXCEPTIONS
*"      NO_SUCH_JOB
*"      NOT_ABAP_LIST
*"      JOB_CONTAINS_NO_DATA
*"      SELECTION_EMPTY
*"      NO_PERMISSION
*"      CAN_NOT_ACCESS
*"      READ_ERROR
*"----------------------------------------------------------------------
  data: mem_tab like abaplist occurs 10.
  data: data_is_otf type c.
  data: subrc type sy-subrc.
  data: c_subrc(10).
  data: rqid type rqident.
  data: doctype type rspodoctyp.
  data  l_msg type symsg.

  clear: subrc, c_subrc.
  rqid = rqident.

  call function 'RSPO_CHECK_JOB_ID_PERMISSION'
    exporting
      rqident       = rqident
      access        = 'DISP'
    exceptions
      no_such_job   = 1
      no_permission = 2.
  if sy-subrc <> 0.
    subrc = sy-subrc.
    c_subrc = subrc.
    condense c_subrc.
    perform write_trace using 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
                              'RSPO_CHECK_JOB_ID_PERMISSION' "#EC NOTEXT
                              'Exception:'                  "#EC NOTEXT
                              c_subrc.
    case subrc.
      when 1. raise no_such_job.
      when 2. raise no_permission.
    endcase.
  endif.

  call function 'RSPO_GET_TYPE_SPOOLJOB'
    exporting
      rqident        = rqident
    importing
      is_otf         = data_is_otf
      doctype        = doctype
    exceptions
      can_not_access = 1.
  if sy-subrc <> 0.
    subrc = sy-subrc.
    c_subrc = subrc.
    condense c_subrc.
    perform write_trace using 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
                              'RSPO_GET_TYPE_SPOOLJOB'      "#EC NOTEXT
                              'Exception:'                  "#EC NOTEXT
                              c_subrc.
    case subrc.
      when 1. raise can_not_access.
    endcase.
  endif.

  if last_line ne 0 and last_line < first_line.
    raise selection_empty.
  endif.

  if data_is_otf = 'X' or doctype <> 'LIST'.
    raise not_abap_list.
  else.
    submit rspolist exporting list to memory and return
                    with rqident = rqident
                    with first = first_line
                    with last = last_line
                    with pages = pages.
    import msg = l_msg from memory id 'RSPOLIST'.
    if l_msg is not initial.
      free memory id 'RSPOLIST'.
      message id l_msg-msgid type c_type_error number l_msg-msgno
       with l_msg-msgv1 l_msg-msgv2 l_msg-msgv3 l_msg-msgv4 raising read_error.
    endif.


    call function 'LIST_FROM_MEMORY'
      tables
        listobject = mem_tab
      exceptions
        not_found  = 1
        others     = 2.
    if sy-subrc <> 0.
      c_subrc = sy-subrc.
      condense c_subrc.
      perform write_trace using 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
                                'LIST_FROM_MEMORY'          "#EC NOTEXT
                                'Exception:'                "#EC NOTEXT
                                c_subrc.
      free memory id 'LAST_PAGE'.
      free memory id 'CURRENT_SPOOLID'.
      raise read_error.
    else.
      list_object[] = mem_tab[].
    endif.

*    CALL FUNCTION 'LIST_TO_ASCI'
**        EXPORTING
**             LIST_INDEX         = -1
*      TABLES
*        listasci           = buffer
*        listobject         = mem_tab
*      EXCEPTIONS
*        empty_list         = 1
*        list_index_invalid = 2
*        OTHERS             = 3.
*    IF sy-subrc <> 0.
*      subrc = sy-subrc.
*      IF subrc = 1.
*        IF first_line <> 1 OR NOT last_line IS INITIAL.
*          PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
*                                    'LIST_TO_ASCI'          "#EC NOTEXT
*                                    'Exception:'            "#EC NOTEXT
*                                    'SELECTION_EMPTY'.      "#EC NOTEXT
*          RAISE selection_empty.
*        ELSE.
*          PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
*                                     'LIST_TO_ASCI'         "#EC NOTEXT
*                                     'Exception:'           "#EC NOTEXT
*                                     'JOB_CONTAINS_NO_DATA'. "#EC NOTEXT
*          RAISE job_contains_no_data.
*        ENDIF.
*      ELSE.
*        PERFORM write_trace USING 'RSPO_RETURN_ABAP_SPOOLJOB %s %s %s\n' "#EC NOTEXT
*                                    'LIST_TO_ASCI'          "#EC NOTEXT
*                                    'Exception:'            "#EC NOTEXT
*                                    'READ_ERROR'.           "#EC NOTEXT
*        RAISE read_error.
*      ENDIF.
*    ENDIF.

    call function 'LIST_FREE_MEMORY'
      tables
        listobject = mem_tab
      exceptions
        others     = 1.
    if sy-subrc <> 0.
      free memory id 'LAST_PAGE'.
      free memory id 'CURRENT_SPOOLID'.
      raise read_error.
    endif.
  endif.

endfunction.
