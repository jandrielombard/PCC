*----------------------------------------------------------------------*
*   Parts                                                              *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  CHECK_PARTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PARTS  text
*      <--P_PGC  text
*----------------------------------------------------------------------*
form check_parts tables   parts structure rspopart
                          toc   structure rspotoce
                 using ptype
                 changing pgc.
  data: rq like tsp01.
  data: pg type i.

  refresh toc.
  clear toc.

  pgc = 0.
  loop at parts.
    call function 'RSPO_GET_PAGES_SPOOLJOB'
         exporting
              rqident     = parts-rqsubid
         importing
              pages       = pg
              rq          = rq
         exceptions
              no_such_job = 1
*             OTHERS      = 2
              .
    if sy-subrc <> 0.
      message e126 with parts-rqsubid raising no_such_part.
    endif.
    if rq-rqfinal <> 'C'.
      message e464 with rq-rqident raising job_not_final.
    endif.
    call function 'RSPO_CHECK_JOB_PERMISSION'
         exporting
              access        = 'COMP'
              spoolreq      = rq
         exceptions
              no_permission = 1
              others        = 2.
    if sy-subrc <> 0.
      route_exception no_permission.
    endif.

    perform add_printer using rq-rqdest.
    if prts-type <> ptype.
      message e465 with prts-type rq-rqident
                                        raising illegal_device_type.
    endif.
    if parts-rqdesc is initial.
      parts-rqdesc = rq-rqtitle.
      modify parts.
    endif.
    toc-title = parts-rqdesc.
    if toc-title is initial.
      perform build_title using rq changing toc-title.
    endif.
    toc-page = pgc.
    append toc.
    pgc = pgc + pg.
  endloop.

endform.                               " CHECK_PARTS






















