*----------------------------------------------------------------------*
*   INCLUDE LSPOXF04                                                   *
*----------------------------------------------------------------------*

form add_contents tables contents structure rspotoce using add ident.
  tsp02a-pjident = ident.
  tsp02a-pjnumber = 0.
  tsp02a-param = sattr_contents.
  tsp02a-line = 0.
  if add is initial.
    delete from tsp02a where pjident = tsp02a-pjident and
                             pjnumber = tsp02a-pjnumber and
                             param    = tsp02a-param.
  else.
    select count(*) from tsp02a where  pjident = tsp02a-pjident and
                                       pjnumber = tsp02a-pjnumber and
                                       param    = tsp02a-param.
    tsp02a-line = sy-dbcnt.
  endif.
  loop at contents.
      select count(*) from tsp02a where pjident  = tsp02a-pjident and
                                        pjnumber = tsp02a-pjnumber and
                                        param    = tsp02a-param and
                                        value    = contents.
      if sy-dbcnt = 0.
        add 1 to tsp02a-line.
        tsp02a-value = contents.
        insert tsp02a.
        if sy-subrc ne 0.
          message e418 raising incomplete.
        endif.
      endif.
  endloop.
endform.

form set_handler.
  perform on_commit on commit.
  perform on_rollback on rollback.
endform.

form on_commit.
  data: l type i.
  describe table toc lines toc_committed.

  loop at spool_handles.
    describe table spool_handles-toc lines l.
    if l > 0.
      perform add_contents tables spool_handles-toc
                           using 'X' spool_handles-rqid.
      refresh spool_handles-toc.
      modify spool_handles.
    endif.
  endloop.
endform.

form on_rollback.
  data: s type i.
  s = toc_committed + 1.
  delete toc from s.

  loop at spool_handles.
    refresh spool_handles-toc.
    modify spool_handles.
  endloop.
endform.
