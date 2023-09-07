*----------------------------------------------------------------------*
*   INCLUDE LSPOXF02                                                   *
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM get_pages                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  new                                                           *
*  -->  old                                                           *
*---------------------------------------------------------------------*
* form get_pages changing new old.
*   data: pc type i value 0.
*
*  call 'C_PRINT_PAGE_BREAK'
*     id 'HANDLE'       field spool_handles-handle
*     id 'ADDPAGECOUNT' field pc
*     id 'NEWPAGES'     field new
*     id 'OLDPAGES'     field old
*     id 'RC'           field rc
*     id 'MESSAGE'      field errmsg.
* endform.

*---------------------------------------------------------------------*
*       FORM get_new_pages                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  pages                                                         *
*---------------------------------------------------------------------*
* form get_new_pages changing pages.
*   data: pc type i value 0.
*
*   call 'C_PRINT_PAGE_BREAK'
*      id 'HANDLE'       field spool_handles-handle
*      id 'ADDPAGECOUNT' field pc
*      id 'NEWPAGES'     field pages
*      id 'RC'           field rc
*      id 'MESSAGE'      field errmsg.
* endform.

*---------------------------------------------------------------------*
*       FORM add_new_pages                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  pages                                                         *
*---------------------------------------------------------------------*
* form add_new_pages using pages.
*   call 'C_PRINT_PAGE_BREAK'
*      id 'HANDLE'       field spool_handles-handle
*      id 'ADDPAGECOUNT' field pages
*      id 'RC'           field rc
*      id 'MESSAGE'      field errmsg.
* endform.

*---------------------------------------------------------------------*
*       FORM add_new_pages2                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  pages                                                         *
*  -->  g                                                             *
*---------------------------------------------------------------------*
form add_new_pages2 using pages changing g.
  call 'C_PRINT_PAGE_BREAK'
     id 'HANDLE'       field spool_handles-handle
     id 'ADDPAGECOUNT' field pages
     id 'NEWPAGES'     field g
     id 'RC'           field rc
     id 'MESSAGE'      field errmsg.
endform.

*---------------------------------------------------------------------*
*       FORM set_new_pages                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  pages                                                         *
*---------------------------------------------------------------------*
* form set_new_pages changing pages.
*   call 'C_PRINT_PAGE_BREAK'
*      id 'HANDLE'       field spool_handles-handle
*      id 'PAGECOUNT'    field pages
*      id 'RC'           field rc
*      id 'MESSAGE'      field errmsg.
* endform.

*---------------------------------------------------------------------*
*       FORM get_parts                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  new                                                           *
*  -->  old                                                           *
*---------------------------------------------------------------------*
form get_parts changing new old.
  data: pc type i value 0.

  call 'C_PRINT_LINE'
     id 'HANDLE'       field spool_handles-handle
     id 'ADDPARTCOUNT' field pc
     id 'NEWPARTS'     field new
     id 'OLDPARTS'     field old
     id 'RC'           field rc
     id 'MESSAGE'      field errmsg.
endform.

*---------------------------------------------------------------------*
*       FORM get_new_parts                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  parts                                                         *
*---------------------------------------------------------------------*
form get_new_parts changing parts.
  data: pc type i value 0.

  call 'C_PRINT_LINE'
     id 'HANDLE'       field spool_handles-handle
     id 'ADDPARTCOUNT' field pc
     id 'NEWPARTS'     field parts
     id 'RC'           field rc
     id 'MESSAGE'      field errmsg.
endform.

*---------------------------------------------------------------------*
*       FORM get_old_parts                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  parts                                                         *
*---------------------------------------------------------------------*
form get_old_parts changing parts.
  data: pc type i value 0.

  call 'C_PRINT_LINE'
     id 'HANDLE'       field spool_handles-handle
     id 'ADDPARTCOUNT' field pc
     id 'OLDPARTS'     field parts
     id 'RC'           field rc
     id 'MESSAGE'      field errmsg.
endform.

*---------------------------------------------------------------------*
*       FORM add_new_parts                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  parts                                                         *
*---------------------------------------------------------------------*
form add_new_parts using parts.
  call 'C_PRINT_LINE'
     id 'HANDLE'       field spool_handles-handle
     id 'ADDPARTCOUNT' field parts
     id 'RC'           field rc
     id 'MESSAGE'      field errmsg.
endform.

*---------------------------------------------------------------------*
*       FORM add_new_parts2                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  parts                                                         *
*  -->  g                                                             *
*---------------------------------------------------------------------*
form add_new_parts2 using parts changing g.
  call 'C_PRINT_LINE'
     id 'HANDLE'       field spool_handles-handle
     id 'ADDPARTCOUNT' field parts
     id 'NEWPARTS'     field g
     id 'RC'           field rc
     id 'MESSAGE'      field errmsg.
endform.

*---------------------------------------------------------------------*
*       FORM set_new_parts                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  parts                                                         *
*---------------------------------------------------------------------*
form set_new_parts using parts.
  call 'C_PRINT_LINE'
     id 'HANDLE'       field spool_handles-handle
     id 'PARTCOUNT'    field parts
     id 'RC'           field rc
     id 'MESSAGE'      field errmsg.
endform.

form get_page_info changing subpage totalpage oldpages.
    call 'RSPO_CACHE_CONTROL' id 'AREA'      field 'HANDLE'
                              id 'OP'        field 'GET'
                              id 'SUBPAGE'   field subpage
                              id 'TOTALPAGE' field totalpage
                              id 'OLDPAGES'  field oldpages.
endform.

form get_page_info_for_handle changing subpage totalpage oldpages.
    call 'RSPO_CACHE_CONTROL' id 'AREA'      field 'HANDLE'
                              id 'OP'        field 'GET'
                              id 'HANDLE'    field spool_handles-handle
                              id 'SUBPAGE'   field subpage
                              id 'TOTALPAGE' field totalpage
                              id 'OLDPAGES'  field oldpages.
endform.

form get_totalpage_info_for_handle changing totalpage.
    call 'RSPO_CACHE_CONTROL' id 'AREA'      field 'HANDLE'
                              id 'OP'        field 'GET'
                              id 'HANDLE'    field spool_handles-handle
                              id 'TOTALPAGE' field totalpage.
endform.

form get_totalpage_info changing totalpage.
    call 'RSPO_CACHE_CONTROL' id 'AREA'      field 'HANDLE'
                              id 'OP'        field 'GET'
                              id 'TOTALPAGE' field totalpage.
endform.
