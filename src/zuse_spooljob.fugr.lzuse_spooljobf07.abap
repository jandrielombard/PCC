*----------------------------------------------------------------------*
*   Printer Cache                                                      *
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM add_printer                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  name                                                          *
*---------------------------------------------------------------------*
form add_printer using name.
  read table prts with key prt = name.
  if sy-subrc ne 0.
    perform _add_printer using name.
  endif.
endform.

form add_printer2 using name dest.
  if name is initial.
    if dest is initial.
      message e121(xm) raising device_missing.
    endif.
    perform add_printer using dest.
  else.
    if not dest is initial.
      message e123(xm) raising name_twice.
    endif.
    read table prts with key name = name.
    if sy-subrc ne 0.
      select single padest into dest from tsp03l where lname = name.
      if sy-subrc ne 0.
        message e122(xm) with name raising no_such_device.
      endif.
      perform _add_printer using dest.
    endif.
  endif.
endform.

form _add_printer using name.
    prts-prt = name.
    prts-name = name.
    clear: prts-use_abap, prts-driver.
    select single lname into prts-name from tsp03l where padest = name.
    select single patype into prts-type
                         from tsp03
                         where padest = name.
    if sy-subrc ne 0.
       message e122(xm) with name raising no_such_device.
    else.
      select single driver into prts-driver
                           from tsp0a where patype = prts-type.
      if sy-subrc eq 0.
        case prts-driver.
          when 'HPL2' or
               'POST' or
               'PRES' or
               'RDIF' or
               'STND' or
               'STN2' or
               'SWIN'.  prts-use_abap = ' '.
          when others.
            select single * from tsp09 where driver = prts-driver.
            if sy-subrc = 0.
              prts-use_abap = tsp09-dabap.
            endif.
        endcase.
      endif.
    endif.
    append prts.
endform.





















