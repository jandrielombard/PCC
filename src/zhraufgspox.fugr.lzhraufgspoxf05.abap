*----------------------------------------------------------------------*
*   Attribute                                                          *
*----------------------------------------------------------------------*

form add_attributes tables attributes structure rspoattr
                                                        using add ident.
  tsp02a-pjident = ident.
  tsp02a-pjnumber = 0.
  tsp02a-line = 0.
  clear: tsp02a-param.
  sort attributes stable by param.
  if add is initial.
    delete from tsp02a where pjident  = tsp02a-pjident and
                             pjnumber = tsp02a-pjnumber.
  endif.
  loop at attributes.
    if not attributes-param is initial.
      if tsp02a-param <> attributes-param.
        tsp02a-line = 0.
        tsp02a-param = attributes-param.
        if not add is initial.
          delete from tsp02a where pjident  = tsp02a-pjident and
                                   pjnumber = tsp02a-pjnumber and
                                   param    =  tsp02a-param.
        endif.
      endif.
      add 1 to tsp02a-line.
      tsp02a-value = attributes-value.
      insert tsp02a.
      if sy-subrc ne 0.
        message e418 raising incomplete.
      endif.
    endif.
  endloop.
endform.

* POSS options
form determine_printoptions using opttab type poss_jobtickettab
                                  use_options type c
                                  options type x
                                  usrstring09 type rspoposs_optstring
                                  usrstring16 type rspoposs_optstring
                                  usrstring17 type rspoposs_optstring
                                  usrstring18 type rspoposs_optstring.
data max_opt type i.
data numopt type i.
data optid type rspoposs_optid.
data ofs type i.
data wa_opt type poss_jobticket.

clear: use_options, usrstring09, usrstring16, usrstring17, usrstring18.
perform kernel_check_for_poss(SAPLRSPOPOSS) using max_opt space.
if sy-subrc <> 0.
  exit. "kernel does not support print options
endif.
describe field options length numopt in byte mode.
clear options.
do numopt times.
  optid = sy-index.
  read table opttab into wa_opt with key poption = optid.
  if sy-subrc = 0.
    if wa_opt-optvalue > 0.
      ofs = optid - 1.
      options+ofs(1) = wa_opt-optvalue.
      use_options = 'X'.
      case optid.
        when 9.  usrstring09 = wa_opt-optstring.
        when 16. usrstring16 = wa_opt-optstring.
        when 17. usrstring17 = wa_opt-optstring.
        when 18. usrstring18 = wa_opt-optstring.
      endcase.
    endif.
  endif.
enddo.
endform.
