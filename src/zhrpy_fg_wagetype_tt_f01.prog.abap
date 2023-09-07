*&---------------------------------------------------------------------*
*&  Include           ZZHR_FG_WAGETYPE_TT_XXX
*&---------------------------------------------------------------------*
module zz_init output.                                      "#EC CALLED
  zhrau_t_wagetype-molga = '13'.
endmodule.

module zz_pai.

  if zhrau_t_wagetype-zzchar = 'E'.
    clear: zhrau_t_wagetype-prcls, zhrau_t_wagetype-prclv .
    if zhrau_t_wagetype-evcls is initial or zhrau_t_wagetype-evclv is initial.
      message e999(zh) with 'Please choose evaluation class and proficiency.'.
    endif.
  else.
    clear: zhrau_t_wagetype-evcls, zhrau_t_wagetype-evclv .
    if zhrau_t_wagetype-prcls is initial or zhrau_t_wagetype-prclv is initial.
      message e999(zh) with 'Please choose processing class and specification.'.
    endif.
  endif.

  if zhrau_t_wagetype-ztext co ' '.
    message e999(zh) with 'Please fill description.'.
  endif.
endmodule.
