METHOD GET_DATA_SOURCE_TYPE .
  read table mt_pyd_inst into data(ls_pyd_inst) with table key id = iv_pyd_id.
  if sy-subrc = 0.
    rv_pyd_type = ls_pyd_inst-type.
  else.
    select single type into ls_pyd_inst-type
      from pyd_d_inst
      where id = iv_pyd_id.
    if sy-subrc = 0.
      ls_pyd_inst-id = iv_pyd_id.
      insert ls_pyd_inst into table mt_pyd_inst.
      rv_pyd_type = ls_pyd_inst-type.
    endif.
  endif.
ENDMETHOD.
