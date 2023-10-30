*-----------------------------------------------------------------------*
* Program Name  : ZUSEHRAUREPY_PCC_EXECUTE
* Title         : Execute a single PCC validation
* Create Date   : 16.09.2021
* Release       : ECC 6.0
*-----------------------------------------------------------------------*
* Description   : Submit PYC_EXECUTE_POLICIES for a single validation and
*                 particular employee numbers
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Date       | User ID      |Description                   |Change Label *
*-----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
report zusehraurepy_pcc_execute.

nodes: pernr.

constants: gc_memory_id type c length 11 value 'PCC_EXECUTE',
           gc_period    type pyd_par_type value 'PERIOD',
           gc_proc      type pyd_par_type value 'PYP_PROC'.

data: gt_pernr      type pernr_tab,
      gv_period     type paper,
      gt_instid     type table of pyd_d_inst,
      gs_instid     type pyd_d_inst,
      gv_validation type pyd_instid,
      gv_pypi_id    type pyc_proc_inst_id,
      gv_proc_id    type pyc_proc_id.

data: gs_validation type pyd_d_inst.

selection-screen begin of block r1 with frame title text-001.
parameters:
  p_proc   type pyd_classid matchcode object zusehrpy_pcc_classid_sh obligatory,
  p_valid  type pyd_type matchcode object zuse_pcc_validation_sh obligatory,
  p_procid like pyc_d_pypi-proc_id.
selection-screen end of block r1.

data gv_instance type pyd_d_inst.

select-options ro_inst for gv_instance no intervals no-display.

at selection-screen.
  if pnpabkrs-low is initial.
    message e000(pyc_conf_wb) with text-002.
  endif.

start-of-selection.

  clear gt_pernr.

* + 28/09/2021 commented out/replaced with direct select to improve performance
*GET pernr.
*
*  APPEND pernr-pernr TO gt_pernr.

  "populate the selected employees
  select a~pernr into table gt_pernr
    from pa0001 as a inner join pa0000 as b on a~pernr = b~pernr
    where a~pernr in pnppernr and
          a~abkrs in pnpabkrs and
          a~persg in pnppersg and
          a~persk in pnppersk and
          a~btrtl in pnpbtrtl and
          a~werks in pnpwerks and
          a~bukrs in pnpbukrs and
          a~kostl in pnpkostl and
          a~begda <= pn-endda and
          a~endda >= pn-begda and
          b~stat2 in pnpstat2 and
          b~begda <= pn-endda and
          b~endda >= pn-begda.
* - 28/09/2021 commented out/replaced with direct select to improve performance

end-of-selection.

  concatenate pn-pabrj pn-pabrp into gv_period.

  "get process instance
  if p_procid is not initial.
  gv_proc_id = p_procid.
    select single pypi_id   into   gv_pypi_id
      from pyc_d_pypi
  where  time_sel_par_type = gc_period and
            time_sel_par_val = gv_period and
            original_pypte_id = p_proc and

            proc_id = gv_proc_id.
  else.
    select single pypi_id proc_id into ( gv_pypi_id, gv_proc_id )
      from pyc_d_pypi as a inner join pyc_d_pypisp as b
        on a~proc_id = b~id
     where a~time_sel_par_type = gc_period and
           a~time_sel_par_val = gv_period and
           a~original_pypte_id = p_proc and
           b~value = pnpxabkr.
  endif.

  check sy-subrc = 0.

  "get latest validation instance
  select a~* into table @gt_instid
    from pyd_d_inst as a inner join pyd_d_instp as b
                         on a~id = b~id
    where a~type = @p_valid and
          b~par_type = @gc_proc and
          b~low = @gv_proc_id.

  check sy-subrc = 0.

  sort gt_instid by id descending.

  read table gt_instid into gs_instid index 1.

  insert value #( sign = 'I' option = 'EQ' low = gs_instid-id ) into table ro_inst.

  "populate set of PERNRs into memory for later use in submit
  export gt_pernr = gt_pernr to memory id gc_memory_id.

  "start the validation
  submit pyc_execute_policies with p_proc_i = gv_pypi_id
                              with so_inst in ro_inst
                              with p_testpy  = abap_true.
