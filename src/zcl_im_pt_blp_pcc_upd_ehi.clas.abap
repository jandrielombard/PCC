class zcl_im_pt_blp_pcc_upd_ehi definition
  public
  final
  create public .

  public section.

    interfaces if_ex_pt_blp_user .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_IM_PT_BLP_PCC_UPD_EHI IMPLEMENTATION.


  method if_ex_pt_blp_user~process_data.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |14-10-2021 |1130848|Event Handler table logging for   |PTX-3763      |CFAK900981        *
*                         master data updates                                                 *
*---------------------------------------------------------------------------------------------*
    constants: c_partype_period type pyc_d_pypi-time_sel_par_type value 'PERIOD',
               c_par_type_pernr type pyd_par_type value 'PERNR',
               c_prstat_ok      type tim_tmwprstat value 'OK'.

    field-symbols <lv_pernr> type pernr_d.
    data: lv_rcfound type sysubrc value 8,
          lo_rec     type ref to if_pt_td_control,
          lv_empnr   type pernr_d,
          lt_pernr   type hrpay99_pernr_table.

    data: lo_event_handler type ref to if_pyc_event_handler,
          lv_par_type      type pyd_par_type,
          lv_roid          type pyd_roid.

* Collect Employee List
    loop at i_time_data into lo_rec.
      check lo_rec->prstat eq c_prstat_ok.
      lv_empnr = lo_rec->data->employee->pernr.
      insert lv_empnr into table lt_pernr.
    endloop.
    sort lt_pernr. delete adjacent duplicates from lt_pernr.

* Update Event Handler Table
    if not lt_pernr is initial.
      try .
          lo_event_handler = cl_pyc_event_handler_factory=>get_event_handler_instance( ).
        catch cx_pyc_eh.
          return.
      endtry.

      loop at lt_pernr into lv_empnr.
        lv_roid     = lv_empnr.
        lv_par_type = c_par_type_pernr.

        try .
            call method lo_event_handler->event_handler_item_create_list
              exporting
                iv_par_type = lv_par_type
                iv_id       = lv_roid.
          catch cx_pyc_eh.
        endtry.
      endloop.
    endif.

  endmethod.
ENDCLASS.
