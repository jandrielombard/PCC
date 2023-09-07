class lcl_zhraupy_ei_pcc_oc_pre_dme definition deferred.
class cl_pyc_stt_oc_create_pre_dme definition local friends lcl_zhraupy_ei_pcc_oc_pre_dme.
class lcl_zhraupy_ei_pcc_oc_pre_dme definition.
  public section.
    class-data obj type ref to lcl_zhraupy_ei_pcc_oc_pre_dme. "#EC NEEDED
    data core_object type ref to cl_pyc_stt_oc_create_pre_dme . "#EC NEEDED
 INTERFACES  IOW_ZHRAUPY_EI_PCC_OC_PRE_DME.
    methods:
      constructor importing core_object
                              type ref to cl_pyc_stt_oc_create_pre_dme optional.
endclass.
class lcl_zhraupy_ei_pcc_oc_pre_dme implementation.
  method constructor.
    me->core_object = core_object.
  endmethod.

  method iow_zhraupy_ei_pcc_oc_pre_dme~handle_container_notify.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods HANDLE_CONTAINER_NOTIFY
*"  importing
*"    !IT_CONTAINER type SWCONTTAB
*"    !IO_RES_CONTEXT type ref to IF_PYD_RES_CONTEXT
*"  raising
*"    CX_PYC_CONT .
*"------------------------------------------------------------------------*
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |20-12-2021 |1130848|Performance Improvements          |PTX-3763      |CFAK901328        *
*---------------------------------------------------------------------------------------------*
    data:
      lo_container_access type ref to if_pyc_pi_container_access,
*>>> Start of WOW Specific Enhancement
*      lt_gp_value         type ty_t_gp_value,
*      lt_gp_value_create  type ty_t_gp_value,
      lt_gp_value         type cl_pyc_stt_oc_create_pre_dme=>ty_t_gp_value,
      lt_gp_value_create  type cl_pyc_stt_oc_create_pre_dme=>ty_t_gp_value,
*>>> End of WOW Specific Enhancement
      ls_container        type swcont,
      lv_roid             type pyd_roid,
      lv_gp_value         type cl_pyc_stt_base=>ty_gp_value,
      lv_gp_value_del     type cl_pyc_stt_base=>ty_gp_value,
      lv_laufd            type laufd,
      lv_laufi            type laufi,
      lx_exc              type ref to cx_root,
      lt_reguh            type table of reguh_bf,
      lt_regup            type table of regup_bf,
      lt_selection        type if_pyc_selection_oc_access=>ty_t_selection,
      ls_reguh            type reguh,
      lv_proc_inst_id     type pyc_proc_inst_id.

*>>> Start of WOW Specific Enhancement
    data: lo_pi_cont_access_factory type ref to cl_pyc_pi_cont_access_factory.
    data: lt_rgdir type hrpy_tt_rgdir,
          ls_rgdir type pc261,
          lv_molga type molga.
    data: rt_laufd type fkk_laufd_range_tab.

* To improve performance set date range for REGUH Selection
* Low date = Current Day - 1 and High Date = Current Date
    insert value #(
              sign   = 'I'
              option = 'BT'
              low    = sy-datum - 1
              high   = sy-datum )
            into table rt_laufd.
*<<< End of WOW Specific Enhancement

    try.
*        lo_container_access = me->pi_container_access_get( io_res_context = io_res_context ).
        lo_pi_cont_access_factory = cl_pyc_pi_cont_access_factory=>get_instance( ).
        lo_container_access = lo_pi_cont_access_factory->get_by_step_inst( io_res_context ).

        clear: lv_laufd, lv_laufi.
        loop at it_container into ls_container.
          case ls_container-element.
            when 'LAUFD'.
              lv_laufd = ls_container-value.
            when 'LAUFI'.
              lv_laufi = ls_container-value.
            when others.
          endcase.
        endloop.

        check lv_laufd is not initial and lv_laufi is not initial.
        concatenate lv_laufd lv_laufi into lv_roid separated by '/'.
        check lv_roid is not initial.
        lv_gp_value = lv_roid.

        call method lo_container_access->list_get
          exporting
*>>> Start of WOW Specific Enhancement
*           iv_gp_list_type = gc_gp_type " 'OCBTR'
            iv_gp_list_type = cl_pyc_stt_oc_create_pre_dme=>gc_gp_type " 'OCBTR'
*<<< End of WOW Specific Enhancement
          receiving
            rt_gp_value     = lt_gp_value.

        read table lt_gp_value with table key table_line = lv_gp_value transporting no fields.
        if sy-subrc <> 0.
*          INSERT lv_gp_value INTO TABLE lt_gp_value.
*        ENDIF.
          "check the validaty of current runstamp.
          split lv_gp_value at '/' into lv_laufd lv_laufi.

          call function 'HRCA_PAYMENT_DATA_GET'
            exporting
              run_date       = lv_laufd
              run_id         = lv_laufi
            tables
              reguh_data     = lt_reguh
              regup_data     = lt_regup
            exceptions
              not_found      = 1
              invalid_status = 2
              others         = 3.
          if sy-subrc <> 0 or ( lt_reguh is initial and lt_regup is initial ).
            return.
          endif.

          "find the package for current value and remove the existing ones for the same package.
          lv_proc_inst_id = cl_pyd_fnd_aux=>get_resp_fixed_value( iv_par_type = cl_pyc_rt_facade=>gcs_par_type-proc_inst
                                                                  it_par      = io_res_context->mt_par ).
          lt_selection = cl_pyc_selection_oc_access=>get_instance( )->selection_list_get( iv_proc_inst_id = lv_proc_inst_id ).
          loop at lt_selection into data(ls_selection).
            at new package_id.
              read table lt_selection into data(ls_sel_single) index sy-tabix.

*>>> Start of WOW Specific Enhancement
*              SELECT r~* FROM reguh AS r
*               INNER JOIN hrpy_rgdir AS h ON r~pernr = h~pernr AND r~seqnr = h~seqnr
*                INTO @ls_reguh
*               WHERE h~pernr = @ls_sel_single-pernr AND h~payty = @ls_sel_single-payty AND h~payid = @ls_sel_single-payid AND h~ocrsn = @ls_sel_single-ocrsn
*                 AND ( h~bondt = @ls_sel_single-bondt OR h~fpper = @ls_sel_single-fpper )
*               ORDER BY r~laufd DESCENDING, r~laufi DESCENDING.

* New logic to overcome performance issue
              " Read RGDIR data for the Employee
              call function 'CA_CU_READ_RGDIR_NEW'
                exporting
                  persnr          = ls_sel_single-pernr
                importing
                  molga           = lv_molga
                tables
                  cu_ca_rgdir     = lt_rgdir
                exceptions
                  no_record_found = 1
                  others          = 2.

              " Read Pay Sequence Number
              loop at lt_rgdir into ls_rgdir
                where payty = ls_sel_single-payty
                  and payid = ls_sel_single-payid
                  and ocrsn = ls_sel_single-ocrsn
                  and ( bondt = ls_sel_single-bondt or fpper = ls_sel_single-fpper ).
                exit.
              endloop.
              if not ls_rgdir is initial.
                " Read REGUH Record for the Pay
                select r~* from reguh as r into @ls_reguh
                  where r~laufd in @rt_laufd
                    and r~pernr = @ls_sel_single-pernr
                    and r~seqnr = @ls_rgdir-seqnr
                  order by r~laufd descending, r~laufi descending.
*<<< End of WOW Specific Enhancement

                  if ls_reguh is not initial.
                    if ls_reguh-laufd = lv_laufd and ls_reguh-laufi = lv_laufi.
                      "current runstamp. keep and store package info.
                      data(ls_sel_cur) = ls_sel_single.
                    elseif ls_sel_cur is not initial.
                      "same package with current runstamp obsolete. remove from lt_gp_value.
                      concatenate ls_reguh-laufd '/' ls_reguh-laufi into lv_gp_value_del.
                      delete lt_gp_value where table_line = lv_gp_value_del.
                    else.
                      exit.
                    endif.
                  endif.
                endselect.
              endif.
            endat.
          endloop.

          append lv_gp_value to lt_gp_value.
        endif.

*        LOOP AT lt_gp_value INTO lv_gp_value.
*          "resovle run date and run id.
*          SPLIT lv_gp_value AT '/' INTO lv_laufd lv_laufi.
*
*          CALL FUNCTION 'HRCA_PAYMENT_DATA_GET'
*            EXPORTING
*              run_date      = lv_laufd
*              run_id        = lv_laufi
*            TABLES
*              reguh_data    = lt_reguh
*              regup_data    = lt_regup
*            EXCEPTIONS
*              not_found         = 1
*              invalid_status    = 2
*              OTHERS            = 3.
*          IF sy-subrc <> 0 OR ( lt_reguh IS INITIAL AND lt_regup IS INITIAL ).
*            DELETE lt_gp_value.
*          ENDIF.
*        ENDLOOP.

        call method lo_container_access->list_set
          exporting
*>>> Start of WOW Specific Enhancement
*           iv_gp_list_type = gc_gp_type " 'OCBTR'
            iv_gp_list_type = cl_pyc_stt_oc_create_pre_dme=>gc_gp_type " 'OCBTR'
*<<< End of WOW Specific Enhancement
            it_gp_value     = lt_gp_value.
      catch cx_root into lx_exc.
        raise exception type cx_pyc_cont exporting previous = lx_exc.
    endtry.

  endmethod.
ENDCLASS.
