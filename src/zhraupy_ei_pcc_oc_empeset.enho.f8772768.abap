"Name: \TY:CL_PYC_OFF_CYCLE_DPC_EXT\ME:EMPLOYEESET_GET_ENTITYSET\SE:BEGIN\EI
ENHANCEMENT 0 ZHRAUPY_EI_PCC_OC_EMPESET.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |01-12-2021 |1130848|change Employee Entityset read    |PTX-3763      |CFAK901652        *
*                          to PERNR read for nuemeric value                                                      *
*---------------------------------------------------------------------------------------------*
* WOW Specific read for Employee Entity Set
    data: lt_wow_filter        type /iwbep/t_mgw_select_option,
          ls_wow_filter        like line of lt_wow_filter,
          lv_wow_search_string type pyd_name.
    data: lv_pernr              type pernr-pernr,
          lv_wow_search_string2 type pyd_name.

    lt_wow_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    read table lt_wow_filter into ls_wow_filter with key property = gcs_attributes-search_string.
    if sy-subrc = 0.
      delete lt_wow_filter index sy-tabix.
      lv_wow_search_string = ls_wow_filter-select_options[ 1 ]-low.
    endif.

    lv_wow_search_string2 = lv_wow_search_string .
    if not lv_wow_search_string2 is INITIAL.
      replace all occurrences of '*' in lv_wow_search_string2 with ''.
      if lv_wow_search_string2 co '0123456789'.
        move lv_wow_search_string to lv_pernr.
        et_entityset = read_employee( exporting iv_employee_id = lv_pernr ).
      else.
        et_entityset = read_employee( exporting iv_search_string = lv_wow_search_string ).
      endif.
    else.
      et_entityset = read_employee( exporting iv_search_string = lv_wow_search_string ).
    endif.

    apply_odata_query_options( exporting io_tech_request_context = io_tech_request_context
                               changing  cs_response_context     = es_response_context
                                         ct_entityset            = et_entityset ).

    exit.

ENDENHANCEMENT.
