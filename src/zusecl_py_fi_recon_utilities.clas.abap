class ZUSECL_PY_FI_RECON_UTILITIES definition
  public
  final
  create public .

public section.

  constants GC_BASE_DIRECTORY type STRING value '/sapmnt' ##NO_TEXT.
  constants GC_CUSTOMER_DATA type STRING value 'customerdata' ##NO_TEXT.
  constants GC_INTERFACE type STRING value 'interface' ##NO_TEXT.
  constants GC_FI_RECON_DIRECTORY type STRING value 'FIRE' ##NO_TEXT.
  constants GC_FI_RECON_DIRECTORY_NZ type STRING value 'FRNZ' ##NO_TEXT.
  constants GC_ASDIR_SEPARATOR type STRING value '/' ##NO_TEXT.
  constants:
    gc_fi_recon(12)       type c value 'POSTING_DATA' ##NO_TEXT.
  constants:
    gc_pp(2)              type c value 'PP' ##NO_TEXT.
  constants:
    gc_oc(2)              type c value 'OC' ##NO_TEXT.
  constants GC_FI_RECON_MSGCLS type SY-MSGID value 'ZX_PY_FI_RECON_MSG' ##NO_TEXT.
  constants GC_MSGTY_ERROR type SY-MSGTY value 'E' ##NO_TEXT.
  constants GC_MSGTY_INFO type SY-MSGTY value 'I' ##NO_TEXT.
  constants GC_MOLGA_AU type MOLGA value '13' ##NO_TEXT.
  constants GC_MOLGA_NZ type MOLGA value '43' ##NO_TEXT.

  class-methods GET_FI_RECON_ASDIR
    importing
      !IV_MOLGA type MOLGA
    exporting
      !EV_ASDIR_NAME type EPSF-EPSDIRNAM
      !EV_FILE_MASK type EPSF-EPSFILNAM .
  class-methods CREATE_XLSX_FROM_ITAB
    importing
      !IT_FIELDCAT type LVC_T_FCAT optional
      !IT_SORT type LVC_T_SORT optional
      !IT_FILT type LVC_T_FILT optional
      !IS_LAYOUT type LVC_S_LAYO optional
      !IT_HYPERLINKS type LVC_T_HYPE optional
      value(IT_DATA) type STANDARD TABLE
    returning
      value(R_XSTRING) type XSTRING .
  class-methods DELETE_ASDIR_FILE
    importing
      !IV_FILE_NAME type STRING optional
    exceptions
      NO_AUTHORIZATION
      FILE_OPEN_ERROR .
  class-methods F4_LOCALFILE_DIRECTORY
    changing
      !CV_LOCALDIR type STRING optional .
  class-methods RAISE_LONGTEXT_MSG
    importing
      !IV_MSGID type SY-MSGID
      !IV_MSGTY type SY-MSGTY
      !IV_MSGNO type SY-MSGNO
      !IV_TEXT type STRING
    returning
      value(RV_MSG) type STRING .
  protected section.
  private section.
ENDCLASS.



CLASS ZUSECL_PY_FI_RECON_UTILITIES IMPLEMENTATION.


  method create_xlsx_from_itab.

    data(lt_data) = ref #( it_data ).

    if it_fieldcat is initial.
      field-symbols: <tab> type standard table.
      assign lt_data->* to <tab>.
      try.
          cl_salv_table=>factory(
          exporting
            list_display = abap_false
          importing
            r_salv_table = data(salv_table)
          changing
            t_table      = <tab> ).

          data(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                   r_columns      = salv_table->get_columns( )
                                   r_aggregations = salv_table->get_aggregations( ) ).
        catch cx_salv_msg.
          return.
      endtry.

    else.
      lt_fcat = it_fieldcat.
    endif.

    cl_salv_bs_lex=>export_from_result_data_table(
      exporting
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table =  cl_salv_ex_util=>factory_result_data_table(
                                                r_data                      = lt_data
                                                s_layout                    = is_layout
                                                t_fieldcatalog              = lt_fcat
                                                t_sort                      = it_sort
                                                t_filter                    = it_filt
                                                t_hyperlinks                = it_hyperlinks )
      importing
        er_result_file       = r_xstring ).

  endmethod.


  method delete_asdir_file.
* Delete File from Application Server Directory
    data: lo_cx_root type ref to cx_root.

* delete file
    try.
        delete dataset iv_file_name.
      catch cx_sy_file_authority into lo_cx_root.
        " No authorization for access to file
        raise no_authorization.
      catch cx_sy_file_open into lo_cx_root .
        " File cannot be opened
        raise file_open_error.
    endtry.


  endmethod.


  method f4_localfile_directory.
* Local Directory Selection
    data: lv_path         type string,
          lv_full_path    type string,
          lv_window_title type string.

    lv_window_title = text-001.
    call method cl_gui_frontend_services=>directory_browse
      exporting
        window_title         = lv_window_title
        initial_folder       = lv_path
      changing
        selected_folder      = lv_full_path
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        others               = 4.
    if sy-subrc <> 0.
      message i008(zx_py_fi_recon_msg).
    endif.

    call method cl_gui_cfw=>flush.
    concatenate lv_full_path '\' into cv_localdir.

  endmethod.


  method get_fi_recon_asdir.
* Build Directory Name
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |19-07-2022 |1130848|Enhanced Posting Run ID Selection |E2P-2576      |CFAK903587        *
*    |           |       |NZ FI Recon Implementation        |              |                  *
*---------------------------------------------------------------------------------------------*
*>>> Start of MOD001++
    data: lv_fi_recon_directory type string.
* FI Recon Sub Folder
    case iv_molga.
      when gc_molga_au.
        lv_fi_recon_directory = zusecl_py_fi_recon_utilities=>gc_fi_recon_directory.
      when gc_molga_nz.
        lv_fi_recon_directory = zusecl_py_fi_recon_utilities=>gc_fi_recon_directory_nz.
      when others.
        lv_fi_recon_directory = zusecl_py_fi_recon_utilities=>gc_fi_recon_directory.
    endcase.
*<<< End of MOD001++
* Directory Name
    concatenate zusecl_py_fi_recon_utilities=>gc_base_directory
                sy-sysid
                zusecl_py_fi_recon_utilities=>gc_customer_data
                zusecl_py_fi_recon_utilities=>gc_interface
*                zusecl_py_fi_recon_utilities=>gc_fi_recon_directory  "MOD001--
                lv_fi_recon_directory                              "MOD001++
    into ev_asdir_name separated by zusecl_py_fi_recon_utilities=>gc_asdir_separator.

    concatenate ev_asdir_name  zusecl_py_fi_recon_utilities=>gc_asdir_separator into ev_asdir_name.
* File Mask
    concatenate '*' zusecl_py_fi_recon_utilities=>gc_fi_recon '*' into ev_file_mask.

  endmethod.


  method raise_longtext_msg.
* Split Message
    data: lv_msgv1 type  syst_msgv,
          lv_msgv2 type  syst_msgv,
          lv_msgv3 type  syst_msgv,
          lv_msgv4 type  syst_msgv.

    constants: lc_max_length type i value 50.
    data: lt_components type standard table of swastrtab.
    data: begin of ls_msgvn,
            filler(7) type c value 'LV_MSGV',
            num       type c.
    data: end of ls_msgvn.
    field-symbols: <msgvn> type syst_msgv.

    if strlen( iv_text ) > lc_max_length.
      call function 'SWA_STRING_SPLIT'
        exporting
          input_string                 = iv_text
          max_component_length         = lc_max_length
        tables
          string_components            = lt_components
        exceptions
          max_component_length_invalid = 1
          others                       = 2.

      if sy-subrc eq 0.
        loop at lt_components into data(ls_components).
          if sy-tabix le 4.
            move sy-tabix to ls_msgvn-num.
          else.
            exit.
          endif.
          assign (ls_msgvn) to <msgvn>.
          <msgvn>  = ls_components-str.
        endloop.
      else.
        lv_msgv1 = iv_text.
      endif.
    else.
      lv_msgv1 = iv_text.
    endif.

* Build the message
    message id iv_msgid type iv_msgty number iv_msgno
     with lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 into rv_msg.

  endmethod.
ENDCLASS.
