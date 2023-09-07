*&---------------------------------------------------------------------*
*&  Include           ZHRNZPYIN_PCBURZ993
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------*
* CHANGE HISTORY
*----------------------------------------------------------------------------------------------*
*Mod | Date      | User ID|Description                       |Change Label  |Workbench Request *
*----------------------------------------------------------------------------------------------*
*001 |31-01-2023 | 1130848|Include for Custom operation Z_TER|              |CFAK902801
*                          called as part of PCC Test Payroll
*                          Cycle to keep Time Evaluation Upto
*                          Date until the end of the Period
*----------------------------------------------------------------------------------------------*
form fuz_ter.

  data: lv_subrc like sy-subrc.

  data: lv_program_var like vari-variant.
  data: lv_rec_date    type endda.
  data: lt_p0003  like p0003 occurs 0 with header line.

  ranges: lr_pernr for pernr-pernr.
  constants: c_no_time_evaluation type p0007-zterf value '0'.

* Payroll Log
  data: proto      like as-proto.
  data: lv_message type plog_text-text1.

  data: lt_message_handler type ref to cl_hrpa_message_list,
        lv_leaving_date    type endda,
        lv_is_ok           type boole_d.

  clear: $message_ptext, $ptext.
  refresh: $message_ptext, $ptext.

  if sw_prot ne space and as-proto eq space.
    move 'X' to proto.
  endif.

*  check tst_on eq abap_false.
* Run Time Evaluation until the last period End date
  loop at aper into data(ls_aper).
  endloop.

* Do not Execute Time Evaluation for No Time Evaluation Employees
  loop at p0007 into data(ls_p0007)
    where begda le ls_aper-endda
      and endda ge ls_aper-endda.
    exit.
  endloop.
  check ls_p0007-zterf ne c_no_time_evaluation.

* check infotype 0003 earliest pdc recalculation date
  refresh lt_p0003.
  call function 'HR_READ_INFOTYPE'
    exporting
      pernr           = pernr-pernr
      infty           = '0003'
      begda           = low-date
      endda           = high-date
      bypass_buffer   = abap_true
    tables
      infty_tab       = lt_p0003
    exceptions
      infty_not_found = 1
      others          = 2.

  read table lt_p0003 index 1.
* Employee PDC Re Calculation Date
  if proto eq abap_true.
    message i001(zhrpy_pcc_msg) with lt_p0003-bderr into lv_message.
    perform log_list_low in program h13plog0 tables $ptext using lv_message 1.
  endif.

* Read Employee Leaving Date
  call function 'HR_ECM_GET_LEAVING_DATE'
    exporting
      pernr           = pernr-pernr
      message_handler = lt_message_handler
    importing
      leaving_date    = lv_leaving_date
      is_ok           = lv_is_ok.
  if lv_leaving_date eq '00000000'.
    move high-date to lv_leaving_date.
  endif.
* Set the Comparison Date for PDC Recalculation Date
  if lv_leaving_date lt ls_aper-endda.
    lv_rec_date = lv_leaving_date + 1.
  else.
    lv_rec_date = ls_aper-endda + 1.
  endif.

  if lt_p0003-bderr lt lv_rec_date.
* Run Time evaluation
    refresh lr_pernr.
    lr_pernr-option = 'EQ'.  lr_pernr-sign = 'I'.
    lr_pernr-low = lr_pernr-high = pernr-pernr.
    append lr_pernr.

* Read Employee details for Payroll details processing
    loop at wpbp into data(ls_wpbp).
    endloop.

* Determine Time Evaluation Variant
    clear lv_program_var.
    perform determine_te_variant
      using ls_wpbp ls_aper changing lv_program_var.

* rptime00 expects pernr to be unlocked.
    perform ter_dequeue_employee using pernr-pernr lv_subrc.
    if lv_subrc ne 0.
* Can't unlock the employee trigger rerun
      perform update_event_handler using pernr-pernr.
    else.
* with variant
      if not lv_program_var is initial.
        submit rptime00 using selection-set lv_program_var
                        with  pnppernr in lr_pernr
                        with  enddate = ls_aper-endda
                        with logtomem = 'X'
                        and  return.
* without variant
      else.
        submit rptime00
                with pnppernr  in lr_pernr
                with schema   = 'YM04'
                with var_edt  = 'SAP&TEDT'
                with enddate  = ls_aper-endda
                with testopt1 = abap_false
                with testopt2 = abap_false
                with form_x   = abap_false
                with logtomem = 'X'
                and   return.
      endif.
* Employee Time Evaluation Variant
      if proto eq abap_true.
        message i002(zhrpy_pcc_msg) with lv_program_var into lv_message.
        perform log_list_low in program h13plog0 tables $ptext using lv_message 1.
      endif.

* lock the employee again for further processing
*    perform ter_enqueue_employee using pernr-pernr.
      perform enqueue_pernr_prel using pernr-pernr.

* check whether in infotype 0003 earliest pdc recalculation date is
* updated to rptime00 run date + 1. if not raise an error.
      refresh lt_p0003.
      call function 'HR_READ_INFOTYPE'
        exporting
          pernr           = pernr-pernr
          infty           = '0003'
          begda           = low-date
          endda           = high-date
          bypass_buffer   = abap_true
        tables
          infty_tab       = lt_p0003
        exceptions
          infty_not_found = 1
          others          = 2.

      read table lt_p0003 index 1.
* Employee new PDC Re Calculation Date
      if proto eq abap_true.
        message i003(zhrpy_pcc_msg) with lt_p0003-bderr into lv_message.
        perform log_list_low in program h13plog0 tables $ptext using lv_message 1.
      endif.

* set the comparison date for pdc recalculation date
      if  lv_leaving_date lt ls_aper-endda.
        lv_rec_date = lv_leaving_date + 1.
      else.
        lv_rec_date = ls_aper-endda + 1.
      endif.

      if lt_p0003-bderr ne lv_rec_date.
* Can't run time evaluation for the employee trigger rerun
        if lt_p0003-kobde eq abap_false.
          perform update_event_handler using pernr-pernr.
        endif.

* Time evaluation run was not successful or no employee selected in time
        perform write_pcc_messages
          using '011' lt_p0003-bderr ' ' '' ''.
      endif.

    endif.
  endif.
* Print Payroll Log
  if not ( $ptext[] is initial and $message_ptext[] is initial ).
    loop at $ptext.
      move-corresponding $ptext to ptext.
      append ptext.
    endloop.
    loop at $message_ptext.
      move-corresponding $message_ptext to message_ptext.
      append message_ptext.
    endloop.
    perform messages tables message_ptext.
  endif.

endform.
*---------------------------------------------------------------------*
*       FORM DETERMINE_GO_LIVE_DATE                                   *
*---------------------------------------------------------------------*
*       Read Go Live Date for NOT WT Functionality                    *
*---------------------------------------------------------------------*
form determine_te_variant using ps_wpbp type pc205
                                ps_aper type pc2aper
                       changing p_variant type vari-variant.
*
  constants: c_te_variant_feature type t549d-namen value 'ZPCCT',
             c_emp_tclas          type pspar-tclas value 'A'.

  data: ls_pme04 type pme04,
        lv_val   type string.
  data: lo_molga_finder type ref to cl_hrpa_molga,
        lv_molga        type molga.

  move-corresponding ps_wpbp to ls_pme04.
  ls_pme04-abkrs = ps_aper-abkrs.
  ls_pme04-tclas = c_emp_tclas.

* Country Grouping
* Prepare object to find Country Grouping (MOLGA) by Personnel Area (WPBP-WERKS)
  try.
      cl_hrpa_masterdata_factory=>get_read_molga(
        importing
          read_molga = lo_molga_finder ).
    catch cx_hrpa_violated_assertion .
      "if there is an error getting molga object, execute code to get molga
      "from table directly
  endtry.

  clear: lv_molga.
  try.
      if lo_molga_finder is bound.
        lv_molga = lo_molga_finder->read_molga_by_persa(
          exporting
            persa  = ps_wpbp-werks ).
      else.
        lv_molga = '43'.
      endif.
    catch cx_root.
      lv_molga = '43'.
  endtry.
  ls_pme04-molga = lv_molga.

  try.
      cl_hrpa_feature=>get_value(
        exporting
          feature       = c_te_variant_feature
          struc_content = ls_pme04
        importing
          return_value  = lv_val ).

      p_variant = lv_val.

    catch cx_root into data(lx_error).

  endtry.

endform.
*&---------------------------------------------------------------------*
*&      Form  TER_DEQUEUE_EMPLOYEE
*&---------------------------------------------------------------------*
*  Unlock the employee.
*----------------------------------------------------------------------*
*-->PERNR  personnel number
*----------------------------------------------------------------------*
form ter_dequeue_employee using p_pernr type pernr-pernr
                       changing e_subrc type sy-subrc.
*
  data: ls_return type bapireturn1.
  data: lv_number like  bapip0001-pernr.

  clear: e_subrc.
  lv_number = p_pernr.
  call function 'BAPI_EMPLOYEE_DEQUEUE'
    exporting
      number = lv_number
    importing
      return = ls_return.

  if ls_return-type eq 'E'.
    e_subrc = 8.
  endif.
*
endform.                    " TER_DEQUEUE_EMPLOYEE
*&---------------------------------------------------------------------*
*&      Form  TER_ENQUEUE_EMPLOYEE
*&---------------------------------------------------------------------*
*  Lock the employee.
*----------------------------------------------------------------------*
*-->PERNR  personnel number
*----------------------------------------------------------------------*
form ter_enqueue_employee  using pernr.
*
  data: lv_return type bapireturn1.
*
  do 50 times.
    clear: lv_return.
*
    call function 'BAPI_EMPLOYEE_ENQUEUE'
      exporting
        number = pernr
      importing
        return = lv_return.
*
    if lv_return is initial.
      exit.
    else.
      wait up to 1 seconds.
    endif.
  enddo.
*
  perform do_nothing(sapfp50p).
*
endform.                    " TER_ENQUEUE_EMPLOYEE
*&---------------------------------------------------------------------*
*&      Form  WRITE_PCC_MESSAGES
*&---------------------------------------------------------------------**----------------------------------------------------------------------*
*  -->  msg_num   message number
*  -->  param1    parameter 1 for message
*  -->  param2    parameter 2 for message
*----------------------------------------------------------------------*
form write_pcc_messages
  using value(msg_num)
        value(param1) value(param2) value(param3) value(param4).

  if not msg_num is initial.
    message id 'ZHRPY_PCC_MSG' type 'S' number msg_num
          into data(msg_text)
          with param1 param2 param3 param4.

    move: 'S'      to messages-msgar.
    concatenate 'ZHRPY_PCC_MSG' msg_num msg_text  into msg_text separated by '->'.

    move msg_text  to messages-mstxt.
    append messages.
  endif.

endform.                               " WRITE_PCC_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  UPDATE_EVENT_HANDLER_
*&---------------------------------------------------------------------*
*  Create an entru in Event Handler Table
*----------------------------------------------------------------------*
*-->PERNR  personnel number
*----------------------------------------------------------------------*
form update_event_handler using p_pernr type pernr-pernr.
*
  constants: c_par_type_pernr type pyd_par_type value 'PERNR'.
  data: lo_event_handler type ref to if_pyc_event_handler,
        lv_par_type      type pyd_par_type,
        lv_roid          type pyd_roid.

* Update Event Handler Table
  if not p_pernr is initial.
    try .
        lo_event_handler = cl_pyc_event_handler_factory=>get_event_handler_instance( ).
      catch cx_pyc_eh.
        return.
    endtry.

    lv_roid     = p_pernr.
    lv_par_type = c_par_type_pernr.

    try .
        call method lo_event_handler->event_handler_item_create_list
          exporting
            iv_par_type = lv_par_type
            iv_id       = lv_roid.
      catch cx_pyc_eh.
    endtry.
  endif.
*
endform.                    " UPDATE_EVENT_HANDLER_
