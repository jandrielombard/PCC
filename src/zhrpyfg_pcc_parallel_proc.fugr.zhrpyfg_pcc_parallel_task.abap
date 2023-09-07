function zhrpyfg_pcc_parallel_task.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IO_CHECK_OBJNM) TYPE  SEOCLSNAME
*"     VALUE(IT_PERNR) TYPE  HR99S_PERNR_RANGE
*"     VALUE(IV_DELAY) TYPE  STRING OPTIONAL
*"     VALUE(IV_SERIALIZED_OBJECT) TYPE  ZHRPY_DE_PCC_OBJDATA
*"  EXPORTING
*"     VALUE(EV_SERIALIZED_OBJECT) TYPE  ZHRPY_DE_PCC_OBJDATA
*"     VALUE(EV_ERROR_MSG) TYPE  STRING
*"----------------------------------------------------------------------
  data: lo_check_object type ref to object,
        lv_method       type string value 'READ_CHECK_DATA'.
  data: lo_exception_error type ref to cx_st_error,
        lv_error_msg       type string.

* Deserialize the input object
  try.
      call transformation id_indent source xml iv_serialized_object result checkobject = lo_check_object.
    catch cx_st_error into lo_exception_error.
      ev_error_msg = lo_exception_error->get_text( ).
      exit.
  endtry.

  if iv_delay is not initial.
    wait up to iv_delay seconds.
  endif.

* Execute Result read
  "lo_check_object->read_check_data( it_pernr = it_pernr ).
  call method lo_check_object->(lv_method) exporting it_pernr = it_pernr .

* Serialize the object and return info
  clear: ev_serialized_object.
  try.
      call transformation id_indent source checkobject = lo_check_object result xml ev_serialized_object.
    catch cx_st_error into lo_exception_error.
      ev_error_msg = lo_exception_error->get_text( ).
  endtry.



endfunction.
