class ZUSECL_PYC_STT_ASYNC_MAN_BASE definition
  public
  inheriting from CL_PYC_STT_ASYNC_MAN_BASE
  final
  create public .

public section.
protected section.

  methods FP3_EXE_DET_INFO_GET
    redefinition .
private section.

  data MT_PAR type IF_PYD_FND_TYPES=>TY_T_RESP .
  constants GC_TEXT_PARAM type PYD_PAR_TYPE value 'Z99_MAN_TEXT' ##NO_TEXT.
  constants GC_PROCESS type PYD_PAR_TYPE value 'PYP_PROC_TEMPLATE' ##NO_TEXT.
ENDCLASS.



CLASS ZUSECL_PYC_STT_ASYNC_MAN_BASE IMPLEMENTATION.


  METHOD FP3_EXE_DET_INFO_GET.
* redefined to allow long text in process step

    DATA: lt_text    TYPE TABLE OF zuse_pcc_text,
          ls_text    TYPE zuse_pcc_text,
          lv_process TYPE pyd_classid.
break-point 'JLOMBARD'.
    CLEAR rs_redef-text.

    "get process template name
    READ TABLE io_res_context->mt_par INTO DATA(ls_par)
     WITH KEY par_type = gc_process.
    IF sy-subrc = 0.
      lv_process = ls_par-low.
    ENDIF.

    "retrieve all the text lines for the current process / step:
    SELECT * INTO TABLE lt_text
      FROM zuse_pcc_text
      WHERE process_template = lv_process AND
            step_template = if_pyd_ty_rt~mv_type.

    IF sy-subrc NE 0.
      "call superclass
      TRY.
          CALL METHOD super->fp3_exe_det_info_get
            EXPORTING
              io_res_context = io_res_context
            RECEIVING
              rs_redef       = rs_redef.
        CATCH cx_pyc_frw.
      ENDTRY.
    ELSE.
      "populate with custom text
      SORT lt_text BY seqnr ASCENDING.

      LOOP AT lt_text INTO ls_text.
        CONCATENATE rs_redef-text ls_text-textline cl_abap_char_utilities=>cr_lf INTO rs_redef-text.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
