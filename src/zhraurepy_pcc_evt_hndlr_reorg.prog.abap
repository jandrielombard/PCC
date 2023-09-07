*-----------------------------------------------------------------------*
* Program Name  : ZHRAUREPY_PCC_EVT_HNDLR_REORG
* Title         : Delete PCC Event Handler Items (table PYC_D_EHI)
*                 with status Not Relevant
* Create Date   : 08.08.2022
* Release       : ECC 6.0
* Author        : 1130843
*-----------------------------------------------------------------------*
* Description   : Delete records with processing status Not Relevant
*                 (PYC_D_EHI-PROC_STATUS = '03')
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Date       | User ID      |Description                   |Change Label *
*-----------------------------------------------------------------------*
*08-Aug-2022| 1130843      |Initial creation CFAK902121.  |             *
*           |              |This is a copy of program     |             *
*           |              |PYC_EVENT_HANDLER_REORG       |             *
*           |              |and only code to delete table |             *
*           |              |PYC_D_EHI with PROC_STATUS 03 |             *
*           |              |are retained. If the standard |             *
*           |              |program can delete all 03     |             *
*           |              |without deleting non-03 record|             *
*           |              |this program is not needed    |             *
*           |              |anymore                       |             *
*-----------------------------------------------------------------------*
REPORT zhraurepy_pcc_evt_hndlr_reorg.

TABLES:
  pyc_d_ehi.

SELECT-OPTIONS:
  s_pstat FOR pyc_d_ehi-proc_status.
PARAMETERS:
  p_uc_d(3) TYPE n DEFAULT '001',
  p_sim     TYPE abap_bool AS CHECKBOX DEFAULT abap_false..

TYPE-POOLS:
  icon.

TYPES:
  BEGIN OF ty_s_log,
    id          TYPE i,
    error       TYPE icon_l4,
    uname       TYPE syuname,
    pi_id       TYPE pyc_proc_inst_id,
    pernr       TYPE pernr_d,
    detail(120) TYPE c,
  END OF ty_s_log,

  ty_t_log TYPE STANDARD TABLE OF ty_s_log WITH NON-UNIQUE KEY id.

CONSTANTS:
  gc_processing_status_not_rlvnt TYPE pyc_ehi_proc_status VALUE '03'.

DATA:
  gx_fnd_exc TYPE REF TO cx_pyd_fnd,
  gt_log     TYPE ty_t_log WITH HEADER LINE,
  gv_logid   TYPE i,
  gv_string  TYPE string.

START-OF-SELECTION.

  TRY.
      IF cl_pyd_switch_check=>pyd_sfws_sc_07( ) = abap_false.
        "even raise in batch to cancel job and to get message into job log
        MESSAGE e111(pyd_fnd) INTO gv_string.
        PERFORM log_add USING 'A' space space space gv_string.
      ENDIF.
    CATCH cx_pyd_fnd INTO gx_fnd_exc.
      gv_string  = gx_fnd_exc->get_text( ).
      PERFORM log_add USING 'A' space space space gv_string.
      EXIT.
  ENDTRY.

  PERFORM delete_records.


END-OF-SELECTION.
  IF gt_log IS INITIAL.
    PERFORM log_add USING 'I' sy-uname '' '' 'No entry to reorg'(009).
  ENDIF.
  PERFORM display_log.

FORM delete_records.
  TYPES: BEGIN OF lty_pyc_d_ehi,
           mandt    TYPE pyc_d_ehi-mandt,
           pypi_id  TYPE pyc_d_ehi-pypi_id,
           tsl      TYPE pyc_d_ehi-tsl,
           par_type TYPE pyc_d_ehi-par_type,
           id       TYPE pyc_d_ehi-id,
         END OF lty_pyc_d_ehi.
  DATA:
    lv_uc_ts         TYPE timestamp,
    lv_uc_del        TYPE timestamp,
    lt_pyc_d_ehi     TYPE TABLE OF lty_pyc_d_ehi,
    lt_pyc_d_ehi_del TYPE TABLE OF lty_pyc_d_ehi,
    ls_pyc_d_ehi     TYPE lty_pyc_d_ehi.

  TRY.

      GET TIME STAMP FIELD lv_uc_ts.

      lv_uc_del = cl_abap_tstmp=>subtractsecs( tstmp = lv_uc_ts secs = p_uc_d * 24 * 60 * 60 ).

      SELECT mandt
        pypi_id
        tsl
        par_type
        id
        FROM pyc_d_ehi INTO TABLE lt_pyc_d_ehi
        WHERE proc_status IN s_pstat
          AND lc_dt <= lv_uc_del.

      IF lt_pyc_d_ehi IS INITIAL.
        PERFORM log_add USING 'I' sy-uname '' '' 'No entry to reorg'(009).
        PERFORM display_log.
        RETURN.
      ENDIF.

      LOOP AT lt_pyc_d_ehi INTO ls_pyc_d_ehi.
        APPEND ls_pyc_d_ehi TO lt_pyc_d_ehi_del.
        gv_string  = text-007.
        PERFORM log_add USING 'S' sy-uname ls_pyc_d_ehi-pypi_id ls_pyc_d_ehi-id gv_string.
        CLEAR ls_pyc_d_ehi.
      ENDLOOP.

      DELETE pyc_d_ehi FROM TABLE @lt_pyc_d_ehi_del.

      IF p_sim = abap_true.
        ROLLBACK WORK.
      ENDIF.

    CATCH cx_pyd_fnd.
  ENDTRY.
ENDFORM.

FORM log_add USING iv_error  TYPE c
                 iv_uname  TYPE sy-uname
                 iv_pi_id  " TYPE pyc_proc_inst_id
                 iv_pernr  " TYPE pernr_d
                 iv_detail TYPE string.

  ADD 1 TO gv_logid.
  gt_log-id = gv_logid.

  CASE iv_error.
    WHEN 'A'.
      gt_log-error = icon_failure.
    WHEN 'E'.
      gt_log-error = icon_led_red.
    WHEN 'I'.
      gt_log-error = icon_led_green.
    WHEN 'S'.
      gt_log-error = icon_led_green.
    WHEN 'W'.
      gt_log-error = icon_warning.
    WHEN OTHERS.
      gt_log-error = icon_led_green.
  ENDCASE.

  gt_log-uname = iv_uname.
  gt_log-pi_id = iv_pi_id.
  gt_log-pernr  = iv_pernr.
  gt_log-detail = iv_detail.

  APPEND gt_log.

ENDFORM.

FORM display_log.

  DATA:
    lt_fc     TYPE slis_t_fieldcat_alv WITH HEADER LINE,
    ls_layout TYPE slis_layout_alv,
    ls_print  TYPE slis_print_alv.

  ls_layout-colwidth_optimize = abap_true.

  lt_fc-col_pos   = 1.
  lt_fc-fieldname = 'ID'.
  lt_fc-tech      = abap_true.
  APPEND lt_fc.
  CLEAR lt_fc.

  lt_fc-col_pos   = 2.
  lt_fc-fieldname = 'ERROR'.
  lt_fc-icon      = abap_true.
  lt_fc-outputlen = 2.
  lt_fc-ddictxt   = 'M'.
  lt_fc-seltext_m = text-003.
  APPEND lt_fc.
  CLEAR lt_fc.

  lt_fc-col_pos   = 3.
  lt_fc-fieldname = 'UNAME'.
  lt_fc-outputlen = 12.
  lt_fc-ddictxt   = 'M'.
  lt_fc-seltext_m = text-004.
  APPEND lt_fc.
  CLEAR lt_fc.

  lt_fc-col_pos   = 4.
  lt_fc-fieldname = 'PI_ID '.
  lt_fc-outputlen = 120.
  lt_fc-ddictxt   = 'M'.
  lt_fc-seltext_m = text-005.
  APPEND lt_fc.
  CLEAR lt_fc.

  lt_fc-col_pos   = 5.
  lt_fc-fieldname = 'PERNR'.
  lt_fc-outputlen = 8.
  lt_fc-ddictxt   = 'M'.
  lt_fc-lzero     = 'X'.
  lt_fc-seltext_m = text-010.
  APPEND lt_fc.
  CLEAR lt_fc.

  lt_fc-col_pos   = 6.
  lt_fc-fieldname = 'DETAIL'.
  lt_fc-outputlen = 120.
  lt_fc-ddictxt   = 'M'.
  lt_fc-seltext_m = text-006.
  APPEND lt_fc.
  CLEAR lt_fc.

  ls_print-no_print_listinfos = 'X'.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      is_layout   = ls_layout
      it_fieldcat = lt_fc[]
      is_print    = ls_print
    TABLES
      t_outtab    = gt_log
    EXCEPTIONS
*     PROGRAM_ERROR                  = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    "just do nothing, don't display log
  ENDIF.

ENDFORM.
