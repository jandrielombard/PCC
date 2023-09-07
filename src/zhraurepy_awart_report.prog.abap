*&---------------------------------------------------------------------*
*& Report           : ZHRAUREPY_AWART_REPORT                           *
*& Tiltle           : Absence and Attendance Report                    *
*& Transactio Code  :                                                  *
*& Create Date      : 17 Aug 2021                                      *
*& Release          : ECC 6.0                                          *
*& Author           : Satya Aluru                                      *
*&                                                                     *
*&  Custom utility program  to report attributes of                    *
*&  Absence and Attendance wage types and their usage in payroll rules *                                                     *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*&---------------------------------------------------------------------*
*& DATE        User    Description                          TR Number  *
*&---------------------------------------------------------------------*
*&                                                                     *
*&---------------------------------------------------------------------*
report zhraurepy_awart_report line-size 132 message-id zh.

tables: t554s.

selection-screen begin of block one with frame title text-t01.
select-options:
so_awart       for t554s-subty.
parameters pa_mol like t500l-molga memory id mol default '13'.
selection-screen end of block one.

selection-screen begin of block two with frame title text-t02.
parameter:
pa_key like rpdxxxxx-von default sy-datum.
selection-screen end of block two.

data:
  gt_t554t type standard table of t554t,
  gs_t554t type t554t,
  gt_t554s type standard table of t554s with header line,
  gs_t554s type t554s.

include zhraurepy_awart_report_f00.

start-of-selection.
  perform prepare_data.

  read table gt_t554s index 1 transporting no fields.
  if sy-subrc <> 0.
    message e999(zh) with 'No data selected.'.
    exit.
  endif.

  set pf-status 'MAIN'.

  loop at gt_t554s into gs_t554s.
    format color col_group intensified off.
    write: / gs_t554s-moabw.
    write: 10 gs_t554s-subty.
    write: 20 gs_t554s-begda.
    write: 40 gs_t554s-endda.
    write: 60 gs_t554s-klbew.
    write: 80 gs_t554s-abknd.
  endloop.

at user-command.
  case sy-ucomm.
    when 'RULE'.
      perform cycle_matchup_awart using ''.
    when 'RULE_SEL'.
      perform cycle_matchup_awart using 'X'.
    when 'TIMEID'.
      perform write_timeid.
    when others.

  endcase.
