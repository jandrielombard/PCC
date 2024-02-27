*&---------------------------------------------------------------------*
*& Report ZAEC_POSTING_FILE_WRAPPER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zaec_posting_file_wrapper message-id zhrau_rpt..
tables pernr.

data: w_runstat  like  pevsh-status,
      w_value    like  pevat-value,
      w_inper    type  pnppabrp,
      w_inyear   type  pnppabrj,
      w_forper   type  pnppabrp,
      w_foryear  type  pnppabrj,
      w_reccount type i,
      w_period   type p_attrval.

data gt_runids type standard table of  pevat.
data gt_valid_runids type standard table of  pevat.



constants:
  c_payrun(2)         type c value 'PP',
  c_run_stat_released type p_evstatus value '32'.

parameters: p_off as checkbox.

*initialization.
*  pnpxabkr = 'QT'.

start-of-selection.

get pernr.

end-of-selection.

* Determine the posting runid based on payroll period
***  select single simu into pevst-simu from  pevst
***         where  type   = c_payrun
***         and    runid  = p_run.
***
***  if pevst-simu = 'X'.
***    message e051 with p_run.
****   Payrun not yet posted.
***  endif.

* Build the AKPER parameter which has the payroll area and payroll period

  if pnptimr9 = 'X'. "current pay period
    w_period = |{ pnpxabkr }/{ pnpdispp }/{ pnpdispj }|.
  else.
    w_period = |{ pnpxabkr }/{ pnppabrp }/{ pnppabrj }|.
  endif.

* Now get the runids for that period
  if p_off = space.
    select *   from  pevat into table gt_runids
           where  type   = c_payrun
           and    attr   = 'AKPER'
           and    id     = '00'
           and    value  = w_period.
  else.
    select *   from  pevat into table gt_runids
           where  type   = c_payrun
           and    attr   = 'SPECRUN'
           and    id     = '00'.

  endif.


* We are only interested in payruns with status 42
  loop at gt_runids assigning field-symbol(<ids>).
    call function 'HR_EVAL_STATUS_GET'
      exporting
        type          = c_payrun
        runid         = <ids>-runid
*       LOCK          = ' '
      importing
        status        = w_runstat
*       NAME          =
*       SIMU          =
*       CREATOR       =
*       CREADATE      =
*       CREATIME      =
      exceptions
        run_not_found = 1
        run_locked    = 2
        others        = 3.

    if sy-subrc <> 0.                                       "#EC *
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.                                                  "#EC *

    if w_runstat =  c_run_stat_released.
      append <ids> to gt_valid_runids.
    endif.
  endloop.

* error if none found
  if gt_valid_runids is initial.
    message e052 with w_period  c_run_stat_released.
  endif.

* error if more than one found
  describe table gt_valid_runids lines data(lin).
  if lin gt 1.
    message e053 with w_period  c_run_stat_released.
  elseif lin = 1.
    data(runid) = gt_valid_runids[ 1 ]-runid.
  endif.

  " call program
  submit zaec_posting_file with p_run = runid
                                using selection-set 'AECPOSTING' exporting list to memory and return.
  data list_tab type table of abaplist.
  call function 'LIST_FROM_MEMORY'
    tables
      listobject = list_tab
    exceptions
      not_found  = 1
      others     = 2.

  if sy-subrc = 0.
    call function 'WRITE_LIST'
      tables
        listobject = list_tab.
  endif.
