class ZUSECL_M99_PCC_PAYROLL_RESULTS definition
  public
  final
  create public .

public section.

  class-methods READ_TPY_AU_PAYROLL_RESULT
    importing
      !CLUSTERID type PCL2-RELID default 'RQ'
      !EMPLOYEENUMBER type PC200-PERNR
      !SEQUENCENUMBER type PC261-SEQNR
    exporting
      !PAYROLL_RESULT type PAYAU_RESULT .
  class-methods READ_TPY_M99_PAYROLL_RESULT
    importing
      !CLUSTERID type PCL2-RELID default 'RQ'
      !EMPLOYEENUMBER type PC200-PERNR
      !SEQUENCENUMBER type PC261-SEQNR
    exporting
      !PAYROLL_RESULT type PAY99_RESULT .
protected section.
private section.
ENDCLASS.



CLASS ZUSECL_M99_PCC_PAYROLL_RESULTS IMPLEMENTATION.


  method READ_TPY_AU_PAYROLL_RESULT.
* Data Tables
    data: lt_p2rx_rt    type sorted table of p2rx_rt with non-unique key dct_seqnr,
          lt_p2rx_wpbp  type sorted table of p2rx_wpbp with non-unique key dct_seqnr,
          lt_p2rx_v0    type sorted table of p2rx_v0 with non-unique key dct_seqnr,
          lt_p2rx_versc type sorted table of p2rx_versc with non-unique key dct_seqnr,
          lt_p2rq_qsup  type sorted table of p2rq_qsup with non-unique key dct_seqnr.
    data: ls_p2rx_versc type p2rx_versc.

* Read Data from Decluster Tables
    refresh: lt_p2rx_rt, lt_p2rx_versc, lt_p2rx_wpbp, lt_p2rx_v0, lt_p2rq_qsup.
    select * from p2rx_rt into table lt_p2rx_rt
      where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.
    select * from p2rx_wpbp into table lt_p2rx_wpbp
      where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.
    select * from p2rx_v0 into table lt_p2rx_v0
      where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.
    select * from p2rx_versc into table lt_p2rx_versc
      where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.
    select * from p2rq_qsup into table lt_p2rq_qsup
     where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.

* Populate result table
    read table lt_p2rx_versc into ls_p2rx_versc with key dct_seqnr = sequencenumber.
    move-corresponding  lt_p2rx_rt to payroll_result-inter-rt.
    move-corresponding  lt_p2rx_wpbp to payroll_result-inter-wpbp.
    move-corresponding  ls_p2rx_versc to payroll_result-inter-versc.
    move-corresponding  lt_p2rx_v0 to payroll_result-inter-v0.
    move-corresponding  lt_p2rq_qsup to payroll_result-nat-qsup.

  endmethod.


  method READ_TPY_M99_PAYROLL_RESULT.
* Data Tables
    data: lt_p2rx_rt    type sorted table of p2rx_rt with non-unique key dct_seqnr,
          lt_p2rx_wpbp  type sorted table of p2rx_wpbp with non-unique key dct_seqnr,
          lt_p2rx_v0    type sorted table of p2rx_v0 with non-unique key dct_seqnr,
          lt_p2rx_versc type sorted table of p2rx_versc with non-unique key dct_seqnr.
    data: ls_p2rx_versc type p2rx_versc.

* Read Data from Decluster Tables
    refresh: lt_p2rx_rt, lt_p2rx_versc, lt_p2rx_wpbp, lt_p2rx_v0.
    select * from p2rx_rt into table lt_p2rx_rt
      where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.
    select * from p2rx_wpbp into table lt_p2rx_wpbp
      where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.
    select * from p2rx_v0 into table lt_p2rx_v0
      where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.
    select * from p2rx_versc into table lt_p2rx_versc
      where dct_pernr eq employeenumber and dct_seqnr eq sequencenumber.

* Populate result table
    read table lt_p2rx_versc into ls_p2rx_versc with key dct_seqnr = sequencenumber.
    move-corresponding  lt_p2rx_rt to payroll_result-inter-rt.
    move-corresponding  lt_p2rx_wpbp to payroll_result-inter-wpbp.
    move-corresponding  ls_p2rx_versc to payroll_result-inter-versc.
    move-corresponding  lt_p2rx_v0 to payroll_result-inter-v0.

  endmethod.
ENDCLASS.
