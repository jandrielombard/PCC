*&----------------------------------------------------------------------*
*& Report           : ZUSEHRAUREPY_PRBTTBL_TDUPD00                      *
*& Tiltle           : BT Table Transfer Date Update                     *
*& Create Date      : 14 Sep 2022                                       *
*& Release          : ECC 6.0                                           *
*&                                                                      *
*&  This custom program has been created from RPUCRT00 to update        *
*&  Transfer Date and Time in BT table                                  *
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Mod | Date      | User ID  |Description                    |Change Req *
*-----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
report  zusehraurepy_prbttbl_tdupd00  message-id rpucrt00messages.

tables: t549a, t500l, tfdir, t500p.
tables:  pernr.

infotypes: 0001.

type-pools: stree.

*for ALV-TREE-Control: icons for the buttons of the toolbar
type-pools icon.

include: cnt4defs.

************************************************************************

selection-screen begin of block frm3 with frame title text-f03.
parameters: molga like t500l-molga obligatory memory id mol.
selection-screen end of block frm3.

selection-screen begin of block frm2 with frame title text-f02.
*>>> Start of WOW Custom Change
*parameters: begda like rpuxxxxx-datum1 obligatory.
parameters: begda like rpuxxxxx-datum1 no-display.
selection-screen begin of line.
parameters: noc type h99cwtr-nooc default 'X' radiobutton group oc
        modif id pe2.
selection-screen comment 4(31) text-s12 for field noc modif id pe2.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-192 for field abkr_cal modif id pe1.
selection-screen position pos_low.
parameters: abkr_cal like t569v-abkrs modif id pe1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-200 for field abrp_cal "ACC
modif id pa1.                                              "ACC
selection-screen position pos_low.
parameters:
  abrp_cal like t549q-pabrp modif id pe1, "calculation period
  abrj_cal like t549q-pabrj modif id pe1.
parameters: p_fpbeg type pc261-fpbeg no-display,
            p_fpend type pc261-fpend no-display,
            p_ipend type pc261-ipend no-display.
selection-screen end of line.

selection-screen begin of line.
parameters: yoc type h99cwtr-yesoc radiobutton group oc modif id pe2.
selection-screen comment 4(31) text-s13 for field yoc modif id pe2.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-s11 modif id pe2
    for field pyty_cal.
parameters: pyty_cal like pc261-payty modif id pe2 value check,
            pyid_cal like pc261-payid modif id pe2,
            bond_cal like pc261-bondt modif id pe2.
parameters: p_bondt type pc261-bondt no-display,
            p_payid type pc261-payid no-display,
            p_payty type pc261-payty no-display.
selection-screen end of line.
*<<< End of WOW Custom Change
selection-screen end of block frm2.

*>>> Start of WOW Custom Change
selection-screen begin of block frm4 with frame title text-f04.
selection-screen begin of line.
parameters: p_clrdt type xfeld default 'X' radiobutton group cdt.
selection-screen comment 4(31) text-s14 for field p_clrdt.
selection-screen end of line.

selection-screen begin of line.
parameters: p_setdt type xfeld radiobutton group cdt.
selection-screen comment 4(31) text-s15 for field p_setdt.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-s16 for field p_dtadt.
parameters: p_dtadt like pc209-dtadt,
            p_dtati like pc209-dtati.
selection-screen end of line.
selection-screen end of block frm4.
*<<< End of WOW Custom Change

selection-screen begin of block frm1 with frame title text-f01.
parameters: test     like rpuxxxxx-kr_feld1 default 'X',
*>>> Start of WOW Coustom Change
*            crtlog   like rpuxxxxx-kr_feld2 default 'X',
*            natlog   like rpuxxxxx-kr_feld3,
*            crtexlog like rpuxxxxx-kr_feld4.
            crtlog   like rpuxxxxx-kr_feld2 no-display,
            natlog   like rpuxxxxx-kr_feld3 no-display,
            crtexlog like rpuxxxxx-kr_feld4 no-display.
*<<< End of WOW custom change
parameters: alltab like rpuxxxxx-kr_feld5
            default 'X' no-display.
selection-screen end of block frm1.

******global data definitions*******************************************

*------'COMMIT WORK' after how many employees?--------------------------
constants: cw_trigger type i value 10.

*-----colors for log tree-----------------------------------------------
constants: color_header      value 1,       "level 1
           intens_header     value 1,
           color_pernrinfo   value 1,                         "      2
           intens_pernrinfo  value 0,
           color_statistics  value 1,                        "      2
           intens_statistics value 0,
           color_pernr       value 4,                             "      3
           intens_pernr      value 1,
           color_rgdirinfo   value 4,                         "      4
           intens_rgdirinfo  value 0,
           color_tabhead     value 2,                           "      5
           intens_tabhead    value 1,
           color_tab         value 0,                               "      6
           intens_tab        value 0,
           color_err         value 6,          " ERROR
           intens_err        value 0,
           color_warning     value 3,      "WARNING
           intens_warning    value 0.

*-----global data for report control------------------------------------
data: employee_number like pc200-pernr,
      cluster_id      like t500l-relid,
      isocode         like t500l-intca.

data: commit_counter type i.    "counter for 'COMMIT WORK' trigger
data: safety_checked.            "flag: if initial, DDIC structure is OK
data: lv_print type x value '02'.   "Note 966251

*-----buffered tables --------------------------------------------------
data: i500l like t500l occurs 0 with header line.
data: i549a like t549a occurs 0 with header line.
data: last_t500p like t500p.
*-----data for statistics-----------------------------------------------
data: seqnrs         type i,                   "Relevant records of PCL2
      updated_seqnrs type i,
      pumerr         type i,                   "Erroneous records
      ppernr         type i, "Number of selected employee numbers
      err_pernr      type i. "number of employee numbers with  error

*-----return codes------------------------------------------------------
data: importbuff_subrc   like sy-subrc,
      importcu_subrc     like sy-subrc,
      addcu_subrc        like sy-subrc,
      setcu_subrc        like sy-subrc,
      addcucountry_subrc like sy-subrc,
      setcucountry_subrc like sy-subrc,
      importcurr_subrc   like sy-subrc,
      importprev_subrc   like sy-subrc,
      export_subrc       like sy-subrc.

*-----payroll result information----------------------------------------
field-symbols: <payresult_current>,
               <payresult_previous>,
               <cumulation_tables>.

data: rgdir_entry     like pc261,
      prev_rgdirentry like pc261,
      relevant_rgdir  like rgdir_entry occurs 30 with header line,
      full_rgdir      like rgdir_entry occurs 100 with header line.


*-----tree tables-------------------------------------------------------
data: treeline like snodetext.         "workarea
data: treetab like treeline occurs 0.  "tree for one employee
data: tree like treeline occurs 0.     "tree for successful ones
data: treeerr like treeline occurs 0.  "erroneous employees
data: globaltree like treeline occurs 0.  "whole tree
data: pernr_error.                     "flag: employee has error

*-----payroll log tables------------------------------------------------
data: crt_logtext       like plog_text occurs 0 with header line,
      crt_errortext     like plog_text occurs 0 with header line,
      crtlog_national   like plog_text occurs 0 with header line,
      crterr_national   like plog_text occurs 0 with header line,
      titlebar_national like treeline-text.

*-----data for generation of main routine-------------------------------
data: subpool_name   like sy-repid,
      code_text(80),
      generated_code like code_text occurs 0,
      gen_subrc      like sy-subrc,
      message(80),
      line           type i,
      word(80).

*-----data for country exit SETCU/ADDCU---------------------------------
data: addcu_country_module(30) value 'PYXX_ADD_CUMULATION',
      setcu_country_module(30) value 'PYXX_SET_CUMULATION'.
data: addcu_country_exists,
      setcu_country_exists.

*-----window for cumulations (out-of-sequence-checks solution)
data: window type pay_cum_win.
data: payroll_until type d.

*-----variables for dynpro 100; ALV-List-Tree model------------------*
data ok_code             like sy-ucomm.

data ref_container       type ref to cl_gui_docking_container.
data ref_split_container type ref to cl_gui_splitter_container.
data ref_cell_top        type ref to cl_gui_container.
data ref_cell_bot        like ref_cell_top.

data ref_tree_model      type ref to cl_list_tree_model.
data ref_toolbar         type ref to cl_gui_toolbar.

data lt_events           type cntl_simple_events.
data wa_event            like line of lt_events.

data lt_hit_nodes        type treemnotab.
data statistics_tab      like treeline occurs 0.

class lcl_event_handler definition.
  public section.
    class-methods on_function_selected
                for event function_selected of cl_gui_toolbar
      importing fcode.
endclass.

*-------------------------------------------------------------------*
*this include contains the whole coding necessary for the
*ALV-Tree-Control Log. Within the main routine no special code
*is reqired except the jump to dynpro 100. All definitions
*are implemented in the central Data-definition part
include cntlof01.
*-------------------------------------------------------------------*

*#######################################################################
at selection-screen.
  perform re500l using molga
                       cluster_id
                       isocode.

  perform selection_screen_input.
*#######################################################################
start-of-selection.
*>>> Start of WOW Coustom Change
  p_payty = pyty_cal.
  p_payid = pyid_cal.
  p_bondt = bond_cal.
*<<< End of WOW Coustom Change
  perform assign_payresult.
  perform check_countryspecs.
  perform logtree_header.

*##main routine#########################################################
get pernr.
*---------
  ppernr = ppernr + 1.
  perform init_pernr.
*--------
  perform check_molga.
  if not pernr_error is initial.
    perform logtree_pernr.
    reject.
  endif.
*--------
  perform read_rgdir tables full_rgdir "get payroll directory
                            relevant_rgdir.
*--------
  if not relevant_rgdir[] is initial.
    perform import_to_buffer tables relevant_rgdir[].
    if importbuff_subrc eq 0.
      perform rebuild_cumulations using <payresult_current>
                                        <payresult_previous>.
      if test = space.
        perform update_results using test.
      endif.
      "ELSE. "don't do an update
    else.
      perform logtree_cu_mismatch.     "cluster CU <> payroll results
    endif.

  else.
    perform logtree_noresults.         "cluster CU is empty for pernr
  endif.
*----------
  perform logtree_pernr.
  perform commit_work.
*#######################################################################
end-of-selection.
  perform logtree_all_pernr.
  perform logtree_statistics using ppernr
                                   err_pernr
                                   seqnrs
                                   pumerr.

* log has to be modified; an ALV-List-Tree is used instead
  if ( sy-batch = 'X' ) or
     ( sy-subty o lv_print ).                              "Note 966251
    perform display_log_tree.
  else.
    perform display_alv_logtree.
  endif.

*#######################################################################
* form routines
*---------------------------------------------------------------------*
*       FORM REBUILD_CUMULATIONS                                      *
*---------------------------------------------------------------------*
*       rebuilds table CRT for all payroll results in RGDIR starting
*       with the first payroll run after BEGDA.
*       After importing the current payroll result, the previous
*       result is read with a simulated 'IMPORT L'.
*       Table CRT is rebuilt by a simulation of 'SETCU' and 'ADDCU'.
*       The updated result is then exported to the buffer.
*       For country-specific actions which are performed in SETCU-NATIO
*       and ADDCU-NATIO (during payroll run), function modules
*       PYXX_ADD_CUMULATION and PYXX_SET_CUMULATION have to be
*       installed in function group HRPAY99_RPUCRT00.
*       Exception: From 4.6C, new cumulations for US/CA have integrated
*       country exit
*---------------------------------------------------------------------*
*   -->  PREVIOUS_RESULT
*  <-->  CURRENT_RESULT                                               *
*---------------------------------------------------------------------*

form rebuild_cumulations using current_result
                               previous_result.
  loop at relevant_rgdir into rgdir_entry.

    seqnrs = seqnrs + 1.
*---adjust PAYROLL_UNTIL - latest regular payroll converted
    if rgdir_entry-payty = space and rgdir_entry-fpend > payroll_until.
      payroll_until = rgdir_entry-fpend.
    endif.
*-----------
    perform import_current_result using current_result.
*>>> Start of WOW Custom Code
*-----------
*    perform import_previous_result using current_result   "'IMPORT L'
*                                         previous_result
*                                         prev_rgdirentry.
**-----------
*    perform set_cumulation using current_result
*                                 previous_result.    " 'SETCU'
**-----------
*    perform add_cumulation using current_result
*                                 previous_result.    " 'ADDCU

    perform set_transfer_date using current_result.   "Set Transfer Date
*>>> End Of WOW Custom Code
*-----------
    perform export_current_result using current_result.    " 'EXPRT'
*-----------
    perform logtree_seqnr.
*-----------
    perform logtree_cumulation using current_result.
*-----------
    updated_seqnrs = updated_seqnrs + 1.
*------------
  endloop.

endform.

*---------------------------------------------------------------------*
*       FORM ADD_CUMULATION                                           *
*---------------------------------------------------------------------*
*       simulates payroll function ADDCU                              *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
form add_cumulation using p_current_result
                          p_previous_result.

  clear: addcu_subrc.
  field-symbols: <curr_rt>        type hrpay99_rt,
                 <curr_crt>       type hrpay99_crt,
                 <curr_versc>     type pc202,
                 <crt_upd>        type hrpay99_crt,
                 <cumul_tab_curr> type table,
                 <cumul_tab_upd>  type table.
  data: cumul_tab_name(80).

  data: help_aper type pc2aper.
  data: i56c8 type t56c8 occurs 0 with header line.

  assign component 'INTER-RT' of structure p_current_result to <curr_rt>.
  assign component 'INTER-CRT'
                     of structure p_current_result to <curr_crt>.
  assign component 'INTER-VERSC'
                      of structure p_current_result to <curr_versc>.

  if window eq 0.

*---old cumulation logic
    call function 'RP_CUMULATE_RT_CRT'
      exporting
        begda             = rgdir_entry-fpbeg
        endda             = rgdir_entry-fpend         "XAIK043247
        bondt             = rgdir_entry-bondt
        molga             = molga
        paper             = rgdir_entry-fpper
        permo             = rgdir_entry-permo
*       prcls             = 'P30'
        log               = crtexlog
        amt_curr          = <curr_versc>-waers
      tables
        crt               = <curr_crt>
        rt                = <curr_rt>
        logtext           = crt_logtext
        errortext         = crt_errortext
      exceptions
        cumulation_failed = 1
        no_entry_found    = 2
        others            = 3.
    if sy-subrc <> 0.
      addcu_subrc = sy-subrc.
      perform errorlog_pernr using p_current_result.
    endif.

*----this corresponds to PERFORM ADDCU-NATIO----------------------------
    if not alltab is initial.                           "QNZL9CK030386
      perform addcu_country using p_current_result
                                  p_previous_result.
    endif.                                              "QNZL9CK030386
  else.
*---new cumulation logic

*  prepare APER fields for ADDCU module
    help_aper-paper = rgdir_entry-fpper.
    help_aper-chkdt = rgdir_entry-paydt.
    help_aper-pbegd = rgdir_entry-fpbeg.
    help_aper-pendd = rgdir_entry-fpend.
    help_aper-permo = rgdir_entry-permo.

*  move cumulation tables to structure
    clear <cumulation_tables>.
    <cumulation_tables> = p_current_result.

*  ADDCU
    call function 'HR_UPDATE_CUMULATION_TABLES'
      exporting
        imp_molga            = molga
        imp_processing_class = 'P30'
        imp_rt               = <curr_rt>
        imp_log              = crtexlog
        imp_payroll_result   = p_current_result
        imp_currency         = <curr_versc>-waers
        imp_aper             = help_aper
        imp_employeenumber   = pernr-pernr
      tables
        logtext              = crt_logtext
      changing
        cumul_tab_all        = <cumulation_tables>
      exceptions
        cumulation_failed    = 1.

    if not sy-subrc is initial.
      addcu_subrc = sy-subrc.
      perform errorlog_pernr using p_current_result.
    endif.

*  move cumulation tables back to result structure
    assign component 'INTER-CRT' of structure <cumulation_tables>
                        to <crt_upd>.
    <curr_crt> = <crt_upd>.
    sort <curr_crt> by cumyr descending          "note 1337468
        lgart
        cumty
        cumno descending.
    if not alltab is initial.                       "QNZL9CK030386
      select * from t56c8 into table i56c8 where molga = molga.
      loop at i56c8.
        concatenate 'NAT-' i56c8-tabname into cumul_tab_name.
        assign component cumul_tab_name of structure p_current_result
              to <cumul_tab_curr>.
        assign component cumul_tab_name of structure <cumulation_tables>
              to <cumul_tab_upd>.
        <cumul_tab_curr> = <cumul_tab_upd>.
      endloop.
    endif.                                           "QNZL9CK030386
  endif.

endform.                               " CUMULATION.

*---------------------------------------------------------------------*
*       FORM IMPORT_PREVIOUS_RESULT                                    *
*---------------------------------------------------------------------*
*       simulates payroll function 'IMPORT L'
*---------------------------------------------------------------------*
form import_previous_result using p_current_result
                                  p_previous_result
                                  p_prev_rgdirentry type pc261.
  field-symbols: <evp>        type pc261,
                 <curr_versc> type pc202,
                 <prev_versc> type pc202.

  clear p_previous_result.
  clear importprev_subrc.

  call function 'CD_READ'
    exporting
      in_seqnr        = rgdir_entry-seqnr
    importing
      out_seqnr       = p_prev_rgdirentry-seqnr
    tables
      new_rgdir       = full_rgdir
    exceptions
      no_record_found = 1
      others          = 2.

  if sy-subrc eq 0.                    "there is a previous result
    call function 'PYXX_READ_PAYROLL_RESULT'
      exporting
        clusterid                    = cluster_id
        employeenumber               = employee_number
        sequencenumber               = p_prev_rgdirentry-seqnr
*       READ_ONLY_BUFFER             = ' '
*       READ_ONLY_INTERNATIONAL      = ' '
        filter_cumulations           = space
      changing
        payroll_result               = p_previous_result
      exceptions
        illegal_isocode_or_clusterid = 1
        error_generating_import      = 2
        import_mismatch_error        = 3
        subpool_dir_full             = 4
        no_read_authority            = 5
        no_record_found              = 6
        versions_do_not_match        = 7
        others                       = 8.
    importprev_subrc = sy-subrc.
    if importprev_subrc ne 0.
      perform errorlog_pernr using p_current_result.
    else.                              "result successfully imported

      read table full_rgdir with key seqnr = p_prev_rgdirentry-seqnr
                                        into p_prev_rgdirentry.
      assign component 'EVP' of structure p_previous_result to <evp>.
      <evp> = p_prev_rgdirentry.
      assign component 'INTER-VERSC'
                       of structure p_current_result to <curr_versc>.
      assign component 'INTER-VERSC'
                       of structure p_previous_result to <prev_versc>.
      if <curr_versc>-waers ne <prev_versc>-waers.
        perform convert_result using employee_number
                                     <curr_versc>-waers
                                     <prev_versc>-waers
                            changing p_previous_result.
      endif.

    endif.                             "subrc ne 0
  endif.                               "no previous result found
endform.

*---------------------------------------------------------------------*
*       FORM SET_CUMULATION                                           *
*---------------------------------------------------------------------*
*       simulates payroll function 'SETCU'                            *
*---------------------------------------------------------------------*
form set_cumulation using p_current_result
                          p_previous_result.

  data: current_datmo like t549a-datmo.
  data: error_msg type hrplog_msg.
  data: i56c8 type t56c8 occurs 0 with header line.
  data: cumul_tab_name(80).

*---help paper becouse of type check
  data: l_paper type pc2paper.

  field-symbols: <curr_crt>       type hrpay99_crt,
                 <prev_crt>       type hrpay99_crt,
                 <cumul_tab_curr> type table,
                 <cumul_tab_prev> type table.

  clear: crt_errortext, crt_errortext[],
         crt_logtext, crt_logtext[].
  clear: setcu_subrc.

  assign component 'INTER-CRT' of structure p_current_result
                                to <curr_crt>.
  if window eq 0.

*---old cumulation logic
    assign component 'INTER-CRT' of structure p_previous_result
                                  to <prev_crt>.

    <curr_crt> = <prev_crt>.

    perform re549a using rgdir_entry-abkrs
                    changing current_datmo.

    call function 'RP_PREPARE_CRT'
      exporting
        abkrs           = rgdir_entry-abkrs
        begda           = rgdir_entry-fpbeg
        bondt           = rgdir_entry-bondt
        datmo           = current_datmo
        molga           = molga
        paper           = rgdir_entry-fpper
        payid           = rgdir_entry-payid
        payty           = rgdir_entry-payty
        permo           = rgdir_entry-permo
        seqnr           = rgdir_entry-seqnr
        log             = crtexlog
      tables
        crt             = <curr_crt>
        new_rgdir       = full_rgdir
        logtext         = crt_logtext
        errortext       = crt_errortext
      exceptions
        clean_up_failed = 1
        no_entry_found  = 2
        others          = 3.
    if sy-subrc <> 0.
      setcu_subrc = sy-subrc.
      perform errorlog_pernr using p_current_result.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

*-----this corresponds to PERFORM SETCU-NATIO in the payroll run--------
    if not alltab is initial.                             "QNZL9CK030386
      perform setcu_country using p_current_result           "SETCU-NATIO
                                  p_previous_result.
    endif.                                                "QNZL9CK030386
  else.
*---new cumulation logic -just eliminate CRT entries outside window
    l_paper = rgdir_entry-fpper.

* move cumulation tables to structure
    clear <cumulation_tables>.
    <cumulation_tables> = p_previous_result.

* SETCU
    call function 'HR_PREPARE_CUMULATION_TABLES'
      exporting
        paper         = l_paper
        begda         = rgdir_entry-fpbeg
        endda         = rgdir_entry-fpend
        paydt         = rgdir_entry-paydt
        payty         = rgdir_entry-payty
        molga         = molga
        payroll_until = payroll_until
        log           = crtexlog
      tables
        logtext       = crt_logtext
      changing
        cumul_tab_all = <cumulation_tables>
      exceptions
        others        = 1.
    if sy-subrc <> 0.
      setcu_subrc = sy-subrc.
      perform errorlog_pernr using p_current_result.
    endif.

* move cumulation tables back to result structure
    assign component 'INTER-CRT' of structure <cumulation_tables>
                        to <prev_crt>.
    <curr_crt> = <prev_crt>.

    if not alltab is initial.                              "QNZL9CK030386
      select * from t56c8 into table i56c8 where molga = molga.
      loop at i56c8.
        concatenate 'NAT-' i56c8-tabname into cumul_tab_name.
        assign component cumul_tab_name of structure p_current_result
              to <cumul_tab_curr>.
        assign component cumul_tab_name of structure <cumulation_tables>
              to <cumul_tab_prev>.
        <cumul_tab_curr> = <cumul_tab_prev>.
      endloop.
    endif.                                                 "QNZL9CK030386

    perform complete_cumul_for_oos_yea
            using p_current_result
                  p_previous_result.

  endif.

endform.

*---------------------------------------------------------------------*
*       FORM READ_RGDIR                                               *
*---------------------------------------------------------------------*
*       imports cluster CU to FULL_RGDIR. FULL_RGDIR is then cut to
*       RELEVANT_RGDIR which contains all results that have to be
*       updated
*---------------------------------------------------------------------*
form read_rgdir tables p_full_rgdir structure pc261
                       p_relevant_rgdir structure pc261.
  data: local_molga like t500l-molga.

  call function 'CU_READ_RGDIR'
    exporting
      persnr          = employee_number
    importing
      molga           = local_molga
    tables
      in_rgdir        = p_full_rgdir
    exceptions
      no_record_found = 1
      others          = 2.
*>>> Start of WOW Custom Code
*  if sy-subrc eq 0.
*    call function 'CD_READ_ALL_FROM_DATE'
*      exporting
*        from_date = begda
*      tables
*        in_rgdir  = full_rgdir
*        out_rgdir = p_relevant_rgdir.
*  else.
*    clear relevant_rgdir.
*  endif.
  if sy-subrc eq 0.
    clear: p_relevant_rgdir.
    loop at p_full_rgdir.
      if noc eq abap_true.
        check p_full_rgdir-abkrs eq abkr_cal.          "MOD001++
        check p_full_rgdir-fpbeg eq p_fpbeg.
        check p_full_rgdir-fpend eq p_fpend.
*        check p_full_rgdir-ipend eq p_ipend.          "MOD001--
      else.
        check p_full_rgdir-payty eq p_payty.
        check p_full_rgdir-payid eq p_payid.
        check p_full_rgdir-bondt eq p_bondt.
*        check p_full_rgdir-ipend eq p_ipend.          "MOD001--
      endif.

      move-corresponding p_full_rgdir to p_relevant_rgdir.
      append p_relevant_rgdir. clear p_relevant_rgdir.
    endloop.
  else.
    clear relevant_rgdir.
  endif.
*<<< End of WOW Custom Code
endform.                               "READ_RGDIR

*---------------------------------------------------------------------*
*       FORM LOGTREE_STATISTICS                                       *
*---------------------------------------------------------------------*
*       when everything is finished: statistics of the PCL2 update
*---------------------------------------------------------------------*
form logtree_statistics using p_pernr
                              p_err_pernr
                              p_prel
                              p_pumerr.

  data: p_succ_pernr type i.
  data: p_succ_prel type i.

  p_succ_pernr = p_pernr - p_err_pernr.
  p_succ_prel = p_prel - p_pumerr.

  clear treeline.

  treeline-text = employee_number.
  treeline-tintensiv = intens_statistics.
  treeline-tlevel = 2.
  treeline-tlength = strlen( text-u00 ).
  treeline-tcolor = color_statistics.
  treeline-text = text-u00.            "statistics
  append treeline to globaltree.

  treeline-tlevel = 3.
  treeline-tlength = 45.

  write: text-u02 to treeline-text,    "successful pernrs
         p_succ_pernr to treeline-text+34 left-justified.
  append treeline to globaltree.
* especially for ALV tree-control
  append treeline to statistics_tab.

  write: text-u03 to treeline-text,    "pernrs with error
        p_err_pernr to treeline-text+34 left-justified.
  append treeline to globaltree.
* especially for ALV tree-control
  append treeline to statistics_tab.

  write: text-u04 to treeline-text,    "relevant results
        p_prel to treeline-text+34 left-justified.
  append treeline to globaltree.
* especially for ALV tree-control
  append treeline to statistics_tab.

  write: text-u06 to treeline-text,    "successful result
        p_succ_prel to treeline-text+34 left-justified.
  append treeline to globaltree.
* especially for ALV tree-control
  append treeline to statistics_tab.

  write: text-u09 to treeline-text,    "results with error
        p_pumerr to treeline-text+34 left-justified.
  append treeline to globaltree.
* especially for ALV tree-control
  append treeline to statistics_tab.


endform.                               "end of LOGTREE_STATISTICS.

*&---------------------------------------------------------------------*
*&      FORM IMPORT_CURRENT_RESULT
*&---------------------------------------------------------------------*
*      reads the current payroll result from the buffer
*----------------------------------------------------------------------*
*  <--  p2        text
*----------------------------------------------------------------------*
form import_current_result using p_current_result.
  field-symbols: <evp> type pc261.
  data: rt_header like pc207.
*  FIELD-SYMBOLS: <rt> TYPE hrpay99_rt.
  data: safety_switch.

  call function 'PYXX_READ_PAYROLL_RESULT'
    exporting
      clusterid                    = cluster_id
      employeenumber               = employee_number
      sequencenumber               = rgdir_entry-seqnr
*     READ_ONLY_BUFFER             = ' '
*     READ_ONLY_INTERNATIONAL      = ' '
      filter_cumulations           = space
    changing
      payroll_result               = p_current_result
    exceptions
      illegal_isocode_or_clusterid = 1
      error_generating_import      = 2
      import_mismatch_error        = 3
      subpool_dir_full             = 4
      no_read_authority            = 5
      no_record_found              = 6
      versions_do_not_match        = 7
      others                       = 8.
  importcurr_subrc = sy-subrc.
  assign component 'EVP' of structure p_current_result to <evp>.
  <evp> = rgdir_entry.
  if importcurr_subrc ne 0.
    perform errorlog_pernr using p_current_result.
  endif.

*--- check: out-of-sequence reversal and no retrocalc performed?------

  if rgdir_entry-reversal = 'R' and rgdir_entry-srtza = 'A'.
    importcurr_subrc = 9.
    perform errorlog_pernr using p_current_result.
  endif.

*----------------------------------------------------------------------
endform.                               " IMPORT_PERIOD
*
*&---------------------------------
*------------------------------------*
*&      Form  RE500L
*&---------------------------------
*------------------------------------*
*      reads iso-code and cluster RELID from T500L for the selected
*      molga
*----------------------------------
*------------------------------------*
*      -->P_CLUSTER_ID  text
*      -->P_ISOCODE  text
*----------------------------------
*------------------------------------*
form re500l using    p_molga
                     p_cluster_id
                     p_isocode.
  read table i500l with key molga = p_molga.
  if sy-subrc ne 0.
    select * from t500l where molga = p_molga.
    endselect.
    if sy-subrc = 0.
      i500l = t500l.
      append i500l.
    else.
      message e000 with molga.
    endif.
  endif.
  p_cluster_id = i500l-relid.
  p_isocode    = i500l-intca.

endform.                               " RE500L
*&---------------------------------------------------------------------*
*&      Form  EXPORT_CURRENT_RESULT
*&---------------------------------------------------------------------*
*       writes the current payroll result to the buffer table
*       EXPORT_DATA (in function group HRPAY99_BUFFER)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form export_current_result using p_current_result.

  call function 'PYXX_WRITE_PAYROLL_RESULT'
    exporting
      clusterid                    = cluster_id
      employeenumber               = employee_number
      sequencenumber               = rgdir_entry-seqnr
      payroll_result               = p_current_result
    exceptions
      illegal_isocode_or_clusterid = 1
      error_generating_export      = 2
      subpool_dir_full             = 4
      no_update_authority          = 5
      others                       = 6.
  export_subrc = sy-subrc.
  if export_subrc ne 0.
    perform errorlog_pernr using p_current_result.
*    message id sy-msgid type sy-msgty number sy-msgno
*           with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                               " EXPORT_RESULT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RESULTS
*&---------------------------------------------------------------------*
*       updates PCL2 with the contents of the buffer table EXPORT_DATA
*       (if test switch is off)
*----------------------------------------------------------------------*
*      -->P_TEST  text
*----------------------------------------------------------------------*
form update_results using    p_test.

  if p_test = space.
    call function 'HR_FLUSH_BUFFER_UPDATE_PCLX'
      exceptions
        insert_error        = 1
        no_update_authority = 2
        others              = 3.
    case sy-subrc .
      when 1.
        pernr_error = 'X'.
        clear treeline.
        treeline-text = 'Fehler beim Update der PCL2!'(e10).
        treeline-tintensiv = intens_rgdirinfo.
        treeline-tlevel = 4.
        treeline-tlength = strlen( treeline-text ).
        treeline-tcolor = color_err.
        treeline-text9 = 'X'.
        append treeline to treetab.
      when 2.
        message id sy-msgid type 'A' number sy-msgno "no update authority-
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4."stop processing
    endcase.
  endif.
endform.                               " UPDATE_RESULTS
*&---------------------------------------------------------------------*
*&      Form  IMPORT_TO_BUFFER
*&---------------------------------------------------------------------*
*       import all entries into buffer
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form import_to_buffer tables p_buff_rgdir structure pc261.


  if p_buff_rgdir[] is initial.        "no payroll results!
    importbuff_subrc = 4.
    exit.
  endif.

  call function 'HR_IMPORT_BUFFER_FROM_PCLX'
    exporting
      employee_number   = employee_number
      cluster_id        = cluster_id
*     from_sequence_number =
*     to_sequence_number   =
*     INVALIDATE_BUFFER =
    tables
      rgdir             = p_buff_rgdir
    exceptions
      no_results        = 1
      no_read_authority = 2
      others            = 3.
  importbuff_subrc = sy-subrc.
  if importbuff_subrc = 2.
    message id sy-msgid type 'A' number sy-msgno  "no read authority -
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.     "abort processing
  endif.
endform.                               " IMPORT_TO_BUFFER
*&---------------------------------------------------------------------*
*&      Form  RE549A
*&---------------------------------------------------------------------*
*       T549A has to be read with the current payroll area because
*       the function module 'RP_PREPARE_CRT' (SETCU) needs the DATMO
*----------------------------------------------------------------------*
form re549a  using p_abkrs
             changing p_datmo.
  read table i549a with key abkrs = p_abkrs.
  if sy-subrc ne 0.
    select * from t549a into i549a where abkrs = p_abkrs.
    endselect.
    if sy-subrc eq 0.
      append i549a.
    endif.
  endif.
  p_datmo = i549a-datmo.
endform.                               " RE549A
*&---------------------------------------------------------------------*
*&      Form  logtree_cumulation
*&---------------------------------------------------------------------*
*       displays:   CRT (if CRTLOG is selected)
*                   country-specific cumulation (if NATLOG is selected)
*                   setcu/addcu log (if crtexlog is selected)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form logtree_cumulation using p_current_result.


  field-symbols: <crt>    type hrpay99_crt,
                 <versc>  type pc202,
                 <text>   like treeline-text1,
                 <length> like treeline-tlength1.
  data: crt_header     like pc22y,
        long_text      like sy-lisel,
        field_name(40) type c,
        text_length    type i,
        counter(1)     type n.

  if not crtlog is initial.

    assign component 'INTER-CRT' of structure p_current_result to <crt>.
    assign component 'INTER-VERSC'
                      of structure p_current_result to <versc>.

*----CRT: header line
    clear treeline.
    treeline-tlevel = 5.
    treeline-text = 'CRT'(t07).
    treeline-tlength = strlen( text-t07 ).
    treeline-tcolor = color_tabhead.
    treeline-tintensiv = intens_tabhead.
    treeline-hotspot = 1.
    append treeline to treetab.
*---CRT: table header.
    clear treeline.
    treeline-tlevel = 6.
    concatenate
       'Lohnart  Typ der Kum.    Anzahl          Betrag'(p03)
       <versc>-waers into treeline-text separated by space.

    treeline-tlength = strlen( text-p03 ) + 4.
    treeline-tcolor = color_tab.
    treeline-tintensiv = 0.
    append treeline to treetab.
    treeline-tcolor = color_tab.
    treeline-tintensiv = intens_tab.
    treeline-text = sy-uline.
    append treeline to treetab.

*----CRT: body
    clear treeline.
    treeline-tlevel = 6.
    treeline-tcolor = color_tab.
    treeline-tintensiv = intens_tab.
*    treeline-tlength1 = strlen( text-p03 ) + 4.
    treeline-tlength1 = 8.
    treeline-tintensiv1 = 1.
    treeline-tlength2 = 15.
    treeline-tlength3 = 15.
    treeline-tlength4 = 15.
    treeline-tintensiv4 = 1.
    loop at <crt> into crt_header.
      write: crt_header-lgart to treeline-text1(4),
             crt_header-cumty to treeline-text2(1),
             crt_header-anzhl to treeline-text3 left-justified,
             crt_header-betrg currency <versc>-waers
                              to treeline-text4 left-justified.
      append treeline to treetab.
    endloop.
  endif.                               "not CRTLOG is initial
  if not crtexlog is initial.
*-------CRT cumulation: header line
    clear treeline.
    treeline-tlevel = 5.
    treeline-text = 'Kumulation aus RT in CRT'(t08).
    treeline-tlength = strlen( text-t08 ).
    treeline-tcolor = color_tabhead.
    treeline-tintensiv = intens_tabhead.
    append treeline to treetab.
*------RT: log
    loop at crt_logtext.
      clear treeline.
      treeline-tlevel = 6.
      treeline-tlength1 = crt_logtext-tlength1.
      treeline-tlength2 = crt_logtext-tlength2.
      treeline-tlength3 = crt_logtext-tlength3.
      treeline-tlength4 = crt_logtext-tlength4.
      treeline-text1 = crt_logtext-text1.
      treeline-text2 = crt_logtext-text2.
      treeline-text3 = crt_logtext-text3.
      treeline-text4 = crt_logtext-text4.
      treeline-tintensiv1 = crt_logtext-tintensiv1.
      treeline-tintensiv2 = crt_logtext-tintensiv2.
      treeline-tintensiv3 = crt_logtext-tintensiv3.
      treeline-tintensiv4 = crt_logtext-tintensiv4.
      treeline-tcolor = color_tab.
      append treeline to treetab.
    endloop.
  endif.                               "not crtexlog is initial

*---log: national cumulation

  if ( not natlog is initial and not setcu_country_exists is initial ).
    clear treeline.
    treeline-tlevel = 5.
    treeline-text = titlebar_national.
    treeline-tlength = strlen( titlebar_national ).
    treeline-tcolor = color_tabhead.
    treeline-tintensiv = intens_tabhead.
    append treeline to treetab.

    loop at crtlog_national.
      clear treeline.
      treeline-tlevel = 6.
      treeline-tcolor = color_tab.
      if crtlog_national-tlength1 gt 75 or                  "WOGL9CK054879
           crtlog_national-tlength2 gt 75 or                      "!
           crtlog_national-tlength3 gt 75 or                      "!
           crtlog_national-tlength4 gt 75.                        "!
*------At least one text is too long --> build text field new--------
        concatenate crtlog_national-text1 crtlog_national-text2 "!
                    crtlog_national-text3 crtlog_national-text4 "!
                    into long_text separated by space.          "!
        text_length = strlen( long_text ).                      "!
        do 4 times.                                             "!
          counter = sy-index.                                   "!
          concatenate 'TREELINE-TEXT' counter into field_name.  "!
          assign (field_name) to <text>.                        "!
          if text_length gt 75.                                 "!
            <text> = long_text(75).                             "!
            long_text = long_text+75.                           "!
            concatenate 'TREELINE-TLENGTH' counter into field_name.
            assign (field_name) to <length>.                    "!
            <length> = 75.                                      "!
            subtract 75 from text_length.                       "!
          else.                                                 "!
            <text> = long_text(text_length).                    "!
            long_text = long_text+text_length.                  "!
            concatenate 'TREELINE-TLENGTH' counter into field_name.
            assign (field_name) to <length>.                    "!
            <length> = text_length.                             "!
            exit.                                               "!
          endif.                                                "!
        enddo.                                                  "!
      else.                                                     "!
        treeline-tlength1 = crtlog_national-tlength1.
        treeline-tlength2 = crtlog_national-tlength2.
        treeline-tlength3 = crtlog_national-tlength3.
        treeline-tlength4 = crtlog_national-tlength4.
        treeline-text1 = crtlog_national-text1.
        treeline-text2 = crtlog_national-text2.
        treeline-text3 = crtlog_national-text3.
        treeline-text4 = crtlog_national-text4.
        treeline-tintensiv1 = crtlog_national-tintensiv1.
        treeline-tintensiv2 = crtlog_national-tintensiv2.
        treeline-tintensiv3 = crtlog_national-tintensiv3.
        treeline-tintensiv4 = crtlog_national-tintensiv4.
      endif.                                             "WOGL9CK054879
*     IF crtlog_national-tlength1 > 75.                          "!
*        treeline-text1 = crtlog_national-text1+75.              "!
*        APPEND treeline TO treetab.                             "!
*      ENDIF.                                                    "!
      append treeline to treetab.
    endloop.

  endif.

*----error exporting result?----------------
  if export_subrc ne 0.
    clear treeline.
    treeline-tlevel = 5.
    treeline-text =
      'Interner Fehler beim Export der Daten in den Puffer'(e02).
    treeline-tlength = strlen( text-e02 ).
    treeline-tcolor = color_err.
    treeline-tintensiv = intens_err.
    treeline-tlength9 = 1.
    append treeline to treetab.
  endif.

*------CRT cumulation: error?
  if addcu_subrc ne 0 or setcu_subrc ne 0.
    loop at crt_errortext.
      clear treeline.
      treeline-tlevel = 5.
      treeline-text = crt_errortext-text1.
      treeline-tlength = 75.
      treeline-tcolor = color_err.
      treeline-tintensiv = intens_err.
      treeline-tlength9 = 1.
      append treeline to treetab.
    endloop.
  endif.

endform.                               " logtree_cumulation
*&---------------------------------------------------------------------*
*&      Form  addcu_country
*&---------------------------------------------------------------------*
*     Simulates PERFORM ADDCU-NATIO in payroll function ADDCU:
*     Calls function module PY<ISO code>_ADD_CUMULATION (if existing)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form addcu_country using p_p_current_result
                         p_p_previous_result.
  clear: addcucountry_subrc.
  if addcu_country_exists = 'X'.
    call function addcu_country_module
      exporting
        log             = natlog
      tables
        local_ptext     = crtlog_national
        errortext       = crterr_national
      changing
        current_result  = p_p_current_result
        previous_result = p_p_previous_result
      exceptions
        error_addcu     = 1.
    addcucountry_subrc = sy-subrc.
  endif.
endform.                               " addcu_country
*&---------------------------------------------------------------------*
*&      Form  CHECK_COUNTRYSPECS
*&---------------------------------------------------------------------*
*       checks if there are country-specific function modules for
*       ADDCU and SETCU. The naming convention is
*       'PY<ISO_code>_ADD_CUMULATION'
*       and 'PY<ISO code>_SET_CUMULATION'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_countryspecs.

  replace 'XX' with isocode into addcu_country_module.
  select single * from tfdir where funcname = addcu_country_module.
  if sy-subrc = 0.
    addcu_country_exists = 'X'.
  endif.
  replace 'XX' with isocode into setcu_country_module.
  select single * from tfdir where funcname = setcu_country_module.
  if sy-subrc = 0.
    setcu_country_exists = 'X'.
  endif.

*---determine window for cumulation (out-of-sequence-checks)

  call function 'HR_GET_CUMULATION_WINDOW'
    exporting
      imp_molga  = molga
    importing
      exp_window = window.

endform.                               " CHECK_COUNTRYSPECS
*&---------------------------------------------------------------------*
*&      Form  SETCU_COUNTRY
*&---------------------------------------------------------------------*
*      Simulates PERFORM SETCU-NATIO in payroll function SETCU
*      Calls function module PY<ISO code>_SET_CUMULATION (if existing)
*----------------------------------------------------------------------*
*      -->P_PREVIOUS_RESULT  text
*----------------------------------------------------------------------*
form setcu_country using    p_p_current_result
                            p_p_previous_result.
  clear: crtlog_national[], crterr_national[], setcucountry_subrc.
  if setcu_country_exists = 'X'.
    call function setcu_country_module
      exporting
        log             = natlog
        pernr           = pernr-pernr            "XMS note 581121
      tables
        local_ptext     = crtlog_national
        errortext       = crterr_national
        rgdir           = full_rgdir
      changing
        current_result  = p_p_current_result
        previous_result = p_p_previous_result
        log_titlebar    = titlebar_national
      exceptions
        error_setcu     = 1.
    setcucountry_subrc = sy-subrc.
  endif.

endform.                               " SETCU_COUNTRY
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG_TREE
*&---------------------------------------------------------------------*
*       end of processing: show log tree
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_log_tree.

  data: expand_table like seutexpand occurs 0 with header line.
  data: expand_header like seutexpand.

  data: color_table type stree_ctl_col_mapping_tab.
  data: color_header like line of color_table.

  data: read_index like sy-tabix.

  color_header-color = 6.
  color_header-intensiv = 0.
  color_header-style = treev_style_emphasized_negativ.
  insert color_header into table color_table.

  color_header-color = 3.
  color_header-intensiv = 0.
  color_header-style = treev_style_emphasized.
  insert color_header into table color_table.



  call function 'RS_TREE_CONSTRUCT'
*    EXPORTING
*         INSERT_ID          = '000000'
*         RELATIONSHIP       = ' '
    tables
      nodetab            = globaltree
    exceptions
      tree_failure       = 1
      id_not_found       = 2
      wrong_relationship = 3
      others             = 4.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

*------find errors in tree----------------------------
  loop at globaltree into treeline where tlength9 = 1.
    move-corresponding treeline to expand_header.
    append expand_header to expand_table.
  endloop.
*------expand statistic in batch----------------------
  if ( sy-batch eq 'X' ) or
     ( sy-subty o lv_print ) .  "Note 966251        "WOGL9CK054879
*------Statistic is the last note---------------------           "!
    describe table globaltree lines read_index.                  "!
    do.                                                          "!
      read table globaltree into treeline                        "!
                            index read_index.                    "!
      if treeline-text = text-u00.                               "!
        move-corresponding treeline to expand_header.            "!
        append expand_header to expand_table.                    "!
        exit.                                                    "!
      endif.                                                     "!
      subtract 1 from read_index.                                "!
    enddo.                                                       "!
  endif.                                                         "!
*------expand tree where errors have happened---------
  if not expand_table[] is initial.
    call function 'RS_TREE_SET_CURRENT_LAYOUT'
*       EXPORTING
*            CURSOR_COLUMN       = 3
*            CURSOR_LINE         = 2
*            FIRST_NODE          = 1
*            FIRST_NODE_TYPE     = ' '
*            LIST_COLUMN         = 1
*            LIST_LINE           = 1
*            LAYOUT_MODE         = STREE_LAYOUT_NORMAL
*       IMPORTING
*            INCONSISTENT_LAYOUT =
      tables
        layout = expand_table.
  endif.
  call function 'RS_TREE_CONTROL_PREPARE'
    exporting
*     CONTROL_PATTERN    = STREE_CTL_GENERIC
*     HIERARCHY_HEADER   =
*     INITIAL_HEADER_WIDTH  =
*     LIST_ITEM_HEADER   =
*     MULTIPLE_SELECTION = STREE_FALSE
*     ITEM_SELECTION     = STREE_FALSE
      suppress_node_icon = stree_true
*     suppress_folder_icon  = stree_false
*     CALLBACK_PROGRAM   =
*     CALLBACK_ITEM_DISPLAY =
      color_mapping      = color_table
*     TYPE_MAPPING       =
*      IMPORTING
*     SUBSCREEN_PROGRAM  =
*     SUBSCREEN_DYNNR    =
    exceptions
      not_available      = 1
      others             = 2.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

  call function 'RS_TREE_LIST_DISPLAY'
    exporting
*     status              = 'BASIC'
      callback_program    = 'RPUCRT00'
      callback_gui_status = 'SET_GUI_STATUS'
      use_control         = stree_use_control.

endform.                               " DISPLAY_LOG_TREE

*&--------------------------------------------------------------------*
*&      Form  SET_GUI_STATUS
*&--------------------------------------------------------------------*
form set_gui_status.

  set pf-status 'STATUS_TREE'.
  set titlebar  'OUTPUT'.

endform.                    "SET_GUI_STATUS

*&---------------------------------------------------------------------*
*&      Form  LOGTREE_HEADER
*&---------------------------------------------------------------------*
*       creates the header line of the log tree
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form logtree_header.
  clear treeline.
  treeline-text = 'Wiederaufbau der Kumulationstabellen'(t01).
  treeline-tintensiv = intens_header.
  treeline-tlevel = 1.
  treeline-tlength = strlen( text-t01 ).
  treeline-tcolor = color_header.
  append treeline to globaltree.


endform.                               " LOGTREE_PERNR
*&---------------------------------------------------------------------*
*&      Form  LOGTREE_PERNR
*&---------------------------------------------------------------------*
*       display employee number in the log tree
*----------------------------------------------------------------------*
*      -->P_SUCCESS_PERNR  text
*----------------------------------------------------------------------*
form logtree_pernr.
  clear treeline.
  treeline-text = employee_number.
  treeline-tintensiv = intens_pernr.
  treeline-tlevel = 3.
  treeline-tlength = 8.
  treeline-tcolor = color_pernr.
  if pernr_error is initial.
*    if not crtlog is initial.
    append treeline to tree.
    append lines of treetab to tree.
*    endif.
  else.
    err_pernr = err_pernr + 1.
    treeline-tlength9 = 1.
    append treeline to treeerr.
    append lines of treetab to treeerr.
  endif.
endform.                               " LOGTREE_PERNR
*&---------------------------------------------------------------------*
*&      Form  LOGTREE_SEQNR
*&---------------------------------------------------------------------*
*       displays RGDIR information for the current payroll result
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form logtree_seqnr.


  data: periodinfo(50).
  data: date_help(10).

  if importcurr_subrc ne 0  or importprev_subrc ne 0.
    perform logtree_import_failed.
    exit.
  endif.
*>>> Start of WOW Custom Code
*  check ( crtlog ne space or pernr_error = 'X' ).
*  check ( pernr_error = 'X' ).
*<<< End of WOW Custom Code
  if rgdir_entry-bondt is initial.
    concatenate
    'Fürperiode:'(t05)
    rgdir_entry-fpper+4(2)
    '.'
    rgdir_entry-fpper(4)
    '|'
    'Inperiode'(t11)
    rgdir_entry-inper+4(2)
    '.'
    rgdir_entry-inper(4)
    into periodinfo.
  else.
    write rgdir_entry-bondt dd/mm/yyyy to date_help.
    concatenate
    'Bonuslauf am'(t06)
    date_help
    into periodinfo.
  endif.

  clear treeline.
  concatenate
          periodinfo
          '|'
          'Seqnr:'(t03)
          rgdir_entry-seqnr
          '|'
          'Vor-Seqnr:'(t04)
          prev_rgdirentry-seqnr
          into treeline-text.
  treeline-tintensiv = intens_rgdirinfo.
  treeline-tlevel = 4.
  treeline-tlength = strlen( treeline-text ).
  treeline-tcolor = color_rgdirinfo.
  if export_subrc ne 0 or addcu_subrc ne 0 or setcu_subrc ne 0.
    treeline-tlength9 = 1.
  endif.
  append treeline to treetab.

endform.                               " LOGTREE_SEQNR
*&---------------------------------------------------------------------*
*&      Form  LOGTREE_NORESULTS
*&---------------------------------------------------------------------*
*       is called if no results have been found
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form logtree_noresults.
*>>> Start of WOW Custom Code
*  check not crtlog is initial.
*<<< End of WOW Custom Code
  clear treeline.
  treeline-tlevel = 4.
  treeline-text = 'Keine Abrechnungsergebnisse gefunden'(e05).
  treeline-tlength = strlen( text-e05 ).
  treeline-tcolor = color_warning.
  treeline-tintensiv = intens_warning.
  append treeline to treetab.
endform.                               " LOGTREE_NORESULTS

*&---------------------------------------------------------------------*
*&      Form  LOGTREE_IMPORT_FAILED
*&---------------------------------------------------------------------*
*       is called if the import of payroll results goes wrong
*       --> mark log text red
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form logtree_import_failed.

  data: lv_symsg_dummy type symsg.

  clear treeline.
  treeline-tlevel = 4.
  treeline-tcolor = color_err.
  treeline-tintensiv = intens_err.
  treeline-tlength9 = 1.
  treeline-tlength = 80.
  write: text-t03 to treeline-text,
         rgdir_entry-seqnr to treeline-text+8.
  case importcurr_subrc.
    when 5.                                                 "GWY2471158
      message id sy-msgid type sy-msgty number sy-msgno
         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         into lv_symsg_dummy.
      write lv_symsg_dummy to treeline-text+16.  "not enough authority
    when 6.
      write text-e07 to treeline-text+16.  "result not found
    when 9.
      write text-e12 to treeline-text+16.  "out-of-sequence reversal
    when others.
      if importprev_subrc = 5.
        clear treeline-text.
        message id sy-msgid type sy-msgty number sy-msgno
           with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           into lv_symsg_dummy.
        write lv_symsg_dummy to treeline-text.  "not enough authority

      else.
        write text-e03 to treeline-text+16.  "internal error
      endif.
  endcase.
  append treeline to treetab.
endform.                               " LOGTREE_IMPORT_FAILED
*&---------------------------------------------------------------------*
*&      Form  INIT_PERNR
*&---------------------------------------------------------------------*
*       initializes the PCL2 buffer and the RGDIR buffer tables
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form init_pernr.
  call function 'HR_PCLX_INIT_BUFFER'.
  clear:  prev_rgdirentry.
  clear: relevant_rgdir[],
         full_rgdir[],
         rgdir_entry.
  clear: treetab[].
  clear: pernr_error.
  clear: importcurr_subrc, importprev_subrc.
  clear: addcu_subrc, setcu_subrc.
  clear: export_subrc.
  employee_number = pernr-pernr.
endform.                               " INIT_PERNR
*&---------------------------------------------------------------------*
*&      Form  CONVERT_RESULT
*&---------------------------------------------------------------------*
*       if currencies differ in current and previous result: convert
*       RT, CRT, ARRRS, DDNTK
*----------------------------------------------------------------------*
*      -->P_<CURR_VERSC>_WAERS  text
*      -->P_<PREV_VERSC>_WAERS  text
*      -->P_P_PREVIOUS_RESULT  text
*----------------------------------------------------------------------*
form convert_result using    value(pernr) type p_pernr
                             new_currency type pc202-waers
                             old_currency type pc202-waers
                 changing    p_p_previous_result.
  field-symbols: <prev_rt>    type hrpay99_rt,
                 <prev_crt>   type hrpay99_crt,
                 <prev_arrrs> type hrpay99_arrrs,
                 <prev_ddntk> type hrpay99_ddntk.

  assign component 'INTER-RT'
               of structure p_p_previous_result to <prev_rt>.
  assign component 'INTER-CRT'
               of structure p_p_previous_result to <prev_crt>.
  assign component 'INTER-ARRRS'
               of structure p_p_previous_result to <prev_arrrs>.
  assign component 'INTER-DDNTK'
               of structure p_p_previous_result to <prev_ddntk>.
  call function 'HR_CONVERT_CURRENCY_RESULT'
    exporting
      country_grouping       = molga
      conversion_date        = rgdir_entry-fpbeg
      foreign_currency       = old_currency
      local_currency         = new_currency
      pernr                  = pernr             "GWY839032
    tables
      result_table           = <prev_rt>
      cumulated_result_table = <prev_crt>
*     SUBSEQUENT_TIME_TICKET_TABLE =
      arrears_table          = <prev_arrrs>
      deduction_table        = <prev_ddntk>
    exceptions
      invalid_wagetype       = 1
      error_conversion       = 2
      others                 = 3.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                               " CONVERT_RESULT
*&---------------------------------------------------------------------*
*&      Form  LOGTREE_CU_MISMATCH
*&---------------------------------------------------------------------*
*       is called if there is an inconsistency between the payroll
*       directory and the payroll cluster on PCL2
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form logtree_cu_mismatch.
  clear treeline.
  treeline-tlevel = 4.
  treeline-text =
  'Inkonsistenz zwischen Directory und Abrechnungsergebnissen'(e08).
  treeline-tlength = strlen( text-e08 ).
  treeline-tcolor = color_err.
  treeline-tintensiv = intens_err.
  treeline-text9 = 'X'.
  append treeline to treetab.
  pernr_error = 'X'.
endform.                               " LOGTREE_NORESULTS
*&---------------------------------------------------------------------*
*&      Form  LOGTREE_ALL_PERNR
*&---------------------------------------------------------------------*
*       collects the two trees for successful and unsucccessful
*       employee numbers into one
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form logtree_all_pernr.
*-----all successful employees
  clear treeline.
  treeline-text = 'Erfolgreich umgesetzte Personalnummern'(t02).
  treeline-tintensiv = intens_pernrinfo.
  treeline-tlevel = 2.
  treeline-tlength = strlen( text-t02 ).
  treeline-tcolor = color_pernrinfo.
  append treeline to globaltree.
  append lines of tree to globaltree.
*------and the less lucky ones
  treeline-text = 'Fehlerhafte Personalnummern'(t10).
  treeline-tintensiv = intens_pernrinfo.
  treeline-tlevel = 2.
  treeline-tlength = strlen( text-t02 ).
  treeline-tcolor = color_pernrinfo.
  treeline-tlength9 = 1.
  append treeline to globaltree.
  append lines of treeerr to globaltree.

endform.                               " LOGTREE_ALL_PERNR
*&---------------------------------------------------------------------*
*&      Form  LOGTREE_WRONGMOLGA
*&---------------------------------------------------------------------*
*       if employee number does not fit to country grouping
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form logtree_wrongmolga using wrong_molga.
*>>> Start of WOW Custom Code
*  check not crtlog is initial.
*<<< End of WOW Custom Code
  clear treeline.
  treeline-tlevel = 4.
  treeline-text = text-e11.
  replace '&' with wrong_molga into treeline-text.
  treeline-tlength = strlen( text-e05 ).
  treeline-tcolor = color_err.
  treeline-tintensiv = intens_err.
  append treeline to treetab.
endform.                               " LOGTREE_NORESULTS
*&---------------------------------------------------------------------*
*&      Form  CHECK_MOLGA
*&---------------------------------------------------------------------*
*       check if employee's country assignment is OK
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_molga.

  rp_provide_from_last p0001 space begda '99991231'.

  if p0001-werks ne last_t500p-persa.
    select * from t500p into last_t500p
                        where persa = p0001-werks.
    endselect.
  endif.
  if last_t500p-molga ne molga.
    pernr_error = 'X'.
    perform logtree_wrongmolga using last_t500p-molga.
  endif.

endform.                               " CHECK_MOLGA
*&---------------------------------------------------------------------*
*&      Form  ERRORLOG_PERNR
*&---------------------------------------------------------------------*
*       if an error occurs during processing: log it and reject
*       employee
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form errorlog_pernr using p_current_result.
  pumerr = pumerr + 1.
  pernr_error = 'X'.
  perform logtree_seqnr.

*-----------
  perform logtree_cumulation using p_current_result.
*-----------
  perform logtree_pernr.
  reject.
endform.                               " ERRORLOG_PERNR
*&---------------------------------------------------------------------*
*&      Form  COMMIT_WORK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form commit_work.

  if test eq space.
    commit_counter = commit_counter + 1.
    if commit_counter = cw_trigger.
      clear commit_counter.
      commit work.
    endif.
  endif.
endform.                               " COMMIT_WORK
*&---------------------------------------------------------------------*
*&      Form  assign_payresult
*&---------------------------------------------------------------------*
*       create a data type for the payroll result in question,
*       e.g.: cluster RU -> PAYUS_RESULT
*       and assign this to pointers to: the current result
*                                       the previous result
*                                       a structure for cumul. tables
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assign_payresult.
  data: refvar_curr type ref to data.
  data: refvar_prev type ref to data.
  data: refvar_cumul type ref to data.
  data: t52relid_header type t52relid.
  select single * from t52relid into t52relid_header
                         where relid = cluster_id.
  create data refvar_curr type (t52relid_header-typename).
  create data refvar_prev type (t52relid_header-typename).
  create data refvar_cumul type (t52relid_header-typename).
  assign refvar_curr->* to <payresult_current>.
  assign refvar_prev->* to <payresult_previous>.
  assign refvar_cumul->* to <cumulation_tables>.

endform.                               " assign_payresult

*&---------------------------------------------------------------------*
*&      Form  complete_cumul_for_OOS_YEA                  "note 0433407
*&---------------------------------------------------------------------*
form complete_cumul_for_oos_yea using p_current_result
                                      p_previous_result.
  data: aper_paper like pc2paper.
  aper_paper = rgdir_entry-fpper.
  check molga = '07' or molga = '10'.
  check not rgdir_entry-outofseq is initial.
  field-symbols: <crt>  type hrpay99_crt.
  assign component 'INTER-CRT' of structure p_current_result
                               to <crt>.

  call function 'HRPAYNA_OOS_YEA_COMPLETE_CUMUL'
    exporting
      imp_paydt       = rgdir_entry-paydt
      imp_payty       = rgdir_entry-payty
      imp_payid       = rgdir_entry-payid
      imp_begda       = rgdir_entry-fpbeg
      imp_endda       = rgdir_entry-fpend
      imp_paper       = aper_paper
      imp_molga       = molga
      imp_outofseq    = rgdir_entry-outofseq
      imp_oosdate     = rgdir_entry-oosdate
      payroll_until   = rgdir_entry-oosdate
      imp_pernr       = employee_number
      imp_clstrid     = cluster_id
    tables
      imp_rgdir       = full_rgdir[]
    changing
      cumul_tab_all   = p_current_result
      cumul_tab_all_o = p_previous_result.

endform.                    " complete_cumul_for_OOS_YEA
*>>> Start of WOW Custom Code
*&---------------------------------------------------------------------*
*&      Form  selection_screen_input
*&---------------------------------------------------------------------*
form selection_screen_input.
  data: lv_permo type t549a-permo.

  if yoc eq abap_true.
    perform check_field_filled_w_text using pyty_cal text-z01.
    perform check_field_filled_w_text using bond_cal text-z02.
    p_payty = pyty_cal.
    p_payid = pyid_cal.
    p_bondt = bond_cal.
    p_ipend = bond_cal.
    p_fpbeg = bond_cal.
    p_fpend = bond_cal.
  else.
    perform check_field_filled_w_text using abkr_cal text-z04.
    perform check_field_filled_w_text using abrp_cal text-z05.
    perform check_field_filled_w_text using abrj_cal text-z06.

    perform check_abkrs_pabrp_pabrj using abkr_cal
      changing p_fpbeg p_fpend abrp_cal abrj_cal lv_permo.
    perform check_pnpabkrs_abkrs using lv_permo.

    p_ipend = p_fpend.
  endif.
* Transfer Date Options
  if p_setdt eq abap_true.
    perform check_field_filled_w_text using p_dtadt text-z07.
    perform check_field_filled_w_text using p_dtati text-z08.
  endif.

endform.                    " selection_screen_input
*&---------------------------------------------------------------------*
*&      Form  check_field_filled_w_text
*&---------------------------------------------------------------------*
form check_field_filled_w_text using iv_field iv_msg.
  if iv_field is initial.
    message e208(00) with iv_msg.
  endif.
endform.                    "check_field_filled_w_text
*&---------------------------------------------------------------------*
*&      Form  check_abkrs_pabrp_pabrj
*&---------------------------------------------------------------------*
form check_abkrs_pabrp_pabrj using    value(iv_abkr_cal)
        changing ov_begd_cal ov_endd_cal ov_abrp_cal ov_abrj_cal ov_permo.

  data lv_pnpxabkr like pnpxabkr.
  lv_pnpxabkr = iv_abkr_cal. "the types are different.
  call function 'PA03_PERIODDATES_GET'
    exporting
      f_abkrs               = lv_pnpxabkr
    importing
      f_permo               = ov_permo
      f_current_begda       = ov_begd_cal
      f_current_endda       = ov_endd_cal
    changing
      f_current_period      = ov_abrp_cal
      f_current_year        = ov_abrj_cal
    exceptions
      pcr_does_not_exist    = 1
      abkrs_does_not_exist  = 2
      period_does_not_exist = 3
      others                = 4.
  if sy-subrc ne 0.
    message id sy-msgid type 'E' number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " check_abkrs_pabrp_pabrj
*&---------------------------------------------------------------------*
*&      Form  check_pnpabkrs_abkrs
*&---------------------------------------------------------------------*
form check_pnpabkrs_abkrs using    value(iv_permo_cal).
  data ls_549a type t549a.
  if not pnpabkrs[] is initial.
    select * from t549a into ls_549a where abkrs in pnpabkrs.
      if ls_549a-permo ne iv_permo_cal.
        message e506(3r).
      endif.
    endselect.
  endif.
endform.                    " check_pnpabkrs_abkrs
*---------------------------------------------------------------------*
*       FORM SET_TRANSFER_DATE                                        *
*---------------------------------------------------------------------*
form set_transfer_date using p_current_result.

  clear: addcu_subrc.
  field-symbols: <curr_rt>        type hrpay99_rt,
                 <curr_crt>       type hrpay99_crt,
                 <curr_bt>        type hrpay99_bt,
                 <curr_versc>     type pc202,
                 <crt_upd>        type hrpay99_crt,
                 <cumul_tab_curr> type table,
                 <cumul_tab_upd>  type table.
  data: cumul_tab_name(80).

  data: curr_bt like line of <curr_bt> occurs 0 with header line.
  data: help_aper type pc2aper.
  data: i56c8 type t56c8 occurs 0 with header line.
  assign component 'INTER-BT' of structure p_current_result
                                 to <curr_bt>.

  clear: curr_bt, curr_bt[].
  append lines of <curr_bt> to curr_bt.
  loop at curr_bt.
    if p_clrdt eq abap_true.
      clear: curr_bt-dtadt, curr_bt-dtati.
    else.
      curr_bt-dtadt = p_dtadt.
      curr_bt-dtati = p_dtati.
    endif.
    modify curr_bt.
  endloop.
  refresh: <curr_bt>.
  append lines of curr_bt to <curr_bt>.
  clear: curr_bt, curr_bt[].

endform.
*<<< End of WOW Custom Code
