*----------------------------------------------------------------------*
*   INCLUDE H99CWTR0_FORMS                                             *
*----------------------------------------------------------------------*
*FEL 26.10.2005 note 892422: new checks for empty ABKRS, PABRP, PABRJ
*FELPCOK004125 22.04.2005 ignore reversals
*FELL6DK008023 9.9.2004   improved sorting when comparing periods
*FELL6DK003646 6.7.2004   use pnpabkrs in period selection
*FELL6DK002201 21.06.2004 fix up selection with in-period view, OC B
*FELL6BK060496 25.03.2004 sort when comparing periods (good 4 retroes)
*FELL6BK050334 30.10.2003 new comboboxes: current period / other period
*FELL6BK045660 9.9.2003 existence check does not find ALV user-specific
*                       variants
*---------------------------------------------------------------------*
*       FORM PROCESS_EVP                                              *
*---------------------------------------------------------------------*
form process_evp  using it_evp type hrpy_tt_rgdir
                        value(iv_molga) type molga
                        value(iv_pernr) like pernr-pernr
                        value(iv_relid) like t500l-relid
                        value(ix_ycompper)
                        value(ix_ref_per)
                        value(ix_arc_read).

  data ls_evp like line of it_evp.
  data ls_result type pay99_result.
  data ls_lastwpbp like line of ls_result-inter-wpbp.
  data ls_wpbp like line of ls_result-inter-wpbp.
  data lx_retro type xfeld.
  data: begin of lt_wpbp_dir occurs 30.
          include structure pc261.
          include structure pc205.
        data  end of lt_wpbp_dir.
  data: wpbp_temp like line of ls_result-inter-wpbp.
  statics pnp_in_wpbp type c.
  data:es_t77dct_option type t77dct_option,
       lv_dbcon         type dbcon_name.
  if flg_fetchdcl ne abap_true. "Improvement idea 457
    perform setup_buffer tables it_evp
    using iv_pernr iv_relid ix_arc_read.
  endif.
  if pnp_in_wpbp is initial.           "Static variable
    if      pnpbukrs[] is initial
        and pnpwerks[] is initial
        and pnpbtrtl[] is initial
        and pnppersg[] is initial
        and pnppersk[] is initial
        and pnpkostl[] is initial  "Note 2892422
        and pnpplans[] is initial
        and pnpansvh[] is initial
        and pnporgeh[] is initial
        and pnpstell[] is initial.
      pnp_in_wpbp = 'N'.
    else.
      pnp_in_wpbp = 'X'.
    endif.
  endif.
  if flg_fetchdcl eq abap_true."Improvement idea 457
    if cl_hrdct_switch_check=>hrdct_sfws_sc_01( ) eq abap_on and cl_hrdct_switch_check=>hrdct_sfws_ui_01( ) eq abap_on.
      cl_hrdct_t77dct_option=>read_by_relid( exporting iv_pcltabname = 'PCL2'
        iv_relid          = iv_relid
      importing es_t77dct_option  = es_t77dct_option ).
      lv_dbcon = es_t77dct_option-dbcon.
      refresh :gt_p2rx_rt,gt_p2rx_versc,gt_p2rx_wpbp.
      select * from p2rx_rt into table gt_p2rx_rt for all entries in it_evp where  dct_pernr eq iv_pernr and dct_seqnr eq it_evp-seqnr and lgart in s_lgart.
      select * from p2rx_wpbp into table gt_p2rx_wpbp for all entries in it_evp where  dct_pernr eq iv_pernr and dct_seqnr eq it_evp-seqnr.
      select * from p2rx_versc into table gt_p2rx_versc for all entries in it_evp where  dct_pernr eq iv_pernr and dct_seqnr eq it_evp-seqnr.
    else.
      message text-s32 type 'E'.
    endif.
  endif.
*>>> Start of TYP Enhancement       "MOD001++
  if not gt_tpy_rgdir is initial.
* Read Test Payroll Results
    refresh: gt_tpy_p2rx_rt, gt_tpy_p2rx_versc, gt_tpy_p2rx_wpbp.
    select * from p2rx_rt into table gt_tpy_p2rx_rt for all entries in gt_tpy_rgdir where dct_pernr eq iv_pernr and dct_seqnr eq gt_tpy_rgdir-dct_seqnr and lgart in s_lgart.
    select * from p2rx_wpbp into table gt_tpy_p2rx_wpbp for all entries in gt_tpy_rgdir where  dct_pernr eq iv_pernr and dct_seqnr eq gt_tpy_rgdir-dct_seqnr.
    select * from p2rx_versc into table gt_tpy_p2rx_versc for all entries in gt_tpy_rgdir where  dct_pernr eq iv_pernr and dct_seqnr eq gt_tpy_rgdir-dct_seqnr.
  endif.
*<<< End of TPY Enhancements         "MOD001++
  if pnp_in_wpbp = 'X'.
    if for_view <> 'X'.                "In View
      perform find_original_periods tables it_evp[] lt_wpbp_dir[]
                                    using  iv_pernr iv_relid.
    endif.
  endif.

  loop at it_evp into ls_evp.
    move-corresponding ls_evp to inper.
    move-corresponding ls_evp to fpper.
*>>> Start of TPY Enhancement.     "MOD001++
    read table gt_tpy_rgdir into gs_tpy_rgdir
      with key dct_seqnr = ls_evp-seqnr.
    if sy-subrc eq 0.
      perform fetch_tpy_declustered_results using  iv_pernr iv_relid ls_evp-seqnr
        changing ls_result.
    else.
*<<< End of TPY Enhancement       "MOD001++
      if flg_fetchdcl eq abap_true. "Improvement idea 457
        perform fetch_declustered_results using  iv_pernr iv_relid ls_evp-seqnr
        changing ls_result.
      else.
        call function 'PYXX_READ_PAYROLL_RESULT'
          exporting
            clusterid                    = iv_relid
            employeenumber               = iv_pernr
            sequencenumber               = ls_evp-seqnr
*           READ_ONLY_BUFFER             = ' '
            read_only_international      = 'X'
          changing
            payroll_result               = ls_result
          exceptions
            illegal_isocode_or_clusterid = 1
            error_generating_import      = 2
            import_mismatch_error        = 3
            subpool_dir_full             = 4
            no_read_authority            = 5
            no_record_found              = 6
            versions_do_not_match        = 7
            others                       = 8.
      endif.
*>>> Start of TPY Enhancement.
    endif.                               "MOD001++
*<<< End of TPY Enhancement
    loop at ls_result-inter-wpbp into ls_lastwpbp. endloop.
    loop at ls_result-inter-wpbp[] into ls_wpbp. endloop.

    call function 'CD_RETROCALC_PERIOD'
      exporting
        entry = ls_evp
      importing
        calcd = lx_retro.
    if lx_retro ne 'X' and pnp_in_wpbp = 'X'. "Begin of Note 2807848
*      CLEAR ls_wpbp. "Note 2892422
      loop at ls_result-inter-wpbp[] into ls_wpbp where ( bukrs in pnpbukrs
                                                      and werks in pnpwerks
                                                      and btrtl in pnpbtrtl
                                                      and persg in pnppersg
                                                      and persk in pnppersk
                                                      and kostl in pnpkostl  "Note 2892422
                                                      and plans in pnpplans
                                                      and ansvh in pnpansvh
                                                      and orgeh in pnporgeh
                                                      and stell in pnpstell ).
      endloop.
    endif."End of Note 2807848
    wpbp_temp = ls_wpbp.

    if pnp_in_wpbp = 'X'.
      if for_view <> 'X'.                "In View
        if lx_retro eq true.
          loop at lt_wpbp_dir
            where inper = ls_evp-inper
*              AND iabkrs  = ls_evp-iabkrs
            and iperm   = ls_evp-iperm
            and inpty   = ls_evp-inpty
            and inpid   = ls_evp-inpid
            and ipend   = ls_evp-ipend.
            exit.                          "First occurance (original)
          endloop.
          if sy-subrc = 0.
            move-corresponding lt_wpbp_dir to wpbp_temp.
          endif.
        endif.
      endif.
    endif.

    if pnp_in_wpbp <> 'X' or
        ( wpbp_temp-bukrs in pnpbukrs
      and wpbp_temp-werks in pnpwerks
      and wpbp_temp-btrtl in pnpbtrtl
      and wpbp_temp-persg in pnppersg
      and wpbp_temp-persk in pnppersk
      and wpbp_temp-kostl in pnpkostl   "Note 2892422
      and wpbp_temp-plans in pnpplans
      and wpbp_temp-ansvh in pnpansvh
      and wpbp_temp-orgeh in pnporgeh
      and wpbp_temp-stell in pnpstell ).

      perform process_rt using ls_result-inter-rt[]
                               ls_result-inter-wpbp[]
                               ls_lastwpbp
                               ls_evp-fpend
                               ls_evp-fpper                 "2562469
                               ls_result-inter-versc-waers
*UNNI1414271
                               ls_result-inter-versc-juper
                               iv_molga
                               ls_evp-srtza
                               ix_ycompper ix_ref_per.
    endif.
  endloop.                 "it_evp
endform.                              "process_evp
*---------------------------------------------------------------------*
*       FORM READ_RELID                                               *
*---------------------------------------------------------------------*
form read_relid using value(iv_molga) like t500l-molga
                      ov_relid like t500l-relid.

  tables: t500l.
  select single * from t500l where molga = iv_molga.
  ov_relid = t500l-relid.
endform.                    "read_relid
*---------------------------------------------------------------------*
*       FORM CSORTHIERARCHY_INITFORLGA                                *
*---------------------------------------------------------------------*
form csorthierarchy_initforlga
     changing sorthierarchy type tlga_sorthierarchy.

  data: sortitem type tlga_sortitem.

  clear sorthierarchy.
  sortitem-object = 'pernr'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Personalnummer'(o01).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'pernr'.
  sortitem-attribute = 'pernr'.
  sortitem-longtext = 'Personalnummer'(a01).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'pernr'.
  sortitem-attribute = 'ename'.
  sortitem-longtext = 'Name'(a02).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'pernr'.
  sortitem-attribute = 'sname'.
  sortitem-longtext = 'Sortiername'(a03).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_bukrs'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Buchungskreis'(o02).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_bukrs'.
  sortitem-attribute = 'bukrs'.
  sortitem-longtext = 'Buchungskreis'(a04).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_bukrs'.
  sortitem-attribute = 'bukrs_txt'.
  sortitem-longtext = 'Buchungskreistext'(a05).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persa'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Personalbereich'(o03).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persa'.
  sortitem-attribute = 'persa'.
  sortitem-longtext = 'Personalbereich'(a06).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persa'.
  sortitem-attribute = 'persa_txt'.
  sortitem-longtext = 'Personalbereichstext'(a07).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'btrtl'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Personalteilbereich'(o04).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'btrtl'.
  sortitem-attribute = 'persa'.
  sortitem-longtext = 'Personalbereich'(a08).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'btrtl'.
  sortitem-attribute = 'btrtl'.
  sortitem-longtext = 'Personalteilbereich'(a09).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'btrtl'.
  sortitem-attribute = 'btrtl_txt'.
  sortitem-longtext = 'Personalteilbereichstext'(a10).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kokrs'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Stammkostenrechnungskreis'(o05).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kokrs'.
  sortitem-attribute = 'kokrs'.
  sortitem-longtext = 'Stammkostenrechnungskreis'(a11).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kokrs'.
  sortitem-attribute = 'kokrs_txt'.
  sortitem-longtext = 'Stammkostenrechnungskreistext'(a12).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kostl'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Stammkostenstelle'(o06).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kostl'.
  sortitem-attribute = 'kokrs'.
  sortitem-longtext = 'Stammkostenrechnungskreis'(a13).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kostl'.
  sortitem-attribute = 'kostl'.
  sortitem-longtext = 'Stammkostenstelle'(a14).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kostl'.
  sortitem-attribute = 'kostl_txt'.
  sortitem-longtext = 'Stammkostenstellentext'(a15).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kostl'.
  sortitem-attribute = 'sgmnt'.
  sortitem-longtext = 'Segment für Segmentberichterstattung'(a40).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'wpbp_kostl'.
  sortitem-attribute = 'profit_ctr'.
  sortitem-longtext = 'Profitcenter'(a41).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persg'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Mitarbeitergruppe'(o07).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persg'.
  sortitem-attribute = 'persg'.
  sortitem-longtext = 'Mitarbeitergruppe'(a16).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persg'.
  sortitem-attribute = 'persg_txt'.
  sortitem-longtext = 'Mitarbeitergruppentext'(a17).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persk'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Mitarbeiterkreis'(o08).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persk'.
  sortitem-attribute = 'persk'.
  sortitem-longtext = 'Mitarbeiterkreis'(a18).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'persk'.
  sortitem-attribute = 'persk_txt'.
  sortitem-longtext = 'Mitarbeiterkreistext'(a19).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'lgart'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Lohnart'(o09).
  sortitem-key = ''.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'lgart'.
  sortitem-attribute = 'molga'.
  sortitem-longtext = 'Länderkennzeichen Lohnart'(a20).
  sortitem-key = 'X'.
  sortitem-read_late = ''.
  sortitem-fi_co = ''.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'lgart'.
  sortitem-attribute = 'lgart'.
  sortitem-longtext = 'Lohnart'(a21).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list. " improvement request 499
  sortitem-object = 'lgart'.
  sortitem-attribute = 'apznr'.
  sortitem-longtext = 'Teilt'(s33).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'lgart'.
  sortitem-attribute = 'lgart_txt'.
  sortitem-longtext = 'Lohnartentext'(a22).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = ''.
  sortitem-longtext = 'In-Periode'(o10).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = 'iabkrs'.
  sortitem-longtext = 'Abrechnungskreis In-Periode'(a23).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = 'iabkrs_txt'.
  sortitem-longtext = 'Abrechnungskreistext In-Periode'(a24).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = 'iperm'.
  sortitem-longtext = 'Periodenmodifikator In-Periode'(a25).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = 'iperm_txt'.
  sortitem-longtext = 'Periodenmodifikatortext In-Periode'(a26).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = 'inper'.
  sortitem-longtext = 'In-Periode'(a27).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = 'ipend'.
  sortitem-longtext = 'Endedatum In-Periode'(a28).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = 'inpty'.
  sortitem-longtext = 'Abrechnungstyp In-Periode'(a29).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'inper'.
  sortitem-attribute = 'inpid'.
  sortitem-longtext = 'Abrechnungs-ID In-Periode'(a30).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Für-Periode'(o11).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = 'abkrs'.
  sortitem-longtext = 'Abrechnungskreis Für-Periode'(a31).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = 'abkrs_txt'.
  sortitem-longtext = 'Abrechnungskreistext Für-Periode'(a32).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = 'permo'.
  sortitem-longtext = 'Periodenmodifikatior Für-Periode'(a33).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = 'permo_txt'.
  sortitem-longtext = 'Periodenmodifikatortext Für-Periode'(a34).
  sortitem-key = ' '.
  sortitem-read_late = 'X'.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = 'fpper'.
  sortitem-longtext = 'Für-Periode'(a35).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = 'paydt'.
  sortitem-longtext = 'Zahldatum Für-Periode'(a36).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = 'payty'.
  sortitem-longtext = 'Abrechnungstyp Für-Periode'(a37).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'fpper'.
  sortitem-attribute = 'payid'.
  sortitem-longtext = 'Abrechnungs-ID Für-Periode'(a38).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'pernr'.       "XFE
  sortitem-attribute = 'perid'.
  sortitem-longtext = 'Personal-Identifikationsnummer'(a39).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

*Added Organizational Unit to Object Selection - UNNI975578
  sortitem-object = 'orgeh'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Organisatorische Einheit'(o12).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'orgeh'.
  sortitem-attribute = 'orgeh'.
  sortitem-longtext = 'Organisatorische Einheit'(a42).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'orgeh'.
  sortitem-attribute = 'orgeh_txt'.
  sortitem-longtext = 'Name der organisatorischen Einheit'(a43).
  sortitem-key = ''.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sorthierarchy-validsortitems-number = 12.

*UNNI1414271
  sortitem-object = 'vdsk1'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Organisationsschlüssel'(o13).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'vdsk1'.
  sortitem-attribute = 'vdsk1'.
  sortitem-longtext = 'Organisationsschlüssel'(a44).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 13.
  sortitem-object = 'juper'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Juristische Person'(o14).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'juper'.
  sortitem-attribute = 'juper'.
  sortitem-longtext = 'Juristische Person'(a45).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 14.
  "Request id 484
  sortitem-object = 'sachp'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Administrator'(a47).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'sachp'.            "Note 2779056
  sortitem-attribute = 'sbmod'.
  sortitem-longtext = 'Administrator Group'(a84).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'sachp'.            "Note 2779056
  sortitem-attribute = 'sachp'.
  sortitem-longtext = 'Administrator for HR Master Data'(a83).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'sachp'.
  sortitem-attribute = 'sachp_txt'.
  sortitem-longtext = 'Administrator for HR Master Data'(a83).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'sachp'.           "Note 2779056
  sortitem-attribute = 'sacha'.
  sortitem-longtext = 'Payroll Administrator'(a81).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'sachp'.             "Note 2722292
  sortitem-attribute = 'sacha_txt'.
  sortitem-longtext = 'Payroll Administrator'(a81).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'sachp'.            "Note 2779056
  sortitem-attribute = 'sachz'.
  sortitem-longtext = 'Administrator for Time Recording'(a82).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'sachp'.
  sortitem-attribute = 'sachz_txt'.
  sortitem-longtext = 'Administrator for Time Recording'(a82).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 15.
*************Note 2722292, moving Gender, Date of Birth down as they are not fixed fields and there inclusion in object selection is controlled
*************by switches.

  sortitem-object = 'plans'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Position'(a46).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'plans'.
  sortitem-attribute = 'plans'.
  sortitem-longtext = 'Position'(a46).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'plans'.
  sortitem-attribute = 'plans_txt'.
  sortitem-longtext = 'Position Text'(a50).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 16.

  sortitem-object = 'statusdata'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Customer-Specific Status'(a51).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'statusdata'.
  sortitem-attribute = 'stat1'.
  sortitem-longtext = 'Customer-Specific Status'(a51).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'statusdata'.
  sortitem-attribute = 'stat2'.
  sortitem-longtext = 'Employment Status'(a80).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 17.

  sortitem-object = 'job'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Job'(a53).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'job'.
  sortitem-attribute = 'stell'.
  sortitem-longtext = 'Job'(a53).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'job'.
  sortitem-attribute = 'stell_txt'.
  sortitem-longtext = 'Job Text'(a54).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 18.

  sortitem-object = 'workschedule'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Employee Time Management Status'(a68).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'workschedule'.
  sortitem-attribute = 'zterf'.
  sortitem-longtext = 'Employee Time Management Status'(a68).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'workschedule'.
  sortitem-attribute = 'schkz'.
  sortitem-longtext = 'Work Schedule Rule'(a69).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'workschedule'.
  sortitem-attribute = 'empct'.
  sortitem-longtext = 'Employment percentage'(a70).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 19.

  sortitem-object = 'workingtime'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Daily Working Hours'(a71).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'workingtime'.
  sortitem-attribute = 'arbst'.
  sortitem-longtext = 'Daily Working Hours'(a71).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'workingtime'.
  sortitem-attribute = 'wkwdy'.
  sortitem-longtext = 'Weekly Workdays'(a72).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'workingtime'.
  sortitem-attribute = 'divgv'.
  sortitem-longtext = 'Working Hours per Payroll Period'(a73).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'workingtime'.
  sortitem-attribute = 'bsgrd'.
  sortitem-longtext = 'Capacity Utilization Level'(a74).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 20.

  sortitem-object = 'payscale'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Pay scale type'(a75).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'payscale'.
  sortitem-attribute = 'trfar'.
  sortitem-longtext = 'Pay scale type'(a75).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'payscale'.
  sortitem-attribute = 'tartx'.
  sortitem-longtext = 'Pay scale type'(a75).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'payscale'.
  sortitem-attribute = 'trfgb'.
  sortitem-longtext = 'Pay scale area'(a76).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'payscale'.
  sortitem-attribute = 'tgbtx'.
  sortitem-longtext = 'Pay scale area'(a76).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'payscale'.
  sortitem-attribute = 'trfgr'.
  sortitem-longtext = 'Pay scale group'(a77).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'payscale'.
  sortitem-attribute = 'trfst'.
  sortitem-longtext = 'Pay scale level'(a78).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 21.

  sortitem-object = 'gesch'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Gender'(a79).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'gesch'.
  sortitem-attribute = 'gen_text'.
  sortitem-longtext = 'Gender'(a79).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 22.

  sortitem-object = 'gbdat'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Date of Birth'(a49).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.

  sortitem-object = 'gbdat'.
  sortitem-attribute = 'gbdat'.
  sortitem-longtext = 'Date of Birth'(a49).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sorthierarchy-validsortitems-number = 23.


  sortitem-object = 'address'.
  sortitem-attribute = ''.
  sortitem-longtext = 'Address type'(a59).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'stext'.
  sortitem-longtext = 'Address type'(a59).
  sortitem-key = 'X'.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'stras'.
  sortitem-longtext = 'Street'(a60).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'ort01'.
  sortitem-longtext = 'City'(a61).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'ort02'.
  sortitem-longtext = 'District'(a62).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'pstlz'.
  sortitem-longtext = 'Postal code'(a63).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'locat'.
  sortitem-longtext = 'Second address line'(a66).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'state_txt'.
  sortitem-longtext = 'State'(a67).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'land1'.
  sortitem-longtext = 'Country key'(a64).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.
  sortitem-object = 'address'.
  sortitem-attribute = 'landx'.
  sortitem-longtext = 'Country'(a65).
  sortitem-key = ' '.
  sortitem-read_late = ' '.
  sortitem-fi_co = ' '.
  append sortitem to sorthierarchy-validsortitems-list.


  sorthierarchy-validsortitems-number = 24.
  "End of Request id 484

endform.                    "csorthierarchy_initforlga
*---------------------------------------------------------------------*
*       FORM CSORTHIERARCHY_SELECTSORTITEMS                           *
*---------------------------------------------------------------------*
form csorthierarchy_selectsortitems
       using is_sorthierarchy type tlga_sorthierarchy
       changing ov_h_string type c.

  data: ls_sortitem type tlga_sortitem.
  data: begin of lt_text_symbol_relation occurs 30.
          include structure pnpstringt.
        data: end of lt_text_symbol_relation.

  loop at is_sorthierarchy-validsortitems-list into ls_sortitem
          where attribute is initial.
    unpack sy-tabix to lt_text_symbol_relation-shortstrg(2).
    lt_text_symbol_relation-optiontext = ls_sortitem-longtext.
    append lt_text_symbol_relation.
  endloop.
  call function 'RP_OPTIONS_INTO_STRING'
    exporting
      max_chosen_number          = 85
*     delimiter_sign             = '/'
      text_title                 = text-s04
      text_left                  = text-s05
      text_right                 = text-s06
      status                     = 'ORDER'
    tables
      text_symbol_relation_tab   = lt_text_symbol_relation
    changing
      string_value               = ov_h_string
    exceptions
      table_string_inconsistency = 1
      unknown_status             = 2
      others                     = 3.
  if sy-subrc ne 0."Note 2779056
    message text-204 type 'E'.
  endif.
endform.                    "csorthierarchy_selectsortitems
*---------------------------------------------------------------------*
*       FORM CSORTHIERARCHY_STR2SORTITEMS                             *
*---------------------------------------------------------------------*
form csorthierarchy_str2sortitems using value(iv_h_string) type c
                     changing ios_sorthierarchy type tlga_sorthierarchy.

  data: ls_sortitem type tlga_sortitem.
  data: lv_hierarchystring(255) type c.
  data: lv_sortitemindex type i.
  data: lv_object like ls_sortitem-object.
  data: pa_fields type xfeld.

  lv_hierarchystring = iv_h_string.
  clear ios_sorthierarchy-activesortitems-list.
  translate lv_hierarchystring using '/ '.
  condense lv_hierarchystring no-gaps.
  while lv_hierarchystring(2) ne space.
    pack lv_hierarchystring(2) to lv_sortitemindex.
    read table ios_sorthierarchy-validsortitems-list
               index lv_sortitemindex into ls_sortitem.
    lv_object = ls_sortitem-object.
    loop at ios_sorthierarchy-validsortitems-list into ls_sortitem
            where not attribute is initial
              and object eq lv_object.
      append ls_sortitem to ios_sorthierarchy-activesortitems-list.
      add 1 to ios_sorthierarchy-activesortitems-number.
    endloop.
    shift lv_hierarchystring by 2 places.
  endwhile.
  "Request id 484
*  LOOP AT ios_sorthierarchy-activesortitems-list INTO ls_sortitem WHERE (  object = 'plans' OR object = 'sachp' OR object = 'gesch' OR object = 'gbdat' or object = 'statusdata'  "Note 2779056
*  OR object = 'job' OR object = 'address' OR object = 'workschedule' OR object = 'payscale' ).
*    pa_fields = true.
*    EXIT.
*  ENDLOOP.
*  IF pa_fields = true.
*    READ TABLE ios_sorthierarchy-activesortitems-list WITH KEY object = 'pernr' TRANSPORTING NO FIELDS.
*    IF sy-subrc NE 0.
*      MESSAGE text-096 TYPE 'E'.
*    ENDIF.
*  ENDIF.
  "Request id 484
endform.                    "csorthierarchy_str2sortitems
*---------------------------------------------------------------------*
*       FORM BUILD_DRIVER                                             *
*---------------------------------------------------------------------*
form build_driver using value(iv_template) like sy-repid
                        value(ix_ycompper)
                        is_field_directory type tlga_sorthierarchy
                  changing ov_driver like sy-repid.

  data: lt_code(72)     type c occurs 0 with header line,
        lv_codeline(72) type c,
        ls_sortitem     type tlga_sortitem,
        lv_lkey(25)     type c.

  read report iv_template into lt_code.
  if ix_ycompper eq true.
    loop at lt_code.
      check lt_code(2) eq '*A'.
      lv_codeline = lt_code+2.
      lt_code = lv_codeline.
      modify lt_code.
    endloop.
  else.  "ix_ycompper eq false
    loop at lt_code.
      check lt_code(2) eq '*E'.
      lv_codeline = lt_code+2.
      lt_code = lv_codeline.
      modify lt_code.
    endloop.
  endif.  "ix_ycompper = ?
  loop at lt_code.
    check lt_code(2) eq '*B'.
    lv_codeline = lt_code+2.
    lt_code = lv_codeline.
    modify lt_code.
  endloop.
  loop at is_field_directory-activesortitems-list into ls_sortitem.
    clear lv_codeline.
    concatenate '*D' ls_sortitem-object '.' ls_sortitem-attribute
            into lv_lkey.
    loop at lt_code.
      check lt_code(25) eq lv_lkey(25).
      lv_codeline = lt_code+25.
      lt_code = lv_codeline.
      modify lt_code.
    endloop.
  endloop.
*
** For any changes required before the driver is generated.
** Following example code is for adapting in Enhancement spot to avoid overflows.
*  loop at lt_code.
*    if lt_code+9 = 'betrg like pc207-betrg,'.
*      lt_code+9 = 'betrg like zz207-betrg,'.
*      modify lt_code.
*    endif.
*  endloop.
enhancement-point change_driver_code spots hrpay_cwtr include bound .
*
  if sw-rate = 'X'.
    loop at lt_code.
      if lt_code+9 = 'betpe type BETPE_SUM,'.
        lt_code+9 = 'betpe like h99cwtr_sum-betpe,'.
        modify lt_code.
      endif.
      if lt_code+9 = 'rte_waers type WAERS,'.
        lt_code+9 = 'rte_waers like h99cwtr_sum-rte_waers,'.
        modify lt_code.
      endif.
      if ix_ycompper = 'X'.
        if lt_code+9 = 'betpe_ref type BETPE_REF_SUM,'.
          lt_code+9 =  'betpe_ref like h99cwtr_sum-betpe_ref,'.
          modify lt_code.
        endif.
        if lt_code+9 = 'rte_waers_ref type WAERS_REF,'.
          lt_code+9 =  'rte_waers_ref like h99cwtr_sum-rte_waers_ref,'.
          modify lt_code.
        endif.
      endif.
    endloop.
  endif.
*
  data mm(512) type c.
  data ll type i.
  generate subroutine pool lt_code name ov_driver message mm line ll.
  if sy-subrc ne 0 or ov_driver is initial."Note 2779056
    message text-e09 type 'E'.
  endif.
endform.                    "build_driver
*---------------------------------------------------------------------*
*       FORM READ_KOKRS                                               *
*---------------------------------------------------------------------*
form read_kokrs using value(iv_bukrs)
                      value(iv_gsber)
                      ov_kokrs.
  types: begin of kokrs_st,
           bukrs type bukrs,
           gsber type gsber,
           kokrs type kokrs,
         end of kokrs_st.
  statics: s_kokrs  type kokrs_st,
           st_kokrs type hashed table of kokrs_st
              with unique key bukrs gsber.

  clear ov_kokrs.                                           "2395671
  read table st_kokrs into s_kokrs with key bukrs = iv_bukrs
                                            gsber = iv_gsber.
  if sy-subrc = 0.
    ov_kokrs = s_kokrs-kokrs.
    exit.
  endif.

  call function 'HRCA_CONTROLLINGAREA_FIND'
    exporting
      companycode  = iv_bukrs
      businessarea = iv_gsber
    importing
      contrlarea   = ov_kokrs
    exceptions
      not_found    = 1
      others       = 2.

  if sy-subrc = 0.
    clear s_kokrs.
    s_kokrs-bukrs = iv_bukrs.
    s_kokrs-gsber = iv_gsber.
    s_kokrs-kokrs = ov_kokrs.
    insert s_kokrs into table st_kokrs.
  endif.
endform.                    "read_kokrs
*&---------------------------------------------------------------------*
*&      Form  params_show_hide
*&---------------------------------------------------------------------*
form params_show_hide using value(iv_triggering_param)
                                 value(iv_screen_group).
*this form displays fields with MODIF ID = IV_SCREEN_GROUP
*if IV_TRIGGERING_PARAM eq TRUE and hides them otherwise.
  data lx_active like screen-active. "(1) type n.
  data lx_invisible like screen-active. "(1) type n.

  if iv_triggering_param eq true.
    lx_active = 1.
    lx_invisible = 0.
  else.
    lx_active = 0.
    lx_invisible = 1.
  endif.

  loop at screen.
    if screen-group1 eq iv_screen_group.
      screen-invisible = lx_invisible.
      screen-active = lx_active.
      modify screen.
    endif.
  endloop.
endform.                    " params_show_hide
*&---------------------------------------------------------------------*
*&      Form  params_gray_in_out
*&---------------------------------------------------------------------*
form params_gray_in_out using value(iv_triggering_param)
                                 value(iv_screen_group).
*this form activates fields with MODIF ID = IV_SCREEN_GROUP
*if IV_TRIGGERING_PARAM eq TRUE and greys them out otherwise.
  data lx_input like screen-input.

  if iv_triggering_param eq true.
    lx_input = 1.
  else.
    lx_input = 0.
  endif.

  loop at screen.
    if screen-group1 eq iv_screen_group.
      screen-input = lx_input.
      modify screen.
    endif.
  endloop.
endform.                    " params_gray_in_out
*&---------------------------------------------------------------------*
*&      Form  start_of_selection
*&---------------------------------------------------------------------*
form start_of_selection using value(ix_ypernodt)
                              value(iv_p_h_strg)
                     changing os_field_directory type tlga_sorthierarchy
                              iox_ycompper
                              ov_driver
                              ov_ip_beg ov_ip_end ov_fp_beg ov_fp_end
                              ov_ip_beg_ref ov_ip_end_ref
                              ov_fp_beg_ref ov_fp_end_ref.

  data lt_specified_objects(20) type c occurs 0 with header line.
*comparisons may only take place with the period view.
  if ix_ypernodt eq false.
    iox_ycompper = false.
  endif.
*initialise dates
  perform init_beg_end using pn-begda pn-endda
               changing ov_ip_beg ov_ip_end ov_fp_beg ov_fp_end.
  if iox_ycompper eq true.
    perform init_beg_end using begd_ref endd_ref
                 changing ov_ip_beg_ref ov_ip_end_ref
                             ov_fp_beg_ref ov_fp_end_ref.
  endif.
*get field hierarchy.
  perform csorthierarchy_str2sortitems using iv_p_h_strg
                                       changing os_field_directory.
*excel is not allowed in batch sessions.
  if excel eq true and not sy-batch is initial.
    excel = false. lv = true.
  endif.

* Delete duplicate non-key fields (sname, ename, texts).
  field-symbols: <wa_list> type tlga_sortitem.
  data: lt_list type standard table of tlga_sortitem.
  loop at os_field_directory-activesortitems-list assigning <wa_list> where key eq false.
    read table lt_list with key attribute = <wa_list>-attribute transporting no fields.
    if sy-subrc = 0.
      delete os_field_directory-activesortitems-list.
    else.
      append <wa_list> to lt_list.
    endif.
  endloop.

*generate code (...)
  if excel eq true.
    perform build_driver using c_excel_report_template iox_ycompper
                               os_field_directory
                         changing ov_driver.
  elseif lv eq true.
    perform build_driver using c_alv_report_template iox_ycompper
                               os_field_directory
                         changing ov_driver.
  elseif grid eq true.
    perform build_driver using c_grid_report_template iox_ycompper
                               os_field_directory
                         changing ov_driver.
  endif.
*the following is for the cluster-reading buffering
  lt_specified_objects = 'INTER-VERSC'.
  append lt_specified_objects.
  lt_specified_objects = 'INTER-RT'.
  append lt_specified_objects.
  lt_specified_objects = 'INTER-WPBP'.
  append lt_specified_objects.
  call function 'PYXX_SPECIFY_OBJECTS'
    tables
      specified_objects = lt_specified_objects.
*refresh error list (for the programme log)
  call function 'HR_REFRESH_ERROR_LIST'.
endform.                    " start_of_selection
*&---------------------------------------------------------------------*
*&      Form  core_proc
*&---------------------------------------------------------------------*
form core_proc using value(iv_ip_beg) type d value(iv_ip_end) type d
                     value(iv_fp_beg) type d value(iv_fp_end) type d
                     value(iv_payty) value(iv_payid) value(iv_bondt)
                     value(it_s_pyty_cal) type tt_s_pyty
                     value(ix_ycompper)
                     value(ix_arc_read)
                     value(ix_ref_per)
                     value(ix_yoc)
                     value(ix_ypernodt)
                     value(ix_for_view).

  data lt_tweaked_evp type hrpy_tt_rgdir.
  data lt_evp_related_records type hrpy_tt_rgdir.
  data lt_rgdir type hrpy_tt_rgdir.
  data lt_inper_directory type tt_inper_directory.
  data ls_inper_directory_entry type ts_inper_directory_entry.
  data lv_molga like t500l-molga.
  data lv_evp_lines_number type i.
  data lv_relid like t500l-relid.
  data lt_eval_tab  type pay_t_eval_period.

  field-symbols <eval_wa> type pay_eval_period.

  if ix_ref_per eq false.
    perform pernr_counter using c_update c_selected changing idle.
  endif.
  rp_provide_from_last p0001 space iv_ip_beg iv_ip_end.
  rp_provide_from_last p0002 space iv_ip_beg iv_ip_end.
  rp_provide_from_last p0006 space iv_ip_beg iv_ip_end."Note 2779056
  pernr_common-pernr = pernr-pernr.
  pernr_common-sname = p0001-sname.
  pernr_common-perid = p0002-perid. "XFE
* get lt_rgdir
*>>> Start of TPY Enhancements
  data: lv_tpy_data type boolean.                               "MOD001++
*  perform get_rgdir using pernr-pernr ix_ref_per               "MOD001--
*                    changing lv_molga lt_rgdir[].              "MOD001--
  refresh: gt_tpy_rgdir.                                        "MOD001++
  perform get_rgdir using pernr-pernr ix_ref_per                "MOD001++
                    changing lv_molga lv_tpy_data lt_rgdir[].   "MOD001++
  if lv_tpy_data eq abap_true.                                  "MOD001++
    pernr_common-perid = c_text_tpy_results.                    "MOD001++
  endif.                                                        "MOD001++
*<<< End of TPY Enhancements
  call function 'RP_EDIT_NAME'
    exporting
      format    = $$format
      langu     = sy-langu
      molga     = lv_molga
      pp0002    = p0002
    importing
      edit_name = pernr_common-ename.
*here I reduce LT_RGDIR to improve performance.
  perform reduce_rgdir using iv_fp_beg iv_fp_end ix_for_view
    changing lt_rgdir[].
*here I build a catalogue of record keys, which I will feed
*to CD_EVALUATION_PERIODS. Such keys consist of in-period relevant
*fields only.
  perform fill_inper_dir using ix_ypernodt ix_yoc ix_for_view
      iv_ip_beg iv_ip_end iv_payty iv_payid iv_bondt lt_rgdir[]
      it_s_pyty_cal[]
    changing lt_inper_directory[].
* Here I fill table LT_EVP, which is a particular view of RGDIR.
* Every period calculated or overwritten in each selected period
* is listed, and its in-period relevant fields are set equal to
* those of the period when the calculation took place.
* SRTZA is also changed accordingly.
* Some records will appear duplicated with respect to the ordinary
* RGDIR. Periods that were eventually overwritten will appear
* once with their originary in-period data and once with the
* in-period data of the recalculation period(s).
  loop at lt_inper_directory into ls_inper_directory_entry.
    clear lt_evp_related_records. refresh lt_evp_related_records.
    lt_eval_tab = cl_hr_cd_manager=>eval_periods(
                      imp_inpty      = ls_inper_directory_entry-inpty
                      imp_inper      = ls_inper_directory_entry-inper
                      imp_iperm      = ls_inper_directory_entry-iperm
                      imp_iabkrs     = pnpabkrs[]
                      imp_bondt      = ls_inper_directory_entry-ipend
                      imp_inpid      = ls_inper_directory_entry-inpid
                      imp_rgdir      = lt_rgdir
                      imp_all_of_run = false ).
    loop at lt_eval_tab assigning <eval_wa>.
      append lines of <eval_wa>-evp to lt_evp_related_records.
    endloop.
*    call function 'CD_EVALUATION_PERIODS'
*      exporting
*        bonus_date         = ls_inper_directory_entry-ipend
*        inper_modif        = ls_inper_directory_entry-iperm
*        inper              = ls_inper_directory_entry-inper
*        pay_type           = ls_inper_directory_entry-inpty
*        pay_ident          = ls_inper_directory_entry-inpid
*        all_results_of_run = false      "required for off-cycle type 'S'
*      tables
*        rgdir              = lt_rgdir
*        evpdir             = lt_evp_related_records
*      exceptions
*        no_record_found    = 1
*        others             = 2.
*    if sy-subrc eq 2. exit. endif.
    if sy-subrc eq 0.
      perform filter_tweaked_rgdir using ix_ypernodt ix_yoc ix_for_view
          iv_fp_beg iv_fp_end iv_payty iv_payid iv_bondt
          it_s_pyty_cal[] ls_inper_directory_entry
        changing lt_evp_related_records[] lt_tweaked_evp[].
    endif.
  endloop.
* at this point all periods to process are in LT_EVP: go ahead
  describe table lt_tweaked_evp lines lv_evp_lines_number.
  if lv_evp_lines_number gt 0.
* sort for sensible comparisons
    if ix_ycompper eq true.
      if ix_for_view eq true.  "for-view
        sort lt_tweaked_evp by ipend ascending seqnr ascending.
      else.  "in-view
        sort lt_tweaked_evp by fpend descending seqnr ascending.
      endif.
    endif.
    perform read_relid using lv_molga lv_relid.
    perform process_evp   using lt_tweaked_evp[] lv_molga
        pernr-pernr lv_relid ix_ycompper ix_ref_per ix_arc_read.
  endif.
  perform procman_pernr_succ_completed using pernr-pernr.
endform.                              "core_proc
*&---------------------------------------------------------------------*
*&      Form  end_of_selection
*&---------------------------------------------------------------------*
form end_of_selection using value(iv_driver) like sy-repid
                value(iv_excel_template)
                value(is_alv_template) like disvariant
                value(is_grid_template) like disvariant
                value(ix_excel) value(ix_lv) value(ix_grid).
  data lv_current_prog like sy-repid.
  lv_current_prog = sy-repid.
  perform fill_pnp_error_list.
  perform stick_counters_into_error_list.
  perform sort_error_list.
  if ix_excel eq true.
    perform init_excel in program (iv_driver) using iv_excel_template.
    call screen 2000.
    perform exit_excel in program (iv_driver).
  elseif ix_lv eq true.
    perform output in program (iv_driver)
                     using iv_driver is_alv_template lv_current_prog.
  elseif ix_grid eq true.
    perform output in program (iv_driver)
                     using iv_driver is_grid_template lv_current_prog.
  endif.
  call function 'HRPY_PROCESS_FIRE_EVENT'           "process model
    exporting
      imp_parcel = pyparaid.
  leave list-processing.
endform.                    " end_of_selection
*&---------------------------------------------------------------------*
*&      Form  get_alv_variant
*&---------------------------------------------------------------------*
form get_alv_variant changing ov_template type disvariant
                              ov_temp_var like disvariant-variant.
  data: lv_rc type c.

  ov_template-report = sy-repid.
  ov_template-username = sy-uname.
  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = ov_template
      i_save        = 'A'
    importing
      e_exit        = lv_rc
      es_variant    = ov_template
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.
  if lv_rc is initial.           "dialog not cancelled
    ov_temp_var = ov_template-variant.
  endif.

endform.                    " get_alv_variant
*&---------------------------------------------------------------------*
*&      Form  procman_pernr_succ_completed
*&---------------------------------------------------------------------*
form procman_pernr_succ_completed using    value(iv_pernr) type p_pernr.
* process manager: pernr successfully completed
  call function 'HRPY_PROCESS_SET_PERNR_STATUS'
    exporting
      imp_pernr   = iv_pernr
      imp_parcel  = pyparaid
      imp_set_suc = true
    exceptions
      others      = 0.
endform.                    " procman_pernr_succ_completed
*&---------------------------------------------------------------------*
*&      Form  init_beg_end
*&---------------------------------------------------------------------*
form init_beg_end using value(pn_begda) type d value(pn_endda) type d
                  changing ip_beg type d ip_end type d
                           fp_beg type d fp_end type d.
  ip_beg = pn_begda.
  fp_beg = pn_begda.
  ip_end = pn_endda.
  fp_end = pn_endda.
  if pn_begda is initial.
    ip_beg = low-date. "it was '19000101'. ####### new
    fp_beg = low-date. "it was '19000101'. ####### new
  endif.
  if pn_endda is initial.
    ip_end = high-date.
    fp_end = high-date.
  endif.

  if in_view eq false.       "for-period view
    ip_end = high-date.
  else.                      "in-period view.
    fp_beg = low-date.
  endif.
endform.                    " init_beg_end
*&---------------------------------------------------------------------*
*&      Form  selection_screen_output
*&---------------------------------------------------------------------*
form selection_screen_output using value(ix_ypernodt)
                                   value(ix_hide_oc)
                                   value(ix_yoc)
                                   value(iv_begcalsh)
                                   value(iv_begrefsh).

  data lx_ask_4_dates type boolean.
  data lx_show_begcalsh type boolean.
  data lx_show_begrefsh type boolean.
  data lx_ypernodt_and_acc type boolean.
  data lx_show_begcalsh_and_acc type boolean.
  data lx_show_begrefsh_and_acc type boolean.
  data ls_gsval type gsval. "Improvement idea 457
*show/hide or activate/grey out fields as required
  if ix_ypernodt eq true.
    lx_ask_4_dates = false.
  else.
    lx_ask_4_dates = true.
  endif.
  if ix_yoc eq false and ix_ypernodt eq true
          and not iv_begcalsh is initial.
    lx_show_begcalsh = true.
  endif.
  if ix_yoc eq false and ix_ypernodt eq true
          and not iv_begrefsh is initial.
    lx_show_begrefsh = true.
  endif.
  if ix_ypernodt eq true and gx_acc_mode_on eq true.
    lx_ypernodt_and_acc = true.
  else.
    lx_ypernodt_and_acc = false.
  endif.
  if lx_show_begcalsh eq true and gx_acc_mode_on eq true.
    lx_show_begcalsh_and_acc = true.
  else.
    lx_show_begcalsh_and_acc = false.
  endif.
  if lx_show_begrefsh eq true and gx_acc_mode_on eq true.
    lx_show_begrefsh_and_acc = true.
  else.
    lx_show_begrefsh_and_acc = false.
  endif.

  perform params_gray_in_out using false 'PE3'.
  perform params_gray_in_out using false 'PE4'.
  perform params_gray_in_out using false 'PE5'.
  perform params_show_hide using lx_ask_4_dates 'DA1'.
  perform params_show_hide using ix_ypernodt 'PE1'.
  perform params_show_hide using lx_ypernodt_and_acc 'PA1'.
  perform params_show_hide using lx_ask_4_dates 'DA2'.
  perform params_show_hide using ix_ypernodt 'PE2'.
  perform params_show_hide using lx_ypernodt_and_acc 'PA2'.
  perform params_show_hide using ix_ypernodt 'PE3'.
  perform params_show_hide using lx_show_begcalsh 'PE4'.
  perform params_show_hide using lx_show_begcalsh_and_acc 'PA4'.
  perform params_show_hide using lx_show_begrefsh 'PE5'.
  perform params_show_hide using lx_show_begrefsh_and_acc 'PA5'.
  if ix_hide_oc eq true.
    perform params_show_hide using false 'DA2'.
    perform params_show_hide using false 'PE2'.
    perform params_show_hide using false 'PA2'.
    perform params_show_hide using false 'PE3'.
  endif.
*update captions on period/timespan frame and its button
  if ix_ypernodt eq true.
    label01 = text-dat.
    label04 = text-per.
  else.
    label01 = text-per.
    label04 = text-dat.
  endif.
  if sy-cprog ne 'H99CWTR0'.
    loop at screen.
      if ( screen-group1 eq 'DCL' or screen-group1 eq 'SPL' ).
        screen-invisible = '1'.
        screen-active = '0'.
        modify screen.
      endif.
    endloop.
  endif.
*>>> Start of TPY Enhancements
  perform params_gray_in_out using false 'TPY'.
*<<< End of TPY Enhancements
endform.                    " selection_screen_output
*&---------------------------------------------------------------------*
*&      Form  selection_screen_input
*&---------------------------------------------------------------------*
form selection_screen_input
   using value(iv_field_directory) value(iv_pyty_cal) value(iv_pyid_cal)
         value(iv_pyid_ref)
         value(ix_yoc)
         value(iv_abkr_cal)
         value(ix_curr_per_cal)
         value(ix_curr_per_ref)
   changing iox_ypernodt ox_ycompper ov_p_h_strg ov_abkr_ref
         ov_begd_cal ov_endd_cal ov_abrp_cal ov_abrj_cal ov_bond_cal
         ov_begd_ref ov_endd_ref ov_abrp_ref ov_abrj_ref ov_bond_ref
         ov_pyty_ref
         ov_begcalsh ov_endcalsh ov_begrefsh ov_endrefsh
         ov_alv_template type disvariant ov_lvtemp
         ov_grid_template type disvariant ov_grtemp
         ov_excel_template ov_extemp.
  data lv_permo_cal like pn-permo.
  data lv_permo_ref like pn-permo.
  data: lt_field_directory type tlga_sorthierarchy, "Note 2779056
        ls_sortitem        type tlga_sortitem,
        pa_fields          type xfeld.
* For Process Model Begin "Note 2394856
  if pyparaid is not initial.
    if pnpxabkr is not initial.
      abkr_cal = pnpxabkr.
    endif.
*   off-cycle run
    if bondt is not initial.
      bond_cal = bondt.
      pyid_cal = payid.
      pyty_cal = payty.
      yoc = 'X'.
      noc = space.
    else.
*     regular payroll
      if ( pnppabrp is not initial  and pnppabrj is not initial ).
        abrp_cal = pnppabrp.
        abrj_cal = pnppabrj.
      elseif ( pnpdispp is not initial and pnpdispj is not initial ).
        abrp_cal = pnpdispp.
        abrj_cal = pnpdispj.
      endif.
      yoc = space.
      noc = 'X'.
    endif.
    if pnptimr9 eq 'X'.
      pcurperc = 'X'.       " current period
    else.
      pcurperc = space.     " other period
    endif.
  endif.
* For Process Model End
  perform clear_error_message.
*check *_cal fields
  if iox_ypernodt eq true.
    if ix_yoc eq true.
      perform check_field_filled_w_text using iv_pyty_cal text-e01.
      perform check_field_filled_w_text using ov_bond_cal text-e02.
      perform check_bondt changing ov_bond_cal.
      ov_begd_cal = ov_bond_cal.
      ov_endd_cal = ov_bond_cal.
      pn-begda = ov_bond_cal.
      pn-endda = ov_bond_cal.
    else.
*UNNI1164011 - To remove the error message while toggling betweeen periods
*      perform check_field_filled_w_text using iv_abkr_cal text-e04.
*      if ix_curr_per_cal eq true.
*        clear ov_abrp_cal. clear ov_abrj_cal.
*      else.
*        perform check_field_filled_w_text using ov_abrp_cal text-e05.
*        perform check_field_filled_w_text using ov_abrj_cal text-e06.
*      endif.
      if sscrfields-ucomm = c_perordat.
        if ix_curr_per_cal eq true.
          clear ov_abrp_cal. clear ov_abrj_cal.
        endif.
      else.
        perform check_field_filled_w_text using iv_abkr_cal text-e04.
        if ix_curr_per_cal eq true.
          clear ov_abrp_cal. clear ov_abrj_cal.
        else.
          perform check_field_filled_w_text using ov_abrp_cal text-e05.
          perform check_field_filled_w_text using ov_abrj_cal text-e06.
        endif.
      endif.
*End of Changes UNNI1164011
      perform check_abkrs_pabrp_pabrj using iv_abkr_cal
        changing ov_begd_cal ov_endd_cal ov_abrp_cal ov_abrj_cal
                 lv_permo_cal.
      perform check_pnpabkrs_abkrs using lv_permo_cal.
      pn-begda = ov_begd_cal.
      pn-endda = ov_endd_cal.
      ov_begcalsh = ov_begd_cal.
      ov_endcalsh = ov_endd_cal.
    endif.
  else.             "iox_ypernodt = false.
    perform check_begda_endda changing ov_begd_cal ov_endd_cal.
    pn-begda = ov_begd_cal.
    pn-endda = ov_endd_cal.
  endif.            "iox_ypernodt = true ?.
*UNNI- 1088551
  pn-begps = pn-begda.
  pn-endps = pn-endda.

*check *_ref fields
  if ox_ycompper eq true and iox_ypernodt eq true.
    "skip check if period comparison is OFF.
    "mind that comparisons are only allowed with the period view.
    if ix_yoc eq true.
      ov_pyty_ref = iv_pyty_cal.
      perform check_field_filled_w_text using ov_bond_ref text-e03.
      perform check_bondt changing ov_bond_ref.
      ov_begd_ref = ov_bond_ref.
      ov_endd_ref = ov_bond_ref.
    else.    "ix_yoc eq false
      if ov_abkr_ref is initial. ov_abkr_ref = iv_abkr_cal. endif.
      if ix_curr_per_ref eq true.
        clear ov_abrp_ref. clear ov_abrj_ref.
      else.
        perform check_field_filled_w_text using ov_abrp_ref text-e07.
        perform check_field_filled_w_text using ov_abrj_ref text-e08.
      endif.
      perform check_abkrs_pabrp_pabrj using ov_abkr_ref
          changing ov_begd_ref ov_endd_ref ov_abrp_ref ov_abrj_ref
                   lv_permo_ref.
      if lv_permo_ref ne lv_permo_cal.
        message e506(3r).
      endif.
      ov_begrefsh = ov_begd_ref.
      ov_endrefsh = ov_endd_ref.
    endif.  "ix_yoc = ?
  endif.      "ix_ycompper eq true and iox_ypernodt eq true.
*check alv variants
  perform check_alv_variant using ov_lvtemp changing ov_alv_template.
  perform check_alv_variant using ov_grtemp changing ov_grid_template.
*check if excel template exists
  perform check_xlt_file using ov_extemp changing ov_excel_template.
*If I made it to here, the input is all right.
*execute user command
  case sscrfields-ucomm.
    when c_fieldselect.
      perform csorthierarchy_selectsortitems using iv_field_directory
                                             changing ov_p_h_strg.
    when c_perordat.
      if iox_ypernodt eq false.
        iox_ypernodt = true.
      else.
        iox_ypernodt = false.
      endif.
    when others.
  endcase.
  if ov_p_h_strg is initial.
    message e507(3r).
  endif.
*********When pernr not selected in object selection but pa fields are present "Note 2779056
  lt_field_directory = iv_field_directory.
  perform csorthierarchy_str2sortitems using ov_p_h_strg changing lt_field_directory.
  loop at lt_field_directory-activesortitems-list into ls_sortitem where (  object = 'plans' or object = 'sachp' or object = 'gesch' or object = 'gbdat' or object = 'statusdata'
  or object = 'job' or object = 'address' or object = 'workschedule' or object = 'payscale' ).
    pa_fields = true.
    exit.
  endloop.
  if pa_fields = true.
    read table lt_field_directory-activesortitems-list with key object = 'pernr' transporting no fields.
    if sy-subrc ne 0.
      message text-096 type 'E'.
    endif.
  endif.
endform.                    " selection_screen_input
*&---------------------------------------------------------------------*
*&      Form  clear_error_message
*&---------------------------------------------------------------------*
form clear_error_message.
  clear: sy-msgno,
         sy-msgty,
         sy-msgid,
         sy-msgv1,
         sy-msgv2,
         sy-msgv3,
         sy-msgv4.
endform.                    " clear_error_message
*&---------------------------------------------------------------------*
*&      Form  check_begda_endda
*&---------------------------------------------------------------------*
form check_begda_endda changing ov_begda ov_endda.
  data lv_rc like sy-subrc.
  if ov_begda ne c_null_date and ov_begda lt low-date.
    ov_begda = low-date.
  endif.
  if ov_endda ne c_null_date and ov_endda lt ov_begda.
    ov_endda = ov_begda.
  endif.
  if ov_begda gt ov_endda.
    sy-msgno = '003'. sy-msgty = 'E'. sy-msgid = 'PN'.
    sy-msgv1 = ov_begda. sy-msgv2 = ov_endda.
    lv_rc = 1.
  endif.
  if lv_rc ne 0.
    message id sy-msgid type 'E' number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " check_begda_endda
*&---------------------------------------------------------------------*
*&      Form  check_bondt
*&---------------------------------------------------------------------*
form check_bondt changing ov_bondt.
  if ov_bondt ne c_null_date and ov_bondt lt low-date.
    ov_bondt = low-date.
  endif.
endform.                    " check_bondt
*&---------------------------------------------------------------------*
*&      Form  check_field_filled
*&---------------------------------------------------------------------*
* obsolete, still here for compatibility with ADD-ON systems.
form check_field_filled using iv_field.
  if iv_field is initial.
    message e055(00).
  endif.
endform.                    "check_field_filled
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
        changing ov_begd_cal ov_endd_cal ov_abrp_cal ov_abrj_cal
                 ov_permo like pn-permo.
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
*&      Form  check_alv_variant
*&---------------------------------------------------------------------*
form check_alv_variant using value(iv_p_temp)
                       changing ov_template type disvariant.
  ov_template-report = sy-repid.
  ov_template-variant = iv_p_temp.
  check not iv_p_temp is initial.
  call function 'REUSE_ALV_VARIANT_EXISTENCE'
    changing
      cs_variant = ov_template
    exceptions
      not_found  = 1
      others     = 2.
  if sy-subrc <> 0.
    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = 'A'
      changing
        cs_variant = ov_template
      exceptions
        not_found  = 1
        others     = 2.
    if sy-subrc <> 0.
      message id sy-msgid type 'E' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.
endform.                    " check_alv_variant
*&---------------------------------------------------------------------*
*&      Form  check_xlt_file
*&---------------------------------------------------------------------*
form check_xlt_file using value(iv_p_extemp) changing ov_excel_template.
  data lx_file_exists type boolean.
  data lv_file_name type string.

  check sy-batch eq false.       "a check in bckgr would be pointless
  "because no screens are brought up, so no excel sheets either.
  concatenate 'file://' iv_p_extemp into ov_excel_template.
  check not iv_p_extemp is initial.
  lv_file_name = iv_p_extemp.
  call method cl_gui_frontend_services=>file_exist
    exporting
      file            = lv_file_name
    receiving
      result          = lx_file_exists
    exceptions
      cntl_error      = 1
      error_no_gui    = 2
      wrong_parameter = 3
      others          = 4.
  if sy-subrc <> 0 or lx_file_exists eq false.
    message e003(pc).
  endif.
endform.                    " check_xlt_file
*&---------------------------------------------------------------------*
*&      Form  setup_buffer
*&---------------------------------------------------------------------*
form setup_buffer tables   it_evp structure pc261
            using value(iv_pernr) value(iv_relid) value(ix_arc_read).
  data ls_evp like line of it_evp.
  data lv_prev_arc_group like ls_evp-arc_group.

** clear the buffer - > Moved to beginning of pernr processing. Perf issue with comparision
*  call function 'HR_PCLX_INIT_BUFFER'.

* append archived results into buffer, for every arc_group
  if ix_arc_read = true.
    loop at it_evp into ls_evp where not arc_group is initial.
      if ls_evp-arc_group ne lv_prev_arc_group.
        lv_prev_arc_group = ls_evp-arc_group.
        call function 'HR_IMPORT_BUFFER_FROM_ARCHIVE'
          exporting
            persnr                = iv_pernr
            arc_group             = ls_evp-arc_group
          exceptions
            error_reading_archive = 1
            others                = 2.
        if sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        endif.
      endif. "if ls_evp-arc_group ne lv_prev_arc_group.
    endloop.
  endif. "if ix_arc_read = true.
* append non-archived results into buffer
  call function 'HR_IMPORT_RGDIR_FROM_PCLX'
    exporting
      employee_number   = iv_pernr
      cluster_id        = iv_relid
    tables
      import_rgdir      = it_evp
    exceptions
      no_results        = 1
      no_read_authority = 2
      others            = 3.
endform.                    " setup_buffer
*&---------------------------------------------------------------------*
*&      Form  popup_error_list
*&---------------------------------------------------------------------*
form popup_error_list using value(iv_ucomm) like sy-ucomm
                            rs_selfield type slis_selfield.
  if iv_ucomm eq '&ERL'.
    call function 'HR_DISPLAY_ERROR_LIST'
      exceptions
        invalid_linesize = 1
        others           = 2.
  endif.
endform.                    "popup_error_list
*&---------------------------------------------------------------------*
*&      Form  alv_status_set
*&---------------------------------------------------------------------*
form alv_status_set using rt_extab type slis_t_extab.
*UNNI -1095294 - excluding rt_extab so that ony allowed buttons come in the output
*  SET PF-STATUS '3000'.
  set pf-status '3000' excluding rt_extab.
endform.                    "alv_status_set
*&---------------------------------------------------------------------*
*&      Form  fill_pnp_error_list
*&---------------------------------------------------------------------*
form fill_pnp_error_list.
* Get error messages from PNP (missing authorization or PERNR locked)
  data lt_pnp_error type table of hrerror.
  data ls_pnp_error type hrerror.
  call function 'HR_GET_ERRORTAB_FROM_PNP_INFO'
    tables
      errortab = lt_pnp_error.
  loop at lt_pnp_error into ls_pnp_error.
* PERNR must not be initial for sorting purposes
    if ls_pnp_error-pernr is initial.
      ls_pnp_error-pernr = c_null_pernr.
    endif.
    call function 'HR_APPEND_ERROR_LIST'
      exporting
        pernr  = ls_pnp_error-pernr
        arbgb  = ls_pnp_error-arbgb
        msgty  = ls_pnp_error-msgty
        msgno  = ls_pnp_error-msgno
        msgv1  = ls_pnp_error-msgv1
        msgv2  = ls_pnp_error-msgv2
        msgv3  = ls_pnp_error-msgv3
        msgv4  = ls_pnp_error-msgv4
      exceptions
        others = 1.
  endloop.
endform.                    " fill_pnp_error_list
*&---------------------------------------------------------------------*
*&      Form  stick_counters_into_error_list
*&---------------------------------------------------------------------*
form stick_counters_into_error_list.
  data lv_rej type i.
  data lv_sel type i.
  data lv_proc type i.
  perform pernr_counter using c_read c_selected changing lv_sel.
  perform pernr_counter using c_read c_processed changing lv_proc.
  perform pernr_counter using c_read c_rejected changing lv_rej.

  call function 'HR_APPEND_ERROR_LIST'
    exporting
      pernr  = c_null_pernr
      arbgb  = '3R'
      msgty  = 'I'
      msgno  = 503
      msgv1  = lv_sel
    exceptions
      others = 1.
  call function 'HR_APPEND_ERROR_LIST'
    exporting
      pernr  = c_null_pernr
      arbgb  = '3R'
      msgty  = 'I'
      msgno  = 504
      msgv1  = lv_proc
    exceptions
      others = 1.
  call function 'HR_APPEND_ERROR_LIST'
    exporting
      pernr  = c_null_pernr
      arbgb  = '3R'
      msgty  = 'I'
      msgno  = 505
      msgv1  = lv_rej
    exceptions
      others = 1.
endform.                    " stick_counters_into_error_list
*&---------------------------------------------------------------------*
*&      Form  pernr_counter
*&---------------------------------------------------------------------*
form pernr_counter using value(iv_action) value(iv_sel_proc_err)
                   changing ov_out_cnt.
  statics ss_counters type st_counters.
  field-symbols <cnt> type i.

  assign component iv_sel_proc_err of structure ss_counters to <cnt>.
  if iv_action eq c_update.
    add 1 to <cnt>.
  elseif iv_action eq c_read.
    ov_out_cnt = <cnt>.
  endif.
endform.                    " pernr_counter
*&---------------------------------------------------------------------*
*&      Form  sort_error_list
*&---------------------------------------------------------------------*
form sort_error_list.
  data ls_errortab type hrerror.
  data lt_errortab type table of hrerror.

  call function 'HR_GET_ERROR_LIST'
    exporting
      use_abap_memory = 'X'
    tables
      error           = lt_errortab
    exceptions
      no_errors       = 1
      others          = 2.
  call function 'HR_REFRESH_ERROR_LIST'.
  sort lt_errortab stable by pernr ascending.
  loop at lt_errortab into ls_errortab.
    call function 'HR_APPEND_ERROR_LIST'
      exporting
        pernr  = ls_errortab-pernr
        arbgb  = ls_errortab-arbgb
        msgty  = ls_errortab-msgty
        msgno  = ls_errortab-msgno
        msgv1  = ls_errortab-msgv1
        msgv2  = ls_errortab-msgv2
        msgv3  = ls_errortab-msgv3
        msgv4  = ls_errortab-msgv4
      exceptions
        others = 1.
  endloop.
endform.                    " sort_error_list
*&---------------------------------------------------------------------*
*&      Form  process_rt
*&---------------------------------------------------------------------*
form process_rt using    it_rt type hrpay99_rt
                         it_wpbp type hrpay99_wpbp
                         is_lastwpbp type pc205
                         value(iv_fpend) type pc261-fpend
                         value(iv_fpper) type pc261-fpper
                         value(iv_waers) type waers
*UNNI1414271
                         value(iv_juper) type juper
                         value(iv_molga) type molga
                         value(iv_srtza) type srtza
                         value(ix_ycompper)
                         value(ix_ref_per).

  data ls_rt type pc207.
  data ls_rt_temp type pc207.
  data ls_wpbp type pc205.
  data ls_wpbp_apznr type pc205. "Note 2819310

  lgart-molga = iv_molga.
  if sw-rate = 'X'.
    sort it_rt by lgart ascending rte_curr descending.
  endif.
  clear: sachp-sachp, sachp-sbmod, sachp-sacha, sachp-sachz, sachp-sachp_txt, sachp-sacha_txt,sachp-sachz_txt, address-landx, address-state_txt, address-stext. "Note 2886550
  read table field_directory-activesortitems-list transporting no fields with key object = 'sachp'.
  if sy-subrc = 0.
    sachp-sbmod = p0001-sbmod.
    if p0001-sachp is not initial .
      sachp-sachp = p0001-sachp."Administrator for HR Master Data
      perform read_sachptext using sachp-sbmod sachp-sachp changing sachp-sachp_txt.
    endif.
    if p0001-sacha is not initial.
      sachp-sacha = p0001-sacha."Administrator for Payroll Administrator
      perform read_sachptext using sachp-sbmod sachp-sacha changing sachp-sacha_txt.
    endif.
    if p0001-sachz is not initial.
      sachp-sachz = p0001-sachz."Administrator for Time recording
      perform read_sachptext using sachp-sbmod sachp-sachz changing sachp-sachz_txt.
    endif.
  endif.
  gbdat-gbdat = p0002-gbdat.
  read table field_directory-activesortitems-list transporting no fields with key object = 'address'.
  if sy-subrc = 0.
    read table p0006 with key anssa = '1'.
    if sy-subrc = 0.
      address-anssa = p0006-anssa.  "Address type
      address-stras = p0006-stras.  "Street
      address-ort01 = p0006-ort01.  "City
      address-ort02 = p0006-ort02.  "District
      address-pstlz = p0006-pstlz.  "Postal code
      address-land1 = p0006-land1.  "Country key
      address-locat = p0006-locat.  "Second address line
      address-state = p0006-state.  "State
      perform read_countrytext using p0006-land1 changing address-landx.
      perform read_statetext using p0006-land1 p0006-state changing address-state_txt.
      perform read_addresstypetext using p0006-anssa changing address-stext.
    endif.
  endif.
*  READ TABLE field_directory-activesortitems-list TRANSPORTING NO FIELDS WITH KEY object = 'plans'. "Note 2848996
*  IF sy-subrc = 0.
*    plans-plans = is_lastwpbp-plans. "Position
*    IF plans-plans IS NOT INITIAL.
*      PERFORM read_positiontext USING plans-plans
*            iv_fpend
*      CHANGING plans-plans_txt."Position text
*    ENDIF.
*  ENDIF.
*  READ TABLE field_directory-activesortitems-list TRANSPORTING NO FIELDS WITH KEY object = 'job'.
*  IF sy-subrc = 0.
*    job-stell = is_lastwpbp-stell. "Job
*    IF job-stell IS NOT INITIAL.
*      PERFORM read_jobtext USING job-stell
*            iv_fpend
*      CHANGING job-stell_txt."Job text
*    ENDIF.
*  ENDIF.  "Note 2848996
  clear gesch-gen_text.
  read table field_directory-activesortitems-list transporting no fields with key object = 'gesch'. "note 2850890
  if sy-subrc = 0.
*    job-stell = is_lastwpbp-gende. "gender
*    IF job-stell IS NOT INITIAL.
    perform read_gendertext using p0002-gesch iv_molga changing gesch-gen_text.
*
*    ENDIF.
  endif.  "note 2850890
  loop at it_rt into ls_rt where lgart in s_lgart.
    lgart-lgart = ls_rt-lgart.
    read table it_wpbp
            with key apznr = ls_rt-apznr into ls_wpbp.
    if sy-subrc ne 0.
      ls_wpbp = is_lastwpbp.
    endif.
*    ls_wpbp = is_lastwpbp. "Note 2886550
*    read table it_wpbp with key apznr = ls_rt-apznr into ls_wpbp_apznr.
*    if sy-subrc ne 0.
*      ls_wpbp_apznr = is_lastwpbp.
*    endif.
*    lgart-apznr       = ls_wpbp_apznr-apznr.
    bukrs-bukrs = ls_wpbp-bukrs.
    lgart-apznr       = ls_wpbp-apznr."improvement request 499
    persa-persa = ls_wpbp-werks.
    btrtl-btrtl_persa = ls_wpbp-werks.
    btrtl-btrtl = ls_wpbp-btrtl.
    perform read_kokrs using ls_wpbp-bukrs ls_wpbp-gsber
                             kokrs-kokrs.
    perform read_profit_centre using kokrs-kokrs ls_wpbp-kostl iv_fpend
                               changing kostl-profit_ctr.
    kostl-kostl_kokrs = kokrs-kokrs.
    kostl-kostl = ls_wpbp-kostl.
    kostl-kdate = iv_fpend.
    kostl-kfpper = iv_fpper.          "Begin of Note 2562469
    clear kostl-kostl_txt.
    if kostl-kostl is not initial. "Note 2683888
      perform read_costcenter_text_2 using kostl-kostl_kokrs
                                           kostl-kostl
                                           kostl-kdate
                                           changing kostl-kostl_txt.  "End of Note 2562469
    endif.
    kostl-sgmnt = ls_wpbp-sgmnt.
    persg-persg = ls_wpbp-persg.
    persk-persk = ls_wpbp-persk.
    orgeh-orgeh = ls_wpbp-orgeh.                            "UNNI975578
*UNNI1414271
    vdsk1 = ls_wpbp-vdsk1.
    juper = iv_juper.
    "Request id 484
*    CLEAR: sachp-sachp_txt, address-landx, address-state_txt. "Note 2886550
*    CLEAR: sachp-sacha_txt,sachp-sachz_txt. "Note 2722292
*    CLEAR: sachp-sachp,sachp-sachp,sachp-sacha,sachp-sachz,sachp-sacha_txt,sachp-sachz_txt. "Note 2779056 "Note 2886550
*    plans-plans = p0001-plans. "Position "Note 2779056  "Commented for Note 2848996
*    plans-plans = ls_wpbp-plans. "Position
*    IF plans-plans IS NOT INITIAL.
*      PERFORM read_positiontext USING plans-plans
*            iv_fpend
*      CHANGING plans-plans_txt."Position text
*    ENDIF.
*    sachp-sachp = p0001-sachp."Administrator for HR Master Data
*    sachp-sbmod = p0001-sbmod.
*    PERFORM read_sachptext USING sachp-sbmod sachp-sachp CHANGING sachp-sachp_txt.
*    sachp-sacha = p0001-sacha."Administrator for Payroll Administrator                  "Note 2722292
*    PERFORM read_sachptext USING sachp-sbmod sachp-sacha CHANGING sachp-sacha_txt.      "Note 2779056
*    sachp-sachz = p0001-sachz."Administrator for Time recording
*    PERFORM read_sachptext USING sachp-sbmod sachp-sachz CHANGING sachp-sachz_txt.      "Note 2779056
*    gesch-gen_text = ."Gender Text
*    PERFORM read_gendertext USING p0002-gesch CHANGING gesch-gen_text.
*    gbdat-gbdat = p0002-gbdat.
*    job-stell = ls_wpbp-stell. "Job  "Commented for Note 2848996
*    IF job-stell IS NOT INITIAL.
*      PERFORM read_jobtext USING job-stell
*            iv_fpend
*      CHANGING job-stell_txt."Position text
*    ENDIF.
    read table field_directory-activesortitems-list transporting no fields with key object = 'plans'. "Note 2886550
    if sy-subrc = 0.
      plans-plans = ls_wpbp-plans. "Position
      if plans-plans is not initial.
        perform read_positiontext using plans-plans
              iv_fpend
        changing plans-plans_txt."Position text
      endif.
    endif.
    read table field_directory-activesortitems-list transporting no fields with key object = 'job'.
    if sy-subrc = 0.
      job-stell = ls_wpbp-stell. "Job
      if job-stell is not initial.
        perform read_jobtext using job-stell
              iv_fpend
        changing job-stell_txt."Job text
      endif.
    endif.  "Note 2886550
    workschedule-zterf = ls_wpbp-zterf. "Employee Time Management Status
    workschedule-schkz = ls_wpbp-schkz. "Work Schedule Rule
    workschedule-empct = ls_wpbp-empct. "Employment percentage

    workingtime-arbst = ls_wpbp-arbst.  "Daily Working Hours
    workingtime-wkwdy = ls_wpbp-wkwdy.  "Weekly Workdays
    workingtime-divgv = ls_wpbp-divgv.  "Working Hours per Payroll Period
    workingtime-bsgrd = ls_wpbp-bsgrd.  "Capacity Utilization Level
    payscale-trfar = ls_wpbp-trfar. "Pay scale type
    payscale-trfgb = ls_wpbp-trfgb. "Pay Scale Area
    payscale-trfgr = ls_wpbp-trfgr. "Pay Scale Group
    payscale-trfst = ls_wpbp-trfst. "Pay Scale Level
    read table field_directory-activesortitems-list transporting no fields with key object = 'payscale'.
    if sy-subrc = 0.
      perform read_payscale_typetext using ls_wpbp-trfar iv_molga changing payscale-tartx.
      perform read_payscale_areatext using ls_wpbp-trfgb iv_molga changing payscale-tgbtx.
    endif.
    statusdata-stat1 = ls_wpbp-stat1. "Customer-Specific Status
    statusdata-stat2 = ls_wpbp-stat2. "Employment Status
*    READ TABLE p0006 WITH KEY anssa = '1'. "Begin of Note 2779056
*    IF sy-subrc = 0.
*    address-anssa = p0006-anssa.  "Address type
*    address-stras = p0006-stras.  "Street
*    address-ort01 = p0006-ort01.  "City
*    address-ort02 = p0006-ort02.  "District
*    address-pstlz = p0006-pstlz.  "Postal code
*    address-land1 = p0006-land1.  "Country key
*    address-locat = p0006-locat.  "Second address line
*    address-state = p0006-state.  "State
*    PERFORM read_countrytext USING p0006-land1 CHANGING address-landx.
*    PERFORM read_statetext USING p0006-land1 p0006-state CHANGING address-state_txt.
*    PERFORM read_addresstypetext USING p0006-anssa CHANGING address-stext.
*    ENDIF.    "End of Note 2779056
    "End of Request id 484.
    if iv_srtza eq 'A'.
      anzhl = ls_rt-anzhl.
      betrg = ls_rt-betrg.
      if sw-rate = 'X'.
        betpe = ls_rt-betpe.
        if ls_rt-rte_curr is not initial.
          rte_waers = ls_rt-rte_curr.
        else.
          read table it_rt with key lgart = ls_rt-lgart
            into ls_rt_temp binary search.
          if sy-subrc = 0 and ls_rt_temp-rte_curr is not initial.
            rte_waers = ls_rt_temp-rte_curr.
          else.
            rte_waers = iv_waers.
          endif.
        endif.
      endif.
    else.
      anzhl = - ls_rt-anzhl.
      betrg = - ls_rt-betrg.
      if sw-rate = 'X'.
        betpe = - ls_rt-betpe.
        if ls_rt-rte_curr is not initial.
          rte_waers = ls_rt-rte_curr.
        else.
          rte_waers = iv_waers.
        endif.
      endif.
    endif.
    waers = iv_waers.
    perform process in program (driver)
            using ix_ycompper ix_ref_per.
  endloop.               "rt

endform.                    " process_rt
*&---------------------------------------------------------------------*
*&      Form  get_xlt_file
*&---------------------------------------------------------------------*
form get_xlt_file changing ov_p_extemp
                         ov_excel_template.

  data lv_window_title type string.
  data lv_file_filter type string.
  data lt_filetable type filetable.
  data ls_filetable type line of filetable.
  data lv_rc like sy-subrc.

  lv_window_title = text-s08.
  concatenate text-s20 text-s21 into lv_file_filter.
  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title            = lv_window_title
      file_filter             = lv_file_filter
      multiselection          = false
    changing
      file_table              = lt_filetable
      rc                      = lv_rc
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      others                  = 4.

  if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  else.
    read table lt_filetable into ls_filetable index 1.
    ov_p_extemp = ls_filetable-filename.
    concatenate 'file://' ov_p_extemp into ov_excel_template.
  endif.
endform.                    " get_xlt_file
*&---------------------------------------------------------------------*
*&      Form  convert_old_variants
*&---------------------------------------------------------------------*
form convert_old_variants changing ox_ypernodt
                                   ov_begd_cal
                                   ov_endd_cal
                                   ov_abkr_cal
                                   ov_abrp_cal
                                   ov_abrj_cal.
*old variants should produce a timespan view, since this has the
*function of the previous programme version. The period view has
*evolved into a new functionality and it is now entirely different.
  if not pnpbegda is initial and not pnpendda is initial.
    ov_begd_cal = pnpbegda.
    ov_endd_cal = pnpendda.
    ox_ypernodt = false.
  elseif not pnpxabkr is initial. "just in case
    ov_abkr_cal = pnpxabkr.
    ov_abrp_cal = pnppabrp.
    ov_abrj_cal = pnppabrj.
    ox_ypernodt = true.
  elseif not pnpbegda is initial or not pnpendda is initial.
    ov_begd_cal = pnpbegda.
    ov_endd_cal = pnpendda.
    ox_ypernodt = false.
  else.   "do nothing: we have done the conversion already.
  endif.
  clear pnpbegda. clear pnpendda.
  clear pnppabrp. clear pnppabrj. clear pnpxabkr.
endform.                    " convert_old_variants
*&---------------------------------------------------------------------*
*&      Form  filter_tweaked_rgdir
*&---------------------------------------------------------------------*
form filter_tweaked_rgdir using
         value(ix_ypernodt)
         value(ix_yoc)
         value(ix_for_view)
         value(iv_fp_beg) type d
         value(iv_fp_end) type d
         value(iv_payty) type pc261-payty
         value(iv_payid) type pc261-payid
         value(iv_bondt) type pc261-bondt
         value(it_s_pyty_cal) type tt_s_pyty
         value(is_inper_directory_entry) type ts_inper_directory_entry
       changing it_rgdir type hrpy_tt_rgdir
         ot_rgdir type hrpy_tt_rgdir.
* see forms FILTER_INPER_DIR and CORE_PROC for explanations.
  data ls_rgdir like line of it_rgdir.

  sort it_rgdir by ipend descending srtza descending seqnr ascending.
  if ix_ypernodt eq true."""""""""""""""""""""""""""""""""""""""""""
    if ix_yoc eq false .
      clear iv_payty. clear iv_payid. iv_bondt = c_null_date.
    endif.
    if ix_for_view eq true.
      loop at it_rgdir into ls_rgdir
          where fpend between iv_fp_beg and iv_fp_end
            and void eq cd_c-void_false
            and bondt eq iv_bondt
            and ( payty eq iv_payty and payid eq iv_payid ).
        if ls_rgdir-srtza ne cd_c-actual.
          move-corresponding is_inper_directory_entry to ls_rgdir.
        endif.
        append ls_rgdir to ot_rgdir.
      endloop.
    else."in-view
      loop at it_rgdir into ls_rgdir
          where void eq cd_c-void_false.
        if ls_rgdir-srtza ne cd_c-actual.
          move-corresponding is_inper_directory_entry to ls_rgdir.
        endif.
        append ls_rgdir to ot_rgdir.
      endloop.
    endif."in-view or for-view
  else.      "ix_ypernodt eq false."""""""""""""""""""""""""""""""""
    if it_s_pyty_cal[] is initial.      "all records
      if ix_for_view eq true.
        loop at it_rgdir into ls_rgdir
            where fpend between iv_fp_beg and iv_fp_end
              and void eq cd_c-void_false.
          if ls_rgdir-srtza ne cd_c-actual.
            move-corresponding is_inper_directory_entry to ls_rgdir.
          endif.
          append ls_rgdir to ot_rgdir.
        endloop.
      else."in-view
        loop at it_rgdir into ls_rgdir
            where void eq cd_c-void_false.
          if ls_rgdir-srtza ne cd_c-actual.
            move-corresponding is_inper_directory_entry to ls_rgdir.
          endif.
          append ls_rgdir to ot_rgdir.
        endloop.
      endif.  "in- or for-view?
    else.                    "only one PAYTY / INPTY
      if ix_for_view eq true.
        loop at it_rgdir into ls_rgdir
            where fpend between iv_fp_beg and iv_fp_end
              and void eq cd_c-void_false
              and payty in it_s_pyty_cal.
          if ls_rgdir-srtza ne cd_c-actual.
            move-corresponding is_inper_directory_entry to ls_rgdir.
          endif.
          append ls_rgdir to ot_rgdir.
        endloop.
      else."in-view
        loop at it_rgdir into ls_rgdir
            where void eq cd_c-void_false.
          if ls_rgdir-srtza ne cd_c-actual.
            move-corresponding is_inper_directory_entry to ls_rgdir.
          endif.
          append ls_rgdir to ot_rgdir.
        endloop.
      endif."in-view or for-view
    endif."IF it_s_pyty_cal[] IS INITIAL .
  endif.    "ix_ypernodt = ?"""""""""""""""""""""""""""""""""""""""""
endform.                    " filter_tweaked_rgdir
*&---------------------------------------------------------------------*
*&      Form  fill_inper_dir
*&---------------------------------------------------------------------*
* In-period relevant fields for the selected calculation periods are
* stored in table LT_INPER_DIRECTORY. Every combination of values
* that appears in the selected calculation periods is listed in it
* and no records are duplicated.
* In-view: the programme must produce all the records (of any payroll
* type and with any payroll ID), which were calculated in the periods,
* which fulfill the selection criteria. Thus, a filter over INPTY and
* INPID is applied here and no more filters are required later.
* For-view: the programme must produce the records, which fulfill the
* selection criteria and were calculated in periods of any payroll type
* and with any payroll ID. Thus, a filter must be applied later. I use
* a filter here as well, only to kick up performance. It doesn't affect
* the results.
form fill_inper_dir using
                  value(ix_ypernodt)
                  value(ix_yoc)
                  value(ix_for_view)
                  value(iv_ip_beg) type d
                  value(iv_ip_end) type d
                  value(iv_payty) type pc261-payty
                  value(iv_payid) type pc261-payid
                  value(iv_bondt) type pc261-bondt
                  it_rgdir type hrpy_tt_rgdir
                  value(it_s_pyty_cal) type tt_s_pyty
         changing ot_inper_directory type tt_inper_directory.

  data ls_rgdir like line of it_rgdir.
  data ls_inper_directory like line of ot_inper_directory.

  if ix_ypernodt eq true."""""""""""""""""""""""""""""""""""""""""""
    if ix_yoc eq false .
      clear iv_payty. clear iv_payid. iv_bondt = c_null_date.
    endif.
    if ix_for_view eq true.
      loop at it_rgdir into ls_rgdir
          where ipend between iv_ip_beg and iv_ip_end
            and void eq cd_c-void_false
            and abkrs in pnpabkrs
            and bondt eq iv_bondt
            and payty eq iv_payty
            and payid eq iv_payid.
        move-corresponding ls_rgdir to ls_inper_directory.
        collect ls_inper_directory into ot_inper_directory.
      endloop.
    else."in-view
      loop at it_rgdir into ls_rgdir
          where ipend between iv_ip_beg and iv_ip_end
            and void eq cd_c-void_false
            and iabkrs in pnpabkrs
*            and bondt eq iv_bondt
            and ( inpty eq iv_payty and inpid eq iv_payid ).
        move-corresponding ls_rgdir to ls_inper_directory.
        collect ls_inper_directory into ot_inper_directory.
      endloop.
    endif."in-view or for-view
  else.      "ix_ypernodt eq false."""""""""""""""""""""""""""""""""
    if ix_for_view eq true.
      loop at it_rgdir into ls_rgdir
          where ipend between iv_ip_beg and iv_ip_end
            and void eq cd_c-void_false
            and abkrs in pnpabkrs
            and payty in it_s_pyty_cal.        "4 per4mance
        move-corresponding ls_rgdir to ls_inper_directory.
        collect ls_inper_directory into ot_inper_directory.
      endloop.
    else."in-view
      loop at it_rgdir into ls_rgdir
          where ipend between iv_ip_beg and iv_ip_end
            and void eq cd_c-void_false
            and iabkrs in pnpabkrs
            and inpty in it_s_pyty_cal.
        move-corresponding ls_rgdir to ls_inper_directory.
        collect ls_inper_directory into ot_inper_directory.
      endloop.
    endif."in-view or for-view
  endif.    "ix_ypernodt = ?"""""""""""""""""""""""""""""""""""""""""
endform.                    " fill_inper_dir
*&---------------------------------------------------------------------*
*&      Form  reduce_rgdir
*&---------------------------------------------------------------------*
*this form excludes records, whose PAYTY/INPTY *surely* don't match.
form reduce_rgdir using
                  value(iv_fp_beg) type d
                  value(iv_fp_end) type d
                  value(ix_for_view)
         changing ot_rgdir type hrpy_tt_rgdir.

*for-period view: delete all records with wrong for-period.
*this reduces the size of OT_RGDIR, thus speeding up the processing.
  if ix_for_view eq true.
    delete ot_rgdir where
*      not ( fpbeg between iv_fp_beg and iv_fp_end ) or
      not ( fpend between iv_fp_beg and iv_fp_end ).
  endif."ix_for_view = ?
endform.                    " reduce_rgdir
*&---------------------------------------------------------------------*
*&      Form  get_rgdir
*&---------------------------------------------------------------------*
form get_rgdir using    value(iv_pernr) like pernr-pernr
                        value(ix_ref_per)
               changing ov_molga type molga
*>>> Start of TPY Enhancements
                        ov_tpy_data type boolean
*End of TPY Enhancements
                        ot_rgdir type hrpy_tt_rgdir.

  statics: sv_pernr like pernr-pernr,
           sv_molga type molga,
           st_rgdir type hrpy_tt_rgdir.
  if sv_pernr = iv_pernr.
    ot_rgdir[] = st_rgdir[].
    ov_molga = sv_molga.
    return.
  endif.
  call function 'CA_CU_READ_RGDIR_NEW'
    exporting
      persnr          = iv_pernr
    importing
      molga           = ov_molga
    tables
      cu_ca_rgdir     = ot_rgdir
    exceptions
      no_record_found = 1
      others          = 2.
*>>> Start of TPY Enhancements
  data ls_rgdir type pc261.
  if ix_ref_per eq abap_false.
    if p_tst_py eq abap_true.
      delete ot_rgdir where ipend between ip_beg and ip_end.
    endif.
* Check Pay results exists for last payroll period
    read table ot_rgdir into ls_rgdir
     with key fpbeg = ip_beg
              fpend = ip_end.
    if sy-subrc <> 0.
* Add Test Payroll Results
      refresh gt_tpy_rgdir.
      select * into corresponding fields of table gt_tpy_rgdir
        from hrdct_tpy_rgdir
       where hrdct_tpy_rgdir~dct_pernr eq iv_pernr
         and hrdct_tpy_rgdir~dct_is_tpy eq 'X'.
      delete gt_tpy_rgdir where ipend not between  ip_beg and ip_end.

      if not gt_tpy_rgdir is initial.
        ov_tpy_data = abap_true.
      endif.
      loop at gt_tpy_rgdir into gs_tpy_rgdir.
        move-corresponding gs_tpy_rgdir to ls_rgdir.
        move gs_tpy_rgdir-dct_seqnr to ls_rgdir-seqnr.
        append ls_rgdir to ot_rgdir.
      endloop.
    endif.
  endif.
*  if sy-subrc <> 0.      " Check using OT_RGDIR lines
  if ot_rgdir is initial.
*<<< End of TPY Enhancements
    clear sv_pernr.
    if ix_ref_per eq false.
      perform pernr_counter using c_update c_rejected changing idle.
    endif.
    perform procman_pernr_succ_completed using pernr-pernr.
    reject.
  else.
    if ix_ref_per eq false.
      perform pernr_counter using c_update c_processed changing idle.
    endif.
    perform filter_rgdir_for_voids changing ot_rgdir.
    perform filter_rgdir_for_osrs changing ot_rgdir.
    sv_pernr = iv_pernr.
    sv_molga = ov_molga.
    st_rgdir[] = ot_rgdir[].
  endif.
endform.                    " get_rgdir
*&---------------------------------------------------------------------*
*&      Form  initialise
*&---------------------------------------------------------------------*
form initialise changing os_field_directory type tlga_sorthierarchy.
  label02 = text-s04. "'Objektauswahl'
  perform csorthierarchy_initforlga changing os_field_directory.
  call function 'GET_ACCESSIBILITY_MODE'
    importing
      accessibility = gx_acc_mode_on
    exceptions
      others        = 1.
  if sy-subrc <> 0.
    gx_acc_mode_on = false.
  endif.
  perform adjustfielddirectory changing os_field_directory.
endform.                    " initialise
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
*&---------------------------------------------------------------------*
*&      Form  FILTER_RGDIR_FOR_OSRS
*&---------------------------------------------------------------------*
form filter_rgdir_for_osrs changing ot_rgdir type hrpy_tt_rgdir.
* delete all reversed periods: no matter if they have been
* printed out on pay statements or not, they must not come into
* sight here coz they would make trouble with the retro deltas.
  data ls_rgdir_1 type line of hrpy_tt_rgdir.
  data lx_retro type boole_d.
  data wa_ot_rgdir type line of hrpy_tt_rgdir.

  loop at ot_rgdir into ls_rgdir_1.
* loop at rgdir and bring up the original periods, which are reversed
    call function 'CD_RETROCALC_PERIOD'
      exporting
        entry = ls_rgdir_1
      importing
        calcd = lx_retro.
    if lx_retro eq false.
      if ls_rgdir_1-reversal ne pycdc_reversal_false.
*Check if in-period is reveresed with a period already selected
        loop at ot_rgdir into wa_ot_rgdir
                            where abkrs = ls_rgdir_1-abkrs
                            and fpper  = ls_rgdir_1-fpper
                            and payty  = ls_rgdir_1-payty
                            and payid  = ls_rgdir_1-payid
                            and seqnr <> ls_rgdir_1-seqnr
                            and bondt  = ls_rgdir_1-bondt
                            and ipend between
                                 ip_beg and ip_end
                            and ( voidd > ls_rgdir_1-rundt or
                                voidd = ls_rgdir_1-rundt
                                and voidt > ls_rgdir_1-runtm ).
          exit.
        endloop.
        if sy-subrc = 0.
*     delete the reversed record and the retroes done in it
          delete ot_rgdir where ipend = ls_rgdir_1-ipend
                          and iperm = ls_rgdir_1-iperm
                          and inpty = ls_rgdir_1-inpty
                          and inpid = ls_rgdir_1-inpid.  "bondt ?
        endif.    "found original of reverse?
      endif.      "reversed ?
    endif.    "original period ?
  endloop.

endform.      "FILTER_RGDIR_FOR_OSRS
*&---------------------------------------------------------------------*
*&      Form  FILTER_RGDIR_FOR_VOIDS
*&---------------------------------------------------------------------*
form filter_rgdir_for_voids changing ot_rgdir type hrpy_tt_rgdir.
  delete ot_rgdir where void ne pycdc_void_false.
endform.      "FILTER_RGDIR_FOR_VOIDS
*&---------------------------------------------------------------------*
*&      Form  read_profit_centre
*&---------------------------------------------------------------------*
form read_profit_centre  using    value(iv_kokrs_kokrs)
                                  value(iv_kostl)
                                  value(iv_fpend)
                         changing ov_profit_ctr.
  types: begin of cctr_st,
           kokrs type kokrs,
           kostl type kostl,
           endda type datum,
           prctr type prctr,
         end of cctr_st.
  statics: s_cctr  type cctr_st,
           st_cctr type hashed table of cctr_st
              with unique key kokrs kostl endda.

  clear ov_profit_ctr.                                      "2395671
  read table st_cctr into s_cctr with key kokrs = iv_kokrs_kokrs
                                          kostl = iv_kostl
                                          endda = iv_fpend.
  if sy-subrc = 0.
    ov_profit_ctr = s_cctr-prctr.
    exit.
  endif.

  call function 'HRCA_COSTCENTER_GETDETAIL'
    exporting
      controllingarea = iv_kokrs_kokrs
      costcenter      = iv_kostl
      read_date       = iv_fpend
    importing
      profit_ctr      = ov_profit_ctr
    exceptions
      others          = 0.

  if sy-subrc = 0.
    clear s_cctr.
    s_cctr-kokrs = iv_kokrs_kokrs.
    s_cctr-kostl = iv_kostl.
    s_cctr-endda = iv_fpend.
    s_cctr-prctr = ov_profit_ctr.
    insert s_cctr into table st_cctr.
  endif.
endform.                    " read_profit_centre
*&---------------------------------------------------------------------*
*&      Form  replace_objselstrg
*&---------------------------------------------------------------------*
form replace_objselstrg  changing p_p_h_strg.

  replace '22' with '24' into p_p_h_strg.
  replace '25' with '27' into p_p_h_strg.
  replace '28' with '30' into p_p_h_strg.
  replace '32' with '34' into p_p_h_strg.
  replace '41' with '43' into p_p_h_strg.
  replace '43' with '44' into p_p_h_strg. "improvement request 499
  replace '34' with '35' into p_p_h_strg. "Note 2722292
  replace '53' with '54' into p_p_h_strg.
  replace '56' with '57' into p_p_h_strg.
  replace '58' with '59' into p_p_h_strg.
  replace '63' with '69' into p_p_h_strg. "Note 2807848

endform.                    " replace_objselstrg
*&---------------------------------------------------------------------*
*&      Form  find_original_periods
*&---------------------------------------------------------------------*
form find_original_periods tables it_evp type hrpy_tt_rgdir
                                  lt_wpbp_dir
                           using
                                  value(iv_pernr) like pernr-pernr
                                  value(iv_relid) like t500l-relid.

  data ls_evp like line of it_evp.
  data ls_result type pay99_result.
  data ls_wpbp like line of ls_result-inter-wpbp.
  data lx_retro type xfeld.
  data:ls_p2rx_wpbp like line of gt_p2rx_wpbp."Improvement idea 457
  loop at it_evp into ls_evp.

    call function 'CD_RETROCALC_PERIOD'
      exporting
        entry = ls_evp
      importing
        calcd = lx_retro.

    if lx_retro <> true.           "Originals
      move-corresponding ls_evp to inper.
      move-corresponding ls_evp to fpper.
      if flg_fetchdcl eq abap_true. "Improvement idea 457
        loop at gt_p2rx_wpbp into ls_p2rx_wpbp where dct_seqnr = ls_evp-seqnr.
        endloop.
        move-corresponding ls_p2rx_wpbp to ls_wpbp.
        move-corresponding ls_evp to lt_wpbp_dir.
        move-corresponding ls_wpbp to lt_wpbp_dir.
        append lt_wpbp_dir.
      else.
        call function 'PYXX_READ_PAYROLL_RESULT'
          exporting
            clusterid                    = iv_relid
            employeenumber               = iv_pernr
            sequencenumber               = ls_evp-seqnr
*           READ_ONLY_BUFFER             = ' '
            read_only_international      = 'X'
          changing
            payroll_result               = ls_result
          exceptions
            illegal_isocode_or_clusterid = 1
            error_generating_import      = 2
            import_mismatch_error        = 3
            subpool_dir_full             = 4
            no_read_authority            = 5
            no_record_found              = 6
            versions_do_not_match        = 7
            others                       = 8.
      endif.
      loop at ls_result-inter-wpbp[] into ls_wpbp. endloop.

      move-corresponding ls_evp to lt_wpbp_dir.
      move-corresponding ls_wpbp to lt_wpbp_dir.
      append lt_wpbp_dir.
    endif.

  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  FETCH_DECLUSTERED_RESULTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_PERNR  text
*      -->P_IV_RELID  text
*      -->P_LS_EVP_SEQNR  text
*      <--P_LS_RESULT  text
*----------------------------------------------------------------------*
form fetch_declustered_results  using  value(iv_pernr) like pernr-pernr
                                       value(iv_relid) like t500l-relid
                                       value(iv_seqnr) type cdseq
                                       changing  lv_result type pay99_result.
  data: lt_p2rx_rt    type table of p2rx_rt,
        lt_p2rx_wpbp  type table of p2rx_wpbp,
        ls_p2rx_rt    type p2rx_rt,
        ls_p2rx_wpbp  type p2rx_wpbp,
        ls_p2rx_versc type p2rx_versc.

  refresh: lt_p2rx_rt,lt_p2rx_wpbp,lv_result-inter-wpbp,lv_result-inter-rt.
  clear:ls_p2rx_versc, lv_result-inter-versc.
  loop at gt_p2rx_rt into ls_p2rx_rt where dct_seqnr = iv_seqnr.
    append ls_p2rx_rt to lt_p2rx_rt.
  endloop.
  loop at gt_p2rx_wpbp into ls_p2rx_wpbp where dct_seqnr = iv_seqnr.
    append ls_p2rx_wpbp to lt_p2rx_wpbp.
  endloop.
  read table gt_p2rx_versc into ls_p2rx_versc with key dct_seqnr = iv_seqnr.
  if ( ( lt_p2rx_rt is not initial ) and ( lt_p2rx_wpbp is not initial ) and ( ls_p2rx_versc is not initial ) ).
    move-corresponding  lt_p2rx_rt to lv_result-inter-rt.
    move-corresponding  lt_p2rx_wpbp to lv_result-inter-wpbp.
    move-corresponding  ls_p2rx_versc to lv_result-inter-versc.
  endif.


endform.
*>>> Start of TPY Enhancement
*&---------------------------------------------------------------------*
*&      Form  FETCH_TPY_DECLUSTERED_RESULTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_PERNR  text
*      -->P_IV_RELID  text
*      -->P_LS_EVP_SEQNR  text
*      <--P_LS_RESULT  text
*----------------------------------------------------------------------*
form fetch_tpy_declustered_results  using  value(iv_pernr) like pernr-pernr
                                           value(iv_relid) like t500l-relid
                                           value(iv_seqnr) type cdseq
                                           changing  lv_result type pay99_result.
  data: lt_tpy_p2rx_rt    type table of p2rx_rt,
        lt_tpy_p2rx_wpbp  type table of p2rx_wpbp,
        ls_tpy_p2rx_rt    type p2rx_rt,
        ls_tpy_p2rx_wpbp  type p2rx_wpbp,
        ls_tpy_p2rx_versc type p2rx_versc.

  refresh: lt_tpy_p2rx_rt,lt_tpy_p2rx_wpbp, lv_result-inter-wpbp,lv_result-inter-rt.
  clear: ls_tpy_p2rx_versc, lv_result-inter-versc.
  loop at gt_tpy_p2rx_rt into ls_tpy_p2rx_rt where dct_seqnr = iv_seqnr.
    append ls_tpy_p2rx_rt to lt_tpy_p2rx_rt.
  endloop.
  loop at gt_tpy_p2rx_wpbp into ls_tpy_p2rx_wpbp where dct_seqnr = iv_seqnr.
    append ls_tpy_p2rx_wpbp to lt_tpy_p2rx_wpbp.
  endloop.
  read table gt_tpy_p2rx_versc into ls_tpy_p2rx_versc with key dct_seqnr = iv_seqnr.
  if ( ( lt_tpy_p2rx_rt is not initial ) and ( lt_tpy_p2rx_wpbp is not initial ) and ( ls_tpy_p2rx_versc is not initial ) ).
    move-corresponding  lt_tpy_p2rx_rt to lv_result-inter-rt.
    move-corresponding  lt_tpy_p2rx_wpbp to lv_result-inter-wpbp.
    move-corresponding  ls_tpy_p2rx_versc to lv_result-inter-versc.
  endif.

endform.
*<<< End of TPY Enhancement
*&                                                                     *
*&      Form  READ_POSITIONTEXT
*&                                                                     *
*       text
*                                                                      *
*        >IV_PLANS  text
*        >IV_FPEND  text
*      <  PLANS_TXT  text
*                                                                      *
form read_positiontext  using  iv_plans
                               iv_fpend
                        changing plans_txt.
  types: begin of pos_st,
           plans type plans,
           fpend type datum,
           plstx type plstx,
         end of pos_st.
  statics: s_pos  type pos_st,
           st_pos type hashed table of pos_st with unique key plans fpend.
*  DATA: s_t582t TYPE T528T.
  data: lv_plstx type plstx.

  clear :plans_txt,s_pos,lv_plstx.
  read table st_pos into s_pos with key plans = iv_plans
                                        fpend = iv_fpend .
  if s_pos is not initial.
    plans_txt = s_pos-plstx.
    exit.
  endif.

*  SELECT SINGLE plstx FROM T528T INTO s_t582t WHERE SPRSL EQ sy-langu "Note 2848996
  select single plstx from t528t into lv_plstx where sprsl eq sy-langu
  and   otype eq 'S '
  and   plans eq iv_plans
  and   begda le iv_fpend
  and   endda ge iv_fpend.
*  IF SY-SUBRC EQ 0.            "Note 2848996
*    plans_txt = s_t582t-PLSTX.
*    CLEAR s_pos.
*    s_pos-plans = s_t582t-plans.
*    s_pos-fpend = iv_fpend.
*    s_pos-plstx = s_t582t-plstx.
*    INSERT s_pos INTO TABLE st_pos.
*  ELSE.
*    CLEAR plans_txt.
*  ENDIF.

  plans_txt = lv_plstx.
  clear s_pos.
  s_pos-plans = iv_plans.
  s_pos-fpend = iv_fpend.
  s_pos-plstx = lv_plstx.
  insert s_pos into table st_pos.

endform.                    " READ_POSITIONTEXT
*&                                                                     *
*&      Form  READ_GENDERTEXT
*&                                                                     *
*       text
*                                                                      *
*        >IV_GESCH  text
*      <  CV_GEN_TEXT  text
*                                                                      *
form read_gendertext  using    iv_gesch iv_molga

                      changing cv_gen_text.


  types: begin of gender_st,
           gesch   type gesch,
           molga   type molga,
           gentext type hrpad_gender_t,
         end of gender_st.
  statics: s_gender  type gender_st,
           st_gender type table of gender_st.
*  DATA: l_itab  TYPE STANDARD TABLE OF dd07v,
*        l_wa    TYPE dd07v.
  data ls_t77pad_gender_t type t77pad_gender_t.

*  IF st_gender IS INITIAL.
*  CALL FUNCTION 'DD_DOMVALUES_GET'
*  EXPORTING
*    DOMNAME              = 'GESCH'
*    TEXT                 = 'X'
*    LANGU                = sy-langu
*  TABLES
*    DD07V_TAB            = l_itab.
*  IF sy-subrc = 0.
*    LOOP at l_itab into l_wa.
*    CLEAR s_gender.
*    s_gender-gesch = l_wa-domvalue_l.
*    s_gender-gentext =  l_wa-ddtext.
**    INSERT s_gender INTO TABLE st_gender.
*    APPEND s_gender to st_gender.
*    ENDLOOP.
*  ENDIF.
*  ENDIF.
  clear :cv_gen_text,s_gender.
  " READ TABLE st_gender WITH KEY gesch = iv_gesch INTO s_gender.
  " cv_gen_text = s_gender-gentext.
  read table st_gender with key gesch = iv_gesch molga = iv_molga into s_gender.
  if sy-subrc = 0.
    cv_gen_text = s_gender-gentext.
  else.
    call method cl_hr_t77pad_gender_t=>read
      exporting
        iv_molga           = iv_molga
        iv_gender          = iv_gesch
      importing
        es_t77pad_gender_t = ls_t77pad_gender_t.
    if ls_t77pad_gender_t-gender_text is initial.
      call method cl_hr_t77pad_gender_t=>read
        exporting
          iv_molga           = '99'
          iv_gender          = iv_gesch
        importing
          es_t77pad_gender_t = ls_t77pad_gender_t.
    endif.
    cv_gen_text = ls_t77pad_gender_t-gender_text.
    s_gender-gesch = iv_gesch.
    s_gender-molga = iv_molga.
    s_gender-gentext = ls_t77pad_gender_t-gender_text.
    append s_gender to st_gender.
  endif.

endform.                    " READ_GENDERTEXT

*&                                                                     *
*&      Form  READ_SACHPTEXT
*&                                                                     *
*       text
*                                                                      *
*        >IV_SBMOD  text
*        >IV__SACHP  text
*      <  SACHP_TXT  text
*                                                                      *
form read_sachptext  using   iv_sbmod
                             iv_sachp
                   changing sachp_txt.
  types: begin of sachp_st,
           sbmod     type sbmod,
           sachp     type sachp,
           sachp_txt type sachn,
         end of sachp_st.
  statics: s_sachp  type sachp_st,
           st_sachp type hashed table of sachp_st with unique key sbmod sachp.
  data: s_t526 type t526.

  clear :sachp_txt,s_sachp.
  read table st_sachp into s_sachp with key sbmod = iv_sbmod
                                            sachp = iv_sachp.
  if sy-subrc = 0.
    sachp_txt = s_sachp-sachp_txt.
    exit.
  endif.
  select single * from t526 into s_t526 where werks eq iv_sbmod
                                           and sachx eq iv_sachp.
  if sy-subrc eq 0.
    sachp_txt = s_t526-sachn.
    clear s_sachp.
    s_sachp-sbmod = s_t526-werks.
    s_sachp-sachp = s_t526-sachx.
    s_sachp-sachp_txt = s_t526-sachn.
    insert s_sachp into table st_sachp.
  else.
    clear sachp_txt.
  endif.

endform.                    " READ_SACHPTEXT
*&                                                                     *
*&      Form  READ_JOBTEXT
*&                                                                     *
*       text
*                                                                      *
*        >iv_stell  text
*        >IV_FPEND  text
*      <  STELL_TXT  text
*                                                                      *
form read_jobtext  using    iv_stell
                            iv_fpend
                   changing stell_txt.
  types: begin of job_st,
           stell type stell,
           fpend type datum,
           stltx type stltx,
         end of job_st.
  statics: s_job  type job_st,
           st_job type hashed table of job_st with unique key stell fpend.
*  DATA: s_t513s TYPE T513S.
  data: lv_stltx type stltx.
  clear :stell_txt,s_job, lv_stltx.
  read table st_job into s_job with key stell = iv_stell
                                        fpend = iv_fpend .
  if s_job is not initial.
    stell_txt = s_job-stltx.
    exit.
  endif.

*  SELECT SINGLE * FROM T513S INTO s_t513s WHERE SPRSL EQ sy-langu "Note 2848996
  select single stltx from t513s into lv_stltx where sprsl eq sy-langu
                                             and  stell eq iv_stell
                                             and  begda le iv_fpend
                                             and  endda ge iv_fpend.
*  IF SY-SUBRC EQ 0.     "Note 2848996
*    stell_txt = s_t513s-stltx.
*    CLEAR s_job.
*    s_job-stell = s_t513s-stell.
*    s_job-fpend = iv_fpend.
*    s_job-stltx = s_t513s-stltx.
*    INSERT s_job INTO TABLE st_job.
*  ELSE.
*    CLEAR stell_txt.
*  ENDIF.

  stell_txt = lv_stltx.
  clear s_job.
  s_job-stell = iv_stell.
  s_job-fpend = iv_fpend.
  s_job-stltx = lv_stltx.
  insert s_job into table st_job.

endform.                    " READ_JOBTEXT
*&---------------------------------------------------------------------*
*&      Form  ADJUSTFIELDDIRECTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--OV_FIELD_DIRECTORY  text
*----------------------------------------------------------------------*
form adjustfielddirectory  changing ov_field_directory.
  data: wa_list           type tlga_sortitem,
        lt_fielddirectory type tlga_sorthierarchy,
        lt_sortitems      type tlga_sortitemlist.

  ranges: s_object for wa_list-object.
enhancement-point set_object_selection spots hrpay_cwtr include bound .

  if sw-dob is initial.
    s_object-sign = 'I'.
    s_object-option = 'EQ'.
    s_object-low = 'gbdat'.
    append s_object.
  endif.

  if sw-address is initial.
    s_object-sign = 'I'.
    s_object-option = 'EQ'.
    s_object-low = 'address'.
    append s_object.
  endif.

  if sw-gender is initial.
    s_object-sign = 'I'.
    s_object-option = 'EQ'.
    s_object-low = 'gesch'.
    append s_object.
  endif.

  if s_object[] is not initial.
    clear:  lt_fielddirectory, lt_sortitems.
    lt_fielddirectory = ov_field_directory.
    lt_sortitems = lt_fielddirectory-validsortitems.

    delete lt_sortitems-list where ( object in  s_object ).
    lt_fielddirectory-validsortitems = lt_sortitems.
    ov_field_directory = lt_fielddirectory.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  READ_COUNTRYTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->iv_land1 text
*      <--cv_landx  text
*----------------------------------------------------------------------*
form read_countrytext  using   iv_land1
                       changing cv_landx.
  types: begin of country_st,
           land1 type land1,
           landx type landx,
         end of country_st.
  statics: s_country  type country_st,
           st_country type hashed table of country_st with unique key land1.
  data: s_t005t type t005t.

  clear :cv_landx,s_country,s_t005t.
  read table st_country into s_country with key land1 = iv_land1.
  if s_country is not initial.
    cv_landx = s_country-landx.
    exit.
  endif.
  select single * from t005t into s_t005t where spras eq sy-langu
                                           and  land1 eq iv_land1.
  if sy-subrc eq 0.
    cv_landx = s_t005t-landx.
  else.
    clear cv_landx. "Note 2886550
  endif.
  clear s_country.
*  MOVE-CORRESPONDING s_t005t TO s_country.
  s_country-land1 = iv_land1.
  s_country-landx = cv_landx.
  insert s_country into table st_country.

endform.
*&---------------------------------------------------------------------*
*&      Form  READ_STATETEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->iv_land1  text
*      -->iv_state text
*      <--cv_state_txt  text
*----------------------------------------------------------------------*
form read_statetext  using   iv_land1
                             iv_state
                   changing cv_state_txt.
  types: begin of state_st,
           land1     type land1,
           state     type state,
           state_txt type bezei,
         end of state_st.
  statics: s_state  type state_st,
           st_state type hashed table of state_st with unique key land1 state.
  data: s_t005u type t005u.

  clear :cv_state_txt,s_state, s_t005u.
  read table st_state into s_state with key land1 = iv_land1 state = iv_state.
  if s_state is not initial.
    cv_state_txt = s_state-state_txt.
    exit.
  endif.

  select single * from t005u into s_t005u where spras eq sy-langu
                                           and  land1 eq iv_land1
                                           and  bland eq iv_state.
  if sy-subrc eq 0.
    cv_state_txt = s_t005u-bezei.
  else.
    clear cv_state_txt.
  endif.
  clear s_state. "Note 2886550
  s_state-land1 = iv_land1.
  s_state-state = iv_state.
  s_state-state_txt = cv_state_txt.
  insert s_state into table st_state.
endform.
*&---------------------------------------------------------------------*
*&      Form  READ_ADDRESSTYPETEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->iv_anssa  text
*      <--cv_stext  text
*----------------------------------------------------------------------*
form read_addresstypetext  using    iv_anssa
                           changing cv_stext.
  types: begin of type_st,
           anssa type subty_591a,
           stext type sbttx,
         end of type_st.
  statics: s_type  type type_st,
           st_type type hashed table of type_st with unique key anssa.
  data: s_t591s type t591s.
  clear :cv_stext,s_type.
  read table st_type into s_type with key anssa = iv_anssa.
  if s_type is not initial.
    cv_stext = s_type-stext.
    exit.
  endif.

  select single * from t591s into s_t591s where sprsl eq sy-langu
                                           and  infty eq '0006'
                                           and  subty eq iv_anssa.
  if sy-subrc eq 0.
    cv_stext = s_t591s-stext.
  else.
    clear cv_stext.
  endif.
  clear s_type. "Note 2886550
  s_type-anssa = iv_anssa.
  s_type-stext = cv_stext.
  insert s_type into table st_type.


endform.
*&---------------------------------------------------------------------*
*&      Form  READ_PAYSCALE_TYPETEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_TRFAR  text
*      <--CV_TARTX  text
*----------------------------------------------------------------------*
form read_payscale_typetext  using    iv_trfar
                                      iv_molga
                             changing cv_tartx.
  types: begin of tartx_st,
           molga type molga,
           trfar type trfar,
           tartx type tartx,
         end of tartx_st.
  statics: s_tartx  type tartx_st,
           st_tartx type hashed table of tartx_st with unique key molga trfar.
  data: s_t510a type t510a.
  clear :cv_tartx,s_tartx,s_t510a.
  read table st_tartx into s_tartx with key molga = iv_molga trfar = iv_trfar.
  if s_tartx is not initial.
    cv_tartx = s_tartx-tartx.
    exit.
  endif.

  select single * from t510a into s_t510a where molga eq iv_molga
                                           and  trfar eq iv_trfar.

  if sy-subrc eq 0.
    cv_tartx = s_t510a-tartx.
  else.
    clear cv_tartx.
  endif.
  clear s_tartx.
  s_tartx-molga = iv_molga.
  s_tartx-trfar = iv_trfar.
  s_tartx-tartx = cv_tartx.
  insert s_tartx into table st_tartx.

endform.
*&---------------------------------------------------------------------*
*&      Form  READ_PAYSCALE_AREATEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_TRFGB  text
*      -->IV_MOLGA  text
*      <--CV_TGBTX  text
*----------------------------------------------------------------------*
form read_payscale_areatext  using    iv_trfgb
                                      iv_molga
                             changing cv_tgbtx.
  types: begin of tgbtx_st,
           molga type molga,
           trfgb type trfar,
           tgbtx type tgbtx,
         end of tgbtx_st.
  statics: s_tgbtx  type tgbtx_st,
           st_tgbtx type hashed table of tgbtx_st with unique key molga trfgb.
  data: s_t510g type t510g.
  clear :cv_tgbtx,s_tgbtx,s_t510g .
  read table st_tgbtx into s_tgbtx with key molga = iv_molga trfgb = iv_trfgb.
  if s_tgbtx is not initial.
    cv_tgbtx = s_tgbtx-tgbtx.
    exit.
  endif.

  select single * from t510g into s_t510g where molga eq iv_molga
                                           and  trfgb eq iv_trfgb.

  if sy-subrc eq 0.
    cv_tgbtx = s_t510g-tgbtx.
  else.
    clear cv_tgbtx.
  endif.
  clear s_tgbtx.
  s_tgbtx-molga = iv_molga.
  s_tgbtx-trfgb = iv_trfgb.
  s_tgbtx-tgbtx = s_t510g-tgbtx.
  insert s_tgbtx into table st_tgbtx.

endform.
*&---------------------------------------------------------------------*
*&      Form  CHANGEFIELDDIRECTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_SHWSPLIT  text
*      <--OV_FIELD_DIRECTORY  text
*----------------------------------------------------------------------*
form changefielddirectory  using    iv_shwsplit
                           changing ov_field_directory.

  data  : wa_list           type tlga_sortitem,
          wa_fielddirectory type tlga_sorthierarchy,
          wa_sortitems      type tlga_sortitemlist.
  field-symbols : <fs_directory> type any,
                  <fs_sortitems> type tlga_sortitemlist,
                  <fs_list>      type any.
  if iv_shwsplit is initial.
    wa_fielddirectory = ov_field_directory.
    wa_sortitems = wa_fielddirectory-validsortitems.
    delete wa_sortitems-list where attribute = 'apznr'.
    wa_fielddirectory-validsortitems = wa_sortitems.
    ov_field_directory = wa_fielddirectory.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  READ_COSTCENTER_TEXT_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KOSTL_KOSTL_KOKRS  text
*      -->P_KOSTL_KOSTL  text
*      -->P_KOSTL_KDATE  text
*      <--P_KOSTL_KOSTL_TXT  text
*----------------------------------------------------------------------*
form read_costcenter_text_2  using  value(kokrs) like hrca_contr-co_area
                                    value(kostl) like hrca_costc-costcenter
                                    value(iv_kdate) type pc261-fpend
                         changing   kostl_txt like hrca_costc-name.

  types: begin of cctrtxt_st,"Begin of Note 2683888
           kokrs     type kokrs,
           kostl     type kostl,
           endda     type datum,
           kostl_txt type ktext,
         end of cctrtxt_st.
  statics: s_cctrtxt  type cctrtxt_st,
           st_cctrtxt type hashed table of cctrtxt_st
              with unique key kokrs kostl endda.

  clear kostl_txt.
  read table st_cctrtxt into s_cctrtxt with key kokrs = kokrs
                                          kostl = kostl
                                          endda = iv_kdate.
  if sy-subrc = 0.
    kostl_txt = s_cctrtxt-kostl_txt.
    exit.
  endif.  "End of Note 2683888
  call function 'HRCA_COSTCENTER_TEXT'
    exporting
      controllingarea = kokrs
      costcenter      = kostl
      read_date       = iv_kdate
    importing
      name            = kostl_txt
    exceptions
      nothing_found   = 1
      others          = 2.
  if sy-subrc = 0.
    clear s_cctrtxt. "Begin of Note 2683888
    s_cctrtxt-kokrs = kokrs.
    s_cctrtxt-kostl = kostl.
    s_cctrtxt-endda = iv_kdate.
    s_cctrtxt-kostl_txt = kostl_txt.
    insert s_cctrtxt into table st_cctrtxt. "End of Note 2683888
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  SET_SELECTION_SCREEN_PARAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--CV_FCHDCL  text
*      <--CV_SHWSPLIT  text
*----------------------------------------------------------------------*
form set_selection_screen_param  changing cv_fchdcl
                                          cv_shwsplit.
enhancement-point set_selection_screen_param spots hrpay_cwtr include bound .

  if sw-fetchdcl = abap_false.  "improvement request 457
    loop at screen.
      if screen-group1 eq 'DCL'.
        screen-invisible = '1'.
        screen-active = '0'.
        modify screen.
      endif.
    endloop.
    cv_fchdcl = space.
  endif.
  if sw-shwsplit = abap_false.  "improvement request 499
    loop at screen.
      if screen-group1 eq 'SPL'.
        screen-invisible = '1'.
        screen-active = '0'.
        modify screen.
      endif.
    endloop.
    cv_shwsplit = space.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  SET_DECLUSTERED_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_FCHDCL  text
*----------------------------------------------------------------------*
form set_declustered_flag  using  iv_fchdcl.
  if iv_fchdcl = 'X'.
    flg_fetchdcl = 'X'.
  else.
    flg_fetchdcl = space.
  endif.
endform.
