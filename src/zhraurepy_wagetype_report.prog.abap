*&---------------------------------------------------------------------*
*& Report           : ZHRAUREPY_WAGETYPE_REPORT                        *
*& Tiltle           : Report on Wage Type Characteristics              *
*& Transactio Code  :                                                  *
*& Create Date      : 17 Aug 2021                                      *
*& Release          : ECC 6.0                                          *
*& Author           : Satya Aluru                                      *
*&                                                                     *
*&  Custom utility program  to report selected wage types              *
*&  charectorstics and their usage in payroll rules                    *
*&---------------------------------------------------------------------*
*& Change History :                                                    *
*&---------------------------------------------------------------------*
*& DATE        User    Description                          TR Number  *
*&---------------------------------------------------------------------*
*&                                                                     *
*&---------------------------------------------------------------------*
report zz_wagetype_report no standard page heading
line-size 132
message-id 5d.
*
*-----------   Tabellen                       ------------------------*
tables: t512w,   t512t, t52d8, t52d9, t52da, t52db, t554s.


*-----------   Select-Options und Parameter   ------------------------*
data: molga like t001p-molga.     "Fuer Select-Option MODLGART notwendig

selection-screen begin of block r1 with frame title text-t10.
parameters:
  pa_lgart type c radiobutton group g1 default 'X',
  pa_lgar1 type c radiobutton group g1,
  pa_awart type c radiobutton group g1.
selection-screen end of block r1.

selection-screen begin of block one with frame title text-t01.
parameters p_molga type molga obligatory memory id mol default '13'.
select-options:
lgart       for t512w-lgart,                      "Suchbegriff L/G-Art
awart       for t554s-subty no-display,
txt         for t512t-lgtxt no-display.                 "Suchbegriff Text L/G-Art
*modlgart    FOR molga MEMORY ID mol.
parameters modlgart like t500l-molga memory id mol default '13' no-display.
*PARAMETERS:
*p_abtype    TYPE checkbox USER-COMMAND flag.
selection-screen end of block one.

selection-screen begin of block awart with frame title text-t09.
select-options:
so_awart       for t554s-subty.
selection-screen end of block awart.
selection-screen begin of block two with frame title text-t02.
parameter:
klartext like rpdxxxxx-klartext default 'X' no-display, "Schalter Anzeige Kumul.
sortiere like rpdxxxxx-sortiere default 'T' no-display, "Schalter Sortierfolge
von like rpdxxxxx-von default sy-datum,    "Von-Datum"VIAK11K071964
bis like rpdxxxxx-bis default sy-datum no-display,    "Bis-Datum"VIAK11K071964
auto like rpdxxxxx-auto default ' ' no-display,      "Schalter Betriebsart
call_tab like rpdxxxxx-call_tab no-display.  "Info Ruftabelle
selection-screen end of block two.



** added alv **
parameters: pa_alv like rpdxxxxx-inh_vrz default 'X' no-display.
**

*-----------   Interne Tabellen               ------------------------*
data: begin of lga occurs 400,         "Daten zum Aufbau der Grundliste
        molga     like t512w-molga,
        lgart     like t512w-lgart,
        endda     like t512w-endda,
        begda     like t512w-begda,
        lgtxt(25),
        kumul     like t512w-kumul,
        vklas     like t512w-vklas,
        aklas     like t512w-aklas,
      end of lga.
data: begin of abtype occurs 500,
        moabw type  moabw,
        subty type  awart,
        endda type  endda,
        begda type  begda,
        klbew type  bewkl,
        abknd type  abknd,
      end of abtype.
data: begin of kum occurs 500,         "Daten Kumulationslohnarten
        molga     like t512w-molga,
        kumla     like t512w-lgart,
        lgart     like t512w-lgart,
        lgtxt(25),
      end of kum.

data: begin of vkl occurs 500,         "Daten Verarbeitungsklassen
        molga     like t512w-molga,
        klass(2),
        auspr(1),
        lgart     like t512w-lgart,
        lgtxt(25),
      end of vkl.

data: begin of akl occurs 500,         "Daten Auswertungsklassen
        molga     like t512w-molga,
        klass(2),
        auspr(2),
        lgart     like t512w-lgart,
        lgtxt(25),
      end of akl.

data: begin of molgas occurs 10,       "selektierte Molgas
        mo like t001p-molga,
      end of molgas.

data fcode type table of sy-ucomm with header line.
*-----------   Allgemeine Variablen           ------------------------*
data: anz             type p,              "Hilfsfeld Anzahl Zeilen Tabelle LGA
      dummy_klasse(8),          "Hilfsfeld Ausgabe Aus/Verarbeitungskl.
      endda           like t512w-endda,          "Fuer HIDE
      kumu96(96),               "Char-Feld fuer 96 kumulationsfelder
      hexnull(12)     type x value '00',
      kumul(1),
      lohnart         like t512w-lgart,        "Fuer HIDE
      absence         type awart.
*------------- texts for evaluation and processing classes ----------*
data: prclt_text like t52d8-prclt.
data: prcvt_text like t52d9-prcvt.
data: evclt_text like t52da-evclt.
data: evcvt_text like t52db-evcvt.
data: eol type i value 79.
*-----------   Konstanten                     ------------------------*
data: ja(1)   type c value 'X',
      nein(1) type c value ' '.

*---------------------------------------------------------------------*
*-----------       BEGINN PROGRAMMTEIL        ------------------------*
*---------------------------------------------------------------------*

include zhraurepy_wagetype_report_alv.

at selection-screen.
  if pa_lgar1 = 'X'.
    select count(*) from t512w into gv_count
      where lgart in lgart.
    if gv_count eq 0.
      message e999(zh) with 'Select 1 wage type.'.
    endif.
  endif.


start-of-selection.
  modlgart = p_molga.

  if pa_awart = 'X'.
    submit zhraurepy_awart_report
    with so_awart in so_awart
    with pa_key = von
    and return.

    return.
  endif.

  if pa_lgar1 = 'X'.
    submit zhraurepy_wt_attributes_rep00
    with modlgart = p_molga
    with sel in lgart
    with begdat = von
    with enddat = von
    with matrix = 'X'
    with withowt = ''
    and return.

    return.
  endif.

  begdat = von.
  enddat = bis = von.
  gs_alv-molga = modlgart.
  perform dokmolga.

  if 1 = 1. "p_abtype = abap_false.
*----------   Wertzuweisungen div. Variablen -------------------------*
    set pf-status '0000'.
    translate klartext to upper case.                    "#EC TRANSLANG
    translate auto     to upper case.                    "#EC TRANSLANG
    translate call_tab to upper case.                    "#EC TRANSLANG
*----------   Bestimmen Molgas entprechend Select-Option -------------*
*    PERFORM fill_molgas.
*----------   Lesen Lohnarten T512W fuer selekt. Molgas  -------------*
    clear: lga. lga-kumul = 0.
*    LOOP AT molgas.
    perform read_from_t512w using modlgart.
*    ENDLOOP.
*----------   Pruefen, ob L/G-Arten gefunden  ------------------------*
    describe table lga lines anz.
    if anz < 1.                          "zu Selektion nichts gefunden
      message e999(zh) with 'No data selected.'.
      exit.
    endif.
*----------   Sortieren und Ausgabe    -------------------------------*
    case sortiere.
      when 'L'.
        sort lga by molga lgart endda ascending.
      when 'E'.
        sort lga by molga endda lgart endda ascending.
      when others.
        sort lga by molga lgtxt lgart endda ascending.
    endcase.
    perform write_from_lga.
    sort kum.
    sort vkl.
    sort akl.
*----------   Grundliste erweitern, falls Betriebsart automatisch ----*
    if auto eq 'X'.
      perform auto_list.
    else.
      fcode = 'ABS_VAL'.
      append fcode.
      fcode = 'ABS_WAGE'.
      append fcode.
      fcode = 'VK01'.
      append fcode.
      fcode = 'AK01'.
      append fcode.
      if akl[] is initial.
        fcode = 'AK01'.
        append fcode.
        fcode = 'AKUB'.
        append fcode.
      endif.
      if vkl[] is initial.
        fcode = 'VK01'.
        append fcode.
        fcode = 'VKUB'.
        append fcode.
      endif.
*      IF kum[] IS INITIAL.
*        fcode = 'KUUB'.
*        APPEND fcode.
*      ENDIF.
      if fcode[] is not initial.
        set pf-status '0000' excluding fcode immediately.
      endif.
    endif.
  else.
***********************************************
    fcode = 'AK01'.
    append fcode.
    fcode = 'AKUB'.
    append fcode.
    fcode = 'KUUB'.
    append fcode.
    fcode = 'VK01'.
    append fcode.
    fcode = 'VKUB'.
    append fcode.
    fcode = 'DOCO'.
    append fcode.
    fcode = 'WBCD'.
    append fcode.
    fcode = 'DOCE'.
    append fcode.
    fcode = 'RULE_SEL'.
    append fcode.

    set pf-status '0000' excluding fcode immediately.

    select moabw subty endda begda klbew abknd into table abtype
             from t554s
             where subty in awart
             and   begda le bis
             and   endda ge von.
    describe table abtype lines anz.
    if anz < 1.                          "zu Selektion nichts gefunden
      message e999(zh) with 'No data selected.'.
      exit.
    endif.
    delete abtype where moabw is initial.
    loop at abtype.
      format color col_group intensified off.
      write: / abtype-moabw.
      write: 10 abtype-subty.
      write: 20 abtype-begda.
      write: 40 abtype-endda.
      write: 60 abtype-klbew.
      write: 80 abtype-abknd.
      clear absence.
    endloop.

  endif.

top-of-page.
*----------   Top of Page Grundliste      ----------------------------*
*  IF p_abtype = abap_false.
  case sy-pfkey.
*    WHEN '0000'.                       "Standard-Header
*      IF p_abtype EQ abap_false.
*        WRITE: /01 sy-datum DD/MM/YY, 12 sy-title, 72 sy-repid.
*        ULINE.
*        FORMAT COLOR COL_HEADING INTENSIFIED ON.
*        WRITE: / text-044, 60 ' '.
*        FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
*        WRITE: 62 text-003 ,
*        lga-molga.
*        FORMAT COLOR OFF.
*        IF klartext EQ nein.
*          FORMAT COLOR COL_HEADING  INTENSIFIED OFF.
*          WRITE: /01 text-006,
*          10 text-008,
*          32 text-009.
*          WRITE: / text-004 UNDER text-009
*          COLOR COL_HEADING      INTENSIFIED OFF ,
*          82 text-005 COLOR COL_HEADING INTENSIFIED OFF.
*          ULINE.
*        ELSE.
*          FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*          WRITE: /01 text-006,
*          10 text-008,
*          40 text-022,
*          47 text-023,
*          58 text-024, AT eol ' '.
*          FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*          WRITE: /10 text-010,  AT eol ' '.
*
*          ULINE.
*        ENDIF.
*        FORMAT RESET.
*      ELSE.
*        FORMAT COLOR COL_HEADING  INTENSIFIED ON.
*        WRITE: / 'PSG'.
*        WRITE: 10 'Abs Type'.
*        WRITE: 20 'Start Date'.
*        WRITE: 40 'End Date'.
*        WRITE: 60 'Abs Val Rule'.
*        WRITE: 80 'Abs Category'.
*
*        ULINE.
*      ENDIF.
    when 'KUMU'.                       "Top of Page Kumulationen
      perform top_kumu.
    when 'VKLA'.                       "Top of Page Verarbeitungsklassen
      perform top_vkla.
    when 'AKLA'.                       "Top of Page Auswertungsklassen
      perform top_akla.
    when 'DOCR'.
      perform top_doco.
    when 'WBCD'.
      perform top_wbcd.
  endcase.
*  ELSE.
**********************************************
*  ENDIF.

top-of-page during line-selection.
*----------   Top of Page weitere Liststufen  ------------------------*
  if sy-pfkey = 'KUMU'.
    perform top_kumu.
  endif.
  if sy-pfkey = 'VKLA'.
    perform top_vkla.
  endif.
  if sy-pfkey = 'VKL1'.
    write: /01 sy-datum dd/mm/yy, 12 sy-title, 72 sy-repid. uline.

    format color col_heading intensified on.
    write: / text-025, lohnart, 60 ' '.
    format color col_total intensified off.
    write:  62 text-003  ,
    vkl-molga .
    format color col_heading intensified off.
    write: / text-042, '/', text-036 , at eol ' '.
    format color off.
    uline.
  endif.
  if sy-pfkey = 'AKLA'.
    perform top_akla.
  endif.
  if sy-pfkey = 'AKL1'.
    summary.
    write: /01 sy-datum dd/mm/yy, 12 sy-title, 72 sy-repid. uline.
    format color col_heading intensified on.
    write: / text-027, lohnart, 60 ' '.
    format color col_total intensified off.
    write:  62 text-003 ,
    akl-molga .
    format color col_heading intensified off.
    write: / text-043, '/', text-036 , at eol ' '.
    format reset.
    uline.
  endif.
  if sy-pfkey = 'DOCR'.
    perform top_doco.
  endif.
  if sy-pfkey = 'WBCD'.
    perform top_wbcd.
  endif.
*----------   Ende der Grundliste      -------------------------------*
*---------------------------------------------------------------------*
*-----------       BEGINN UNTERROUTINEN       ------------------------*
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* FORM READ_FROM_T512W using p1.                                      *
*---------------------------------------------------------------------*
* Lesen der Lohnarten in Tabelle T512W fuer jeweiligen Molga          *
*---------------------------------------------------------------------*
form read_from_t512w using $molga.
  select * from t512w where molga eq $molga
  and endda ge von
  and begda le bis.
    check: lgart.
    select single * from t512t where sprsl eq sy-langu
    and   molga eq t512w-molga
    and   lgart eq t512w-lgart.
    check: txt.
    move t512t-lgtxt to lga-lgtxt.
    move-corresponding t512w to lga.
    append lga.
  endselect.
endform.                               "READ_FROM_T512W

*---------------------------------------------------------------------*
* FORM WRITE_FROM_LGA.                                                *
*---------------------------------------------------------------------*
* Ausgabe der Grundliste aus den Werten der internen Tabelle LGA      *
*---------------------------------------------------------------------*
form write_from_lga.
  data col_switch type i.
  sort lga by lgart.
  loop at lga.

    at new molga.
      new-page.
    endat.

    if lga-lgart eq '0   ' and klartext eq nein. new-page. endif.
    format color col_group intensified off.
    write: / lga-lgart.
    write: 10 lga-lgtxt.

    move: lga-molga to molga, lga-lgart to lohnart,
    lga-endda to endda.
    hide: molga, lohnart, endda.
    col_switch = 0.
    if klartext eq nein.
      format reset.
      write: lga-vklas under text-004 color col_normal intensified off.
    else.
      write: lga-begda dd/mm/yyyy under text-023   ,
      lga-endda dd/mm/yyyy under text-024 ,
      at eol ' '.
      format color col_normal.
    endif.
    move-corresponding lga to kum.     "MOLGA, LGART etc. in KUM
    move-corresponding lga to vkl.     "MOLGA, LGART etc. in VKL
    move-corresponding lga to akl.     "MOLGA, LGART etc. in AKL
    perform fill_vkl.              "Fuellen VKL aus aktueller Lohnart
    perform fill_akl.              "Fuellen AKL aus aktueller Lohnart
    if pa_alv = 'X'. continue. endif.
    if klartext eq nein.
      if pa_alv = ''.
        if lga-kumul ne hexnull.         "Ausgabe Uebersichtsleiste
          t512w-kumul = lga-kumul.
          perform hexinchar.

          write: / kumu96 under lga-vklas color col_normal intensified on.

          move: lga-molga to molga, lga-lgart to lohnart,
          lga-endda to endda.
          hide: molga, lohnart, endda.
          perform fill_kum.          "Fuellen KUM aus aktueller Lohnart
        endif.
      endif.
    else.
      move: space     to t512t,        "Ausgabe Klartexte
      lga-molga to t512t-molga,
      '/1'      to t512t-lgart(2),
      sy-langu  to t512t-sprsl.
      if lga-kumul ne hexnull.
        t512w-kumul = lga-kumul.
        perform hexinchar.
        while sy-index le 96
        vary kumul from kumu96(1) next kumu96+1(1)     "UC XMS
        range kumu96.                       "UC XMS
          if kumul eq 1.
            unpack sy-index to t512t-lgart+2(2).   "Lesen Kum-Lohnart
            read table t512t.
            kum-kumla = t512t-lgart.   "Fuellen KUM
            append kum. perform change_col using col_switch.
            write: / t512t-lgart under text-010,
            t512t-lgtxt , at eol ' ' .
            move: lga-molga to molga, t512t-lgart to lohnart,
            space     to endda.
            hide: molga, lohnart, endda.
          endif.
        endwhile.                      "While SY-INDEX < 96
      endif.
    endif.                             "If KLARTEXT eq NEIN
  endloop.                             "Loop at LGA
  clear: molga, lohnart, endda.
endform.                               "WRITE_FROM_LGA.

*---------------------------------------------------------------------*
* FORM WRITE_VKL_FOR_LGART.                                           *
*---------------------------------------------------------------------*
* Listen der Verarbeitungsklassen fuer selektierte L/G-Art            *
*---------------------------------------------------------------------*
form write_vkl_for_lgart.
  data: klasse(2).
  clear klasse.
  loop at vkl where molga = molga
  and   lgart = lohnart.
    if vkl-klass ne klasse.
      perform read_prcl_text using molga
            vkl-klass
            prclt_text.
      skip.
      reserve 3 lines.
      format color col_group intensified off.
      write: / vkl-klass, 4 prclt_text,  at eol ' '.
      move vkl-klass to klasse.
    endif.

    perform read_prclv_text using molga
          vkl-klass
          vkl-auspr
          prcvt_text.
    format color col_normal intensified off.
    write: /4 vkl-auspr, prcvt_text, at eol ' '.
*    hide: molga, lohnart, endda.
  endloop.
  if sy-subrc <> 0.
    skip. write: / text-021. exit.
  endif.
  clear: molga, lohnart, endda.
endform.                               "WRITE_VKL_FOR_LGART

*---------------------------------------------------------------------*
* FORM WRITE_AKL_FOR_LGART.                                           *
*---------------------------------------------------------------------*
* Listen der Auswertungsklassen fuer selektierte L/G-Art              *
*---------------------------------------------------------------------*
form write_akl_for_lgart.
  data: klasse(2).
  loop at akl where molga = molga
  and   lgart = lohnart.
    if akl-klass ne klasse.
      perform read_evcl_text using molga
            akl-klass
            evclt_text.
      skip.
      reserve 3 lines.
      format color col_group intensified off.
      write: / akl-klass, 4  evclt_text, at eol ' '.
      move akl-klass to klasse.
    endif.
    perform read_evclv_text using molga
          akl-klass
          akl-auspr
          evcvt_text.
    format color col_normal intensified off.
    write: /4  akl-auspr, evcvt_text, at eol ' '.
*    hide: molga, lohnart, endda.
  endloop.
  if sy-subrc <> 0.
    skip. write: / text-032. exit.
  endif.
  clear: molga, lohnart, endda.
endform.                               "WRITE_AKL_FOR_LGART

*---------------------------------------------------------------------*
* FORM WRITE_FROM_KUM.                                                *
*---------------------------------------------------------------------*
* Listen der Kumulationslohnarten aus der Tabelle KUM                 *
*---------------------------------------------------------------------*
form write_from_kum.
  data: col_switch type i.
  loop at kum.
    at new molga.
      new-page.
    endat.
    at new kumla.
      select single * from t512t where sprsl eq sy-langu
      and   molga eq kum-molga
      and   lgart eq kum-kumla.
      reserve 3 lines. skip.
      format color col_group intensified off.
      write: / kum-kumla, t512t-lgtxt,  at eol ' '.
      move: kum-molga to molga, kum-kumla to lohnart,
      space to endda.
      format color col_normal. col_switch = 0.
      hide: molga, lohnart, endda.
    endat.
    if kum-lgtxt = space.
      clear t512t.
      select single * from t512t where sprsl eq sy-langu
      and   molga eq kum-molga
      and   lgart eq kum-lgart.
      move t512t-lgtxt to kum-lgtxt.
      modify kum.
    endif.
    perform change_col using col_switch.
    write: / kum-lgart under t512t-lgtxt, kum-lgtxt, at eol ' '.
    move: kum-molga to molga, kum-lgart to lohnart,
    space to endda.
    hide: molga, lohnart, endda.
  endloop.
  clear: molga, lohnart, endda.
endform.                               "WRITE_FROM_KUM.

*---------------------------------------------------------------------*
* FORM WRITE_FROM_VKL.                                                *
*---------------------------------------------------------------------*
* Listen der Verarbeitungsklassen aus der Tabelle VKL                 *
*---------------------------------------------------------------------*
form write_from_vkl.
  data: col_switch type i.
  loop at vkl.
    at new molga.
      new-page.
    endat.
    at new klass.
*     PERFORM READ_PRCL_TEXT USING MOLGA                  "XAIL9CK050258
      perform read_prcl_text using vkl-molga              "XAIL9CK050258
            vkl-klass
            prclt_text.
      move vkl-klass to dummy_klasse(2).
      summary. skip.
      reserve 3 lines.
      format color col_group intensified off."ON.
      write: / vkl-klass, prclt_text,  at eol ' '.
*      clear: molga, lohnart, endda.
*      hide: molga, lohnart, endda.
    endat.
    at new auspr.
*     PERFORM READ_PRCLV_TEXT USING MOLGA                 "XAIL9CK050258
      perform read_prclv_text using vkl-molga             "XAIL9CK050258
            vkl-klass
            vkl-auspr
            prcvt_text.
      format color col_group intensified off.
      write: /4 vkl-auspr, prcvt_text, at eol ' '.
      format color col_normal. col_switch = 0.
*      clear: molga, lohnart, endda.
*      hide: molga, lohnart, endda.
    endat.
    if vkl-lgtxt = space.
      clear t512t.
      select single * from t512t where sprsl eq sy-langu
      and   molga eq vkl-molga
      and   lgart eq vkl-lgart.
      move t512t-lgtxt to vkl-lgtxt.
      modify vkl.
    endif.
    perform change_col using col_switch.
    write: / vkl-lgart under prcvt_text,
    vkl-lgtxt, at eol ' '.
    move: vkl-molga to molga, vkl-lgart to lohnart,
    space to endda.
*    hide: molga, lohnart, endda.
  endloop.
  clear: molga, lohnart, endda.
endform.                               "WRITE_FROM_VKL.

*---------------------------------------------------------------------*
* FORM WRITE_FROM_AKL.                                                *
*---------------------------------------------------------------------*
* Listen der Auswertungsklassen aus der Tabelle AKL                   *
*---------------------------------------------------------------------*
form write_from_akl.
  data: col_switch type i.
  loop at akl.
    at new molga.
      new-page.
    endat.
    at new klass.
*     PERFORM READ_EVCL_TEXT USING MOLGA                  "XAIL9CK050258
      perform read_evcl_text using akl-molga              "XAIL9CK050258
            akl-klass
            evclt_text.
      move akl-klass to dummy_klasse(2).
      summary. skip.
      reserve 3 lines.
      format color col_group intensified off."ON.
      write: / akl-klass, evclt_text, at eol ' '.
*      clear: molga, lohnart, endda.
*      hide: molga, lohnart, endda.
    endat.
    at new auspr.
*     PERFORM READ_EVCLV_TEXT USING MOLGA                 "XAIL9CK050258
      perform read_evclv_text using akl-molga             "XAIL9CK050258
            akl-klass
            akl-auspr
            evcvt_text.
      format color col_group intensified off.
      write: /4 akl-auspr, evcvt_text, at eol ' '.
      format color col_normal. col_switch = 0 .
*      clear: molga, lohnart, endda.
*      hide: molga, lohnart, endda.
    endat.
    if akl-lgtxt = space.
      clear t512t.
      select single * from t512t where sprsl eq sy-langu
      and   molga eq akl-molga
      and   lgart eq akl-lgart.
      move t512t-lgtxt to akl-lgtxt.
      modify akl.
    endif.
    perform change_col using col_switch.
    write: / akl-lgart under evcvt_text ,
    akl-lgtxt, at eol ' '.
    move: akl-molga to molga, akl-lgart to lohnart,
    space to endda.
*    hide: molga, lohnart, endda.
  endloop.
  clear: molga, lohnart, endda.
endform.                               "WRITE_FROM_AKL.

*---------------------------------------------------------------------*
* FORM AUTO_LIST: Erweiterte Grundliste bei autom. Betriebsart        *
*---------------------------------------------------------------------*
form auto_list.
  set pf-status 'KUMU'.
  perform write_from_kum.

  set pf-status 'VKLA'.
  perform write_from_vkl.

  set pf-status 'AKLA'.
  perform write_from_akl.

*  SET PF-STATUS 'BACK'.
endform.                               "AUTO_LIST

*---------------------------------------------------------------------*
*       FORM HEXINCHAR                                                *
*---------------------------------------------------------------------*
*       Umwandlung einer Hexzahl in den entsprechenden Bitstring.     *
*---------------------------------------------------------------------*
form hexinchar.

  data: div1 type p,
        hex  type x,
        hexp type p.

*   T512W-Kumul hat die Laenge 12 --> Konvertierung erfolgt byteweise
  do 12 times
  varying hex from t512w-kumul(1) next t512w-kumul+1(1)  "UC XMS
  range t512w.                               "UC XMS
    div1 = 256. hexp = hex.
*     Vergleich mit 128, 64, 32 ... und setzen Bit je nach Vergleich
    do 8 times.
      shift kumu96.
      div1 = div1 / 2.
      if hexp >= div1.
        move '1' to kumu96+95(1).
        hexp = hexp - div1.
      else.
        move '0' to kumu96+95(1).
      endif.
    enddo.
  enddo.
endform.                               "END OF HEXINCHAR

*---------------------------------------------------------------------*
* FORM FILL_KUM.                                                      *
*---------------------------------------------------------------------*
* Fuellen KUM, falls Ausgabe Grundliste nicht im Klartext erfolgt.    *
*---------------------------------------------------------------------*
form fill_kum.

  data:  fill96 like kumu96.

  move kumu96 to fill96.
  move '/1' to kum-kumla(2).
  do 96 times.
    if fill96(1) = '1'.
      unpack sy-index to kum-kumla+2(2).
      append kum.
    endif.
    shift fill96.
  enddo.
endform.                               "Fill_KUM

*---------------------------------------------------------------------*
* FORM FILL_VKL.                                                      *
*---------------------------------------------------------------------*
* Fuellen VKL, falls Ausgabe Grundliste nicht im Klartext erfolgt.    *
*---------------------------------------------------------------------*
form fill_vkl.
  data: vkl100 like t512w-vklas.
  move lga-vklas to vkl100.
  do 100 times.
    if vkl100(1) <> space.
      unpack sy-index to vkl-klass.
      move vkl100(1) to vkl-auspr.
      append vkl.
    endif.
    shift vkl100.
  enddo.
endform.                               "Fill_VKL

*---------------------------------------------------------------------*
* FORM FILL_AKL.                                                      *
*---------------------------------------------------------------------*
* Fuellen AKL.                                                        *
*---------------------------------------------------------------------*
form fill_akl.
  data: akl40 like t512w-aklas.

  move lga-aklas to akl40.
  do 20 times.
    if akl40(2) <> space.
      unpack sy-index to akl-klass.
      move akl40(2) to akl-auspr.
      append akl.
    endif.
    shift akl40 by 2 places.
  enddo.
endform.                               "Fill_AKL


*---------------------------------------------------------------------*
* FORM READ_PRCL_Text:                                                *
*               Read Text for Processing Class from   T52D8           *
*---------------------------------------------------------------------*
form read_prcl_text using $molga $prcls $classtext.         "WUR
  clear: $classtext, t52d8.

  select single * from  t52d8
  where  sprsl       = sy-langu
  and    molga       = $molga
  and    prcls       = $prcls          .

  if sy-subrc eq 0.
    move t52d8-prclt to  $classtext.
  endif.
  if $classtext eq space.
    move text-039 to $classtext.
  endif.
endform.                               "READ_prcl_text
*---------------------------------------------------------------------*
* FORM READ_PRCLV_Text: Read Text for Specification of Processing Class*
*                  from T52D9                                         *
*---------------------------------------------------------------------*
form read_prclv_text using $molga $prcls $prclv $classtext.
  clear: $classtext, t52d9.

  select single * from  t52d9
  where  sprsl       = sy-langu
  and    molga       = $molga
  and    prcls       = $prcls
  and    prclv       = $prclv .
  if sy-subrc eq 0.
    move t52d9-prcvt to  $classtext.
  endif.
  if $classtext eq space.
    move text-040 to $classtext.
  endif.
endform.                               "READ_prclv_text

*---------------------------------------------------------------------*
* FORM READ_EVCL_text: Read Text for Evaluation Class                *
*                  from T52DA                                         *
*---------------------------------------------------------------------*
form read_evcl_text using $molga $evcls  $classtext.        "WUR
  clear: $classtext, t52da.

  select single * from  t52da
  where  sprsl       = sy-langu
  and    molga       = $molga
  and    evcls       = $evcls .

  if sy-subrc eq 0.
    move t52da-evclt to  $classtext.
  endif.
  if $classtext eq space.
    move text-028 to $classtext.
  endif.
endform.                               "read_evcl_text
*---------------------------------------------------------------------*
* FORM READ_EVCLV_TEXT:                                               *
*  Read Text for Specification of Evaluation Class from T52DB         *
*---------------------------------------------------------------------*
form read_evclv_text using $molga $evcls $evclv $classtext.
  clear: $classtext, t52db.

  select single * from  t52db
  where  sprsl       = sy-langu
  and    molga       = $molga
  and    evcls       = $evcls
  and    evclv       = $evclv .
  if sy-subrc eq 0.
    move t52db-evcvt to  $classtext.
  endif.
  if $classtext eq space.
    move text-041 to $classtext.
  endif.
endform.                               "READ_evclv_text




*---------------------------------------------------------------------*
* FORM Fill_MOLGAS:  Modifikatoren in Tabelle MOLGAS schreiben        *
*---------------------------------------------------------------------*
* Molga-Selektion von MODLGART in Molgatabelle MOLGAS uebertragen     *
*---------------------------------------------------------------------*
*FORM fill_molgas.
*
*  DATA: i      TYPE p.                 "Hilfsvariable
*
*  MOVE 0 TO i.
*  WHILE i < 99.                        "Sammeln der in der Select-Option
*    i = i + 1.                         "aufgezaehlten Werte
*    UNPACK i TO molga.
*    CHECK modlgart.
*    MOVE molga TO molgas-mo.
*    APPEND molgas.
*  ENDWHILE.
*  DESCRIBE TABLE molgas LINES i.       "Setzen Molga, wenn eindeutig
*  IF i = 1.
*    READ TABLE molgas INDEX i.
*    SET PARAMETER ID 'MOL' FIELD molgas-mo.
*  ENDIF.
*ENDFORM.                               "FILL_MOLGAS.

*---------------------------------------------------------------------*
* FORM TOP_VKLA: Top of Page fuer PF-Status 'VKLA'                    *
*---------------------------------------------------------------------*
form top_vkla.
  write: /01 sy-datum dd/mm/yy, 12 sy-title, 72 sy-repid. uline.
  format color col_heading intensified on.
  write: /(60) text-002, 60 ' '. format color col_total intensified off.
  write:  62 text-003 ,
  vkl-molga.

  format color col_heading intensified off.
  write: / text-042,'/', text-036, '/', text-037, at eol ' '.
  format reset.
  uline.
endform.                               "TOP_VKLA

*---------------------------------------------------------------------*
* FORM TOP_AKLA: Top of Page fuer PF-Status 'AKLA'                    *
*---------------------------------------------------------------------*
form top_akla.
  write: /01 sy-datum dd/mm/yy, 12 sy-title, 72 sy-repid. uline.

  format color col_heading intensified on.
  write: /(60) text-029, 60 ' '.
  format color col_total intensified off.
  write:  62 text-003  ,
  akl-molga.
  format color col_heading intensified off.
  write: / text-043, '/',  text-036, '/', text-037, at eol ' '.
  format reset.
  uline.
endform.                               "TOP_AKLA

*---------------------------------------------------------------------*
* FORM TOP_KUMU: Top of Page fuer PF-Status 'KUMU'                    *
*---------------------------------------------------------------------*
form top_kumu.
  write: /01 sy-datum dd/mm/yy, 12 sy-title, 72 sy-repid. uline.
  format color col_heading intensified on.
  write: /(60) text-001, 60 ' '.
  format color col_total intensified off.

  write:                                  62 text-003, kum-molga.
  format color col_heading intensified off.
  write: / text-035, '/', text-037 intensified off.
  format reset.
  uline.
endform.                               "TOP_KUMU

at user-command.

  refresh: list_alv, list_alvw, list_alvm, list_alvc.
  clear list_alv.

  case sy-ucomm.
    when 'AK01'.
*----------   Auswertungsklassen je selektierte Lohnart  -----------*
*      IF akl[] IS NOT INITIAL.
      set pf-status 'AKL1'.
      perform write_akl_for_lgart.
*      ENDIF.
    when 'VK01'.
*----------   Verarbeitungsklassen je selektierte Lohnart  -----------*
*      IF vkl[] IS NOT INITIAL.
      set pf-status 'VKL1'.
      perform write_vkl_for_lgart.
*      ENDIF.
    when 'KUUB'.
*----------   Uebersicht Kumulationen    -----------------------------*
*      IF kum[] IS NOT INITIAL.
      set pf-status 'KUMU'.
      if pa_alv = ''.
        perform write_from_kum.
      else.
        perform write_from_kum1.
      endif.
*      ENDIF.
    when 'VKUB'.
*----------   Uebersicht Verarbeitungsklassen   ----------------------*
*      IF vkl[] IS NOT INITIAL.
      set pf-status 'VKLA'.
      if pa_alv = ''.
        perform write_from_vkl.
      else.
        perform write_from_vkl1.
      endif.
*      ENDIF.
    when 'AKUB'.
*----------   Uebersicht Auswertungsklassen     ----------------------*
*      IF akl[] IS NOT INITIAL.
      set pf-status 'AKLA'.
      if pa_alv = ''.
        perform write_from_akl.
      else.
        perform write_from_akl1.
      endif.
*      ENDIF.
    when 'DOCO'.
      perform write_documentation.
    when 'WBCD'.
      perform write_wb_code.
    when 'DOCE'.
      perform edit_documentation.
    when 'RULE'.
      if 1 = 1. "p_abtype EQ abap_false.
        perform cycle_matchup using abap_true.
      else.
        perform cycle_matchup_awart.
      endif.
    when 'RULE_SEL'.
      perform cycle_matchup using abap_false.
*    WHEN 'ABS_VAL'.
*      SET PARAMETER ID 'MOL' FIELD modlgart-low.
*      CALL TRANSACTION 'ZZZ_330215_ABS_VAL' AND SKIP FIRST SCREEN.
    when 'PSID'.
      perform write_psid.
    when 'MOWT'.
      perform write_mowt.
    when 'VALC'.
      perform write_valc.
    when 'DOCC'.
      perform create_document.
    when others.
  endcase.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_COL
*&---------------------------------------------------------------------*
*       changes format from intensified on to of and vice versa        *
*----------------------------------------------------------------------*
form change_col using $col.

  if $col = 0.
    format intensified off. $col = 1.
  else.
    format intensified on. $col = 0.
  endif.
endform.                               " CHANGE_COL
*&---------------------------------------------------------------------*
*&      Form  WRITE_DOCUMENTATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_documentation .
  call function 'HRDSYS_DOCU_SHOW'
    exporting
      otype          = 'WTYP'
      oname          = lohnart
      molga          = molga
    exceptions
      wrong_otype    = 1
      loio_not_found = 2
      internal_error = 3
      others         = 4.
  if sy-subrc = 2.
    set pf-status 'DOCC'.
    write: text-007.
  endif.
endform.                    " WRITE_DOCUMENTATION
form write_wb_code.
  data: ls_t512t type t512t.
  select single * into ls_t512t
                  from t512t
                  where sprsl = sy-langu
                  and   molga = molga
  and   lgart = lohnart.
  if sy-subrc = 0.
    format color col_heading intensified on.
    write:/ ls_t512t-lgart.
    write 15 ls_t512t-lgtxt.
    write 60 ls_t512t-kztxt.
    format color off.
  else.
    write: 'Wagetype - WB Code does not exist for this Wage Type.'(014).
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  TOP_DOCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form top_doco .
  write: /01 sy-datum dd/mm/yy, 12 sy-title, 72 sy-repid. uline.

  format color col_heading intensified on.
  write: / text-012, lohnart, 60 ' '.
  format color col_total intensified off.
  write:  62 text-003  ,
  molga .
  format color col_heading intensified off.
  write: / text-045, '/', text-036 , at eol ' '.
  format color off.
  uline.
endform.                    " TOP_DOCO

form top_wbcd .
  write: /01 sy-datum dd/mm/yy, 12 sy-title, 72 sy-repid. uline.

  format color col_heading intensified on.
  write: / text-013, lohnart, 60 ' '.
  format color col_total intensified off.
  write:  62 text-003  ,
  molga .
  uline.
endform.                    " TOP_WBCD
*&---------------------------------------------------------------------*
*&      Form  EDIT_DOCUMENTATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form edit_documentation .
  call function 'HRDSYS_DOCU_EDIT'
    exporting
      otype           = 'WTYP'
      oname           = lohnart
      molga           = molga
      change_mode     = 'X'
    exceptions
      wrong_otype     = 1
      loio_not_found  = 2
      internal_error  = 3
      cancel_editor   = 4
      nothing_changed = 5
      others          = 6.
  if sy-subrc = 2.
    set pf-status 'DOCC'.
    write: text-007.
  endif.

endform.                    " EDIT_DOCUMENTATION
*&---------------------------------------------------------------------*
*&      Form  CYCLE_MATCHUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cycle_matchup using pa_i_sel type boolean.
  types: begin of ty_alv_op,
           ccycl type ccycl,
           abart type abrar,
           lgart type lgart,
           vargt type vrarg,
           seqno type seqln,
           mode  type c,
           opt1  type char10,
           opt2  type char10,
           opt3  type char10,
           opt4  type char10,
           opt5  type char10,
           opt6  type char10,
         end of ty_alv_op,
         ty_t_alv_op type table of ty_alv_op.

  data: lt_t52c5       type standard table of t52c5,
        lt_t52c5_final type standard table of t52c5,
        lt_t52ba       type standard table of t52ba,
        lt_alv_op      type ty_t_alv_op,
        lw_alv_op      type ty_alv_op,
        lw_t52ba       type t52ba,
        lw_t52c5       type t52c5,
        lt_r_selopt    type table of selopt,
        lw_r_selopt    type selopt,
        l_pwert        type patwe,
        l_string       type string.

  data: lo_table      type ref to cl_salv_table,
        lo_header     type ref to cl_salv_form_layout_grid,
        lo_h_flow     type ref to cl_salv_form_layout_flow,
        lo_columns    type ref to cl_salv_columns,
        lo_column     type ref to cl_salv_column_table,
        lo_functions  type ref to cl_salv_functions_list,
        lo_selections type ref to cl_salv_selections,
        lo_salv_msg   type ref to cx_salv_msg,
        lo_sorts      type ref to cl_salv_sorts,
        lo_layout     type ref to cl_salv_layout,
        l_text        type txt255,
        lw_key        type salv_s_layout_key.
  data: lv_string type string.

  lw_r_selopt-sign   = 'I'.
  lw_r_selopt-option = 'CP'.

  if pa_i_sel = abap_false.
    if lohnart is initial.
      write: 'Please select the Wage type and click on this button.'.
      return.
    endif.
    lw_r_selopt-sign   = 'I'.
    lw_r_selopt-option = 'CP'.
    l_string = '*' && lohnart && '*'.
    lw_r_selopt-low    = l_string.
    append lw_r_selopt to lt_r_selopt.

  else.
    loop at lga.
      clear: l_string,
             lw_r_selopt-low.

      lw_r_selopt-sign   = 'I'.
      lw_r_selopt-option = 'CP'.

      l_string = '*' && lga-lgart && '*'.
      lw_r_selopt-low    = l_string.
      append lw_r_selopt to lt_r_selopt.
    endloop.
  endif.

* Progress Indicator for Performance
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Reading Rules data from DB'.
  select * from t52c5 into table lt_t52c5.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Filtering Rules Data Using WT Info'.
  loop at lt_t52c5 into lw_t52c5.
    concatenate lw_t52c5-lgart lw_t52c5-vargt lw_t52c5-vinfo
           into l_string separated by '|'.
    if l_string in lt_r_selopt.
      append lw_t52c5 to lt_t52c5_final.
    endif.
  endloop.
  lt_t52c5[] = lt_t52c5_final[].
  free lt_t52c5_final.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Final Processing'.
*  if sy-subrc = 0.
  if not lt_t52c5 is initial.
    l_pwert = molga.
    refresh lt_r_selopt[].
    loop at lt_t52c5 into lw_t52c5.
      clear lw_r_selopt-low.
      lw_r_selopt-low    = lw_t52c5-ccycl.
      append lw_r_selopt to lt_r_selopt.
    endloop.
    sort lt_r_selopt by low.
    delete adjacent duplicates from lt_r_selopt comparing low.
    select * into table lt_t52ba
             from t52ba
             where potyp = 'CYCL'
             and   ponam in lt_r_selopt
             and   pattr = 'CNT'
    and   pwert in ( l_pwert, '*' ).
    if sy-subrc = 0.
      refresh lt_r_selopt[].
      clear lw_r_selopt-low.
      loop at lt_t52ba into lw_t52ba.
        clear lw_r_selopt-low.
        lw_r_selopt-low    = lw_t52ba-ponam.
        append lw_r_selopt to lt_r_selopt.
      endloop.
      delete lt_t52c5 where ccycl not in lt_r_selopt.
      loop at lt_t52c5 into lw_t52c5.
        write: lw_t52c5-ccycl to lw_alv_op-ccycl,
               lw_t52c5-abart to lw_alv_op-abart,
               lw_t52c5-lgart to lw_alv_op-lgart,
               lw_t52c5-vargt to lw_alv_op-vargt,
               lw_t52c5-seqno to lw_alv_op-seqno,
               lw_t52c5-vinfo(1) to lw_alv_op-mode,
               lw_t52c5-vinfo+1(10) to lw_alv_op-opt1,
               lw_t52c5-vinfo+12(10) to lw_alv_op-opt2,
               lw_t52c5-vinfo+23(10) to lw_alv_op-opt3,
               lw_t52c5-vinfo+34(10) to lw_alv_op-opt4,
               lw_t52c5-vinfo+45(10) to lw_alv_op-opt5,
               lw_t52c5-vinfo+56(10) to lw_alv_op-opt6.
        append lw_alv_op to lt_alv_op.
      endloop.
    else.
      write: 'The Wage Type is not used in any Rules for this country.'.
    endif.
  else.
    write: 'The Wage Type is not used in any Rules.'(011).
  endif.

  if lt_alv_op[] is not initial.
*   Header object
    create object lo_header.
*   Get New Instance for ALV Table Object
    try.
        cl_salv_table=>factory(
        exporting
          list_display   = if_salv_c_bool_sap=>false
        importing
          r_salv_table = lo_table
        changing
          t_table      = lt_alv_op ).
      catch cx_salv_msg into lo_salv_msg.
        message e000 with lo_salv_msg->msgv1
        lo_salv_msg->msgv2
        lo_salv_msg->msgv3
        lo_salv_msg->msgv4.
    endtry.

*   Get the coloumns from the table
    lo_columns = lo_table->get_columns( ).
*   Optimise the coloumns width
    lo_columns->set_optimize( ).
*   Functions
    lo_functions = lo_table->get_functions( ).
    lo_functions->set_all( value = abap_true ).
*   Heading for the report
    l_text = 'Country specific Rules using the specified wagetypes'.
    lo_h_flow = lo_header->create_flow( row = 1  column = 1 ).
    lo_h_flow->create_text( text = l_text ).

*   Selections
    lo_selections = lo_table->get_selections( ).
    lo_selections->set_selection_mode(
    cl_salv_selections=>if_salv_c_selection_mode~row_column ).
*   Set the column headings on the output
    perform set_column_names
            using lo_columns: 'CCYCL' 'Rule'
                              changing lo_column,
                              'ABART' 'Employee subgroup grouping'
                              changing lo_column,
                              'LGART' 'Wage Type'
                              changing lo_column,
                              'VARGT' 'Variable Key'
                              changing lo_column,
                              'SEQNO' 'Next Line'
                              changing lo_column,
                              'MODE' 'Mode'
                              changing lo_column,
                              'OPT1' 'Operation'
                              changing lo_column,
                              'OPT2' 'Operation'
                              changing lo_column,
                              'OPT3' 'Operation'
                              changing lo_column,
                              'OPT4' 'Operation'
                              changing lo_column,
                              'OPT5' 'Operation'
                              changing lo_column,
                              'OPT6' 'Operation'
                              changing lo_column.

    call method lo_table->get_layout
      receiving
        value = lo_layout.
    lw_key-report = sy-repid.
    lo_layout->set_key( lw_key ).
*   Set Layout selected on screen
    lo_layout->set_default( abap_true ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   Set the top of list using the header for Online.
    lo_table->set_top_of_list( lo_header ).
    lo_table->display( ).
  endif.

endform.                    " CYCLE_MATCHUP
form set_column_names using io_columns type ref to cl_salv_columns
                            i_colname  type lvc_fname
                            i_coltext  type scrtext_l
                   changing eo_column type ref to cl_salv_column_table.

  data: l_scrtext_m type scrtext_m,
        l_scrtext_s type scrtext_s.

  l_scrtext_m = i_coltext.
  l_scrtext_s = i_coltext.

  try.
      eo_column ?= io_columns->get_column( i_colname ).
      eo_column->set_long_text( i_coltext ).
      eo_column->set_medium_text( l_scrtext_m ).
      eo_column->set_short_text( l_scrtext_s ).
    catch cx_salv_not_found.
      message e999(zh) with 'No column found with the given entry'.
  endtry.

endform.                    " SET_COLUMN_NAMES
*&---------------------------------------------------------------------*
*&      Form  CYCLE_MATCHUP_AWART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cycle_matchup_awart .
  types: begin of ty_alv_op,
           ccycl type ccycl,
           abart type abrar,
           lgart type lgart,
           vargt type vrarg,
           seqno type seqln,
           mode  type c,
           opt1  type char10,
           opt2  type char10,
           opt3  type char10,
           opt4  type char10,
           opt5  type char10,
           opt6  type char10,
         end of ty_alv_op,
         ty_t_alv_op type table of ty_alv_op.

  data: lt_t52c5    type standard table of t52c5,
        lt_t52ba    type standard table of t52ba,
        lt_alv_op   type ty_t_alv_op,
        lw_alv_op   type ty_alv_op,
        lw_t52ba    type t52ba,
        lw_t52c5    type t52c5,
        lt_r_selopt type table of selopt,
        lw_r_selopt type selopt,
        l_string    type string,
        l_tabix     type sytabix.

  data: lo_table      type ref to cl_salv_table,
        lo_header     type ref to cl_salv_form_layout_grid,
        lo_h_flow     type ref to cl_salv_form_layout_flow,
        lo_columns    type ref to cl_salv_columns,
        lo_column     type ref to cl_salv_column_table,
        lo_functions  type ref to cl_salv_functions_list,
        lo_selections type ref to cl_salv_selections,
        lo_salv_msg   type ref to cx_salv_msg,
        lo_sorts      type ref to cl_salv_sorts,
        lo_layout     type ref to cl_salv_layout,
        l_text        type txt255,
        lw_key        type salv_s_layout_key.

  clear absence.
  l_tabix = sy-curow - 2.
  read table abtype index l_tabix.
  move abtype-subty to absence.
  lw_r_selopt-sign   = 'I'.
  lw_r_selopt-option = 'CP'.
  if absence is initial.
    write: 'Please select the Absence type and click on this button.'.
    return.
  endif.
  l_string = '*' && absence && '*'.
  lw_r_selopt-low    = l_string.
  append lw_r_selopt to lt_r_selopt.

  select * from t52c5
           into table lt_t52c5
           where vargt in lt_r_selopt
              or vinfo in lt_r_selopt.
  if sy-subrc = 0.
    refresh lt_r_selopt[].
    loop at lt_t52c5 into lw_t52c5.
      clear lw_r_selopt-low.
      lw_r_selopt-low    = lw_t52c5-ccycl.
      append lw_r_selopt to lt_r_selopt.
    endloop.
    sort lt_r_selopt by low.
    delete adjacent duplicates from lt_r_selopt comparing low.
    select * into table lt_t52ba
             from t52ba
             where potyp = 'CYCL'
             and   ponam in lt_r_selopt
             and   pattr = 'CNT'
            and    pwert  = '*' .
    if sy-subrc = 0.
      refresh lt_r_selopt[].
      clear lw_r_selopt-low.
      loop at lt_t52ba into lw_t52ba.
        clear lw_r_selopt-low.
        lw_r_selopt-low    = lw_t52ba-ponam.
        append lw_r_selopt to lt_r_selopt.
      endloop.
      delete lt_t52c5 where ccycl not in lt_r_selopt.
      loop at lt_t52c5 into lw_t52c5.
        write: lw_t52c5-ccycl to lw_alv_op-ccycl,
               lw_t52c5-abart to lw_alv_op-abart,
               lw_t52c5-lgart to lw_alv_op-lgart,
               lw_t52c5-vargt to lw_alv_op-vargt,
               lw_t52c5-seqno to lw_alv_op-seqno,
               lw_t52c5-vinfo(1) to lw_alv_op-mode,
               lw_t52c5-vinfo+1(10) to lw_alv_op-opt1,
               lw_t52c5-vinfo+12(10) to lw_alv_op-opt2,
               lw_t52c5-vinfo+23(10) to lw_alv_op-opt3,
               lw_t52c5-vinfo+34(10) to lw_alv_op-opt4,
               lw_t52c5-vinfo+45(10) to lw_alv_op-opt5,
               lw_t52c5-vinfo+56(10) to lw_alv_op-opt6.
        append lw_alv_op to lt_alv_op.
      endloop.
    else.
      write: 'The Absence Type is not used in any Rules for this country.'.
    endif.
  else.
    write: 'The Absence Type is not used in any Rules.'(011).
  endif.

  if lt_alv_op[] is not initial.
*   Header object
    create object lo_header.
*   Get New Instance for ALV Table Object
    try.
        cl_salv_table=>factory(
        exporting
          list_display   = if_salv_c_bool_sap=>false
        importing
          r_salv_table = lo_table
        changing
          t_table      = lt_alv_op ).
      catch cx_salv_msg into lo_salv_msg.
        message e000 with lo_salv_msg->msgv1
        lo_salv_msg->msgv2
        lo_salv_msg->msgv3
        lo_salv_msg->msgv4.
    endtry.

*   Get the coloumns from the table
    lo_columns = lo_table->get_columns( ).
*   Optimise the coloumns width
    lo_columns->set_optimize( ).
*   Functions
    lo_functions = lo_table->get_functions( ).
    lo_functions->set_all( value = abap_true ).
*   Heading for the report
    l_text = 'Rules using the specified Absence types'.
    lo_h_flow = lo_header->create_flow( row = 1  column = 1 ).
    lo_h_flow->create_text( text = l_text ).

*   Selections
    lo_selections = lo_table->get_selections( ).
    lo_selections->set_selection_mode(
    cl_salv_selections=>if_salv_c_selection_mode~row_column ).
*   Set the column headings on the output
    perform set_column_names
            using lo_columns: 'CCYCL' 'Rule'
                              changing lo_column,
                              'ABART' 'Employee subgroup grouping'
                              changing lo_column,
                              'LGART' 'Wage Type'
                              changing lo_column,
                              'VARGT' 'Variable Key'
                              changing lo_column,
                              'SEQNO' 'Next Line'
                              changing lo_column,
                              'MODE' 'Mode'
                              changing lo_column,
                              'OPT1' 'Operation'
                              changing lo_column,
                              'OPT2' 'Operation'
                              changing lo_column,
                              'OPT3' 'Operation'
                              changing lo_column,
                              'OPT4' 'Operation'
                              changing lo_column,
                              'OPT5' 'Operation'
                              changing lo_column,
                              'OPT6' 'Operation'
                              changing lo_column.

    call method lo_table->get_layout
      receiving
        value = lo_layout.
    lw_key-report = sy-repid.
    lo_layout->set_key( lw_key ).
*   Set Layout selected on screen
    lo_layout->set_default( abap_true ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   Set the top of list using the header for Online.
    lo_table->set_top_of_list( lo_header ).
    lo_table->display( ).
  endif.

endform.                    " CYCLE_MATCHUP
