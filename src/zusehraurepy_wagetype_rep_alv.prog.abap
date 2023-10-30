*&---------------------------------------------------------------------*
*&  Include           ZUSEHRAUREPY_WAGETYPE_REP_ALV
*&---------------------------------------------------------------------*
data: lisel_1(79),  lisel_2(79), header(79), header_2(79).

data: begin of list occurs 100,        "Hilfstabelle
        klnum(2),
        klwrt     like t512t-lgart,
        lgtxt(72),
        lgart     like t512t-lgart,
      end of list.

data: begin of vkl1 occurs 500,
        sprsl    like t512u-sprsl,
        klnum(2),                     "klassen-nummer
        klwrt(2),                     "klassen-wert (auspraegung)
        kltxt    like t512u-dtext,
      end of vkl1.
data: begin of akl1 occurs 100,
        sprsl    like t512u-sprsl,
        klnum(2),                     "klassen-nummer
        klwrt(2),                     "klassen-wert (auspraegung)
        kltxt    like t512u-dtext,
      end of akl1.

data:
  char96(96),
  hexin(12)  type x,
  hex        type x, hextm type x,
  x128       type x value '80',
  switch,
  ps         type p, pc type p,
  lvnam      like t599i-lvnam,
  begdat     type d,
  enddat     type d,
  it599i     like t599i occurs 1 with header line,
  gt_t512w   type standard table of t512w,
  gs_t512w   type t512w,
  gt_t512t   type standard table of t512t,
  gs_t512t   type t512t,
  gt_t510j   type standard table of t510j,
  gs_t510j   type t510j,
  gt_t503t   type standard table of t503t,
  gs_t503t   type t503t,
  gt_t52dz   type standard table of t52dz,
  gs_t52dz   type t52dz,
  gt_usr21   type standard table of usr21,
  gs_usr21   type usr21,
  gt_adrp    type standard table of adrp,
  gs_adrp    type adrp,
  gv_count   type i.

data: gt_ps_wtg_dv type table of zuse_ps_wtg_dv,
      gs_ps_wtg_dv type zuse_ps_wtg_dv.

tables: t599i.

data:  begin of bw occurs 500.
         include structure t512w.
         data:    kumula       like char96,
         kumuflag(1),
         d1durch      like char96(32),
         d2durch      like char96(32),
         d3durch      like char96(32),
         d4durch      like char96(32),
         d5durch      like char96(32),
         durchflag(1),
         aklflag(1),
       end of bw.

data: begin of ku occurs 096,          "Kumulationen
        lgart like t512t-lgart,
        lgtxt like t512t-lgtxt,
      end of ku.

data: begin of dg occurs 032,          "Durchschnitts-Grundlagen
        lgart like t512t-lgart,
        lgtxt like t512t-lgtxt,
      end of dg.

data: begin of bg occurs 100,          "Bewertungs-Grundlagen
        lgart like t512t-lgart,
        lgtxt like t512t-lgtxt,
      end of bg.

*&---------------------------------------------------------------------*
*&  Include  ZZ_WAGETYPE_REPORT_ALV
*&---------------------------------------------------------------------*
data: g_container        type scrfname value 'ALV_LIST_CONT1',
      ok_code            like sy-ucomm,
      grid1              type ref to cl_gui_alv_grid,
      g_custom_container type ref to cl_gui_custom_container,
      gs_layout          type lvc_s_layo,
      gt_fieldcatalog    type lvc_t_fcat,
      gs_fieldcatalog    type lvc_s_fcat.

types: begin of ty_s_alv,
         molga type molga,
         klnum type char2,
         kntxt type prclt,
         klwrt type auspa,
         kwtxt type prcvt,
         lgart type lgart,
         lgtxt type lgtxt,
       end of ty_s_alv.

types: ty_s_alvc type zuse_ps_wtg_dv.

types: begin of ty_s_alvm,
         clart     type lgart,
         mlart     type lgart,
         cltxt     type lgtxt,
         mltxt     type lgtxt,
         udate     type sydatum,
         uname     type syuname,
         name_text type ad_namtext,
       end of ty_s_alvm.

types:
  begin of ty_s_alvw,
    lgart    type lgart,
    lgtxt    type lgtxt,
    valm(30) type c,  "valuation method - rate / constant
    wert1    type wert1,
    rlart    type lgart,
    rltxt    type lgtxt,
    gvala    type gvala,
    sltxt    type lgtxt,
    gvpro    type gvpro,
  end of ty_s_alvw.

data:
  list_alv  type table of ty_s_alv,
  gs_alv    type ty_s_alv,
  list_alvc type table of ty_s_alvc,
  gs_alvc   type ty_s_alvc,
  list_alvm type table of ty_s_alvm,
  gs_alvm   type ty_s_alvm,
  list_alvw type table of ty_s_alvw,
  gs_alvw   type ty_s_alvw.

data: i500p like t500p occurs 1 with header line.

*&---------------------------------------------------------------------*
*&      Form  LINE-SEL-ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form line-sel-alv.
  condense lisel_1.
  refresh list.
  clear list.
  refresh list_alv.
  clear list_alv.
  case lisel_1.
    when text-002.
      perform persbe_list.
      perform liste_2.
    when text-000.
      perform sc512w-vkl-alv.
      perform set_fieldcat_vkl.
      if not grid1 is initial.
        call method grid1->set_frontend_fieldcatalog
          exporting
            it_fieldcatalog = gt_fieldcatalog.
        call method grid1->set_gridtitle
          exporting
            i_gridtitle = 'Meaning of processing classes and their values'(000).
      endif.
      call screen 100.
    when text-008.
      perform sc512w-akl1-alv.
      perform set_fieldcat_akl1.
      if not grid1 is initial.
        call method grid1->set_frontend_fieldcatalog
          exporting
            it_fieldcatalog = gt_fieldcatalog.
        call method grid1->set_gridtitle
          exporting
            i_gridtitle = 'Meaning of evaluation classes and their values'(008).
      endif.
      call screen 100.
    when text-001.
      perform sc512w-ku-alv.
      perform set_fieldcat_ku.
      if not grid1 is initial.
        call method grid1->set_frontend_fieldcatalog
          exporting
            it_fieldcatalog = gt_fieldcatalog.
        call method grid1->set_gridtitle
          exporting
            i_gridtitle = 'Meaning of cumulation wage types'(001).
      endif.
      call screen 100.
    when text-007.
      perform sc512w-dg-alv.
      perform set_fieldcat_dg.
      if not grid1 is initial.
        call method grid1->set_frontend_fieldcatalog
          exporting
            it_fieldcatalog = gt_fieldcatalog.
        call method grid1->set_gridtitle
          exporting
            i_gridtitle = 'Meaning of average bases'(007).
      endif.
      call screen 100.
    when text-009.
      perform sc512w-bg-alv.
      perform set_fieldcat_bg.
      if not grid1 is initial.
        call method grid1->set_frontend_fieldcatalog
          exporting
            it_fieldcatalog = gt_fieldcatalog.
        call method grid1->set_gridtitle
          exporting
            i_gridtitle = 'Use of valuation bases'(009).
      endif.
      call screen 100.
    when others.
  endcase.
endform.                    "LINE-SEL-ALV

*---------------------------------------------------------------------*
*       FORM SC512W-vkl1-ALV                                               *
*---------------------------------------------------------------------*
form sc512w-vkl-alv.
  loop at vkl1 where klwrt = space.
    gs_alv-kntxt = vkl1-kltxt.
    loop at bw.
      move-corresponding bw to t512w.
      ps = vkl1-klnum - 1.
      shift t512w-vklas by ps places.
      if t512w-vklas(1) ne space.
        t512w-vklas+1(1) = space.        "2-stellige ausgabe
        perform prlgart_alv using vkl1-klnum t512w-vklas(2) 'VKL'.
      endif.
    endloop.
  endloop.
endform.                               "end of SC512W-vkl1-ALV.

*---------------------------------------------------------------------*
*       FORM SC512W-AKL1-ALV                                              *
*---------------------------------------------------------------------*
form sc512w-akl1-alv.
  loop at akl1 where klwrt = space.
    gs_alv-kntxt = akl1-kltxt.
    loop at bw.
      if bw-aklflag is initial.
        move-corresponding bw to t512w.
        ps = ( akl1-klnum - 1 ) * 2.
        shift t512w-aklas by ps places.
        if t512w-aklas(2) ne space.
          perform prlgart_alv using akl1-klnum t512w-aklas(2) 'AKL1'.
        endif.
      endif.
    endloop.
  endloop.
endform.                               "end of SC512W-AKL1-ALV.

*---------------------------------------------------------------------*
*       FORM SC512W-KU-ALV                                                *
*---------------------------------------------------------------------*
form sc512w-ku-alv.
  loop at ku.
    loop at bw.
      if not bw-kumuflag is initial.
        move-corresponding bw to t512w.
        ps = ku-lgart+2(2) - 1.
        char96 = bw-kumula.
        shift char96 by ps places.
        if char96(1) eq '1'.
          perform prlgart_alv using ku-lgart+2(2) '  ' 'KU'.
        endif.
      endif.
    endloop.
  endloop.
endform.                               "end of SC512W-KU-ALV.

*---------------------------------------------------------------------*
*       FORM SC512W-DG-ALV                                                *
*---------------------------------------------------------------------*
form sc512w-dg-alv.
  loop at dg.
    loop at bw.
      if not bw-durchflag is initial.
        move-corresponding bw to t512w.
        switch = 'N'.
        ps = dg-lgart+2(2) - 1.
        shift bw-d1durch by ps places.
        shift bw-d2durch by ps places.
        shift bw-d3durch by ps places.
        shift bw-d4durch by ps places.
        shift bw-d5durch by ps places.
        if bw-d1durch(1) eq '1'.
          switch = 'J'.
        elseif bw-d2durch(1) eq '1'.
          switch = 'J'.
        elseif bw-d3durch(1) eq '1'.
          switch = 'J'.
        elseif bw-d4durch(1) eq '1'.
          switch = 'J'.
        elseif bw-d5durch(1) eq '1'.
          switch = 'J'.
        endif.
        if switch eq 'J'.
          perform prlgart_alv using dg-lgart+2(2) '  ' 'DG'.
        endif.
      endif.
    endloop.
  endloop.
endform.                               "end of SC512W-DG-ALV.

*---------------------------------------------------------------------*
*       FORM SC512W-BG-ALV                                                *
*---------------------------------------------------------------------*
form sc512w-bg-alv.
  loop at bg.
    loop at bw.
      move-corresponding bw to t512w.
      if t512w-gvbla eq bg-lgart+2(2).   "Grungverguetung
        perform prlgart_alv using bg-lgart+2(2) 'GV' 'BG'.
      endif.
      if t512w-pzbla eq bg-lgart+2(2).   "steuerpfl. Zuschlag
        perform prlgart_alv using bg-lgart+2(2) 'PZ' 'BG'.
      endif.
      if t512w-fzbla eq bg-lgart+2(2).   "steuerfreier Zuschlag
        perform prlgart_alv using bg-lgart+2(2) 'FZ' 'BG'.
      endif.
    endloop.
  endloop.
endform.                               "end of SC512W-BG-alv.

*---------------------------------------------------------------------*
*       FORM PRLGART_ALV                                                  *
*---------------------------------------------------------------------*
form prlgart_alv using nummer wert class.
  select single * from t512t where sprsl eq sy-langu
                               and molga eq gs_alv-molga
                               and lgart eq t512w-lgart.
  if sy-subrc ne 0. t512t-lgtxt = '(N.N.)'. endif. "text not found
  gs_alv-klnum = nummer.
  gs_alv-klwrt = wert.
  gs_alv-lgart = t512w-lgart.
  gs_alv-lgtxt = t512t-lgtxt.
  case class.
    when 'VKL'.
      read table vkl1 with key klnum = nummer
                              klwrt = wert.
      gs_alv-kwtxt = vkl1-kltxt.
    when 'AKL'.
      read table akl1 with key klnum = nummer
                              klwrt = wert.
      gs_alv-kwtxt = akl1-kltxt.
    when 'KU'.
      concatenate ku-lgart ku-lgtxt into gs_alv-kntxt
                  separated by space.
    when 'DG'.
      concatenate dg-lgart dg-lgtxt into gs_alv-kntxt
                  separated by space.
    when 'BG'.
      concatenate bg-lgart bg-lgtxt into gs_alv-kntxt
                  separated by space.
  endcase.
  append gs_alv to list_alv.
endform.                               "end of PRLGART_ALV

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATALOG_VKL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_vkl1 .

  gs_layout-grid_title = 'Bedeutung der Verarbeitungs-Klassen und ihrer Werte'(000).
  clear gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'MOLGA'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'MOLGA'.
  gs_fieldcatalog-ref_field = 'MOLGA'.
  gs_fieldcatalog-ref_table = 'T52D9'.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-no_out = 'X'.
  gs_fieldcatalog-checktable = 'T500L'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLNUM'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'VKLAX'.
  gs_fieldcatalog-ref_field = 'PRCLS'.
  gs_fieldcatalog-ref_table = 'T52D9'.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-lzero = 'X'.
  gs_fieldcatalog-checktable = 'T52D1'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KNTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'PRCLT'.
  gs_fieldcatalog-outputlen = 33.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLWRT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'AUSPV'.
  gs_fieldcatalog-ref_field = 'PRCLV'.
  gs_fieldcatalog-ref_table = 'T52D9'.
  gs_fieldcatalog-outputlen = 2.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KWTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'PRCVT'.
  gs_fieldcatalog-outputlen = 36.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGART'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
endform.                    " SET_FIELDCAT_VKL

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATALOG_AKL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_akl1 .

  gs_layout-grid_title = text-008.
  clear gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'MOLGA'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'MOLGA'.
  gs_fieldcatalog-ref_field = 'MOLGA'.
  gs_fieldcatalog-ref_table = 'T52DB'.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-no_out = 'X'.
  gs_fieldcatalog-checktable = 'T500L'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLNUM'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'AKL1AX'.
  gs_fieldcatalog-ref_field = 'EVCLS'.
  gs_fieldcatalog-ref_table = 'T52DB'.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-lzero = 'X'.
  gs_fieldcatalog-checktable = 'T52D3'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KNTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'EVCLT'.
  gs_fieldcatalog-outputlen = 34.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLWRT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'AUSPA'.
  gs_fieldcatalog-ref_field = 'EVCLV'.
  gs_fieldcatalog-ref_table = 'T52DB'.
  gs_fieldcatalog-outputlen = 2.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KWTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'EVCVT'.
  gs_fieldcatalog-outputlen = 35.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGART'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
endform.                    " SET_FIELDCAT_AKL1

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATALOG_KU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_ku .

  gs_layout-grid_title = text-001.
  clear gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'MOLGA'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLNUM'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-reptext = text-023.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-lzero = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KNTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-reptext = text-023.
  gs_fieldcatalog-outputlen = 35.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLWRT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KWTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGART'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
endform.                    " SET_FIELDCAT_KU

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATALOG_DG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_dg.

  gs_layout-grid_title = 'Bedeutung der Durchschnittsgrundlagen'(007).
  clear gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'MOLGA'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLNUM'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-reptext = 'Durchschnittsgrundlagen'(024).
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-lzero = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KNTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-reptext = 'Durchschnittsgrundlagen'(024).
  gs_fieldcatalog-outputlen = 35.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLWRT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KWTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGART'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
endform.                    " SET_FIELDCAT_DG

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATALOG_BG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_bg.

  gs_layout-grid_title = 'Verwendung der Bewertungsgrundlagen'(009).
  clear gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'MOLGA'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLNUM'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-reptext = 'Bewertungsgrundlagen'(025).
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-lzero = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KNTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-reptext = 'Bewertungsgrundlagen'(025).
  gs_fieldcatalog-outputlen = 35.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLWRT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KWTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-no_out = 'X'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGART'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
endform.                    " SET_FIELDCAT_BG

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pbo output.
*  SET PF-STATUS '0100'.
  case sy-ucomm.
    when others.
      set pf-status 'VKLA'.
  endcase.

  if g_custom_container is initial.
    create object g_custom_container
      exporting
        container_name = g_container.
    create object grid1
      exporting
        i_parent = g_custom_container.

    case sy-ucomm.
      when 'PSID'.
        call method grid1->set_table_for_first_display
          exporting
            is_layout       = gs_layout
          changing
            it_outtab       = list_alvc
            it_fieldcatalog = gt_fieldcatalog.

      when 'MOWT'.
        call method grid1->set_table_for_first_display
          exporting
            is_layout       = gs_layout
          changing
            it_outtab       = list_alvm
            it_fieldcatalog = gt_fieldcatalog.

      when 'VALC'.
        call method grid1->set_table_for_first_display
          exporting
            is_layout       = gs_layout
          changing
            it_outtab       = list_alvw
            it_fieldcatalog = gt_fieldcatalog.

      when others.
        call method grid1->set_table_for_first_display
          exporting
            is_layout       = gs_layout
          changing
            it_outtab       = list_alv
            it_fieldcatalog = gt_fieldcatalog.
    endcase.
  else.

    case sy-ucomm.
      when 'PSID'.
        call method grid1->set_table_for_first_display
          exporting
            is_layout       = gs_layout
          changing
            it_outtab       = list_alvc
            it_fieldcatalog = gt_fieldcatalog.

      when 'MOWT'.
        call method grid1->set_table_for_first_display
          exporting
            is_layout       = gs_layout
          changing
            it_outtab       = list_alvm
            it_fieldcatalog = gt_fieldcatalog.

      when 'VALC'.
        call method grid1->set_table_for_first_display
          exporting
            is_layout       = gs_layout
          changing
            it_outtab       = list_alvw
            it_fieldcatalog = gt_fieldcatalog.

      when others.
        call method grid1->set_table_for_first_display
          exporting
            is_layout       = gs_layout
          changing
            it_outtab       = list_alv
            it_fieldcatalog = gt_fieldcatalog.
    endcase.

    call method grid1->refresh_table_display.
  endif.
  call method cl_gui_control=>set_focus exporting control = grid1.
endmodule.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai input.
  case ok_code.
    when 'EXIT' or 'BACK' or 'CANC'.
      set screen 0. leave screen.
    when others.
  endcase.
  clear ok_code.
endmodule.                 " PAI  INPUT

*---------------------------------------------------------------------*
*       FORM LISTE_2                                                  *
*---------------------------------------------------------------------*
form liste_2.
  data: flag(1) value ' '.
  data: linecount type i.
  clear list.
  describe table list lines linecount.
  if linecount ne 0.
    loop at list.
      if flag = ' '.
        format intensified off color col_normal. flag = 'X'.
      else.
        format intensified on color col_normal. flag = ' '.
      endif.
      write: /2 list-lgart,
              9 list-lgtxt+0(71).
    endloop.
  else.
    perform no_lines_in_table.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM NO_LINES_IN_TABLE                                        *
*---------------------------------------------------------------------*
*       leere Liste soll nicht angezeigt werden                       *
*---------------------------------------------------------------------*
form no_lines_in_table.
  if 1 = 1.   "MODUS_LI IS INITIAL.
    format intensified off color col_normal.
  endif.
  header = text-108.
  write: / header.
endform.

*----------------------------------------------------------------------*
*       Form  PERSBE_LIST
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
form persbe_list.
  loop at i500p.
    clear list.
    list-lgart = i500p-persa.
    list-lgtxt = i500p-name1.
    append list.
  endloop.
endform.                    " PERSBE_LIST

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATALOG_VKL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_vkl .

  gs_layout-grid_title = text-000.
  clear gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'MOLGA'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'MOLGA'.
  gs_fieldcatalog-ref_field = 'MOLGA'.
  gs_fieldcatalog-ref_table = 'T52D9'.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-no_out = 'X'.
  gs_fieldcatalog-checktable = 'T500L'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLNUM'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'VKLAX'.
  gs_fieldcatalog-ref_field = 'PRCLS'.
  gs_fieldcatalog-ref_table = 'T52D9'.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-lzero = 'X'.
  gs_fieldcatalog-checktable = 'T52D1'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KNTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'PRCLT'.
  gs_fieldcatalog-outputlen = 33.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLWRT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'AUSPV'.
  gs_fieldcatalog-ref_field = 'PRCLV'.
  gs_fieldcatalog-ref_table = 'T52D9'.
  gs_fieldcatalog-outputlen = 2.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KWTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'PRCVT'.
  gs_fieldcatalog-outputlen = 36.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGART'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
endform.                    " SET_FIELDCAT_VKL

*&---------------------------------------------------------------------*
*&      Form  WRITE_FROM_VKL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_from_vkl1 .
  data:
    lv_title type text70.
  perform sc512w-vkl-alv.
  perform set_fieldcat_vkl.
  if not grid1 is initial.
    call method grid1->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = gt_fieldcatalog.
    lv_title = text-000.
    call method grid1->set_gridtitle
      exporting
        i_gridtitle = lv_title.
  endif.
  call screen 100.
endform.

tables: t52d1, t52d2, t52d3, t52d4.
form re512u.
*---------------------------------------------------------------------*
*  sammeln der Auswertungsklassen in der Tabelle AKL                  *
*---------------------------------------------------------------------*
  select * from  t52d3                 "lesen aller Auswertungsklassen
         where  molga = modlgart.
    akl1-klnum  = t52d3-evcls.
    akl1-klwrt  = ' '.
    akl1-kltxt  = ' '.

    select * from  t52da               "lesen des Textes zur Auswertungs
           where  sprsl  = sy-langu    "klasse
           and    molga  = modlgart
           and    evcls  = t52d3-evcls.
      akl1-kltxt = t52da-evclt.
    endselect.
    append akl1.

    select * from t52d4                "lesen aller Ausprägungen zu
           where  molga  = modlgart    "einer Auswertungsklasse
           and    evcls  = t52d3-evcls.
      akl1-klnum  = t52d3-evcls.
      akl1-klwrt  = t52d4-evclv.
      akl1-kltxt  = ' '.

      select * from  t52db             "lesen der Texte zu den Ausprä-
             where  sprsl  = sy-langu  "gungen der Auswertungsklassen
             and    molga  = modlgart
             and    evcls  = t52d3-evcls
             and    evclv  = t52d4-evclv.
        akl1-kltxt = t52db-evcvt.
      endselect.

      if not ( akl1-klwrt = space ).                        "GWY651319
        append akl1.
      endif.                                                "GWY651319

    endselect.
  endselect.

  sort akl1 by klnum klwrt.

*>>> Comment out by SA
* override text
*  data:
*    "lt_text type STANDARD TABLE OF ZZHR_WAGETYPE_TT,
*    ls_text type ZZHR_WAGETYPE_TT.
*
*  select * from ZZHR_WAGETYPE_TT into table lt_text
*    where molga = modlgart
*      and zzchar = 'E'.
*  sort lt_text.


*  loop at akl1.
*    READ TABLE lt_text into ls_text with key evcls = akl1-klnum evclv = akl1-klwrt BINARY SEARCH.
*    if sy-subrc = 0.
*      akl1-kltxt = ls_text-ztext.
*      modify akl1.
*    endif.
*  ENDLOOP.
*<<< Cooment from SA
*---------------------------------------------------------------------*
*   Sammeln der Verarbeitungsklassen in der Tabelle VKL               *
*---------------------------------------------------------------------*
  select * from  t52d1                 "lesen der Verarbeitungsklassen
         where  molga = modlgart.
    vkl1-klnum  = t52d1-prcls.
    vkl1-klwrt  = ' '.
    vkl1-kltxt  = ' '.

    select * from  t52d8               "lesen des Textes der Verarbeit
           where  sprsl  = sy-langu    "ungsklasse
           and    molga  = modlgart
           and    prcls  = t52d1-prcls.
      vkl1-kltxt = t52d8-prclt.
    endselect.
    append vkl1.

    select * from t52d2                "lesen der Ausprägungen zu
           where  molga  = modlgart    "einer Verarbeitungsklasse
           and    prcls  = t52d1-prcls.
      vkl1-klnum  = t52d1-prcls.
      vkl1-klwrt  = t52d2-prclv.
      vkl1-kltxt  = ' '.

      select * from  t52d9             "lesen des Textes zu der
             where  sprsl  = sy-langu  "Ausprägung der Vklasse
             and    molga  = modlgart
             and    prcls  = t52d1-prcls
             and    prclv  = t52d2-prclv.
        vkl1-kltxt = t52d9-prcvt.
      endselect.

      if not ( vkl1-klwrt = space ).                        "GWY385065
        append vkl1.
      endif.                                                "GWY385065
    endselect.

  endselect.
  sort vkl1.

*>>> Comment out by SA
* override text
*  clear: lt_text.
*
*  select * from ZZHR_WAGETYPE_TT into table lt_text
*    where molga = modlgart
*      and zzchar = 'P'.
*  sort lt_text.
*
*  loop at vkl1.
*    READ TABLE lt_text into ls_text with key prcls = vkl1-klnum prclv = vkl1-klwrt BINARY SEARCH.
*    if sy-subrc = 0.
*      vkl1-kltxt = ls_text-ztext.
*      modify vkl1.
*    endif.
*  ENDLOOP.
*<<< Comment out by SA

endform.

*---------------------------------------------------------------------*
*       FORM RE512W                                                   *
*---------------------------------------------------------------------*
form re512w.

  select * from t512w where molga eq modlgart and lgart in lgart.   "SEL.
    if t512w-endda ge begdat and t512w-begda le enddat.
      move t512w to bw.
      if bw-aklas is initial. bw-aklflag = 'X'. endif.
      clear: char96, hexin.
      hexin = t512w-kumul.
      perform hexinchar1 using 12.
      bw-kumula = char96.
      if not bw-kumula is initial.
        bw-kumuflag = 'X'.
      endif.
      clear: char96, hexin.
      hexin(4) = t512w-d1kum.
      perform hexinchar1 using 4.
      bw-d1durch = char96(32).
      if not bw-d1durch is initial. bw-durchflag = 'X'. endif.
      clear: char96, hexin.
      hexin(4) = t512w-d2kum.
      perform hexinchar1 using 4.
      bw-d2durch = char96(32).
      if not bw-d2durch is initial. bw-durchflag = 'X'. endif.
      clear: char96, hexin.
      hexin(4) = t512w-d3kum.
      perform hexinchar1 using 4.
      bw-d3durch = char96(32).
      if not bw-d3durch is initial. bw-durchflag = 'X'. endif.
      clear: char96, hexin.
      hexin(4) = t512w-d4kum.
      perform hexinchar1 using 4.
      bw-d4durch = char96(32).
      if not bw-d4durch is initial. bw-durchflag = 'X'. endif.
      clear: char96, hexin.
      hexin(4) = t512w-d5kum.
      perform hexinchar1 using 4.
      bw-d5durch = char96(32).
      if not bw-d5durch is initial. bw-durchflag = 'X'. endif.
      append bw.
    endif.
  endselect.
endform.                               "end of RE512W.

*---------------------------------------------------------------------*
*       FORM HEXINCHAR                                                *
*---------------------------------------------------------------------*
form hexinchar1 using anzahl.           "und Feld HEXIN.
  if not hexin is initial.
* maximale Laenge HEXINPUT ist 12 = L'HEXIN bzw. L'CHAR96/8 s. a. MAX
    do anzahl times
      varying hex from hexin(1) next hexin+1(1) range hexin.  "UC XMS
      shift char96 by 8 places.
      ps = 0.                            "set sum
      pc = 10000000.                     "set char
      hextm = x128.                      "set hex
      do 8 times.
        if hex o hextm. ps = ps + pc. endif.
        hextm = hextm / 2. pc = pc / 10.
      enddo.
      unpack ps to char96+88(8).
    enddo.
    ps = ( 12 - anzahl ) * 8. "MAX = 12, maximale Laenge HEXINPUT
    shift char96 by ps places.           "linksbuendig nach CHAR96
  endif.
endform.                               "END OF HEXINCHAR

*---------------------------------------------------------------------*
*       FORM DOKMOLGA                                                 *
*---------------------------------------------------------------------*
*       Hauptroutine:  Lesen der Tabellen                             *
*                      Verzweigen zur Listanzeige                     *
*---------------------------------------------------------------------*
form dokmolga.
  perform re512u.                      "AKLA und VKLA lesen
  perform re512w.
  perform re512t-ku.
  perform re512t-dg.
  perform re512t-bg.

  perform ret599i.
  perform re500p.

  perform payslip_wt_grouping.

  select * from t512w into table gt_t512w
    where molga = modlgart
      and lgart in lgart
      and begda <= begdat
      and endda >= begdat.
  sort gt_t512w by lgart.

  select * from t510j into table gt_t510j
    where molga = modlgart
      and lgart in lgart
      and begda <= begdat
      and endda >= begdat.
  sort gt_t510j by lgart.

  select * from t512t into table gt_t512t
    where sprsl = sy-langu
      and molga = modlgart.
  sort gt_t512t by lgart.

  select * from t503t into table gt_t503t
    where sprsl = sy-langu.
  sort gt_t503t by persk.

  select * from t52dz into table gt_t52dz
    where molga = modlgart
      and clart in lgart.

  select * from usr21 into table gt_usr21 for all entries in gt_t52dz
    where bname = gt_t52dz-uname.

  select * from adrp into table gt_adrp for all entries in gt_usr21
    where persnumber = gt_usr21-persnumber
      and date_from <= begdat
      and date_to >= begdat.

*  IF MODUS_BA EQ 'X'.              "Baumstruktur
*    PERFORM BAUMSTRUKTUR.
*  ENDIF.
*  IF MODUS_TA EQ 'X' or MODUS_AL EQ 'X'.  "Menu or ALV-Liste
*    SET PF-STATUS '0000'.
*    PERFORM BUILD_MENU.
*  ENDIF.
*  IF MODUS_LI EQ 'X'.              "Endlosliste
*    SET PF-STATUS '0000'.
*    PERFORM ENDLOS_LIST.
*  ENDIF.
endform.                               "end of DOKMOLGA.

*----------------------------------------------------------------------*
*       Form  RE500P
*----------------------------------------------------------------------*                                                         *
form re500p.
  select * from t500p into table i500p where molga = modlgart.
endform.                    " RE500P

*---------------------------------------------------------------------*
*       FORM RE512T-BG                                                *
*---------------------------------------------------------------------*
form re512t-bg.
  data alphnum(36) type c.
  concatenate sy-abcde '0123456789' into alphnum.

  select * from t512t where sprsl eq sy-langu
                        and molga eq modlgart
                        and lgart like    '/0%'.
    check t512t-lgart+2(1) co alphnum.
    bg-lgart = t512t-lgart.
    bg-lgtxt = t512t-lgtxt.
    append  bg.
  endselect.
  bg = space.
  bg-lgart+2(2) = 'K '.
  bg-lgtxt = 'konst. Bewertung Tab 510J'(016).
  append  bg.
  bg-lgart+2(2) = 'T '.
  bg-lgtxt = 'Bewertung T510 o.Gr/St'(017).
  append  bg.
  bg-lgart+2(2) = 'TS'.
  bg-lgtxt = 'Bewertung T510 o.Stufe'(018).
  append  bg.
  bg-lgart+2(2) = 'TG'.
  bg-lgtxt = 'Bewertung T510'(019).
  append  bg.
endform.                               "end of RE512T-BG.

*---------------------------------------------------------------------*
*       FORM RE512T-DG                                                *
*---------------------------------------------------------------------*
form re512t-dg.
  select * from t512t where sprsl eq sy-langu
                        and molga eq modlgart
                        and lgart like '/2%'.
    check t512t-lgart+2(1) co '0123'.
    check t512t-lgart+2(2) co '0123456789'.
    if t512t-lgart+2(1) eq '3' and t512t-lgart+3(1) cn '012'.
      check 0 eq 1.
    endif.
    dg-lgart = t512t-lgart.
    dg-lgtxt = t512t-lgtxt.
    append  dg.
  endselect.
endform.                               "end of RE512T-DG.

*---------------------------------------------------------------------*
*       FORM RE512T-KU                                                *
*---------------------------------------------------------------------*
form re512t-ku.
  select * from t512t where sprsl eq sy-langu
                        and molga eq modlgart
                        and lgart like '/1%'.
    check t512t-lgart+2(2) co '0123456789'.
    ku-lgart = t512t-lgart.
    ku-lgtxt = t512t-lgtxt.
    append  ku.
  endselect.
endform.                               "end of RE512T-KU.

*---------------------------------------------------------------------*
*       FORM RET599I                                                  *
*---------------------------------------------------------------------*
*  Lesen der t599i                                                    *
*---------------------------------------------------------------------*
form ret599i.
  lvnam+0(2) = modlgart.
  lvnam+2(2) = '_%'.
  clear it599i.
  refresh it599i.
  select * from t599i where lvnam like lvnam.           "#EC CI_GENBUFF
    move-corresponding t599i to it599i.
    append it599i.
  endselect.
endform.
*&---------------------------------------------------------------------*
*&      Form  WRITE_FROM_AKL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_from_akl1 .
  perform sc512w-akl-alv.
  perform set_fieldcat_akl.
  if not grid1 is initial.
    call method grid1->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = gt_fieldcatalog.
    call method grid1->set_gridtitle
      exporting
        i_gridtitle = text-008.
  endif.
  call screen 100.
endform.
*&---------------------------------------------------------------------*
*&      Form  SC512W-AKL-ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sc512w-akl-alv .
  loop at akl1 where klwrt = space.
    gs_alv-kntxt = akl1-kltxt.
    loop at bw.
      if bw-aklflag is initial.
        move-corresponding bw to t512w.
        ps = ( akl1-klnum - 1 ) * 2.
        shift t512w-aklas by ps places.
        if t512w-aklas(2) ne space.
          perform prlgart_alv using akl1-klnum t512w-aklas(2) 'AKL'.
        endif.
      endif.
    endloop.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATALOG_AKL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_akl .

  gs_layout-grid_title = text-008.
  clear gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'MOLGA'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'MOLGA'.
  gs_fieldcatalog-ref_field = 'MOLGA'.
  gs_fieldcatalog-ref_table = 'T52DB'.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-no_out = 'X'.
  gs_fieldcatalog-checktable = 'T500L'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLNUM'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'AKLAX'.
  gs_fieldcatalog-ref_field = 'EVCLS'.
  gs_fieldcatalog-ref_table = 'T52DB'.
  gs_fieldcatalog-outputlen = 2.
  gs_fieldcatalog-lzero = 'X'.
  gs_fieldcatalog-checktable = 'T52D3'.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KNTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'EVCLT'.
  gs_fieldcatalog-outputlen = 34.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KLWRT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'AUSPA'.
  gs_fieldcatalog-ref_field = 'EVCLV'.
  gs_fieldcatalog-ref_table = 'T52DB'.
  gs_fieldcatalog-outputlen = 2.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'KWTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'EVCVT'.
  gs_fieldcatalog-outputlen = 35.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGART'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALV'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear gs_fieldcatalog.
endform.                    " SET_FIELDCAT_AKL
*&---------------------------------------------------------------------*
*&      Form  WRITE_FROM_KUM1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_from_kum1 .
  perform sc512w-ku-alv.
  perform set_fieldcat_ku.
  if not grid1 is initial.
    call method grid1->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = gt_fieldcatalog.
    call method grid1->set_gridtitle
      exporting
        i_gridtitle = text-001.
  endif.
  call screen 100.
endform.
*&---------------------------------------------------------------------*
*&      Form  PAYSLIP_WT_GROUPING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form payslip_wt_grouping.

  refresh gt_ps_wtg_dv.
  select * from zuse_ps_wtg_dv
    into corresponding fields of table gt_ps_wtg_dv
    where molga = p_molga
      and lgart in lgart
      and begda <= von
      and endda >= von.

  sort gt_ps_wtg_dv by molga lgart.
  delete adjacent duplicates from gt_ps_wtg_dv comparing all fields.

  sort gt_ps_wtg_dv by molga ps_category ps_wt_grp ps_grpwt_order lgart.

endform.
*&---------------------------------------------------------------------*
*&      Form  WRITE_PSID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_psid .

  perform get_psid_alv.
  perform set_fieldcat_psid.
  if not grid1 is initial.
    call method grid1->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = gt_fieldcatalog.
    call method grid1->set_gridtitle
      exporting
        i_gridtitle = text-020.
  endif.
  call screen 100.

endform.
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_PSID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_psid .
  data: lt_fieldcat type  slis_t_fieldcat_alv.
  data: ls_fieldcat type  line of slis_t_fieldcat_alv.

  gs_layout-grid_title = text-020.
  clear gt_fieldcatalog.  clear gs_fieldcatalog.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = sy-repid
      i_structure_name       = 'zuse_ps_wtg_dv'
    changing
      ct_fieldcat            = lt_fieldcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

  loop at lt_fieldcat into ls_fieldcat.
    move-corresponding ls_fieldcat to gs_fieldcatalog.
    gs_fieldcatalog-ref_field = ls_fieldcat-fieldname.
    gs_fieldcatalog-ref_table = 'zuse_ps_wtg_dv'.
    append gs_fieldcatalog to gt_fieldcatalog.
  endloop.


endform.
*&---------------------------------------------------------------------*
*&      Form  GET_PSID_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_psid_alv .

  loop at gt_ps_wtg_dv into gs_ps_wtg_dv.
    clear: gs_alvc.
    move-corresponding gs_ps_wtg_dv to gs_alvc.
    append gs_alvc to list_alvc.
  endloop.

  sort list_alvc by molga ps_grpwt_order endda.

endform.

form create_document.

  data:
    lv_molga type sdok_prv_s,
    lv_oname type sdok_prv_l.

  lv_molga = molga.
  lv_oname = lohnart.

  call function 'HRDSYS_OBJECT_CHECK_EXIST'
    exporting
      otype                 = 'WTYP'
      oname                 = lv_oname
      molga                 = lv_molga
    exceptions
      object_does_not_exist = 4
      others                = 6.
  case sy-subrc.
    when 0.                    " create the loio
      call function 'HRDSYS_LOIO_CREATE'
        exporting
          otype  = 'WTYP'
          oname  = lohnart
          molga  = molga
        exceptions
          others = 4.
      if sy-subrc <> 0.
        message id sy-msgid type 'I' number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        exit.
      else.
        call function 'HRDSYS_DOCU_EDIT'
          exporting
            otype           = 'WTYP'
            oname           = lohnart
            molga           = molga
            change_mode     = 'X'
          exceptions
            cancel_editor   = 4
            nothing_changed = 5
            others          = 6.
        case sy-subrc.
          when 4 or 5.
            message id sy-msgid type sy-msgty number sy-msgno
                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            exit.
          when 6.
            message id sy-msgid type 'I' number sy-msgno
                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            exit.
        endcase.
      endif.
    when others.
  endcase.
endform.
*&---------------------------------------------------------------------*
*&      Form  WRITE_MOWT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_mowt .
  perform get_mowt_alv.
  perform set_fieldcat_mowt.
  if not grid1 is initial.
    call method grid1->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = gt_fieldcatalog.
    call method grid1->set_gridtitle
      exporting
        i_gridtitle = text-021.
  endif.
  call screen 100.
endform.
*&---------------------------------------------------------------------*
*&      Form  GET_MOWT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_mowt_alv .
  sort: gt_t52dz by clart, gt_usr21 by bname, gt_adrp by persnumber.

  loop at gt_t52dz into gs_t52dz.
    clear gs_alvm.
    move-corresponding gs_t52dz to gs_alvm.
    read table gt_t512t into gs_t512t with key lgart = gs_alvm-clart binary search.
    if sy-subrc = 0.
      gs_alvm-cltxt = gs_t512t-lgtxt.
    endif.
    read table gt_t512t into gs_t512t with key lgart = gs_alvm-mlart binary search.
    if sy-subrc = 0.
      gs_alvm-mltxt = gs_t512t-lgtxt.
    endif.
    read table gt_usr21 into gs_usr21 with key bname = gs_alvm-uname binary search.
    if sy-subrc = 0.
      read table gt_adrp into gs_adrp with key persnumber = gs_usr21-persnumber binary search.
      if sy-subrc = 0.
        gs_alvm-name_text = gs_adrp-name_text.
      endif.
    endif.
    append gs_alvm to list_alvm.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_MOWT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_mowt .

  gs_layout-grid_title = text-021.

  clear gt_fieldcatalog.
  clear gs_fieldcatalog.

  gs_fieldcatalog-fieldname = 'CLART'.
  gs_fieldcatalog-tabname = 'LIST_ALVM'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'CLTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALVM'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.

  gs_fieldcatalog-fieldname = 'MLART'.
  gs_fieldcatalog-tabname = 'LIST_ALVM'.
  gs_fieldcatalog-rollname = 'P00_MLGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'MLTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALVM'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 25.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'UDATE'.
  gs_fieldcatalog-tabname = 'LIST_ALVM'.
  gs_fieldcatalog-rollname = 'SYDATUM'.
  gs_fieldcatalog-reptext = 'Change Date'.
  gs_fieldcatalog-outputlen = 10.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'UNAME'.
  gs_fieldcatalog-tabname = 'LIST_ALVM'.
  gs_fieldcatalog-rollname = 'SYUNAME'.
  gs_fieldcatalog-outputlen = 12.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'NAME_TEXT'.
  gs_fieldcatalog-tabname = 'LIST_ALVM'.
  gs_fieldcatalog-rollname = 'AD_NAMETEXT'.
  gs_fieldcatalog-outputlen = 40.
  append gs_fieldcatalog to gt_fieldcatalog.
endform.
*&---------------------------------------------------------------------*
*&      Form  WRITE_VALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_valc .
  perform get_valc_alv.
  perform set_fieldcat_valc.
  if not grid1 is initial.
    call method grid1->set_frontend_fieldcatalog
      exporting
        it_fieldcatalog = gt_fieldcatalog.
    call method grid1->set_gridtitle
      exporting
        i_gridtitle = text-022.
  endif.
  call screen 100.
endform.
*&---------------------------------------------------------------------*
*&      Form  GET_VALC_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_valc_alv .

  loop at gt_t512w into gs_t512w.

    clear gs_alvw.
    move-corresponding gs_t512w to gs_alvw.
    read table gt_t512t into gs_t512t with key lgart = gs_alvw-lgart binary search.
    if sy-subrc = 0.
      gs_alvw-lgtxt = gs_t512t-lgtxt.
    endif.

    perform check_vbase using gs_t512w-gvbla gs_t512w-gvala gs_t512w-gvpro.
    perform check_vbase using gs_t512w-pzbla gs_t512w-pzala gs_t512w-pzpro.
    perform check_vbase using gs_t512w-fzbla gs_t512w-fzala gs_t512w-fzpro.

  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT_VALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldcat_valc .
  gs_layout-grid_title = text-022.

  clear gt_fieldcatalog.
  clear gs_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGART'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-rollname = 'LGART'.
  gs_fieldcatalog-outputlen = 4.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'LGTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-rollname = 'LGTXT'.
  gs_fieldcatalog-outputlen = 20.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'VALM'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-reptext = 'Val.Method'.
  gs_fieldcatalog-outputlen = 30.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'RLART'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-reptext = 'Val.WT'.
  gs_fieldcatalog-outputlen = 8.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'RLTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-reptext = 'Val.WT.Name'.
  gs_fieldcatalog-outputlen = 20.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'WERT1'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-reptext = 'Cons.Value'.
  gs_fieldcatalog-outputlen = 11.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'GVALA'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-rollname = 'GVALA'.
  gs_fieldcatalog-outputlen = 10.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'SLTXT'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-reptext = 'StatementWTName'.
  gs_fieldcatalog-outputlen = 20.
  append gs_fieldcatalog to gt_fieldcatalog.

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname = 'GVPRO'.
  gs_fieldcatalog-tabname = 'LIST_ALVW'.
  gs_fieldcatalog-rollname = 'GVPRO'.
  gs_fieldcatalog-outputlen = 7.
  append gs_fieldcatalog to gt_fieldcatalog.

endform.
*&---------------------------------------------------------------------*
*&      Form  CHECK_VBASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_T512W_GVBLA  text
*      -->P_GS_T512W_GVALA  text
*      -->P_GS_T512W_GVPRO  text
*----------------------------------------------------------------------*
form check_vbase  using    value(p_vm)
                           value(p_lgart)
                           value(p_percent).

  check not p_vm is initial.

  clear: gs_alvw-valm, gs_alvw-rlart, gs_alvw-wert1.

  gs_alvw-gvala = p_lgart.
  if not p_lgart is initial.
    read table gt_t512t into gs_t512t with key lgart = p_lgart binary search.
    if sy-subrc = 0.
      gs_alvw-sltxt = gs_t512t-lgtxt.
    endif.
  endif.
  gs_alvw-gvpro = p_percent.

  if p_vm co '0123456789'.
    gs_alvw-valm = 'WageType'.
    gs_alvw-rlart = '/0' && p_vm.
    read table gt_t512t into gs_t512t with key lgart = gs_alvw-rlart binary search.
    if sy-subrc = 0.
      gs_alvw-rltxt = gs_t512t-lgtxt.
    endif.
    append gs_alvw to list_alvw.
    return.
  endif.

  case p_vm.
    when 'K'.
      gs_alvw-valm = 'Cons.Value'.
      read table gt_t510j with key lgart = gs_t512w-lgart into gs_t510j binary search.
      if sy-subrc = 0.
        gs_alvw-wert1 = gs_t510j-wert1.
      endif.
    when others.
      if p_vm+0(1) = 'T'.
        gs_alvw-valm = 'Pay-scale-Dependent (' && p_vm && ')'.
      else.
        gs_alvw-valm = 'Other (' && p_vm && ')'.
      endif.
  endcase.
  append gs_alvw to list_alvw.

endform.
