*----------------------------------------------------------------------*
*   INCLUDE H99CWTR0_SCR_DEF                                           *
*----------------------------------------------------------------------*
*N762901 Clean up for Selection Screens
* FELL6BK050334 30.10.2003 new comboboxes: current period / other period
****** screen definition ******
*hidden parameter: selection over payroll period, not over begda/endda
*>>> Start of TPY Enhancements
*parameters: ypernodt type xfeld no-display default space. "MOD001---
parameters: ypernodt type xfeld no-display default 'X'.  "MOD001+++
*<<< End of TPY Enhancements
*hidden parameter: hide off-cycle relevant fields.
*It can be used to call the programme with a SUBMIT, which set HIDE_OC
*to 'X'. This way, display of all off-cycle relevant fields will be
*disabled.
parameters: hide_oc type xfeld no-display default space.
*global fields for select options. they are declared here (and not used
*anywhere), so this include is independent of the data declaration
*include.
tables t512w.
tables pc261.
data absnum like h99cwtr-anzhl_absdiff.
data absamo like h99cwtr-betrg_absdiff.
data relnum like h99cwtr-anzhl_reldiff.
data relamo like h99cwtr-betrg_reldiff.
data payty_cal like pc261-payty.

*begin of frame for the selection period
selection-screen begin of block calc_periods with frame title label04.

selection-screen begin of line.
selection-screen comment 1(31) text-202 for field begd_cal modif id da1.
parameters: begd_cal like t549q-begda modif id da1.
selection-screen comment 52(5) text-095 for field endd_cal modif id da1.
parameters: endd_cal like t549q-endda modif id da1.
selection-screen end of line.

select-options s_pyty_c for payty_cal modif id da2.

parameters: begd_ref like t549q-begda no-display,
            endd_ref like t549q-endda no-display.

selection-screen begin of line.
selection-screen comment 33(20) text-s22 modif id pe1.
selection-screen position pos_high.
parameters: ycompper type h99cwtr-compper as checkbox default space
modif id pe1.
selection-screen comment 60(20) text-s23 for field ycompper
modif id pe1.
selection-screen end of line.

selection-screen begin of line.
parameters: noc type h99cwtr-nooc default 'X' radiobutton group oc
        modif id pe2.
selection-screen comment 4(31) text-s12 for field noc modif id pe2.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-192 for field abkr_cal modif id pe1.
selection-screen position pos_low.
parameters: abkr_cal like t569v-abkrs modif id pe1.
selection-screen comment 57(40) text-195 for field abkr_ref
    visible length 1 modif id pa1. "ACC
selection-screen position pos_high.
parameters: abkr_ref like t569v-abkrs modif id pe1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-197 for field pcurperc
modif id pe1.
selection-screen position pos_low.
parameters: pcurperc type timra9 as listbox visible length 23
default space modif id pe1.
selection-screen comment 57(40) text-198 for field pcurperr "ACC
    visible length 1 modif id pa1. "ACC
selection-screen position pos_high.
parameters: pcurperr type timra9 as listbox visible length 23
default space modif id pe1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-200 for field abrp_cal "ACC
modif id pa1.                                              "ACC
selection-screen position pos_low.
parameters:
  abrp_cal like t549q-pabrp modif id pe1, "calculation period
  abrj_cal like t549q-pabrj modif id pe1.
selection-screen comment 57(40) text-201 for field abrp_ref
    visible length 1 modif id pa1. "ACC
selection-screen position pos_high.
parameters:
  abrp_ref like t549q-pabrp modif id pe1, "reference period
  abrj_ref like t549q-pabrj modif id pe1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 4(28) text-202 for field begcalsh "ACC
modif id pa4.                                              "ACC
selection-screen position pos_low.
parameters: begcalsh like t549q-begda modif id pe4.
selection-screen comment (1) text-199 for field endcalsh
modif id pe4.
parameters: endcalsh like t549q-endda modif id pe4.
selection-screen comment 57(40) text-203 for field begrefsh
    visible length 1 modif id pa5. "ACC
selection-screen position pos_high.
parameters: begrefsh like t549q-begda modif id pe5.
selection-screen comment (1) text-199 for field endrefsh
modif id pe5.
parameters: endrefsh like t549q-endda modif id pe5.
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
parameters: bondt type pc261-bondt no-display, "Note 2394856parameters for process model
            payid type pc261-payid no-display,
            payty type pc261-payty no-display.
selection-screen comment 57(40) text-s31 modif id pa2
    visible length 1 for field pyty_ref. "ACC
selection-screen position pos_high.
parameters: pyty_ref like pc261-payty modif id pe3 value check,
            pyid_ref like pc261-payid modif id pe2,
            bond_ref like pc261-bondt modif id pe2.
selection-screen end of line.
*begin of subframe for the comparison period
selection-screen begin of block comp_periods with frame title text-s09.

select-options: s_absnum for absnum modif id pe1,
                s_absamo for absamo modif id pe1,
                s_relnum for relnum modif id pe1,
                s_relamo for relamo modif id pe1.

selection-screen end of block comp_periods .
*end of subframe for the comparison period
*>>> Start of TPY Enhancements
*selection-screen pushbutton /1(24) label01 user-command perordat .   "MOD001--
selection-screen pushbutton /1(24) label01 user-command perordat modif id tpy.  "MOD001++
*<<< End of TPY Enhancements
selection-screen end of block calc_periods .
*end of frame for the selection period
*frame: in-period or for-period criterion
selection-screen begin of block time with frame title text-s01.
selection-screen begin of line.
parameters: in_view like h99cwtr-inperview radiobutton group view.
*selection-screen comment 4(40) text-s14.                       "N762901
selection-screen comment 4(40) text-s14 for field in_view.  "N762901
selection-screen end of line.
selection-screen begin of line.
parameters: for_view like h99cwtr-forperview radiobutton group view.
*selection-screen comment 4(40) text-s15.                       "N762901
selection-screen comment 4(40) text-s15 for field for_view. "N762901
selection-screen end of line.
selection-screen end of block time.
*frame: other selections (wage type, sort crit, archived results)
selection-screen begin of block field with frame title text-s02.
select-options: s_lgart for t512w-lgart.
parameters: arc_read like h99cwtr-arch_read default space.
parameters: nullrecs like h99cwtr-shownullrecords default space.
parameters: p_fchdcl type h99cwtr-fetchdeclusteredrecords default space modif id dcl."Improvement idea 457
parameters: shwsplit type showsplits default space modif id spl."Improvement idea 499
*>>> Start of TPY Enhancement
parameters: p_tst_py type hrdct_is_tpy default space.    "MOD001++
*<<< End of TPY Enhancements
selection-screen pushbutton /1(24) label02 user-command fieldselect.
*>>> Start of TPY Enhancement
*parameters: p_h_strg(256) type c no-display default '05/08/30/43'.   "MOD001--
parameters: p_h_strg(256) type c no-display default '01/30/43'.       "MOD001++
*<<< End of TPY Enhancement
selection-screen end of block field.
*frame: output on ALV list, MS Excel, ALV grid
selection-screen begin of block out with frame title text-s03.

selection-screen begin of line.
parameters: lv like h99cwtr-alvlist radiobutton group out.
selection-screen comment 4(20) text-s16 for field lv.
selection-screen comment 32(25) text-s25 for field p_lvtemp.
selection-screen position pos_high.
parameters: p_lvtemp like disvariant-variant.
selection-screen end of line.

selection-screen begin of line.
parameters: grid like h99cwtr-gridlist radiobutton group out.
selection-screen comment 4(20) text-s17 for field grid.
selection-screen comment 32(25) text-s25 for field p_grtemp.
selection-screen position pos_high.
parameters: p_grtemp like disvariant-variant.
selection-screen end of line.

selection-screen begin of line.
parameters: excel like h99cwtr-xxllist radiobutton group out.
selection-screen comment 4(20) text-s18 for field excel.
selection-screen comment 32(25) text-s26 for field p_extemp.
selection-screen position pos_high.
parameters: p_extemp like h99cwtr-xxlpcfile visible length 40.
selection-screen end of line.

selection-screen end of block out.

*types for passing select options to forms
types tt_s_pyty like table of s_pyty_c.

*accessibility mode flag
data gx_acc_mode_on type boolean.
