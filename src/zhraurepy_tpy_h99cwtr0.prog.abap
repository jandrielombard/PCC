* FELL6BK051310 14.11.2003 performance improvement with trimmings as
*                              side effect
* FELL6BK050334 30.10.2003 new comboboxes: current period / other period
* 46C FELL9CK137604 23.06.2003 new version with period comparison,
*    archived data, programme log, various bugfixes.
* 46C FELL9CK132959 28.04.2003 improved compatibility w process manager
* 4.0C new development C.Frey: wage type reporter (successor of
*    wage type statement/distribution RPCLGA00/09 RPCLGV00/09
report zhraurepy_tpy_h99cwtr0 no standard page heading.
*-----------------------------------------------------------------------*
* Program Name  : ZHRAUREPY_TPY_H99CWTR0
* Title         : Wage type Reporter to Report Test Payroll Results
* Create Date   : 08.10.2021
* Release       : ECC 6.0
* Author        : 1130848
*-----------------------------------------------------------------------*
* Description   : Wage type Reporter to Report Test Payroll Results, when
*                 standard payroll results are not available period
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Mod | Date      | User ID  |Description                    |Change Req *
*-----------------------------------------------------------------------*
*001 |08-10-2021 | 1130848  |PTX-3763 Initial creation      |CFAK900928 *
*-----------------------------------------------------------------------*
include zusehraurepy_tpy_wtr_scr_def.   "MOD001++
*include h99cwtr0_scr_def.           "MOD001--

include h99cwtr0_data_def.
*>>> Start of TPY Enhancements  "MOD001++
data: c_text_tpy_results type p0002-perid value 'Test Payroll Results'.

data: gt_tpy_rgdir type table of hrdct_tpy_rgdir,
      gs_tpy_rgdir type hrdct_tpy_rgdir.
data: gt_tpy_p2rx_rt    type sorted table of p2rx_rt with non-unique key dct_seqnr,
      gt_tpy_p2rx_wpbp  type sorted table of p2rx_wpbp with non-unique key dct_seqnr,
      gt_tpy_p2rx_versc type sorted table of p2rx_versc with non-unique key dct_seqnr.
*<<< End of TPY Enhancements     "MOD001++

initialization.
  rp-set-name-format.
  perform initialise changing field_directory.

at selection-screen on value-request for p_extemp.
  perform get_xlt_file changing p_extemp excel_template.

at selection-screen on value-request for p_lvtemp.
  perform get_alv_variant changing alv_template p_lvtemp.

at selection-screen on value-request for p_grtemp.
  perform get_alv_variant changing grid_template p_grtemp.

at selection-screen.
*983979 - UNNI To change the object selection string of old variants
  perform replace_objselstrg changing p_h_strg.
  perform selection_screen_input
    using field_directory pyty_cal pyid_cal pyid_ref
          yoc abkr_cal pcurperc pcurperr
    changing ypernodt ycompper p_h_strg abkr_ref
        begd_cal endd_cal abrp_cal abrj_cal bond_cal
        begd_ref endd_ref abrp_ref abrj_ref bond_ref pyty_ref
        begcalsh endcalsh begrefsh endrefsh
        alv_template p_lvtemp
        grid_template p_grtemp
        excel_template p_extemp.

*perform changefielddirectory using shwsplit changing field_directory. "improvement request 499

at selection-screen output.
  perform convert_old_variants changing ypernodt begd_cal endd_cal
      abkr_cal abrp_cal abrj_cal.
  perform selection_screen_output using ypernodt hide_oc yoc
                    begcalsh begrefsh.
  perform set_selection_screen_param changing p_fchdcl shwsplit. "improvement request 499,457

start-of-selection.
  if ycompper eq true.
    if begd_ref lt pn-begda.
      rp-set-data-interval 'ALL' begd_ref pn-endda.
    else.
      rp-set-data-interval 'ALL' pn-begda endd_ref.
    endif.
  else.
    rp-set-data-interval 'ALL' pn-begda pn-endda.
  endif.
  perform changefielddirectory using shwsplit changing field_directory. "improvement request 499
  perform start_of_selection using ypernodt p_h_strg
      changing field_directory ycompper driver
               ip_beg ip_end fp_beg fp_end
               ip_beg_ref ip_end_ref fp_beg_ref fp_end_ref.
  perform set_declustered_flag using p_fchdcl. "improvement request 457

get pernr.
  call function 'HR_PCLX_INIT_BUFFER'.
  perform core_proc
    using ip_beg ip_end fp_beg fp_end
          pyty_cal pyid_cal bond_cal s_pyty_c[]
          ycompper arc_read false yoc ypernodt for_view.
  if ycompper eq true.
    perform core_proc
      using ip_beg_ref ip_end_ref fp_beg_ref fp_end_ref
            pyty_ref pyid_ref bond_ref s_pyty_c[]
            ycompper arc_read true yoc ypernodt for_view.
    perform filter_diffs in program (driver) using
        s_absnum[] s_absamo[] s_relnum[] s_relamo[].
  endif.
*  if nullrecs eq false.
*    perform delete_null_fields in program (driver).
*  endif.

end-of-selection.

  if nullrecs eq false.
    perform delete_null_fields in program (driver).
  endif.

  perform end_of_selection using driver excel_template alv_template
                                 grid_template excel lv grid.

  include zusehraurepy_tpy_wtr_forms.     "MOD001++
*include h99cwtr0_forms.               "MOD001--
  include h99cwtr0_screen_2000.
