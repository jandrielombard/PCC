*&---------------------------------------------------------------------*
*&  Include          ZHRAUREPY_DEL_DCT_DATA_DEF
*&---------------------------------------------------------------------*

tables: pernr.

types:
  begin of ty_s_sel_option,
    sign   type ddsign,
    option type ddoption,
    low    type string,
    high   type string,
  end of ty_s_sel_option,
  ty_t_sel_options type table of ty_s_sel_option.

data: gv_col_date         type string,
      gv_count            type i,
      gv_pernr_count      type i,
      gv_dbcon            type dbcon_name,
      pernr_so            type ty_t_sel_options,
      gt_tpy_rgdir        like table of hrdct_tpy_rgdir,
      gt_rgdir            like table of hrpy_rgdir,
      gv_date             type datum,
      go_declustermanager type ref to cl_hrdct_manager.

data : begin of ls_pclkey.
data: pernr like hrpy_rgdir-pernr,
      seqnr like hrpy_rgdir-seqnr.
data:  end of ls_pclkey.

data: gt_pclkeys type hrdct_t_pclkey,
      gs_pclkeys type hrdct_s_pclkey.
data  gt_hrdct_d_tables     type table of hrdct_d_tables.
data  gs_hrdct_d_tables     type  hrdct_d_tables.
*>>> Start of WOW Specific Enhancements
data: gt_tablename type table of        dbobj_name,
      gs_tablename type                 dbobj_name.
constants: begin of gc_tvarvc,
             dct_del type rvari_vnam value 'ZHRAUREPY_DEL_DCT_DATA',
             type_p  type rsscr_kind value 'P',
           end of gc_tvarvc.
data: gv_dct_var type tvarvc-low.
data: gv_emp_num type i.
*<<< End of WOW Specific Enhancements
