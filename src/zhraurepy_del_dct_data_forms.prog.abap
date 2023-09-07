*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_DEL_DCT_DATA_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       init select screen value
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
form initialization .

  gv_count = 0.
  gv_pernr_count = 0.
  if p_date-low is initial.
    p_date-low = '19000101'.
  endif.
  if p_date-high is initial.
    p_date-high = '99991231'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GET_HRDCT_TABLES
*&---------------------------------------------------------------------*
*       for deleting the declustered data, program has to check with all tables
*----------------------------------------------------------------------*
*  -->  p_pclx  p_relid       PCLX   RELID
*  <--  p_t_hrdct_d_tables    list of all declustered tables
*----------------------------------------------------------------------*
form get_hrdct_tables using p_t_hrdct_d_tables like gt_hrdct_d_tables.

  data: lt_hrdct_d_tables    type sorted table of hrdct_d_tables with non-unique key pcltabname relid,
        ls_hrdct_d_tables    type                 hrdct_d_tables,
        lt_hrdct_d_tables_cu type sorted table of hrdct_d_tables with non-unique key pcltabname relid, " SUPPORT RELID CU
        lv_d_tablename       type dbobj_name,
        lr_message_handler   type ref to cl_hrpay00_message_handler,
        l_dummy              type string.

  clear p_t_hrdct_d_tables.
  clear lt_hrdct_d_tables.
  try.

      select single tabname from hrdct_d_tables into lv_d_tablename      " GET THE VERSC TABLE NAME
        where internal_tabname = 'VERSC'
          and pcltabname = p_pclx
          and relid = p_relid.

      "no candidate table for VERSC, declustering is not supported.
      if sy-subrc <> 0.
        if sy-batch = abap_true.
          message s002(hrdct_msg) into l_dummy with p_relid p_pclx.
        else.
          message e002(hrdct_msg) into l_dummy with p_relid p_pclx.
          lr_message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
          lr_message_handler->add_message( ).
          reject.
        endif.
      endif.

      call method cl_hrdct_d_tables=>read_by_relid
        exporting
          iv_pcltabname     = p_pclx
          iv_relid          = p_relid
        importing
          et_hrdct_d_tables = lt_hrdct_d_tables[].

      " CHANGE FOR SUPPORT RELID CU
      call method cl_hrdct_d_tables=>read_by_relid
        exporting
          iv_pcltabname     = p_pclx
          iv_relid          = 'CU'
        importing
          et_hrdct_d_tables = lt_hrdct_d_tables_cu.
      if p_tpy = abap_true and p_pdt = abap_false.
        delete lt_hrdct_d_tables_cu where internal_tabname = 'EVAL_PERIOD'.
      endif.
      if p_tpy = abap_false and p_pdt = abap_true.
        delete lt_hrdct_d_tables_cu where internal_tabname = 'T_EVAL_PERIOD'.
      endif.
      insert lines of lt_hrdct_d_tables_cu into table lt_hrdct_d_tables.
      if p_tpy = abap_false and p_pdt = abap_false.
        clear lt_hrdct_d_tables.
      endif.

      p_t_hrdct_d_tables[] = lt_hrdct_d_tables[].
*>>> Start of WOW Specific Enhancements
      loop at lt_hrdct_d_tables into ls_hrdct_d_tables.
        gs_tablename = ls_hrdct_d_tables-tabname.
        insert gs_tablename into table gt_tablename.
      endloop.

      select single dbcon from t77dct_option into gv_dbcon
       where pcltabname = p_pclx and
             relid      = p_relid.

      check cl_hrdct_check=>check_target_table(
      it_tablename = gt_tablename iv_dbconn = gv_dbcon
      iv_relid = p_relid  ) = abap_true.

      pn-begps = pn-begda.
      pn-endps = pn-endda.
*<<< End of WOW Specific Enhancements
    catch cx_root.
  endtry.

endform.
*&---------------------------------------------------------------------*
*&      Form  GET_DEL_PCLKEY_TAB
*&---------------------------------------------------------------------*
*       get the list of PCLKEYS
*----------------------------------------------------------------------*
*  -->  p_pclx p_relid gv_col_date p_tpy
*  <--  p_t_pclkeys  gt_tpy_rgdir  gt_rgdir
*----------------------------------------------------------------------*
form get_del_pclkey_tab using p_t_pclkeys like gt_pclkeys.

  data:
    ls_hrdct_pclkey type hrdct_s_pclkey,
    ls_tpy_rgdir    like hrdct_tpy_rgdir,
    ls_rgdir        like hrpy_rgdir,
    lv_pclkey       type string.

  if p_tpy = abap_true.

    select * from hrdct_tpy_rgdir into ls_tpy_rgdir "#EC CI_DYN_WHERE. "GET PCLKEY FROM RGDIR  "#EC CI_NOFIRST.
       where dct_pernr = pernr-pernr and dct_is_tpy = 'X'. "#EC CI_DYN_WHERE. "#EC CI_NOFIRST.
      concatenate ls_tpy_rgdir-dct_pernr ls_tpy_rgdir-dct_seqnr into lv_pclkey.
      ls_hrdct_pclkey-sgart = p_pclx(1) && p_pclx+3(1).
      ls_hrdct_pclkey-relid = p_relid.
      ls_hrdct_pclkey-srtfd = lv_pclkey.
      append ls_hrdct_pclkey to p_t_pclkeys.
      append ls_tpy_rgdir to gt_tpy_rgdir.
    endselect.

  endif.

  if p_pdt = abap_true. .

    select * from hrpy_rgdir into ls_rgdir "#EC CI_DYN_WHERE. "GET PCLKEY FROM RGDIR "#EC CI_NOFIRST.
       where pernr = pernr-pernr and ipend in p_date."-low AND p_date-high.    "#EC CI_NOFIRST.                           "#EC CI_DYN_WHERE.
      concatenate ls_rgdir-pernr ls_rgdir-seqnr into lv_pclkey.
      ls_hrdct_pclkey-sgart = p_pclx(1) && p_pclx+3(1).
      ls_hrdct_pclkey-relid = p_relid.
      ls_hrdct_pclkey-srtfd = lv_pclkey.
      append ls_hrdct_pclkey to p_t_pclkeys.
      append ls_rgdir to gt_rgdir.
    endselect.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GET_PERNR_SO
*&---------------------------------------------------------------------*
*      put the rejected pernrs into perner_so which can be used in sql where clause
*----------------------------------------------------------------------*
*      -->PERNR-PERNR
*      <--P_PERNR_SO
*----------------------------------------------------------------------*
form get_pernr_so  using  p_pernr_so type  ty_t_sel_options.

  data ls_so type ty_s_sel_option.

  ls_so-sign   = 'I'.
  ls_so-option = 'EQ'.
  ls_so-low    = pernr-pernr.
  append ls_so to p_pernr_so.

endform.
*&---------------------------------------------------------------------*
*&      Form  DEL_DATA
*&---------------------------------------------------------------------*
*      call declustermanager to do 'delete' operation
*----------------------------------------------------------------------*
*  -->  gt_pclkeys  gt_hrdct_d_tables
*----------------------------------------------------------------------*
form del_data .
*>>> Start of WOW Specific Enhancements
*    TRY.
*
*
*    go_declustermanager = cl_hrdct_manager=>get_instance( ).
*
*    CALL METHOD go_declustermanager->delete_data
*      EXPORTING
*        it_clusterkeys    = gt_pclkeys    "gt_pclx
*        iv_switch_check   = abap_false
*        it_hrdct_d_tables = gt_hrdct_d_tables[].
*    IF p_sim = abap_false.
*        CALL METHOD go_declustermanager->flush_all.
*        IF p_tpy = abap_true.
*          READ TABLE gt_hrdct_d_tables TRANSPORTING NO FIELDS WITH KEY internal_tabname = 'T_EVAL_PERIOD'.
*          IF sy-subrc = 0.
*            DELETE FROM p2rx_tpy_eval_p WHERE dct_pernr IN pernr_so.
*          ENDIF.
*        ENDIF.
*    ENDIF.
*  CATCH cx_root.
*
*  ENDTRY.

* Delete Employees Text data from the Tables
  try.
      if p_sim = abap_false and not pernr_so is initial.
        loop at gt_tablename into gs_tablename.
          " delete base on generic key and db connection
          if gv_dbcon is initial.
            delete from (gs_tablename) where dct_pernr in pernr_so
                                          and dct_seqnr between '99001' and '99999'
            %_hints adabas 'KEYACCESS'.
          else.
            delete from (gs_tablename) connection (gv_dbcon)
                  where dct_pernr in pernr_so
                    and dct_seqnr between '99001' and '99999'
            %_hints adabas 'KEYACCESS'.
          endif.
        endloop.
      endif.
    catch cx_root.
  endtry.
*<<< End of WOW Specific Enhancements
endform.
*&---------------------------------------------------------------------*
*&      Form  DEL_TPY_RGDIR
*&---------------------------------------------------------------------*
*       delete entries in hrdct_tpy_rgdir
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
form del_tpy_rgdir .

  if p_tpy = abap_true and p_sim = abap_false.
    if not pernr_so is initial.
      delete from hrdct_tpy_rgdir                      "#EC CI_NOFIRST.
           where dct_pernr in pernr_so.                "#EC CI_NOFIRST.
    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  LOG_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form log_display .

  data:
    lr_message_handler type ref to cl_hrpay00_message_handler,
    lt_message_table   type hrpay00t_message_table,
    l_display_profile  type hrpad_pal_disp_prof,
    l_root_node        type         hrpad_pal_node_key value 'ROOT',
    l_employee_node    type         hrpad_pal_node_key,
    l_table_detail     type hrpad_pal_node_key,
    lt_fcat_pernr      type         slis_t_fieldcat_alv,
    lt_fcat_tpy_pernr  type         slis_t_fieldcat_alv,
    lt_fcat_table      type         slis_t_fieldcat_alv,
    ls_fcat_pernr      type    slis_fieldcat_alv,
    ls_layout          type slis_layout_alv,
    ls_hrdct_d_tables  type hrdct_d_tables,
    ls_tabname         type tabname,
    lv_node_txt        type string,
    lv_msg_txt         type symsgv,
    l_dummy            type string.                         "#EC NEEDED

  lr_message_handler =
           cl_hrpay00_message_handler=>get_message_handler( ).

  " General Messages
  lr_message_handler->add_messages( ).

  lv_msg_txt = 'Number of Employee Processed'(006).
  " Statistics Node
  call method lr_message_handler->add_statistics                     " add statistics for pernr processed
    exporting
      i_count = gv_pernr_count
      i_msg   = lv_msg_txt
      i_msgty = 'S'.


  " Detail Log: Payroll Result Processed
  if p_log = abap_true.
    " Transparent Table Updated Node
    clear lt_fcat_table[].
    call method lr_message_handler->create_fcat                     " generate declustered tables log fcat
      exporting
        i_structure_name = 'HRDCT_D_TABLES'
      importing
        et_fcat          = lt_fcat_table[].

*    IF p_tpy = abap_true.
*      ls_hrdct_d_tables-tabname = 'HRDCT_TPY_RGDIR'.
*      APPEND ls_hrdct_d_tables TO gt_hrdct_d_tables.
*    ENDIF.

    lr_message_handler->add_table( i_parent_node_key = l_root_node              " add declustered tables node
                               it_append_table   = gt_hrdct_d_tables
                               it_fcat           = lt_fcat_table
                               i_node_txt        = 'Transparent Table Selected'(005) ).

    " if detail log
    if p_pdt = abap_true.
      call method lr_message_handler->create_fcat                   " generate rgdir log fcat
        exporting
          i_structure_name = 'HRPY_RGDIR'
        importing
          et_fcat          = lt_fcat_pernr[].

      lr_message_handler->add_table( i_parent_node_key = l_root_node  " add detail entries log table node
                               it_append_table   = gt_rgdir
                               it_fcat           = lt_fcat_pernr
                               i_node_txt        = 'Records of Changed Rgdir '(007)  " 2013/07/10 CHANGED FROM 'Detailed Log'
                               is_layout         = ls_layout
                              ).

    endif.
    if p_tpy = abap_true.
      call method lr_message_handler->create_fcat                   " generate rgdir log fcat
        exporting
          i_structure_name = 'HRDCT_TPY_RGDIR'
        importing
          et_fcat          = lt_fcat_tpy_pernr[].

      lr_message_handler->add_table( i_parent_node_key = l_root_node  " add detail entries log table node
                               it_append_table   = gt_tpy_rgdir
                           it_fcat           = lt_fcat_tpy_pernr  " changed on HRIK142656 2016/3/7
                               i_node_txt        = 'Records of Changed Test Payroll Rgdir '(008)  " 2013/07/10 CHANGED FROM 'Detailed Log'
                               is_layout         = ls_layout
                              ).

    endif.

  endif.

  clear l_display_profile.

  l_display_profile-title = sy-title.
  l_display_profile-tree_size = 6.
  l_display_profile-tree_ontop = 'X'.

  call method lr_message_handler->display_pal
    exporting
      is_display_profile = l_display_profile.

endform.

form date_option_check.


  loop at screen.
    if screen-name ='P_DATE-LOW' or screen-name ='P_DATE-HIGH'.
      if p_pdt = abap_true.
        screen-input = 1.
      else.
        screen-input = 0.
      endif.
    endif.

    if screen-name ='P_TPY' or screen-name ='P_PDT'.
      screen-input = 0.
    endif.

    modify screen.
  endloop.


endform.
*&---------------------------------------------------------------------*
*& Form del_pcl2_xt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form del_pcl2_xt .
*>>> Start of WOW Specific Enhancements
*  IF p_tpy = abap_true AND p_sim = abap_false.
*     DATA(lt_srtfd_so) = pernr_so.
*     LOOP AT lt_srtfd_so ASSIGNING FIELD-SYMBOL(<fs>).
*       <fs>-sign = 'I'.
*       <fs>-option = 'CP'.
*       <fs>-low = '++' && <fs>-low && '*'.
*     ENDLOOP.
*     DELETE FROM pcl2
*      WHERE relid EQ cl_hrdct_tpy_manager=>gc_tpy_relid
*        AND srtfd IN lt_srtfd_so.
*   ENDIF.

  data lt_srtfd_so_new type ty_t_sel_options.

  if p_tpy = abap_true and p_sim = abap_false.
    loop at gt_pclkeys into gs_pclkeys.
      append initial line to lt_srtfd_so_new assigning field-symbol(<fs_new>).
      <fs_new>-sign = 'I'.
      <fs_new>-option = 'EQ'.
      <fs_new>-low = gs_pclkeys-relid && gs_pclkeys-srtfd.
    endloop.
    if not lt_srtfd_so_new is initial.
      delete from pcl2
       where relid eq cl_hrdct_tpy_manager=>gc_tpy_relid
         and srtfd in lt_srtfd_so_new.
    endif.
  endif.
*<<< End of WOW Specific Enhancements
endform.
