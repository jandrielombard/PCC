*&---------------------------------------------------------------------*
*& Report ZHRPY_KRN_DELTA_STATS1
*&---------------------------------------------------------------------*
*& This program creates a checksum for all PAxxxx data of an employee
*& and persists the checksum for later comparison by ZHRXX_EPIUSE_UTIL2
*&---------------------------------------------------------------------*
*& Copyright  2002-2023 EPI-USE Systems Limited, All Rights Reserved.
*&---------------------------------------------------------------------*
*& Author : Jan deMooij - Feb 2023 - EPI-USE Global Services
*&---------------------------------------------------------------------*
*&
*&
*&
*&
*&---------------------------------------------------------------------*
report zhrpy_krn_delta_stats1.
tables pa0003.
tables t582a.
tables pskey.

types begin of type_key.
types pernr type  pernr_d. "8
types infty type  infty.   "4
types begda type  begda.   "8 (tot max 20)
types end of type_key.

types begin of type_stat.
types infty type  infty.
types dat1 type  begda.
types diff type  i.
types nodiff type  i.
types total  type  i.
types end of type_stat.

select-options s_pernr for pa0003-pernr.
select-options s_infty for t582a-infty default '2000' to '2999'.
selection-screen skip 1.
parameters p_gener radiobutton group 001 default 'X'.
parameters p_reprt radiobutton group 001.
"select-options s_repda for pskey-begda default sy-datum to sy-datum.
selection-screen skip 1.
parameters p_delet radiobutton group 001.
select-options s_begda for pskey-begda default sy-datum to sy-datum.

*--------------------------------------------------------------------*
load-of-program.
*--------------------------------------------------------------------*
  perform prep_selscreen.

*--------------------------------------------------------------------*
start-of-selection.
*--------------------------------------------------------------------*
  case p_gener.
    when 'X'.
      perform calc_checksums.
    when others.
      if p_delet = space.
        perform checksum_stats.
      else.
        perform checksum_delet.
      endif.
  endcase.

*--------------------------------------------------------------------*
form calc_checksums.

  data lt_t777d type table of t777d.
  data ls_t777d type t777d.
  select * from t777d into table lt_t777d where infty in s_infty.

  data lt_pernr type table of pernr_d.
  data lv_pernr type pernr_d.
  select pernr from pa0003 into table lt_pernr where pernr in s_pernr.

  data: l_prog type p decimals 2.
  data: l_mod  type p decimals 4.
  data: l_idx  type i.
*--------------------------------------------------------------------*
  loop at lt_pernr into lv_pernr.
*--------------------------------------------------------------------*
    add 1 to l_idx.
    l_prog = ( l_idx / lines( lt_pernr ) ) * 100. "percentage processed
    l_mod = l_prog mod 5. "every 5 percent
    if  l_mod  = 0 and l_prog > 0.
      commit work.
    endif.
    l_mod = l_prog mod 2. "every 2 percent
    if  l_mod  = 0.
      cl_progress_indicator=>progress_indicate( i_text = |Processing employee { l_idx }/{ lines( lt_pernr ) }| i_output_immediately = abap_true i_processed = l_idx i_total = lines( lt_pernr ) ).
    endif.

*--------------------------------------------------------------------*

    loop at lt_t777d into ls_t777d where dbtab is not initial.

      data ls_pskey type type_key.
      data ls_pclhr type pclhr.
      data lv_checksum type c length 40.
      data lv_xmlstring type string.
      ls_pskey-pernr = lv_pernr.
      ls_pskey-infty = ls_t777d-infty.
      ls_pskey-begda = sy-datum. "date of the snapshot/checksum
      ls_pclhr-aedtm = ls_pskey-begda. "date of the snapshot/checksum
      ls_pclhr-uname = sy-uname.

      data lr_data_tab  type ref to data.
      data lr_data_line type ref to data.
      field-symbols <data_tab> type standard table.
      field-symbols <data_line> type any.

      create data lr_data_tab type standard table of (ls_t777d-dbtab).
      assign lr_data_tab->* to <data_tab>.

      select * from (ls_t777d-dbtab) into table <data_tab> where pernr = lv_pernr.
      if lines( <data_tab> ) = 0.
        delete from database pclhr(ze) id ls_pskey.
      else.

        sort <data_tab>.
        call transformation id source data_tab = <data_tab> result xml lv_xmlstring.

        call function 'CALCULATE_HASH_FOR_CHAR'
          exporting
            alg  = 'MD5'
            data = lv_xmlstring
          importing
            hash = lv_checksum.

        export lv_checksum to database pclhr(ze) id ls_pskey from ls_pclhr.
      endif.

    endloop.

*--------------------------------------------------------------------*
  endloop.

  commit work.
  message |Checksum generation ended - { lines( lt_pernr ) } employees evaluated| type 'S'.

endform.

*--------------------------------------------------------------------*
form checksum_stats.

  data lt_t777d type table of t777d.
  data ls_t777d type t777d.
  select * from t777d into table lt_t777d where infty in s_infty.

  data lt_pernr type table of pernr_d.
  data lv_pernr type pernr_d.
  select pernr from pa0003 into table lt_pernr where pernr in s_pernr.

  data: l_prog type p decimals 2.
  data: l_mod  type p decimals 4.
  data: l_idx  type i.

  data ls_pskey type type_key.
  data ls_pclhr type pclhr.
  data lv_checksum type c length 40.
  data lv_checksum_prev type c length 40.

  data lt_dates type table of aedtm.
  data lv_aedtm type aedtm.
  data lv_aedtm_earliest type aedtm.

*--------------------------------------------------------------------*
  loop at lt_pernr into lv_pernr.
*--------------------------------------------------------------------*
    add 1 to l_idx.
    l_prog = ( l_idx / lines( lt_pernr ) ) * 100. "percentage processed
    l_mod = l_prog mod 2. "every 2 percent
    if  l_mod  = 0.
      cl_progress_indicator=>progress_indicate( i_text = |Processing employee { l_idx }/{ lines( lt_pernr ) }| i_output_immediately = abap_true i_processed = l_idx i_total = lines( lt_pernr ) ).
    endif.

*--------------------------------------------------------------------*
    data lv_new type abap_bool.

    sort lt_t777d by infty.
    loop at lt_t777d into ls_t777d where dbtab is not initial.

      lv_new = abap_true.

      data lv_search type indx_srtfd.
      lv_search = |{ lv_pernr }{ ls_t777d-infty }%|.

      free lt_dates.
      select distinct aedtm from pclhr into table lt_dates where relid = 'ZE' and srtfd like lv_search.
      sort lt_dates.
      read table lt_dates into lv_aedtm index 1.
      check sy-subrc = 0.
      if lv_aedtm < lv_aedtm_earliest or lv_aedtm_earliest is initial.
        lv_aedtm_earliest = lv_aedtm.
      endif.

      clear lv_checksum_prev.
      clear lv_checksum.

      data lt_stats type table of type_stat.
      data ls_stats type type_stat.

      check lines( lt_dates ) > 0.
      loop at lt_dates into lv_aedtm.
        lv_checksum_prev = lv_checksum.
        ls_pskey-pernr = lv_pernr.
        ls_pskey-infty = ls_t777d-infty.
        ls_pskey-begda = lv_aedtm.
        import lv_checksum from database pclhr(ze) id ls_pskey to ls_pclhr.
        if sy-subrc = 0.
          if lv_new = abap_false or lv_aedtm = lv_aedtm_earliest.
            if lv_checksum_prev <> lv_checksum and lv_checksum_prev is not initial.
              "difference found
              ls_stats-infty = ls_t777d-infty.
              ls_stats-dat1  = lv_aedtm.
              ls_stats-diff = 1.
              ls_stats-nodiff = 0.
              ls_stats-total = 1.
              collect ls_stats into lt_stats.
*              write : / '1', ls_pskey, lv_aedtm, lv_aedtm_earliest.
            else.
              ls_stats-infty = ls_t777d-infty.
              ls_stats-dat1  = lv_aedtm.
              ls_stats-diff = 0.
              ls_stats-nodiff = 1.
              ls_stats-total  = 1.
              collect ls_stats into lt_stats.
            endif.
            ls_stats-infty = 'TOT'.
            ls_stats-dat1  = '99991231'.
            collect ls_stats into lt_stats.

          else. "not found previously, report as delta
            "difference found
            ls_stats-infty = ls_t777d-infty.
            ls_stats-dat1  = lv_aedtm.
            ls_stats-diff = 1.
            ls_stats-nodiff = 0.
            ls_stats-total = 1.
            collect ls_stats into lt_stats.
            ls_stats-infty = 'TOT'.
            ls_stats-dat1  = '99991231'.
            collect ls_stats into lt_stats.
*            write : / '2', ls_pskey, lv_aedtm, lv_aedtm_earliest.
          endif.
          lv_new = abap_false.
        else.
          continue.
        endif.

      endloop.

    endloop.

*--------------------------------------------------------------------*
  endloop.

  uline.
  data lv_perc1 type p length 5 decimals 2.
  loop at lt_stats into ls_stats where infty <> 'TOT'.

    lv_perc1 = ls_stats-diff / ls_stats-total * 100.
    write : / '|', 'Infotype:', ls_stats-infty,
              20 '|', 'Date:', ls_stats-dat1,
              40 '|', 'EEs with delta:', ls_stats-diff,
              70 '|', 'EEs without delta:', ls_stats-nodiff,
              110 '|', 'Total EEs:', ls_stats-total,
              '| Delta% ', lv_perc1, '%', '|'.
  endloop.


  uline.
  loop at lt_stats into ls_stats where infty = 'TOT'.

    lv_perc1 = ls_stats-diff / ls_stats-total * 100.
    write : / '|', 'TOTAL:',
              40 '|', 'EEs with delta:', ls_stats-diff,
              70 '|', 'EEs without delta:', ls_stats-nodiff,
              110 '|', 'Total EEs:', ls_stats-total,
              '| Delta% ', lv_perc1, '%', '|'.
  endloop.
  uline.

  message |Checksum report ended - { lines( lt_pernr ) } employees evaluated| type 'S'.

endform.

*--------------------------------------------------------------------*
form checksum_delet.

  data lv_diff type i.
  data lv_nodiff type i.

  data lt_t777d type table of t777d.
  data ls_t777d type t777d.
  select * from t777d into table lt_t777d where infty in s_infty.

  data lt_pernr type table of pernr_d.
  data lv_pernr type pernr_d.
  select pernr from pa0003 into table lt_pernr where pernr in s_pernr.

  data: l_prog type p decimals 2.
  data: l_mod  type p decimals 4.
  data: l_idx  type i.
*--------------------------------------------------------------------*
  loop at lt_pernr into lv_pernr.
*--------------------------------------------------------------------*
    add 1 to l_idx.
    l_prog = ( l_idx / lines( lt_pernr ) ) * 100. "percentage processed
    l_mod = l_prog mod 2. "every 2 percent
    if  l_mod  = 0.
      cl_progress_indicator=>progress_indicate( i_text = |Processing employee { l_idx }/{ lines( lt_pernr ) }| i_output_immediately = abap_true i_processed = l_idx i_total = lines( lt_pernr ) ).
    endif.

*--------------------------------------------------------------------*

    loop at lt_t777d into ls_t777d where dbtab is not initial.

      data ls_pskey type type_key.
      data ls_pclhr type pclhr.
      data lv_checksum type c length 40.
      data lv_checksum_prev type c length 40.

      data lt_dates type table of aedtm.
      data lv_aedtm type aedtm.
      free lt_dates.

      data lv_search type indx_srtfd.
      lv_search = |{ lv_pernr }{ ls_t777d-infty }%|.

      select distinct aedtm from pclhr into table lt_dates where relid = 'ZE' and srtfd like lv_search.
      clear lv_checksum_prev.
      clear lv_checksum.

      check lines( lt_dates ) > 0.
      loop at lt_dates into lv_aedtm.
        check lv_aedtm in s_begda.
        ls_pskey-pernr = lv_pernr.
        ls_pskey-infty = ls_t777d-infty.
        ls_pskey-begda = lv_aedtm.
        delete from database pclhr(ze) id ls_pskey.

      endloop.

    endloop.

*--------------------------------------------------------------------*
  endloop.

  commit work.
  message |Checksum report ended - { lines( lt_pernr ) } employees evaluated| type 'S'.

endform.

*--------------------------------------------------------------------*
form prep_selscreen.

  data: lt_text type table of textpool.
  data: ls_text type textpool.

  ls_text-id = 'S'. ls_text-key = 'S_PERNR'.
  ls_text-entry = '        Selected Employees'.
  append ls_text to lt_text.
  ls_text-id = 'S'. ls_text-key = 'S_INFTY'.
  ls_text-entry = '        Selected Infotypes'.
  append ls_text to lt_text.

  ls_text-id = 'S'. ls_text-key = 'P_GENER'.
  ls_text-entry = '        Generate Checksums'.
  append ls_text to lt_text.

  ls_text-id = 'S'. ls_text-key = 'P_REPRT'.
  ls_text-entry = '        Report on differences'.
  append ls_text to lt_text.

  ls_text-id = 'S'. ls_text-key = 'P_DELET'.
  ls_text-entry = '        Delete Checksums'.
  append ls_text to lt_text.

  ls_text-id = 'S'. ls_text-key = 'S_BEGDA'.
  ls_text-entry = '        Dates (for delete)'.
  append ls_text to lt_text.

  ls_text-id = 'S'. ls_text-key = 'S_REPDA'.
  ls_text-entry = '        Dates (for report)'.
  append ls_text to lt_text.

  ls_text-id = 'R'. ls_text-key = space.
  ls_text-entry = 'EPI-USE : Infotype checksum generation/comparison'.
  append ls_text to lt_text.

  sort lt_text by id key.
  insert textpool sy-repid from lt_text language sy-langu.

endform.
