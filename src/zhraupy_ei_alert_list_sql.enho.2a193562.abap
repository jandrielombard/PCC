"Name: \TY:CL_PYC_TEAM_ACTIVE\ME:GET_ALERT_LIST_WITH_SPLIT\SE:BEGIN\EI
ENHANCEMENT 0 ZHRAUPY_EI_ALERT_LIST_SQL.
*---------------------------------------------------------------------------------------------*
*Mod | Date      |User ID|Description                       |Change Label  |Workbench Request *
*---------------------------------------------------------------------------------------------*
*001 |22-09-2021 |1130848|Performance Impprovement          |PTX-3763      |CFAK900861        *
*---------------------------------------------------------------------------------------------*
*002 |17-01-2023 |593740 |HRSP A0-B2 SPAU activity - Replace|PSC-332       |X24K912884        *
*                         query with std one with db hints
*---------------------------------------------------------------------------------------------*

*>>> Start of PTX-3763 - PCC Implementation Enhancement
* With standard SAP SQL statements Q1 payroll area alerts read taking very long time and through
* This enhancement those SQL statements data read order changed using DB Hints
*
    "prepare alerts with PA0001 splits within the period, for override the period considers earliest retro
    "the result contains:
    " - alert key
    " - pa0001 (with splits in the given process recurrence period)
    " - team assignment (can be empty for not assigned)
    " - override status
*Start of Mod-002++
*Replace with SAP SQL query & db hints for performance improvement
*As part of HRSP A0-B2 Upgrade
    SELECT DISTINCT reso~instid, reso~par_hash, reso~par_type, reso~id, alt~team_id, alt~team_name, alt~override, alt~check_id, PA0001~*
      FROM pyd_d_reso AS reso
      LEFT JOIN pa0001 ON pa0001~pernr = reso~id and pa0001~begda <= @is_proc_inst-time_sel_par_val_endda and pa0001~endda >= @is_proc_inst-time_sel_par_val_begda
      LEFT JOIN pyc_d_pyptm_alt AS alt ON reso~instid   = alt~instid AND
                                          reso~par_hash = alt~par_hash AND
                                          reso~par_type = alt~par_type AND
                                          reso~id       = alt~id
     FOR ALL ENTRIES IN @it_res
     WHERE reso~instid     = @it_res-instid
       AND reso~par_hash   = @it_res-par_hash
       AND ( alt~override = @abap_false OR alt~override IS NULL )
       %_hints adabas 'ORDERED' ADABAS 'KEYACCESS'
     INTO TABLE @et_alert.                             "#EC CI_BUFFJOIN
*End of Mod-002++

    DATA(lv_EI_earliest_retro_date) = mo_team_aux->get_earliest_retro_date( is_proc_inst-proc_id ).

    "override alert, need to read back to earliest retro date.
    "MAX( process earliest retro date, personnel earliest retro, hire date )
    " - process earliest retro date => control record of payroll areas.
    " - personnel earliest retor    => infotype 3 PRDAT
    " - hire date                   => enforced by the infotype 0001.
*Start of Mod-002++
*Replace with SAP SQL query & db hints for performance improvement
*As part of HRSP A0-B2 Upgrade
    SELECT DISTINCT reso~instid, reso~par_hash, reso~par_type, reso~id, alt~team_id, alt~team_name, alt~override, alt~check_id, PA0001~*
      FROM pyd_d_reso AS reso
     INNER JOIN pa0001 ON pa0001~pernr = reso~id
     INNER JOIN pa0003 AS p3 ON pa0001~pernr = p3~pernr
     INNER JOIN pyc_d_pyptm_alt AS alt ON reso~instid   = alt~instid AND
                                          reso~par_hash = alt~par_hash AND
                                          reso~par_type = alt~par_type AND
                                          reso~id       = alt~id
     FOR ALL ENTRIES IN @it_res
     WHERE reso~instid     = @it_res-instid
       AND reso~par_hash   = @it_res-par_hash
       AND pa0001~begda <= @is_proc_inst-time_sel_par_val_endda
       AND pa0001~endda >= @lv_EI_earliest_retro_date AND pa0001~endda >= p3~prdat
       AND alt~override = @abap_true
       %_hints adabas 'ORDERED' ADABAS 'KEYACCESS'
     APPENDING TABLE @et_alert.                        "#EC CI_BUFFJOIN
*End of Mod-002++
      EXIT.
*<<< Enf of PTX-3763 - PCC Implementation Enhancement
ENDENHANCEMENT.
