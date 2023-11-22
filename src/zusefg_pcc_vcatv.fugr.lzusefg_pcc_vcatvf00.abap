*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZUSE_PCC_VCATV..................................*
FORM GET_DATA_ZUSE_PCC_VCATV.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZUSE_PCC_VCAT WHERE
(VIM_WHERETAB) .
    CLEAR ZUSE_PCC_VCATV .
ZUSE_PCC_VCATV-MANDT =
ZUSE_PCC_VCAT-MANDT .
ZUSE_PCC_VCATV-VALIDATION =
ZUSE_PCC_VCAT-VALIDATION .
ZUSE_PCC_VCATV-CATEGORY =
ZUSE_PCC_VCAT-CATEGORY .
    SELECT SINGLE * FROM PYD_D_TYT WHERE
SPRSL = 'E' AND
TYPE = ZUSE_PCC_VCAT-VALIDATION .
    IF SY-SUBRC EQ 0.
ZUSE_PCC_VCATV-NAME =
PYD_D_TYT-NAME .
    ENDIF.
<VIM_TOTAL_STRUC> = ZUSE_PCC_VCATV.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZUSE_PCC_VCATV .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZUSE_PCC_VCATV.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZUSE_PCC_VCATV-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZUSE_PCC_VCAT WHERE
  VALIDATION = ZUSE_PCC_VCATV-VALIDATION .
    IF SY-SUBRC = 0.
    DELETE ZUSE_PCC_VCAT .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZUSE_PCC_VCAT WHERE
  VALIDATION = ZUSE_PCC_VCATV-VALIDATION .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZUSE_PCC_VCAT.
    ENDIF.
ZUSE_PCC_VCAT-MANDT =
ZUSE_PCC_VCATV-MANDT .
ZUSE_PCC_VCAT-VALIDATION =
ZUSE_PCC_VCATV-VALIDATION .
ZUSE_PCC_VCAT-CATEGORY =
ZUSE_PCC_VCATV-CATEGORY .
    IF SY-SUBRC = 0.
    UPDATE ZUSE_PCC_VCAT ##WARN_OK.
    ELSE.
    INSERT ZUSE_PCC_VCAT .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZUSE_PCC_VCATV-UPD_FLAG,
STATUS_ZUSE_PCC_VCATV-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZUSE_PCC_VCATV.
  SELECT SINGLE * FROM ZUSE_PCC_VCAT WHERE
VALIDATION = ZUSE_PCC_VCATV-VALIDATION .
ZUSE_PCC_VCATV-MANDT =
ZUSE_PCC_VCAT-MANDT .
ZUSE_PCC_VCATV-VALIDATION =
ZUSE_PCC_VCAT-VALIDATION .
ZUSE_PCC_VCATV-CATEGORY =
ZUSE_PCC_VCAT-CATEGORY .
    SELECT SINGLE * FROM PYD_D_TYT WHERE
SPRSL = 'E' AND
TYPE = ZUSE_PCC_VCAT-VALIDATION .
    IF SY-SUBRC EQ 0.
ZUSE_PCC_VCATV-NAME =
PYD_D_TYT-NAME .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZUSE_PCC_VCATV-NAME .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZUSE_PCC_VCATV USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZUSE_PCC_VCATV-VALIDATION TO
ZUSE_PCC_VCAT-VALIDATION .
MOVE ZUSE_PCC_VCATV-MANDT TO
ZUSE_PCC_VCAT-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZUSE_PCC_VCAT'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZUSE_PCC_VCAT TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZUSE_PCC_VCAT'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZUSE_PCC_VCATV USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZUSE_PCC_VCAT-MANDT =
ZUSE_PCC_VCATV-MANDT .
ZUSE_PCC_VCAT-VALIDATION =
ZUSE_PCC_VCATV-VALIDATION .
ZUSE_PCC_VCAT-CATEGORY =
ZUSE_PCC_VCATV-CATEGORY .
    SELECT SINGLE * FROM PYD_D_TYT WHERE
SPRSL = 'E' AND
TYPE = ZUSE_PCC_VCAT-VALIDATION .
    IF SY-SUBRC EQ 0.
ZUSE_PCC_VCATV-NAME =
PYD_D_TYT-NAME .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZUSE_PCC_VCATV-NAME .
    ENDIF.
ENDFORM.
