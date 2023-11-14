*&---------------------------------------------------------------------*
*&  Include           LSPOXF08
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Form  GET_SPOOL_LINE
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM get_spool_line.

  DATA: length TYPE i,
          flen TYPE i,      " note 1053675
        bufflg TYPE i,      " note 1053675
        c_rc(5).

  DESCRIBE FIELD data_set_line LENGTH flen IN CHARACTER MODE. " 1053675

  IF is_otf IS NOT INITIAL.
    bufflg = 1006.
  ELSE.
    bufflg = flen.
  ENDIF.

  DO.
    IF temse_rectyp+1(1) = 'Y'.
      CALL 'C_RSTS_READ'
         ID 'HANDLE'  FIELD temse_handle
         ID 'BUFF'    FIELD data_set_line
         ID 'BUFFLG'  FIELD bufflg         " note 1053675
         ID 'ALLINE'  FIELD 'X'
         ID 'BINARY'  FIELD ' '
         ID 'SHOWLG'  FIELD 'X'
         ID 'LENGTH'  FIELD length
         ID 'RC'      FIELD rc
         ID 'ERRMSG'  FIELD errmsg.
      status = sy-subrc.
    ELSE.
      CALL 'C_RSTS_READ'
         ID 'HANDLE'  FIELD temse_handle
         ID 'BUFF'    FIELD data_set_line+1
         ID 'BUFFLG'  FIELD bufflg
         ID 'ALLINE'  FIELD 'X'
         ID 'BINARY'  FIELD ' '
         ID 'SHOWLG'  FIELD 'X'
         ID 'LENGTH'  FIELD length
         ID 'RC'      FIELD rc
         ID 'ERRMSG'  FIELD errmsg.
      status = sy-subrc.
      data_set_line(5) = data_set_line+1(5).
      data_set_line-precol = ' '.
      ADD 1 TO data_set_line-data_length.
    ENDIF.
    status = sy-subrc.
    IF status <> 0 OR errmsg IS NOT INITIAL.
      c_rc = status.
      PERFORM write_trace2 USING 'C_RSTS_READ %s\n' errmsg c_rc. "#EC NOTEXT
    ENDIF.
    IF status <> 6.               " EOF, error condition, or got data
      EXIT.
    ENDIF.
* end of this part, try to open next part
    ADD 1 TO temse_part.
    CALL 'C_RSTS_CLOSE'
          ID 'HANDLE'  FIELD temse_handle
          ID 'RC'      FIELD rc
          ID 'ERRMSG'  FIELD errmsg.
    status = sy-subrc.
    IF status = 0.
      CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
        EXPORTING
          authority     = 'SP01'
          client        = temse_client                          "hjl
          name          = temse_name
          part          = temse_part
        IMPORTING
          charco        = temse_charco
*         CREATER       =
*         CREDATE       =
*         DELDATE       =
*         MAX_CREDATE   =
*         MAX_DELDATE   =
*         NON_UNIQ      =
*         NOOF_PARTS    =
          rectyp        = temse_rectyp
*         SIZE          =
*         STOTYP        =
*         type          =
          objtype       = temse_objtyp
        EXCEPTIONS
          fb_error      = 1
          fb_rsts_other = 2
          no_object     = 3
          no_permission = 4
          OTHERS        = 5.
      status = sy-subrc.
    ENDIF.
    IF status = 0.
      CALL 'C_RSTS_OPEN_READ'
        ID 'HANDLE'   FIELD temse_handle
        ID 'CLIENT'   FIELD temse_client                     "hjl
        ID 'NAME'     FIELD temse_name
        ID 'PART'     FIELD temse_part
        ID 'TYPE'     FIELD temse_objtyp
        ID 'CONV'     FIELD ' '
        ID 'ALLINE'   FIELD 'X'
        ID 'BINARY'   FIELD ' '
        ID 'RECTYP'   FIELD temse_rectyp
        ID 'CHARCO'   FIELD temse_charco
        ID 'PROM'     FIELD 'I'
        ID 'RC'       FIELD rc
        ID 'ERRMSG'   FIELD errmsg.
      status = sy-subrc.
    ENDIF.
  ENDDO.
  IF status = 4.
    status = 12.    "EOF
  ENDIF.
  IF status = 8.
    status = 40.    "Line too long
  ENDIF.
  data_set_length = data_set_line-data_length.

ENDFORM.                    "GET_SPOOL_LINE

*&--------------------------------------------------------------------*
*&      Form  READ_DATA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->BUFFER     text
*      -->TSP01      text
*      -->VALUE(FIRSTtext
*      -->VALUE(LAST)text
*      -->CODEPAGE   text
*---------------------------------------------------------------------*
FORM read_data TABLES buffer
                      buffer_wide              " note 1053675
               USING tsp01 LIKE tsp01
                     value(first) TYPE i
                     value(last) TYPE i
               CHANGING codepage LIKE tst01-dcharcod.

  DATA: lines TYPE i.
  DATA: c_rc(5).
*  DATA: end_of_spool TYPE boolean.

  REFRESH buffer.
  REFRESH buffer_wide.                      " note 1053675
  CLEAR is_otf.
*  end_of_spool = abap_false.

  temse_client = tsp01-rqclient.
  temse_name = tsp01-rqo1name.
  temse_part = 1.

  CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
    EXPORTING
      authority     = 'SP01'
      client        = temse_client
      name          = temse_name
      part          = temse_part
    IMPORTING
      charco        = codepage
*     CREATER       =
*     CREDATE       =
*     DELDATE       =
*     MAX_CREDATE   =
*     MAX_DELDATE   =
*     NON_UNIQ      =
*     NOOF_PARTS    =
      rectyp        = temse_rectyp
*     SIZE          =
*     STOTYP        =
*     type          =
      objtype       = temse_objtyp
    EXCEPTIONS
      fb_error      = 1
      fb_rsts_other = 2
      no_object     = 3
      no_permission = 4
      OTHERS        = 5.

  IF sy-subrc = 0.
    IF temse_objtyp(3) = 'OTF' or temse_objtyp+1(3) = 'OTF'.
      is_otf = 'X'.
    ENDIF.
  ELSE.
    c_rc = sy-subrc.
    PERFORM write_trace1 USING 'RSTS_GET_ATTRIBUTES %s\n' c_rc. "#EC NOTEXT
    MESSAGE e466 WITH tsp01-rqident RAISING read_error.
  ENDIF.

  CLEAR temse_handle.
  CALL 'C_RSTS_OPEN_READ'
      ID 'HANDLE'   FIELD temse_handle
      ID 'CLIENT'   FIELD temse_client                       "hjl
      ID 'NAME'     FIELD temse_name
      ID 'PART'     FIELD temse_part
      ID 'TYPE'     FIELD temse_objtyp
      ID 'CONV'     FIELD ' '
      ID 'ALLINE'   FIELD 'X'
      ID 'BINARY'   FIELD ' '
      ID 'RECTYP'   FIELD temse_rectyp
      ID 'CHARCO'   FIELD codepage
      ID 'PROM'     FIELD 'I'
      ID 'RC'       FIELD rc
      ID 'ERRMSG'   FIELD errmsg.
  status = sy-subrc.
  IF status <> 0 OR errmsg IS NOT INITIAL.
    c_rc = status.
    PERFORM write_trace2 USING 'C_RSTS_READ %s\n' errmsg c_rc. "#EC NOTEXT
  ENDIF.

  IF status = 0.

    DO.
      PERFORM get_spool_line.
      IF status <> 0 AND status <> 40 AND status <> 12.
        PERFORM close_job.
        MESSAGE e466 WITH tsp01-rqident RAISING read_error.
      ENDIF.
      IF status <> 12.                                      " 12 = End
        IF NOT ( data_set_length IS INITIAL ).
          data_set_line-data_length = data_set_length - 1.
        ENDIF.
        ADD 1 TO lines.
        IF lines >= first.
* start of note 1053675
          IF use_wide IS INITIAL.
            IF data_set_length <= 1000.
              APPEND data_set_line TO buffer.
            ELSE.
              IF lines > 0.
                buffer_wide[] = buffer[].
                REFRESH buffer.
              ENDIF.
              APPEND data_set_line TO buffer_wide.
              use_wide = 'X'.
            ENDIF.
          ELSE.
            APPEND data_set_line TO buffer_wide.
          ENDIF.
* end of note 1053675
        ENDIF.

        IF ( NOT last IS INITIAL ) AND ( lines >= last ).
          EXIT.
        ENDIF.
      ELSE.
        IF lines = 0.
          PERFORM close_job.
          MESSAGE e467 WITH tsp01-rqident RAISING job_contains_no_data.
        ENDIF.
        IF lines < first .
          PERFORM close_job.
          MESSAGE e420 RAISING selection_empty.
        ENDIF.
* pass EOF to caller
*        end_of_spool = abap_true.
*        EXPORT eod = end_of_spool TO MEMORY ID 'END_OF_SPOOL_DATA'.
        EXIT.   " insert WO
      ENDIF.
    ENDDO.
    PERFORM close_job.
  ENDIF.
ENDFORM.                    "READ_DATA


*---------------------------------------------------------------------*
*       FORM CLOSE_JOB                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM close_job.

  IF status <> 0 AND status <> 12.
    CALL 'C_RSTS_CLOSE'
          ID 'HANDLE'  FIELD temse_handle
          ID 'RC'      FIELD rc
          ID 'ERRMSG'  FIELD errmsg.
    MESSAGE e112(po) WITH status rc errmsg RAISING read_error.
  ENDIF.
  CALL 'C_RSTS_CLOSE'
     ID 'HANDLE'  FIELD temse_handle
     ID 'RC'      FIELD rc
     ID 'ERRMSG'  FIELD errmsg.
  status = sy-subrc.
  IF status <> 0.
    MESSAGE e112(po) WITH status rc errmsg RAISING read_error.
  ENDIF.
ENDFORM.                    "CLOSE_JOB


*&--------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->BUFFER     text
*      -->RQPAPER    text
*      -->RQID       text
*      -->LCODEPAGE  text
*---------------------------------------------------------------------*
FORM display_data TABLES buffer USING rqpaper LIKE tsp01-rqpaper
                                      rqid LIKE tsp01-rqident
                                      lcodepage.

  DATA: line_length TYPE i, gcol TYPE i, glines TYPE i,
        line_length2 LIKE rststype-linelength,
        v, v2.
*  DATA: rq LIKE tsp01.
  DATA: rq TYPE tsp01sys.
  DATA: wa_tsp02l TYPE tsp02l.
  DATA: c_rc(3).
  DATA: ls_sy TYPE t_sy.

  CLASS cl_abap_char_utilities DEFINITION LOAD.

  CALL FUNCTION 'RSPO_SPOOLDATA_WRITE_INIT'
    EXPORTING
      codepage = lcodepage.

*  CALL FUNCTION 'RSPO_OPTION_GET'
*    EXPORTING
*      name  = spopt_realwidth
*    IMPORTING
*      value = v.
*  CALL FUNCTION 'RSPO_OPTION_GET'
*    EXPORTING
*      name  = spopt_realheight
*    IMPORTING
*      value = v2.
*  IF NOT v IS INITIAL OR NOT v2 IS INITIAL.
*    gcol = 0.
*    glines = 0.
*    SELECT SINGLE * FROM tsp02l INTO wa_tsp02l WHERE pjident = rqid
*                                AND pjnummer = 0.
*    IF sy-subrc = 0.
*      gcol = wa_tsp02l-columns.
*      glines = wa_tsp02l-lines.
*    ELSE.
*      CALL FUNCTION 'RSPO_GET_SIZE_OF_LAYOUT'
*        EXPORTING
*          layout  = rqpaper
*        IMPORTING
**         ANSWER  =
*          columns = gcol
*          lines   = glines
**         PFORMAT =
*        .
*    ENDIF.
*  ENDIF.
*
*  IF gcol < 80 OR v IS INITIAL.
*    gcol = 255.
*  ENDIF.
*
*  IF glines < 5 OR v2 IS INITIAL.
*    glines = 0.
*  ENDIF.

  rq-rqident = rqid.

  CALL FUNCTION 'RSPO_GET_LINES_AND_COLUMNS'
    EXPORTING
      rq     = rq
    IMPORTING
      lines  = glines
      col    = gcol
    EXCEPTIONS
      error  = 1
      OTHERS = 2.
  IF sy-subrc <> 0.
    c_rc = sy-subrc.
    MOVE-CORRESPONDING syst TO ls_sy.
    PERFORM write_trace1 USING 'RSPO_GET_LINES_AND_COLUMNS %s\n' c_rc. "#EC NOTEXT
    MESSAGE ID ls_sy-msgid TYPE ls_sy-msgty NUMBER ls_sy-msgno
           WITH ls_sy-msgv1 ls_sy-msgv2 ls_sy-msgv3 ls_sy-msgv4.
    CLEAR ls_sy.
  ENDIF.

* Buffer only 1000

*  IF gcol >= 1000.
*    gcol = 999.
*  ENDIF.

  IF cl_abap_char_utilities=>charsize = 1.
*    ADD 1 TO gcol.
    IF gcol > 1023.
      gcol = 1023.
    ENDIF.
  ENDIF.

  NEW-PAGE NO-HEADING NO-TITLE LINE-SIZE gcol
                                 LINE-COUNT glines.  " make a wide list
  SET BLANK LINES ON.
  DATA: tablines LIKE sy-tabix.                             "ABO 630
  DESCRIBE TABLE buffer LINES tablines.
  LOOP AT buffer.
    data_set_line = buffer.
    IF data_set_line-precol = 'P'.
      IF data_set_line(1) = ' '.    " Echter Vorschub ?"
        NEW-PAGE.
      ENDIF.
      CONTINUE.
    ENDIF.
*   Zeilenlaenge berechnen, falls unbekannt.
    IF data_set_line-data_length IS INITIAL.
      line_length = strlen( data_set_line-data_line ).
    ELSE.
      line_length = data_set_line-data_length.
    ENDIF.

    IF line_length > 0.
      line_length2 = line_length.

      IF cl_abap_char_utilities=>charsize > 1.
        CALL FUNCTION 'RSPO_SPOOLDATA_WRITE'
          EXPORTING
            spool_data  = data_set_line-data_line
            data_length = line_length2.
      ELSE.
        CALL FUNCTION 'RSPO_SPOOLDATA_WRITE_OLD'
          EXPORTING
            spool_data  = data_set_line-data_line
            data_length = line_length2.
      ENDIF.

    ELSE.
      " Leerzeile
      IF sy-tabix < tablines.
        SKIP.
      ELSE.
        WRITE: / ' '.         "SKIP at end of list is ignored!
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  get_width_of_format
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*      -->P_LIST_FORMAT  text
*      <--P_LIST_WIDTH  text
*----------------------------------------------------------------------*
FORM get_width_of_format  USING    list_format
                          CHANGING list_width.

  CALL FUNCTION 'RSPO_GET_SIZE_OF_LAYOUT'
    EXPORTING
      layout  = list_format
    IMPORTING
*     ANSWER  =
      columns = list_width
*     LINES   =
*     PFORMAT =
    .

ENDFORM.                    " get_width_of_format
*&---------------------------------------------------------------------*
*&      Form  get_type_of_request
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*      -->P_REQUEST  text
*      <--P_REQ_TYPE  text
*----------------------------------------------------------------------*
FORM get_type_of_request  USING    p_rqident.

  CALL FUNCTION 'RSPO_GET_TYPE_SPOOLJOB'
    EXPORTING
      rqident = p_rqident
    IMPORTING
      is_otf  = is_otf.
*   SP_LANG              =.

ENDFORM.                    " get_type_of_request

*&--------------------------------------------------------------------*
*&      Form  CHECK_AUTHORITY
*&--------------------------------------------------------------------*
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM check_authority USING rq STRUCTURE tsp01
                           action.

  CALL FUNCTION 'RSPO_CHECK_JOB_PERMISSION'
    EXPORTING
      access        = action
      spoolreq      = rq
    EXCEPTIONS
      no_permission = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    route_exception no_permission.
  ENDIF.

ENDFORM.                    "check_authority

*&---------------------------------------------------------------------*
*&      Form  gui_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BUFFER_WIDE  text
*      -->P_STARTLINE  text
*      -->P_FNSTR  text
*----------------------------------------------------------------------*
FORM gui_download  TABLES   gui_buffer
                   USING    filesize
                            filetype
                            startline
                            fnstr
                            p_encoding.

  DATA: append.
  DATA: codepage TYPE abap_encod.
  DATA: cp TYPE tcp00-cpcodepage.
  DATA: write_bom type boolean.

  CLEAR codepage.

  IF startline > 1.
    append = 'X'.
  ENDIF.

  IF p_encoding IS NOT INITIAL.
    cp = p_encoding.
    CALL FUNCTION 'SCP_GET_CODEPAGE_PROPERTIES'
      EXPORTING
        codepage         = cp
      EXCEPTIONS
        codepage_unknown = 1
        OTHERS           = 2.
    IF sy-subrc = 0.
      codepage = cp.
    ENDIF.
  ENDIF.

  IF filetype = 'BIN'.
    CLEAR codepage.
  ENDIF.

  IF filetype = cl_rspo_constants=>c_type_dat.
    write_bom = abap_true.
  ELSE.
    CLEAR write_bom.
  ENDIF.

  CALL FUNCTION 'GUI_DOWNLOAD'
   EXPORTING
        bin_filesize            = filesize
        codepage                = codepage
        filename                = fnstr
        filetype                = filetype
        append                  = append
        WRITE_BOM               = write_bom
*           WK1_N_FORMAT            = ' '
*           WK1_N_SIZE              = ' '
*           WK1_T_FORMAT            = ' '
*           WK1_T_SIZE              = ' '
*           COL_SELECT              = ' '
*           COL_SELECTMASK          = ' '
*           NO_AUTH_CHECK           = ' '
*      IMPORTING
*           FILELENGTH              =
   TABLES
        data_tab                = gui_buffer
*           FIELDNAMES              =
   EXCEPTIONS
file_write_error              = 1
no_batch                      = 2
gui_refuse_filetransfer       = 3
invalid_type                  = 4
no_authority                  = 5
unknown_error                 = 6
header_not_allowed            = 7
separator_not_allowed         = 8
filesize_not_allowed          = 9
header_too_long               = 10
dp_error_create               = 11
dp_error_send                 = 12
dp_error_write                = 13
unknown_dp_error              = 14
access_denied                 = 15
dp_out_of_memory              = 16
disk_full                     = 17
dp_timeout                    = 18
OTHERS                        = 19.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING download_failed.
  ENDIF.

ENDFORM.                    " gui_download


*&---------------------------------------------------------------------*
*&      Form  convert_otf_byteorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->OTFCP      text
*      -->SYSCP      text
*      -->OTF_LINE   text
*----------------------------------------------------------------------*
FORM convert_otf_byteorder USING otfcp TYPE tcp00-cpcodepage
                                 syscp TYPE tcp00-cpcodepage
                           CHANGING otf_line TYPE rspo_ds2.
  DATA: c(72) TYPE c,
      otfcmd LIKE itcoo,
      otfout LIKE itcoo,
      parlen TYPE i.
  FIELD-SYMBOLS <p>.                                        "#EC *

  CHECK syscp(2) = '41'.
  CHECK otfcp <> syscp.
  otfcmd = otf_line-data_line.
* convert OTF CMD ID
  PERFORM translate(rstxtranslate) USING otfcmd-tdprintcom
                                         otfcp syscp.

  CLEAR otfout.
  otfout-tdprintcom = otfcmd-tdprintcom.
  otfout-tdprintpar = otfcmd-tdprintpar. "copy all params
* convert parameters
  CASE otfcmd-tdprintcom.
    WHEN pc_id_control.       parlen = 31.
    WHEN pc_id_info.          parlen = 46.
    WHEN pc_id_open_page.     parlen = 65.
    WHEN pc_id_close_page.    parlen = 0.
    WHEN pc_id_codepage.      parlen = 9.
    WHEN pc_id_move_to.       parlen = 10.
    WHEN pc_id_string.        parlen = 7.
    WHEN pc_id_character.     parlen = 10.
    WHEN pc_id_space_width.   parlen = 5.
    WHEN pc_id_supersub.      parlen = 7.
    WHEN pc_id_call_font.
      if otfcmd-tdprintpar+69(1) <> ' '.
        parlen = 70. "UPE TTF  font calls
      else.
        parlen = 43. "resident font calls
      endif.
    WHEN pc_id_uline.         parlen = 15.
    WHEN pc_id_barcode.       parlen = 47.
    WHEN pc_id_barcodeparams. parlen = 20.
    WHEN pc_id_barcode_string. parlen = 0.
    WHEN pc_id_raw_data.      parlen = 4.
    WHEN pc_id_raw_text.      parlen = 0.
    WHEN pc_id_box.           parlen = 28.
    WHEN pc_id_line.          parlen = 21.
    WHEN pc_id_color_box.     parlen = 8.
    WHEN pc_id_color_text.    parlen = 8.
    WHEN pc_id_link_begin.    parlen = 0.
    WHEN pc_id_link_end.      parlen = 0.
    WHEN pc_id_link.          parlen = 4.
    WHEN pc_id_bitmap.        parlen = 59.
    WHEN pc_id_print_control. parlen = 5.
    WHEN OTHERS.              parlen = 70.
  ENDCASE.
  IF parlen = 0.
    IF otfcmd-tdprintcom <> pc_id_barcode_string.
      CLEAR otfout-tdprintpar.
    ENDIF.
  ELSE.
    PERFORM translate(rstxtranslate) USING otfcmd-tdprintpar(parlen)
                                           otfcp syscp.
    otfout-tdprintpar(parlen) = otfcmd-tdprintpar(parlen).
  ENDIF.
  otf_line-data_line = otfout.
ENDFORM.                    "convert_otf_byteorder

*&---------------------------------------------------------------------*
*&      Form  SET_FNSTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FNAME  text
*      -->P_FULLPATH  text
*      <--P_FNSTR  text
*      <--P_DIRECTORY  text
*----------------------------------------------------------------------*
FORM set_fnstr  USING    p_fname TYPE rlgrap-filename
                         p_fullpath TYPE string
                CHANGING p_fnstr TYPE string
                         p_directory TYPE string.

  DATA: separator TYPE c.
  DATA: sapworkdir TYPE string.
  DATA: gui_is_its.
  DATA: result_tab TYPE TABLE OF string.
  DATA: result_lines TYPE i.
  DATA: result TYPE string.

  IF p_fullpath IS NOT INITIAL.
    CLEAR p_directory.
  ENDIF.

  CALL FUNCTION 'GUI_IS_ITS'
    IMPORTING
      return = gui_is_its.

  CALL METHOD cl_gui_frontend_services=>get_file_separator
    CHANGING
      file_separator       = separator
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4             ##SUBRC_OK.

  IF p_directory IS INITIAL AND p_fullpath IS INITIAL.

    IF gui_is_its IS INITIAL.

      IF p_fname NS separator.
        CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
          CHANGING
            sapworkdir            = sapworkdir
          EXCEPTIONS
            get_sapworkdir_failed = 1
            cntl_error            = 2
            error_no_gui          = 3
            not_supported_by_gui  = 4
            OTHERS                = 5.
        IF sy-subrc <> 0.
          MESSAGE e375 WITH 'Download failed'(200)
          RAISING download_failed.
        ENDIF.

        CONCATENATE sapworkdir separator p_fname INTO p_fnstr.
      ELSE.
        p_fnstr = p_fname.
      ENDIF.
    ELSE.
      p_fnstr = p_fname.
    ENDIF.
  ELSEIF p_fullpath IS INITIAL AND p_directory IS NOT INITIAL.
    CONCATENATE p_directory separator p_fname INTO p_fnstr.
  ELSE.
    p_fnstr = p_fullpath.
  ENDIF.

  IF p_directory IS INITIAL.
    IF sapworkdir IS NOT INITIAL.
      p_directory = sapworkdir.
    ELSE.
      SPLIT p_fnstr AT separator INTO TABLE result_tab.
      DESCRIBE TABLE result_tab LINES result_lines.
      LOOP AT result_tab INTO result.
        IF sy-tabix = result_lines.
          EXIT.
        ENDIF.
        IF p_directory IS INITIAL.
          p_directory = result.
        ELSE.
          CONCATENATE p_directory result INTO p_directory
                               SEPARATED BY separator.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " SET_FNSTR

*&--------------------------------------------------------------------*
*&      Form  RETURN_BINARY_SPOOLJOB
*&--------------------------------------------------------------------*
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*

FORM return_binary_spooljob USING spooljobatt STRUCTURE tsp01
                            CHANGING datatab TYPE hextable
                                     bytecount.

  DATA: jobtype TYPE c,
        numpages,
        client LIKE tst01-dclient,
        name LIKE tst01-dname,
        charco LIKE tst01-dcharcod,
        rectype LIKE rststype-rectyp,
        objtype LIKE rststype-type,
        type LIKE rststype-type,
        noofparts LIKE tst01-dnoparts,
        part TYPE tst01-dnoparts,
        handle TYPE rststype-handle.

  DATA: numbytes(5) TYPE n,
        datatab_ofs TYPE i,
        datatab_bytesinline TYPE i.

  DATA: precolatt,
        endofdata,
        endofpart,
        l(1024) TYPE x,
        wa_datatab TYPE hexline.

  CONSTANTS: c_jobtype_binary VALUE 'B',
             c_datatab_size TYPE i VALUE 100,
             c_tms_noprecolumn TYPE c VALUE 'N'.

  PERFORM check_authority USING spooljobatt 'DOWN'.

  client = spooljobatt-rqclient.
  name   = spooljobatt-rqo1name.
  CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
    EXPORTING
      authority     = 'SP01'
      client        = client
      name          = name
      part          = 1
    IMPORTING
      charco        = charco
      noof_parts    = noofparts
      rectyp        = rectype
*     SIZE          =
*     STOTYP        =
      type          = type
      objtype       = objtype
    EXCEPTIONS
      fb_error      = 1
      fb_rsts_other = 2
      no_object     = 3
      no_permission = 4.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      MESSAGE e466 WITH name RAISING cannot_return.
    WHEN 2.
      MESSAGE e466 WITH name RAISING cannot_return.
    WHEN 3.
      MESSAGE e028(ts) WITH name RAISING cannot_return.
    WHEN 4.
      MESSAGE e044(ts) WITH name RAISING cannot_return.     "#EC *
    WHEN OTHERS.
      MESSAGE e466 WITH name RAISING cannot_return.
  ENDCASE.
  numpages = spooljobatt-rqapprule.
  jobtype = c_jobtype_binary.      " 'B'

  datatab_bytesinline = c_datatab_size.
  datatab_ofs = 0.
  precolatt = c_tms_noprecolumn.

  DO noofparts TIMES.
    part = sy-index.
    PERFORM temse_open USING precolatt
                             part
                             client
                             name
                             charco
                             rectype
                             objtype
                             type
                             handle.
    IF sy-subrc <> 0.
      MESSAGE e466 WITH name RAISING cannot_return.
    ENDIF.
    DO.
      numbytes = 0.
      PERFORM temse_read_listline_bin USING handle
                                      l
                                      numbytes
                                      endofdata
                                      endofpart.
      IF endofdata = 'X' OR endofpart = 'X' OR sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF numbytes > 0.
        ADD numbytes TO bytecount.
        WHILE numbytes > datatab_bytesinline.
          wa_datatab-l+datatab_ofs(datatab_bytesinline) = l.
          APPEND wa_datatab TO datatab.
          l = l+datatab_bytesinline.
          numbytes = numbytes - datatab_bytesinline.
          datatab_bytesinline = c_datatab_size.
          datatab_ofs = 0.
        ENDWHILE.
        IF numbytes > 0.
          wa_datatab-l+datatab_ofs(numbytes) = l.
          ADD numbytes TO datatab_ofs.
          datatab_bytesinline = datatab_bytesinline - numbytes.
        ENDIF.
        IF datatab_ofs = c_datatab_size.
          APPEND wa_datatab TO datatab.
          datatab_ofs = 0.
          datatab_bytesinline = c_datatab_size.
        ENDIF.
      ENDIF.
    ENDDO.
    PERFORM temse_close USING handle.
  ENDDO.
  IF datatab_ofs > 0.
    APPEND wa_datatab TO datatab.
  ENDIF.

ENDFORM.                    "RETURN_BINARY_SPOOLJOB

*&---------------------------------------------------------------------*
*&      Form  temse_open
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE        text
*      -->(PRECOLUMN)  text
*      -->VALUE        text
*      -->(PART)       text
*      -->VALUE        text
*      -->(CLIENT)     text
*      -->VALUE        text
*      -->(NAME)       text
*      -->VALUE        text
*      -->(CHARCO)     text
*      -->VALUE        text
*      -->(RECTYPE)    text
*      -->VALUE        text
*      -->(OBJTYPE)    text
*      -->VALUE        text
*      -->(TYPE)       text
*      -->TEMSEHANDLE  text
*----------------------------------------------------------------------*
FORM temse_open USING value(precolumn)
                      value(part) LIKE tst01-dpart
                      value(client) LIKE tst01-dclient
                      value(name) LIKE tst01-dname
                      value(charco) LIKE tst01-dcharcod
                      value(rectype) LIKE rststype-rectyp
                      value(objtype) LIKE rststype-type
                      value(type) LIKE rststype-type
                      temsehandle LIKE rststype-handle.
  DATA: errmsg(70),
        rc(5),
        prom LIKE rststype-prom,
        alline,
        conv.

  CONSTANTS: c_tms_withprecolumn TYPE c VALUE 'Y'.

  prom = 'I'. "processing mode internal modus
  conv = ' '. "if X, data is converted from temse to current syscp
  IF precolumn = c_tms_withprecolumn.
    alline = 'X'.
  ELSE.
    alline = ' '.
  ENDIF.
  CALL 'C_RSTS_OPEN_READ'
      ID 'HANDLE'  FIELD temsehandle
      ID 'CLIENT'  FIELD client
      ID 'NAME'    FIELD name
      ID 'PART'    FIELD part
      ID 'CONV'    FIELD conv
      ID 'BINARY'  FIELD ' '
      ID 'ALLINE'  FIELD alline
      ID 'TYPE'    FIELD type
      ID 'RECTYP'  FIELD rectype
      ID 'CHARCO'  FIELD charco
      ID 'PROM'    FIELD prom
      ID 'RC'      FIELD rc
      ID 'ERRMSG'  FIELD errmsg.
  IF rc <> 0.
    MESSAGE e283 WITH errmsg RAISING cannot_return.
  ENDIF.
  sy-subrc = rc.
ENDFORM.                    "temse_open


*&---------------------------------------------------------------------*
*&      Form  temse_read_listline_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HANDLE  text
*      -->P_L  text
*      -->P_NUMBYTES  text
*      -->P_ENDOFDATA  text
*      -->P_ENDOFPART  text
*----------------------------------------------------------------------*
FORM temse_read_listline_bin USING temsehandle LIKE rststype-handle
                               line TYPE x
                               numbytes TYPE n
                               eod
                               eop.

  STATICS: rc(5) TYPE c,
           msg(70) TYPE c,
           init(2012) TYPE x,
           buf(1006) TYPE c,
           charlen TYPE i.
  FIELD-SYMBOLS <p>.

  charlen = cl_abap_char_utilities=>charsize. "ZV 20081028
  " DESCRIBE FIELD 'A' LENGTH charlen IN BYTE MODE. "Unicode fix

  IF charlen = 2.
    ASSIGN init TO <p> TYPE 'C'.
    buf = <p>. "initialize with hex 00
  ENDIF.

  CALL 'C_RSTS_READ'
      ID 'HANDLE'  FIELD temsehandle
      ID 'BUFF'    FIELD buf
      ID 'BUFFLG'  FIELD 1006
      ID 'ALLINE'  FIELD 'X'
      ID 'BINARY'  FIELD 'X'
      ID 'SHOWLG'  FIELD 'X'  "return ddddd length + data
      ID 'RC'      FIELD rc
      ID 'ERRMSG'  FIELD msg.
  IF sy-subrc = 0.
    numbytes = buf(5).
    ASSIGN buf+5(*) TO <p> TYPE 'X'.
    line = <p>.
    numbytes = numbytes * charlen. "Unicode fix
    "ZV 20081028
*    IF charlen = 2.
*      IF line+numbytes(1) <> '00'.
*        numbytes = numbytes + 1.
*      ENDIF.
*    ENDIF.
  ELSE.
    CASE sy-subrc.
      WHEN 4. eod = 'X'.
      WHEN 6. eop = 'X'.
      WHEN OTHERS.
        eod = eop = space.
    ENDCASE.
  ENDIF.

ENDFORM.                    "temse_read_listline_bin


*&---------------------------------------------------------------------*
*&      Form  temse_close
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HANDLE  text
*----------------------------------------------------------------------*
FORM temse_close  USING  temsehandle.

  DATA: errmsg(70),
        rc(5).

  CALL 'C_RSTS_CLOSE'
      ID 'HANDLE'  FIELD temsehandle
      ID 'RC'      FIELD rc
      ID 'ERRMSG'  FIELD errmsg.
  IF rc <> 0.
    MESSAGE e283 WITH errmsg RAISING cannot_return.
  ENDIF.
  sy-subrc = rc.

ENDFORM.                    "temse_close
