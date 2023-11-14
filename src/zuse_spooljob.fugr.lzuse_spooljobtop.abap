FUNCTION-POOL ZUSE_SPOOLJOB     MESSAGE-ID PO.
INCLUDE rspoopt.

INCLUDE rspotrc.      " note 791763
INCLUDE rstxdataotf.  " note 1159672

DATA:   rc(10) TYPE c,                 " Returncode einer C-Funktion
        errmsg(100) TYPE c,            " Fehlermeldung einer C-Funktion
        status LIKE sy-subrc.

TYPES: BEGIN OF normal_list,   " note 1053675
         data_length(5),
         precol(1),
         data_line(1000),
       END OF normal_list.

TYPES: BEGIN OF wide_list,     " note 1053675
         data_length(5),
         precol(1),
         data_line(4096),
       END OF wide_list.

*data: begin of data_set_line,
*        data_length(5),
*        precol(1),
*        data_line(1000),
*      end of data_set_line,
*      data_set_length(5) type c.

DATA: data_set_line TYPE wide_list.

DATA: data_set_length(5) TYPE c.

DATA: use_wide TYPE c.          " note 1053675

* MSZ (06.02.2004) : Extended structure to hold accessibility info
DATA: BEGIN OF acc_data_set_line,
        data_length(5),
        precol(1),
        data_line(8192),
      END OF acc_data_set_line.

TABLES: tsp01, tsp02, tsp02a, tsp01p, tsp09.

DATA: toc TYPE rspotoce OCCURS 20 WITH HEADER LINE,
      last_title LIKE rspotoce-title,
      toc_committed TYPE i.

DATA: BEGIN OF spool_handles OCCURS 3,
        handle(20) TYPE x,
        rqid  LIKE tsp01-rqident,
        ptype LIKE tsp03-patype,
        comp,
        active,
        last_title LIKE rspotoce-title,
        toc LIKE rspotoce OCCURS 20,
      END OF spool_handles.

DATA: temse_name LIKE tst01-dname,
      temse_client LIKE tst01-dclient,
      temse_handle LIKE rststype-handle,
      temse_part LIKE tst01-dpart,
      temse_objtyp LIKE tst01-dtype,
      temse_rectyp LIKE rststype-rectyp,
      temse_charco LIKE tst01-dcharcod.
DATA: is_otf.

DATA: BEGIN OF prts OCCURS 10,
        prt LIKE tsp03-padest,
        name LIKE tsp03d-name,
        type LIKE tsp03-patype,
        driver LIKE tsp0a-driver,
        use_abap,
      END OF prts.

DATA: caller(30).

DATA: append_id TYPE tsp01-rqident.                  " note 742538

TYPES: BEGIN OF hexline,
  l(128) TYPE x,
END OF hexline.

TYPES: hextable TYPE TABLE OF hexline.

TYPES: BEGIN OF t_sy,
  msgid TYPE sy-msgid,
  msgty TYPE sy-msgty,
  msgno TYPE sy-msgno,
  msgv1 TYPE sy-msgv1,
  msgv2 TYPE sy-msgv2,
  msgv3 TYPE sy-msgv3,
  msgv4 TYPE sy-msgv4,
END OF t_sy.

CONSTANTS:   c_sapconnect_smart_forms TYPE soli VALUE 'Smart Forms', "#EC NOTEXT
             c_sapconnect_sapscript   TYPE soli VALUE 'SAPscript'. "#EC NOTEXT

* Trace for UPE
DATA: gv_trc_upe_active TYPE tspoptions-value,
      gv_check_trc_upe TYPE abap_bool.

constants c_type_error type char1 VALUE 'E'.

DEFINE handle_error.
  if sy-subrc <> 0.
    if sy-index = 1.
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            raising cannot_return.
    else.
      exit.
    endif.
  endif.
END-OF-DEFINITION.

INCLUDE rspoutil.
INCLUDE rspoattr.
INCLUDE fp_spool_constants.
