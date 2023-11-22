*&---------------------------------------------------------------------*
*&  Include           ZUSEIHR_SPOOL_MERGE_TOP
*&---------------------------------------------------------------------*
TABLES: tsp01, tbtc_spoolid, tbtco, zuse_rpcs0000_a.

TYPES: BEGIN OF ty_spool,
         spoolid  TYPE tbtc_spoolid-spoolid,
         jobname  TYPE tbtc_spoolid-jobname,
         jobcount TYPE tbtc_spoolid-jobcount,
         rqident  TYPE  tsp01-rqident,
         rq1name  TYPE tsp01-rq1name,
         rq2name  TYPE tsp01-rq2name,
         rqtitle  TYPE tsp01-rqtitle,
       END OF ty_spool.

TYPES: BEGIN OF ty_title,
         rqtitle      TYPE zuse_rpcs0000_a-rqtitle,
         job_variance TYPE zuse_rpcs0000_a-job_variance,
         spool_format TYPE zuse_rpcs0000_a-spool_format,
       END OF ty_title.

**Begin of Change PO-986, MOD-002

TYPES : Begin of ty_table ,
         bukrs type bukrs,                "MOD-003++
         field_1 type char30,
         field_2 type char50,
         field_3 type char30,
         field_4 type char20,
         field_5 type char20,
         field_6 type char30,
         field_7 type char30,
         field_8 type char30,
        End of ty_table,

        Begin of ty_table_s ,
          field_1 type LGART,
          field_2 type TEXT50, "LGTXT,   "MOD-003++
          field_3 type P DECIMALS 2,
          field_4 type P DECIMALS 2,
          field_5 type P DECIMALS 2,
          field_6 type P DECIMALS 2,
          field_7 type P DECIMALS 2,
          field_8 type P DECIMALS 2,
        END OF ty_table_s,

        BEGIN OF ty_lsum_wt,
           LGART TYPE LGART,
           LGTXT type LGTXT,
           TP_ANZHL(6)  TYPE P DECIMALS 2,
           TP_BETRG(8)  TYPE P DECIMALS 2,
           MTD_ANZHL(6) TYPE P DECIMALS 2,
           MTD_BETRG(8) TYPE P DECIMALS 2,
           YTD_ANZHL(8) TYPE P DECIMALS 2,
           YTD_BETRG(8) TYPE P DECIMALS 2,
        END OF ty_lsum_wt ,

*Begin of Changw PO-986, MOD-003
        BEGIN OF ty_ccode,
          bukrs TYPE bukrs,
          butxt TYPE butxt,
        END OF ty_ccode,
*End of Changw PO-986, MOD-003

        BEGIN OF ty_lsum_ret,
           LGART TYPE LGART,
           LGTXT type LGTXT,
           RETCHAR type C,
           TP_ANZHL(6)  TYPE P DECIMALS 2,
           TP_BETRG(8)  TYPE P DECIMALS 2,
           MTD_ANZHL(6) TYPE P DECIMALS 2,
           MTD_BETRG(8) TYPE P DECIMALS 2,
           YTD_ANZHL(8) TYPE P DECIMALS 2,
           YTD_BETRG(8) TYPE P DECIMALS 2,
        END OF ty_lsum_ret .

DATA : gt_itable TYPE STANDARD TABLE OF ty_table,
       gt_ccode TYPE STANDARD TABLE OF ty_ccode,             "MOD-003++
       gt_wt TYPE STANDARD TABLE OF ty_lsum_wt.
**End of Change PO-986, MOD-002

DATA: g_completed  TYPE abap_bool,
      g_not_locked TYPE abap_bool,
      gt_tbtco     TYPE STANDARD TABLE OF tbtco,
      gs_tbtco     TYPE  tbtco,
      gt_spool_job TYPE STANDARD TABLE OF ty_spool,
      gt_spool     TYPE STANDARD TABLE OF ty_spool,
      gt_titles    TYPE STANDARD TABLE OF ty_title,
      gt_report    TYPE list_string_table,
      g_jobnam     TYPE tbtc_spoolid-jobname,
      gt_spool_header     TYPE bapixmspow OCCURS 0 WITH HEADER LINE,    "Mod-001++
      gv_flag      TYPE flag.                                           "Mod-001++


SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-006.
PARAMETERS: p_call   TYPE sy-repid OBLIGATORY,
            p_jobnam TYPE tbtc_spoolid-jobname LOWER CASE OBLIGATORY,
            p_jobcnt TYPE tbtc_spoolid-jobcount OBLIGATORY,
            p_secs   TYPE zhr_mer_var DEFAULT '600',
            p_date   TYPE sy-datum OBLIGATORY,
            p_user   TYPE sy-uname  OBLIGATORY.

SELECT-OPTIONS: s_title  FOR tsp01-rqtitle.
SELECTION-SCREEN END OF BLOCK a1.
