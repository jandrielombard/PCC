*-----------------------------------------------------------------------*
* Program Name  : ZHRAUREPY_PCC_EXECUTE
* Title         : Execute a single PCC validation
* Create Date   : 16.09.2021
* Release       : ECC 6.0
* Author        : 1168007
*-----------------------------------------------------------------------*
* Description   : Submit PYC_EXECUTE_POLICIES for a single validation and
*                 particular employee numbers
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Date       | User ID      |Description                   |Change Label *
*-----------------------------------------------------------------------*
*16-Sep-2021| 1168007      |Initial creation              |             *
*           |              |CFAK900849                    |             *
*           |              |                              |             *
*-----------------------------------------------------------------------*
REPORT zhraurepy_pcc_execute.

NODES: pernr.

CONSTANTS: gc_memory_id TYPE c LENGTH 11 VALUE 'PCC_EXECUTE',
           gc_period    TYPE pyd_par_type VALUE 'PERIOD',
           gc_proc      TYPE PYD_PAR_TYPE VALUE 'PYP_PROC'.

DATA: gt_pernr      TYPE pernr_tab,
      gv_period     TYPE paper,
      gt_instid     TYPE TABLE OF pyd_d_inst,
      gs_instid     TYPE pyd_d_inst,
      gv_validation TYPE pyd_instid,
      gv_pypi_id    TYPE pyc_proc_inst_id,
      gv_proc_id    TYPE pyc_proc_id.

DATA: gs_validation TYPE pyd_d_inst.

SELECTION-SCREEN BEGIN OF BLOCK r1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_proc  TYPE pyd_classid MATCHCODE OBJECT zhrpy_pcc_classid_sh OBLIGATORY,
  p_valid TYPE pyd_type MATCHCODE OBJECT zhrpy_pcc_validation_sh OBLIGATORY.
SELECTION-SCREEN END OF BLOCK r1.

DATA gv_instance TYPE pyd_d_inst.

SELECT-OPTIONS ro_inst FOR gv_instance NO INTERVALS no-DISPLAY.

at SELECTION-SCREEN.
  IF pnpabkrs-low is INITIAL.
    MESSAGE e000(PYC_CONF_WB) WITH text-002.
  ENDIF.

START-OF-SELECTION.

  CLEAR gt_pernr.

* + 28/09/2021 commented out/replaced with direct select to improve performance
*GET pernr.
*
*  APPEND pernr-pernr TO gt_pernr.

  "populate the selected employees
  SELECT a~pernr INTO TABLE gt_pernr
    FROM pa0001 as a INNER JOIN pa0000 as b ON a~pernr = b~pernr
    WHERE a~pernr IN pnppernr AND
          a~abkrs IN pnpabkrs AND
          a~persg IN pnppersg AND
          a~persk IN pnppersk AND
          a~btrtl IN pnpbtrtl AND
          a~werks IN pnpwerks AND
          a~bukrs IN pnpbukrs AND
          a~kostl IN pnpkostl AND
          a~begda <= pn-endda AND
          a~endda >= pn-begda AND
          b~stat2 IN pnpstat2 AND
          b~begda <= pn-endda AND
          b~endda >= pn-begda.
* - 28/09/2021 commented out/replaced with direct select to improve performance

END-OF-SELECTION.

  CONCATENATE pn-pabrj pn-pabrp INTO gv_period.

  "get process instance
  SELECT SINGLE pypi_id proc_id INTO ( gv_pypi_id, gv_proc_id )
    FROM pyc_d_pypi AS a INNER JOIN pyc_d_pypisp AS b
      ON a~proc_id = b~id
   WHERE a~time_sel_par_type = gc_period AND
         a~time_sel_par_val = gv_period AND
         a~original_pypte_id = p_proc AND
         b~value = pnpxabkr.

    CHECK sy-subrc = 0.

    "get latest validation instance
    SELECT a~* INTO TABLE @gt_instid
      FROM pyd_d_inst as a INNER JOIN pyd_d_instp as b
                           ON a~id = b~id
      WHERE a~type = @p_valid AND
            b~par_type = @gc_proc AND
            b~low = @gv_proc_id.

      CHECK sy-subrc = 0.

      sort gt_instid by id DESCENDING.

      READ TABLE gt_instid INTO gs_instid INDEX 1.

      INSERT VALUE #( sign = 'I' option = 'EQ' low = gs_instid-id ) INTO TABLE ro_inst.

      "populate set of PERNRs into memory for later use in submit
      EXPORT gt_pernr = gt_pernr TO MEMORY ID gc_memory_id.

      "start the validation
      SUBMIT pyc_execute_policies WITH p_proc_i = gv_pypi_id
                                  WITH so_inst IN ro_inst
                                  WITH p_testpy  = abap_true.
