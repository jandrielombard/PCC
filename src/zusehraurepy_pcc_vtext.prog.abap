*-----------------------------------------------------------------------*
* Program Name  : ZHRAUREPY_PCC_VTEXT
* Title         : Launch program for SM34 cluster on Validations
* Create Date   : 10.09.2021
* Release       : ECC 6.0
* Author        : 1168007
*-----------------------------------------------------------------------*
* Description   : Direct call for cluster maintenance
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Date       | User ID      |Description                   |Change Label *
*-----------------------------------------------------------------------*
*           |              |                              |             *
*-----------------------------------------------------------------------*
report zhraurepy_pcc_vtext.

constants: gc_cluster      type vcl_name value 'ZUSEMC_PCC_VTEXT', "SM34 maintenance cluster
           gc_action       type c value 'U', "Update
           gc_spinifex(4)  type c value 'SPIN',
           gc_custom       type c value 'V',
           gc_custom_au(2) type c value 'AU',
           gc_custom_nz(2) type c value 'NZ'.

data: gt_val   type table of pyd_d_tyt,
      gt_cat   type table of pyd_d_tyt,
      gs_val   type pyd_d_tyt,
      gt_key   type table of zuse_pcc_vtextk,
      gs_key   type zuse_pcc_vtextk,
      gv_modif.

* Before the call - populate the key table with custom validations
select type name
  into corresponding fields of table gt_val
  from pyd_d_tyt
  where sprsl = sy-langu.

if sy-subrc = 0.
  "keep only custom validations for text editing
  delete gt_val where not ( type(1) = gc_custom or
                            type(2) = gc_custom_au or
                            type(2) = gc_custom_nz or
                            type(4) = gc_spinifex ).

  sort gt_val by type ascending.
endif.

* Fetch all the key table for comparison
select validation name
  into corresponding fields of table gt_key
  from zuse_pcc_vtextk.

clear gv_modif.
loop at gt_val into gs_val.
  read table gt_key with key validation = gs_val-type transporting no fields.
  if sy-subrc ne 0.
    "add the missing entry
    gs_key-validation = gs_val-type.
    gs_key-name = gs_val-name.
    append gs_key to gt_key.
    gv_modif = gc_action.
  endif.
endloop.

if gv_modif eq gc_action.
  modify zuse_pcc_vtextk from table gt_key.

  commit work.
endif.

call function 'VIEWCLUSTER_MAINTENANCE_CALL'
  exporting
    viewcluster_name             = gc_cluster
    maintenance_action           = gc_action
  exceptions
    client_reference             = 1
    foreign_lock                 = 2
    viewcluster_not_found        = 3
    viewcluster_is_inconsistent  = 4
    missing_generated_function   = 5
    no_upd_auth                  = 6
    no_show_auth                 = 7
    object_not_found             = 8
    no_tvdir_entry               = 9
    no_clientindep_auth          = 10
    invalid_action               = 11
    saving_correction_failed     = 12
    system_failure               = 13
    unknown_field_in_dba_sellist = 14
    missing_corr_number          = 15
    others                       = 16.
