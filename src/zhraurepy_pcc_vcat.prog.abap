*-----------------------------------------------------------------------*
* Program Name  : ZHRAUREPY_PCC_VCAT
* Title         : Launch program for SM30 VIEW on Validations Category
* Create Date   : 22.09.2021
* Release       : ECC 6.0
* Author        : 1168007
*-----------------------------------------------------------------------*
* Description   : Direct call for view maintenance and populating data
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Date       | User ID      |Description                   |Change Label *
*-----------------------------------------------------------------------*
*22-Sep-2021| 1168007      |Initial creation              |             *
*           |              |CFAK900862                   |             *
*           |              |                              |             *
*-----------------------------------------------------------------------*
report zhraurepy_pcc_vcat.

constants: gc_view         type vcl_name value 'ZHRPY_PCC_VCAT',
           gc_action       type c value 'U', "Update
           gc_spinifex(4)  type c value 'SPIN',
           gc_custom       type c value 'V',
           gc_custom_au(2) type c value 'AU',
           gc_custom_nz(2) type c value 'NZ'.

data: gt_val   type table of pyd_d_tyt,
      gs_val   type pyd_d_tyt,
      gt_key   type table of zhrpy_pcc_vcat,
      gs_key   type zhrpy_pcc_vcat,
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
select validation category
  into corresponding fields of table gt_key
  from zhrpy_pcc_vcat.

clear gv_modif.
loop at gt_val into gs_val.
  read table gt_key with key validation = gs_val-type transporting no fields.
  if sy-subrc ne 0.
    "add the missing entry
    gs_key-validation = gs_val-type.
    append gs_key to gt_key.
    gv_modif = gc_action.
  endif.
endloop.

if gv_modif eq gc_action.
  modify zhrpy_pcc_vcat from table gt_key.

  commit work.
endif.

call function 'VIEW_MAINTENANCE_CALL'
  exporting
    action                       = gc_action
    view_name                    = gc_view
  exceptions
    client_reference             = 1
    foreign_lock                 = 2
    invalid_action               = 3
    no_clientindependent_auth    = 4
    no_database_function         = 5
    no_editor_function           = 6
    no_show_auth                 = 7
    no_tvdir_entry               = 8
    no_upd_auth                  = 9
    only_show_allowed            = 10
    system_failure               = 11
    unknown_field_in_dba_sellist = 12
    view_not_found               = 13
    others                       = 14.
